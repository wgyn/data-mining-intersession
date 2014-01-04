##
## Basics: Dataframes
##

## set github path
setwd('~/GitHub/data-mining-intersession/day1')

## read in data frame
nba <- read.csv('box_2012to2013.csv', stringsAsFactors=FALSE)
class(nba) # reads in as a "data.frame"
nba[1:5,1:5]

## data contains 31,375 rows (entries) and 18 columns (variables)
nrow(nba)
ncol(nba)
dim(nba)

## each row represents a line item from a box score
## see e.g. http://scores.espn.go.com/nba/boxscore?gameId=400277721
nba[1,]

## each column represents a particular statistic or identifier
## e.g. pid --> player's id, player --> player's name
names(nba)

## can view the data as a spreadsheet
View(nba) # avoid typing data itself because it will overflow the console

##
## Basics: Vectors
##

## introducing vectors
v1 <- c(1,2,3,4,5) # 
v2 <- c(6,7,8,9,10)
v1
is.vector(v1)

## element-wise vector operations
v1 + v2
v1 * v2
v1 + 3
sqrt(v1)

## Some useful vector functions
1:5 # create "sequences" using the : operator (?colon)
runif(5) # create random vectors using runif (stands for random uniform)

## besides numeric values, can also have boolean values (TRUE or FALSE)
(TRUE | FALSE) 
!(!T & F) # T and F are abbreviations
1:5 > 2

##
## Exploring data
##

## can access subsets of the data using sequences
nba[1:5,] # rows 1 to 5
nba[1:5,1:5] # rows 1 to 5, columns 1 to 5

## can access specific columns with the "extract" operator (type ?Extract)
nba$pid[1:5]
nba[1:5,'player'] # rows 1 to 5, "player" column

## can subset data according to certain conditions
lbj <- nba[nba$player == "LeBron James",] # box scores for LeBron James
basic <- c('player','min','pts','reb')
lbj[1:5,basic]

## how many players were active?
v1 <- c('a','a','b','d')
unique(v1) # the "unique" function returns the unique values in the vector
length(unique(v1)) # the "length" function returns the length of the vector
length(unique(nba$pid))

## exercises:
## 1) create a vector containing the even numbers between 1 and 50
## 2) create a subset of the data that contains entries for all point guards (PG)
## 3) what are the dimensions of the data.frame from 2)?
## 4) how many total games were played in the season? (note: some questions you don't need data)

##
## Exploring data cont.
## 

## how many points did LeBron James score?
sum(lbj$pts) # introducing the missing value indicator
lbj[lbj$game_id=='400278818',basic] # LeBron did not play in this game, missing values are distinguished
sum(lbj$pts, na.rm=T) # need to specify an option for the "sum" function
# check against http://espn.go.com/nba/player/stats/_/id/1966/lebron-james

## data says 2055, ESPN says 2036 - what gives?
nba[nba$game_id=='400436572',][1:5,basic] # this is a pretty crazy lineup... because it's the all-star game
nba <- subset(nba, nba$game_id!='400436572') # remove this game
lbj <- nba[nba$player == "LeBron James",] # recreate lbj
sum(lbj$pts, na.rm=T) # that's better

## who scored most points per game?
nba$gp <- as.integer(!is.na(nba$min)) # boolean values can be converted to integers
ppg <- aggregate(nba[,c('pts','gp')], list(pid=nba$pid), sum, na.rm=T)
ppg$ppg <- with(ppg, pts/gp) # the "with" function is a shortcut

order_ppg <- order(ppg$ppg, decreasing=T)
order_ppg[1:5] # the "order" function returns a sorted vector of indices

plyrs <- unique(nba[,c('pid','player')]) # bring back player names
ppg <- merge(ppg, plyrs, by='pid') # the "merge" function joins two dataframes
ppg <- ppg[order_ppg,]
ppg[1:5,]

## check that this matches ESPN http://espn.go.com/nba/statistics/_/year/2013

## exercises:
## 1) check that your favorite player's season stats match his ESPN page
## 2) which player scored the most total points? (try ?max)
## 3) which player gathered the most rebounds per game?
## 4) which team scored the most points per game?

##
## Visualizing data
##

## what is the distribution of players in terms of points per game?
hist(ppg$ppg) # the "hist" function computes frequencies e.g. # of players who scored 0-5 ppg
hist(ppg$ppg, breaks=seq(from=0,to=30,by=2.5))

## bar chart of top players
par(las=2)
with(per_game[order_ppg[1:10],], barplot(pts, horiz=T, names.arg=player, xlim=c(0,30)))

## how does LeBron's ppg look over the course of the season?
lbj <- subset(nba, player=='LeBron James')
lbj_pts <- replace(lbj$pts, is.na(lbj$pts), 0)
lbj_ppg <- cumsum(lbj_pts) / cumsum(lbj$gp) # this is a "rolling" calculation
plot(lbj_ppg, type='l', ylab='PPG', xlab='Game #')

## how does it compare to Kevin Durant's?
kd <- subset(nba, player=='Kevin Durant')
kd_pts <- replace(kd$pts, is.na(kd$pts), 0)
kd_ppg <- cumsum(kd_pts) / cumsum(kd$gp) # this is a "rolling" calculation
plot(lbj_ppg, type='l', ylab='PPG', xlab='Game #', ylim=c(20,30))
lines(kd_ppg, type='l', col='red') # add lines to an existing plot

## what if we wanted to compare any two players?
square <- function(x) { return(x^2) } # minor segue on defining functions (see ?function)
square(2)
square(1:5)

## note: we are sidestepping the issue that not all players were active in every game
##       e.g. Carmelo Anthony has only 74 entries
nba2 <- nba[order(nba$pid, nba$game_id),] # sort our dataframe by player and game
nba2$pts <- replace(nba2$pts, is.na(nba2$pts), 0)
ppg_roll <- tapply(nba2$pts, nba2$pid, cumsum) # "tapply" is a general version of aggregate and returns a list
gp_roll <- tapply(nba2$gp, nba2$pid, cumsum)

plot(ppg_roll[['1966']] / gp_roll[['1966']], type='l', ylim=c(20,35)) # LeBron James
lines(ppg_roll[['3202']] / gp_roll[['3202']], type='l', col='red') # Kevin Durant
lines(ppg_roll[['110']] / gp_roll[['110']], type='l', col='blue') # Kobe Bryant

## what is the relationship between various statistics?
## e.g. do high scorers collect more rebounds?
stats <- c('min','reb','ast','stl','blk','tos','pts','gp')
season <- aggregate(nba[,stats], list(pid=nba$pid), sum, na.rm=T)
season <- merge(season, plyrs, by='pid')
per_game <- cbind(season[,c('pid','player')], season[,2:8]/season$gp)

with(per_game, plot(y=pts, x=reb)) # a scatter plot of ppg versus rpg
with(per_game, plot(pts~tos)) # a scatter plot of ppg versus tpg
with(per_game, cor(pts, tos, use='complete')) # a measure of the relationship's strength
# seems to be a positive relationship between points and rebounds

## exercises:
## 1: plot a histogram of rebounds per game by player. what does it tell you?
## 2: think of other ways to describe the distribution of ppg and rpg (e.g. try ?sd, ?mean, ?quantile)
## 3: try scatter plots of different statistics. what is a drawback of per game statistics? (hint: try minutes played)
## 4 (challenge): fix the active player issue above so that you can trace
##			their statistics through all 82 games of the season
##			(hint: you will need game dates from "box_ov_2012to2013.csv")

##
## Linear regression
##

## per game statistics are influenced by minutes played
## per minute (typically per 48 minute) statistics are better for certain comparisons
with(per_game, plot(pts~min))
with(per_game, plot(reb~min))
per_48min <- cbind(season[,c('pid','player')], season[,3:8]/season$min*48)

per_48min2 <- per_48min[order(per_48min$pts, decreasing=T),]
per_48min2[1:5,] # Henry Sims is first??
season[season$pid==6647,] # he played only 5 minutes... we have a sample size problem
per_48min$qualify <- with(season, gp>=70 | pts>=1400) # try espn's criteria

p48m_q <- per_48min[per_48min$qualify,]
with(p48m_q, plot(pts~reb))
with(p48m_q, cor(pts,reb)) # slightly negative relationship

## using linear regression (i.e. fitting a line through the scatter plot)
m1 <- lm(pts~reb, data=p48m_q)
summary(m1) # (Intercept) is the intercept, reb is the slope of the line
with(p48m_q, plot(pts~reb))
abline(m1, col='red')

## multiple controls using multivariate regression
m2 <- lm(pts~reb+min, data=per_game)
summary(m2) # typical interpretation: for each additional mpg, the average player scores .52 additional ppg

m3 <- lm(pts~reb+min+tos, data=per_game)
summary(m3) # accounting for TOs, there is essentially no relation between ppg and rpg

## regression models can be used as predictive models
fake_player <- as.data.frame(list(reb=8.0,min=20.0,tos=1.5))
predict(m3, newdata=fake_player) # the model predicts he would score 8.6ppg

## exercises:
## 1: In simple linear regression, the slope is given by cov(x,y)/var(x)
##    The (pearson) correlation is given by cor(x,y)=cov(x,y)/(sd(x)*sd(y))
##    Calculate the slope for m2 by hand using ?cor and ?sd functions. What does this tell you about correlation?
## 2: Create a regression model for predicting a player's scoring (ppg) from previous year's scoring
##    a - Read in "box_2011to2012.csv" and calculate ppg for each player
##    b - Plot ppg_12to13 against ppg_11to12
##    c - Fit a regression model
##    d - Qualitatively test how well the model predicts 2013-2014 stats from 2012-2013 (use ESPN player pages e.g. http://espn.go.com/nba/player/_/id/110)
##    e - Consider including other variables e.g. minutes, team, etc.