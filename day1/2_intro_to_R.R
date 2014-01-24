##
## I. A LOOK AT THE DATA
##

## set path
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
View(nba) # avoid typing nba itself because it will overflow the console

##
## II. R BASICS
##

## access specific elements of the data with [row, column] coordinates
nba[1,1] # row 1, column 1
nba[2,1] # row 2, column 1
nba[1:2,1:2], # rows 1 to 2, columns 1 to 2

## introducing vectors
v1 <- c(1,2,3,4,5) # 
v2 <- c(6,7,8,9,10)
v3 <- c('a','b','c','d','e')
is.vector(v1)

v1
v2
v3

v1[1] # access specific elements of the vector
v1[5]

## element-wise vector operations
v1 + 3
v1 + v1
sqrt(v1)
sum(v1^2) # sum takes in a vector, returns a number

## create "sequences" using the : operator (?colon)
x <- 1:5
x
x[1]


## boolean values are TRUE and FALSE
(TRUE | FALSE) 
!(!T & F) # T and F are abbreviations

x == v1 # element-wise comparisons for vectors
1:5 > 2 # can create conditions

## exercises:
## 1) create a vector of the even numbers between 1 and 50
## 2) what happens if you use ?sum on a vector of boolean values?
## 3) what would happen if you ran nba[,]?
## 4) explore R's built-in constants by reading the documentation (type ?Constants)

##
## III. Exploring data
##

## can access subsets of the data using sequences
nba[1:5,1:5] # rows 1 to 5, columns 1 to 5
nba[1:5,] # rows 1 to 5, all columns
View(nba[1:5,])

## can access specific columns with the ?Extract operator
names(nba)
nba$pid[1:5]
nba[1:5,'pid'] # rows 1 to 5, "player" column

## can subset data according to certain conditions
lbj <- nba[nba$player == 'LeBron James',] # subset of box scores for LeBron James
basic <- c('player','min','pts','reb')
lbj[1:5,basic]

## how many players were active?
v1 <- c('a','a','b','d')
unique(v1) # the "unique" function returns the unique values in the vector
length(unique(v1)) # the "length" function returns the length of the vector
length(unique(nba$pid))

## exercises:
## 1) examine the nba dataset with ?View. What does it contain?
## 2) what is the difference between the 'pid' and 'player' columns?
## 3) what set of variables is an identifier for the dataset (i.e. there are 31,351 unique combinations)? in other words, what is the level of the dataset?
## 4) create a subset of the data that contains entries for all point guards (PG). what are its dimensions?
## 5) how many total games were played in the '12-'13 season? Is it what you would expect (hint: 30 teams play 82 games each)?
## 6) what is the average number of minutes played? points scored? (try ?mean)

##
## IV. SCORING STATISTICS
## 

## how many points did LeBron James score?
sum(lbj$pts)
sum(nba$pts[nba$player=='LeBron James'])
# check against http://espn.go.com/nba/player/stats/_/id/1966/lebron-james

## who scored most points per game (ppg)?
nba <- nba[nba$min > 0, ] # remove games in which players were inactive

pid <- 9 # want to repeatedly do this
tmp <- nba[nba$pid==pid,]
mean(tmp$pts)
## mini exercises:
## 1) how would you check that pid 9 corresponds to Ray Allen?
## 2) how would you check the player name for any given pid?
## 3) Write a for loop to calculate ppg for every player (use ?Control, see accompanying slides for additional help)

## calculate ppg using R's built in functions
ppg <- aggregate(x = nba$pts,
                 by = list(pid = nba$pid),
                 FUN = mean)
max(ppg$x)

## can also use Hadley Wickham's very popular plyr package
# install.packages('plyr')
require(plyr)
ppg2 <- ddply(nba, .(pid), summarize, ppg = mean(pts))
ppg2 <- ddply(nba, .(pid, player, summarize, ppg = mean(pts)) # if we wanted player names too

## player with most ppg
idx <- which.max(ppg$x)
pid <- ppg$pid[idx]
pid
nba$player[nba$pid==pid][1]

## top 5 scorers
plyrs <- unique(nba[,c('pid','player')]) # haha
plyrs[1:5,]
ppg <- merge(ppg, plyrs, by='pid') # or we could just use ppg2...
ppg[1:5,]

order_ppg <- order(ppg$x, decreasing=T)
order_ppg[1:5] # the "order" function returns a sorted vector of indices

ppg <- ppg[order_ppg,] # pass in the vector of indices
ppg[1:5,]
## check that this matches ESPN http://espn.go.com/nba/statistics/_/year/2013

## exercises:
## 1) Calculate rebounds, assists, and blocks per game (reb, ast, blk) for each player. Who are the top players in each category?
## 2) (Challenge) Calculate all per-game stats with a single command
## 3) Which team scored the most points per game?

##
## V. VISUALIZING DATA
##

## what is the distribution of players in terms of points per game?
hist(ppg$x)
hist(ppg$x, breaks=seq(from=0,to=30,by=2.5))

## how does LeBron's ppg look over the course of the season?
lbj <- subset(nba, player=='LeBron James')
lbj_ppg <- cumsum(lbj$pts) / 1:length(lbj$pts) # this is a "rolling" calculation
plot(lbj_ppg, type='l', ylab='PPG', xlab='Game #')

## how does it compare to Kevin Durant's?
kd <- subset(nba, player=='Kevin Durant')
kd_ppg <- cumsum(kd$pts) / 1:length(kd$pts)
plot(lbj_ppg, type='l', ylab='PPG', xlab='Game #', ylim=c(20,30))
lines(kd_ppg, type='l', col='red') # add lines to an existing plot

## minor segue: defining functions
square <- function(x) { return(x^2) }
square(2)
square(1:5)

sum2 <- function(x,y) { return(x+y) }
sum2(1,2)

## what if we wanted to compare any two players?
roll_calc <- function(x) { # define a custom function
  ppg <- cumsum(x$pts) / 1:length(x$pts)
  return(ppg)
}
kd_ppg2 <- roll_calc(subset(nba, pid==3202))

ppg_roll <- dlply(nba, .(pid), roll_calc) # return a list of vectors after applying roll_calc
ppg_roll[[1]] # access by index
ppg_roll[['9']] # access by key
names(ppg_roll)

plot(ppg_roll[['1966']], type='l', ylim=c(20,30)) # LeBron James
lines(ppg_roll[['3202']], type='l', col='red') # Kevin Durant
lines(ppg_roll[['110']], type='l', col='blue') # Kobe Bryant

## what is the relationship between various statistics?
## e.g. do high scorers collect more rebounds?
stats <- c('min','reb','ast','stl','blk','tos','pts')
pg <- aggregate(nba[,stats], list(pid=nba$pid), mean)
pg <- merge(pg, plyrs, by='pid')
pg[1:5,]

plot(y=pg$pts, x=pg$reb) # scatter plot
cor(pg$pts, pg$reb) # correlation
plot(pg[,stats]) # series of scatter plots

## exercises:
## 1: plot a histogram of rebounds per game by player. what does it tell you?
## 2: what is an issue with our rolling calculation (hint: how many games has each player played)? How might you address it?
## 3: (Challenge) Fix the issue (ask me for game dates matched to game_ids, as game_ids are not exactly ordered by time)
## 4: it looks like there is a positive relationship between points and rebounds per game. does this mean better scorers are better rebounders? why or why not?
## 5: try scatter plots of different statistics. what is a drawback of per game statistics? (hint: try comparing to minutes played)
## 6: calculate points and rebounds per 48 minutes. what does this relationship tell you?

##
## VI. LINEAR REGRESSION
##

## per game statistics are influenced by minutes played
with(pg, plot(pts~min))
with(pg, plot(reb~min))

## investigate the relationship between scoring and minutes using linear regression
m1 <- lm(pts~min, data=pg)
summary(m1) # (Intercept) is the intercept, reb is the slope of the line
with(pg, plot(pts~min))
abline(m1, col='red')

## what does the scoring/rebound relationship look like after accounting for minutes?
m2 <- lm(pts~reb+min, data=pg)
summary(m2) # insignificant relationship

m3 <- lm(pts~reb+min+tos, data=pg)
summary(m3) # relationship is nearly zero after accounting for more variables

## regression models can be used as predictive models
fake_player <- as.data.frame(list(reb=8.0,min=20.0,tos=1.5))
predict(m3, newdata=fake_player) # the model predicts he would score 8.6ppg

## exercises:
## 1: Rank the top scorers by points per 48 minutes
##    a - How does this ranking compare to the points per game ranking?
##    b - What additional considerations must you make to get at scoring ability?
## 2: (Challenge) Create a regression model for predicting a player's scoring (ppg) from previous year's scoring
##    a - Read in and prepare data from the '11-'12 season
##    b - Fit a model that predicts '12-'13 ppg from '11-'12 statistics
##    c - Evaluate the model's performance on '13-'14 (so far)