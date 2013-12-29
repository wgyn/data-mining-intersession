##
## Basics: Dataframes
##

## set dropbox path
# setwd('C:/Users/RYWANG/Documents/GitHub/data-mining-intersession/day1')
setwd('C:/Users/R_Wang/Documents/Dropbox/basketball/data/box')

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

mod_cs <- function(x) { # define a modified function for cumulative sum
  x2 <- replace(x, is.na(x), 0)
  return(cumsum(x2))
}

nba <- nba[order(nba$pid, nba$game_id),] # sort our dataframe by player and game
ppg_roll <- tapply(nba2$pts, nba2$player, mod_cs) # TO DO: explain output
plot(ppg_roll[['LeBron James']] / cumsum(lbj$gp), type='l', ylim=c(20,30)) # TO DO: explain lists
lines(ppg_roll[['Kevin Durant']] / cumsum(lbj$gp), type='l', col='red')

## exercises:
## 1: TO DO
## 2: nba does not contain entries for when players are inactive e.g.
##    Carmelo only has 67 entries, fix the calculation for ppg_roll so that
##    he is comparable to players with 82 entries (challenge)

##
## TO DO: Scatter plots, segue into linear regression
##

## how do stats vary with each other?
stats <- c('reb','ast','stl','blk','tos','pts','gp')
season <- aggregate(nba[,stats], list(pid=nba$pid), sum, na.rm=T)
season <- merge(season, plyrs, by='pid')
per_game <- cbind(season[,c('pid','player')], season[,2:7]/season$gp)
