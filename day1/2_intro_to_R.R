##
## A taste of the data
##

## set dropbox path
setwd('C:/Users/RYWANG/Documents/GitHub/data-mining-intersession/day1')

## read in data frame
nba <- read.csv('nba_box_2012to2013.csv', stringsAsFactors=FALSE)
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
## Getting started with R
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
runif(5)

## besides numeric values, can also have boolean values (TRUE or FALSE)
(TRUE | FALSE) 
!(!T & F) # T and F are abbreviations
1:5 > 2

##
## exploring data
##

## or can access subsets of the data using "sequences" (type ?colon)
1:5
6:10
nba[1:5,]
nba[1:5,1:5]

## can access specific columns with the "extract" operator (type ?Extract)
nba$pid[1:5]
nba[1:5, 'player']

## can subset data according to certain conditions
lbj <- nba[nba$player == "LeBron James",] # box scores for LeBron James
lbj[1:5,3:7]

## how many players played in the 2012-2013 season?
v1 <- c('a','a','b','d')
unique(v1) # the "unique" function returns the unique values in the vector
length(unique(v1)) # the "length" function returns the length of the vector
length(unique(nba$pid))

## exercises
## 1) create a vector containing the even numbers between 1 and 50
## 2) create a subset of the data that contains entries for all point guards (PG)
## 3) what are the dimensions of the data.frame from 2)?
## 4) how many total games were played in the season? (note: some questions you don't need data)

##
## season-level statistics
## 

## how many points did LeBron James score?
sum(lbj$pts)

## dealing with missing values
