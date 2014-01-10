# Section 0: Loading the packages and data

# install.packages('class')
require(class)
setwd('~/GitHub/data-mining-intersession/day2')
train <- read.csv('train.csv', stringsAsFactors=F)
test <- read.csv('test.csv', stringsAsFactors=F)

# Section ??: Extract cabin features

# install.packages(stringr)
require(stringr)

extract_num_cabs <- function(data) {
  cab_str <- strsplit(as.character(data$Cabin), " ")
  data$num_cabs <- sapply(cab_str, length)
  return(data)
}

train2 <- extract_num_cabs(train)
test2 <- extract_num_cabs(test)

extract_cab_deck <- function(data) {  
  cab_str2 <- str_split_fixed(data$Cabin, " ", max(data$num_cabs)) # cabin deck
  cab_str2 <- substr(cab_str2, 1, 1)  
  for(cab in c('A','B','C','D','E','F','G','T')) {
    data[,paste(cab,'_deck')] <- apply(cab_str2, MARGIN=1, FUN=function(r) as.integer(cab %in% r))
  }  
  data  
}

train3 <- extract_cab_deck(train2)
test3 <- extract_cab_deck(test2)
