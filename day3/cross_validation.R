## day 3: cross-validation (or how well does the model generalize?)


## read in the data

setwd('~/GitHub/data-mining-intersession/day3')
movies <- read.csv('einav_movie_data.csv', stringsAsFactors=F)
movies <- movies[movies$max_scrn > 600,]
movies$log_rev <- log(movies$cum_rev)
movies$log_rev_w1 <- log(movies$week1)
  # description: box-office revenues for all movies released between 1/1/1985 and 12/31/1999
  # source: Einav (2007)
  #         http://www.stanford.edu/~leinav/pubs/RAND2007.pdf



## goal: predict cumulative revenue with only week 1 information
## we've seen how to fit a model on the full data

m1 <- lm(log_rev~scrns1, data=movies)
summary(m1)

plot(log_rev~scrns1, data=movies)
abline(m1, col='red')

new_movie <- list(scrns1=1000)
pred <- predict.lm(m1, newdata=new_movie) # see ?predict.lm
exp(pred)

## how 'good' is the model? can calculate model fit, typically the sum of squared errors

fitteds <- m1$coefficients[1] + m1$coefficients[2] * movies$scrns1 # by hand
fitteds <- m1$fitted.values # alternatively

summary(m1) # R-squared is a common measurement
cor(fitteds, movies$log_rev)^2 # it is the correlation between fitteds and actuals, squared

residuals <- movies$log_rev - fitteds # by hand
residuals <- m1$residuals # alternatively
mean(residuals^2) # mean squared error (MSE) is another

n <- nrow(movies)
sqrt(1/(n-1) * sum(residuals^2)) # the correct answer, close to the above (corresponds to 'Residual standard error' in summary(m1))

## alternatively, want to know how well the model generalizes
## split the data in half, fit the model on one half and see how well it predicts on the other

set.seed(23)
n <- nrow(movies)

splits <- sample(x=1:2, size=n, replace=T)
train <- movies[splits==1,]
test <- movies[splits==2,]

m2 <- lm(log_rev~scrns1, data=train)
preds <- predict.lm(m2, newdata=test)
mse <- mean((preds-test$log_rev)^2)
mse

## get more estimates of error
## k-fold cross validation

k <- 5
splits <- sample(x=1:k, size=n, replace=T) # split data into k pieces

errs <- rep(NA, 5)
for(i in 1:5) {
  train <- movies[splits==i,]
  test <- movies[splits!=i,]
  mk <- lm(log_rev~scrns1, data=train)
  preds <- predict.lm(m2, newdata=test)
  mse <- mean((preds-test$log_rev)^2)
  errs[i] <- mse
}

errs
mean(errs)
