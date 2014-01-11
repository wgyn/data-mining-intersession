# Can always add variables to increase R2
# Let's run a quick experiment

set.seed(23)

X <- rnorm(100) # Create random data
M <- 5
E <- 20 * rnorm(100)
Y <- M * X + E

m1 <- lm(Y~X) # Fit a linear regression model
summary(m1) # R2 = .05141

plot(Y~X)
abline(m1, col='red')

Z <- rnorm(100) # Totally uninformative predictor
plot(Y~Z)

m2 <- lm(Y~X+Z)
summary(m2) # R-squared increased to .05146

