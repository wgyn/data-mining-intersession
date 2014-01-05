set.seed(23)

## Price over season
p0 <- 15
p <- 15 - c(.25 * 1:10, 15 * seq(.2,.4,.05)) * runif(15)
wk <- 1:length(p)
pdf(file='figs/price_season.pdf',height=4,width=7)
plot(p~wk, type='l',ylab='Price',xlab='Week')
dev.off()

## Sell-through rates
st <- c(runif(100), seq(.3,.7,.01))
pdf(file='figs/sell_through.pdf',height=6,width=6)
hist(st,breaks=10,xlab='Sell-through rates',ylab='No. of products',main='')
dev.off()

## Quantity versus price
d <- 1000
eps <- -2
k <- 2

p <- 5:25
q <- d*p^eps
n <- length(p)*k

lp_e <- rep(log(p),k)
lq_e <- rep(log(q),k) + runif(n)*mean(log(q)) * (2*runif(n)-1)

pdf('figs/pq.pdf',height=4,width=4)
plot(exp(lp_e)~exp(lq_e),ylab='Price',xlab='Quantity',xlim=c(0,50))
lines(p~q,lty=2,col='red')
dev.off()

pdf('figs/pq_log.pdf',height=4,width=4)
plot(lp_e~lq_e,ylab='Log Price',xlab='Log Quantity')
abline(lm(lp_e~lq_e),lty=2,col='red')
dev.off()


