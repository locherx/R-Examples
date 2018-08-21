## gam.R


require(gam)
?gam
?lo
?s


set.seed(0)
## fake some data...
f1 <- function(x) {exp(2 * x)}
f2 <- function(x) {
  0.2*x^11*(10*(1 - x))^6 + 10*(10*x)^3*(1 - x)^10
}
f3 <- function(x) {x*0}

n<-200
sig2<-4
x0 <- rep(1:4,50)
x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)
x3 <- runif(n, 0, 1)
e <- rnorm(n, 0, sqrt(sig2))
y <- 2*x0 + f1(x1) + f2(x2) + f3(x3) + e
x0 <- factor(x0)
dat.02 <- data.frame(y, x0, x1, x2, x3)

gam.00 <- gam(y ~ x0 + lo(x1, span = 0.5) + lo(x2, span = 0.5) + lo(x3, span = 0.5), data = dat.02)
windows(10, 10)
par(mfrow = c(2, 2))
plot(gam.00, residuals = TRUE, se = TRUE, cex = 1, scale = 16)
summary(gam.00)

gam.01 <- gam(y ~ x0 + lo(x1, span = 0.5) + lo(x2, span = 0.1) + lo(x3, span = 0.5), data = dat.02)
windows(10, 10)
par(mfrow = c(2, 2))
plot(gam.00, residuals = TRUE, se = TRUE, cex = 1, scale = 16)
summary(gam.01)
