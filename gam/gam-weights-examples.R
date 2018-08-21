###########################################################################
## gam-weights.R
library(mgcv)

## test data with constant variation coefficient
n <- 100
x <- 1:n
y <- 1000+2*x+rnorm(n,sd=x)
xy <- data.frame(x,y)


## lm prediction intervalls are as expected:
t.lm <- lm(y~x, weight=1/x^2, dat=xy)

summary(t.lm)
## Weighted Residuals:
##      Min       1Q   Median       3Q      Max
## -2.92176 -0.50026 -0.04489  0.70317  2.31704

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) 999.1371     0.8878 1125.35   <2e-16 ***
## x             2.0171     0.1135   17.77   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 1.038 on 98 degrees of freedom
## Multiple R-squared: 0.7631,	Adjusted R-squared: 0.760


t.pr <- predict(t.lm,se.fit=T)
t.VC <- mad(resid(t.lm)/xy$x)*(n-1)/(n-2)

t.PI <- data.frame(x=c(xy$x,
                     rev(xy$x)),
                   y=c(t.pr$fit+2*sqrt((xy$x*t.VC)^2+(t.pr$se)^2),
                     rev(t.pr$fit-2*sqrt((xy$x*t.VC)^2+(t.pr$se)^2))))
windows()
plot(t.PI,type="n", main = "lm")
polygon(t.PI,col="gray")
points(xy)
abline(t.lm)
## QED

## se are a smooth function of x
windows()
plot(t.pr$se.fit, main = "lm")
## QED


##################################################################
## and now the same thing with gam

## prior weights = case weights = weights used in lm()
t.gam <- gam(y ~ s(x),
             weight=1/x^2,
             dat=xy)

summary(t.gam)
## Parametric coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) 1101.002      5.434   202.6   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Approximate significance of smooth terms:
##      edf Ref.df     F p-value
## s(x)   1      1 315.7  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## R-sq.(adj) =   0.34   Deviance explained = 76.3%
## GCV score = 1.0987  Scale est. = 1.0767    n = 100

## But multiplying weights by 100 gives a scale est.
## which is 100 times larger -> scale are resid * weights
## -> scale weights so that good observations have weight = 1

summary(gam(y ~ s(x),
            weight=100/x^2,
            dat=xy))

t.gpr <- predict(t.gam,se.fit=T)
cbind(t.pr$se.fit, t.gpr$se.fit)
## lm() and gam() are identical!!!!

t.gVC <- mad(resid(t.gam, type = "response")/xy$x)*(n-1)/(n-2)
cbind(t.VC, t.gVC)
## lm() and gam() are identical!!!

##
t.gPI <- data.frame(x=c(xy$x,
                    rev(xy$x)),
                    y=c(t.gpr$fit+2*sqrt((xy$x*t.gVC)^2+(t.gpr$se)^2),
                    rev(t.gpr$fit-2*sqrt((xy$x*t.gVC)^2+(t.gpr$se)^2))))

windows()
plot(t.gPI, type="n", main="gam")
polygon(t.gPI,col="gray")
points(xy)

windows()
plot(t.gam, residuals = TRUE, pch=1, main="gam")


## prediction interval
windows()
plot(t.gpr$se.fit, main="gam")


windows()
plot(t.pr$se.fit, col = "blue")
points(t.gpr$se.fit, col = "red")
## Lines lie on top of each other!!!
