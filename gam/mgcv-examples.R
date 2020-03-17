###########################################################################
## mgcv.R
## Examples to cubic splines
## Author: Simon Wood, enriched by Rene Locher
## Version 2018-03-16
options(help_type = "html", width = 200)

require(mgcv)
?gam

## To define smoothness manually:
## s(evar, bs = "cr", k = "number of knots", sp = "smoothing parameter")
## k must be chosen large enough
## sp depends on k (see below)
## http://127.0.0.1:28268/library/mgcv/html/s.html
?s         ## for individual smoothing
?choose.k  ## for definition of s(term, k=)
?smooth.construct.cr.smooth.spec ## for cyclic cubic spline s(sp="cc",...)

## ----------------------------------------
set.seed(0)
## fake some data for demonstration of smoothing
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

## fitting
gam00 <- gam(y ~ x0 + s(x1) + s(x2) + s(x3), dat = dat.02)

## Partial residual plots
?plot.gam
x11()
plot(gam00, pages = 1, residuals = TRUE, all.terms = TRUE, shade = TRUE, shade.col = 2)

## better coverage intervals
x11()
plot(gam00, pages = 1, residuals = TRUE, all.terms = TRUE, seWithMean = TRUE)
mtext(text = "Thin plate: s(x2)", line = 2)

## Parametric term only
x11()
termplot(gam00, terms = "x0", se = TRUE)

par(mfrow = c(2, 2))
ylim <- c(-9, 12)
x <- 0:1000/1000
for (i in 1:3) {
    plot(gam00, select = i, rug = FALSE, se = TRUE, residuals = TRUE,
         ylim = ylim, cex = 1, pch = 21)
    eval(parse(text = paste0("fx <- f", i, "(x)")))
    fx <- fx - mean(fx)
    lines(x, fx, col = "red") ## overlay `truth'
}
termplot(gam00, terms = "x0", rug = FALSE, se = TRUE, residuals = TRUE,
         ylim = ylim, cex = 1, pch = 21)
mtext(line = -2, "Fit including true values in red", outer = TRUE)

gam01 <- gam(y ~ x0 + s(x1, bs = "cs") + s(x2, bs = "cs") + s(x3, bs = "cs"), dat = dat.02)
x11()
plot(gam01, pages = 1, residuals = TRUE, all.terms = TRUE,
     seWithMean = TRUE, ylim = ylim, cex = 1, pch = 21)
mtext(line = -2, 'Cubic spline regression with shrinking: s(x2, bs = "cs")', outer = TRUE)
## First 3 figures are identical, ylim ignored for parametric terms

summary(gam00)
summary(gam01)
## Compare degrees of freedoms (edf) of smooth terms
##             edf Ref.df     F p-value
## s(x1) 1.852e+00      9 10.55  <2e-16 ***
## s(x2) 6.881e+00      9 44.78  <2e-16 ***
## s(x3) 5.626e-07      9  0.00   0.892

## Maximum number of knots k too small
gam02 <- gam(y ~ x0 + s(x1, bs = "cs") + s(x2, bs = "cs", k = 2) + s(x3, bs = "cs"),
             dat = dat.02)
x11()
plot(gam02, pages = 1, residuals = TRUE, all.terms = TRUE,
     seWithMean = TRUE, ylim = ylim, cex = 1, pch = 21)
mtext(line = -2, 'k too small: s(x2, bs = "cs", k = 2)', outer = TRUE)
summary(gam02)
## edf(x2) = 1.9 ~ 2
## k is for sure too small

## k adequate
gam03 <- gam(y ~ x0 + s(x1, bs = "cs") + s(x2, bs = "cs", k = 8) + s(x3, bs = "cs"),
             dat = dat.02)
x11()
plot(gam03, pages = 1, residuals = TRUE, all.terms = TRUE,
     seWithMean = TRUE, ylim = ylim, cex = 1, pch = 21)
mtext(line = -2, 'k adequate: s(x2, bs = "cs", k = 8)', outer = TRUE)
summary(gam03)
## edf(x2) = 6.4 <~ 8

## k = 20 is not much different from k = 8
gam04 <- gam(y ~ x0 + s(x1, bs = "cs") + s(x2, bs = "cs", k = 20) + s(x3, bs = "cs"),
             dat = dat.02)
x11()
plot(gam04, pages = 1, residuals = TRUE, all.terms = TRUE,
     seWithMean = TRUE, ylim = ylim, cex = 1, pch = 21)
mtext(line = -2, 'k = 8 and k = 20 are adequate: s(x2, bs = "cs", k = 20)', outer = TRUE)
summary(gam04)
## edf(x2) = 7.8 < 20
## k is big enough for sure

## Smoothing parameter too strong
gam05 <- gam(y ~ x0 + s(x1, bs = "cs") + s(x2, bs = "cs", k = 20, sp = 1) +
                 s(x3, bs = "cs"), dat = dat.02)
x11()
plot(gam05, pages = 1, residuals = TRUE, all.terms = TRUE,
     seWithMean = TRUE, ylim = ylim, cex = 1, pch = 21)
mtext(line = -2, 'Smoothing parameter too weak: s(x2, bs = "cs", k = 20, sp = 1)', outer = TRUE)
summary(gam05)
## edf(x2) = 17.9 < 20
## too shaky

gam06 <- gam(y ~ x0 + s(x1, bs = "cs") + s(x2, bs = "cs", k = 20, sp = 10) + s(x3, bs = "cs"), dat = dat.02)
x11()
plot(gam06, pages = 1, residuals = TRUE, all.terms = TRUE,
     seWithMean = TRUE, ylim = ylim, cex = 1, pch = 21)
mtext(line = -2, 'Smoothing parameter too weak: s(x2, bs = "cs", k = 20, sp = 10)', outer = TRUE)
summary(gam06)
## still too shaky

gam07 <- gam(y ~ x0 + s(x1, bs = "cs") + s(x2, bs = "cs", k = 20, sp = 100) + s(x3, bs = "cs"), dat = dat.02)
x11()
plot(gam07, pages = 1, residuals = TRUE, all.terms = TRUE,
     seWithMean = TRUE, ylim = ylim, cex = 1, pch = 21)
mtext(line = -2, 'Smoothing parameter ok: s(x2, bs = "cs", k = 20, sp = 100)', outer = TRUE)
summary(gam07)
## Smoothing parameter ok

gam09 <- gam(y ~ x0 + s(x1, bs = "cs") + s(x2, bs = "cs", k = 40, sp = 100) + s(x3, bs = "cs"), dat = dat.02)
x11()
plot(gam09, pages = 1, residuals = TRUE, all.terms = TRUE,
     seWithMean = TRUE, ylim = ylim, cex = 1, pch = 21)
mtext(line = -2, 'Sp  depends on k: s(x2, bs = "cs", k = 40, sp = 100)', outer = TRUE)
summary(gam09)
## again too shaky -> sp depends from k!

gam10 <- gam(y ~ x0 + s(x1, bs = "cs") + s(x2, bs = "cs", k = 20, sp = 1000) + s(x3, bs = "cs"), dat = dat.02)
x11()
plot(gam10, pages = 1, residuals = TRUE, all.terms = TRUE,
     seWithMean = TRUE, ylim = ylim, cex = 1, pch = 21)
mtext(line = -2, 'Too smooth: s(x2, bs = "cs", k = 20, sp = 1000)', outer = TRUE)
summary(gam10)
## Too smooth

gam11 <- gam(y ~ x0 + s(x1, bs = "cs") + s(x2, bs = "cs") + s(x3, bs = "cs"),
           dat = dat.02, gamma = 10)
x11()
plot(gam11, pages = 1, residuals = TRUE, all.terms = TRUE,
     seWithMean = TRUE, ylim = ylim, cex = 1, pch = 21)
mtext(line = 2, "gamma = 10 does not have the effect intended")
summary(gam11)


gam12 <- gam(y ~ x0 + s(x1, bs = "cs") + s(x2, bs = "cs"),
           dat = dat.02, gamma = 10)
x11()
plot(gam12, pages = 1, residuals = TRUE, all.terms = TRUE,
     seWithMean = TRUE, ylim = ylim, cex = 1, pch = 21)
mtext(line = 2, "gamma = 10, without x3 is now really smooth!!", cex = 2)
summary(gam12)
## Extremely smooth

## detach(package:mgcv)

## ----------------------------------------

f.scale1 <- function(dat, ## data used
                     res, ## corresponding linear model
                     df.s ## df of s term
                    ) {
    n <- nrow(dat) ## number of observations
    sqrt(var(resid(res, type="response"))*(n+1)/(n-1-df.s))
}

f.scale2 <- function(dat, ## data used
                     res, ## corresponding gam model
                     df.s ## df of s term
                     ) {
    n <- nrow(dat) ## number of observations
    nL <- length(unique(dat$code))+10 ## number of df for factor levels including intercept and months of year
    sqrt(var(resid(res, type="response"))*(n+1)/(n-nL-df.s))
}

## test data with constant variance
n <- 100
x <- (1:n) ##/10  ## changing the denominator does not change things too much
y <- 1000 + 2*x + rnorm(n, sd = 1)
dat.00 <- data.frame(x, y)


## lm prediction intervalls are as expected:
lm.00 <- lm(y~x, dat=dat.00)

summary(lm.00)
## Call:
## lm(formula = y ~ x, data = dat.00)

## Residuals:
##      Min       1Q   Median       3Q      Max
## -2.07057 -0.62450 -0.08753  0.59826  2.13054

## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept) 999.90307    0.18821 5312.81   <2e-16 ***
## x             2.00633    0.03236   62.01   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 0.934 on 98 degrees of freedom
## Multiple R-squared: 0.9751,	Adjusted R-squared: 0.9749
## F-statistic:  3845 on 1 and 98 DF,  p-value: < 2.2e-16

## Residuals:
##      Min       1Q   Median       3Q      Max
## -2.07057 -0.62450 -0.08753  0.59826  2.13054

## Residual standard error: 0.934 is almost 1, QED!!!

f.scale1(dat.00, lm.00, 1) ## 0.9433692


##################################################################
## and now the same thing with gam

## prior weights = case weights = weights used in lm()
gam.00 <- gam(y ~ s(x), dat = dat.00)

summary(gam.00)
## Family: gaussian
## Link function: identity

## Formula:
## y ~ s(x)

## Parametric coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept) 1.010e+03  9.298e-02   10863   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Approximate significance of smooth terms:
##        edf Ref.df    F p-value
## s(x) 1.618  2.008 1932  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## R-sq.(adj) =  0.975   Deviance explained = 97.6%
## GCV score = 0.8878  Scale est. = 0.86455   n = 100

plot(gam.00, residuals = TRUE)

f.scale1(dat.00, gam.00, 4.15)
## 0.9410424: Difference to gam.00 bigger than in lm, but still ok


## ----------------------------------------
## ----------------------------------------
## ----------------------------------------
## real data

## cf. NH3-map.05.R
## load(file="radiello2.rda")
## temp <- table(radiello2$code)
## t.sel <- names(temp[temp>200])
## dat.01 <- radiello2[radiello2$code %in% t.sel, c("lNH3m","code","t.y","m")]
## save(file="dat_01.rda",dat.01)

load(file="dat_01.rda")
str(dat.01)

gam.01 <- gam(lNH3m ~ code + m + s(t.y), data=dat.01)

summary(gam.01)
## Approximate significance of smooth terms:
##          edf Ref.df    F  p-value
## s(t.y) 8.809   8.99 8.74 4.65e-13 ***

## R-sq.(adj) =  0.675   Deviance explained = 67.8%
## GCV score = 0.26856  Scale est. = 0.26556   n = 3109

## same results with model without m, QED!

f.scale2(dat.01, gam.01, 8.997)
##  0.5154192 far too big???!!!
## Hypothesis für difference:
## estimation of scale in gam is not directly based on response residuals

par(mfrow=c(2,2))
plot(gam.01, all.terms = TRUE, ylim = c(-1, 1), resid = TRUE)

gam.01.cr <- gam(lNH3m ~ code + m + s(t.y, bs = "cr"), data = dat.01)

summary(gam.01.cr)
## Approximate significance of smooth terms:
##         edf Ref.df     F  p-value
## s(t.y) 8.71  8.975 7.694 3.19e-11 ***

## R-sq.(adj) =  0.674   Deviance explained = 67.7%
## GCV score = 0.26936  Scale est. = 0.26635   n = 3109

f.scale2(dat.01, gam.01.cr, 8.996)
## 0.5137219


