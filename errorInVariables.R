## ErrorInVariables.R
## Demonstration of effect of error in variables

x <- rnorm(1000,sample(1:50,1000,replace=TRUE)) ## wahre Werte
x1 <- x+rnorm(1000,0,10)  ## mit kleinem Beobachtungsfehler
x2 <- x+rnorm(1000,0,20)  ## mit grossem Beobachtungsfehler

y <- rnorm(1000,sample(1:50,1000,replace=TRUE)) ## wahre Werte
Y <- x+rnorm(1000,0,20)   ## wahre Werte, abhängig von x

z <- x+2*y+rnorm(1000,0,20)  ## Zielvariable mit Beobachtungsfehler
Z <- x+2*Y+rnorm(1000,0,20)  ## Zielvariable mit Beobachtungsfehler

cor(x,y)
cor(x,Y)
## 0.5787028

sd(x)
## 14.31921

plot(x,y)
scatter.smooth(x,x1)
plot(x,z)
plot(y,z)
plot(Y,Z)
plot(x,Z)
dat <- data.frame(x,x1,x2,
                  y,Y,
                  z, Z)

summary(lm(z~x+y,dat))
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.03172    1.70241  -0.019    0.985
## x            0.98837    0.04426  22.330   <2e-16 ***
## y            2.02773    0.04344  46.682   <2e-16 ***
## Residual standard error: 20.03 on 997 degrees of freedom


## Beobachtungsfehler auch in x-Variable:
## ergibt geringere Steigung und signifikanten Offset!!
summary(lm(z~x1+y,dat))
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  8.58418    1.71875   4.994 6.96e-07 ***
## x1           0.64740    0.03967  16.318  < 2e-16 ***
## y            2.04843    0.04725  43.355  < 2e-16 ***
## Residual standard error: 21.79 on 997 degrees of freedom

## Grösserer Beobachtungsfehler in x-Variable
## ergibt noch geringere Steigung und noch signifikanteren Offset!!
summary(lm(z~x2+y,dat))
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) 16.48993    1.67737   9.831   <2e-16 ***
## x2           0.32650    0.03007  10.860   <2e-16 ***
## y            2.04825    0.05029  40.726   <2e-16 ***
## Residual standard error: 23.19 on 997 degrees of freedom


## ----------------------------------------
## Y korreliert mit x

## x ohne Beobachtungsfehler:
## Steigung ist wie im nicht korrelierten Fall
summary(lm(Z~x+Y,dat))
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  1.65539    1.31332    1.26    0.208
## x            0.93868    0.05477   17.14   <2e-16 ***
## Y            1.98907    0.03356   59.28   <2e-16 ***
## Residual standard error: 20.22 on 997 degrees of freedom

## x1 mit Beobachtungsfehler:
## Steigung von x1 deutlich kleiner als 1,
## Steigung von Y etwas grösser als 2
summary(lm(Z~x1+Y,dat))
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  8.01657    1.22450   6.547  9.4e-11 ***
## x1           0.56581    0.04360  12.978  < 2e-16 ***
## Y            2.12881    0.03242  65.673  < 2e-16 ***
## Residual standard error: 21.27 on 997 degrees of freedom

## x1 mit grossem Beobachtungsfehler:
## Steigung von x2 noch kleiner als bei x1,
## Steigung von Y deutlich grösser als 2
summary(lm(Z~x2+Y,dat))
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) 13.62944    1.16099  11.740  < 2e-16 ***
## x2           0.22691    0.03106   7.305 5.66e-13 ***
## Y            2.23793    0.03244  68.987  < 2e-16 ***
## Residual standard error: 22.41 on 997 degrees of freedom
