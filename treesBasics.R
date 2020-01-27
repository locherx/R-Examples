## Trees.R

library(help=rpart)
library(help=mvpart)
 
library(rpart)

## Der Faktor A bzw. B ist vollständig durch x1 bestimmt:
test <- data.frame(Fac=c(rep("A",300),rep("B",300)),
                   x1=c(rnorm(300),rnorm(300)+1),
                   x2=rnorm(600),
                   x3=rnorm(600),
                   x4=rnorm(600))

set.seed(1013)
r.00 <- rpart(Fac ~ ., data=test, method="class")
r.00

summary(r.00)

par(xpd=NA)
plot(r.00)
text(r.00, use.n=TRUE, digits=3)

## pruning by hand
r.00pint <- snip.rpart(r.00)

## Ripley p. 258
printcp(r.00)

## Vorsicht: Die geplotteten Werte entsprechen NICHT den Tabellenwerten!!
## Sind aber leider beide mit cp angeschreiben.
plotcp(r.00)
cp0 <- r.00$cptable[,1]
sqrt(cp0*c(Inf, cp0[-length(cp0)]))

## für das Stutzen sind die Werte im Plot entscheidend
r.00p <- prune(r.00, cp=0.065)
par(xpd=NA)
plot(r.00p)
text(r.00p, use.n=TRUE, digits=3)

table(test$Fac,predict(r.00p,type="class"))
## test-Werte = Zeilen
## detach(package:rpart)

##----------------------------------------

library(help=randomForest)
library(randomForest)

r.10 <- randomForest(Fac ~ ., data=test, 
                     importance = TRUE, proximity = TRUE,
                     do.trace = 50)

r.10
## Hier ist ist der einfache, richtig gestutzte Baum
## leicht besser als random forest!
