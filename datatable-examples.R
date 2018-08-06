## -*- coding: utf-8 -*-
## examples.R
## Authors: Vasily Tolkachev, René Locher
## Version: 2018-10-03

library(MASS)
library(data.table)

## data.table(Boston)
## as.data.table(Boston)
data(Boston)
dat <- data.table(Boston)
dat

class(dat)

## subset rows from 11 to 20
dat[11:20]

## subset rows from 11 to 20 of variable age (result is a vector, not a data.table)
dat[11:20, age]

## to get data.table after subsetting, use list() or .() in j argument
dat[11:20, list(age)]

## equivalently:
dat[11:20, .(age)]

## the usual data.frame subsetting style (with columns in j argument)
dat[11:20, 1, with = FALSE]

## Find all rows where tax variable is equal to 216
dat[tax == 216]

## Find the range of crim (criminality) variable
dat[, range(crim)]

## Display values of rad (radius) variable
dat[, table(rad)]

## you can also select all columns between some two variables
dat[, indus:age, with = TRUE]
dat[, indus:age, with = FALSE]

dat
## Without column "age"
dat[, -"age", with = TRUE]
dat[, -"age", with = FALSE]

## BUT with negative age
dat[, .(-age), with = TRUE]

## Add a new variable with :=
dat[, rad.f := as.factor(rad)]
dat[, levels(rad.f)]

## i.e. we defined a new factor variable(rad.f) in the data table
## from the integer variable radius (rad), which describes accessibility to radial highways

## Compute mean of house prices for every level of rad.f
dat[, mean(medv), by = rad.f]

## Recall that j argument is a function, so in this case it’s
## a function calling a variable medv:
dat[, medv]

## Below it’s a function which is equal to 5
dat[, 5]

## Select several variables (result is a data.table)
dat[, list(nox, age, black)]

## Or equivalently:
dat[, .(nox, age, black)]

## Compute several functions
dat[, .(mean(nox), sd(age), mad(black))]

## Compute these functions for groups (levels) of rad.f
dat[, .(mean(nox), sd(age), mad(black)), by = rad.f]

## Compute functions for every level of rad.f and return a data.table with column names
data[, .( Var1 = mean(nox), Var2 = sd(age), Var3 = mad(black) ), by = rad.f]

## Add many new variables with `:=`().
## If a variable attains only a single value, copy it for each observation
dat[, `:=`(Var1 = mean(nox), Var2 = sd(age), Var3 = mad(black))]

## Compute a more complicated function for groups.
## It’s a weighted mean of house prices, with dis
## (distances to Boston employment centers) as weights
dat[, sum(medv*dis)/sum(dis), by = rad.f ]
#=====================================================================


## Dynamic variable creation.
## Now let’s create a variable of weighted means (mean_w),
## and then use it to create a variable for weighted standard deviation (std_w).

dat[,  `:=`(mean_w = mean_w <- sum(medv*dis)/sum(dis),
            std_w = sqrt(sum(dis*(medv - mean_w)^2 )/sum(dis))),
    by = rad.f][]

## To use some variables with long names, specify them in SDcols and use SD instead:
dat[, `:=`(x = sum(.SD[[1]]^2) / sum(.SD[[1]]),
           y = sum(.SD[[2]]^2) / sum(.SD[[2]]) ),
        by = rad.f,
    .SDcols = c("medv", "age")][]

## Multiple expressions in j could be handled with { }
par(mfrow = c(1,2))
dat[, {hist(log(crim), col = "royalblue3")
       plot(rm, medv, pch = 16)
       grid()}]

## Separate data.table with dynamically created variables can be done by
dat[, {list(mean_w = mean_w <- sum(medv*dis)/sum(dis),
            std_w = sqrt( sum(dis*(medv - mean_w)^2 )/sum(dis))
            )},
      by = rad.f]
#=====================================================================

## Changing a subset of observations.
## Let’s create another factor variable crim.f
## with 3 levels standing for low, medium and severe crime rates per capita:

dat[          , crim.f := "low"]
dat[crim >= 1 , crim.f := "medium"]
dat[crim >= 10, crim.f := "severe"]
str(dat)

dat[          , crim.f := as.factor(crim.f)][]
str(dat)

table(dat$crim.f)

## Chaining
dat[, crim.f := "low"] [crim >= 1, crim.f := "medium"]
dat[ crim >= 10, crim.f := "severe"][, crim.f := as.factor(crim.f)]
levels(dat$crim.f)

## Equivalent chaining in one command
dat[ , crim.f := "low"] [
  crim >= 1, crim.f := "medium"] [
    crim >= 10, crim.f := "severe"][,
      , crim.f := as.factor(crim.f)]
#=====================================================================

## we can also apply functions in j on two groups
dat[, .(mean(medv), sd(medv)), by = .(rad.f, crim.f) ]

## .N function count the number observations in a group:
dat[, .N, by = .(rad.f, crim.f) ]

## Another useful function is .SD which contains values
## of all variables except the one used for grouping
dat[, .SD, by =  crim.f ]

## Use setnames() and setcolorder() functions to change column names or reorder them:
setnames(dat, c("rm", "zn"), c("rooms_average", "proportion_zoned") )[]

## set the key
setkey(dat, rad.f, crim.f)

## use binary search (fast, O(log(n) )
dat[ .("7", "low")]

## DO NOT use vector scan (slow, O(n) )
dat[rad.f =="7" & crim.f == "low"]

## Avoid using data.frame’s vector scan inside data.table:
dat[ dat$rad.f == "7" & dat$crim.f == "low", ]

## avoid using $ inside the data.table,
## whether it’s for subsetting, or updating some subset of the observations:
dat[ dat$rad.f == "7", ] = dat[ dat$rad.f == "7", ] + 1

## ----------------------------------------
## One more examples
DT <- data.table(x = rep(c("b","a","c"), each = 3),
                 y = c(1,3,6), v = 1:9)
DT
DT[1]
DT[2, 2]
DT[, v]
DT[, 3]
DT[, names(DT)[names(DT) == "v"], with = FALSE]

DT[, c(x, y)]