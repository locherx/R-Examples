## -*- coding: utf-8 -*-
## examples.R
## Authors: Vasily Tolkachev, refined and extended by René Locher
## Version: 2018-08-08

rm(list = objects(pattern = ".*"))

library(MASS)
library(data.table)

## data.table(Boston)
## as.data.table(Boston)
data(Boston)
dat <- data.table(Boston)
dat

class(dat)

## data.table is also a data.frame
sapply(dat, is)

## Addressing data --------------------
## subset rows from 11 to 20
dat[11:20]
dat[11:20, ]

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
dat[tax == 216, ]

## Find all rows where tax is equal to tax of first row
dat[tax == tax[1], ]

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

## j argument is actually a function, so in this case it’s
## a function calling variable medv:
dat[, medv]

## Below it’s a function which is equal to 5
dat[, 5]

## Select several variables (result is a data.table)
dat[, list(nox, age, black)]

## Or equivalently:
dat[, .(nox, age, black)]

DT <- data.table(x = rep(c("b","a","c"), each = 3),
                 y = c(1,3,6), v = 1:9)
DT
DT[1]
DT[-c(5, 1),]
DT[2, 2]
DT[, v]
DT[, 3]
DT[, names(DT)[names(DT) == "v"], with = FALSE]

DT[, c(x, y)]
DT[, list(x, y)]
DT[, .(x, y)]


## Computing --------------------
## Find the range of crim (criminality) variable
dat[, range(crim)]

## Add a new variable with :=
dat[, rad.f := as.factor(paste0("f", rad))]
dat[, levels(rad.f)]

## i.e. we defined a new factor variable(rad.f) in the data table
## from the integer variable radius (rad), which describes accessibility to radial highways

## Add many new variables with `:=`().
## If a variable attains only a single value, copy it for each observation
dat[, `:=`(Var1 = mean(nox), Var2 = sd(age), Var3 = mad(black))]

## Compute a more complicated function for groups.
## It’s a weighted mean of house prices, with dis
## (distances to Boston employment centers) as weights
dat[, sum(medv*dis)/sum(dis), by = rad.f ]

## Computing mean of house prices for every level of rad.f
dat[, mean(medv), by = rad.f]

## Computing several variables at once
dat[, .(meanNox = mean(nox), sdAge = sd(age), madBlack = mad(black))]

## Changing a subset of observations --------------------
## Let’s create another factor variable crim.f
## with 3 levels standing for low, medium and severe crime rates per capita:

dat[          , crim.f := "low"]
dat[crim >= 1 , crim.f := "medium"]
dat[crim >= 10, crim.f := "severe"]
str(dat)
## crim.f is still a character, contrary to data.frame!

dat[          , crim.f := as.factor(crim.f)][]
str(dat)

table(dat$crim.f)

## we can also apply functions in j on two or more groups
dat[, .(mean(medv), sd(medv)), by = .(rad.f, crim.f) ]


## Aggregating and merging --------------------
dat[, .(meanNox = mean(nox), sdAge = sd(age), madBlack = mad(black)), by = rad.f]
dat[, .(meanNox = mean(nox), sdAge = sd(age), madBlack = mad(black)), by = rad]

## Combined aggregating and merging
dat[, meanNox := mean(nox), by = rad.f]
dat

## cf. with
dat[, mean(nox), by = rad.f]

## Merging
x <- data.table(k = 5:1, a = 20:24, zoo = 5:1 )
y <- data.table(k = 1:6, b = 30:35, boo = 10:15)
setkey(x, k)
setkey(y, k)

merge(x, y)
merge(x, y, all = TRUE)

## Dynamic variable creation --------------------
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

## New data.table with dynamically created variables can be created by
dat[, {list(mean_w = mean_w <- sum(medv*dis)/sum(dis),
            std_w = sqrt( sum(dis*(medv - mean_w)^2 )/sum(dis))
            )},
      by = rad.f]

## Chaining --------------------
dat[, crim.f := "low"] [crim >= 1, crim.f := "medium"]
dat[ crim >= 10, crim.f := "severe"][, crim.f := as.factor(crim.f)]
levels(dat$crim.f)

## Equivalent chaining in one command
dat[ , crim.f := "low"] [
  crim >= 1, crim.f := "medium"] [
    crim >= 10, crim.f := "severe"][,
      , crim.f := as.factor(crim.f)]


## Special functions --------------------
## .N function count the number observations in a group:
dat[, .N, by = .(rad.f, crim.f) ]

## Another useful function is .SD which contains values
## of all variables except the one used for grouping
dat[, .SD, by =  crim.f ]

## Renaming column names --------------------
names(dat)
setnames(dat, c("rm", "zn"), c("rooms_average", "proportion_zoned") )
names(dat)

## Sorting columns
setcolorder(dat, rev(names(dat)))
names(dat)

## Sorting rows --------------------
## set the key. Order is always ascending
setkey(dat, rad.f, crim.f)

## use binary search (fast, O(log(n) )
dat[.("f7", "low"), ]

dat[, .(crim, tax)]
setorder(dat, crim, tax)
dat[, .(crim, tax)]
setorder(dat, -crim, tax) ## Descending order for crim!
dat[, .(crim, tax)]

## DO NOT use vector scan (slow, O(n) )
dat[rad.f =="f7" & crim.f == "low"]

## Avoid using data.frame’s vector scan inside data.table:
dat[dat$rad.f == "f7" & dat$crim.f == "low", ]
