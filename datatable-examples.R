## -*- coding: utf-8 -*-
## examples.R
## Author: René Locher on the basis of a script of Vasily Tolkachev
## Version: 2018-02-13

## See also http://datatable.r-forge.r-project.org/datatable-faq.pdf

rm(list = objects(pattern = ".*"))

library(MASS)
library(data.table)

## data.table(Boston)
## as.data.table(Boston)
data(Boston)
dtbl1 <- data.table(Boston)
dtbl1

class(dtbl1)

## data.table is also a data.frame
sapply(dtbl1, is)

## Addressing data --------------------
## subset rows from 11 to 20
dtbl1[11:20]
dtbl1[11:20, ]

## subset rows from 11 to 20 of variable age (result is a vector, not a dtbl1a.table)
dtbl1[11:20, age]

## to get data.table after subsetting, use list() or .() in j argument
dtbl1[11:20, list(age)]
dtbl1[11:20, .(age)]

## the usual data.frame subsetting style (with columns in j argument)
dtbl1[11:20, 1]

## Find all rows where tax variable is equal to 216
dtbl1[tax == 216]
dtbl1[tax == 216, ]

## Find all rows where tax is equal to tax of first row
dtbl1[tax == tax[1], ]

## Display values of rad (radius) variable
dtbl1[, table(rad)]

## you can also select all columns between some two variables
dtbl1[, indus:age, with = TRUE]
dtbl1[, indus:age, with = FALSE]

dtbl1
## Without column "age"
dtbl1[, -"age", with = TRUE]
dtbl1[, -"age", with = FALSE]

## BUT with negative age
dtbl1[, .(-age), with = TRUE]

## j argument is actually a function, so in this case it’s
## a function calling variable medv:
dtbl1[, medv]

## Below it’s a function which is equal to 5
dtbl1[, 5]

## Select several variables (result is a data.table)
dtbl1[, list(nox, age, black)]

## Or equivalently:
dtbl1[, .(nox, age, black)]

dtbl2 <- data.table(x = rep(c("b","a","c"), each = 3),
                    y = c(1,3,6), v = 1:9)
str(dtbl2)
dtbl2

dtbl2[1]
dtbl2[-c(5, 1),]
dtbl2[2, 2]

## Returns a vector
dtbl2[, v]
dtbl2[["v"]]
unlist(dtbl2[, 3])

dtbl2[, c(x, y)]

## Returns a data.table
dtbl2[, "v", with = FALSE]
dtbl2[, .SD, .SDcols = "v"] ## identical to line above
dtbl2[, 3]

dtbl2[, list(x, y)]
dtbl2[, .(x, y)]

## Template of data.table dtbl2
str(dtbl2[0])

## Computing --------------------
## Find the range of crim (criminality) variable
dtbl1[, range(crim)]

## Add a new variable with :=
dtbl1[, rad.f := as.factor(paste0("f", rad))]
dtbl1[, levels(rad.f)]
str(dtbl1)

## i.e. we defined a new factor variable(rad.f) in the data table
## from the integer variable radius (rad), which describes accessibility to radial highways

## Add many new variables with `:=`().
## If a variable attains only a single value, copy it for each observation
dtbl1[, `:=`(Var1 = mean(nox), Var2 = sd(age), Var3 = mad(black))]

## Compute a more complicated function for groups.
## It’s a weighted mean of house prices, with dis
## (distances to Boston employment centers) as weights
dtbl1[, sum(medv*dis)/sum(dis), by = rad.f ]

## Computing mean of house prices for every level of rad.f
dtbl1[, mean(medv), by = rad.f]

## Computing several variables at once
dtbl1[, .(meanNox = mean(nox), sdAge = sd(age), madBlack = mad(black))]

## Changing a subset of observations --------------------
## Let’s create another factor variable crim.f
## with 3 levels standing for low, medium and severe crime rates per capita:

dtbl1[          , crim.f := "low"]
dtbl1[crim >= 1 , crim.f := "medium"]
dtbl1[crim >= 10, crim.f := "severe"]
str(dtbl1)
## crim.f is still a character, contrary to data.frame!

dtbl1[, crim.f := as.factor(crim.f)][]
str(dtbl1)

table(dtbl1$crim.f)

## we can also apply functions in j on two or more groups
dtbl1[, .(mean(medv), sd(medv)), by = .(rad.f, crim.f) ]


## Aggregating and merging --------------------

## Aggregating
dtbl1[, .(meanNox = mean(nox), sdAge = sd(age), madBlack = mad(black)), by = rad.f]
dtbl1[, .(meanNox = mean(nox), sdAge = sd(age), madBlack = mad(black)), by = rad]

## Aggregating and merging results in original data.table
dtbl1[, meanNox := mean(nox), by = rad.f]
dtbl1

## cf. with
dtbl1[, mean(nox), by = rad.f]

## Merging
(x <- data.table(k = 5:0, a = 20:25, zoo = 5:0 ))
(y <- data.table(l = 1:6, b = 30:35, boo = 10:15))
setkey(x, k)
setkey(y, l)
## on = is not necessary when keys are set
## BUT makes code more transparent

merge(x, y, by.x = "k", by.y = "l", all.x = TRUE)
merge(x, y, by.x = "k", by.y = "l", all.y = TRUE)

## This is faster and more efficient!! See FAQ
x[y, on = c(k="l")]  ## Take all rows of y and join with corresponding rows of x
y[x, on = c(l="k")]  ## Take all rows of x and join with corresponding rows of y

## This join is rarely used and only available in merge()
merge(x, y, by.x = "k", by.y = "l", all = TRUE)

## Dynamic variable creation --------------------
## Now let’s create a variable of weighted means (mean_w),
## and then use it to create a variable for weighted standard deviation (std_w).

dtbl1[,  `:=`(meanW = meanW <- sum(medv*dis)/sum(dis),
              stdW = sqrt(sum(dis*(medv - meanW)^2 )/sum(dis))),
      by = rad.f][]

## To use some variables with long names, specify them in .SDcols and use .SD instead:
dtbl1[, `:=`(x = sum(.SD[[1]]^2) / sum(.SD[[1]]),
             y = sum(.SD[[2]]^2) / sum(.SD[[2]]) ),
      by = rad.f,
      .SDcols = c("medv", "age")][]

## Multiple expressions in j could be handled with { }
par(mfrow = c(1,2))
dtbl1[, {hist(log(crim), col = "royalblue3")
         plot(rm, medv, pch = 16)
         grid()}]

##???
dtbl1[, lapply(.SD, hist), .SDcols = c("crim", "age")]
dtbl1[, lapply(seq(.SD), function(i, x) hist(x[[i]], .SD)), .SDcols = c("crim", "age")]

## New data.table with dynamically created variables can be created by
dtbl1[, {list(meanW = meanW <- sum(medv*dis)/sum(dis),
              stdW = sqrt( sum(dis*(medv - meanW)^2 )/sum(dis))
              )},
      by = rad.f]

## Chaining --------------------
dtbl1[, crim.f := "low"] [crim >= 1, crim.f := "medium"]
dtbl1[ crim >= 10, crim.f := "severe"][, crim.f := as.factor(crim.f)]
levels(dtbl1$crim.f)

## Equivalent chaining in one command
dtbl1[ , crim.f := "low"] [
                    crim >= 1, crim.f := "medium"] [
                                          crim >= 10, crim.f := "severe"][,
                                                               , crim.f := as.factor(crim.f)]

## Renaming column names --------------------
names(dtbl1)
setnames(dtbl1, c("rm", "zn"), c("rooms_average", "proportion_zoned") )
names(dtbl1)

## Sorting columns
setcolorder(dtbl1, rev(names(dtbl1)))
names(dtbl1)

## Sorting rows --------------------
## set the key. Order is always ascending
setkey(dtbl1, rad.f, crim.f)

## use binary search (fast, O(log(n) )
dtbl1[.("f7", "low"), .(rad.f, crim.f, tax)]

dtbl1[, .(crim, tax)]
setorder(dtbl1, crim, tax)
dtbl1[, .(crim, tax)]
setorder(dtbl1, -crim, tax) ## Descending order for crim!
dtbl1[, .(crim, tax)]

## DO NOT use vector scan (slow, O(n) )
dtbl1[rad.f =="f7" & crim.f == "low"]

## Avoid using data.frame’s vector scan inside data.table:
dtbl1[dtbl1$rad.f == "f7" & dtbl1$crim.f == "low", ]


## Special functions --------------------
## .N function count the number observations in a group:
dtbl1[, .N, by = .(rad.f, crim.f)]
dtbl1[, .N]

## Another useful function is .SD which contains values
## of all variables except the one used for grouping
dtbl1[, .SD[1], by =  crim.f]
dtbl1[, lapply(.SD, mean), by =  crim.f, .SDcols = tax:age]
dtbl1[, lapply(.SD, mean), by =  crim.f, .SDcols = c("tax", "age")]

## Group counter .GRP
dtbl1[, .(.I, .GRP, .N), by =  crim.f]

## Runtime Length ID
dtab <- data.table(bit = sample(0:1, 20, replace = TRUE),
                   L = sample(LETTERS[1:3], 20, replace = TRUE))
dtab[, .SD]
rleid(dtab$bit)
dtab[, bit, by = rleid(bit)]
dtab[, bit, by = rleid(L)]
setkey(dtab, bit)
dtab[, bit, by = rleid(bit)]

## data.table within functions --------------------
testScope0 <-
    function(dtbl){
        dtbl[, bit2 := bit*0.5]
        return(dtbl)
    }

dtab1 <- testScope0(dtab)
identical(dtab, dtab1)
## dtab AND dtab1 are identical!!!
## -> data.table are passed by reference (exception!!)
dtab1
rm(dtab1)

testScope1 <-
    function(dtbl){
        dtbl.local <- dtbl
        dtbl.local[, bit2 := bit*2]
        return(dtbl.local)
    }

dtab1 <- testScope1(dtab)

dtbl.local ## error, QED
dtab1
dtab
identical(dtab, dtab1)
## dtab AND dtab1 are identical!!!

## data.tables are passed by reference and not by copy!!!
## Solution: Copy data.table within function first!
testScope2 <-
    function(dtbl){
        dtbl.local <- copy(dtbl)
        dtbl.local[, bit2 := bit*3]
        return(dtbl.local)
    }

dtab <- data.table(bit = sample(0:1, 10, replace = TRUE),
                   L = sample(LETTERS[1:3], 10, replace = TRUE))
dtab2 <- testScope2(dtab)
dtbl.local ## error, QED
identical(dtab, dtab2)
## [1] FALSE, QED

## Adressing variables dynamically within a function ----------
testDynProg1 <-
    function(dtab, nam){
        return(dtab[, nam, with = FALSE])
    }

testDynProg1(dtab, "bit")  ## ok
testDynProg1(dtab, .(bit)) ## error

testDynProg2 <-
    function(dtab, expr){
        e <- substitute(expr)
        return(dtab[, eval(e)])
    }

testDynProg2(dtab, "bit")  ## error
testDynProg2(dtab, .(bit)) ## ok
testDynProg2(dtab, bit)    ## ok

testDynProg3 <-
    function(dtab, nam){
        nam <- parse(text = nam)
        return(dtab[, eval(nam)])
    }

testDynProg3(dtab, "bit")  ## ok
testDynProg3(dtab, bit)    ## error

## Creating dynamically a new variable ----------
testDynProg4 <-
    function(dtab, jText){
        j <- parse(text = jText)
        return(dtab[, eval(j)])
    }

testDynProg4(dtab, "bit3 := 3*bit")  ## ok!
dtab

## Differences between data.table and data.frame ----------------------------------------
dframe <- data.frame(x = c(2, NA, 3), y = 1:3, z = letters[1:3])
(dtable <- data.table(x = c(2, NA, 3), y = 1:3, z = letters[1:3]))

str(dframe)            ## z is a factor!
str(dtable)            ## z is a character string!

dframe[dframe$x > 2, ] ## NA results in a row of NAs
dtable[x > 2, ]        ## NA is equivalent to FALSE

dframe[, "x"]
dtable[, x]

dframe[, c("x", "y")]
dtable[, c("x", "y")]
dtable[, c("x", "y"), with = FALSE]
dtable[, c(x, y)]      ## Just a vector!
dtable[, .(x, y)]
dtable[, x:y]

dtable[, c(FALSE, TRUE, TRUE)]
dtable[, c(FALSE, TRUE, TRUE), with = FALSE]
dtable[is.na(x), x := 99]
