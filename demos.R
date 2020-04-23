## -*- coding: utf-8 -*-
## demos.R
## Author: René Locher
## Version: 2020-04-22

vect <-
    round(runif(10, 0, 100))
names(vect) <- letters[seq(along.with = vect)]
vect

mat <-
    matrix(round(runif(50, 0, 100), 1), ncol = 5)
rownames(mat) <- letters[1:nrow(mat)]
colnames(mat) <- LETTERS[1:ncol(mat)]
mat

vectBool <-
    sample(c(TRUE, FALSE), size = 10, replace = TRUE)
names(vectBool) <- letters[seq(along.with = vectBool)]
vectBool

