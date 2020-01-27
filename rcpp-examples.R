## -*- coding: utf-8 -*-
## rcpp-examples.R
## Author Rene Locher, ZHAW
## Version 2019-11-12

## First steps with Rccp

## Definitions ----------------------------------------
rm(list = objects(pattern = ".*"))

## options(width = 80, error = NULL)  ## Laptop
## options(width = 160, error = NULL) ## Rychi
options(width = 250, error = NULL)

## Definitions ----------------------------------------

## Standard directory structure for all IDP R scripts
root <- paste0(getwd(), "/")
rootData <- paste0(root, "data/")
pathIn <- paste0(rootData, "in/")     ## Path for data to be read into R
pathOut <- paste0(rootData, "out/")   ## Path for Output produced by R
pathSrc <- paste0(root, "sources/")   ## Path for Output produced by R
pathCpp <- paste0(root, "rcpp/")      ## Path for Output produced by R

## Loading packages
require(Rcpp)


## Sourcing
sourceCpp( paste0(pathCpp, "add.cpp"))

addInt(1L, 2L)
addInt(1.5, 2.5)

str(addInt(1, 2))

addNum(1.5, 2.1)
