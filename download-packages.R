## download-packages.R
## Author: Rene Locher, rene.locher@zhaw.ch
## Version 2020-02-18

## -------
## in emacs automatisierbar:
## 1. neues R herunterladen und installieren
## 2. in C:\Linux\emacs\site-lisp\site-start.el Zeilen
## (setq-default inferior-R-program-name
## 	      "C:/Program Files/R/R-2.13.0/bin/x64/Rterm.exe")
## anpassen

chooseCRANmirror()
chooseBioCmirror()

## CRAN und CRANextra
local({r <- getOption("repos")
      r["CRAN"] <- "http://stat.ethz.ch/CRAN/"
      options(repos=r)})
repos1 <- options()$repos

## übrige interessante repositories
## check first http://www.bioconductor.org/ for new link!
repos2 <- c("http://www.omegahat.org/R/",
            "http://www.bioconductor.org/packages/2.10/bioc/",
            "http://www.bioconductor.org/packages/2.10/extra/",
            "http://www.rforge.net/")
names(repos2) <- c("Omegahat","BioCsoft","BioCextra","RforgeNet")

## bin/windows/contrib/2.13/, wird mit contrib.url automatisch an Pfad angehängt

## repos <- repos2[c(2,3)]

repos <- c(repos1, repos2)

## weitere repositories R-Forge (= bioC & bioCextra),

path <- "F:/Software/R/R3.3.1"

for (ii in 1:length(repos)) {
    destdir <- paste(path,names(repos)[ii],sep="")
    error <- FALSE
    if (!file.exists(destdir)) error <- !dir.create(destdir)
    print(repos[ii])
    if (error) {
        print("Fehler beim Erstellen des Verzeichnisses! Kopierprozess abgebrochen.")
        break
    }
    download.packages(pkgs=available.packages(contriburl =
                      contrib.url(repos=repos[ii]))[,"Package"],
                      contriburl = contrib.url(repos=repos)[ii],
                      destdir = destdir)
}

## Installed packages
packInst <- installed.packages()
str(packInst)
head(packInst)

rownames(packInst)

##-----------------------
## in R-CMD-Windows starten (läuft nicht in emacs)
## nicht zu empfehlen, da immer wieder Packages wegen Netzproblemen
## nicht vollständig heruntergeladen werden.
## Falls sich aber obige Pfadstruktur ändert, kann damit herausgefunden werden, wie diese
## neu heissen.
setRepositories()  ## alle auswählen, welche heruntergeladen werden sollen
## ohne BioC annotation, BioC experiment (sehr gross und nur Daten!)
## und ohne R-Forge (nur Entwicklungsversionen)
chooseCRANmirror() ## Switzerland auswählen



##-------------------------
source("http://bioconductor.org/biocLite.R")
biocLite("PROcess")

options(width=150)
getCRANmirrors()

## when starting code of this script, no packages should be loaded

## It should go more elegantly, but I do not have found out how!
## for urls of repositories see: ...\R\etc\repositories
pkgs <- available.packages(contriburl = "http://stat.ethz.ch/CRAN/bin/windows/contrib/2.7/")
head(pkgs[,1:3])
pkgs[6:7,]

##
## for (url in URL) {
##   download.packages(available.packages(contriburl = url)[,1],
##                     contriburl = url,
##                     destdir = "D:/temp/R")
## }
##


## Installs from CRAN
install.packages(pkgs=c("IDPmisc"),
                 repos = repos["CRAN"], dependencies = TRUE)

## ----------------------------------------
## ----------------------------------------
##install my favorite R packages
favPack <-
    c("akima",        ## 2- and 3D-Interpolation
      ##"car",
      "roxygen2",     ## for documentation purposes, ## sudo apt install libcurl4-openssl-dev
      "IDPmisc",
      "SwissAir",
      "sfsmisc",
      "microbenchmark",
      "ellipse",
      ##"gbm",    ## von Statistiker implementiertes Boosting
      "xgboost",
      "locpol",   ## Splines
      "pspline",  ## Splines
      "numDeriv",
      "gridBase",
      "gridDebug",
      "gridExtra",
      ## "lattice",   ## contained in base
      "latticeExtra",
      ## "histogram",    ## better histograms
      "pheatmap",     ## better heatmaps
      "RColorBrewer", ## referenced in IDPmisc
      "directlabels", ## setting lables on lattice plots automagically
      "gplots",       ## textplot = printing within figures, enhanced Heatmap,
      "ggplot2",      ## Hadley
      ## "scales",    ## Hadley package for pretty scales
      ## "ggplus",    ## Lattice-Funtionalität mit ggplot, erst auf Github
      ## "xtable",    ## writing latex tables (late updater)
      ## "huxtable",  ## Creating LaTeX and HTML tables, with a friendly, modern interface.
      "dplyr",        ## Hadley-Tools for splitting, applying and combining data
      "tidyr",        ## Hadley-Reshape
      ## "reshape2",  ##
      ## "doBy",      ## Groupwise summary statistics, lapplyBy: Formula based version of lapply, recodeVar: Recode values of a vector
      "Matrix",
      ## "expm",      ## matrix to power
      "mgcv",         ## gam
      "gam",
      "lme4",         ## mixed effects
      "nlme",         ## (late updater)
      "robustbase",   ## robust regressions
      "glmnet",       ## more modern, maintained lasso
      ## "lars",      ## lasso used by rkst
      "sp",           ## spatial statistics
      "gstat",        ## for RAIRMO
      "maps",
      "maptools",
      "rgeos",
      "Rcpp",
      "rmarkdown",
      ## "RODBC",
      ## "RODBCext",
      ## "raster",     ## support for very large files
      ## "gpclib",     ## must be compiled before installing
      ## "maptree",    ## Graphing, pruning, and mapping models from hierarchical clustering, classification and regression trees.
      ## "partitions",   ## Constructing compositional data
      ## "ade4",         ## Among others: Ternary plots
      ## "vcd",          ## Among others: Ternary plots
      ## "seriation",    ## PC. Clustering, Classification
      ## "mclust",          ##
      ## "clue",         ## Clustering, Classification
      ## "clusterSim",
      ## "fpc",          ## DBSCAN, Density-Based Spatial Clustering of Applications with Noise
      ## "mda",
      ## "pvclust",
      "compositions", ## Among others: Ternary plots, ternary pairs plot
      "tclust",
      "randomForest",
      "randomForestSRC",## For survival, regression and classification. Parallel processing!
      "ranger",         ## !!Fastest random forest: https://www.jstatsoft.org/article/view/v077i01!!
      ## "C50",            ## C50 Algorithm where only (n-1) splits are made for a factor with n levels
      "tcltk2",         ##
      "foreign",        ## Read Data Stored by Minitab, S, SAS, SPSS, Stata,Systat, dBase, ...
      "memisc",         ## Reading SPSS and Stata Survey Data Sets
      "data.table",     ## Hadley for handling big data (CSV files > 1GB)
      ## "reader",      ## simplify reading data from files
      "timeDate",       ## Holidays and other calendar tools
      ## "dynlm"        ## Dynamic Linear Regression V0.3-3 for SARIMA only
      ## "dlm",
      ## "xml2",        ## Prognolite, called by "httr"
      "httr",           ## Supports http(s) communication. Prognolite
      ## "ROAuth",      ## Prognolite, called by "httr"
      "RJSONIO",        ## Reading and writing json format
      ## "globals",     ## utility to screen functions
      "robfilter"       ## Packages for smooth first and second deviations
      )

install.packages(pkgs = favPack,
                 repos = options()$repos, dependencies = TRUE)
## 2019-12-05
## 2: In install.packages(pkgs = favPack, repos = options()$repos, dependencies = TRUE) :
##   installation of package 'highlight' had non-zero exit status
## 2018-01-23

## sudo apt install libcurl4-openssl-dev
favPack2 <- c("numDeriv")

install.packages(pkgs = favPack2,
                 repos = options()$repos, dependencies = TRUE)

## Installing from local directory
install.packages(pkgs=c("E:/lor/R/IDPmisc/IDPmisc_0.7.5.zip"),
                 repos = NULL, dependencies = TRUE)

## checking dependencies
pcPack <- c("compositions",
            "IDPmisc",
            ##"Pheatmap",
            "lattice",
            "seriation",
            "XLConnect")
allPack <- available.packages()

dim(allPack)
colnames(allPack)
rownames(allPack)
pcPack[!(pcPack %in% rownames(allPack))]
allPack[pcPack, "Depends"]
table(allPack[, "Repository"])
##In order to check and download packages which are not already installed
## http://www.bioconductor.org/docs/install/

URL <- "http://www.bioconductor.org/packages/release/bioc"

download.packages(new.packages(contriburl = URL),
                  contriburl = URL,
                  destdir = "E:/temp/R")

##In order to check and download packages which should be updated
download.packages(old.packages(contriburl = URL),
                  contriburl = URL,
                  destdir = "E:/temp/R")
## full path to packages on net
contrib.url(repos=repos)

## checking for new packages
x <- packageStatus(repositories = URL)
st <- x$avai["Status"]
pkgs <- rownames(st)[which(st$Status=="not installed")]
pkgs
install.packages(pkgs=pkgs,
                 repos = repos)

## Tips for automatic package installer in
## W:\Software\StatistikProgramme\R-Doc\Rcommander-fox.pdf p. 39

## Installing packages to special directories --------------------

## install.packages("sp", lib = "my_R_folder/library",
##        repos = "http://cran.CH.r-project.org")
## ## and now load it from that library (location):
## library(sp, lib = "my_R_folder/library")

update.packages()

## Installing for RAIRMO ----------------------------------------
install.packages(pkgs = c("sp","gstat","IDPmisc"),
                 repos = options()$repos, dependencies = TRUE)

## Installing from github --------------------
require(devtools)
install_github(locherx/IDPmisc)
require(IDPmisc)
