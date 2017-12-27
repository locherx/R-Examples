## -*- coding: utf-8 -*-
## examples.R
## Author: René Locher
## Version: 2017-11-14

pathIn <- "dat/"

options(width = 72)       ## width of default emacs
options(max.print = 1000) ## stops printing after 1000 values
options(contrasts = c("contr.treatment", "contr.poly"))

## show compiled HTML help
## Once installed, it cannot be unloaded during session
options(chmhelp = TRUE) ## does not work with emacs

onError <- function() {
    graphics.off()
}
options(error = utils::recover)
## enter recover() on error. See further below

rm(list = objects())  ## deletes workspace, sparing all dot objects
rm(list = objects(all.names = TRUE))  ## deletes everything

options()

warnings() ## print last warnings
warning()  ## clear last warnings

as.double() ## equivalent to as.real(), as.numeric()

## HTML-Hilfe in Browser starten
help.start()
help.search("regression")
help.search("regression", fields = "concept")
help.search("regression", fields = "keyword")
help.search("multivariate", fields = "keyword")
?"%%"
?Syntax
library(help = "base")

apropos("plot")

### see also http://stat.ethz.ch/CRAN/index.html / Search /
## .. / Search mail archives / google
## Tip: search for error message

## store objects out of functions into (new) environments
## linking an object to a function
?lockBinding

source("E:\\lor\\S\\goodies.R")
help.search.google('hexagon')
help.search.archive('memory')

vignette() ## Show vignettes of all packages installed
vignette(package = "grid")
vignette("displaylist", package = "grid")
browseVignettes(package = "grid")
citation("grid")
maintainer("grid")
packageVersion("grid")
packageDescription("grid")

read.table(file = "http://stat.ethz.ch/Teaching/Datasets/NDK/air.dat")
## Attention:
## numeric values are rounded to 7 digits before storing values
## you might change that behaviour by options(digits  =

## random seeds
set.seed(1)
save.seed <- .Random.seed
set.seed(save.seed)
sample(1000, 1)

set.seed(save.seed)
sample(1000, 1)

## Testing and Compiling packages
## RD-section: \dontrun{}
## R CMD check gains options --run-dontrun and --run-donttest.
## R CMD build supports pandoc files: *.md
tools::testInstalledPackage()

## loading package from github
library(devtools)
install_github("Oliver4242/testdemo")
library(testdemo)
testfun()



## Measuring Run Time ----------------------------------------

fun <- function(n = 10000, fun = function(x) x) {
  # Purpose: Test if system.time works ok;   n: loop size
  fun(for(i in 1:n) x <- mean(rt(1000, df = 4)))
}

system.time(fun(5000))

Sys.time()
fun(5000)
Sys.time()

microbenchmark::microbenchmark(fun(5000), times = 10)



##----------------------------------------
library(IDPmisc)
detach(package:IDPmisc)
search()  ## search path of R

##----------------------------------------
save.image()                               # save binary data to harddisk
save(list = ls(all = TRUE), file = ".RData") # =  save.image()

load("e:/lor/Charite/0312/.RData") # read binary data from disk
unload()
dump()        # store ojects from ASCII file
source()      # load ojects from ASCII file
data()        # list all datasets included in R and  attached libraries
?iris
head(iris)    # show first 6 lines of dataset
tail(iris, n = 2)
example(head) # Beispiele einer Funktion aufrufen

promptData(sunspots)
prompt(sunspots)

remove(list = objects(pattern = "^t\\.")) # immer mit list-Argument

## alle Namen, welche mit "." Starten
objects(pattern = "^\\.", all.names = TRUE)

## alle ohne Namen, welche mit "." Starten
objects(pattern = "^[^\\.]", all.names = TRUE)


## Umlaute
print("äöü")

## geht nicht (mehr?) korrekt in emacs!!
plot(rnorm(100), main = "äöü")

## redirecting output
sink(file = "test.txt")
1:10
cat("\n\n")
"done"
sink(file = NULL)

##----------------------------------------
## system near functions

## shows the actual capabilities of R, e.g. graphic drivers, tcltk, ...
capabilities()

## Accuracy of R
str(.Machine)


## run this for error reports
sessionInfo()

.Platform
win.version()
version
contributors()

setwd("e:/lor/R/") #setting working directory
getwd()            #getting working directory

?environment
browseEnv()
environment()
globalenv()
.BaseNamespaceEnv

Sys.getenv("PROCESSOR_IDENTIFIER")

## reading Windows path
Sys.getenv("PATH")
system(command = "CMD.EXE /C PATH", intern = TRUE) ## Alternative

## setting Windows path
Sys.setenv(PATH = "C:\\Windows\\system32;C:\\Windows")

Sys.getenv("HOMEPATH")
Sys.getenv("sysname")

## get version of installed java
system("java -version")

## typing time
system(command = "CMD.EXE /C time /t", intern = TRUE) ## Alternative


Sys.getlocale()
## Settings for aprpriate Umlaute in R
## "LC_COLLATE = German_Switzerland.1252;LC_CTYPE = German_Switzerland.1252;LC_MONETARY = German_Switzerland.1252;LC_NUMERIC = C;LC_TIME = German_Switzerland.1252"
## (custom-set-variables '(current-language-environment "UTF-8"))


Sys.info()
Sys.info()["sysname"]
sys.status()
Sys.sleep(1)

.Platform

xx <- c("ex", "col", "old")
list.files(pattern = paste("(",  paste(xx, collapse = ".*R$)|("), ".*R$)", sep = ""))

file.exists()
file.path()
file.show()
fileList <- file.info(list.files("E:/ZHAW", recursive = TRUE))
str(fileList)
range(fileList$size, na.rm = TRUE)
head(fileList)

system.file()
system.file(package = "stats") # The root of package 'stats'
R.home()

Sys.setlocale()
Sys.getlocale()

Sys.localeconv()

?Encoding
iconvlist()
localeToCharset()

## uses system facilities to convert a character vector between encodings: the 'i' stands for 'internationalization'
enc2utf8("Ü")

x <- "fa\xE7ile"
Encoding(x)
x
enc2utf8(x)

xx <- iconv(x, "latin1", "UTF-8")
Encoding(c(x, xx))
c(x, xx)
Encoding(xx) <- "bytes"
xx # will be encoded in hex
cat("xx = ", xx, "\n", sep = "")

iconv(x, "latin1", "ISO8859-1")

## convenient configurations for R on windows
## location for private files of Rconsole etc (see ?Rconsole):
## set R_USER E:\lor\R\profile
## set R_PROFILE E:\lor\R\profile\start.R
## see also CRAN -> an introduction to R ->
##                  invoking R from the command line

##configuration of console and the pager in
## ..\etc\Rconsole
?Rconsole
?Rprofile

## starting R in single windows:
## C:\Programme\R\bin\Rgui.exe --sdi

DOS> rterm < inpFile   #Old Version of Batch-Prozess
Windows> rscript < inpFile

## encoding stdin
rterm --encoding

## CP1252 = latin1 superset

shell("dir")
shell("cd")

## works only in R-GUI
shell("cmd /k cd %userprofile%", intern = TRUE)

object.size

##----------------------------------------
## namespace and classes

library(tkrplot)         # viewing databases, class hierarchies

## show hidden objects
MASS:::rlm.default

## S3-Classes
methods(print)

## the next two examples return identical results:
## getS3method() findet aber auch Methoden, welche im namespace versteckt sind
getS3method("aggregate", "data.frame")
aggregate.data.frame

getS3method("plot", "default")  ## search for S3 object
getAnywhere(plot)            ## search for S4 object
getFromNamespace("plot", "graphics")

library(IDPmisc)
getAnywhere(plot)
getAnywhere(plot)[1]

methods(plot)                  ## print all S3 functions
showMethods(plot)              ## print all S4 functions
## Function: plot (package graphics)
## x = "ANY", y = "ANY"                    ## S3 plot functions
## x = "rose", y = "missing"               ## new function from IDPmisc

library(R.methodsS3)
?R.methodsS3
R.KEYWORDS
?S3

## S4
library(help = methods)
## library(methods)  ## is called by default

?S4
?dotsMethods

selectMethod("plot", "rose")
selectMethod("plot", "ANY")

## plot S4 methods to all existing signatures
findMethods("plot")
findMethods("plot")@names


getMethod("plot", c("rose", "ANY"))
## No method found for function "plot" and signature rose
## -> S4 class!!

## plot method for default method
getMethod("plot", c("ANY", "ANY"))

showClass("rose")

methods ? show

showClass("rose")

MethodsList("rose")
MethodsList("plot")
MethodsList("function")
isGeneric("rose")
isGeneric("plot")

## another example for analysing method-4-functions
library(Biostrings)
showMethods(needwunsQS)
getMethod("needwunsQS", c("BString", "character"))
getMethod("needwunsQS", c("character", "BString"))
getMethod("needwunsQS", c("character", "character"))
Biostrings:::.needwunsQS

##----------------------------------------
## new classes
## be careful: don't use argument "names" in representation()!!

val.dat.dir <- function(x)
  {
    if (length(x@dir)! =  nrow(x@x))
      return("nrow(x) must be equal to length(dir)")
    if (sum(is.na(x@dir)) > 0)
      stop("slot 'dir' must not contain NAs!") else return(TRUE)
    }

setClass("dat.dir",
         representation(x = "matrix", dir = "numeric"),
         validity = val.dat.dir)


getClass("dat.dir")

tt <- new("dat.dir",
          x = matrix(1:4, nrow = 2, dimnames = list(1:2, 1:2)),
          dir = 10:11)
class(tt)

## analysing objects
data(ToothGrowth)
str(ToothGrowth)
class(ToothGrowth)           # "data.frame"
is(ToothGrowth)              # "data.frame" "oldClass"
mode(ToothGrowth)            # "list"

sum(c(NA, NA), na.rm = TRUE) # 0!!
help.start()                 # Starte HTML-Hilfe
library(help = chron)        # Liste der Funktionen in der Bibliothek chron

## packages
.packages(all.available = TRUE) # List all available packages
write.table(.packages(all.available = TRUE), file = "packages.txt")

.Defunct()
Deprecated()

write.matrix()

##
is.element(c(4:5, 11), 0:10)

x <- c(4:5, 11)
is.element(x, 0:10)

match(c(4:5, 11), 0:10)

## scope of objects, lexical scoping
## Gültigkeitsbereich

xx <- 10

tf0 <- function(xx)
  {xx <- xx + 1}

tf0(xx)
xx

tf1 <- function(xx)
  {xx < <-  xx + 1}

tf1(xx)
xx

tf2 <- function(yy)
  {yy.nam <- deparse(substitute(yy))
   ## deparse must be called before first evaluation of yy!

   yy <- yy + 1
   print(paste(yy.nam, yy, sep = ": "))
   assign(yy.nam, yy + 1, env = .GlobalEnv)
 }

xx
tf2(xx)
xx


## Execute a function call
## deparsing does not work as expected within a do.call because
## xx is already evaluated by do.call!!
do.call("tf", list(xx))
xx

expression(pi)
eval(expression(pi))

parse(text = "pi") ## = expression(pi)
eval(parse(text = "pi"))

f1 <- function(x, y = x)             { x <- x + 1; return(y)}
f2 <- function(x, y = eval(x))       { x <- x + 1; return(y) }
f4 <- function(x, y = x) { y <- y - 1; x <- x + 1; return(y) }
s1 <- function(x, y = substitute(x)) { x <- x + 1; return(y) }

s2 <- function(x, y)
{
  if(missing(y)) y <- substitute(x); x <- x + 1; return(y)
}

s3 <- function(x, y = substitute(x)) { x <- x + 1; y <- y-1; return(y) }

s4 <- function(x, y = substitute(x))
{
  y <- y - 1; x <- x + 1; return(y)
}


a <- 10
f1(a)         # 11
f1(10)        # 11
f1(a, 1)       # 1
typeof(f1(a)) # "double"
f2(a)         # 11
f4(a)         # 9

s1(a)         # 11
s2(a)         # a
s3(a)         # 10
s4(a)         # Error in y - 1 : non-numeric argument to binary operator
eval(s2(a))   # 10
eval(s2(10))  # 10
typeof(s2(a)) # "symbol"

## manipulation of functions
names(formals(lm))
args(lm)
gdata::Args(lm)

## Function as an argument
f6 <- function(dev){
    devNam <- deparse(substitute(dev))
    if (devNam ! =  "windows") {
        dev(file = paste0("test.", devNam))
    }
    plot(rnorm(100))
    if (devNam ! =  "windows") dev.off()
} ## f6

f6(windows)
f6(pdf)
f6()

## demonstration for match.arg
f7 <- function(dev = c(c("windows", "pdf", "png"))){
    dev <- match.arg(dev)
    print(dev)
}

f7()
f7("pdf")
f7("a2")
f7(a2)

## demonstration for match.arg (2), functions as argument
f8 <- function(dev = c(c("windows", "pdf", "png"))){
    ext <- match.arg(dev)
    dev <- get(x = ext)
    if (ext ! =  "windows") {
        dev(file = paste0("test.", ext))
    }
    plot(rnorm(100))
    if (ext ! =  "windows") dev.off()
}


f8()
f8("pdf")

## demonstration for ...
f9 <- function(a = c("a1", "a2"),  ...){
    a <- match.arg(a)
    print(a)
    if (length(list(...))) {
        cat("\n...-args:\n")
        print(list(...))
    } else {
        cat("\nno ...-args\n")
    }
}

f9()
f9(a = "a1", "nothing")
f9(a = "a2", b = "xy")
f9(a = 5, b = "xy")

f10 <- function(...){
    args <- list(...)
    print(args)
}

f10()

f10(x1 = 15, x2 = 20:26, 1000)

f11 <- function(x){
    cat("missing(x): ", missing(x), "\n")
    cat("exists(x): ", exists(x), "\n")
    cat("is.null(x): ", is.null(x), "\n")
    cat("is.na(x)", is.na(x), "\n")
    cat(missing(eval(substitute(x))))
}


f11()

f11("bla")
f11(bla)

x <- "bla"
f11(x)
f11(x = x)

f12 <- function(fun, ...)
{
    fun(...)
}

f12(mean, 1:10)
f12(plot, 1:10, rnorm(10))

f13 <- function(fun, ...)
{
    do.call(fun, list(...))
}

f13(mean, 1:10)
f13(mean, c(1:10, NA))
f13(mean, c(1:10, NA), na.rm = TRUE)
f13(plot, 1:10, rnorm(10))
f13(plot, 1:10, quote(rnorm(10)))


## changing single arguments in a ... list
library(lattice)

diag.f <- function(...) {
    par.list <- list(...)
    par.list$draw <- FALSE
    do.call(diag.panel.splom, par.list)
}

splom(~iris, diag.panel = diag.f)


test <- function(x) {
    print(exists(x))
    if (!exists(x, inherits = FALSE))
        print("x existiert nicht") else print(x)
}

test()

## sunflowerplot
sunflowerplot(x = sort(2*round(rnorm(100))), y =  round(rnorm(100), 0),
              main = "Sunflower Plot of Rounded N(0, 1)")

sunflowerplot(x = sort(2*round(rnorm(100))), y =  round(rnorm(100), 0),
              ann = FALSE, axes = FALSE, pch = 1)
box()
title(main = "Sunflower Plot of Rounded N(0, 1)",
      xlab = "x", ylab = "y")
axis(1)
axis(2, at = 1, label = "ja", las = 1)

## bquote for partial substitution
y <- paste(letters[1:4], "x", sep = ".")
bquote(y = =  .(y))

a <- 2
bquote(a = =  a)
quote(a = =  a)

bquote(a = =  .(a))
substitute(a = =  A, list(A = a))

## comparisons
x <- 0.1 * (5:0)
x = =  0.3
x = =  0.5
##
x[3] = =  0.3
identical(x[3], 0.3)
all.equal(x[3], 0.3)
all.equal(x[3], 0.3, tol = 0)

x <- c(1, NA, 3)
x = =  1
identical(x, 1)

x <- c(2, 4, NA, 6, NA, 4)
duplicated(x)
unique(x)

ifelse(x > 4, "yes", "no")

unique(data.frame(A = c(1:3, 3), B = c(1:3, 3)))



##################################################################
## Memory problems

## garbage collection: räumt Memory auf
gc()
## Ncells: fixer Variablenraum in MB
## Vcells: dynamischer Variablenraum in MB (heap)
## per default sind keine maximalen Limiten angegeben
## R can adress a maximum of 4 GB RAM with 32-Bit-Processor
## On XP you can address only 2GB, on XP /3GB 3 GB
## -> http://cran.r-project.org/bin/windows/base/rw-FAQ.html, kap 2.9

## maximum amount of memory in MB obtained from the OS is reported
memory.size(max = TRUE)/1048576

## amount of memory in use
memory.size(max = FALSE)/1048576

## returns memory limit in MB
memory.limit(size = NA)
## XP32 default:  1535 in R-Gui

## sets memory limit in MB
## default: 1 GB
memory.limit(size = 1700)

## setting memory limits during startup:
c:\programme\R\bin\Rgui --max-mem-size = 1700M

## 1 double = 8 Byte

##----------------------------------------
## debugging
browser()      ## set test point in code
c              ## exit browser and continue execution at next statement
n              ## next statement, stepping over function calls
s              ## next statement, stepping into function calls
f              ## finish function of current loop
Q              ## quit browser mode

## Debugging for S3- and S4-methods
?debug
?debugonce
?undebug
?isdebugged

options(error = recover)
## When called, recover prints the list of current calls, and prompts the user to select one of them. The standard R browser is then invoked from the corresponding environment; the user can type ordinary R language expressions to be evaluated in that environment.

## When finished browsing in this call, type c to return to recover from the browser. Type another frame number to browse some more, or type 0 to exit recover.

?bug.report
sessionInfo()

## it shows always the line before executing it
f <- function(x, y){
  if(missing(y)) y <- x^3
  M <- x^2
  return(y-x)
}

debug(f)
f(3, 2)
undebug(f)

trace("f", browser, exit = browser)
f(3)
untrace(f)

## Debug function when non fatal error occurs
fErr <- function(x){
    optionsOld <- options()
    options(error = browser())
    lm(x)
    options(optionsOld)
}

fErr(3)
options()$error

## this is a really comfortable debugging mode:
library(debug)
mtrace(f)
mtrace(f, FALSE)

## main commands in debugging window:
## go(5) ## go to line 5
## go()  ## go on until error occurs
## br(10) ## set break to line 10
## skip(15) ## go to line 15 without executing any code

## More about debugging in R-Extensions.pdf, Debugging


## Profiling Code
Rprof("impute.txt")
impute()
Rprof(NULL)

summaryRprof
## Analyse it by > R CMD Rprof impute.txt

## More sophisticated tools in
library(proftools)

## See also R-Extensions.pdf, Tidying and profiling R code

options(error = recover) #
## After error, 'recover' prints the list of current calls, and
## prompts the user to select one of them.

traceback()           # prints the call stack of the last uncaught error

##----------------------------------------
## error handling

## don't leave loop when there is an error
for (i in 1:10){
  result <- try(myfun(...))
  if(inherits(result, "try-error")) next
  print(result)
}

## executes code defined in on.exit() when exiting a function or R
on.exit()

## prompt error message and exit function
stop()

## prompt warning
warning()

##----------------------------------------
## Operators
a <- c(TRUE, TRUE, FALSE)
b <- c(TRUE, FALSE, FALSE)
a&b
a&&b
a&&rev(b)

#####################################################
## Plotting Devices

## Windows Meta Files
windows(width = 20, height = 20, pointsize = 10)
plot(rnorm(20))
savePlot(filename = "E:\\lor\\test", type = "wmf")

## Lossles compression: 10kB
## Empfohlenes Device für Punkt- und Liniengrafiken für Import nach Word
png(file = "10x10.png", width = 10, height = 10, unit = "cm", res = 300, pointsize = 10)
plot(rnorm(100), col = col)
dev.off()

## Komprimierung mit Qualität 75%: 34kB
jpeg(file = "10x10.jpg", width = 10, height = 10, unit = "cm", res = 300,
     quality = 75, pointsize = 10)
plot(rnorm(100), col = col)
dev.off()

## Verlustfrei komprimiert und vektorbasiert abgespeichert: 6kB
## Empfohlenes Device für Latex
inch <- 2.54
pdf(file = "10x10.pdf", width = 10/inch, height = 10/inch, pointsize = 10)
plot(rnorm(100), col = col)
dev.off()

## für 26"-Bildschirm von Dell
xpinch <- 2560/60*inch
ypinch <- 1440/34*inch
windows(width = 10/inch, height = 10/inch, xpinch = xpinch, ypinch = ypinch)

## The bitmap() device does not support transparency.
bmp()

## not recommended
bitmap()

## Lossy. Useful for images
jpg()

postscript()

## Lossy. Useful for images and especially for series of images
## on one or more pages
## Arguments for landscape A4 plots
pdf(file = "test.pdf",
    width = 26/2.54, height = 18/2.54,
    paper = "a4r", pointsize = 12)

plot(rnorm(100))

dev.off()

## ----------------------------------------

x <- rnorm(100)
qqnorm(x)
qqline(x, col = "red")

#################################################################
## data manipulation
library(help = gdata)
library(help = IDPmisc)

##################################################################
library(nnet)
multinom()

library(help = Design)
## logistic regression, Cox regression, accelerated failure time models,
## ordinary linear models, the Buckley-James model,
## and generalized least squares for serially or spatially correlated observations.
library(Design)
validate.lrm()    ## Resampling Validation of a Logistic Model

#################################################################
##


#####################################
## Input - Output
## Transfer von Daten aus S-Plus nach R

## Reading in data with different length of rows
## Attention: at least header must be complete!!
cat("a b c d e", "2 3 5 7 6", "11 13 17 19", file = "test.txt", sep = "\n")

read.table(file = "test.txt",
           sep = "",
           fill = TRUE,
           na.strings = "",
           header = TRUE)

## Alternatively read.table
scan(file = "test.txt", skip = 1, what = list(x = 0, y = "", z = 0), flush = TRUE)
scan(file = "test.txt", skip = 1, what = list(character(3)), flush = TRUE)
scan(file = "test.txt", skip = 1, what = list(character(3)), flush = FALSE)
L <- rep(list(character(1)), 5)
scan(file = "test.txt", skip = 1, what = L, flush = TRUE)
scan(file = "test.txt", skip = 1, what = L, flush = TRUE, na.string = "")

LL <- scan(file = "test.txt", skip  =  1, what = L, flush = TRUE, na.string = "")
matrix(unlist(LL), nrow = length(LL[[1]]), byrow = FALSE)

scan("test.txt", "character")
count.fields("test.txt", sep = " ")

## reading tables from clipboard
read.DIF("clipboard")

## Read a "table" of *f*ixed *w*idth *f*ormatted data into a 'data.frame'
ff <- tempfile()
cat(file = ff, "123456", "987654", sep = "\n")
read.fwf(ff, width = c(1, 2, 3))    #> 1 23 456 \ 9 87 654
unlink(ff)

## For example, if you want to read lines 1000 through 1100, you'd do it
## like this:
lines <- readLines("foo.txt", 1100)[1000:1100]
result <- scan(textConnection(lines), list( .... ))

## interactive reading
x <- readline()
x

readLines(stdin(), 2)


testit <- function()
{
  cat("Enter delay in seconds: ")

  Delay <- as.double(readLines(stdin(), 1))
  if(is.na(Delay)) stop("non-numeric input")
  Sys.sleep(Delay)
}


## wait for key pressed or mouse moved
?getGraphicsEvent


## ----------------------------------------
##
library(help = tcltk)
library(tcltk)

## ----------------------------------------
## Reading SPSS files and handling data
## see noe/SPSS.R

## recommended package:
library(memisc)

## library(foreign)

## task handlers
?addTaskCallback



library(help = Hmisc)
library(Hmisc)
csv.get() ## reads comma-separated text data files, allowing optional translation to lower case for variable names after making them valid S names. etc.
cleanup.import()

library(help = coda)
library(coda)
x <- read.and.check(message = "Gib eine Zahl ein", what = numeric())
x

################################
## colors
colors()                 ## all possible colors
col2rgb("green")         ## RGB color code

## HSV color code
## hue = color
## saturation = "pastellity" of color (0 = white, 1 = pure color)
## value = purity of color (0 = black, 1 = pure color)

HSV <- rgb2hsv(col2rgb(c("blue", "red", "green", "gold")))
HSV

hh <- seq(0, 1, 0.05)
plot(c(0, 1), ty = "n", ann = FALSE, axes = FALSE)
abline(h = hh, col = sample(colors(), 21), lwd = 20)

abline(h = hh, lwd = 20,
       col = c(rgb(red = 0.5, green = 0.1, blue = 0.5),
             rgb(red = 1, green = 1, blue = 1),
             rgb(red = 0, green = 0, blue = 0),
             rgb(red = 0, green = 0, blue = 1)))

## original colors
abline(h = hh, lwd = 20,
       col = hsv(HSV["h", ], HSV["s", ], HSV["v", ]))

## pastel-colors
abline(h = hh, lwd = 20,
       col = hsv(HSV["h", ], HSV["s", ]-0.6, HSV["v", ]))

## blackish colors
abline(h = hh, lwd = 20,
       col = hsv(HSV["h", ], HSV["s", ], HSV["v", ]-0.5))


library(IDPmisc)
showColors(IDPcolorRamp(100), border = FALSE)

showColors(rainbow(100, start = 0, end = 1/3))
showColors(rainbow(100, start = 1/3, end = 2/3))
showColors(rainbow(100, start = 2/3, end = 1))

showColors(rev(rainbow(100, start = 1/3 + 0.1, end = 2/3 + 0.05)))

showColors(heat.colors(10))
showColors(terrain.colors(20))
showColors(cm.colors(15))

library(help = RColorBrewer)
library(RColorBrewer)

display.brewer.pal(type = )

## all color palettes
brewer.pal.info

## continuous interpolation of colors up to n = 9 (ordered factors)
n <- 9
display.brewer.pal(n, "RdBu")
showColors(brewer.pal(n, "RdBu"))  ## red - white - blue - pattern
showColors(brewer.pal(n, "Greens"))
showColors(brewer.pal(n, "Oranges"))
showColors(brewer.pal(n, "YlGnBu"))
showColors(brewer.pal(n, "YlOrRd"))
showColors(brewer.pal(n, "Accent"))
showColors(brewer.pal(n, "Greys"))

## distinct colores (qualitative colors)
showColors(brewer.pal(12, "Paired"))
showColors(brewer.pal(6, "Paired"))
showColors(brewer.pal(8, "Accent"))
showColors(brewer.pal(9, "Set1"))

col2hsv(brewer.pal(9, "RdBu"))


## blue - white - red
cInt <- data.frame(h = c(0.64, 0.64, 0.98, 0.98),
                   s = c(0.9,    0,    0, 0.9),
                   v = c(0.7, 0.95, 0.95, 0.7))
fr <- c(0.5, 0)
col <- IDPcolorRamp(24, colInt = cInt, fr  = fr)
showColors(col)


## night blue - orange
rgb2hsv(51, 74, 125) ## night blue
rgb2hsv(255, 127, 0) ## night blue

cInt <- data.frame(h = c(0.62, 0.62, 0.08, 0.08),
                   s = c(0.59,  0.1,  0.1, 1),
                   v = c(0.49,    1,    1, 1))
fr <- c(0.5, 0)
col <- IDPcolorRamp(12, colInt = cInt, fr  = fr)

showColors(col)



library(gplots)
## nice colors
nc <- 12
m <- matrix(1:(15*nc) + rnorm(15*nc), nrow = 15, ncol = nc)
opar <- par(bg = "gray", mfrow = c(1, 2))

## distinct colors
matplot(m, type = "l", lty = 1, lwd = 3, col = rich.colors(nc))

## white -> blue -> black
matplot(m, type = "l", lty = 1, lwd = 3, col = rich.colors(nc, "blues")) ##

## Black -> blue -> green -> yellow -> orange -> red
matplot(m, type = "l", lty = 1, lwd = 3, col = rich.colors(nc, "temperature")) ##

ncol <- 100
plot(1:ncol, ylim = c(0, 11), ty = "n")
points(rep(10, ncol), col = heat.colors(ncol), pch = 16, cex = 2)
points(rep(9, ncol), col = rainbow(ncol), pch = 16, cex = 2)
points(rep(8, ncol), col = terrain.colors(ncol), pch = 16, cex = 2)
points(rep(7, ncol), col = topo.colors(ncol), pch = 16, cex = 2)
points(rep(6, ncol), col = cm.colors(ncol), pch = 16, cex = 2)
points(rep(5, ncol), col = greenred(ncol), pch = 16, cex = 2)
points(rep(4.5, ncol), col = redgreen(ncol), pch = 16, cex = 2)
points(rep(4, ncol), col = bluered(ncol), pch = 16, cex = 2)
points(rep(3.5, ncol), col = redblue(ncol), pch = 16, cex = 2)

palette()

library(IDPmisc)
points(rep(2, ncol), col = IDPcolorRamp(ncol), pch = 16, cex = 2)

## gegen Farbenblindheit
library(help = dichromat)
library(dichromat)
points(rep(1, ncol),
       col = colorRampPalette(c("blue", "green", "yellow", "red", "violet"),
           space = "rgb")(ncol), pch = 16, cex = 2)
?colorschemes

library(RColorBrewer)
brewer.pal(7, "Greens")

library(DAAG)
library(help = DAAG)
show.colors()
show.colors(type = "singles")
show.colors(type = "shades")  ## best for displaying colors
show.colors(type = "gray")

library(help = colorspace)


library(hexbin)
showColors(plinrain(100))
showColors(plinrain(100))

## Given (x, y) points, determine their multiplicity - checking for equality only up to some (crude kind of) noise. Note that this is special kind of 2D binning:
xyTable(iris[, 3:4], digits = 6)

## plotting with RGB code
plot(rnorm(100), col = rgb(200, 0, 160, max = 255))

col <- colors()
col <- grep("^((?!grey).)*$", col, value = TRUE, perl = TRUE)
nc <- length(col)

windows(width = 14, height = 14, pointsize = 14)
## adjust window to full screen

plot(c(0, 10), c(0, nc %/% 10), type = "n",
     xlab = "Einer", ylab = "Zehner", main = "Farbcode in R")
for (ii in 1:nc)
    text((ii %% 10), ii %/%10,
         labels = col[ii],
         col = col[ii],
         adj = 0, cex = 0.7)

savePlot(filename = "colortable1", type = "jpg")

## plot colors
gclus:::plotcolors(matrix(1:20, nrow = 4, ncol = 5))

## color vector with well distinguishable colors
col <- c("dodgerblue3", "firebrick2", "green3" , "darkorchid4",
         "deepskyblue", "chocolate1",  "forestgreen",  "violetred",
         "saddlebrown", "grey47")
plot(1:10, col = col, pch = 15, cex = 5)
plot(1:10, col = "chocolate1", pch = 15, cex = 5)

## another version,  I like even more
windows()
col <- c("blue", "green4", "red", "darkorchid4", "black",
         "deepskyblue", "green", "orange", "violetred",
         "grey50", "saddlebrown")
plot(1:11, cex = 5, pch = 15, col = col)

## conventional legends
## for legends in lattice graphics see key
windows()
plot(1:10, cex = 5, pch = 15, col = 1:10)

plot(1:10, cex = 2, pch = 43, type = "n")

legend(1, 10, legend = c("Huber-Mittel", "Mittel", "Median"),
       fill = col)

legend(1, 8, legend = c("Huber-Mittel", "Mittel", "Median"),
       col = col, lwd = 2)

legend(1, 6, legend = c("Huber-Mittel", "Mittel", "Median"),
       pch = 1:3,  col = col, lwd = 2, bty = "n")

legend(1, 4, legend = c("Huber-Mittel", "Mittel", "Median"),
       pch  =  1:3, col  =  col, bty = "n")


## plotting separate legend without large margins
## png(paste(ppath, "legend-sampleAge.png", sep = ""),
##     width = 5.2, height = 1, unit = "cm",
##     res = 200, pointsize = 10)

k <- 2.54
windows(5/k, 1.2/k, pointsize = 10)
par(mar = rep(0, 4))
plot(1:10, 1:10, type = "n", ann = FALSE, axes = FALSE)
legend(0.4, 11,
       legend = c("Fresh test sample", "Test sample from stock"),
       fill = c(green, red),
       bty = "n"
       )
## dev.off()
##

library(IDPmisc)
source("//bordeaux/staff/lore/public/R/goodies/LegendColorRamp.R")
LegendColorRamp()
LegendColorRamp(box.width = 0.2)
LegendColorRamp(IDPcolorRamp(19), ntm = 3)
LegendColorRamp(ntm = 0)

LegendColorRamp(col = IDPcolorRamp(50),
                valmap = seq(0, 40, 5),
                x.lab = 8,
                Unit = quote(~paste(mu, g/m^3)))

## drawing legends and labels of curves at free space
## drawing nice graphs by hand!!
library(Hmisc)
?labcurve

## Steps in plotting
y <- c(0, 2, 3, 5, 7)
x <- seq(along = y)
plot(x, y, ty = "s", lwd = 5, col = "red")
lines(x, y, ty = "S", lwd = 1)

lines(x + 1, y, ty = "S", lwd = 1)

y0 <- c(1, 2, 4, 3)
sfun0  <- stepfun(1:3, y0, f = 0)
plot(sfun0)
sfun.2 <- stepfun(1:3, y0, f = .2)
plot(sfun.2)
sfun1  <- stepfun(1:3, y0, f = 1)
sfun1c <- stepfun(1:3, y0, right = TRUE)# hence f = 1
sfun0

## rugplots
?rug

?jitter


example(boxplot)
example(stripchart)

## many examples of useful color ramps
## http://geography.uoregon.edu/datagraphics/color_scales.htm

## Draws a scatterplot matrix of data.
## Variables may be reordered and panels colored in the display.
cpair()

## Scatterplots with smoothed densities color representation
## http://addictedtor.free.fr/graphiques/graphcode.php?graph = 139
library(geneplotter)

library(car)
scatterplot.matrix()
scatterplot()             ## Scatterplots with Boxplots at the margin

## enhanced scatterplot matrix
library(gclus)
library(help = gclus)
data(USJudgeRatings)
judge.cor <- cor(USJudgeRatings)
judge.color <- dmat.color(judge.cor)
                                        # Colors variables by their correlation.
cpairs(USJudgeRatings, panel.colors = judge.color, pch = ".", gap = .5)



## 2 d graphics
xy.coords(3:4, 1:2)
xy.coords(matrix(1:4, ncol = 2))
xy.coords(as.data.frame(matrix(1:4, ncol = 2)))
xy.coords(4:1)
xy.coords(list(a = 1:2, b = 9:10))
xy.coords(list(x = 1:2, y = 9:10))
xy.coords(3:4, NULL)
xy.coords(matrix(1:9, ncol = 3))

chull()
mosaicplot()

plot(1:10, typ = "n")
segments(1:2, 2:1, 3:4, 4:3, col = "red")
polygon(5 + c(1, 5, 5, 1), 5 + c(1, 1, 5, 5), col = "blue")


plot(1:10, typ = "n")
x.usr <- par("usr")
rect(x.usr[1], x.usr[3], x.usr[2], x.usr[4], col = "red")
polygon(3 + c(1, 5, 6, 1), 3 + c(1, 1, 6, 5), col = "blue")
segments(1:2, 2:1, 3:4, 4:3, col = "orange", lwd = 3)

## conditional plots
coplot()

## 3 D-Grafiken (vgl. auch in lattice und rgl)
library(IDPmisc)
x <- 1:10
y <- 1:5
d <- -1 ## oder 0
ncol <- 200
image(x = x, y = y,
      z = matrix(sample(1:ncol, (length(x) + d)*(length(y) + d),
          replace = TRUE), ncol = (length(y) + d)),
      col = IDPcolorRamp(ncol))

image(x = x, y = y,
      z = matrix(sample(1:ncol, (length(x) + d)*(length(y) + d),
          replace = TRUE), ncol = (length(y) + d)),
      col  =  hsv(seq(0, 1, 1/ncol)))

image(x = 1:3, y = 1:3,
      z = matrix(c(1, 2, 3, 1), ncol = 2),
      col = c("red", "blue", "green"))

## Check, ob Fehlermeldung verbessert!
image(x = 1:2, y = 1:2,
      z = matrix(c(1, 2, 3, 1, 2, 3), ncol = 2 + 1),
      col = c("red", "blue", "green"))


contour()
heatmap()

library(help = scatterplot3d)
library(scatterplot3d)
z <- seq(-10, 10, 0.01)
x <- cos(z)
y <- sin(z)
scatterplot3d(x, y, z, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue", main = "scatterplot3d - 1", pch = 20)


stripchart(rnorm(100))

## special interactive graphics:
ggobi
iPlots

####################
### Plottingsymbols
plot(c(0, 10), c(0, 30), type = "n", cex = 2,
     xlab = "Einer",
     ylab = "Zehner",
     main = "Plottingsymbols in R (pch)")
for (ii in 1:300)
    points((ii %% 10), ii %/%10, pch = ii)
savePlot(filename = "d:\\daten\\S\\plotsymbols", type = "ps")
savePlot(filename = "d:\\daten\\S\\plotsymbols", type = "wmf")

points(0, 0, pch = 1, cex = 4)

#####################
## plotting with reversed axis
plot(1:10)
plot(1:10, xlim = c(10, 0))

## pretty axis
pretty(1:50, n = 2)
t.d <- log10(abs(min(diff(pretty(1:50, n = 2)))))

###############################
## plotting parameters in par()
par()$xpd  # clipping definitions
par()$new  # next plot plots over the last one!
par()$fg   # foreground color for axis, labels, etc.
par()$bg   # background color
par()$ann  # defines whether axis and titles are plottet

## layout definitions
nf <- layout(matrix(1:16, ncol = 4), width = c(lcm(1), rep(1, 2),  lcm(2)),
             height = c(lcm(1), rep(1, 2), lcm(4)), respect = TRUE)
layout.show(nf)

mat <- matrix(1:4, ncol = 2, byrow = TRUE)
mat[4] <- 3
nf <- layout(mat, width = rep(1, 2), height = c(2, 1))
layout.show(nf)

########
## special characters
library(graphics)
library(help = graphics)

demo(Hershey)

plot(rnorm(10), xlab = "\\*a")
plot(rnorm(10), xlab = "\010")

library(Hmisc)
character.table() ## Shows numeric equivalents of all latin characters
show.pch()        ## plots the definitions of the pch parameters.
show.col()
sedit()           ## A set of character handling functions

##########################################################
## printing without frame
plot(window(sz.Ox.f, start = 1319, end = 1349), frame.plot = FALSE, axes = FALSE, ann = FALSE)

## draw rectangles
polygon(c(1332, 1332, 1339, 1339), c(0, 100, 100, 0), col = "lightblue")

## definitions of different plot types
plot(1:10, type = "S")
plot(1:10, type = "s")

plot(1:10, type = "h")
lines(1:10, type = "s")
lines(1:10, type = "c")
lines(1:10, type = "b")

plot(1:10, type = "o")
lines(1:10, type = "b")


#### drawing maps
library(maps)
map()
map("world", region = "Switzerland",  exact = TRUE)


## see also
## http://www.all-about-switzerland.info/swiss-cantons.html
identify(map(fill = TRUE, col = "white"), n = 5, index = FALSE)

library(ggmap)




x <- y <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(x^2, y^2, " + "))
image(x, y, z = z <- cos(r^2)*exp(-r/6), col = gray((0:32)/32))
image(x, y, z, axes = FALSE, main = "Math can be beautiful ...",
      xlab = expression(cos(r^2) * e^{-r/6}))

grid <- expand.grid(x = x,  y = y)
identify(grid, atpen = TRUE)

## France
library(ade4)
data(elec88)
par(mfrow = c(2, 2))
area.plot(elec88$area, cpoint = 1)
area.plot(elec88$area, lab = elec88$lab, clab = 0.75)
area.plot(elec88$area, clab = 0.75)
                                        # elec88$neig <- neig(area = elec88$area)
area.plot(elec88$area, graph = elec88$neig,
          sub = "Neighbourhood graph", possub = "topright")
par(mfrow = c(1, 1))

windows()
image(1:2, 1:2, matrix(1:4, ncol = 2), col = BlueGreenRedViolet(4))

windows()
image(x = list(x = c(1, 2, 5), y = c(1, 3, 7)),
      z = matrix(1:4, ncol = 2), col = BlueGreenRedViolet(4))


## ----------------------------------------
## splitting graphics
split.screen(figs = c(1, 2), erase = TRUE)
screen(2)
plot(rnorm(100))
close.screen(all = TRUE)

split.screen(figs = c(2, 1), erase = TRUE)
screen(2)
plot(rnorm(100))
close.screen(all = TRUE)

split.screen(c(2, 2))
screen(4)
plot(rnorm(100))
screen(1)
split.screen(c(2, 2))
split.screen()
screen(5)
par(mar = rep(0, 4))
plot(rnorm(100), ann = FALSE, labels = FALSE, tcl = 0.5)
screen(8)
par(mar = rep(0, 4))
plot(rnorm(100), ann = FALSE, labels = FALSE, tcl = 0.5)
close.screen(all = TRUE)

## ----------------------------------------
## Plotting into single frames
par(mfrow = c(2, 2))
plot(1:2, type = "n", frame.plot = FALSE, axes = FALSE, ann = FALSE)
par(mfg = c(2, 2))
plot(rnorm(10))
par(mfg = c(2, 1))
plot(10:1)

#####################################################
## Grid
library(help = grid)
library(grid)

grid.newpage()

grid.rect(gp = gpar(lty = "dashed"))
grid.rect(gp = gpar(lty = "solid"))

grid.newpage()
grid.rect(gp = gpar(lty = "dotted")) ## "dotdash", "longdash" or "twodash"

get.gpar()
gpar(cex = 0.8)
get.gpar()

vp.rose <- viewport(x =  0.2,
                    y =  0.3,
                    width = 0.6,
                    height = 0.4,
                    just = c("left", "bottom"),
                    xscale = c(-1, 1),
                    yscale = c(0, 10),
                    name = "vp.rose")
grid.newpage()
pushViewport(vp.rose)
grid.text("Text", 0, 8, default.units = "native",
          gp = gpar(cex = 3))
grid.rect(gp = gpar(lty = "dotted"))
grid.lines(x = c(0, 1), y = c(0, 1))
grid.lines(x = c(0, -1), y = c(0, 5), default.units = "native")
current.vpTree()

grid.newpage()
grid.text("Text", 0, 8, default.units = "native",
          gp = gpar(cex = 3),
          vp = vp.rose)
current.vpTree()
## "vp.rose" is not tracked!

grid.newpage()
grid.draw(gTree(children  =
                    gList(textGrob("Text", 0, 8, default.units = "native",
                                   gp = gpar(cex = 3))),
                vp = vp.rose))
current.vpTree()
## "vp.rose" is not tracked!

grid.newpage()
grid.show.viewport(vp.rose)
current.viewport()
pushViewport(vp.rose)
vpPath("vp.rose")
current.vpTree()

?grid.convert

labels <- c("eins", "zwanzig", "dreihundert")
convertWidth(stringWidth(labels), "mm")
unit(5, "strwidth", labels)
unit.c(5, "strwidth", labels)

grid.newpage()
convertWidth(2*max(stringWidth(labels)), "mm")

grid.newpage()
pushViewport(viewport(gp = gpar(cex = 2)))
convertWidth(max(stringWidth(labels)), "mm")
## gives a littel bit more than *2 !!??

## searching for grobs
getNames()
childNames(grid.get("rose"))

## using layouts
windows(20, 10, pointsize = 14)
grid.newpage()
LayOut <- viewport(layout = grid.layout(nrow = 1, ncol = 2))
pushViewport(LayOut)
leftVP <- viewport(layout.pos.col = 1, layout.pos.row = 1)
pushViewport(leftVP)
grid.text("left", gp = gpar(col = "red"))
current.vpTree()

popViewport()
pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 1))
grid.text("right", gp = gpar(col = "blue"))
popViewport()
grid.text("not right", 0.2, 0.2, gp = gpar(col = "blue"), vp = leftVP)

## ----------------------------------------
## For Interactive Plots in Browser
library(shiny)


############################################################

## Coplots
quantile(quakes$depth)
quantile(quakes$depth, p = seq(0, by = 0.2, 1))

shingle(quakes$depth,
        interval = cbind(quantile(quakes$depth, p = seq(0, by = 0.2, 0.8)),
            quantile(quakes$depth, p = seq(0.2, by = 0.2, 1))))

t.depth <- cut(quakes$depth, breaks = c(0, 80, 190, 400, 560, 700))

xyplot(lat ~ long | t.depth, data  =  quakes)

xyplot(lat ~ long | equal.count(depth), data = quakes)

## with ordinary graphics
windows()
coplot(lat ~ long | t.depth, data = quakes)

## 3d-mountain, 4th dimension is color
data(volcano)  ## 87 x 61 matrix
wireframe(volcano, shade = TRUE,
          aspect = c(61/87, 0.4),
          light.source = c(10, 0, 10))

## 3d-Trellis
library(lattice)
data(iris)
cloud(Sepal.Length ~ Petal.Length * Petal.Width | Species, data = iris,
      screen = list(x = -90, y = 70), distance = .4, zoom = .6)

## trellis image plot:
x <- seq(pi/4, 5 * pi, length = 100)
y <- seq(pi/4, 5 * pi, length = 100)
r <- as.vector(sqrt(outer(x^2, y^2, " + ")))
grid <- expand.grid(x = x, y = y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
levelplot(z~x*y, grid, cuts = 50, scales = list(log = "e"), xlab = "",
          ylab = "", main = "Weird Function", sub = "with log scales",
          colorkey = FALSE, region = TRUE)

levelplot(z~x*y, grid, cuts = 50, scales = list(log = "e"), xlab = "",
          ylab = "", main = "Weird Function", sub = "with log scales",
          colorkey = TRUE, region = TRUE)



## rotating 3d
library(rgl)


####################
## Tabellen in Grafiken
library("gregmisc")
library(help = "gregmisc")

## for V2.0 cf.
library(gdata)
library(gmodels)
library(gplots)

library(gtools)
x <- c("x11", "x2")
mixedsort(x)

library(help = gregmisc)
library(help = gdata)
library(help = gtools)
library(help = gmodel)
library(help = gplots)


balloonplot           # Plot a graphical matrix where each cell
                                        # contains a dot whose size reflects the
                                        # relative magnitude of the corresponding
                                        # component.
bandplot              # Plot x-y Points with Locally Smoothed Mean and
                                        # Standard Deviation
barplot2              # Enhanced Bar Plots
boxplot.n             # Produce a Boxplot Annotated with the Number of
                                        # Observations
combinations          # Enumerate the Combinations or Permutations
                                        # of the Elements of a Vector
invalid               # Tests if a value is missing, empty, or contains
                                        # only NA or NULL values
invalid(log(-3))
invalid(log(0))
invalid(NULL)         # is.nan(NULL) gives error!

is.what()             # Run Multiple is.* Tests on a Given Object
lowess()              # Scatter Plot Smoothing
hist2d()              # Compute and Plot a 2-Dimensional Histogram
                                        # cf. hexbin plots
matchcols             # Select columns names matching certain critera
plotCI()              # Plot Error Bars
read.xls()
textplot()            # Display text information in a graphics plot.

## histograms
x <- c(0, 0.5, 1, 3, 5, 6, 10, 10, 12, 13, 14, 15, 16)
r.hist <- hist(x, breaks = (0:4)*5)
r.hist
r.hist <- hist(x, breaks = (0:4)*5, right = TRUE)

library(gplots)
par(mfrow = c(2, 1))
M <- matrix(c("ab", "cd", "ef", "gh"), ncol = 2)
colnames(M) <- c("A", "B")
rownames(M) <- 1:2
textplot(M)
textplot(matrix("Hallo"), show.rownames = FALSE, show.colnames = FALSE)

## Also good for plotting Venn-Diagrams!

##################################################################
## Densitiyplots ##

## multivariate dataset
dat <- na.omit(read.table("E:\\lor\\S\\inNet.dat", header = TRUE)[c(9:11)])
dat <- na.omit(read.table("E:\\lor\\S\\inNet.dat", header = TRUE)[c(21:23)])
data(iris)

## Hexagon-Plots
library(help = hexbin)
library(hexbin, help = TRUE)
source("E:\\lor\\S\\Dichteplots-hexbin.R")
p.pairs(log(dat + 0.01, base = 2), xbins)

par(mfrow = c(2, 1), pty = "s")
plot(range(r.bin$xbnds), range(r.bin$ybnds), ty = "n")
p.hexagons(r.bin, colramp = BlueGreenRedViolet, )

r.bin <- hexbin(iris[, 1], iris[, 3], xbins = 10)
hexbin(iris[, 3], iris[, 1], xbins = 10)$cell
rx <- range(iris[, 1])
ry <- range(iris[, 2])

plot(r.bin)
plot(r.bin, colramp = inLuft.colors)

plot(r.bin)
plot(r.bin, colramp = inLuft.colors2)

par(mfrow = c(2, 3), pty = "s")
plot(iris[, 1], iris[, 2])

plot(rx, ry, type = "n")
hexagons(r.bin, colramp = BlueGreenRedViolet)

plot(rx, ry, type = "n")
p.hexagons(r.bin, colramp = BlueGreenRedViolet, maxcnt2 = 10)

## single counts in different colors
plot(r.bin,
     colramp = inLuft.colors2,
     colorcut = c(0, seq(1/max(r.bin$cnt), 1, length = min(17, max(r.bin$cnt)))))

## without legend
plot(rx, ry, type = "n")
p.hexagons(r.bin, colramp = BlueGreenRedViolet, maxcnt2 = max(r.bin$cnt))

## Multivariate analyses
library(help = ade4)
library(ade4, help = TRUE)


library(help = knnTree) ## k-nearest-neighbour classifier

## multivariate regression
library(help = pls.pcr)
library(pls.pcr)



###########################################################
## Character Codes
## R uses \"octale value" as ASCII-Code
## Converter of ASCII Code: http://www.cplusplus.com/doc/papers/ascii.html

paste(rep(0:9, 15), collapse = "")

## ESCAPE characters in paste:
## for more ESCAPE characters see ?Quotes
paste0("ab", "\n", "cd") ## LF
paste0("ab", "\"", "cd") ## "
paste0("ab", "\b", "cd") ## BS
paste0("ab", "\t", "cd") ## TAB
paste0("ab", "\r", "cd") ## delete line before \r


paste0("ab", "\\.", "cd")## ESCAPE special character in regexpr
paste0("ab", "[.]", "cd")## workaround

print("\344 \304 \366 \326 \374 \334 \265")

## Umlaute work
regexpr("ü", c("Grüsch", "Buh"))

cat("CR \n this will be wiped out\r LF \n newline\n")

## but:
print("CR \n this will NOT be wiped out\r LF \n newline\n")
print("CR \012 this will NOT be wiped out\015 LF \012 newline\n")
## \012 = CR \015 = LF

## Umlaute, e.g. ö
plot(rnorm(10), xlab = "\366\366")
plot(rnorm(10), xlab = "öö")
## Be careful with GUIs like emacs!!


## ASCII-Code
library(sfsmisc)
library(help = sfsmisc)

AsciiToInt("ä")
AsciiToInt("ö")
AsciiToInt(a)
strcodes("äö")

print("ä")

AsciiToInt("o")
chars8bit(111)
AsciiToInt("A")
chars8bit(65)

chars8bit(AsciiToInt("A"))
chars8bit(AsciiToInt("µ"))
AsciiToInt("µ")

## greek letters and Formulas
## for spelling for greek letters see
##   http://www.mathacademy.com/pr/prime/articles/greek/index.asp
## see ?plotmath and for details and
## demo(plotmath) for more math expressions
##
## file:///C:/Programme/R/library/grDevices/html/plotmath.html

## Gives the current Windows Font Mappings
windowsFonts()

## finding all fonts installed on windows:
## Systemsteuerung -> Schriftarten -> Right Click -> Eigenschaften -> Details
## Titel

## mapping a windows font to another string
windowsFonts(Arial = windowsFont("Arial"))
windowsFonts(X = windowsFont("GIGI"))
windowsFonts(X = windowsFont("Script MT Bold"))
windows(family = "X")

## Setting default font
## png(file = "fonttest.png", width = 20, height = 20, unit = "cm", res = 200, pointsize = 12, family = "X")

## The following objects can be printed in different fonts of the *same* family, set in par()
## 1 corresponds to plain text (the default), 2 to bold face, 3 to italic and 4 to bold italic
## font, font.axis, font.lab, font.main, font.sub

windows()
plot(1, 10, xlim = c(0, 11), ylim = c(0, 11), ty = "n",
     xlab = expression(bold(Periodenmittel) ~~ NH[3]~~
                         "[" ~paste(mu, g/m^3) ~"]"))
text(0, 10 , adj = 0,
     labels = expression(NH[4] ~ (paste(mu, mol))))
text(4, 10, adj = 0,
     labels = expression(NH[4] ~"in" ~"[" ~paste(mu, mol) ~"]"))

text(7, 10, adj = 0,
     labels = expression(bold(NH[4] ~"in" ~"[" ~paste(mu, mol) ~"]")))

text(0, 9.5, adj = 0,
     labels = expression(Tagesmittel ~~ "[" ~paste(mu, g/m^3) ~"]"))

## change the font!
par(family = "Arial")
text(0, 9, labels = expression(alpha + beta), adj = 0)
text(0, 8, labels = expression(widehat(gamma~delta)), adj = 0)
text(0, 7, expression(group("(", list(a, b), "]")), adj = 0)
## phantom() leaves that much space as the character would have
text(0, 6, expression({}[phantom(0)*7]^{14}~C), adj = 0)
text(2, 6, expression(beta[m(t[i])]^{(dist)}))
text(4, 6, expression(beta[phantom(0)*m(t[i])]^{(dist)}), adj = 0)
text(0, 5, labels = expression(paste(Phi, phi, phi1)), adj = 0)
text(0, 4, labels = expression(paste(tilde(Tau), dot(rho), ring(Sigma))), adj = 0)
text(0, 3, labels = expression(widetilde(paste(Delta, Epsilon))), adj = 0)
text(0, 2, labels  =
         expression(median ~bgroup("(", frac(NO[2]^(1), NO[2]^(0)), ")")),
     adj = 0)
text(0, 1, labels = expression(M^(1) ~scriptstyle(Mm^(1))),
     adj = 0)
## dev.off()

plot(1, 10, xlim = c(0, 11), ylim = c(0, 11), ty = "n",
     xlab = expression(bold(Periodenmittel) ~~ NH[3]~~
                           "[" ~paste(mu, g/m^3) ~"]"))
text(9, 1, labels = expression(~paste(mu, g) ~m^{-3}))
x <- 999
text(1, 9,
     labels = parse(text = paste0("Total~Cm~of~particles:~", x, "~paste(mu, g) ~m^{-3}")),
     adj = 0)
text(1, 8,
     labels = parse(text = paste0(x, '~"["~paste(mu, g/m^3)~"]"')),
     adj = 0)



text(8, 1, labels = expression(bgroup("}", atop("blabla", "what so ever"), "")))

text(5, 5, labels = "}", cex = 3)

## Combining "math" and numeric variables
plot(1, 10, xlim = c(0, 11), ylim = c(0, 11), ty = "n", )

## This works only for scalars!!
y1 <- "That works!"
text(5, 2, labels = substitute(bold(y), list(y = y1)), adj = 0)

x <- 1:4
y2 <- paste(letters[1:4], "x", sep = ".")
text(x = x, y = x - 1, labels = y2, adj = 0)

## works not as intended! Takes only the first element
text(x, x, adj = 0, label = substitute(bold(y), list(y = y2)))

## This will work:
for (ii in 1:length(y2)) {
    text(ii, ii + 1,
         label = substitute(bold(y), list(y = y2[ii])),
         adj = 0)
}

## Another example: .() makes bquote evaluating the expression inside
for (ii in 1:length(y2)) {
    text(ii, ii + 2,
         label = bquote(bold(alpha) ^.(y2[ii])),
         adj = 0)
}

## substituting an expression
text(0, 8, label = substitute(x ~ y, list(x = ~alpha, y = ~paste(mu, g/m^3))), adj = 0)

text(0, 9, label = substitute(paste(x, y), list(x = 1, y = ~paste(mu, g/m^3))), adj = 0)

## Give the expression as a string
expr <- "x^y"
text(0, 10, label = parse(text = expr), adj = 0)

## see google "r expression substitute"
plot(1, 10, xlim = c(0, 11), ylim = c(0, 11), ty = "n", )

text(0:3, 1 + (0:3), label = y2, adj = 0, vfont  =  2)

text(0:3, 0:3, labels = letters[1:4])
text(0:3, 0:3, labels = paste(letters[1:4], "x", sep = " "))

text(5, 5, labels = expression(paste(over(omega, x))), adj = 0)
text(5, 3, labels = expression(bold(bar(Omega))), adj = 0)
text(5, 1, labels = expression(zeta[t]^2), adj = 0)
text(7, 5, labels = expression(f ~bgroup("(", frac(omega, x), ")")), adj = 0)
text(7, 4, labels = expression(I~ + ~gamma %.% ab), adj = 0)
text(7, 3, labels = expression(I~ + ~gamma %.% "470m"), adj = 0)

mtext("test", side = 4)
mtext("test", side = 4, line = -3)

## How to combine "math" and (numeric) variables :
plot(1:10, type = "n", xlab = "", ylab = "", main  =  "plot math & numbers")
theta <- 1.23
mtext(bquote(hat(theta) = =  .(theta)), line =  .25)
for(i in 2:9)
    text(i, i + 1, substitute(list(xi, eta) = =  group("(", list(x, y), ")"),
                            list(x = i, y = i + 1)))
## note that both of these use calls rather than expressions.

text(1, 10,  "Derivatives:", adj = 0)
text(1, 9.6, adj = 0,
     expression("             first of f(x) " = =  {f * minute}(x)))
text(1, 9.2, adj = 0,
     expression("      second of f(x) " = =  {f * second}(x)))

plot(c(0, 1), c(0, 1), type = "n")
chemform <- "H[2]*O"
text(0.5, 0.5, label = parse(text = chemform), cex = 2)

plot(-1:1, -1:1, type = "n", xlab = "Re", ylab = "Im")
K <- 16; text(exp(1i * 2 * pi * (1:K) / K), col = 2)


?Hershey
## Drawbacks:
## For Hersheyfonts expressions are meaningless!!!
## Letters are thinner than in standard font
plot(1, 10, xlim = c(0, 11), ylim = c(0, 11), ty = "n", )
text(10, 10, labels = "\\ + f", adj = 0, vfont = c("serif symbol", "plain"), cex = 2)
text(0, 5, labels = "bold", adj = 0, vfont = c("serif", "bold"), cex = 2)
text(0, 4, labels = "bold", adj = 0, vfont = c("sans serif", "bold"), cex = 2)
text(0, 2, labels = "bold", adj = 0, cex = 2)

demo(Hershey)




underlined <- function(x, y, label, ...){
    text(x, y, label, ...)
    sw <- strwidth(label)
    sh <- strheight(label)
    lines(x + c(0, sw), rep(y - 1.5*sh/2, 2))
    ## lines(x + c(-sw/2, sw/2), rep(y - 1.5*sh/2, 2))
}

underlined(1, 4, expression(widehat(x %*% y)))

h <- "theta"
text(6, 6, labels = parse(text = h))

?plotmath

par(ask = TRUE); example(plotmath)

## more string functions
strwrap()

##----------------------------------------
## prettifying output
x <- c("a", "ab", "abcde")
encodeString(x, w = NA, quote = "'", justify = "r")


#################################################
## GUIs with R

## use Windows-Interface
winDialog(type = "yesnocancel", "Hello")

winDialogString("Hello again", "grummel grummel")

file.choose(new = FALSE)
file.choose(new = TRUE)

switch(menu(c("Hello", "grummel"),
            graphics = FALSE,
            "Menuauswahl"),
       "Halli hallo",
       "grummel, grummel")

decide <- function(x){
    switch(x,
           a = print("a"),
           print("else"))
} ## decide

decide("a")
decide("z")


## ----------------------------------------
## draw back gemäss uth:
## Fenster, welche out-of-focus waren, werden erst neu gezeichnet, wenn
## man sie wieder anklickt.

library(help = tcltk)
library(help = widgetTools)
library(help = Rcmdr)

## examples from
## http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/mb.html
## tc command reference:
library(tcltk)

## Spaces in message enlarge windows!?
## Info message box
tkmessageBox(title = "Greetings from R TclTk",
             message = "Hello, world!                  ",
             icon = "info", type = "ok")

## error message box
tkmessageBox(message = "An error has occurred!", icon = "error", type = "ok")

## warning message box
tkmessageBox(message = "This is a warning!", icon = "warning", type = "ok")

## question box
tkmessageBox(message = "Do you want to save before quitting?",
             icon = "question", type = "yesnocancel",
             default = "yes")

## button connected to function
f.plot <- function() plot(rnorm(100))
f.exit <- function() {
    graphics.off()
    tkdestroy(tt)
}

tt <- tktoplevel()
plot.but <- tkbutton(tt, text = "plot data", command = f.plot)
exit.but <- tkbutton(tt, text = "exit", command = f.exit)
tkgrid(plot.but, pady = 2, sticky  =  "w")
tkgrid(exit.but, pady = 2, sticky  =  "w")

tkfocus(tt)

## setting text in window
tt <- tktoplevel()
tkgrid(tklabel(tt, text = "Here is a centered string of text."))
tkgrid(tklabel(tt, text = "Left"), sticky = "w")  ## left and right in separate rows
tkgrid(tklabel(tt, text = "Right"), sticky = "e") ##

tkgrid(tklabel(tt, text = "    ")) # Blank line
tkgrid(tklabel(tt, text = "    ")) # Blank line

## still everything in 1 column
tkgrid(tklabel(tt,
               text = "Here is a much longer string of text, which takes up two columns."), columnspan = 2)

LeftLabel <- tklabel(tt, text = "Left")
RightLabel <- tklabel(tt, text = "Right")
tkgrid(LeftLabel, RightLabel)
## left and right are in 1 row but separate columns and centered

tkgrid.configure(LeftLabel, sticky = "w")
## (the already printed) LeftLabel is now to left

tkgrid.configure(RightLabel, sticky = "e")
## (the already printed) RightLabel is now right

LeftLabel2 <- tklabel(tt, text = "LeftAligned")
RightLabel2 <- tklabel(tt, text = "RightAligned")
tkgrid(RightLabel2, LeftLabel2)
tkgrid.configure(RightLabel2, sticky = "e")
tkgrid.configure(LeftLabel2, sticky = "w")

tkgrid(tklabel(tt, text = "    ")) # Blank line
tkgrid(tklabel(tt, text = "    ")) # Blank line

tkgrid(tklabel(tt,
               text = "This sentence takes up two rows, \n but only one column"),
       rowspan = 2)
tkfocus(tt)

## more goodies in
library(widgetTools)

## adapted for R (2008)
library(gWidgets)

#####
## uth zieht für windows graph apps und R-com/R-Excel vor.

## R as Click-Tool
## the following code works correctly only in R-GUI and best with R --sdi
library(Rcmdr)


#############

## reading xls data
## most flexible package: XLConnect

## Java needs 2GB for scanning 6*100'000 doubles in xlsx-file
## xls is less memory consuming
## cf. http://www.mirai-solutions.com/site/index.cfm?id_art = 69890&actMenuItemID = 31456&plainerr = yes&vsprache = EN

options(java.parameters = "-Xmx2048m")

library(XLConnect)
readWorksheetFromFile("//bordeaux/staff/lore/public/Prionics/Tuberculin/data/TUB11006B_Ba.xls", sheet = 1)





## alternative
library(gdata)

dataWBi <- read.xls(FName,
                    sheet = 2, blank.lines.skip  =  FALSE,
                    fill = TRUE, as.is = TRUE, na.strings = c(" ", ""))


## reading Access data
library(RODBC)

pathIn <- "W:\\admin\\finanzen\\idp-projekte.mdb"

## only usable with 32-bit Windows!!!
## see note in help
DB <- odbcConnectAccess(pathIn)

## or for MS Office 2010
pathIn <- "W:\\admin\\finanzen\\test.accdb"
DB <- odbcConnectAccess2007(pathIn)

##sqlTables(DB)
dat <- sqlFetch(DB, "Sel_Aufwand_Ertrag_pro_Projekt_MA_Quartal",
                as.is = TRUE)
close(DB)

dim(dat)



## more data exchange packages
library(ROracle)
library(RSQLite)

library(RMySQL)
## tips for compiling RMySQL:
## http://www.ahschulz.de/2013/07/23/installing-rmysql-under-windows/

names(dat)
ncol(dat)
nrow(dat)
dat[1:20, c(2:3, 11:15)]
dat[100:105, c(2, 70:77)]

close(XLS1)
## Quelle ist in R zwar nicht mehr ansprechbar aber in Excel immer noch
## schreibgeschützt

###
## reading data via Proxy
library(help = utils)
download.file(url, destfile, method, quiet = FALSE, mode = "w",
              cacheOK = TRUE)

## Latex goodies
## cf Sweave-manual
?SweaveUtils

## for Latex-Tables
library(xtable)
?xtable

## R and Open Office
## cf. Rnews 6/4 (2006)
library(odfWeave)

##################
## Robust methods


## M estimator for location and scale
library(MASS)
hubers()

library(sfsmisc)
library(help = sfsmisc)

huberM()  ## generalized Huber M-Estimator of location


## robust regressions
## !!!!! the new, recommended library!!!!!
library(robustbase)
library(help = robustbase)
## see X:\Public\BAFU\Fehlerrechnung


## alternative packages
library("MASS")
rlm(method = "MM")

## help yourself with mallows type regression -> RAIRMO


######################

## Mallows type regression
x.h <- 1-hat(model.matrix(y~x, dat = dat1))
rlm(y~x, dat = dat1, weights = x.h, wt.method = "case")

## 1-h(ii)
r.h <- 1-hat(model.matrix(NO ~ carType + age + site *(aS + v),
                          data = dat3))

## model selection rfmodsel does not yet work with rlm!!
source("E:\\lor\\S\\RobMeth\\myrlm.R")

## mallows regression = general regression M-estimation
## only good when at most 2 continuous variables
## with leverage points are present
## weight = r.h or sqrt(r.h)
r.n00 <- rflm(NO ~ carType + age + site *(aS + v), data = dat3,
              method = "M", maxit = 200, weights = r.h, wt.method = "case")


x <- 1:200
y <- x + rnorm(200)
xy <- data.frame(x, y)

summary(lm(y~x, xy))

ll <- 11
LM <- matrix(rep(NA, 3*ll), ncol = 3)
RLM <- matrix(rep(NA, 3*ll), ncol = 3)
RLMM <- matrix(rep(NA, 3*ll), ncol = 3)

## Offset as a function of the outlier
for (i in 1:ll) {
    xy$y[100] <- 99 + i
    LM[i, ] <- coef(summary(lm(y~x, xy)))[1, 1:3]
    RLM[i, ] <- coef(summary(rlm(y~x, xy)))[1, 1:3]
    RLMM[i, ] <- coef(summary(rlm(y~x, xy, method = "MM")))[1, 1:3]
    summary(lm(y~x, xy))$r.squared
}

plot(LM[, 1], ty = "l", ylim = c(0, 0.15))
lines(RLM[, 1], col = "blue")
lines(RLMM[, 1], col = "red")

str(summary(lm(y~x, xy)))

## robuster Lokationsschätzer
huber()

## Vorsicht: hubers() gibt defaultmässig zu wenig robuste Erwartungswerte

## no tests implemented for modelselection in MASS
## alpha-version
source("E:\\lor\\S\\RobMeth\\rflm.R") ## modified package MASS

rfmodsel(r.00,
         NO ~ carType + age + site + aS + v,
         test = "Deviance-type")

## more robust methods in
library(help = robustbase) ## official R-cracks
library(help = robust)     ## S + -Version
library(help = roblm)
library(help = wle)
library(wle, help = TRUE)

library(help = multinomRob)


x <- 0:10
y <- x + rnorm(11)
xy <- data.frame(x, y)
lm(y~x, xy)
lm(as.formula("y~x"), xy)
lm(formula("y~x"), xy)

## Bestimmung von Hebelpunkten
?influence.measures
influence.measures(lm.object)
lm.influence(lm.object)$hat

## this works also with rlm etc.
hatvalues(model.matrix(lm.object))

## partial residual = added variable = adjusted variable plots
## works for lm(), glm(), linear terms of mgcv:::gam()
## does NOT work with gam:::gam or mgcv:::gam objects!!
x <- 0:10
y <- x + x^2 + rnorm(11)
xy <- data.frame(x, y)
r.lm <- lm(y~x, xy)
termplot(r.lm, partial = TRUE, se = TRUE, main = TRUE, smooth = panel.smooth)


## oder
library(car)
cr.plot(r.lm, x)


## model selection
r.lm03 <- step(r.lm02, k = log(nrow(datX.d))) ##BIC
r.lm03 <- step(r.lm02, k = 2*log(log(nrow(datX.d)))) ##Hannan-Quinn
r.lm03 <- step(r.lm02, k = 2) ##AIC

## More sophisticated functions for model selection
library(help = leaps)

## residuals
TA.plot(q.rlm,
        cex.main = 1.2, labels = "*", main = "q.rlm",
        parSmooth = list(lwd = 2.5, lty = 4, col = 3), show.call = FALSE)

## the square root gives better distributions for s.
## Note, that only the sqared relative values can be discussed and
## NOT the absolute as E(xi^2) = const.*E(abs(xi)) for normal distribution!
TA.plot(q.rlm, fit = fitted(q.rlm), res = sqrt(abs(resid(q.rlm))),
        cex.main = 1.2, labels = "*", main = "q.rlm",
        ylab = "sqrt(abs(resid(q.rlm)))",
        parSmooth = list(lwd = 2.5, lty = 4, col = 3), show.call = FALSE)

scatter.smooth(fitted(q.rlm), sqrt(abs(resid(q.rlm))))


Image(fitted(q.rlm), resid(q.rlm))
abline(h = 0)
lines(loess.smooth(fitted(q.rlm), resid(q.rlm)), col = "red")

sfsmisc:::p.res.2x

## aspect is chosen so that median absolute slopes are 45-degree
lattice:::banking



#############
####Contrasts

                                        # Default contrasts:
options()$contrasts
                                        #            factor      ordered
                                        # "contr.treatment" "contr.poly"


                                        #Testdatensatz erzeugen
kon <- data.frame(x = rep(c("A", "B", "C"), rep(10, 3)),
                  y = rep(c(3, 10, 20), rep(10, 3)) +  rnorm(30))
kon$x <- as.factor(kon$x)
kon
levels(kon$x)

## generate factor levels
ff <- gl(3, 10, labels = c("A", "B", "C"), ordered = FALSE)
levels(ff)
ff

ff <- gl(3, 1, 30, labels = c("A", "B", "C"), ordered = FALSE)
ff

## recodes factor and reorders levels
ff <- factor(ff, levels = c("B", "A", "C"))
ff
unclass(ff)
as.integer(ff)
as.double(ff)

ff <- as.character(ff)
is(ff)

dummy.coef(aov(y~x, data = kon))
dummy.coef(aov(y~x, data = kon))[[2]] + dummy.coef(aov(y~x, data = kon))[[1]] #QED

r.def <- lm(y~x, data = kon)

## dummy.coef funktioniert nur für contrast = contr.treatment
dummy.coef(r.def)
dummy.coef(r.def)[[2]] + dummy.coef(r.def)[[1]]
summary(r.def, cor = FALSE)
aov(y~x, data = kon)

                                        # Mittelwerte der Faktorstufen
r.def1 <- lm(y~x-1, data = kon)
dummy.coef(r.def1)

## A = 0, keine eigentlichen Kontraste, da nicht orthogonal
## und Summe der Koeffizienten nicht 0!
r.treat <- lm(y~C(x, treatment), data = kon)

                                        # A + B + C = 0, nur othogonal, falls ausbalancierter Datensatz!
r.poly <- lm(y~C(x, poly), data = kon)
r.sum <- lm(y~C(x, sum), data = kon)
r.helm <- lm(y~C(x, helmert), data = kon)

                                        # Explizite Berechnung der Kontraste aus A, B, C bzw. mit Summary
                                        # A.T = 0, B.T  =  B-A, etc.
contr.treatment(3)
summary(r.treat, cor = FALSE)$coefficients
dummy.coef(r.def)[[2]]-dummy.coef(r.def)[[2]][1]
contr.treatment(3) %*% summary(r.treat, cor = FALSE)$coefficients[2:3]

                                        # Polynomiale Kontraste machen nur bei äquidistanten geordneten Faktoren
                                        # mit ausbalancierten Daten Sinn
contr.poly(3)
summary(r.poly, cor = FALSE)$coefficients
dummy.coef(r.def)[[2]]
contr.poly(3) %*% summary(r.poly, cor = FALSE)$coefficients[2:3]

                                        # Summenkontraste:
                                        # A.S = A, B.S = B, C.S  =  0-A.S-B.S
contr.sum(3)
summary(r.sum, cor = FALSE)$coefficients
dummy.coef(r.def)[[2]]
contr.sum(3)%*%summary(r.sum, cor = FALSE)$coefficients[2:3]

                                        # mit verallgemeinerten Inversen können die Kontraste mit
                                        # dummy-Koeffizienten ausgedrückt werden:
library(haplo.score)
Ginv(contr.sum(3))$Ginv %*% dummy.coef(r.def)[[2]]

## cf
r.sum

t.kon <- kon
contrasts(t.kon$x) <- contr.sum(3)
lm(y~x, data = t.kon)

                                        # Helmert-Kontraste
contr.helmert(3)
summary(r.helm, cor = FALSE)$coefficients
dummy.coef(r.def)[[2]]
contr.helmert(3) %*% summary(r.helm, cor = FALSE)$coefficients[2:3]

## Kontraste
t.contr <- matrix(rep(-1, 22*22), ncol = 22)
diag(t.contr) <- 17
t.contr <- cbind(c(rep(1, 17), rep(-17/5, 5)), t.contr[, -c(1:2)])
r.04 <- lm(PS ~ MO + C(stao, t.contr, 20), data = daten, weights = 1/MO^2)


letfac <- factor(rep(rev(letters), 2))
letfac[as.character(letfac)>"j"]
letfac[levels(letfac)[letfac]>"j"]  ##[letfac] works like integer

###
ff <- gl(3, 10, labels = c("A", "B", "C"), ordered = FALSE)
is(ff)

M <- matrix(ff, ncol = 3, byrow = TRUE)
is.factor(M)  ## kein Faktor mehr!!

Fun <- function(x, nam) {
    tab <- table(x)
    y <- rep(0, length(nam))
    names(y) <- nam
    y[nam] <- tab[nam]
    y[is.na(y)] <- 0
    return(y)
}

apply(M, MAR = 1, FUN = Fun, nam = unique(as.vector(M)))

##
ff <- factor(letters[1:4])
ff
ff[-2]
ff[-2, drop = TRUE]

ff1 <- factor(ff, labels = letters[5:8])
ff1

df <- data.frame(a = 12:15, b = 22:25)
df > 20
is(df)

mat <- matrix(sample(1:12), ncol = 3)
mat
as.vector(mat)

mat = =  rev(mat)

as.logical(mat)
is.na(mat)
mat[1:3, 1:3]
mat[1:3]

apply(mat, MAR = 1, function(x) x > 1:3)
t(apply(mat, MAR = 1, function(x) x > 1:3))
mat > 1
mat > 2
mat > 3

sum(mat>1)
mat[mat>1] <- 99

mat[5] <- 4
mat


## for sparse matrices
library(Matrix)

## power of matrices
library(expm)
mat %^% 2
mat %*% mat  ## QED

## Define a named vector
v <- c(a = 1, b = 12)
is(v)

## or
v <- setNames(c(1, 12), c("a", "b"))
v

## define a list
ll <- as.list(rep(NA, 2))
ll[[1]] <- letters
ll[[2]] <- 1:10
ll

## alternatively, a list may be seen as vector of lists
ll <- as.list(rep(NA, 2))
ll[1] <- list(letters)
ll[2] <- list(1:10)
ll

ll[[3]] <- "third element"
ll

c(ll, "fourth element")
list(ll, "fourth element")

ll1 <- as.list(rep(NA, 2))
ll1[[1]] <- 100
ll1[[2]] <- 1:10
ll1[1:2]
sapply(ll1, sum)

########################
## Bestimme das beste Subset der erklärenden Variablen in linearer
## Regression
library(help = leaps)
library(leaps)

## Variance Inflation Factor zum Bestimmen von
## fast kollinearen erklärenden Variablen:
library(DAAG)
library(CAR)
library(design)

leap()

## create design matrix
data(trees)
ff <- log(Volume) ~ log(Height) + log(Girth)
str(m <- model.frame(ff, trees))
mat <- model.matrix(ff, m)

## weighted regressions
## weights in lm() and gam() are
## prior weights = case weights = 1/var()


##################
## differences between matrices und data.frames
mat <- matrix(nrow = 3, ncol = 2)
mat <- row(mat) * col(mat)
mat
mat[, 1]
mat[, 1, drop = FALSE]
as.matrix(mat[, 1])

mat[mat>2] <- 0
mat

mat %*% t(mat)
mat[, 1] %*% mat
## vector : =  dim(1, n), contrary to math def!

M <- matrix(c(1:4, 1:2, NA, NaN), nrow = 2)
colnames(M) <- letters[1:4]
rownames(M) <- c("A", "B")

fix(M)  ## editor for matrices and dataframes

M
M > 1
M > c(0, 3) ## compares per column
is.na(M)
any(is.na(M))
all(is.na(M))

## gives unique columns back!
unique(M, MAR = 2)

is.na(M)
is.finite(M)
is.data.frame(M)

M
which(M > 1, arr.ind = TRUE)
which(M > 1)

M[2, 2] <- 5
M[3:4]
M[3:4] <- 6
M[M < 6] <- 1
M

## but
M <- matrix(c(1:4, 1:2, NA, NaN), nrow = 4)
colnames(M) <- LETTERS[1:2]
rownames(M) <- letters[1:4]
M

as.matrix(M)

DF <- as.data.frame(M)
is.vector(DF)

DF[3:4]
DF[DF < 6]
M[M < 6]
log(DF)
sum(DF)
sum(M)
apply(M, MARGIN = 1, sum)

colnames(DF)
rownames(DF)

is.data.frame(log(DF))
is(log(DF))
is.na(DF)

unlist(DF)[3:4]

## Appending to matrices and data.frames
cbind(M, E = 5:6)
cbind(DF, E = 5:6)


## Converting all columns into a single vector
M <- matrix(1:4, nrow = 2)
DF <- as.data.frame(M)
is.vector(M)
as.vector(M)

as.vector(DF) ## remains data.frame!!
## but:
unlist(DF)

iris
## or
m.df <- stack(iris) ## matrices must be coerced first to data.frames!!

m.df

## or
split(m.df$values, m.df$ind)

split(M, col(M))

## partially reverse action
unstack(m.df)
as.vector(as.matrix(DF)) ## o.k.

length(M)
length(DF)
nrow(DF)
nrow(M)
dim(M)
dim(DF)
ncol(M)
ncol(DF)

colnames(M)
colnames(DF)

DF[DF < 3] <- -1
DF
M[M < 3]
M

N <- matrix(NA, ncol = 2, nrow = 3)
str(N)

N <- matrix(integer(), ncol = 2, nrow = 3)
str(N)

N <- matrix(double(), ncol = 2, nrow = 3)
str(N)

df <- data.frame(aVeryLongVariableName = 1:9, v = 11:19)
df$a


## ----------------------------------------
## Reshaping

## Standard procedure
head(Indometh)
wide  <-
    reshape(Indometh,
            v.names   = "conc",
            idvar     = "Subject",
            timevar   = "time",
            direction = "wide")
wide
str(wide)
## for more examples see Lattice-Example.R

long  <-
    reshape(wide,                                ## Vorsicht! data erwartet einen Dataframe, KEINE Matrix!
            varying = names(wide)[-1],           ## Name der beobachteten Variable(n) im Format "wide", deren Inhalt
                                                 ## in den Variable(n) v.names im Format "long" gespeichert werden.
            v.names = "conc",                    ## Name(n) der Beobachtungsvariable(n) im Format "long".
            times = substr(names(wide)[-1], 6, 99), ## Individuelle Zeit / Attribut einer Beobachtung im Format "wide",
                                                 ## welche in der (den) Variable(n) 'timevar' im Format "long"
                                                 ## gespeichert werden. Meistens sind die Attribute Teil des Namens der
                                                 ## Beobachtungsvariable(n) im Format "wide".
            timevar = "Time",                    ## Variable im Format "long", welche unterschiedliche Beobachtungen
                                                 ## des gleichen Individuums im Format "long" differenziert.
            ids     = wide$Subject,              ## Diese Werte werden in der(den) Variable(n) idvar
                                                 ## im Format "long" gespeichert.
            idvar   = "Subject",                 ## Name(n) der Variable(n) im Format "long" und "wide", welche das
                                                 ## Individuum eindeutig identifiziert (identifizieren).
            direction = "long")
str(long)

## ----------------------------------------
require(tidyr)
## long to wide
Indometh.wide  <-
    spread(data  = Indometh,
           key   = time,
           value = conc)
Indometh.wide
str(Indometh.wide)

## wide to long
Indometh.long  <-
    gather(data  = Indometh.wide,
           key   = time,
           value = conc,
           -1)
str(Indometh.long)

long2  <-
    gather(data  = wide,
           key   = time,
           value = conc,
           -1)
str(long2)

## ----------------------------------------
require(reshape2)
x <- c("a_1", "a_2", "b_2", "c_3")
vars <- colsplit(x, "_", c("trt", "time"))
vars
str(vars)

##
head(Indometh)
Indometh.long <- melt(Indometh,
                      id.vars = c("Subject", "time"),
                      measure.vars = "conc")
head(Indometh.long)

## transform back to almost original data.frame
## Exception: Subject is now an ordered factor!
Ind <- dcast(Indometh.long, Subject + time ~ variable)
Ind
str(Ind)

## 2d array (matrix), x = Subject, y = time, z = variable
acast(Indometh.long, Subject ~ time ~ variable)
acast(Indometh.long, value.var =  "value", Subject ~ time ~ variable)
acast(Indometh.long, value.var =  "value", Subject ~ time)

dcast(Indometh.long, Subject ~ variable, fun.aggregate = mean)

acast(Indometh.long, Subject + time ~ variable)
acast(Indometh.long, variable ~ Subject + time)

names(airquality) <- tolower(names(airquality))
aqm <- melt(airquality, id = c("month", "day"), na.rm = TRUE)
head(airquality)
head(aqm)
str(aqm)

dcast(aqm, day ~ variable)
dcast(aqm, day ~ variable, fun.aggregate = mean)
dcast(aqm, day ~ .,
      fun.aggregate  =
      function(x) {
          if(sum(!is.na(x))>0) min(x, na.rm = TRUE) else -9999
          })
min(aqm$value[aqm$day =  = 1])  ## QED

acast(aqm, day ~ month ~ variable)


## ----------------------------------------
##

## data.frame and characters
test <- data.frame(L = letters[1:10], N = 1:10)
sapply(test, is)
paste(test$L, "n", sep = "")
as.double(test$L)  ## [1]  1  2  3  4  5  6  7  8  9 10

test <- data.frame(L = I(letters[1:10]), N = 1:10)
sapply(test, is)
test
paste(test$L, "n", sep = "")
as.double(test$L)  ## [1] NA NA NA NA NA NA NA NA NA NA

test <- data.frame(L = letters[1:10], N = 1:10, stringsAsFactors  =  FALSE)
sapply(test, is)
test
paste(test$L, "n", sep = "")
as.double(test$L)  ##  [1] NA NA NA NA NA NA NA NA NA NA

sapply(data.frame(test, test), is)

## manipulation of data.frame
library(Hmisc)
upData()

## creating and manipulation lists
L <- list(A = 2, B = letters[4:5])
K <- list(C = 2, D = 4:5)

L1 <- list(L, K, L)
L1
L1[[2]]

c(L, list(L))

str(list(L, L, L))
c(L, K, L, recursive  =  TRUE)
c(L, K, L, recursive  = FALSE)
str(c(L, L, L))

str(rep(L, 6))
## but:
str(rep(list(L, L), 3)) ## for automation!!

## another way to generate automatically lists
LL <- vector("list", 3)
for (i in 1:3) LL[[i]] <- L
str(LL)

##----------------------------------------
## R-GUIs
## editing matrix
fix(M)
A <- edit(M)

select.list(c("A", "B", "C"))

##############
## matrix calculation
M <- matrix(1:4, nrow = 2)
sweep(M, MAR = 2, c(3, 2), "*")

## but
M*c(3, 2)
sweep(M, MAR = 1, c(3, 2), "*")

outer(c(1, 4), c(2, 3))
c(1, 4) %*% c(2, 3)
c(1, 4) * c(2, 3)
t(c(1, 4)) * t(c(2, 3))
(c(1, 4)) * t(c(2, 3))
t(c(1, 4)) * (c(2, 3))

M <- outer(c(1, 4), c(2, 3))
M

## scalar product
kronecker(4, M)

kronecker(M, M)

## block diagonal matrix
kronecker(diag(2), M)

## primitiv imputation
M <- matrix(1:12, ncol = 3)
M[1, 2] <- NA
M[2, 3] <- NA
M
xm <- apply(M, MAR = 2, mean, na.rm = TRUE)
xm

for (ii in 1:nrow(M)) {
    jj <- is.na(M[ii, ])
    if (sum(ii)>0) M[ii, jj] <- xm[jj]
}

M


###############
## na.omit funktioniert nicht mit Listen!!
na.omit(c(3, 5, 1, NA, 99))
na.omit(data.frame(x = c(1, 2, 3), y = c(0, 10, NA)))
na.omit(list(x = c(1, 2, 3), y = c(0, 10, NA)))

#################

dat <- 1:10
fac <- rep(c("A", "B"), 5)

res <- aggregate(data.frame(res = dat), by = list(fac = fac), FUN  =  range)
str(res) ## a data.frame with matrix res!!
res$res
res[res$fac =  = "A", ]
res

colnames(res$res) <- c("min", "max")
res
data.frame(res$fac, res$res)

#########################

## Digesting two lists
x <- list(a = 1:7, b = 14:28)
y <- list(a = 1:5, b = 6:10)
z <- list(a = 1:5, b = 6:10)

lapply(x, mean)
lapply(y, mean)
lapply(z, mean)
lapply(X = seq(along = x), FUN = function(i, c, d) {mean(c[[i]]) * mean(d[[i]])}, x, y)

mapply(FUN = function(c, d) {mean(c) * mean(d)}, x, y)
mapply(mean, x) * mapply(mean, y)

lapply(x,
       function(x) {
           x[x < 15] <-  2
           return(x)})

x <- c(3, 8)
y <- 10:15
lapply(seq(along = x), FUN = function(i, x, y) x[i] + y, x, y)
unlist(lapply(seq(along = x), FUN = function(i, x, y) x[i] + y, x, y))

x <- 0:4
lapply(X = seq(along = x), FUN = function(i, x) 0:x[i], x)

p <- list(a = 1:7, b = 3:15)
lapply(X = p, FUN = function(x) 1:length(x))


xyz <- data.frame(x = c(rep("A", 50), rep("B", 50)),
                  y = rep(c(rep("a", 25), rep("b", 25)), 2),
                  z = rnorm(100),
                  Z = 100*rnorm(100))
aggregate(xyz$z, by = list(xyz$x), mean)
aggregate(z ~ x, data  = xyz, mean)

by(xyz$z, IND = list(xyz$x), mean)
tapply(xyz$z, INDEX = list(xyz$x), mean)

aggregate(xyz$z, by = list(xyz$x), quantile)
tapply(xyz$z, INDEX = list(xyz$x), quantile)

aggregate(xyz[, c("z", "Z")], by = list(xyz$x), median)

t.res <- aggregate(xyz$z, by = list(X = xyz$x, Y = xyz$y), median)

xx <- tapply(xyz$z, INDEX = list(xyz$x), quantile)
cnam <- names(xx[[1]])
xx <- t(matrix(unlist(xx), ncol = length(xx)))
colnames(xx) <- cnam
xx

M <- matrix(c(1:6, 10:12), nrow = 3)
tapply(as.vector(M), IND = as.vector(col(M)), FUN = cumsum)

mapply(rep, x = 1:4, times = 4:1)
rep(1:4, 4:1)

vrep <- Vectorize(rep.int)
vrep(1:4, 4:1)

sapply(list(a = 1:4, b = 4:3), FUN = mean)
mapply(FUN = mean, list(a = 1:4, b = 4:3))
lapply(list(a = 1:4, b = 4:3), FUN = mean)


mapply(FUN = "*", 1:4, 4:1)
(1:4)*(4:1)

## recursive lapply
rapply

## permuting dimensions in arrays
aperm()

## data.frame analog zu apply(matrix) benützen
DF <- data.frame(A = c(2, 3, 5), B = c(8, 7, 1))
DF[] <- lapply(DF, function(x) ifelse(x < 3, NA, x))
DF

DF <- data.frame(A = c(2, 3, 5), B = c(8, 7, 1))
DF <- lapply(DF, function(x) ifelse(x<3, NA, x))
DF

#########
X <- data.frame(A = c(letters[1:3], "c"),
                B = 1:4)
Y <- data.frame(A = c(letters[1:3], "c"),
                B = 100:103)
Z <- data.frame(B = 100:103,
                A = c(letters[1:3], "c"))

z <- data.frame(b = 100:103,
                a = c(letters[1:3], "c"))

rbind(X, Y)

## Binding rows by colnames!
rbind(X, Z)

## error, when colnames are not consistent
rbind(X, z)

cbind()
abind()


########
## result = all possible combinations!!!
X <- data.frame(A = c(letters[3:1], "c"),
                B = 1:4)

Y <- data.frame(A = c(letters[1:3], "c"),
                C = 100:103)
X
Y
merge(X, Y)                ## all possible combinations, sorted
merge(X, Y, sort = FALSE)  ## all possible combinations, but NOT in original order of X!!
plyr::join(X, Y)           ## all possible combinations in original order of X



## result = extended observations!!!
X <- data.frame(A = I(letters[5:1]))

Y <- data.frame(A = letters[1:4], C = 100:103)
str(X)
str(Y)
merge(X, Y, all.x = TRUE, sort = TRUE)
merge(X, Y, all.x = TRUE, sort = FALSE)


####


##################################
## excluding named columns
## If the data frame is named df and
##   nms is a vector names of the columns that you want to exclude, then

df[, -sapply(nms, function(x) which(x = =  names(df)))]

## Create a data frame from all combinations of the
## supplied vectors or factors
expand.grid(height = seq(170, 175), weight = seq(60, 65),
                 sex = c("Male", "Female"))
expand.grid(X = c("A", "B"), Y = c("a", "b"))

## find starting index for interval in which value tt lies
N <- 10
X <- sort(round(rt(N, df = 2), 2))
tt <- c(-100, seq(-2, 2, len = 21),  + 100)
it <- findInterval(tt, X)
tt
X
it

#######
## reelle Zahlen Intervallen zuordnen
cut()

######################
## lme models

library(nlme)
data(Orthodont)
plot(Orthodont)
formula(Orthodont)
summary(Orthodont)

## complex example for augPred
r.lme <- lme(Orthodont)
r.lm <- lm(distance ~ age, data = Orthodont)

plot(augPred(r.lme, ~age),              ##
     groups = augPred(r.lme, ~age)$.type,
     ## selects according to original - predicted

     panel = function(x, y, subscripts, groups, ...){
       set.seed(1313)
       panel.abline(lm(y[groups[subscripts] = =  "original"]~
                        x[groups[subscripts] = =  "original"]),
                    col = "darkgreen", lwd = 2) ## regression on panel data
       panel.abline(r.lm, col = "red", lwd = 2)  ## over all regression
       panel.superpose.2(x, y,
                         groups = groups,
                         subscripts = subscripts, type = c("l", "p"), lwd = 2)
                                           ## data points and lme results
     })


## Plots ordered by mean!
## default: ordered by maximum (cf. Arg. FUN in groupedData)
x <- aggregate(Orthodont$distance, by = list(S = Orthodont$Subject), FUN = mean)
x <- x[1:16, ]
x[order(x$x), ]

## Ordering along a matrix
mat <- matrix(sample(1:5, 20, replace = TRUE), ncol = 2)
mat[order(mat[, 1], mat[, 2]), ]

## This is extensible to multiple columns
matSplit <- split(mat, col(mat))
mat[do.call(order, matSplit), ]


## Plot ohne grauen Hintergrund: geht nur mit PDF defaultmässig!!
## vgl. auch Trellisplot weiter oben
trellis.device(device = "pdf",
               file = "test.pdf")
plot(groupedData(PS ~ MO|stao, data = daten), aspect = 1)

## analoger Plot, aber ohne Linien
xyplot(PS ~ MO|stao, data = daten, aspect = 1, type = "p")

## Daten zusammenstellen, wobei der Faktor stao in org geschachtelt ist
daten1 <- groupedData(PS ~ MO|org/stao, data = daten)


daten1 <- groupedData(PS ~ MO|org/stao, data = datenX)

## Randomeffekte für Intercept und Steigung von stao %in% org
r.1 <- lme(PS~MO, random = ~MO|org/stao, data = daten1)

## Randomeffekte für Steigung von stao %in% org
r.2 <- lme(PS~MO, random = ~MO-1|org/stao, data = daten1)

########
## lmList-Auswertung: 1 regression pro Standort
r.3 <- lmList(log(PS)~log(MO)|stao, datenX)

## Plotten der Intervalle der Koeffizienten
plot(intervals(r.lmList.log1), main = "Test")


## Modellwahl bei lme
## Voraussetzung: Normalverteilung
## REML zur Berechnung des SE und der CI
## ML für Tests zwischen nested models
##
## Merke: Bei Signifikanztests für einzelne fixed effects geben
## die conditional t-Tests (summary bei REML) und conditional
## F-Tests (anova.lme(...., type = "marginal") realistischere Werte als
## ML-Ratio-Tests! (Pinheiro, p. 91)

## nonlinear fitting
nls(prPN~ a0 + a1*
    log(Bus + b2*MR + b3*PW + b4*LW + LKW1 + LKW2),
    start  =  c(a0 = -7, a1 = 0.5, b2 = 1,
      b3 = 1, b4 = 1),
    dat, weights = r.rnls103$w, trace = TRUE)

## fitting monotonely increasing step function
?isoreg

cdplot(Species~Petal.Length, data = iris)


####
data(ToothGrowth)
attach(ToothGrowth)
interaction.plot(dose, supp, len, fixed = TRUE)
interaction(dose, supp)
dose <- ordered(dose)
windows()
interaction.plot(ordered(dose), supp, len, fixed = TRUE, col = 2:3, leg.bty = "o")
detach()

####
## interaction
a <- gl(2, 4, 8)
b <- gl(2, 2, 8, label = c("ctrl", "treat"))
s <- gl(2, 1, 8, label = c("M", "F"))
interaction(a, b)
interaction(a, b, s, sep = ":")

###
## aggregate for lists
require(stats)
data(warpbreaks)
attach(warpbreaks)
by(warpbreaks[, 1:2], tension, summary)
by(warpbreaks$breaks, list(wool, tension), summary)
detach(warpbreaks)

## aggregate for data.frame and ts
## aggregation.ts and aggregation.data.frame give same results
x <- rnorm(96)
cbind(ts = aggregate(ts(x, freq = 12), nfreq = 1, FUN = min),
      df = aggregate(x, by = list(t = rep(1:8, rep(12, 8))), FUN = min)$x)


source("E:\\lor\\R\\moving.aggregate.R")
moving.aggregate()
filter()  # moving mean
runmed()  # moving median

library(help = gregmisc)
library(gregmisc)
running()

########################################
## constructing tables
Aye <- sample(c("Yes", "Si", "Oui"), 177, replace = TRUE)
Bee <- sample(c("Hum", "Buzz"), 177, replace = TRUE)
Sea <- sample(c("White", "Black", "Red", "Dead"), 177, replace = TRUE)
A <- table(Aye, Bee, Sea)
A

margin.table(A)
margin.table(A, 1)
addmargins(A)

ftable(A)
as.data.frame.table(A)

TAB <- data.frame(Aye, Bee, Sea)
xtabs(~Aye + Sea, TAB)


#########################################
## Time and Date
## cf. Rnews_2001-2.pdf, S. 8
## latest example for import: Moleno-01.R for meteo

Sys.timezone()           ## "Westeuropäische Sommerzeit"
Sys.getlocale("LC_TIME") ## "German_Switzerland.1252"
Sys.getenv("TZ")         ## ""

Sys.getlocale("LC_ALL")


## Sys.setenv(TZ = "GMT")
Sys.setenv(TZ = "")


## Length of Months in days
c(31, 28, rep(c(31, 30), 2), 31, rep(c(31, 30), 2), 31)

## POSIXct represents the (signed) number of seconds
## since 1.1.1970 as a numeric vector
## takes date-time-string as GMT if timezone is not valid,
## takes date-time-string as GMT shifted by n hour when tz = xxx-n,
## takes date-time-string as local tz when tz = ""
## tz attribute is just printing information
## prints by default in timezone of computer system
## preferred for data frames and for calculating:
attributes(as.POSIXct("2000-1-31"))
unclass(as.POSIXct("1970-1-1", tz = "UTC"))
unclass(as.POSIXct("1970-1-1", tz = ""))

## remove timezones
tim <- c(as.POSIXct("2000-1-31"), as.POSIXct("1970-1-1", tz = "UTC"))
tim
print(tim, tz = "UTC")

## recreate timezone
attr(tim, "tzone") <- "UTC"
tim

## ****
## time stored and printed as Westeuropäische Sommerzeit
## "2004-3-28 02:00" does not exist in DST!!'
## "2004-10-31 02:00" is ambiguous

tim.char <- c("2004-3-28 00:00",  ## 1 Normaltime
              "2004-3-28 01:00",  ## 2 Normaltime
              "2004-3-28 02:00",  ## 3 Normaltime = = "2004-3-28 03:00" DST
              "2004-3-28 03:00",  ## 4 DST
              "2004-10-31 01:00", ## 5 DST
              "2004-10-31 02:00", ## 6 DST
              "2004-10-31 02:00", ## 7 Normaltime
              "2004-10-31 03:00") ## 8 Normaltime
ct.CET  <-
  as.POSIXct(tim.char, format = "%Y-%m-%d %H:%M", tz = "")
data.frame(tim.char, ct.CET)

attributes(ct.CET)
attr(ct.CET, "tzone")

str(ct.CET)

## ****
## time stored and printed in GMT, NO DST!!
## MEZ-1 gives rubbish during DST !!??
## see work around in next paragraph
ct.GMT  <-
  as.POSIXct(tim.char, format = "%Y-%m-%d %H:%M", tz = "GMT")
ct.UTC  <-
  as.POSIXct(tim.char, format = "%Y-%m-%d %H:%M", tz = "UTC")
ct.UTC1  <-
  as.POSIXct(tim.char, format = "%Y-%m-%d %H:%M", tz = "UTC-1")

data.frame(tim.char, ct.GMT, ct.UTC, ct.UTC1)
## warnings, because UTC + 1 is no official time. Beware sign!!

ct.UTC-ct.GMT
## Time differences in secs
## [1] 0 0 0 0 0 0 0 0       QED

ct.CET-ct.GMT
ct.CET-ct.UTC
##  -1 -1 NA -2 -2 -2 -2 -1 QED
## "2004-03-28 02:00" invalid in CET!
## "2004-10-31 02:00" interpreted as CET DST!

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## to be sure, that numbers are in the proper units!!!
as.numeric(ct.CET-ct.UTC, units = "secs")
as.numeric(ct.CET-ct.UTC, units = "hours")
as.numeric(ct.CET-ct.UTC, units = "days")
as.numeric(ct.CET-ct.UTC, units = "weeks")
as.numeric(ct.CET-ct.UTC, units = "auto") ## smallest units with number>1



ct.CET-ct.UTC1
## Time differences in secs
## [1]     0     0    NA -3600 -3600 -3600 -3600     0


## CET without day saving time!!
ct.UTC1-ct.GMT
ct.UTC1-ct.UTC
## Time differences in hours -1 -1 -1 -1 -1 -1 -1 -1
## Beware sign!!

print(ct.GMT, tz = "")
print(ct.UTC1, tz = "")

## trick to read in correctly CET normaltime during whole the year
print(ct.GMT-3600, tz = "")
format(ct.GMT-3600, tz = "")

format(ct.GMT, format = "y-m-d h:M", tz = "")
format(ct.GMT, format = "%y-%m-%d %H:%M", tz = "")

Sys.setenv(TZ = "GMT")
as.POSIXct(tim.char, tz = "MEZ-1")
Sys.setenv(TZ = "")

format(as.POSIXct("2000-1-31", tz = "UTC"), tz = "UTC + 1")
format(as.POSIXct("2000-1-31", tz = "UTC"), tz = "UTC + 1")


## ****
## change from GMT to CET without DST
ct.CETnorm <- ct.GMT + 3600
attr(ct.CETnorm, "tzone") <- "CETnorm"

ct.CETnorm - ct.UTC
## Time differences in hours
## [1] 1 1 1 1 1 1 1 1
## attr(, "tzone")
## [1] "CETnorm"

data.frame(tim.char, ct.CETnorm, ct.GMT)

##!!!!!!!!!!!!!!!!!!!!!!!!!!!
## as.POSIXlt does IGNORE tz-attribute when x = character
as.POSIXct(tim.char, tz = "GMT")-
as.POSIXlt(tim.char, tz = "GMT")

## BUT
as.POSIXlt(ct.GMT)-ct.GMT
##!!!!!!!!!!!!!!!!!!!!!!!!!!!

## In GMT stored and printed as Westeuropäische Sommerzeit
Sys.setenv(TZ = "")
print(as.POSIXct("2000-8-31 01:00", tz = "GMT"), tz = "")

## In GMT stored and printed
Sys.setenv(TZ = "GMT")
print(as.POSIXct("2000-8-31 01:00", tz = "GMT"), tz = "")
Sys.setenv(TZ = "")

## Mitteleuropäische Normalzeit
print(as.POSIXct("2000-8-31 01:00", tz = "GMT"), tz = "MEZ-1")

print(as.POSIXct("2000-8-31 01:00", tz = "MEZ-1"), tz = "")
print(as.POSIXct("2000-1-31 01:00", tz = "MEZ-1"), tz = "")

## for all format codes see ?strptime
print(strptime("31.8.2000 01:00", format = "%d.%m.%Y %H:%M", tz = "MEZ-1"),
      tz = "")

as.POSIXct(strptime("31.8.2000 01:00", format = "%d.%m.%Y %H:%M",
                    tz = "MEZ-1"), tz = "")

## Mitteleuropäische Sommerzeit
print(as.POSIXct("2000-8-31 01:00", tz = "GMT"), tz = "")

## tz = "GMT" und tz = "UTC" sind äquivalent
as.POSIXct("2000-8-31 01:00", tz = "UTC")-
as.POSIXct("2000-8-31 01:00", tz = "GMT")

## Westeuropäische Sommerzeit geht 2 h vor: Posix-Konvention = -2h
## (ISO Konvention =  + 2 h im Sommer)
as.POSIXct("2000-8-31 01:00", tz = "")-
as.POSIXct("2000-8-31 01:00", tz = "GMT")

## (ISO Konvention =  + 1 h im Winter)
as.POSIXct("2000-12-31 01:00", tz = "")-
as.POSIXct("2000-12-31 01:00", tz = "GMT")


##----------------------------------------
## sequences of time and date

## start time in daylight saving time
## value: POSIXct
start.dst <- seq(ISOdate(2004, 1, 1, 0, 0, tz = ""),
                 ISOdate(2004, 12, 31, 23, 50, tz = ""),
                 by = "10 min")
table(diff(start.dst))

## start time in normal time
start.nt <- seq(ISOdate(2004, 1, 1, 0, 0, tz = "MEZ-1"),
                ISOdate(2004, 12, 31, 23, 50, tz = "MEZ-1"),
                by = "10 min")
table(diff(start.nt))

i.wiso <- which(strftime(as.POSIXlt(start.nt),
                 format = "%d.%m.%Y %H:%M") = =  "28.03.2004 03:00") + (-12:12)
start.dst[i.wiso]
start.nt[i.wiso]

i.sowi <- which(strftime(as.POSIXlt(start.nt),
                 format = "%d.%m.%Y %H:%M") = =  "31.10.2004 02:00") + (-12:12)
start.dst[i.sowi]
start.nt[i.sowi]

print(start.nt[2], tz = "GMT")

##
d.nt <- julian(start.nt, origin = as.POSIXct("2004-1-1", tz = ""))
table(round(diff(d.nt)*24*6))

d.dst <- julian(start.dst, origin = as.POSIXct("2004-1-1", tz = ""))
table(round(diff(d.dst)*24*6))

table(d.nt-d.dst)

## Vorsicht!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## Sommerzeit wird immer dann berechnet, wenn die Windowseinstellungen
## auf "Uhrzeit automatisch auf Sommerzeit umstellen" gesetzt ist.
## andernfalls bleibt alles bei Normalzeit.
## "2003-3-30 02:00" ist (theoretisch) keine gültige Uhrzeit mit
## MEZ Sommerzeit, gibt aber keinen Fehler in R
## Nach dem Umstellen muss R neu gestartet werden!


## "POSIXlt" is a named list of vectors representing
##  sec 0-61: seconds
##  min 0-59: minutes
##  hour 0-23: hours
##  mday 1-31: day of the month
##  mon 0-11: months after the first of the year.
##  year: Years since 1900.
##  wday: 0-6 day of the week, starting on Sunday.
##  yday 0-365: day of the year.
##  isdst: Daylight savings time flag.
##  Positive if in force, zero if not, negative if unknown.
##  tz argument additional information which is
##     NEVER used for calculation!!!
##  list has to be read as human will do

attributes(as.POSIXlt("2000-1-31"))
attributes(as.POSIXlt("2004-8-1 01:00", tz = "UTC"))
unlist(as.POSIXlt("2004-8-1 01:00", tz = "UTC"))
unlist(as.POSIXlt("2004-8-1 01:00", tz = "GMT"))
unlist(as.POSIXlt("2004-8-1 01:00", tz = "MET-1"))
unlist(as.POSIXlt("2004-8-1 01:00", tz = "MET"))


## Je nach Einstellung des Systems
## "Uhrzeit automatisch auf Sommerzeit umstellen" = ja/nein
## wird in der Liste isdst als 0 oder 1 ausgegeben ???
unlist(as.POSIXlt("2003-3-30 01:00"))
unlist(as.POSIXlt("2003-3-30 02:00"))
unlist(as.POSIXlt("2003-3-30 03:00"))

as.POSIXlt("2000-1-31")
as.POSIXlt("2000-1-31", tz = "UTC")

### converts strings to POSIXt
ch.str <- c("27.03.2004 01:30",
           "27.03.2004 02:00",
           "27.03.2004 03:00")
as.POSIXlt(ch.str, format = "%d.%m.%Y %H:%M")
as.POSIXct(ch.str, format = "%d.%m.%Y %H:%M")

strptime(ch.str, format = "%d.%m.%Y %H:%M")

## substraction of seconds from a lt object gives a POSIX ct object!!
tim <- as.POSIXlt(ch.str, format = "%d.%m.%Y %H:%M")-30*60
tim

## character vector without timezone
format(tim, format = "%d.%m.%Y %H:%M")
format(tim, format = "%y%m")

## "28.03.2004 02:00" does not exist in summertime!!
unclass(as.POSIXlt(c("28.03.2004 01:30",
           "28.03.2004 02:00",
           "28.03.2004 03:00"),
         format = "%d.%m.%Y %H:%M"))

## Convenience wrappers. Gives POSIXct back!
ISOdate(2000, 1, 31)
ISOdatetime(day = 31, month = 1, year = 2000, hour = 0, min = 0, sec = 0, tz = "")

## converts POSIXlt objects to strings
format(tim, format = "%d.%m.%Y %H:%M")

## names of month
?format.POSIXlt
format(ISOdate(2004, 1:12, 1), "%B")
format(ISOdate(2004, 1:12, 1), "%b")
## c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
## c("Jan", "Feb", "Mrz", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")

## names of weekday
format(ISOdate(2004, 1, 4:10), "%A")
format(ISOdate(2004, 1, 4:10), "%a")
## c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su")
## c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa")

format(Sys.time(), "%a %b %d %X %Y %Z")
format(Sys.time(), "%a %b %d %X %Y")

## Everything in Normal Time
NT <- seq(ISOdate(2004, 3, 28, 0, tz = "GMT"),
          ISOdate(2004, 3, 28, 4, tz = "GMT"), by = "30 min")
NT

## result in identical character vector
print(NT, tz = "")
format(NT, tz = "", usetz = TRUE)

## character vector without timezone
format(NT, tz = "")

## change from Normal Time to DST on: 2004-03-28 02:00
seq(ISOdate(2004, 3, 28, 0, tz = ""), ISOdate(2004, 3, 28, 4, tz = ""), by = "30 min")


##----------------------------------------
##----------------------------------------

library(timeDate)
library(help = timeDate)

Sys.getenv("TZ")
Sys.setenv(TZ = "GMT")

?Sys.timeDate
Sys.timeDate()

## All implemented holidays
listHolidays()


## Examples
years <- 2000:2020
## Feiertage in der Schweiz (relevant für Verkehr)
## General Swiss Holidays
holiday(years, c("NewYearsDay", "Easter", "CHAscension", "Pentecost",
                 "CHConfederationDay", "ChristmasDay"))

holiday(years, c("LaborDay", "EasterMonday", "PentecostMonday", "ChristmasEve", "BoxingDay", "DENewYearsEve"))



Easter(years)
GoodFriday(years)
Ascension(years)

RogationSunday(years)
Pentecost(years)
AllSaints(years)
AllSouls(years)
CHSechselaeuten(years)
CHKnabenschiessen(years)

## DaylightSavingTimeRules
## Zurich = Zeitpunkt in UTC, ab welchem neuer Offset zu UTC gilt
## Normalzeit -> Sommerzeit: 02:00 wird zu 03:00
## Sommerzeit -> Normalzeit: 03:00 wird zu 02:00
? DaylightSavingTimeRules

## Umschaltzeiten  Sommer / Winter für ...
Zurich()
NewYork()

library(SwissAir)
data(AirQual)
head(AirQual$start)
## 2004-03-28 01:00:00 change to day saving time
AirQual$start[which(AirQual$start = =  "28.03.2004 01:00") + (-5:5)]
table(substr(as.character(AirQual$start), 12, 13))

timeDate("2004-03-28 00:00:00", zone = "Zurich") ## equal to ...
timeDate("28.03.2004 00:00", format = "%d.%m.%Y %H:%M", zone = "Zurich")

timeDate("2004-03-28 01:00:00", zone = "Zurich")
timeDate("2004-03-28 02:00:00", zone = "Zurich") ## day saving time!
timeDate("2004-03-28 03:00:00", zone = "Zurich") ## day saving time!

timeDate("2004-03-28 00:00:00", zone = "Zurich")-
  timeDate("2004-03-28 03:00:00", zone = "Zurich") ## -2 hours, ok

timeDate("2004-03-28 01:00:00", zone = "Zurich")-
  timeDate("2004-03-28 03:00:00", zone = "Zurich") ## -2 hours, NOT ok!!
## should be -1 hour!!

timeDate("2004-03-28 00:00:00", zone = "Zurich")-
  timeDate("2004-03-28 04:00:00", zone = "Zurich") ## -3h, QED

timeDate("2004-03-28 00:00:00", zone = "GMT")    ## not yet day saving time
timeDate("2004-03-28 01:00:00", zone = "GMT")    ## day saving time!

timeDate("2004-03-28 01:00:00", zone = "GMT", FinCenter = "GMT")

timeDate(Sys.time(), format = "%Y-%m-%d %H:%M", zone = "GMT")


timeDate("28.03.2004 00:00", format = "%d.%m.%Y %H:%M", zone = "Zurich")-
  timeDate("22.03.2004 12:00", format = "%d.%m.%Y %H:%M", zone = "Zurich")
difftimeDate(
   timeDate("28.03.2004 00:00", format = "%d.%m.%Y %H:%M", zone = "Zurich"),
   timeDate("22.03.2004 12:00", format = "%d.%m.%Y %H:%M", zone = "Zurich"),
   "hour")

## change from normal time to DST on: 2004-03-28 02:00
## 1 hour later than 00:00
timeDate("2004-03-28 00:00:00", zone = "Zurich") + 60*60
timeDate("2004-03-29 01:00:00", zone = "Zurich") + 60*60
timeDate("2004-03-29 02:00:00", zone = "Zurich") + 60*60
timeDate("2004-03-29 03:00:00", zone = "Zurich") + 60*60


timeSequence(from = "2003-01-01 00:00:00",
             to = "2003-01-10 00:00:00",
             by = "day")

timeSequence(from = "2003-01-01 00:00",
             to = "2003-01-01 23:50",
             format = "%Y-%m-%d %H:%M",
             by = "10 min")

Sys.getenv("TZ")
Sys.setenv(TZ = "")
Sys.setenv(TZ = "GMT")
Sys.setenv(TZ = "German_Switzerland.1252")

## change from normal time to DST on: 2004-03-28 02:00

## ----------------------------------------
## Normal Time Zurich!!
tim <- timeSequence(from = "2004-03-27 23:00",
                      to = "2004-03-28 04:30",
                      format = "%Y-%m-%d %H:%M",
                      by = "30 min")

format(tim, format = "%d.%m.%Y %H:%M")

format(tim, format = "%d.%m.%Y %H:%M", FinCenter = "Zurich")

Sys.setenv(TZ = "")
as.POSIXct(tim)
Sys.setenv(TZ = "GMT")


format(Sys.timeDate(), format = "%Y-%m-%d")

## Normal Time!!
## ----------------------------------------

## ----------------------------------------
## DST Zurich
tim <- timeSequence(from = "2004-03-27 23:00",
                      to = "2004-03-28 04:30",
                      format = "%Y-%m-%d %H:%M",
                      by = "30 min", FinCenter = "GMT")
## DST Zurich
tim
## ----------------------------------------


##??
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## conversion from Normal Time Zurich to DST Zurich!?
DST <- timeDate(tim, FinCenter = "Zurich")-60*60
DST

## The following gives rubbish!!??
## DST <- timeDate(tim-60*60, FinCenter = "Zurich")

format(timeDate(DST, FinCenter = "Zurich"), format = "%d.%m.%Y %H:%M")
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## conversion from DST Zurich to Normal Time
timeDate(tim-60*60, zone = "GMT", FinCenter = "Zurich")
format(timeDate(tim-60*60, FinCenter = "Zurich"), format = "%d.%m.%Y %H:%M")
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


## at the moment




isBizday(as.POSIXct("2017-07-24"))


## wednesday = 3.
day.of.week(m = 11, d = 12, y = 98)

## ----------------------------------------


a <- julian(as.POSIXlt("1999-1-1 12:00", tz = "UTZ"),
            origin = as.POSIXlt("1999-1-1", tz = ""))
a
24*as.double(a)

a <- julian(as.POSIXct("1999-1-1 12:00", tz = "UTZ"),
            origin = as.POSIXct("1999-1-1", tz = ""))
a
24*as.double(a)

## Y: 4-stelliges Jahr, y: 2-stelliges Jahr
## ->> .leap.seconds gibt alle 22 Schaltsekunden wieder
## ->> strptime für Format-Abkürzungen
a <- julian(as.POSIXct(strptime("10.9.2000", format = "%d.%m.%Y")),
            origin = as.POSIXct(strptime("1.1.00", format = "%d.%m.%y")))

(24*as.double(a)) %%24

## 253 Tage Differenz
julian(as.POSIXct(strptime("10.9.2000", format = "%d.%m.%Y"), tz = "UTC"),
       origin = as.POSIXct(strptime("1.1.00", format = "%d.%m.%y"), tz = "UTC"))

julian(as.POSIXct(strptime("10.9.2000", format = "%d.%m.%Y"), tz = "GMT"),
       origin = as.POSIXct(strptime("1.1.00", format = "%d.%m.%y"), tz = "GMT"))

julian(as.POSIXct(strptime("2.1.2003 04:00", format = "%d.%m.%Y%H:%M"),
                  tz = "UTC"),
       origin = as.POSIXct(strptime("1.1.03", format = "%d.%m.%y"), tz = "UTC"))

as.POSIXct(strptime("10.9.2000 00:00", format = "%d.%m.%Y%H:%M"))

as.POSIXct(strptime("10.9.2000 00:00", format = "%d.%m.%Y"), tz = "UTC")
as.data.frame(as.POSIXct(strptime("10.9.2000 00:00", format = "%d.%m.%Y"), tz = "UTC"))
as.data.frame(as.POSIXct(strptime("10.9.2000 00:00", format = "%d.%m.%Y"), tz = "UTC"))[1, 1]

as.POSIXct(strptime("10.9.2000 00:00", format = "%d.%m.%Y"), tz = "")
as.POSIXct(strptime("10.9.2000 00:00", format = "%d.%m.%Y"), tz = "GMT")

timdat <- seq(ISOdate(2002, 1, 1, 0, tz = ""), ISOdate(2002, 1, 2, 0, tz = ""),
              by = "hour")

timax <- seq(ISOdate(2002, 1, 1, 0, tz = ""), ISOdate(2002, 1, 2, 0, tz = ""),
              by = "2 hours")

windows(10, 5)
plot(timdat, rnorm(25), axes = FALSE, ty = "l", lwd = 2)
axis(1, at = timax,
   lab = formatC(as.POSIXlt(timax)$hour, format = "d", width = 2, flag = 0))
axis(2)
box()

######################
dat <- seq(ISOdate(2002, 1, 1, 0, tz = ""), ISOdate(2003, 1, 1, 0, tz = ""),
              by = "1 day")

windows(15, 10)
plot(dat, sin(as.numeric(dat)/24/3600/30*2*pi),
     ty = "l", xlab = "", ylab = "alpha", axes = FALSE)
box()
axis(2)

## label axis at first of month in language of system
axis.POSIXct(1, dat)

## or e.g., label axis at 15th of month
atx <- seq(ISOdate(2002, 1, 15, tz = ""), ISOdate(2002, 12, 15, tz = ""),
           by = "1 month")

axis.POSIXct(1, at = atx, labels = format(atx, format = "%d.%m"))

## or e.g. special labels
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
lab <- month[as.integer(format(atx, format = "%m"))]

axis(1, at = atx, labels = lab)


## gesetzte System-Variablen, welche mit dem Datum zu tun haben:
Sys.getlocale()

today <- Sys.Date()
format(today, "%d %b %Y")  # with month as a word
tenweeks <- seq(today, len = 10, by = "1 week") # next ten weeks
weekdays(today)
months(tenweeks)
as.Date(.leap.seconds)

## Datums-Sequenzen
seq(as.Date("1995-01-01"), by = "months", length = 13)

## defining columns in data.frame for POSIXct
df <- as.data.frame(matrix(rep(NA, 4), ncol = 2))
df[1, 1] <- as.POSIXct("2000-01-01")
class(df[, 2]) <- "POSIXct"
df[1, 2] <- as.POSIXct("2000-01-01")
df
## Attention: This "trick" does not work when df will be exported by
## write.table()!!??



######################
## alternative Zeit-Ojekte und -Funktionen
## keine Sommerzeit?? Auf jeden Fall ziemlich umständlich
library(help = chron)
library(chron)
dates("1/1/70")  ## month / day / year
times(60)

chron(3.25)

chron(3.25) + 1

detach(package:chron)


library(help = clim.pact)
library(clim.pact)
julday () ## date to julian
caldat()  ## julian to date


######################
## time series

np <- 5
lp <- 168
plot(ts(sin((1:(np*lp))/168*2*pi)*
        (1 + rep(abs(rnorm(np, sd = 0.5)), rep(168, np)))
         + rnorm(np*lp, sd = 0.3),
        start = 0, frequency = 168))


time()    ## Sampling Times of Time Series
window()  ## cut time window out of ts


######################################################################
## (extended) regular expressions
## http://www.evolt.org/article/rating/20/22700/
## http://www.english.uga.edu/humcomp/perl/regex2a.html
## http://www.opengroup.org/onlinepubs/7908799/xbd/re.html
## http://www.csci.csusb.edu/dick/samples/regular_expressions.html#complemented
## ranges e.g. [a-z] discriminate between big and small letters only with perl = T

?regex

sub(" *$", "", c("this one   ", "and that one ", "plus this"))
sub(" *", "", c("this one   ", "and that one ", "plus this"))
gsub(" *", "", c("this one   ", "and that one ", "plus this"))

sub("(is)|(at)", "*", c("this one   ", "and that one ", "plus this"))
sub("is|at", "*", c("this one   ", "and that one ", "plus this"))

as.double(gsub(", ", ".", c("0, 04", "10, 8")))
as.double(sub(", ", ".", c("0, 04", "10, 8")))

## Alphanumeric ASCII only
gsub("[^A-Za-z0-9] + ", "", c("515  a b c   ", "a?-/."))

## Backreferences
## Set an x in front of number which is at the beginning of a word (line)
gsub("^([0-9] + )", "x\\1", c("515  a b c   ", "a?-/."))


## Escaping non-metacharacters:
## \a = BEL, \e = ESC, \f = FF, \n = LF, \r = CR, \t = TAB

## translate Umlaute
gsub("ä", "ae",
     gsub("ö", "oe",
          gsub("ü", "ue", "äöü")))

## strip numerals
gsub("^[0-9 ]*", "", c("1    test", "13test 5"))

gsub("^[ ]*", "", c("    test", "13test"))

## exchange single US-letters
x <- "MiXeD cAsE 123"
chartr("iXs", "why", x)

gsub(", ", ".", c("0, , 04", "10, 8"))
sub(", ", ".", c("0, , 04", "10, 8"))
sub(", ", ".", c("0, , 04", "10, 8", ""))

substring("0, 04", 3, 4)
substring("0, 04", 1, 0)

strsplit("0, 04", ", ")

## cf. www.opengroup.org/onlinepubs/007908799/xbd/locale.html#tag_005_003_002_004
strsplit("  ab    c ", "[[:space:]]")
strsplit("  ab    c ", " ")


strsplit("[1 2 3 4]", "[[:space:][:punct:]]")

grep("^a", c("fritz", "berta", "albert"))  ## ^ beginning of string
grep("a$", c("fritz", "berta", "albert"))  ## $ end of string

## metacharacters must stay within []
grep("[.]", c("fritz", "berta.1", "albert"))

grep("[b-z]", c("a", "A"))
grep("[a-z]", c("a1", "A2"))
grep("[A-z]", c("a1", "A2"))

## All but lower case letters!!!!
grep("[^a-z]", c(";", "a", "A", "1"))

## All but upper case letters!!!!
grep("[^A-Z]", c(";", "a", "A", "1"))

## . exactly one letter
grep("r.t", c("fratz", "frt", "fraatz", "fritz"))

## .* zero or more letters
grep("r.*t", c("fratz", "ft", "fraatz", "fraaatz"))

## a +  1 oder mehr a hintereinander
grep("a + ", c("fratz", "frt", "fraatz", "fraaatz"))

## alle Ausdrücke, welche nicht mit einem Buchstaben beginnen
test <- c("Fritz", "FRT", "fraatz", "fuzzy", "3..", "z4..", "Z4..")
grep("^[^A-z]", test)

## alle Ausdrücke, welche nicht mit einem *kleinen* Buchstaben beginnen
grep("^[^a-z]", test)

## [A-Z] alle Ausdrücke, in welchen mindestens ein Buchstabe vorkommt
grep("[A-z]", c("3..", "a", "b", "Fritz", "FRT", "fraatz", "fuzzy"))

## [A-Z] alle Ausdrücke, in welchen mindestens ein GROSSER Buchstabe vorkommt
grep("[A-Z]", test)

## big and small F are different
grep("F", c("Fritz", "FRT", "fraatz", "fuzzy", "3.."))

## [a-d] alle Ausdrücke, welche mind. 1 Buchstaben zw. a und d enthalten
grep("[a-z]", c("Fritz", "DRT", "frAAtz", "fuzzy", "3..", "a"))
grep("Fr|dr", c("Fritz", "DRT", "frAAtz", "fuzzy", "3.."))   ## | oder

## () "mathematischer" Klammerausdruck
grep("(Fr|AA)[^D]", c("Fritz", "frD", "frAAtz", "fuzzy"))
grep("a", c("Fratz", "DRT", "frAtz", "fuzzy", "3.."))       ##

grep("\\.", c("freeze", "fratz", "fre.z"))     ## find "."
grep("\\*", c("freeze", "fra*tz", "fre.z"))    ## find "*"
grep("\n", c("freeze", "fr  \natz", "fre.z"))  ## find EndOfLine

sub(); gsub ## search and replace by regexpr()


x <- "0001001110001101111000101"
n <- paste((1:nchar(x))%%10, collapse = "")
rbind(x, n)

## find patterns with a "1" at the end of the line preceded by one or
## more "0"
regexpr("10 + 1$", x)

## find first sequence
## starting and ending with 1 and at least one zero in between
regexpr("10 + 1", x)

## find all sequences
## starting and ending with 1 and at least one zero in between
## Note that 23 is no sequence as 23 is the last character of sequence 19!
gregexpr("10 + 1", x)

## Split string into single characters
cbind(unlist(strsplit(x, split = "")))

## find first match in string
regexpr("1 + ", x)
regexpr("2 + ", x)

## find all series of 1s in string
gregexpr("1 + ", x)

## find all sequences of exactly 2 zeros in sequence
gregexpr("0{2}", x)

attr(gregexpr("1 + ", x)[[1]], "match.length")

## find all sequences starting at position 1 or with a 1
## followed by 2 zeros at least
gregexpr("(^|[^0])0{2}", x)

gregexpr("(^0|[^0])0{2}", x)

## replace all sequences of 2 zeros by "x"
rbind(x, gsub("0{2}", "x", x))

## find all series of 1 in string which are longer than 1
gregexpr("1{2, }", x)

## find all series of 1 in string
##   which are longer than 1 and shorter than 4
## attention: index gives the place before the first 1
gregexpr("[^1]1{2, 3}[^1]", x)

x <- "00    0100 111000 110111100101"
n <- paste((1:nchar(x))%%10, collapse = "")
rbind(x, n)

## split strings at spaces:
strsplit(x, "  + ")              ## most elegant on windows 7
strsplit(x, "[^0-9A-Za-z.-] + ") ## the complicated way
strsplit(x, "[[:space:]] + ")    ## the most comprehensive way


x <- gregexpr("1 + ",
              paste(sample(0:1, 100, replace = TRUE), collapse = ""))
attr(x[[1]], "match.length")

## stripping blanks and tabs
gsub("^[[:space:]] + |[[:space:]] + $", "", "\t  a text including leading and trailing spaces  ")

fixef.mod <- c("(Intercept)", "praline2", "praline3", "praline4", "age35-54", "age55-", "gendermale", "runVT0809", "praline2:age35-54", "praline3:age35-54", "age55-:gendermale", "praline2:runVT0809", "age55-:runVT0809", "praline1:gendermale:runVT0803", "age55-:gendermale:runVT0803")

cbind(fixef.mod)

fixef.mod[grep("(praline.$|run.$|gender.$)|(praline.*:run)", fixef.mod)]
fixef.mod[-grep("age", fixef.mod)]

## Herauskommen soll:
## 2 3 4 8 12

## Regular expressions in match()
(x <- c("A and B", "A, B and C", "A, B, C and D", "foobar"))
pattern <- "[[:space:]]*(, |and)[[:space:]]"
## Match data from regexpr()
(m <- gregexpr(pattern, x))
regmatches(x, m)
regmatches(x, m, invert = TRUE)

chartr(old, new, x) ## character translation
tolower(x)          ## translates uppercase to lowercase characters
abbreviate(c("test", "a test"), minlength = 2)
abbreviate(c("test", "testen"), minlength = 2)
abbreviate(c("1test", "2testen"), minlength = 2)

abbreviate(c("Lärchen", "Lurche"), minlength = 2)
abbreviate(c("Zug", "Schwyz"), minlength = 2, )



# Searches for approximate matches to 'pattern'
agrep("460325", "0010011100010")
agrep("4", "0010011100010")
agrep("1111", "0010011100010")

## ----------------------------------------
## Formatting numbers
## Formatting 1000000 as 1'000'000
formatC(1000000, format = "fg", big.mark = "'")
formatC(100, format = "fg", big.mark = "'", width = 10)
formatC(1, format = "fg", big.mark = "'", width = 10)

## But:
formatC(0, format = "fg", big.mark = "'", width = 10)


## Formatting 1 as "01"
formatC(1, format = "d", width = 2, flag = 0)

prettyNum(1, format = "d", width = 2)
formatC(1, format = "d", width = 2)



## see also format.default
format(1000000)
format(1000000, scientific = FALSE, big.mark = "'")
format(as.numeric(0), scientific = FALSE, big.mark = "'", width = 10)

format(c(1e-3, 1e-4))
formatC(c(1e-3, 1e-4), format = "f")
formatC(c(1e-3, 1e-4), format = "f", drop0trailing = TRUE)

formatC(0.45, width = 3)

format(1.45, nsmall = 3)
format(0.0001454, digits = 3, scientific = TRUE)

## Formatting numbers with identical number of digits after decimal point
test <- c(a = 5, b = 0, c = 3, d = 1.23)
formatC(test, width = 6, digits = 2, format = "f")
format(test, width = 6, digits = 2, format = "f")
print(test, digits = 2)

## see also
a <- 999
sprintf("Die Zahl %d wird in den Text eingefügt", a)
sprintf("This is %d in hexadecimal: %X", a, a)


## Formatierter Output für einzelne Zahlen
lapply(c("a", "ABC", "and an even longer one"),
       function(ch) sprintf("10-string `%10s'", ch))

sapply(1:18, function(n)
       sprintf(paste("e with %2d digits = %.", n, "g", sep = ""),
               n, exp(1)))

sapply(1:18, function(n)
       sprintf(paste("e with %02d digits = %.", n, "g", sep = ""),
               n, exp(1)))

## sprintf() for translating messages
gettextf()

?capture.output
(ft <- format(help(package = class)))
str(ft)
## See also E:\ZHAW\Public\R\Advanced R\code\highlights.R


#####################################################################
x <- c(3, -1, 9, 4, -1)
which(x<0)
which.min(x)
which.max(x)
match(-1, x)

pmatch(c("a", "b", "c"), c("bu", "cv", "az"))
pmatch(c("a", "b", "C"), c("bu", "cv", "az"))

match(c("a", "b", "c"), c("b", "c", "a"))

txt.list <- c("xxbaxx", "c", "a")
match("a", txt.list)
grep("a", txt.list)
grep("a", txt.list, value = TRUE)
grep("ba", txt.list, value = TRUE)

## perl must be TRUE!! for look ahead pattern.
## explanation: no letter within a line ^.$ should be preceded by "ba"
grep("^((?!ba).)*$", txt.list, value = TRUE, perl = TRUE)

## equals
txt.list[regexpr("ba", txt.list)<0]

## sorting
is.unsorted(6:1)
sort(6:1)

(ii <- grouping(x <- c(1, 1, 3:1, 1:4, 3), y <- c(9, 9:1), z <- c(2, 1:9)))
rbind(x, y, z)[, ii]  ## identical to
rbind(x, y, z)[, order(x)]

###################################################################
## tips for functions
match.call(get, call("get", "abc", i = FALSE, p = 3))
## get(x = "abc", pos = 3, inherits = FALSE)
ll <- as.list(match.call(get, call("get", "abc", i = FALSE, p = 3)))
str(ll)
ll$""
ll[[2]]
eval(ll$"")

## Sorting ----------------------------------------
x <- c(5, 7, 1, 4)
sort(x)
x[order(x)]

iO <- order(x)
x <- sort(x)
x[iO]

############################################################
## symbolic calculations
myplot  <-
function(x, y)
    plot(x, y, xlab = deparse(substitute(x)),
        ylab = deparse(substitute(y)))

myplot(xy$x, xy$y)

####
L1 <- runif(10)
L2 <- runif(20)
L3 <- runif(30)
L4 <- runif(40)
res <- vector(mode = "list", length = 4)

##**
ss <- paste("log(L", 1:4, ")", sep = "")
for(i in 1:4) res[[i]] <- eval(parse(text = ss[i]))
res

##** oder
for(i in 1:122) {
  nm <- paste("L", i, sep = "")
  assign(nm, log(get(nm)))
}

## search for object of name nam:
x <- get(nam)

## Symbolically encode a given numeric or logical vector or array
ii <- 0:8; names(ii) <- ii
symnum(ii, cut =  2*(0:4), sym = c(".", "-", " + ", "$"))



######################################################################
### GAM models
library(mgcv)
## more stable models than in library(gam)
## but attention when data are correlated

## cf. \\bordeaux\staff\lore\Public\MfMU\Erstfeld-08\EF08-00.R

res.gam  <-
    gam(lNOx ~ s(lref, bs = "tp") +
        s(WS, bs = "tp", sp = 0.05) +  ## manual smoothing!
        s(hour, bs = "cc", k = 24) +
        s(wd, bs = "cc", k = 7) +
        s(mon, bs = "cc", k = 12),
        dat = dat, gamma = 2)

res.gam  <-
    gam(lNOx ~ s(lref, bs = "cs", k = 40 ) +
        s(WS, bs = "cs", k = 40) +
        s(hour, bs = "cc", k = 24) +
        s(wd, bs = "cc", k = 7) +
        s(mon, bs = "cc", k = 12),
        dat = dat,
        gamma = 1,
        control = gam.control(max.tprs.knots = 10000))

res <- gam(LNOx ~
           s(LNOxEm, k = 7)  +
           s(LNOxEmMR, k = 7)  +
           s(dT, k = 7)  +
           te(sqrtWShs, WD, k = c(7, 35)) +
           s(julian, k = 365),
           sp = sp,
           dat = dat,
           all.terms = TRUE)

plot(res.gam, all.terms = TRUE)
## be carefull: character variables should be transformed to factors
## for correct representation of factors!

## see ?s for individual smoothing
## see ?choose.k for definition of s(term, k = )
## see ?smooth.construct.cr.smooth.spec  for cyclic cubic spline s(sp = "cc", ...)

library(gam)   ## back fitting alg.
r.gam10 <- gam(lHVS.tot~lo(lPM10vol) + lNOx + lo(T) +
               T.r + WVs, dat = na.omit(dat),
               control = gam.control(maxit = 50, bf.maxit = 50))
plot(r.gam10, se = TRUE)
plot(r.gam10, se = TRUE, residuals = TRUE)

## partial residuals:
r.g101 <- gam(LPN ~ lo(LPNem) + lo(sqrtWShs10)  +
              julday + lo(WD10) + lo(turb10),
              dat = dat, control = gam.control(maxit = 100, bf.maxit = 100))
r.gp101 <- preplot.gam(r.g101)


## logistic regression
## http://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html
library(MASS)
data(menarche)
?menarche

glm.out <- glm(cbind(Menarche, Total - Menarche) ~ Age,
           binomial(link = logit), data = menarche)
summary(glm.out)
plot(glm.out)

plot(Menarche/Total ~ Age, data = menarche)
lines(menarche$Age, glm.out$fitted, type = "l", col = "red")
title(main = "Menarche Data with Logistic Regression Line Fitted by glm")


## logistic regression with mgcv
library(mgcv)
gam.out <- gam(cbind(Menarche, Total - Menarche) ~ s(Age),
           binomial(link = logit), data = menarche)
summary(gam.out)

plot(gam.out, se = TRUE, residuals = TRUE, pch = 20)

plot(Menarche/Total ~ Age, data = menarche)
lines(menarche$Age, gam.out$fitted, type = "l", col = "red")
title(main = "Menarche Data with Logistic Regression Line Fitted by GAM")


## 3d-Plot with irregular spaced values in x / y - direction
library(akima)
data(akima)
akima.li <- interp(akima$x, akima$y, akima$z)
image  (akima.li)
contour(akima.li, add = TRUE)
points (akima, pch = 3)

plot(r.g101)
plot(r.gp101$"lo(LPNem)"$x, r.gp101$"lo(LPNem)"$y)
plot(r.gp101$"lo(LPNem)"$x, r.gp101$"lo(LPNem)"$y + resid(r.g101))

########################################
## constraint B-splines
library(cobs)



######################################################################
## similiar to CART
library(tree)
library(randomForest)

##----------------------------------------
## to and from the web
## http://www.rpad.org/Rpad/



##############################################################
## Tests

## Wald-Test basiert auf asympotischer Normalverteilung der Parameter
##
## F-Test basiert auf asympotischer Chi^2-Verteilung der Residual Deviance
## chi^2.df1 / chi^2.df2
## Bei glm ist die Asymptotik für die F-Test (Devianztest) viel besser als
## der Waldtest.
##
## Bei normalverteilten Daten ist der Wald-Test identisch mit dem F-Test.
## Wenn Ausreisser vorhanden sind, können die beiden Tests unterschiedliche
## Ergebnisse liefern:
## Der F-Test macht eine Aussage über Güte des Fits.
## Der Wald-Test sagt, ob die Parameter-Sätze voneinander verschieden sind.

## to check a model, use drop1(x, test = "F") instead of
## anova() oder Anova()!
## Reason: drop1 respects hierarchy and gives in addition AIC criterion

res <- lm(Sepal.Length ~ Petal.Length*Species, iris)
anova(res)
drop1(res, test = "F")  ## better because hierarchy is respected!!

attributes(res)
comment(res) <- "linear regression"
attributes(res)

res <- lm(Sepal.Length ~ Petal.Length + Species, iris)
anova(res)
## Test für Species mit F-Test:
## {(SS.small-SS.big)/(p-q)}/{SS.big/(n-p)}
1 - pf((7.843/2)/(16.682/146), 2, 140)

## Test für Petal.Length
1 - pf((77.643/1)/(16.682/146), 2, 140)

library(MASS)
res <- rlm(Sepal.Length ~ Petal.Length + Species, iris)
anova(res)
#


## Distribution of standard deviation
## cf. Hartung, p. 161
## P = 99%, n = 3
sqrt(2/qchisq(0.995, 2)) ## 0.4344412
sqrt(2/qchisq(0.005, 2)) ## 14.12443

## P = 99%, n = 10
sqrt(9/qchisq(0.995, 9)) ## 0.6176796
sqrt(9/qchisq(0.005, 9)) ## 2.277613


#######################################
## imageprocessing
library(rimage)


########################################
## clustering
hclust()

## reorder dendrograms, factors
with(InsectSprays, reorder(spray, count, FUN = median))

res.h <- hclust(dist(InsectSprays$count))
plot(as.dendrogram(res.h))
windows()
plot(reorder(as.dendrogram(res.h), InsectSprays$spray, agglo.FUN = sum))

## robust clustering with mixture models
library(tclust)

## for bigger datasets
library(help = "cluster")

## clustering large applications
## Will NOT work with factors
library(cluster)
library(hier.part)

library(amap)  # robust hierarchical Clustering

## Principal component analysis

dat <- data.frame(Maths = c(80, 90, 95), Science = c(85, 85, 80), English = c(60, 70, 40), Music = c(55, 45, 50))
rownames(dat) <- c("John", "Mike", "Kate")
pc <- prcomp(dat)
str(pc)

## Eigenvectors = Loadings in results of princomp
pc$rotation

pc

predict(pc)
## (component) scores = (factor) scores
## observations in principal component coordinates
## scores are automatically calculated only in princomp!

pc2 <- princomp(USArrests)
str(pc2)
## loadings:
## Principal components (eigenvectors of covariance matrix) in original coordinates

pc2$scores
predict(pc2)

biplot(pc2)

## Dataset
load(file = paste0(pathIn, "WAU.RDat"))

## Robust estimation of covariance
library(robustbase) ## covMcd()
library(rrcov)      ## CovRobust()
library(ellipse)
covRmcd <- covMcd(WAU)
str(covRmcd)

## principal components (axes) on the base of the covariance
pcR <- princomp(covmat = covRmcd$cov, cor = FALSE)
str(pcR)

## screeplot
plot(pcR)

pcR$loadings
## plot the principal components
windows()
par(pty = "s")
plot(WAU)

arrows(x0 = 0, y0 = 0, x1 = pcR$loadings[1, ], y1 = pcR$loadings[2, ], col = "red")

## Line through first PC
abline(0, pcR$loadings[2, 1]/pcR$loadings[1, 1], lty = 2, col = "blue")

## Add the center manually (covariance matrix has "no center")
pcR$center <- covRmcd$center

## Generate the coordinates in the pcR system ( =  scores)
pc.pred <- predict(pcR, newdata = WAU)
str(pc.pred)
head(pc.pred)

## Plot the observations
windows()
par(pty = "s")
plot(pc.pred)

## covMCD is in critical datasets too progressive with outliers!!!
load(file = paste0(pathIn, "AIHI.RDat"))
covMcd <- covMcd(AIHI, cor = TRUE)
str(covMcd)
covMcd$cor

covStahel <- CovRobust(AIHI)
str(covStahel)
predict(covStahel)
h <- diag(sqrt(diag(covStahel@cov))^(-1))
corStahel <- h %*% covStahel@cov %*% h


lim <- c(-1.5, 1.5)
plot(AIHI[, "AIHI"], AIHI[, "HUD3"],
     xlim = lim, ylim = lim,
     las = 1)

## Conventional estimation
center <- sapply(AIHI, mean)
## points(center[1], center[2], col = "red", pch = " + ", cex = 2)
lines(ellipse(x = cov(AIHI), centre = center),
      col = "red", lwd = 2, lty = 2)

lines(ellipse(x = covMcd$cov, centre = covMcd$center),
      col = "darkgreen", lwd = 2, lty = 2)

lines(ellipse(x = covStahel@cov, centre = covStahel@center),
      col = "blue", lwd = 2, lty = 2)

outl <- covMcd$mcd.wt < 0.1
points(AIHI[outl, "AIHI"], AIHI[outl, "HUD3"], pch = 47, col = "darkgreen", cex = 1.5)

outl <- covStahel@wt < 0.1
points(AIHI[outl, "AIHI"], AIHI[outl, "HUD3"], pch = 92, col = "blue", cex = 1.5)


#####################################################
##
library(help = mva)
library(mva)

heatmap()   # image() with dendrograms on the left and the top side

##----------------------------------------
## Spatial Statistics, Geostatistik
library(help = sp)        # define general spatial classes
## For examples of reading in ASCII Grid files,
##   converting SpatialPointsDataFrame to SpatialGridDataFrame,
##   plotting
## see E:\lor\inNet\AIRMO\ImmikarteV2\immi-07.R

library(rgdal)
library(help = maptools)  # use GIS data, based on package sp
## Beispiel zu Google-Earth-Karten-Herstellung i
## in E:\lor\inNet\AIRMO\ImmikarteV2\immi-01.R

library(help = gstat)
## variagram, kriging, plotting,
## for inverse distance interpolation see
## E:\lor\inNet\AIRMO\ImmikarteV2\immi-09.R

library(help = grasper)   # automatic production of spatial predictions
library(help = Design)    # spatial GLM
library(help = geoR)
library(help = splancs)   # Spatial and Space-Time Point Pattern Analysis
library(help = spdep)     #


##----------------------------------------
## imputation
library(mix)
approx()

library(Hmisc)
?impute

#############################################
## XML
RSvgDevice    # Standard for scalable Vector Graphics




#############################
## < <-  complex superassignment


#################################################################
## circular statistics
library(help = CircStats)
library(MASS)
library(CircStats)
# Generate 100 observations from a von Mises distribution.
# with mean direction 0 and concentration 3.
data.vm <- rvm(100, 0, 3)
# Plot data set. All points do not fit on plot.
circ.plot(data.vm, stack = TRUE, bins = 150)
# Shrink the plot so that all points fit.
circ.plot(data.vm, stack = TRUE, bins = 150, shrink = 1.5)

data <- runif(50, 0, 2*pi)
rose.diag(data, bins = 18, main = 'Uniform Data')

library(help = circular)  ## newer package
library(circular)
data.vm <- rvonmises(n = 100, mu = 0, kappa = 3)
# Plot data set. All points do not fit on plot.
plot(data.vm, stack = TRUE, bins = 150)


##----------------------------------------
## Polar Coordinates
library(help = plotrix)
library(plotrix)

## Plot values as line lengths on a 24 hour "clockface"
Hour <-  0:23
testlen <- 13:36 + rnorm(24)
lab <- sapply(Hour, function(ch) sprintf("%02.0f", ch))

## radial plot starts in East and continues counter clockwise!
radial.plot(testlen, radial.pos = pi*(0.5 - Hour/12),
            labels = Hour, rp.type = "p",
            col = "red", main = "Test of Polarplot")

## pie plot
pieval <- c(2, 4, 6, 8)
plot(1:5, type = "n")
floating.pie(3, 3, pieval)

x <- rnorm(10)/10
y <- sort(rnorm(10))
plot(x, y, xlim = c(-1, 1), type = "p")
nums <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
spread.labels(x, y, nums, 0.5)

plot(10:20, 30:40, xlim = c(9, 21), ylim = c(29, 42))
axis.break(1, 9.5)
axis.break(2, 29, style = "zigzag")

library(Hmisc)
plot(runif(20), runif(20))
minor.tick()


library(help = ecolitk)
library(ecolitk) ## loading takes quite a bit of time
polar2xy()
xy2polar()
rotate()

n <- 40
nn <- 2
thetas <- seq(0,  nn * 2 * pi, length = n)
rhos <- seq(1, n) / n
plot(c(-1, 1), c(-1, 1), type = "n")
abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
xy <- polar2xy(rhos, thetas)
points(xy$x, xy$y, col = rainbow(n))

##----------------------------------------
## spectroscopy
library(help = mscalib)  ## calibrating MS


## Calibration
library(help = systemfit)


##----------------------------------------
## environment
library(help = pheno)
library(pheno)
daylength() ## Tageslänge als Funktion der Breite
maxdaylength()

library(help = clim.pact)
library(clim.pact)
km2lat()   ## Convert long-lat to km-km
km2lon()   ## Convert long-lat to km-km

library(help = climatol)  ## Monthly data homogenization
depudm(varcli, anyi, anyf, nm = 12, wa = 100, dz.max = 2, difumb = 0.05,
       leer = TRUE, a = 0, b = 1, wz = 0.001, sqrtrans = FALSE, ttip = 3,
       refglob = FALSE, ndec = 1, pval = 0.05, graf = FALSE, auto = FALSE,
       verb = TRUE)

library(climatol)

###################################
## our first package

library(help = IDPmisc)

x <- rnorm(10000)
y <- rnorm(10000, 10)
plot(x + y, y, type = "n")
Image(x + y, y)

## alternatives
library(fields)
image.plot(image.count(as.matrix(data.frame(x = x + y, y = y))))

x <- c(NA, 2, 3, NA, NA, NA, 5, NA, 6, NA)

cbind(x, impute.ts(x))

x <- c(-100, 0, -100, NA, 2, 3, NA, NA, NA, 5, NA, 6, NA, 10)
cbind(x, impute.ts(x))
cbind(x, impute.ts(x, 5))

#############################
## RobFit
library(MASS)
MASS:::rlm.default

##----------------------------------------
## recommended packages:
library(help = lattice)
library(help = mgcv)
library(help = nlme)
library(help = cluster)
library(help = rpart)
library(help = survival)
library(help = codetools)

##
library(help = VLMC)
## bundle:VR
library(help = boot)
library(help = rcompgen)

## ----------------------------------------
## other interesting packages
library(help = StatDataML)  ## XML-Austausch über













