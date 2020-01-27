## Lattice-Example.R
## Example of lattice plots

library(lattice)
library(latticeExtra)

dpath <- "data/"
gpath <- "/media/ntfs/ZHAW/public/R/functions/"
source(paste0(gpath,"graphics.R"))

head(iris)

xyplot(Sepal.Length~Sepal.Width,data=iris,main="first try")


## larger texts
xyplot(Sepal.Length~Sepal.Width|Species,data=iris,
       par.settings=list(fontsize=list(text=14)))

## larger symbols
xyplot(Sepal.Length~Sepal.Width|Species,data=iris,
       par.settings=list(fontsize=list(points=16)))

## identify points in xyplots
trellis.focus("panel", 1, 1)
panel.identify()
trellis.unfocus()

## plotting objects in the same order as in classical plots
xyplot(Sepal.Length~Sepal.Width|Species,data=iris,
       as.table=TRUE)

## modifying strip labels
xyplot(Sepal.Length~Sepal.Width|Species,data=iris,
       strip = strip.custom(factor.levels=abbreviate(levels(iris$Species))))

xyplot(Sepal.Length~Sepal.Width|Species,data=iris,
       strip = strip.custom(factor.levels=as.character(1:3)))


## change the order of the panels by giving an explicit order
xyplot(Sepal.Length~Sepal.Width|Species,data=iris,
       index.cond = list(c(1,3,2)))

## reordering the factor levels leads to reordered plots!!

## change the order of the panels with 2 conditioning variables
## and / or extract some of them
xyplot(yield~year|variety+site,data=barley,
       index.cond = list(3:1,1:2))

## order the panels with ascending minimum in x
xyplot(Sepal.Length~Sepal.Width|Species,data=iris,
       index.cond = function(x, ...) min(x,na.rm=TRUE))

PL <- equal.count(iris$Petal.Length, number=4, overlap = 0.2)
xyplot(Sepal.Length~Sepal.Width|PL, data=iris)

PL <- equal.count(iris$Petal.Length, number=3, overlap = 0.2)
xyplot(Sepal.Length~Sepal.Width|PL*Species, data=iris)

## or even more sophisticated
library(MASS)
r.rlm <- rlm(Sepal.Length~Sepal.Width,method="MM",data=iris)

p.plot <- function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(rlm(y~x,method="MM"),col="darkgreen",lwd=2)
  panel.abline(r.rlm,col="red",lwd=1,lty=2)
}

## only alternating=c(2,1) and alternating=c(1,2) seem to be
## reasonable parameters
xyplot(Sepal.Length~Sepal.Width|Species,data=iris,aspect=1,
       scales=list(alternating=c(2,1),cex=1.4),
       xlab=list(label="Sepal.Width",cex=1.2),
       ylab=list(label="Sepal.Length",cex=1.2),cex=0.8,
       panel=p.plot,
       key=
       list(x=1,y=0.9, corner=c(1,1),
         text=
         list(lab=c("rob. reg. per species",
                "rob. reg. over all species"),
              col=c("darkgreen","red"),
              cex=1),
         lines=list(
           col=c("darkgreen","red"),
           lty=c(1,2),
           lwd=c(2,1)),
         size=2)
       )

trellis.device()
xyplot(Sepal.Length~Sepal.Width|Species,data=iris,aspect=1,
       scales=list(cex=1),
       layout=c(3,1),
       xlab=list(label="Sepal.Width",cex=1.2),
       ylab=list(label="Sepal.Length",cex=1.2),cex=0.8,
       panel=p.plot,
       par.settings=trellis.par.set("background",list(col="lightgreen")),
       key=list(
         space = "bottom",
         text=
         list(lab=c("rob. regr. per species",
                "rob. regr. over all species"),
              col=c("darkgreen","red"),
              cex=1.2),
         lines=list(
           col=c("darkgreen","red"),
           lty=c(1,2),
           lwd=c(2,1)),
         size=2)
       )

## Plots with grouping
dat <- read.table(paste0(dpath,"tuberculin.csv"),sep=";")


## ----------------------------------------
## more than one legend
load(paste(dpath,"sourceProp.dat",sep=""))
col <- c("pink","skyblue","plum")
key1 <- list(rectangles=list(col=col,size=1),
             text=list(lab=c("Juni - August","Dezember - Januar",
                             "ganzes Jahr"),
             col="black",
             cex=0.8))
key2 <- list(text=list(lab=c("Quellen:",
                             "stat = stationaere Quellen",
                             "local = lokaler Verkehr",
                             "transit = Transit-Schwerverkehr A2",
                             "national = nationaler Gueterverkehr A2",
                             "person = Personenverkehr A2"),
             col="black",
             cex=0.8))

barchart(Anteil~source|site,dat=sourceProp,
         groups=period,
         ylim=c(0,50),
         col=col,
         strip=strip.custom(bg="lightgreen"),
         legend = list(inside = list(x=0.58,y=0.9, corner=c(0,1),
                                     fun="draw.key",
                                     args=list(key=key1,draw=FALSE)),
                       inside = list(x=0.58,y=0.75, corner=c(0,1),
                                     fun="draw.key",
                                     args=list(key=key2,draw=FALSE))))
## ----------------------------------------

## You can also declare your own parameters for the panel function
xyplot(Sepal.Length~Sepal.Width|Species,data=iris,
       med = median(iris$Sepal.Length),
       panel = function(x,y,med) {
         panel.grid(h=-1, v=-1)
         panel.points(x,y,col=ifelse(y>med,"red","blue"))
         })

## defining limits
xyplot(Sepal.Length~Sepal.Width|Species,data=iris,
       xlim=c(1,7), ylim=c(3,15))

## another dataset to demonstrate panel function
## using the grouping information
states <- data.frame(state.x77,
                     state.name = dimnames(state.x77)[[1]],
                     state.region = state.region)
xyplot(Murder ~ Population | state.region, data = states,
       groups = state.name,
       panel = function(x, y, subscripts, groups)
                 panel.text(x = x, y = y,
                            label = groups[subscripts], cex=1, adj = 0.5,
                            fontfamily = "HersheySans", fontface = "bold"))
## be aware:
## subscripts are needed for groups and other variables
## BUT NOT for x and y!!!


## like par(ask = TRUE) for conventional graphics
## deprecated: grid::grid.prompt(TRUE)
devAskNewPage(ask = TRUE)


## plotting POSIX-values on nice axis
x <- seq(ISOdate(2004,1,1,tz=""), ISOdate(2005,1,1,tz=""),
         by="1 month")
y <- rnorm(length(x))
dat <- data.frame(x= x, y = y)

xyplot(y ~ x, data = dat, type = "b",
       scales = list(x = list(at = x, labels=format(x,format="%b"))))

## Plotting log axes
## xscale.components -> latticeExtra
## Hint: x and y are within the panel function transformed to corresponding
## logarithm!!
xyplot((1:200)/20 ~ (1:200)/20, type = c("p", "g"),
  scales = list(x = list(log = 2), y = list(log = 10)),
  xscale.components = xscale.components.fractions,
  yscale.components = yscale.components.log10ticks)

## plotting steps
dat <- data.frame(x=rep(1:10,2),
                  y=c(1:10,10:1),
                  kat=c(rep("A",10),rep("B",10)))

xyplot(y~x|kat,dat=dat, type="s")
xyplot(y~x|kat,dat=dat, type="S")


## boxplots
load(file=paste(dpath,"bwplot.dat",sep=""))
source("goodies/goodies.R")

## png(width=20, height=15, unit="cm", res=200, filename="bwplot.png")
## trellis.par.set(list(fontsize = list(text = 10, points = 3)))
## pointsize in png has no effect!!

windows(20,15)
## size of text and points is handled separately in lattice!!
## change of linetype for whiskers
trellis.par.set(list(fontsize = list(text = 14, points = 5),
                     box.umbrella=list(lty = 1)))
trellis.par.get("box.rectangle")

## getting basics:
get.gpar()

## ----------------------------------------
## nicer settings

png("test.png",
    width=16, height=10, unit = "cm", res = 200)

trellis.par.set(list(fontsize         = list(text = 8, points = 3), ## list(text = 8, points = 3),
                     par.main.text    = list(cex = 1),
                     strip.background = list(col = c(lightblue, lightorange, lightred, lightgreen, lightgrey)),
                     dot.symbol       = list(cex = 0.8),
                     plot.symbol      = list(col = c(blue)),
                     superpose.symbol = list(col = c(blue, orange, red, green, grey, violet, brown),
                         cex = rep(0.4,7)),     ## rep(0.8,7))
                     superpose.line   = list(col = c(blue, orange, red, green, grey, violet, brown)),
                     box.umbrella=list(lty = 1))
                )



col <- c(red,green,blue,orange)[ref]
## length of color must correspond to number of levels of id or must be 1

bwplot(diameter~id|sensitine+Dose, data=Exp,
       scales = list(x = list(rot=90, cex=0.45)),
       fill = col,
       rectangle.col = col,
       umbrella.col = col,
       outliers.col = col,
       panel = function(x,y,...){
           panel.bwplot1(x,y,...)
           panel.abline(h=median(y), col="grey70")
       })

bwplot(diameter~id|sensitine+Dose, data=Exp,
       scales = list(x = list(rot=90, cex=0.45)),
       fill = c(red,green,blue,gold)[ref],
       panel = function(x,y,...){
           panel.bwplot1(x,y,...)
           panel.abline(h=median(y), col="grey70")
       })

## dev.off()

histogram(~diameter|sensitine+Dose, data=Exp,  ## [Exp$diameter<10,]
          breaks = c(seq(0,10,1),30),
          type = "percent",
          scales = list(x = list(lim=c(0,10)),
              y = list(lim=c(0,5)))
          )
histogram(~diameter|sensitine+Dose, data=Exp,  ## [Exp$diameter<10,]
          breaks = c(seq(0,10,1),30),
          type = "percent"
          )


head(Exp)
range(Exp$diameter)

## general comments
## nested factors cannot be displayed reasonably
## Work around: paste the two or more levels to one variable and
## make a factor with sorted levels out of it!!

## ----------------------------------------
## general definitions for lattice graphics

## check for all parameter names for lattice plots (cf. par())
names(trellis.par.get())

str(trellis.par.get())


## setting lattice theme with color
trellis.par.set(canonical.theme(color = TRUE))
canonical.theme


## defining other size of characters globally
trellis.par.set(fontsize=list(text=10, points=6))

## setting background color
library(latticeExtra)
trellis.par.set(custom.theme(bg="grey90"))

## scatter plot matrix
super.sym <- trellis.par.get("superpose.symbol")

splom(~iris[1:4], groups = Species, data = iris,
      panel = panel.superpose,
      key = list(title = "Three Varieties of Iris",
          columns = 3,
          points = list(pch = super.sym$pch[1:3],
              col = super.sym$col[1:3]),
          text = list(c("Setosa", "Versicolor", "Virginica"))))

splom(~iris[1:4], groups = Species, data = iris)

## identify points in splom plots
trellis.focus("panel", 1, 1)
extr <- panel.link.splom(verbose = TRUE)
extr

library(hexbin)
library(IDPmisc)
library(SwissAir)

diag.f <- function(cex, ...) {
    par.list <- list(...)
    ##par.list$draw <- FALSE
    par.list$varname.cex <- cex
    par.list$axis.text.cex <- 0.7*cex
    do.call(diag.panel.splom,par.list)
    ## ,pscales=0 ## without axes
}

data(AirQual)
ii <- regexpr("NOx",names(AirQual))>0

splom(~log(AirQual[,ii]), data = iris, cex=1.2,
      diag.panel=diag.f,
      panel=panel.hexbinplot, colramp=IDPcolorRamp)

## with identical limits for all panels
lo <- -1
up <- 8
l0 <- list(at=lo:up,labels=lo:up,limits=c(lo,up))
ll <- rep(list(l0),4)

splom(~iris[1:4], data = iris, cex=1.2,
      pscale=ll,
      diag.panel=diag.f,
      panel=panel.hexbinplot,
      colramp=IDPcolorRamp, xbins = 10,
      xbnds = c(lo,up), ybnds = c(lo,up)
      )

xyplot(Sepal.Length~Sepal.Width, groups=Species, data = iris)

str(rep(list(at=1:5,labels=letters[1:5],limits=c(0,8)),4))
relist(rep(unlist(list(at=1:5,labels=letters[1:5],limits=c(0,8))),4),
       list(at=1:5,labels=letters[1:5],limits=c(0,8)))

str(as.relistable(list(at=1:5,labels=letters[1:5],limits=c(0,8))))

## more examples in "C:/Programme/R/library/lattice/demo"

##
barchart(yield ~ variety | site, data = barley,
         groups = year, layout = c(1,6), stack = TRUE,
         auto.key = list(points = FALSE, rectangles = TRUE,
             space = "right"),
         ylab = "Barley Yield (bushels/acre)",
         scales = list(x = list(rot = 45)))


test <- data.frame(x = 1:2, fac = factor(c("A","B")))
str(test)
barchart(x ~ fac, data=test, horizontal = TRUE)


barchart(Sepal.Length~Sepal.Width| equal.count(Petal.Length),iris)

show.settings()

## Arguments for trellis paramaters
cbind(names(trellis.par.get()))

## read, write background color
trellis.par.get("background")
trellis.par.set("background",list(col="white"))

## show all allowed keywords
names(trellis.par.get())

## show all settings
trellis.par.get()

## set white background and proper colors for points, lines and so on
trellis.par.set(col.whitebg())

## weiteres Beispiel
trellis.par.set(list(superpose.symbol=list(pch=c("o","+",">","s","w","#","{"))))


## Trellisdevices mit verschiedenen Settings

## Hintergrund weiss bzw. transparent mit version=1.4
trellis.device(device="pdf")
trellis.device(device="jpeg")        ##Hintergrund grau
trellis.device(device="bmp")         ##Hintergrund grau
trellis.device(device="png")         ##Hintergrund grau
trellis.device(device="postscript")  ##b7w-Plot

##
trellis.device(device="pdf",file="D:\\KK\\lmeDifferenzen.pdf")
plot(lattice.object)
dev.off()

## editing lattice graphics
trellis.unfocus()

## different background colors in different viewports of xyplot()
## http://tolstoy.newcastle.edu.au/R/help/04/02/0848.html

## handling viewports in lattice
## https://www.stat.math.ethz.ch/pipermail/r-devel/2005-June/033439.html
## https://www.stat.math.ethz.ch/pipermail/r-devel


## Legende im freien Teil der Grafik
## mit spezieller panel.function, konfektionierten Achsenbeschriftungen
## und layout


## graph of predict of mle with additional lines
## may be used also for points in different colors in each group
plot(augPred(r.mle00,~HVS),
     groups=augPred(r.mle00,~HVS)$.type,

     panel=function(x,y,subscripts,groups,...){
         set.seed(1313)
         panel.abline(rlm(y[groups[subscripts]=="original"]~
                          x[groups[subscripts]=="original"], method="MM"),
                      col="darkgreen",lwd=2)
         panel.abline(r.rlm,col="red",lwd=2)
         panel.superpose.2(x,y,
                           groups=groups,
                           subscripts=subscripts, type=c("l","p"),lwd=2)

     })


## shadowed text
g1 <- textGrob(rep("Fancy text", 2),
               x = unit(.5, "npc") + unit(c(.5, 0), "mm"),
               y = unit(.5, "npc") + unit(c(0, .5), "mm"),
               gp = gpar(col = c("black", "red"), cex = 3))

g2 <- textGrob("This is \na text grob")

xyplot(rnorm(10) ~ rnorm(10),
       legend =
       list(inside =
            list(fun = draw.key,
                 x = .3, y = .7,
                 args =
                 list(key = list(points = list(col = 1:3),
                          text = list(letters[1:3])),
                      draw = FALSE)),
            bottom = list(fun = g1),
            top = list(fun = g2)))
## ----------------------------------------
## further grid packages
library(help=gridSVG)



## ----------------------------------------
## ----------------------------------------
## ----------------------------------------
## useful structure building for lattice plots

head(Indometh)
wide <- reshape(Indometh,        ## Beware!! Must be a data.frame, not a matrix
                v.names="conc",  ## name(s) of variables in the long format that correspond to multiple variables in the wide format.
                timevar="time",  ## Variable in long format that differentiates multiple records from the same group or individual:
                ## Its content attributes to v.names in wide format.
                idvar="Subject", ## Names of variable(s) in long format that identify multiple records from the same group/individual.
                direction="wide")
wide
## wide format = each subject has exactly one row for each combinations of values of idvars!!!!

Ind <- Indometh
Ind <- Ind[-1,]
reshape(Ind,
        v.names="conc",
        timevar="time",
        idvar="Subject",
        direction="wide")


## reverse action,
## arguments are taken from reshape attributes
head(reshape(wide, direction="long"))

## explicit version to show behaviour of arguments
head(reshape(wide,
             varying=names(wide)[-1],
             times=substr(names(wide)[-1],6,99),
             idvar="SUBJECT",
             ids=wide$Subject,
             v.names="CONC",
             timevar="TIME",
             drop="Subject",
             direction="long"))

## another example
state.x77 <- as.data.frame(state.x77)
str(state.x77)
long <-
    reshape(state.x77,                ## Beware!! This must be a data.frame *NO* matrix!!!
            varying=names(state.x77), ## List or vector. Names of sets of variables in wide format corresponding to single variables in long format
            ## Here just 1 set
            times=names(state.x77),   ## Individual time / attribute of observation
            idvar="State",            ## Names of variable(s) in long format that identify multiple records of the same group/individual.
            ## Here just 1 variable.
            ids=row.names(state.x77), ## The values to use for a newly created idvar variable in long format.
            v.names="Magnitude",      ## Names of individual variable(s) in long format. Here just 1 variable.
            timevar="Characteristic", ## Variable in long format that differentiates multiple records from the same group or individual:
            ## Its values attribute to v.names in wide format.
            direction="long")
head(long)
head(reshape(long))

long <- data.frame(long)  ## remove reshape information
row.names(long) <- NULL

head(reshape(long,
             idvar="State",
             timevar="Characteristic",
             direction="wide"))
## or
head(reshape(long,
             idvar="Characteristic",
             timevar="State",
             direction="wide"))


## ----------------------------------------

load(paste(dpath,"hours.wide.dat",sep=""))

idvar <- c("MA","Jahr","Quartal")
varying <- c("Unterhalt","Administration","Marketing","Akquisition","Persoenlich","Weiterbildung",
             "Krankheit_Militaer","IDP","produktiv")

hours.long <-
    reshape(hours.wide,
            varying = varying,
            idvar = idvar,
            ids=paste(hours.wide$MA,hours.wide$Jahr,hours.wide$Quartal,sep="."),
            v.names = "Prozent",
            timevar = "Typ",
            times = varying,
            direction = "long")

head(hours.wide)
head(hours.long)

## ----------------------------------------
## multiple sets of variables
load(paste(dpath,"emTraf.dat",sep=""))

emTraf.wide <-
    reshape(emTraf.long,
            v.names=c("NOx","PM","PN"),        ## Names of individual variables in long format.
            timevar="Region",                  ## Variables in long format that differentiate multiple records from the same group or individual.
                                               ## There values attribute to v.names in wide format.
            idvar=c("wd","t1","t2",
                    "Corridor","VehCat"),      ## Names of variable(s) in long format that identify multiple records of the same group/individual.
            direction="wide")
head(emTraf.wide)
head(emTraf.long)


## ----------------------------------------
## This code is extracted from ImmiAnteil-08.R and must be
## redesigned to be useful for public.
## It shows how you can adress other variables than x, y and groups
## within the panel.function
p.plot <- function(x,y,subscripts, groups, start,
                   housingArea, h, mon, distA2,distMR ){
  ## panel.text(x,y,label = h[subscripts])
  panel.abline(h=mean(y),col="grey70", lwd=2)
  panel.abline(rlm(y~x,method="MM"),col="darkgreen",lwd=2)
  ##browser()
  so <- order(x)
  yp <- predict(Ef07.rlm01,
                newdata=data.frame(start=start[subscripts],
                housingArea=housingArea[subscripts],
                mon=mon[subscripts],
                distA2.200=distA2[subscripts],
                distMR.60=distMR[subscripts],
                h=h[subscripts]))
  x <- x[so]
  y <- y[so]
  yp <- yp[so]
  panel.text(x,y-yp,label = groups[subscripts])
}

xyplot(lconc~ldistA2|start*housingArea,dat,groups=site, panel=p.plot,
       start=dat$start, housingArea=dat$housingArea,
       mon=dat$mon, h=dat$h, distA2=dat$distA2.200,
       distMR=dat$distMR.60)


## ----------------------------------------
## some (hidden) functions from the package
lattice:::xyplot
getAnywhere(xyplot)
showMethods(xyplot.default)

lattice:::draw.key
lattice:::panel.xyplot

## sorting the panels
lattice:::packet.panel.default

## further nice plots in
## X:\Public\Prionics\Tuberculin\TPPD-20.R
