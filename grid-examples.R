## grid-example.R
library(grid)
library(gridBase)
library(lattice)

trellis.device(windows, theme = col.whitebg());dev.off()

grid.newpage()
## creating a viewport in the upper part of device
upper1.vp <- viewport(                           ## viewport is centred in x direction (default)
                      y = unit(0.75, "npc"),     ## viewport is positioned centred around 0.75 in y direction
                      width = unit(1, "npc"),    ## viewport has full width of device
                      height = unit(0.5, "npc"), ## viewport has half height of device
                      name = "upper1")
pushViewport(upper1.vp)

## creating a 1-row-2-column layout in the upper part of device
upper1.1.2.vp <- viewport(layout =
                          grid.layout(1, 2,
                                      widths = unit(c(0.5, 0.5), "npc")),
                          name = "upper1.1.2")
pushViewport(upper1.1.2.vp)

## choosing the first cell of this layout
upper1.1.2.1.vp <- viewport(layout.pos.col = 1,
                      name = "upper1.1.2.1")
pushViewport(upper1.1.2.1.vp)

## grid.text("upper1.1.2.1")
## grid.rect()

## creating a lattice plot in this cell,
## which generates itself several viewports within this cell
x <- rnorm(100); y <- rnorm(100); z <- gl(2, 50)

## produces a complex grid object (= lattice object)
xyp <- xyplot(y ~ x | z)

## plots this object
print(xyp, newpage = FALSE)

grid.rect(gp = gpar(col = "blue"))

## back to upper1.1.2
popViewport()

pushViewport(viewport(layout.pos.col = 2,
                      xscale = c(0,11),
                      yscale = c(0,11),
                      name = "upper1.1.2.2"))
## grid.text("upper1.1.2.2")
## grid.rect(gp=gpar(col="red"))

grid.points(x = rep(1:10, 10),
            y = rep(1:10, each = 10), pch = 32:131, gp = gpar(col = 1:100))
grid.rect(gp = gpar(col="red"))

##showViewport(col=rgb(0, 0, 1, 0.2))

## navigating to root viewport
popViewport(n = 0)

## creating a new viewport in the lower part of the device
pushViewport(viewport(x = unit(0.05, "npc"),
                      y = unit(0.25, "npc"),
                      width = unit(0.9, "npc"),
                      height = unit(0.4, "npc"),
                      layout = grid.layout(1, 2,
                                           widths = unit(c(0.9, 0.1), "npc")),
                      just = "left"))
grid.rect(gp = gpar(col = "grey"))

## switching to the left part of the layout created
pushViewport(viewport(layout.pos.col = 1))
grid.rect(gp = gpar(col = "green"))

## defining the outer margins in inches,
## to restrict classical plots to the current viewport
par(omi = gridOMI())

## Then just plot as you are used to do
par(mfrow = c(1, 1), mfg = c(1, 1), mar = c(4,4,0.5,0.5), las = 1)
x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = terrain.colors(100), axes = FALSE)
contour(x, y, volcano, levels = seq(90, 200, by = 5),
        add = TRUE, col = "peru")
axis(1, at = seq(100, 800, by = 100))
axis(2, at = seq(100, 600, by = 100))
box()

## Image-legend
popViewport()
pushViewport(viewport(layout.pos.col = 2))
pushViewport(viewport(width=unit(0.5,"npc"),height=unit(0.8,"npc")))

for (ii in seq(0, 0.99, length = 100)) {
    grid.rect(x = unit(0.5, "npc"),
              y = unit(ii + 0.005, "native"),
              gp = gpar(fill = terrain.colors(100)[ii*100 + 1],
                        col = 0),
              height = unit(0.01, "npc"),
              width = unit(1, "npc"))
}
grid.yaxis(at = seq(0, 1, length = 8), main = FALSE,
    label = round(seq(min(volcano), max(volcano), length = 8)))

## ----------------------------------------
## segments of circle
grid.newpage()
pushViewport(viewport(width = unit(1, "npc"),
                      height = unit(1, "npc"),
                      xscale = c(-1,1),
                      yscale = c(-1,1)))


circle.seg <- function(phi1,phi2,rho=1,ncp=1000) {
    phi <- seq(phi1,phi2,len=ncp)
    x <- sin(2*pi*phi/360)
    y <- cos(2*pi*phi/360)
    grid.segments(x0=x[-length(x)],
                 y0=y[-length(y)],
                 x1=x[-1],
                 y1=y[-1],
                 default.units="native")
}

circle.seg(0,60)

## writing text
grid.newpage()
x <- unit(0.5, "npc")
y <- unit(0.5, "npc")
grid.points(x, y, pch = 10)
grid.text("text", x, y)
grid.text("text top", x, y, just = "top")
grid.text("text bottom", x, y, just = "bottom")


