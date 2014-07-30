## R-Script - biomass plots
## author: Fabian Fassnacht
## mail: fabian.fassnacht@gmx.de
## Manuscript: Importance of sample size, data type and prediction method for remote sensing-based estimations of aboveground forest biomass. Submitted to Remote Sensing of Environment
## last changes: 30.07.2014
##


### first run function given below!!!

## the script created biomass plots based on a shapefile that has been created by code  "preparation_biomass_map_plots.R"

require(rgdal)
require(raster)
require(mapplots)
require(ggplot2)
require(RColorBrewer)
require(maptools)
require(sp)
require(gstat)
require(maps)
require(mapproj)
require(GISTools)

## load shapefile created with "preparation_biomass_map_plots.R"

setwd("D:/my_data/shapes")

my_best_result <- readOGR(".", "my_result)


#### Plot Nr. 1
#### 
bla<-makeTransparent("white", alpha=0)

setwd("D:/my_plots/")


# simple plot best result (RF, lidar, cl4)


pdf("output.pdf", width=9, height=8)
par(mfrow=c(1,2), mar=c(4,6,2,4)) #, oma=c(4.5,0.5,0.5,0.5),
bla<-makeTransparent("white", alpha=0)

baseplot1 <- my_best_result
baseplot1$Col <- cut(baseplot1@data$pred_mean, seq(0, 300, by = 25))

baseplot2 <- my_best_result
baseplot2$Col <- cut(baseplot1@data$cv, seq(0, 0.4, by = 0.035))

# define color scales

colr <-colorRampPalette(c("gray90", "blue","green"))(12) 
colr2 <-colorRampPalette(c("black", "gray90"))(12) 

# RF plots


baseplot <- rf_cl4_li
baseplot$Col <- cut(baseplot@data$pred_mean, seq(0, 300, by = 25))

plot(baseplot, col=colr[baseplot$Col], border=bla)
north.arrow(xb=3456663,yb=5431180,len=80,lab="North")
map.scale(xc=3457750,yc=5431370,len=1000,units="km", ndivs=1, scol = "black", sfcol ="black")
title(main="Mean biomass [t/ha]" )
legend.col(col = colr, lev = baseplot1$pred_mean)
box()

baseplot <- rf_cl4_li
baseplot$Col <- cut(baseplot@data$cv, seq(0, 0.4, by = 0.035))

plot(baseplot, col=colr2[baseplot$Col], border=bla)
north.arrow(xb=3456663,yb=5431180,len=80,lab="North")
map.scale(xc=3457750,yc=5431370,len=1000,units="km", ndivs=1, scol = "black", sfcol ="black")
title(main="CV of biomass estimates" )
legend.col(col = colr2, lev = baseplot2$cv)
box()


dev.off()





### define function for color scale


legend.col <- function(col, lev){
  
  opar <- par
  
  n <- length(col)
  
  bx <- par("usr")
  
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  
  xx <- rep(box.cx, each = 2)
  
  par(xpd = TRUE)
  for(i in 1:n){
    
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
    
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(lev), max(lev)),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = .25)
  par <- opar
}


makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}
