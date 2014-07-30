## R-Script - prepare biomass plots
## author: Fabian Fassnacht
## mail: fabian.fassnacht@gmx.de
## Manuscript: Importance of sample size, data type and prediction method for remote sensing-based estimations of aboveground forest biomass. Submitted to Remote Sensing of Environment
## last changes: 30.07.2014
##


require(raster)
require(rgdal)
require(matrixStats)

#define function for loading .rdata objects into a variable

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

## Loading of FISHNET
## set working directory to directory containing a fishnet that was used to obtain the predictors used to calculate the prediction in modeling.R


setwd("D:/my_fishnet/")

# load shapefile Fishnet.shp

shape <- readOGR(".","Fishnet")

# set output directory

setwd("D:/output_shapes/")

## !!!! IMPORTANT: Some manual editing required!!!    
# copy all fullPred_***.RData files created by "modeling.R" into one directory


data1 <- list.files(path = "D:/path_to_my_prediction_datasets", full.names = T)
data2 <- list.files(path = "D:/path_to_my_prediction_datasets", full.names = F)

for (i in 1:length(data1)) {

  dummy_data <- load_obj(data1[i])

  out <- matrix(ncol = 20188, nrow = 500)

# calculate mean and sd for all cells


  for (ii in 1:20188) {
  
    for (iii in 1:500) {
    
          out[iii,ii] <- dummy_data[[iii]][ii]    
    
    }
  }



dummy_pred_mean<-colMeans(out)
dummy_sd <- colSds(out)
dummy_cv <- dummy_sd/dummy_pred_mean

# copy fishnet file
out_shape <- shape

# create new field called "biom" and store predictions in the column
out_shape$pred_mean <- dummy_pred_mean
out_shape$sd <- dummy_sd
out_shape$cv <- dummy_cv

name4<-gsub("\\..*","",data2[i])

# write shapefile with biomass values for each cell to file
writeOGR(out_shape, ".", paste(name4,"_mean_sd", sep="" ), driver="ESRI Shapefile")

}



