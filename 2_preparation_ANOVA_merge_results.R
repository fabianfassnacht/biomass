## R-Script - plot results (beanplots) for RMSE
## author: Fabian Fassnacht
## mail: fabian.fassnacht@gmx.de
## Manuscript: Importance of sample size, data type and prediction method for remote sensing-based estimations of aboveground forest biomass. Submitted to Remote Sensing of Environment
## last changes: 30.07.2014
##


## set working directory to desired output directory

setwd("D:/my_outputs/")

## !!!! IMPORTANT: Some manual editing required!!!    
# copy all .txt files created by "modeling.R" into one directory
# then load datasets by defining the path to the directory in the next step

filen <- list.files("D:/path_to_my_output_textfiles/")

d <- list()

for (i in 1:length(filen)) {

d[[i]] <- read.table(filen[i], sep="\t", header=TRUE)

}

for (i in 1:length(filen)) {
  
  write.table(d[[i]], "merged_textfiles.txt", sep="\t", append=T, row.names=F, col.names=F)
  
}