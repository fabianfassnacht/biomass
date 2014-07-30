## R-Script - ANOVA analysis
## author: Fabian Fassnacht
## mail: fabian.fassnacht@gmx.de
## Manuscript: Importance of sample size, data type and prediction method for remote sensing-based estimations of aboveground forest biomass. Submitted to Remote Sensing of Environment
## last changes: 30.07.2014
##

#load merged datasets - created with "preparation_ANOVA_merge_results.R"
# some manual editing is required. Please insert the following header to the merged dataset:
#"RMSE"	"R2"	"RMSE_SD"	"R2_SD"	"ExPoMeth"	"NumSamp"	"InData"	"Site"	"Folds"

setwd("D:/my_merged_datafile/")
input<-read.table("merged_datafile.txt", sep="\t", header=TRUE)
attach(input)
head(input)


#calculate anova and store results to textfile  

anova_trees <- aov(R2 ~ (PredMeth+NumSamp+InData+Folds)^4)
#anova_trees <- aov(RMSE ~ (PredMeth+NumSamp+InData+Folds)^4)
sum_anova<-summary(anova_trees)
sum_anova

anova_trees

# print results to file
setwd("D:/my_results")

sink("ANOVA_KA_orig_r2.txt")
print(sum_anova)
sink()

