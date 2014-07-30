## R-Script - plot results (beanplots) for RMSE
## author: Fabian Fassnacht
## mail: fabian.fassnacht@gmx.de
## Manuscript: Importance of sample size, data type and prediction method for remote sensing-based estimations of aboveground forest biomass. Submitted to Remote Sensing of Environment
## last changes: 30.07.2014
##
##
## !!! IMPORTANT: THIS CREATES RMSE PLOTS!!! For corresponding R2 plots - please replace RMSE by R2 throughout the code. E.g.:    SVM_HS_CL1_stat <- boxplot(SVM_HS_CL1$RMSE)$stats[3] WILL BE:     SVM_HS_CL1_stat <- boxplot(SVM_HS_CL1$R2)$stats[3]

library(beanplot)

## load merged dataset (created with "preparation_ANOVA_merge_results.R"
## some manual editing is required. Please insert the following header to the merged dataset:
##"RMSE"	"R2"	"RMSE_SD"	"R2_SD"	"ExPoMeth"	"NumSamp"	"InData"	"Site"	"Folds"

setwd("D:/my_data")
inData<-read.table("merged_data.txt", sep="\t", header=T)

head(inData)

#get entries of each expometh and expometh + datatype

###################### SVM

ids1 <- list()
ids1 <- inData$ExPoMeth == "SVM"
SVM <- inData[ids1,]

  ids11 <- list()
  ids11 <- SVM$InData == "HS"
  SVM_HS <- SVM[ids11,]

    ids111 <- list()
    ids111 <- SVM_HS$NumSamp == "CLASS1"
    SVM_HS_CL1 <- SVM_HS[ids111,]
    SVM_HS_CL1_stat <- boxplot(SVM_HS_CL1$RMSE)$stats[3]

    ids112 <- list()
    ids112 <- SVM_HS$NumSamp == "CLASS2"
    SVM_HS_CL2 <- SVM_HS[ids112,]
    SVM_HS_CL2_stat <- boxplot(SVM_HS_CL2$RMSE)$stats[3]

    ids113 <- list()
    ids113 <- SVM_HS$NumSamp == "CLASS3"
    SVM_HS_CL3 <- SVM_HS[ids113,]
    SVM_HS_CL3_stat <- boxplot(SVM_HS_CL3$RMSE)$stats[3]

    ids114 <- list()
    ids114 <- SVM_HS$NumSamp == "CLASS4"
    SVM_HS_CL4 <- SVM_HS[ids114,]
    SVM_HS_CL4_stat <- boxplot(SVM_HS_CL4$RMSE)$stats[3]

  
  ids12 <- list()
  ids12 <- SVM$InData == "LIDAR"
  SVM_LIDAR <- SVM[ids12,]

    ids111a <- list()
    ids111a <- SVM_LIDAR$NumSamp == "CLASS1"
    SVM_LIDAR_CL1 <- SVM_LIDAR[ids111a,]
    SVM_LIDAR_CL1_stat <- boxplot(SVM_LIDAR_CL1$RMSE)$stats[3]

    ids112a <- list()
    ids112a <- SVM_LIDAR$NumSamp == "CLASS2"
    SVM_LIDAR_CL2 <- SVM_LIDAR[ids112a,]
    SVM_LIDAR_CL2_stat <- boxplot(SVM_LIDAR_CL2$RMSE)$stats[3]

    ids113a <- list()
    ids113a <- SVM_LIDAR$NumSamp == "CLASS3"
    SVM_LIDAR_CL3 <- SVM_LIDAR[ids113a,]
    SVM_LIDAR_CL3_stat <- boxplot(SVM_LIDAR_CL3$RMSE)$stats[3]

    ids114a <- list()
    ids114a <- SVM_LIDAR$NumSamp == "CLASS4"
    SVM_LIDAR_CL4 <- SVM_LIDAR[ids114a,]
    SVM_LIDAR_CL4_stat <- boxplot(SVM_LIDAR_CL4$RMSE)$stats[3]


  ids13<- list()
  ids13 <- SVM$InData == "LIDARHS"
  SVM_LIDARHS <- SVM[ids13,]

    ids111 <- list()
    ids111 <- SVM_LIDARHS$NumSamp == "CLASS1"
    SVM_LIDARHS_CL1 <- SVM_LIDARHS[ids111,]
    SVM_LIDARHS_CL1_stat <- boxplot(SVM_LIDARHS_CL1$RMSE)$stats[3]

    ids112 <- list()
    ids112 <- SVM_LIDARHS$NumSamp == "CLASS2"
    SVM_LIDARHS_CL2 <- SVM_LIDARHS[ids112,]
    SVM_LIDARHS_CL2_stat <- boxplot(SVM_LIDARHS_CL2$RMSE)$stats[3]

    ids113 <- list()
    ids113 <- SVM_LIDARHS$NumSamp == "CLASS3"
    SVM_LIDARHS_CL3 <- SVM_LIDARHS[ids113,]
    SVM_LIDARHS_CL3_stat <- boxplot(SVM_LIDARHS_CL3$RMSE)$stats[3]

    ids114 <- list()
    ids114 <- SVM_LIDARHS$NumSamp == "CLASS4"
    SVM_LIDARHS_CL4 <- SVM_LIDARHS[ids114,]
    SVM_LIDARHS_CL4_stat <- boxplot(SVM_LIDARHS_CL4$RMSE)$stats[3]


################### KNN

ids2 <- list()
ids2 <- inData$ExPoMeth == "KNN"
KNN <- inData[ids2,]

  ids21 <- list()
  ids21 <- KNN$InData == "HS"
  KNN_HS <- KNN[ids21,]


    ids211 <- list()
    ids211 <- KNN_HS$NumSamp == "CLASS1"
    KNN_HS_CL1 <- KNN_HS[ids211,]
    KNN_HS_CL1_stat <- boxplot(KNN_HS_CL1$RMSE)$stats[3]

    ids212 <- list()
    ids212 <- KNN_HS$NumSamp == "CLASS2"
    KNN_HS_CL2 <- KNN_HS[ids212,]
    KNN_HS_CL2_stat <- boxplot(KNN_HS_CL2$RMSE)$stats[3]

    ids213 <- list()
    ids213 <- KNN_HS$NumSamp == "CLASS3"
    KNN_HS_CL3 <- KNN_HS[ids213,]
    KNN_HS_CL3_stat <- boxplot(KNN_HS_CL3$RMSE)$stats[3]

    ids214 <- list()
    ids214 <- KNN_HS$NumSamp == "CLASS4"
    KNN_HS_CL4 <- KNN_HS[ids214,]
    KNN_HS_CL4_stat <- boxplot(KNN_HS_CL4$RMSE)$stats[3]

  ids22 <- list()
  ids22 <- KNN$InData == "LIDAR"
  KNN_LIDAR <- KNN[ids22,]


    ids211a <- list()
    ids211a <- KNN_LIDAR$NumSamp == "CLASS1"
    KNN_LIDAR_CL1 <- KNN_LIDAR[ids211a,]
    KNN_LIDAR_CL1_stat <- boxplot(KNN_LIDAR_CL1$RMSE)$stats[3]

    ids212a <- list()
    ids212a <- KNN_LIDAR$NumSamp == "CLASS2"
    KNN_LIDAR_CL2 <- KNN_LIDAR[ids212a,]
    KNN_LIDAR_CL2_stat <- boxplot(KNN_LIDAR_CL2$RMSE)$stats[3]

    ids213a <- list()
    ids213a <- KNN_LIDAR$NumSamp == "CLASS3"
    KNN_LIDAR_CL3 <- KNN_LIDAR[ids213a,]
    KNN_LIDAR_CL3_stat <- boxplot(KNN_LIDAR_CL3$RMSE)$stats[3]

    ids214a <- list()
    ids214a <- KNN_LIDAR$NumSamp == "CLASS4"
    KNN_LIDAR_CL4 <- KNN_LIDAR[ids214a,]
    KNN_LIDAR_CL4_stat <- boxplot(KNN_LIDAR_CL4$RMSE)$stats[3]

  ids23 <- list()
  ids23 <- KNN$InData == "LIDARHS"
  KNN_LIDARHS <- KNN[ids23,]


    ids211 <- list()
    ids211 <- KNN_LIDARHS$NumSamp == "CLASS1"
    KNN_LIDARHS_CL1 <- KNN_LIDARHS[ids211,]
    KNN_LIDARHS_CL1_stat <- boxplot(KNN_LIDARHS_CL1$RMSE)$stats[3]

    ids212 <- list()
    ids212 <- KNN_LIDARHS$NumSamp == "CLASS2"
    KNN_LIDARHS_CL2 <- KNN_LIDARHS[ids212,]
    KNN_LIDARHS_CL2_stat <- boxplot(KNN_LIDARHS_CL2$RMSE)$stats[3]

    ids213 <- list()
    ids213 <- KNN_LIDARHS$NumSamp == "CLASS3"
    KNN_LIDARHS_CL3 <- KNN_LIDARHS[ids213,]
    KNN_LIDARHS_CL3_stat <- boxplot(KNN_LIDARHS_CL3$RMSE)$stats[3]

    ids214 <- list()
    ids214 <- KNN_LIDARHS$NumSamp == "CLASS4"
    KNN_LIDARHS_CL4 <- KNN_LIDARHS[ids214,]
    KNN_LIDARHS_CL4_stat <- boxplot(KNN_LIDARHS_CL4$RMSE)$stats[3]


######################### GP

ids3 <- list()
ids3 <- inData$ExPoMeth == "GP"
GP <- inData[ids3,]

  ids31 <- list()
  ids31 <- GP$InData == "HS"
  GP_HS <- GP[ids31,]

    ids311 <- list()
    ids311 <- GP_HS$NumSamp == "CLASS1"
    GP_HS_CL1 <- GP_HS[ids311,]
    GP_HS_CL1_stat <- boxplot(GP_HS_CL1$RMSE)$stats[3]

    ids312 <- list()
    ids312 <- GP_HS$NumSamp == "CLASS2"
    GP_HS_CL2 <- GP_HS[ids312,]
    GP_HS_CL2_stat <- boxplot(GP_HS_CL2$RMSE)$stats[3]

    ids313 <- list()
    ids313 <- GP_HS$NumSamp == "CLASS3"
    GP_HS_CL3 <- GP_HS[ids313,]
    GP_HS_CL3_stat <- boxplot(GP_HS_CL3$RMSE)$stats[3]

    ids314 <- list()
    ids314 <- GP_HS$NumSamp == "CLASS4"
    GP_HS_CL4 <- GP_HS[ids314,]
    GP_HS_CL4_stat <- boxplot(GP_HS_CL4$RMSE)$stats[3]


  ids32 <- list()
  ids32 <- GP$InData == "LIDAR"
  GP_LIDAR <- GP[ids32,]

    ids311a <- list()
    ids311a <- GP_LIDAR$NumSamp == "CLASS1"
    GP_LIDAR_CL1 <- GP_LIDAR[ids311a,]
    GP_LIDAR_CL1_stat <- boxplot(GP_LIDAR_CL1$RMSE)$stats[3]

    ids312a <- list()
    ids312a <- GP_LIDAR$NumSamp == "CLASS2"
    GP_LIDAR_CL2 <- GP_LIDAR[ids312a,]
    GP_LIDAR_CL2_stat <- boxplot(GP_LIDAR_CL2$RMSE)$stats[3]

    ids313a <- list()
    ids313a <- GP_LIDAR$NumSamp == "CLASS3"
    GP_LIDAR_CL3 <- GP_LIDAR[ids313a,]
    GP_LIDAR_CL3_stat <- boxplot(GP_LIDAR_CL3$RMSE)$stats[3]

    ids314a <- list()
    ids314a <- GP_LIDAR$NumSamp == "CLASS4"
    GP_LIDAR_CL4 <- GP_LIDAR[ids314a,]
    GP_LIDAR_CL4_stat <- boxplot(GP_LIDAR_CL4$RMSE)$stats[3]

  ids33 <- list()
  ids33 <- GP$InData == "LIDARHS"
  GP_LIDARHS <- GP[ids33,]

    ids311 <- list()
    ids311 <- GP_LIDARHS$NumSamp == "CLASS1"
    GP_LIDARHS_CL1 <- GP_LIDARHS[ids311,]
    GP_LIDARHS_CL1_stat <- boxplot(GP_LIDARHS_CL1$RMSE)$stats[3]

    ids312 <- list()
    ids312 <- GP_LIDARHS$NumSamp == "CLASS2"
    GP_LIDARHS_CL2 <- GP_LIDARHS[ids312,]
    GP_LIDARHS_CL2_stat <- boxplot(GP_LIDARHS_CL2$RMSE)$stats[3]

    ids313 <- list()
    ids313 <- GP_LIDARHS$NumSamp == "CLASS3"
    GP_LIDARHS_CL3 <- GP_LIDARHS[ids313,]
    GP_LIDARHS_CL3_stat <- boxplot(GP_LIDARHS_CL3$RMSE)$stats[3]

    ids314 <- list()
    ids314 <- GP_LIDARHS$NumSamp == "CLASS4"
    GP_LIDARHS_CL4 <- GP_LIDARHS[ids314,]
    GP_LIDARHS_CL4_stat <- boxplot(GP_LIDARHS_CL4$RMSE)$stats[3]

##################### LMSTEP

ids4 <- list()
ids4 <- inData$ExPoMeth == "LMSTEP"
LMSTEP <- inData[ids4,]

  ids41 <- list()
  ids41 <- LMSTEP$InData == "HS"
  LMSTEP_HS <- LMSTEP[ids41,]

    ids411 <- list()
    ids411 <- LMSTEP_HS$NumSamp == "CLASS1"
    LMSTEP_HS_CL1 <- LMSTEP_HS[ids411,]
    LMSTEP_HS_CL1_stat <- boxplot(LMSTEP_HS_CL1$RMSE)$stats[3]

    ids412 <- list()
    ids412 <- LMSTEP_HS$NumSamp == "CLASS2"
    LMSTEP_HS_CL2 <- LMSTEP_HS[ids412,]
    LMSTEP_HS_CL2_stat <- boxplot(LMSTEP_HS_CL2$RMSE)$stats[3]

    ids413 <- list()
    ids413 <- LMSTEP_HS$NumSamp == "CLASS3"
    LMSTEP_HS_CL3 <- LMSTEP_HS[ids413,]
    LMSTEP_HS_CL3_stat <- boxplot(LMSTEP_HS_CL3$RMSE)$stats[3]

    ids414 <- list()
    ids414 <- LMSTEP_HS$NumSamp == "CLASS4"
    LMSTEP_HS_CL4 <- LMSTEP_HS[ids414,]
    LMSTEP_HS_CL4_stat <- boxplot(LMSTEP_HS_CL4$RMSE)$stats[3]

  ids42 <- list()
  ids42 <- LMSTEP$InData == "LIDAR"
  LMSTEP_LIDAR <- LMSTEP[ids42,]

    ids411a <- list()
    ids411a <- LMSTEP_LIDAR$NumSamp == "CLASS1"
    LMSTEP_LIDAR_CL1 <- LMSTEP_LIDAR[ids411a,]
    LMSTEP_LIDAR_CL1_stat <- boxplot(LMSTEP_LIDAR_CL1$RMSE)$stats[3]

    ids412a <- list()
    ids412a <- LMSTEP_LIDAR$NumSamp == "CLASS2"
    LMSTEP_LIDAR_CL2 <- LMSTEP_LIDAR[ids412a,]
    LMSTEP_LIDAR_CL2_stat <- boxplot(LMSTEP_LIDAR_CL2$RMSE)$stats[3]

    ids413a <- list()
    ids413a <- LMSTEP_LIDAR$NumSamp == "CLASS3"
    LMSTEP_LIDAR_CL3 <- LMSTEP_LIDAR[ids413a,]
    LMSTEP_LIDAR_CL3_stat <- boxplot(LMSTEP_LIDAR_CL3$RMSE)$stats[3]

    ids414a <- list()
    ids414a <- LMSTEP_LIDAR$NumSamp == "CLASS4"
    LMSTEP_LIDAR_CL4 <- LMSTEP_LIDAR[ids414a,]
    LMSTEP_LIDAR_CL4_stat <- boxplot(LMSTEP_LIDAR_CL4$RMSE)$stats[3]

  ids43 <- list()
  ids43 <- LMSTEP$InData == "LIDARHS"
  LMSTEP_LIDARHS <- LMSTEP[ids43,]

    ids411 <- list()
    ids411 <- LMSTEP_LIDARHS$NumSamp == "CLASS1"
    LMSTEP_LIDARHS_CL1 <- LMSTEP_LIDARHS[ids411,]
    LMSTEP_LIDARHS_CL1_stat <- boxplot(LMSTEP_LIDARHS_CL1$RMSE)$stats[3]

    ids412 <- list()
    ids412 <- LMSTEP_LIDARHS$NumSamp == "CLASS2"
    LMSTEP_LIDARHS_CL2 <- LMSTEP_LIDARHS[ids412,]
    LMSTEP_LIDARHS_CL2_stat <- boxplot(LMSTEP_LIDARHS_CL2$RMSE)$stats[3]

    ids413 <- list()
    ids413 <- LMSTEP_LIDARHS$NumSamp == "CLASS3"
    LMSTEP_LIDARHS_CL3 <- LMSTEP_LIDARHS[ids413,]
    LMSTEP_LIDARHS_CL3_stat <- boxplot(LMSTEP_LIDARHS_CL3$RMSE)$stats[3]

    ids414 <- list()
    ids414 <- LMSTEP_LIDARHS$NumSamp == "CLASS4"
    LMSTEP_LIDARHS_CL4 <- LMSTEP_LIDARHS[ids414,]
    LMSTEP_LIDARHS_CL4_stat <- boxplot(LMSTEP_LIDARHS_CL4$RMSE)$stats[3]



######################  RF

ids5 <- list()
ids5 <- inData$ExPoMeth == "RF"
RF <- inData[ids5,]

  ids51 <- list()
  ids51 <- RF$InData == "HS"
  RF_HS <- RF[ids51,]
    
    ids511 <- list()
    ids511 <- RF_HS$NumSamp == "CLASS1"
    RF_HS_CL1 <- RF_HS[ids511,]
    RF_HS_CL1_stat <- boxplot(RF_HS_CL1$RMSE)$stats[3]
    
    ids512 <- list()
    ids512 <- RF_HS$NumSamp == "CLASS2"
    RF_HS_CL2 <- RF_HS[ids512,]
    RF_HS_CL2_stat <- boxplot(RF_HS_CL2$RMSE)$stats[3]

    ids513 <- list()
    ids513 <- RF_HS$NumSamp == "CLASS3"
    RF_HS_CL3 <- RF_HS[ids513,]
    RF_HS_CL3_stat <- boxplot(RF_HS_CL3$RMSE)$stats[3]

    ids514 <- list()
    ids514 <- RF_HS$NumSamp == "CLASS4"
    RF_HS_CL4 <- RF_HS[ids514,]
    RF_HS_CL4_stat <- boxplot(RF_HS_CL4$RMSE)$stats[3]


  ids52 <- list()
  ids52 <- RF$InData == "LIDAR"
  RF_LIDAR <- RF[ids52,]

    ids511a <- list()
    ids511a <- RF_LIDAR$NumSamp == "CLASS1"
    RF_LIDAR_CL1 <- RF_LIDAR[ids511a,]
    RF_LIDAR_CL1_stat <- boxplot(RF_LIDAR_CL1$RMSE)$stats[3]

    ids512a <- list()
    ids512a <- RF_LIDAR$NumSamp == "CLASS2"
    RF_LIDAR_CL2 <- RF_LIDAR[ids512a,]
    RF_LIDAR_CL2_stat <- boxplot(RF_LIDAR_CL2$RMSE)$stats[3]

    ids513a <- list()
    ids513a <- RF_LIDAR$NumSamp == "CLASS3"
    RF_LIDAR_CL3 <- RF_LIDAR[ids513a,]
    RF_LIDAR_CL3_stat <- boxplot(RF_LIDAR_CL3$RMSE)$stats[3]

    ids514a <- list()
    ids514a <- RF_LIDAR$NumSamp == "CLASS4"
    RF_LIDAR_CL4 <- RF_LIDAR[ids514a,]
    RF_LIDAR_CL4_stat <- boxplot(RF_LIDAR_CL4$RMSE)$stats[3]

  ids53 <- list()
  ids53 <- RF$InData == "LIDARHS"
  RF_LIDARHS <- RF[ids53,]

    ids511 <- list()
    ids511 <- RF_LIDARHS$NumSamp == "CLASS1"
    RF_LIDARHS_CL1 <- RF_LIDARHS[ids511,]
    RF_LIDARHS_CL1_stat <- boxplot(RF_LIDARHS_CL1$RMSE)$stats[3]

    ids512 <- list()
    ids512 <- RF_LIDARHS$NumSamp == "CLASS2"
    RF_LIDARHS_CL2 <- RF_LIDARHS[ids512,]
    RF_LIDARHS_CL2_stat <- boxplot(RF_LIDARHS_CL2$RMSE)$stats[3]

    ids513 <- list()
    ids513 <- RF_LIDARHS$NumSamp == "CLASS3"
    RF_LIDARHS_CL3 <- RF_LIDARHS[ids513,]
    RF_LIDARHS_CL3_stat <- boxplot(RF_LIDARHS_CL3$RMSE)$stats[3]

    ids514 <- list()
    ids514 <- RF_LIDARHS$NumSamp == "CLASS4"
    RF_LIDARHS_CL4 <- RF_LIDARHS[ids514,]
    RF_LIDARHS_CL4_stat <- boxplot(RF_LIDARHS_CL4$RMSE)$stats[3]



#####################
####################
### PLOTS


# Plot for Hyperspectral predictors

#pdf("Fig_S8_Results_Karlsruhe_RMSE.pdf", paper="a4", width=7, height = 14)

tiff(filename = "Fig_S8_Results_Karlsruhe_RMSE.tif",
     width = 20, height = 35, units = "cm", pointsize = 18, compression = "lzw", bg = "white", res = 600)

par(cex=2.5)
par(mfrow=c(3,1), mar=c(4,5,1.8,1))


beanplot(SVM_HS$RMSE, KNN_HS$RMSE, GP_HS$RMSE, LMSTEP_HS$RMSE, RF_HS$RMSE, main="Results Hyperspectral Predictors", xlab="", ylab="RMSE [t/ha]", ylim=c(25,95), axes=F, log="", cex=2.5)
axis(1, at=1:5, labels=c("SVM", "KNN", "GP", "LMSTEP", "RF"), cex=2.5)
axis(2, ylab="RMSE [t/ha]", cex=2.5)
box()

segments(0.65,SVM_HS_CL1_stat, 0.75, SVM_HS_CL1_stat, col="red", lwd=3)
segments(0.85,SVM_HS_CL2_stat, 0.95, SVM_HS_CL2_stat, col="blue", lwd=3)
segments(1.05,SVM_HS_CL3_stat, 1.15, SVM_HS_CL3_stat, col="orange", lwd=3)
segments(1.25,SVM_HS_CL4_stat, 1.35, SVM_HS_CL4_stat, col="green", lwd=3)

segments(1.65,KNN_HS_CL1_stat, 1.75, KNN_HS_CL1_stat, col="red", lwd=3)
segments(1.85,KNN_HS_CL2_stat, 1.95, KNN_HS_CL2_stat, col="blue", lwd=3)
segments(2.05,KNN_HS_CL3_stat, 2.15, KNN_HS_CL3_stat, col="orange", lwd=3)
segments(2.25,KNN_HS_CL4_stat, 2.35, KNN_HS_CL4_stat, col="green", lwd=3)

segments(2.65,GP_HS_CL1_stat, 2.75, GP_HS_CL1_stat, col="red", lwd=3)
segments(2.85,GP_HS_CL2_stat, 2.95, GP_HS_CL2_stat, col="blue", lwd=3)
segments(3.05,GP_HS_CL3_stat, 3.15, GP_HS_CL3_stat, col="orange", lwd=3)
segments(3.25,GP_HS_CL4_stat, 3.35, GP_HS_CL4_stat, col="green", lwd=3)

segments(3.65,LMSTEP_HS_CL1_stat, 3.75, LMSTEP_HS_CL1_stat, col="red", lwd=3)
segments(3.85,LMSTEP_HS_CL2_stat, 3.95, LMSTEP_HS_CL2_stat, col="blue", lwd=3)
segments(4.05,LMSTEP_HS_CL3_stat, 4.15, LMSTEP_HS_CL3_stat, col="orange", lwd=3)
segments(4.25,LMSTEP_HS_CL4_stat, 4.35, LMSTEP_HS_CL4_stat, col="green", lwd=3)

segments(4.65,RF_HS_CL1_stat, 4.75, RF_HS_CL1_stat, col="red", lwd=3)
segments(4.85,RF_HS_CL2_stat, 4.95, RF_HS_CL2_stat, col="blue", lwd=3)
segments(5.05,RF_HS_CL3_stat, 5.15, RF_HS_CL3_stat, col="orange", lwd=3)
segments(5.25,RF_HS_CL4_stat, 5.35, RF_HS_CL4_stat, col="green", lwd=3)

#dev.off()

# Plot for Lidar predictors
#tiff(filename = "Lidar_Pred_Results.tif",
#     width = 30, height = 20, units = "cm", pointsize = 18, compression = "lzw", bg = "white", res = 600)

beanplot(SVM_LIDAR$RMSE, KNN_LIDAR$RMSE, GP_LIDAR$RMSE, LMSTEP_LIDAR$RMSE, RF_LIDAR$RMSE, main="Results Lidar Predictors", xlab="", ylab="RMSE [t/ha]", ylim=c(25,95), axes=F, log="", cex=2.5)
axis(1, at=1:5, labels=c("SVM", "KNN", "GP", "LMSTEP", "RF"), cex=2.5)
axis(2, ylab="RMSE [t/ha]", cex=2.5)
box()

segments(0.65,SVM_LIDAR_CL1_stat, 0.75, SVM_LIDAR_CL1_stat, col="red", lwd=3)
segments(0.85,SVM_LIDAR_CL2_stat, 0.95, SVM_LIDAR_CL2_stat, col="blue", lwd=3)
segments(1.05,SVM_LIDAR_CL3_stat, 1.15, SVM_LIDAR_CL3_stat, col="orange", lwd=3)
segments(1.25,SVM_LIDAR_CL4_stat, 1.35, SVM_LIDAR_CL4_stat, col="green", lwd=3)

segments(1.65,KNN_LIDAR_CL1_stat, 1.75, KNN_LIDAR_CL1_stat, col="red", lwd=3)
segments(1.85,KNN_LIDAR_CL2_stat, 1.95, KNN_LIDAR_CL2_stat, col="blue", lwd=3)
segments(2.05,KNN_LIDAR_CL3_stat, 2.15, KNN_LIDAR_CL3_stat, col="orange", lwd=3)
segments(2.25,KNN_LIDAR_CL4_stat, 2.35, KNN_LIDAR_CL4_stat, col="green", lwd=3)

segments(2.65,GP_LIDAR_CL1_stat, 2.75, GP_LIDAR_CL1_stat, col="red", lwd=3)
segments(2.85,GP_LIDAR_CL2_stat, 2.95, GP_LIDAR_CL2_stat, col="blue", lwd=3)
segments(3.05,GP_LIDAR_CL3_stat, 3.15, GP_LIDAR_CL3_stat, col="orange", lwd=3)
segments(3.25,GP_LIDAR_CL4_stat, 3.35, GP_LIDAR_CL4_stat, col="green", lwd=3)

segments(3.65,LMSTEP_LIDAR_CL1_stat, 3.75, LMSTEP_LIDAR_CL1_stat, col="red", lwd=3)
segments(3.85,LMSTEP_LIDAR_CL2_stat, 3.95, LMSTEP_LIDAR_CL2_stat, col="blue", lwd=3)
segments(4.05,LMSTEP_LIDAR_CL3_stat, 4.15, LMSTEP_LIDAR_CL3_stat, col="orange", lwd=3)
segments(4.25,LMSTEP_LIDAR_CL4_stat, 4.35, LMSTEP_LIDAR_CL4_stat, col="green", lwd=3)

segments(4.65,RF_LIDAR_CL1_stat, 4.75, RF_LIDAR_CL1_stat, col="red", lwd=3)
segments(4.85,RF_LIDAR_CL2_stat, 4.95, RF_LIDAR_CL2_stat, col="blue", lwd=3)
segments(5.05,RF_LIDAR_CL3_stat, 5.15, RF_LIDAR_CL3_stat, col="orange", lwd=3)
segments(5.25,RF_LIDAR_CL4_stat, 5.35, RF_LIDAR_CL4_stat, col="green", lwd=3)

#dev.off()

# Plot for Lidar + Hyperspectral predictors

#tiff(filename = "LidarHS_Pred_Results.tif",
#     width = 30, height = 20, units = "cm", pointsize = 18, compression = "lzw", bg = "white", res = 600)


beanplot(SVM_LIDARHS$RMSE, KNN_LIDARHS$RMSE, GP_LIDARHS$RMSE, LMSTEP_LIDARHS$RMSE, RF_LIDARHS$RMSE, main="Results Lidar + Hyperspectral Predictors", xlab="", ylab="RMSE [t/ha]", ylim=c(25,95), axes=F, log="", cex=2.5)
axis(1, at=1:5, labels=c("SVM", "KNN", "GP", "LMSTEP", "RF"), cex=2.5)
axis(2, ylab="RMSE [t/ha]", cex=2.5)
box()

segments(0.65,SVM_LIDARHS_CL1_stat, 0.75, SVM_LIDARHS_CL1_stat, col="red", lwd=3)
segments(0.85,SVM_LIDARHS_CL2_stat, 0.95, SVM_LIDARHS_CL2_stat, col="blue", lwd=3)
segments(1.05,SVM_LIDARHS_CL3_stat, 1.15, SVM_LIDARHS_CL3_stat, col="orange", lwd=3)
segments(1.25,SVM_LIDARHS_CL4_stat, 1.35, SVM_LIDARHS_CL4_stat, col="green", lwd=3)

segments(1.65,KNN_LIDARHS_CL1_stat, 1.75, KNN_LIDARHS_CL1_stat, col="red", lwd=3)
segments(1.85,KNN_LIDARHS_CL2_stat, 1.95, KNN_LIDARHS_CL2_stat, col="blue", lwd=3)
segments(2.05,KNN_LIDARHS_CL3_stat, 2.15, KNN_LIDARHS_CL3_stat, col="orange", lwd=3)
segments(2.25,KNN_LIDARHS_CL4_stat, 2.35, KNN_LIDARHS_CL4_stat, col="green", lwd=3)

segments(2.65,GP_LIDARHS_CL1_stat, 2.75, GP_LIDARHS_CL1_stat, col="red", lwd=3)
segments(2.85,GP_LIDARHS_CL2_stat, 2.95, GP_LIDARHS_CL2_stat, col="blue", lwd=3)
segments(3.05,GP_LIDARHS_CL3_stat, 3.15, GP_LIDARHS_CL3_stat, col="orange", lwd=3)
segments(3.25,GP_LIDARHS_CL4_stat, 3.35, GP_LIDARHS_CL4_stat, col="green", lwd=3)

segments(3.65,LMSTEP_LIDARHS_CL1_stat, 3.75, LMSTEP_LIDARHS_CL1_stat, col="red", lwd=3)
segments(3.85,LMSTEP_LIDARHS_CL2_stat, 3.95, LMSTEP_LIDARHS_CL2_stat, col="blue", lwd=3)
segments(4.05,LMSTEP_LIDARHS_CL3_stat, 4.15, LMSTEP_LIDARHS_CL3_stat, col="orange", lwd=3)
segments(4.25,LMSTEP_LIDARHS_CL4_stat, 4.35, LMSTEP_LIDARHS_CL4_stat, col="green", lwd=3)

segments(4.65,RF_LIDARHS_CL1_stat, 4.75, RF_LIDARHS_CL1_stat, col="red", lwd=3)
segments(4.85,RF_LIDARHS_CL2_stat, 4.95, RF_LIDARHS_CL2_stat, col="blue", lwd=3)
segments(5.05,RF_LIDARHS_CL3_stat, 5.15, RF_LIDARHS_CL3_stat, col="orange", lwd=3)
segments(5.25,RF_LIDARHS_CL4_stat, 5.35, RF_LIDARHS_CL4_stat, col="green", lwd=3)

dev.off()

