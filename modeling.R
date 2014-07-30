## R-Script - modeling steps
## author: Fabian Fassnacht
## mail: fabian.fassnacht@gmx.de
## Manuscript: Importance of sample size, data type and prediction method for remote sensing-based estimations of aboveground forest biomass. Submitted to Remote Sensing of Environment
## last changes: 30.07.2014
##

library(kernlab)
library(caret)

##### set working directory
setwd("D:/mydata")


##### load data table with biomass values and predictors of test sites Monte Oscuro & Karlsruhe

biom <- read.table("data.txt", sep="\t", header = TRUE)
head(biom)
str(biom)

##### load identical table with data for the full plot (to create predictions and biomass maps)

fullTS_t <- read.table("KA_predictors_fullTS.txt", sep="\t", header = TRUE)
fullTS <- fullTS_t[,1:14]

head(biom)
head(fullTS)

head(fullTS_t)
head(biom)

### create subset from input dataset (if neeeded)
#biom <- biom[, 1:18]


#############################################
## stratified bootstrap selection of samples
#############################################

# create groups of same sizes, ordered according to their biomass values (input dataset has to be 
# ordered from lowest to highest biomass)

nr_rows <- nrow(biom)
nr_rows_1 <- ceiling(nr_rows / 5)
nr_rows_2 <- nr_rows - 4*nr_rows_1


biom_g1 <- biom[1:nr_rows_1,]
biom_g2 <- biom[(nr_rows_1+1):(2*nr_rows_1),]
biom_g3 <- biom[(2*nr_rows_1+1):(3*nr_rows_1),]
biom_g4 <- biom[(3*nr_rows_1+1):(4*nr_rows_1),]
biom_g5 <- biom[(4*nr_rows_1+1):(nr_rows),]

# select bootstrapped subset of samples 

set.seed(1173)

# create empty lists in which subsets can be stored
input_cl1 <- list()
input_cl2 <- list()
input_cl3 <- list()
input_cl4 <- list()

id_cl1 <- list()
id_cl2 <- list()
id_cl3 <- list()
id_cl4 <- list()

id_cl1a <- list()
id_cl2a <- list()
id_cl3a <- list()
id_cl4a <- list()

# start loop

N = 500
for (i in 1:N){

# create random numbers with replacement to select samples from each group
  
idx1=sample(1:nr_rows_1,(nr_rows_1/4),replace=TRUE)
idx2=sample(1:nr_rows_1,(nr_rows_1/3),replace=TRUE)
idx3=sample(1:nr_rows_1,(nr_rows_1/2),replace=TRUE)
idx4=sample(1:nr_rows_1, nr_rows_1,replace=TRUE)

id_cl1[[i]] <- idx1
id_cl2[[i]] <- idx2
id_cl3[[i]] <- idx3
id_cl4[[i]] <- idx4

# create random numbers with replacement to select samples from the last group which differs in 
# length (due to fact that the full number of samples is not always smoothly dividable by 5)

idx1a = sample(1:nr_rows_2, (nr_rows_2/4),replace=TRUE)
idx2a = sample(1:nr_rows_2, (nr_rows_2/3),replace=TRUE)
idx3a = sample(1:nr_rows_2, (nr_rows_2/2),replace=TRUE)
idx4a = sample(1:nr_rows_2, nr_rows_2, replace=TRUE)

id_cl1a[[i]] <- idx1a
id_cl2a[[i]] <- idx2a
id_cl3a[[i]] <- idx3a
id_cl4a[[i]] <- idx4a

# select subsets of the five groups based on the random numbers

g1_sub <- biom_g1[idx1,]
g2_sub <- biom_g2[idx1,]
g3_sub <- biom_g3[idx1,]
g4_sub <- biom_g4[idx1,]
g5_sub <- biom_g5[idx1a,]

# combine the subsets of the groups

samp_cl1 <- rbind(g1_sub, g2_sub, g3_sub, g4_sub, g5_sub)

# store the combined groups to the list file

input_cl1[[i]]<- samp_cl1

# starting here this process is repeated for all 5 classes to be created (classes differ in number of total samples)

g1_sub <- biom_g1[idx2,]
g2_sub <- biom_g2[idx2,]
g3_sub <- biom_g3[idx2,]
g4_sub <- biom_g4[idx2,]
g5_sub <- biom_g5[idx2a,]

samp_cl2 <- rbind(g1_sub, g2_sub, g3_sub, g4_sub, g5_sub)
input_cl2[[i]]<- samp_cl2

g1_sub <- biom_g1[idx3,]
g2_sub <- biom_g2[idx3,]
g3_sub <- biom_g3[idx3,]
g4_sub <- biom_g4[idx3,]
g5_sub <- biom_g5[idx3a,]

samp_cl3 <- rbind(g1_sub, g2_sub, g3_sub, g4_sub, g5_sub)
input_cl3[[i]]<- samp_cl3


g1_sub <- biom_g1[idx4,]
g2_sub <- biom_g2[idx4,]
g3_sub <- biom_g3[idx4,]
g4_sub <- biom_g4[idx4,]
g5_sub <- biom_g5[idx4a,]

samp_cl4 <- rbind(g1_sub, g2_sub, g3_sub, g4_sub, g5_sub)
input_cl4[[i]]<- samp_cl4

}


##################################
#
# result: 5 datasets: input_cl1 - input_cl4 + full dataset biom as input to the model building process
# each dataset except for biom has N repetitions of stratified bootstrapped subsets of different sizes 
# of the original dataset (class1 has lowest samples, biom has all samples included)
#
##################################




#############################################
## start regression modelling with caret
#############################################

#############################################
## obtain results for class 1
#############################################

# create empty containers to store the results

SVMres <- matrix(0, ncol = 9, nrow = N)
KNNres <- matrix(0, ncol = 9, nrow = N)
GPres <- matrix(0, ncol = 9, nrow = N)
LMSTEPres <- matrix(0, ncol = 9, nrow = N)
RFres <- matrix(0, ncol = 9, nrow = N)
BRTres <- matrix(0, ncol = 9, nrow = N)

res_svm_cl1 <- list()
res_knn_cl1 <- list()
res_gp_cl1 <- list()
res_lmStep_cl1 <- list()
res_rf_cl1 <- list()

pred_svm_cl1 <- list()
pred_knn_cl1 <- list()
pred_gp_cl1 <- list()
pred_lmStep_cl1 <- list()
pred_rf_cl1 <- list()

resp_cl1 <- list()

fullpred_cl1_svm <- list()
fullpred_cl1_knn <- list()
fullpred_cl1_gp <- list()
fullpred_cl1_lmStep <- list()
fullpred_cl1_rf <- list()
  
# run regressions for all 500 bootstrapped subsets of input_cl1
i2=1
for (i2 in 1:N)
  {

  
# extract first bootstrapped datas-matrix, which contains predictors and response
# list of class1 and transform into a dataframe

  input_rgr <- input_cl1[[i2]]
  input_rgr<-data.frame(input_rgr)

# define response (in this case second column of input dataset)
  response <- input_rgr[,2]

# define predictors (in this case columns 3-18 of input dataset)
  predictors <- cbind(input_rgr[,3], input_rgr[,5:17])
  names(predictors)[1]<-paste("Elev.Mean")


# define parameter tuning methods
  fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, returnResamp ="all")

# tune different models and obtain fit results

    svmFit <- train(predictors, response, method = "svmRadial", trControl = fitControl)
    #svmFit$results
    svmPred <- predict(svmFit)
    svmRes <- svmPred - response
    svmPredFull <- predict(svmFit, fullTS)
  
  
    knnFit <- train(predictors, response, method = "knn", trControl = fitControl)
    #knnFit$results
    knnPred <- predict(knnFit)
    knnRes <-  knnPred - response
    knnPredFull <- predict(knnFit, fullTS)
  
    gpFit <- train(predictors, response, method = "gaussprRadial", trControl = fitControl)
    #gpFit$results
    gpPred <- predict(gpFit)
    gpRes <- gpPred - response
    gpPredFull <- predict(gpFit, fullTS)
  
    lmStepFit <- train(predictors, response, method = "lmStepAIC", trControl = fitControl)
    #lmStepFit$results
    lmStepPred <- predict(lmStepFit)
    lmStepRes <- lmStepPred - response
    lmStepPredFull <- predict(lmStepFit, fullTS)
  
    rfFit <- train(predictors, response, method = "rf", trControl = fitControl)
    #rfFit$results
    rfPred <- predict(rfFit)
    rfRes <- rfPred - response
    rfPredFull <- predict(rfFit, fullTS)

    #brtFit <- train(predictors, response, method = "glmboost", trControl = fitControl)
    #brtFit$results

  
  # extract RMSE and Rsquared values of all models (for each model the model with lowest
  # RMSE value is considered)
  
  #SVM
  #identify model with best setting in terms of lowest RMSE and store the index
  minRMSE <- min(svmFit$results$RMSE)
  svmi <- which(svmFit$results$RMSE==minRMSE,arr.ind=TRUE)

  #extract fit statistics for model with best settings with the help of the index
  #and store results into 8 colmuns of the empty container created before the loop
  #columns 5-8 are added in order to allow ANOVA analysis
  #col5 = extrapolation method, col6 = number of samples, col7= input data, col8 = test site
  SVMres[i2,1] <- svmFit$results$RMSE[svmi]
  SVMres[i2,2] <- svmFit$results$Rsquared[svmi]
  SVMres[i2,3] <- svmFit$results$RMSESD[svmi]
  SVMres[i2,4] <- svmFit$results$RsquaredSD[svmi]
  SVMres[i2,5] <- "SVM"
  SVMres[i2,6] <- "CLASS1"
  SVMres[i2,7] <- "LIDARHS"
  SVMres[i2,8] <- "KA"
  SVMres[i2,9] <- "ALL"
  
  #knn
  minRMSE <- min(knnFit$results$RMSE)
  knni <- which(knnFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  KNNres[i2,1] <- knnFit$results$RMSE[knni]
  KNNres[i2,2] <- knnFit$results$Rsquared[knni]
  KNNres[i2,3] <- knnFit$results$RMSESD[knni]
  KNNres[i2,4] <- knnFit$results$RsquaredSD[knni]
  KNNres[i2,5] <- "KNN"
  KNNres[i2,6] <- "CLASS1"
  KNNres[i2,7] <- "LIDARHS"
  KNNres[i2,8] <- "KA"
  KNNres[i2,9] <- "ALL"
  
  #gp
  minRMSE <- min(gpFit$results$RMSE)
  gpi <- which(gpFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  GPres[i2,1] <- gpFit$results$RMSE[gpi]
  GPres[i2,2] <- gpFit$results$Rsquared[gpi]
  GPres[i2,3] <- gpFit$results$RMSESD[gpi]
  GPres[i2,4] <- gpFit$results$RsquaredSD[gpi]
  GPres[i2,5] <- "GP"
  GPres[i2,6] <- "CLASS1"
  GPres[i2,7] <- "LIDARHS"
  GPres[i2,8] <- "KA"
  GPres[i2,9] <- "ALL"
  
  #lmStep
  minRMSE <- min(lmStepFit$results$RMSE)
  lmstepi <- which(lmStepFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  LMSTEPres[i2,1] <- lmStepFit$results$RMSE[lmstepi]
  LMSTEPres[i2,2] <- lmStepFit$results$Rsquared[lmstepi]
  LMSTEPres[i2,3] <- lmStepFit$results$RMSESD[lmstepi]
  LMSTEPres[i2,4] <- lmStepFit$results$RsquaredSD[lmstepi]
  LMSTEPres[i2,5] <- "LMSTEP"
  LMSTEPres[i2,6] <- "CLASS1"
  LMSTEPres[i2,7] <- "LIDARHS"
  LMSTEPres[i2,8] <- "KA"
  LMSTEPres[i2,9] <- "ALL"
  
  #rf
  minRMSE <- min(rfFit$results$RMSE)
  rfi <- which(rfFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  RFres[i2,1] <- rfFit$results$RMSE[rfi]
  RFres[i2,2] <- rfFit$results$Rsquared[rfi]
  RFres[i2,3] <- rfFit$results$RMSESD[rfi]
  RFres[i2,4] <- rfFit$results$RsquaredSD[rfi]
  RFres[i2,5] <- "RF"
  RFres[i2,6] <- "CLASS1"
  RFres[i2,7] <- "LIDARHS"
  RFres[i2,8] <- "KA"
  RFres[i2,9] <- "ALL"
  
#   brt
#   minRMSE <- min(brtFit$results$RMSE)
#   brti <- which(brtFit$results$RMSE==minRMSE,arr.ind=TRUE)
#   
#   BRTres[i2,1] <- brtFit$results$RMSE[brti]
#   BRTres[i2,2] <- brtFit$results$Rsquared[brti]
#   BRTres[i2,3] <- brtFit$results$RMSESD[brti]
#   BRTres[i2,4] <- brtFit$results$RsquaredSD[brti]
#   BRTres[i2,5] <- "BRT"
#   BRTres[i2,6] <- "CLASS1"
#   BRTres[i2,7] <- "LIDARHS"
#   BRTres[i2,8] <- "KA"
#   BRTres[i2,9] <- "ALL"
  

  res_svm_cl1[[i2]] <- svmRes
  res_knn_cl1[[i2]] <- knnRes
  res_gp_cl1[[i2]] <- gpRes
  res_lmStep_cl1[[i2]] <- lmStepRes
  res_rf_cl1[[i2]] <- rfRes
  
  pred_svm_cl1[[i2]] <- svmPred
  pred_knn_cl1[[i2]] <- knnPred
  pred_gp_cl1[[i2]] <- gpPred
  pred_lmStep_cl1[[i2]] <- lmStepPred
  pred_rf_cl1[[i2]] <- rfPred
  
  resp_cl1[[i2]] <- response
  
  fullpred_cl1_svm[[i2]] <- svmPredFull
  fullpred_cl1_knn[[i2]] <- knnPredFull
  fullpred_cl1_gp[[i2]] <- gpPredFull
  fullpred_cl1_lmStep[[i2]] <- lmStepPredFull
  fullpred_cl1_rf[[i2]] <- rfPredFull
}

### print results to textfiles

setwd("/home/fabi/KIT/9_Biomasse_review_exp/14_rerun_biomass_5_fold/Raw_Results_Orig_models_residuals/KA/Outputs/lidarhs")

write.table(SVMres, file="LIDARHSCl1_SVMres.txt", sep="\t")
write.table(KNNres, file="LIDARHSCl1_1KNNres.txt", sep="\t")
write.table(GPres, file="LIDARHS_Cl1_GPres.txt", sep="\t")
write.table(LMSTEPres, file="LIDARHS_Cl1_LMSTEPres.txt", sep="\t")
write.table(RFres, file="LIDARHS_Cl1_RFres.txt", sep="\t")
#write.table(BRTres, file="LIDARHS_Cl1_BRTres.txt", sep="\t")

save(fullpred_cl1_svm, file = "fullpred_cl1_svm.RData")
save(fullpred_cl1_knn, file = "fullpred_cl1_knn.RData")
save(fullpred_cl1_gp, file = "fullpred_cl1_gp.RData")
save(fullpred_cl1_lmStep, file = "fullpred_cl1_lmStep.RData")
save(fullpred_cl1_rf, file = "fullpred_cl1_rf.RData")


### print residual plots

# print residuals of svm
tiff(filename = "Residuals_Cl1_SVM.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl1[[i2]],res_svm_cl1[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl1[[N]],res_svm_cl1[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot SVM class 1" )
abline(h=0)

dev.off()  

# print residuals of knn
tiff(filename = "Residuals_Cl1_KNN.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl1[[i2]],res_knn_cl1[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl1[[N]],res_knn_cl1[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot KNN class 1" )
abline(h=0)

dev.off()  

# print residuals of gp
tiff(filename = "Residuals_Cl1_GP.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl1[[i2]],res_gp_cl1[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl1[[N]],res_gp_cl1[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot GP class 1" )
abline(h=0)

dev.off()  

# print residuals of lmstep
tiff(filename = "Residuals_Cl1_lmStep.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl1[[i2]],res_lmStep_cl1[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl1[[N]],res_lmStep_cl1[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot lmStep class 1" )
abline(h=0)

dev.off() 

# print residuals of rf


tiff(filename = "Residuals_Cl1_RF.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl1[[i2]],res_rf_cl1[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")

}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl1[[N]],res_rf_cl1[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot RF class 1" )
abline(h=0)

dev.off()  

### save residuals, predicted and response values to .RData files
save(resp_cl1, file = "resp_cl1_lidarhs_KA.RData")

save(res_svm_cl1, file = "res_svm_cl1_lidarhs_KA.RData")
save(res_knn_cl1, file = "res_knn_cl1_lidarhs_KA.RData")
save(res_gp_cl1, file = "res_gp_cl1_lidarhs_KA.RData")
save(res_lmStep_cl1, file = "res_lmStep_cl1_lidarhs_KA.RData")
save(res_rf_cl1, file = "res_rf_cl1_lidarhs_KA.RData")

save(pred_svm_cl1, file = "pred_svm_cl1_lidarhs_KA.RData")
save(pred_knn_cl1, file = "pred_knn_cl1_lidarhs_KA.RData")
save(pred_gp_cl1, file = "pred_gp_cl1_lidarhs_KA.RData")
save(pred_lmStep_cl1, file = "pred_lmStep_cl1_lidarhs_KA.RData")
save(pred_rf_cl1, file = "pred_rf_cl1_lidarhs_KA.RData")



#############################################
## obtain results for class 2
#############################################

# create empty containers to store the results

SVMres <- matrix(0, ncol = 9, nrow = N)
KNNres <- matrix(0, ncol = 9, nrow = N)
GPres <- matrix(0, ncol = 9, nrow = N)
LMSTEPres <- matrix(0, ncol = 9, nrow = N)
RFres <- matrix(0, ncol = 9, nrow = N)
BRTres <- matrix(0, ncol = 9, nrow = N)

res_svm_cl2 <- list()
res_knn_cl2 <- list()
res_gp_cl2 <- list()
res_lmStep_cl2 <- list()
res_rf_cl2 <- list()

pred_svm_cl2 <- list()
pred_knn_cl2 <- list()
pred_gp_cl2 <- list()
pred_lmStep_cl2 <- list()
pred_rf_cl2 <- list()

resp_cl2 <- list()

fullpred_cl2_svm <- list()
fullpred_cl2_knn <- list()
fullpred_cl2_gp <- list()
fullpred_cl2_lmStep <- list()
fullpred_cl2_rf <- list()

# run regressions for all 500 bootstrapped subsets of input_cl1

for (i2 in 1:N)
{
  
  
  # extract first bootstrapped datas-matrix, which contains predictors and response
  # list of class1 and transform into a dataframe
  
  input_rgr <- input_cl2[[i2]]
  input_rgr<-data.frame(input_rgr)
  
  # define response (in this case second column of input dataset)
  response <- input_rgr[,2]
  
  # define predictors (in this case columns 3-18 of input dataset)
  predictors <- cbind(input_rgr[,3], input_rgr[,5:17])
  names(predictors)[1]<-paste("Elev.Mean")
  
  
  # define parameter tuning methods
  fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, returnResamp ="all")
  
  # tune different models and obtain fit results
  
    svmFit <- train(predictors, response, method = "svmRadial", trControl = fitControl)
    #svmFit$results
    svmPred <- predict(svmFit)
    svmRes <- svmPred - response
    svmPredFull <- predict(svmFit, fullTS)
  
    knnFit <- train(predictors, response, method = "knn", trControl = fitControl)
    #knnFit$results
    knnPred <- predict(knnFit)
    knnRes <-  knnPred - response
    knnPredFull <- predict(knnFit, fullTS)
  
    gpFit <- train(predictors, response, method = "gaussprRadial", trControl = fitControl)
    #gpFit$results
    gpPred <- predict(gpFit)
    gpRes <- gpPred - response
    gpPredFull <- predict(gpFit, fullTS)
  
    lmStepFit <- train(predictors, response, method = "lmStepAIC", trControl = fitControl)
    #lmStepFit$results
    lmStepPred <- predict(lmStepFit)
    lmStepRes <- lmStepPred - response
    lmStepPredFull <- predict(lmStepFit, fullTS)
  
    rfFit <- train(predictors, response, method = "rf", trControl = fitControl)
    #rfFit$results
    rfPred <- predict(rfFit)
    rfRes <- rfPred - response
    rfPredFull <- predict(rfFit, fullTS)
  
  #brtFit <- train(predictors, response, method = "glmboost", trControl = fitControl)
  #brtFit$results
  
  
  # extract RMSE and Rsquared values of all models (for each model the model with lowest
  # RMSE value is considered)
  
  #SVM
  #identify model with best setting in terms of lowest RMSE and store the index
  minRMSE <- min(svmFit$results$RMSE)
  svmi <- which(svmFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  #extract fit statistics for model with best settings with the help of the index
  #and store results into 8 colmuns of the empty container created before the loop
  #columns 5-8 are added in order to allow ANOVA analysis
  #col5 = extrapolation method, col6 = number of samples, col7= input data, col8 = test site
  SVMres[i2,1] <- svmFit$results$RMSE[svmi]
  SVMres[i2,2] <- svmFit$results$Rsquared[svmi]
  SVMres[i2,3] <- svmFit$results$RMSESD[svmi]
  SVMres[i2,4] <- svmFit$results$RsquaredSD[svmi]
  SVMres[i2,5] <- "SVM"
  SVMres[i2,6] <- "CLASS2"
  SVMres[i2,7] <- "LIDARHS"
  SVMres[i2,8] <- "KA"
  SVMres[i2,9] <- "ALL"
  
  #knn
  minRMSE <- min(knnFit$results$RMSE)
  knni <- which(knnFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  KNNres[i2,1] <- knnFit$results$RMSE[knni]
  KNNres[i2,2] <- knnFit$results$Rsquared[knni]
  KNNres[i2,3] <- knnFit$results$RMSESD[knni]
  KNNres[i2,4] <- knnFit$results$RsquaredSD[knni]
  KNNres[i2,5] <- "KNN"
  KNNres[i2,6] <- "CLASS2"
  KNNres[i2,7] <- "LIDARHS"
  KNNres[i2,8] <- "KA"
  KNNres[i2,9] <- "ALL"
  
  #gp
  minRMSE <- min(gpFit$results$RMSE)
  gpi <- which(gpFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  GPres[i2,1] <- gpFit$results$RMSE[gpi]
  GPres[i2,2] <- gpFit$results$Rsquared[gpi]
  GPres[i2,3] <- gpFit$results$RMSESD[gpi]
  GPres[i2,4] <- gpFit$results$RsquaredSD[gpi]
  GPres[i2,5] <- "GP"
  GPres[i2,6] <- "CLASS2"
  GPres[i2,7] <- "LIDARHS"
  GPres[i2,8] <- "KA"
  GPres[i2,9] <- "ALL"
  
  #lmStep
  minRMSE <- min(lmStepFit$results$RMSE)
  lmstepi <- which(lmStepFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  LMSTEPres[i2,1] <- lmStepFit$results$RMSE[lmstepi]
  LMSTEPres[i2,2] <- lmStepFit$results$Rsquared[lmstepi]
  LMSTEPres[i2,3] <- lmStepFit$results$RMSESD[lmstepi]
  LMSTEPres[i2,4] <- lmStepFit$results$RsquaredSD[lmstepi]
  LMSTEPres[i2,5] <- "LMSTEP"
  LMSTEPres[i2,6] <- "CLASS2"
  LMSTEPres[i2,7] <- "LIDARHS"
  LMSTEPres[i2,8] <- "KA"
  LMSTEPres[i2,9] <- "ALL"
  
  #rf
  minRMSE <- min(rfFit$results$RMSE)
  rfi <- which(rfFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  RFres[i2,1] <- rfFit$results$RMSE[rfi]
  RFres[i2,2] <- rfFit$results$Rsquared[rfi]
  RFres[i2,3] <- rfFit$results$RMSESD[rfi]
  RFres[i2,4] <- rfFit$results$RsquaredSD[rfi]
  RFres[i2,5] <- "RF"
  RFres[i2,6] <- "CLASS2"
  RFres[i2,7] <- "LIDARHS"
  RFres[i2,8] <- "KA"
  RFres[i2,9] <- "ALL"
  
  #   brt
  #   minRMSE <- min(brtFit$results$RMSE)
  #   brti <- which(brtFit$results$RMSE==minRMSE,arr.ind=TRUE)
  #   
  #   BRTres[i2,1] <- brtFit$results$RMSE[brti]
  #   BRTres[i2,2] <- brtFit$results$Rsquared[brti]
  #   BRTres[i2,3] <- brtFit$results$RMSESD[brti]
  #   BRTres[i2,4] <- brtFit$results$RsquaredSD[brti]
  #   BRTres[i2,5] <- "BRT"
  #   BRTres[i2,6] <- "CLASS1"
  #   BRTres[i2,7] <- "LIDARHS"
  #   BRTres[i2,8] <- "KA"
  #   BRTres[i2,9] <- "ALL"
  
  
  res_svm_cl2[[i2]] <- svmRes
  res_knn_cl2[[i2]] <- knnRes
  res_gp_cl2[[i2]] <- gpRes
  res_lmStep_cl2[[i2]] <- lmStepRes
  res_rf_cl2[[i2]] <- rfRes
  
  pred_svm_cl2[[i2]] <- svmPred
  pred_knn_cl2[[i2]] <- knnPred
  pred_gp_cl2[[i2]] <- gpPred
  pred_lmStep_cl2[[i2]] <- lmStepPred
  pred_rf_cl2[[i2]] <- rfPred
  
  resp_cl2[[i2]] <- response
  
  fullpred_cl2_svm[[i2]] <- svmPredFull
  fullpred_cl2_knn[[i2]] <- knnPredFull
  fullpred_cl2_gp[[i2]] <- gpPredFull
  fullpred_cl2_lmStep[[i2]] <- lmStepPredFull
  fullpred_cl2_rf[[i2]] <- rfPredFull
}

### print results to textfiles

#setwd("/home/fabi/FELIS/9_Biomasse_Review_neu/Modelling/Raw_Results_Orig_models_residuals/KA/Outputs/lidarhs")

write.table(SVMres, file="LIDARHS_Cl2_SVMres.txt", sep="\t")
write.table(KNNres, file="LIDARHS_Cl2_1KNNres.txt", sep="\t")
write.table(GPres, file="LIDARHS_Cl2_GPres.txt", sep="\t")
write.table(LMSTEPres, file="LIDARHS_Cl2_LMSTEPres.txt", sep="\t")
write.table(RFres, file="LIDARHS_Cl2_RFres.txt", sep="\t")
#write.table(BRTres, file="LIDARHS_Cl1_BRTres.txt", sep="\t")

save(fullpred_cl2_svm, file = "fullpred_cl2_svm.RData")
save(fullpred_cl2_knn, file = "fullpred_cl2_knn.RData")
save(fullpred_cl2_gp, file = "fullpred_cl2_gp.RData")
save(fullpred_cl2_lmStep, file = "fullpred_cl2_lmStep.RData")
save(fullpred_cl2_rf, file = "fullpred_cl2_rf.RData")

### print residual plots

# print residuals of svm
dev.off() 
tiff(filename = "Residuals_Cl2_SVM.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl2[[i2]],res_svm_cl2[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl2[[N]],res_svm_cl2[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot SVM class 2" )
abline(h=0)

dev.off()  

# print residuals of knn
tiff(filename = "Residuals_Cl2_KNN.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl2[[i2]],res_knn_cl2[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl2[[N]],res_knn_cl2[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot KNN class 2" )
abline(h=0)

dev.off()  

# print residuals of gp
tiff(filename = "Residuals_Cl2_GP.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl2[[i2]],res_gp_cl2[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl2[[N]],res_gp_cl2[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot GP class 2" )
abline(h=0)

dev.off()  

# print residuals of lmstep
tiff(filename = "Residuals_Cl2_lmStep.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl2[[i2]],res_lmStep_cl2[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl2[[N]],res_lmStep_cl2[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot lmStep class 2" )
abline(h=0)

dev.off() 

# print residuals of rf


tiff(filename = "Residuals_Cl2_RF.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl2[[i2]],res_rf_cl2[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl2[[N]],res_rf_cl2[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot RF class 2" )
abline(h=0)

dev.off()  

### save residuals, predicted and response values to .RData files
save(resp_cl2, file = "resp_cl2_lidarhs_KA.RData")

save(res_svm_cl2, file = "res_svm_cl2_lidarhs_KA.RData")
save(res_knn_cl2, file = "res_knn_cl2_lidarhs_KA.RData")
save(res_gp_cl2, file = "res_gp_cl2_lidarhs_KA.RData")
save(res_lmStep_cl2, file = "res_lmStep_cl2_lidarhs_KA.RData")
save(res_rf_cl2, file = "res_rf_cl2_lidarhs_KA.RData")

save(pred_svm_cl2, file = "pred_svm_cl2_lidarhs_KA.RData")
save(pred_knn_cl2, file = "pred_knn_cl2_lidarhs_KA.RData")
save(pred_gp_cl2, file = "pred_gp_cl2_lidarhs_KA.RData")
save(pred_lmStep_cl2, file = "pred_lmStep_cl2_lidarhs_KA.RData")
save(pred_rf_cl2, file = "pred_rf_cl2_lidarhs_KA.RData")

fullpred_cl3_svm <- list()
fullpred_cl3_knn <- list()
fullpred_cl3_gp <- list()
fullpred_cl3_lmStep <- list()
fullpred_cl3_rf <- list()


#############################################
## obtain results for class 3
#############################################

# create empty containers to store the results

SVMres <- matrix(0, ncol = 9, nrow = N)
KNNres <- matrix(0, ncol = 9, nrow = N)
GPres <- matrix(0, ncol = 9, nrow = N)
LMSTEPres <- matrix(0, ncol = 9, nrow = N)
RFres <- matrix(0, ncol = 9, nrow = N)
BRTres <- matrix(0, ncol = 9, nrow = N)

res_svm_cl3 <- list()
res_knn_cl3 <- list()
res_gp_cl3 <- list()
res_lmStep_cl3 <- list()
res_rf_cl3 <- list()

pred_svm_cl3 <- list()
pred_knn_cl3 <- list()
pred_gp_cl3 <- list()
pred_lmStep_cl3 <- list()
pred_rf_cl3 <- list()

resp_cl3 <- list()

# run regressions for all 500 bootstrapped subsets of input_cl1


for (i2 in 1:N)
{
  
  
  # extract first bootstrapped datas-matrix, which contains predictors and response
  # list of class1 and transform into a dataframe
  
  input_rgr <- input_cl3[[i2]]
  input_rgr<-data.frame(input_rgr)
  
  # define response (in this case second column of input dataset)
  response <- input_rgr[,2]
  
  # define predictors (in this case columns 3-18 of input dataset)
  predictors <- cbind(input_rgr[,3], input_rgr[,5:17])
  names(predictors)[1]<-paste("Elev.Mean")
  
  
  # define parameter tuning methods
  fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, returnResamp ="all")
  
  # tune different models and obtain fit results
  
  svmFit <- train(predictors, response, method = "svmRadial", trControl = fitControl)
  #svmFit$results
  svmPred <- predict(svmFit)
  svmRes <- svmPred - response
  svmPredFull <- predict(svmFit, fullTS)
  
  knnFit <- train(predictors, response, method = "knn", trControl = fitControl)
  #knnFit$results
  knnPred <- predict(knnFit)
  knnRes <-  knnPred - response
  knnPredFull <- predict(knnFit, fullTS)
  
  gpFit <- train(predictors, response, method = "gaussprRadial", trControl = fitControl)
  #gpFit$results
  gpPred <- predict(gpFit)
  gpRes <- gpPred - response
  gpPredFull <- predict(gpFit, fullTS)
  
  lmStepFit <- train(predictors, response, method = "lmStepAIC", trControl = fitControl)
  #lmStepFit$results
  lmStepPred <- predict(lmStepFit)
  lmStepRes <- lmStepPred - response
  lmStepPredFull <- predict(lmStepFit, fullTS)
  
  rfFit <- train(predictors, response, method = "rf", trControl = fitControl)
  #rfFit$results
  rfPred <- predict(rfFit)
  rfRes <- rfPred - response
  rfPredFull <- predict(rfFit, fullTS)
  
  #brtFit <- train(predictors, response, method = "glmboost", trControl = fitControl)
  #brtFit$results
  
  
  # extract RMSE and Rsquared values of all models (for each model the model with lowest
  # RMSE value is considered)
  
  #SVM
  #identify model with best setting in terms of lowest RMSE and store the index
  minRMSE <- min(svmFit$results$RMSE)
  svmi <- which(svmFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  #extract fit statistics for model with best settings with the help of the index
  #and store results into 8 colmuns of the empty container created before the loop
  #columns 5-8 are added in order to allow ANOVA analysis
  #col5 = extrapolation method, col6 = number of samples, col7= input data, col8 = test site
  SVMres[i2,1] <- svmFit$results$RMSE[svmi]
  SVMres[i2,2] <- svmFit$results$Rsquared[svmi]
  SVMres[i2,3] <- svmFit$results$RMSESD[svmi]
  SVMres[i2,4] <- svmFit$results$RsquaredSD[svmi]
  SVMres[i2,5] <- "SVM"
  SVMres[i2,6] <- "CLASS3"
  SVMres[i2,7] <- "LIDARHS"
  SVMres[i2,8] <- "KA"
  SVMres[i2,9] <- "ALL"
  
  #knn
  minRMSE <- min(knnFit$results$RMSE)
  knni <- which(knnFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  KNNres[i2,1] <- knnFit$results$RMSE[knni]
  KNNres[i2,2] <- knnFit$results$Rsquared[knni]
  KNNres[i2,3] <- knnFit$results$RMSESD[knni]
  KNNres[i2,4] <- knnFit$results$RsquaredSD[knni]
  KNNres[i2,5] <- "KNN"
  KNNres[i2,6] <- "CLASS3"
  KNNres[i2,7] <- "LIDARHS"
  KNNres[i2,8] <- "KA"
  KNNres[i2,9] <- "ALL"
  
  #gp
  minRMSE <- min(gpFit$results$RMSE)
  gpi <- which(gpFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  GPres[i2,1] <- gpFit$results$RMSE[gpi]
  GPres[i2,2] <- gpFit$results$Rsquared[gpi]
  GPres[i2,3] <- gpFit$results$RMSESD[gpi]
  GPres[i2,4] <- gpFit$results$RsquaredSD[gpi]
  GPres[i2,5] <- "GP"
  GPres[i2,6] <- "CLASS3"
  GPres[i2,7] <- "LIDARHS"
  GPres[i2,8] <- "KA"
  GPres[i2,9] <- "ALL"
  
  #lmStep
  minRMSE <- min(lmStepFit$results$RMSE)
  lmstepi <- which(lmStepFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  LMSTEPres[i2,1] <- lmStepFit$results$RMSE[lmstepi]
  LMSTEPres[i2,2] <- lmStepFit$results$Rsquared[lmstepi]
  LMSTEPres[i2,3] <- lmStepFit$results$RMSESD[lmstepi]
  LMSTEPres[i2,4] <- lmStepFit$results$RsquaredSD[lmstepi]
  LMSTEPres[i2,5] <- "LMSTEP"
  LMSTEPres[i2,6] <- "CLASS3"
  LMSTEPres[i2,7] <- "LIDARHS"
  LMSTEPres[i2,8] <- "KA"
  LMSTEPres[i2,9] <- "ALL"
  
  #rf
  minRMSE <- min(rfFit$results$RMSE)
  rfi <- which(rfFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  RFres[i2,1] <- rfFit$results$RMSE[rfi]
  RFres[i2,2] <- rfFit$results$Rsquared[rfi]
  RFres[i2,3] <- rfFit$results$RMSESD[rfi]
  RFres[i2,4] <- rfFit$results$RsquaredSD[rfi]
  RFres[i2,5] <- "RF"
  RFres[i2,6] <- "CLASS3"
  RFres[i2,7] <- "LIDARHS"
  RFres[i2,8] <- "KA"
  RFres[i2,9] <- "ALL"
  
  #   brt
  #   minRMSE <- min(brtFit$results$RMSE)
  #   brti <- which(brtFit$results$RMSE==minRMSE,arr.ind=TRUE)
  #   
  #   BRTres[i2,1] <- brtFit$results$RMSE[brti]
  #   BRTres[i2,2] <- brtFit$results$Rsquared[brti]
  #   BRTres[i2,3] <- brtFit$results$RMSESD[brti]
  #   BRTres[i2,4] <- brtFit$results$RsquaredSD[brti]
  #   BRTres[i2,5] <- "BRT"
  #   BRTres[i2,6] <- "CLASS1"
  #   BRTres[i2,7] <- "LIDARHS"
  #   BRTres[i2,8] <- "KA"
  #   BRTres[i2,9] <- "ALL"
  
  
  res_svm_cl3[[i2]] <- svmRes
  res_knn_cl3[[i2]] <- knnRes
  res_gp_cl3[[i2]] <- gpRes
  res_lmStep_cl3[[i2]] <- lmStepRes
  res_rf_cl3[[i2]] <- rfRes
  
  pred_svm_cl3[[i2]] <- svmPred
  pred_knn_cl3[[i2]] <- knnPred
  pred_gp_cl3[[i2]] <- gpPred
  pred_lmStep_cl3[[i2]] <- lmStepPred
  pred_rf_cl3[[i2]] <- rfPred
  
  resp_cl3[[i2]] <- response
  
  fullpred_cl3_svm[[i2]] <- svmPredFull
  fullpred_cl3_knn[[i2]] <- knnPredFull
  fullpred_cl3_gp[[i2]] <- gpPredFull
  fullpred_cl3_lmStep[[i2]] <- lmStepPredFull
  fullpred_cl3_rf[[i2]] <- rfPredFull
  
}

save(fullpred_cl3_svm, file = "fullpred_cl3_svm.RData")
save(fullpred_cl3_knn, file = "fullpred_cl3_knn.RData")
save(fullpred_cl3_gp, file = "fullpred_cl3_gp.RData")
save(fullpred_cl3_lmStep, file = "fullpred_cl3_lmStep.RData")
save(fullpred_cl3_rf, file = "fullpred_cl3_rf.RData")

### print results to textfiles

#setwd("/home/fabi/FELIS/9_Biomasse_Review_neu/Modelling/Raw_Results_Orig_models_residuals/KA/Outputs/lidarhs")

write.table(SVMres, file="LIDARHS_Cl3_SVMres.txt", sep="\t")
write.table(KNNres, file="LIDARHS_Cl3_1KNNres.txt", sep="\t")
write.table(GPres, file="LIDARHS_Cl3_GPres.txt", sep="\t")
write.table(LMSTEPres, file="LIDARHS_Cl3_LMSTEPres.txt", sep="\t")
write.table(RFres, file="LIDARHS_Cl3_RFres.txt", sep="\t")
#write.table(BRTres, file="LIDARHS_Cl1_BRTres.txt", sep="\t")



### print residual plots

# print residuals of svm
dev.off() 
tiff(filename = "Residuals_Cl3_SVM.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl3[[i2]],res_svm_cl3[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl3[[N]],res_svm_cl3[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot SVM class 3" )
abline(h=0)

dev.off()  

# print residuals of knn
tiff(filename = "Residuals_Cl3_KNN.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl3[[i2]],res_knn_cl3[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl3[[N]],res_knn_cl3[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot KNN class 3" )
abline(h=0)

dev.off()  

# print residuals of gp
tiff(filename = "Residuals_Cl3_GP.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl3[[i2]],res_gp_cl3[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl3[[N]],res_gp_cl3[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot GP class 3" )
abline(h=0)

dev.off()  

# print residuals of lmstep
tiff(filename = "Residuals_Cl3_lmStep.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl3[[i2]],res_lmStep_cl3[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl3[[N]],res_lmStep_cl3[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot lmStep class 3" )
abline(h=0)

dev.off() 

# print residuals of rf


tiff(filename = "Residuals_Cl3_RF.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl3[[i2]],res_rf_cl3[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl3[[N]],res_rf_cl3[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot RF class 3" )
abline(h=0)

dev.off()  

### save residuals, predicted and response values to .RData files
save(resp_cl3, file = "resp_cl3_lidarhs_KA.RData")

save(res_svm_cl3, file = "res_svm_cl3_lidarhs_KA.RData")
save(res_knn_cl3, file = "res_knn_cl3_lidarhs_KA.RData")
save(res_gp_cl3, file = "res_gp_cl3_lidarhs_KA.RData")
save(res_lmStep_cl3, file = "res_lmStep_cl3_lidarhs_KA.RData")
save(res_rf_cl3, file = "res_rf_cl3_lidarhs_KA.RData")

save(pred_svm_cl3, file = "pred_svm_cl3_lidarhs_KA.RData")
save(pred_knn_cl3, file = "pred_knn_cl3_lidarhs_KA.RData")
save(pred_gp_cl3, file = "pred_gp_cl3_lidarhs_KA.RData")
save(pred_lmStep_cl3, file = "pred_lmStep_cl3_lidarhs_KA.RData")
save(pred_rf_cl3, file = "pred_rf_cl3_lidarhs_KA.RData")



#############################################
## obtain results for class 4
#############################################

# create empty containers to store the results

SVMres <- matrix(0, ncol = 9, nrow = N)
KNNres <- matrix(0, ncol = 9, nrow = N)
GPres <- matrix(0, ncol = 9, nrow = N)
LMSTEPres <- matrix(0, ncol = 9, nrow = N)
RFres <- matrix(0, ncol = 9, nrow = N)
BRTres <- matrix(0, ncol = 9, nrow = N)

res_svm_cl4 <- list()
res_knn_cl4 <- list()
res_gp_cl4 <- list()
res_lmStep_cl4 <- list()
res_rf_cl4 <- list()

pred_svm_cl4 <- list()
pred_knn_cl4 <- list()
pred_gp_cl4 <- list()
pred_lmStep_cl4 <- list()
pred_rf_cl4 <- list()

resp_cl4 <- list()

fullpred_cl4_svm <- list()
fullpred_cl4_knn <- list()
fullpred_cl4_gp <- list()
fullpred_cl4_lmStep <- list()
fullpred_cl4_rf <- list()

# run regressions for all 500 bootstrapped subsets of input_cl1

for (i2 in 1:N)
{
  
  
  # extract first bootstrapped datas-matrix, which contains predictors and response
  # list of class1 and transform into a dataframe
  
  input_rgr <- input_cl4[[i2]]
  input_rgr<-data.frame(input_rgr)
  
  # define response (in this case second column of input dataset)
  response <- input_rgr[,2]
  
  # define predictors (in this case columns 3-18 of input dataset)
  predictors <- cbind(input_rgr[,3], input_rgr[,5:17])
  names(predictors)[1]<-paste("Elev.Mean")
  
  
  # define parameter tuning methods
  fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, returnResamp ="all")
  
  # tune different models and obtain fit results
  
  svmFit <- train(predictors, response, method = "svmRadial", trControl = fitControl)
  #svmFit$results
  svmPred <- predict(svmFit)
  svmRes <- svmPred - response
  svmPredFull <- predict(svmFit, fullTS)
  
  knnFit <- train(predictors, response, method = "knn", trControl = fitControl)
  #knnFit$results
  knnPred <- predict(knnFit)
  knnRes <-  knnPred - response
  knnPredFull <- predict(knnFit, fullTS)
  
  gpFit <- train(predictors, response, method = "gaussprRadial", trControl = fitControl)
  #gpFit$results
  gpPred <- predict(gpFit)
  gpRes <- gpPred - response
  gpPredFull <- predict(gpFit, fullTS)
  
  lmStepFit <- train(predictors, response, method = "lmStepAIC", trControl = fitControl)
  #lmStepFit$results
  lmStepPred <- predict(lmStepFit)
  lmStepRes <- lmStepPred - response
  lmStepPredFull <- predict(lmStepFit, fullTS)
  
  rfFit <- train(predictors, response, method = "rf", trControl = fitControl)
  #rfFit$results
  rfPred <- predict(rfFit)
  rfRes <- rfPred - response
  rfPredFull <- predict(rfFit, fullTS)
  
  #brtFit <- train(predictors, response, method = "glmboost", trControl = fitControl)
  #brtFit$results
  
  
  # extract RMSE and Rsquared values of all models (for each model the model with lowest
  # RMSE value is considered)
  
  #SVM
  #identify model with best setting in terms of lowest RMSE and store the index
  minRMSE <- min(svmFit$results$RMSE)
  svmi <- which(svmFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  #extract fit statistics for model with best settings with the help of the index
  #and store results into 8 colmuns of the empty container created before the loop
  #columns 5-8 are added in order to allow ANOVA analysis
  #col5 = extrapolation method, col6 = number of samples, col7= input data, col8 = test site
  SVMres[i2,1] <- svmFit$results$RMSE[svmi]
  SVMres[i2,2] <- svmFit$results$Rsquared[svmi]
  SVMres[i2,3] <- svmFit$results$RMSESD[svmi]
  SVMres[i2,4] <- svmFit$results$RsquaredSD[svmi]
  SVMres[i2,5] <- "SVM"
  SVMres[i2,6] <- "CLASS4"
  SVMres[i2,7] <- "LIDARHS"
  SVMres[i2,8] <- "KA"
  SVMres[i2,9] <- "ALL"
  
  #knn
  minRMSE <- min(knnFit$results$RMSE)
  knni <- which(knnFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  KNNres[i2,1] <- knnFit$results$RMSE[knni]
  KNNres[i2,2] <- knnFit$results$Rsquared[knni]
  KNNres[i2,3] <- knnFit$results$RMSESD[knni]
  KNNres[i2,4] <- knnFit$results$RsquaredSD[knni]
  KNNres[i2,5] <- "KNN"
  KNNres[i2,6] <- "CLASS4"
  KNNres[i2,7] <- "LIDARHS"
  KNNres[i2,8] <- "KA"
  KNNres[i2,9] <- "ALL"
  
  #gp
  minRMSE <- min(gpFit$results$RMSE)
  gpi <- which(gpFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  GPres[i2,1] <- gpFit$results$RMSE[gpi]
  GPres[i2,2] <- gpFit$results$Rsquared[gpi]
  GPres[i2,3] <- gpFit$results$RMSESD[gpi]
  GPres[i2,4] <- gpFit$results$RsquaredSD[gpi]
  GPres[i2,5] <- "GP"
  GPres[i2,6] <- "CLASS4"
  GPres[i2,7] <- "LIDARHS"
  GPres[i2,8] <- "KA"
  GPres[i2,9] <- "ALL"
  
  #lmStep
  minRMSE <- min(lmStepFit$results$RMSE)
  lmstepi <- which(lmStepFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  LMSTEPres[i2,1] <- lmStepFit$results$RMSE[lmstepi]
  LMSTEPres[i2,2] <- lmStepFit$results$Rsquared[lmstepi]
  LMSTEPres[i2,3] <- lmStepFit$results$RMSESD[lmstepi]
  LMSTEPres[i2,4] <- lmStepFit$results$RsquaredSD[lmstepi]
  LMSTEPres[i2,5] <- "LMSTEP"
  LMSTEPres[i2,6] <- "CLASS4"
  LMSTEPres[i2,7] <- "LIDARHS"
  LMSTEPres[i2,8] <- "KA"
  LMSTEPres[i2,9] <- "ALL"
  
  #rf
  minRMSE <- min(rfFit$results$RMSE)
  rfi <- which(rfFit$results$RMSE==minRMSE,arr.ind=TRUE)
  
  RFres[i2,1] <- rfFit$results$RMSE[rfi]
  RFres[i2,2] <- rfFit$results$Rsquared[rfi]
  RFres[i2,3] <- rfFit$results$RMSESD[rfi]
  RFres[i2,4] <- rfFit$results$RsquaredSD[rfi]
  RFres[i2,5] <- "RF"
  RFres[i2,6] <- "CLASS4"
  RFres[i2,7] <- "LIDARHS"
  RFres[i2,8] <- "KA"
  RFres[i2,9] <- "ALL"
  
  #   brt
  #   minRMSE <- min(brtFit$results$RMSE)
  #   brti <- which(brtFit$results$RMSE==minRMSE,arr.ind=TRUE)
  #   
  #   BRTres[i2,1] <- brtFit$results$RMSE[brti]
  #   BRTres[i2,2] <- brtFit$results$Rsquared[brti]
  #   BRTres[i2,3] <- brtFit$results$RMSESD[brti]
  #   BRTres[i2,4] <- brtFit$results$RsquaredSD[brti]
  #   BRTres[i2,5] <- "BRT"
  #   BRTres[i2,6] <- "CLASS1"
  #   BRTres[i2,7] <- "LIDARHS"
  #   BRTres[i2,8] <- "KA"
  #   BRTres[i2,9] <- "ALL"
  
  
  res_svm_cl4[[i2]] <- svmRes
  res_knn_cl4[[i2]] <- knnRes
  res_gp_cl4[[i2]] <- gpRes
  res_lmStep_cl4[[i2]] <- lmStepRes
  res_rf_cl4[[i2]] <- rfRes
  
  pred_svm_cl4[[i2]] <- svmPred
  pred_knn_cl4[[i2]] <- knnPred
  pred_gp_cl4[[i2]] <- gpPred
  pred_lmStep_cl4[[i2]] <- lmStepPred
  pred_rf_cl4[[i2]] <- rfPred
  
  resp_cl4[[i2]] <- response
  
  fullpred_cl4_svm[[i2]] <- svmPredFull
  fullpred_cl4_knn[[i2]] <- knnPredFull
  fullpred_cl4_gp[[i2]] <- gpPredFull
  fullpred_cl4_lmStep[[i2]] <- lmStepPredFull
  fullpred_cl4_rf[[i2]] <- rfPredFull
  
}

### print results to textfiles

#setwd("/home/fabi/FELIS/9_Biomasse_Review_neu/Modelling/Raw_Results_Orig_models_residuals/KA/Outputs/lidarhs")

write.table(SVMres, file="LIDARHS_Cl4_SVMres.txt", sep="\t")
write.table(KNNres, file="LIDARHS_Cl4_1KNNres.txt", sep="\t")
write.table(GPres, file="LIDARHS_Cl4_GPres.txt", sep="\t")
write.table(LMSTEPres, file="LIDARHS_Cl4_LMSTEPres.txt", sep="\t")
write.table(RFres, file="LIDARHS_Cl4_RFres.txt", sep="\t")
#write.table(BRTres, file="LIDARHS_Cl1_BRTres.txt", sep="\t")

save(fullpred_cl4_svm, file = "fullpred_cl4_svm.RData")
save(fullpred_cl4_knn, file = "fullpred_cl4_knn.RData")
save(fullpred_cl4_gp, file = "fullpred_cl4_gp.RData")
save(fullpred_cl4_lmStep, file = "fullpred_cl4_lmStep.RData")
save(fullpred_cl4_rf, file = "fullpred_cl4_rf.RData")

### print residual plots

# print residuals of svm
dev.off() 
tiff(filename = "Residuals_Cl4_SVM.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl4[[i2]],res_svm_cl4[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl4[[N]],res_svm_cl4[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot SVM class 4" )
abline(h=0)

dev.off()  

# print residuals of knn
tiff(filename = "Residuals_Cl4_KNN.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl4[[i2]],res_knn_cl4[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl4[[N]],res_knn_cl4[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot KNN class 4" )
abline(h=0)

dev.off()  

# print residuals of gp
tiff(filename = "Residuals_Cl4_GP.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl4[[i2]],res_gp_cl4[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl4[[N]],res_gp_cl4[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot GP class 4" )
abline(h=0)

dev.off()  

# print residuals of lmstep
tiff(filename = "Residuals_Cl4_lmStep.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl4[[i2]],res_lmStep_cl4[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl4[[N]],res_lmStep_cl4[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot lmStep class 4" )
abline(h=0)

dev.off() 

# print residuals of rf


tiff(filename = "Residuals_Cl4_RF.tif",
     width = 12, height = 12, units = "cm", pointsize = 8,
     compression = "lzw", bg = "white", res = 600)

for (i2 in 1:(N-1)) {
  par(new=TRUE)
  plot(resp_cl4[[i2]],res_rf_cl4[[i2]], ylim = c(-300,300), xlim=c(0,300), pch='.', axes=F, xlab="", ylab="")
  
}
par(new=TRUE)
par(cex=1.4)
plot(resp_cl4[[N]],res_rf_cl4[[N]], ylim = c(-300,300), xlim=c(0,300), xlab="Response", ylab = "Residuals", pch='.', main = "Residual plot RF class 4" )
abline(h=0)

dev.off()  

### save residuals, predicted and response values to .RData files
save(resp_cl4, file = "resp_cl4_lidarhshs_KA.RData")

save(res_svm_cl4, file = "res_svm_cl4_lidarhs_KA.RData")
save(res_knn_cl4, file = "res_knn_cl4_lidarhs_KA.RData")
save(res_gp_cl4, file = "res_gp_cl4_lidarhs_KA.RData")
save(res_lmStep_cl4, file = "res_lmStep_cl4_lidarhshs_KA.RData")
save(res_rf_cl4, file = "res_rf_cl4_lidarhshs_KA.RData")

save(pred_svm_cl4, file = "pred_svm_cl4_lidarhs_KA.RData")
save(pred_knn_cl4, file = "pred_knn_cl4_lidarhs_KA.RData")
save(pred_gp_cl4, file = "pred_gp_cl4_lidarhs_KA.RData")
save(pred_lmStep_cl4, file = "pred_lmStep_cl4_lidarhs_KA.RData")
save(pred_rf_cl4, file = "pred_rf_cl4_lidarhs_KA.RData")
