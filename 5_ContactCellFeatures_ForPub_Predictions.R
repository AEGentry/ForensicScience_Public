### Age of Evidence: Filtered Contact Cell Features
### Data from Chris Ehrhardt, May 2022
### Scripts by Amanda Elswick Gentry, 2022

### Note, these models were run on Group in Dec 2022 - May 2023
### R version 4.1.1
### glmnet version 4.1.4
### gbm version 2.1.8
### glmmLasso version 1.6.2

### Re-run EL, GBM, and GLMM models for final publications analyses ###
### Script produces the following useful outputs:
### dataframes of predictions Train (6414 x 10) for the training set and
### Test (3807 x 10) for the test set
### Summary of error in the test set Test.Err (4 x 4) and summary
### for the donor-timepoint hold outs DT.model.summary (4 x 4)

### Load the models ###

### These packages are for the MSE and MAE functions
library(MLmetrics)
library(Metrics)

### Set directories
workDir <- "<redacted>"
pubDir <- "<redacted>"

### Load the test and training sets separately
load(paste(workDir, "<redacted>", sep="")) ### dat
load(paste(workDir, "<redacted>", sep="")) ###dat.test

### Make a dataframe to hold the predictions
Test <- data.frame(matrix(nrow=dim(dat.test)[1], ncol=10))
Train <- data.frame(matrix(nrow=dim(dat)[1], ncol=10))
colnames(Test) <- colnames(Train) <- 
  c("Time.Days", "LnTime", 
  "LASSO.Pred.Days", "LASSO.Pred", 
  "RIDGE.Pred.Days", "RIDGE.Pred",
  "GBM.Pred.Days", "GBM.Pred",
  "GLMM.Pred.Days", "GLMM.Pred")

### Put the raw data into the tables
Test$Time.Days <- dat.test$Time_days
Test$LnTime <- dat.test$LnTime
Train$Time.Days <- dat$Time_days
Train$LnTime <- dat$LnTime

### Make a dataframe to hold the errors
Test.Err <- data.frame(matrix(nrow=4, ncol=4))
Train.Err <- data.frame(matrix(nrow=4, ncol=4))
colnames(Test.Err) <- colnames(Train.Err) <- c("MSE", "MSE.Days", "MAE", "MAE.Days")
rownames(Test.Err) <- rownames(Train.Err) <- c("LASSO", "RIDGE", "GBM", "GLMM")

### Probably want to load the libraries as you load the data and perform the predictions

########### Elastic Net ##########
library(glmnet)
load(paste(workDir, "<redacted>", sep="")) # lasso.fit
load(paste(workDir, "<redacted>", sep="")) # ridge.fit
load(paste(workDir, "<redacted>", sep="")) # DTmodel.LASSO
load(paste(workDir, "<redacted>", sep="")) # DTmodel.Ridge
load(paste(workDir, "<redacted>", sep="")) # DTmodel.LASSO.pred
load(paste(workDir, "<redacted>", sep="")) # DTmodel.Ridge.pred
### The loop put weird columns names on these, reset them
colnames(DTmodel.LASSO.pred) <- colnames(DTmodel.Ridge.pred) <-
  c("Samples", "Time_Days", "LnTime", "SampleByTime", "preds", "preds.days")

### To see the lambda values and number of non-zero betas, just print the model objects
lasso.fit
ridge.fit

### Set up the data for prediction
X.train <-  as.matrix(dat[, -which(colnames(dat) %in% c("Time_days", "Group", "Samples", "LnTime", "SamplesByTime"))])
X.test <- as.matrix(dat.test[, -which(colnames(dat.test) %in% c("Time_days", "Group", "Samples", "LnTime","SamplesByTime"))])

### Training Predictions
Train$LASSO.Pred <- predict(lasso.fit, X.train, s="lambda.min")[,1]
Train$LASSO.Pred.Days <- exp(Train$LASSO.Pred)
Train$RIDGE.Pred <- predict(ridge.fit, X.train, s="lambda.min")[,1]
Train$RIDGE.Pred.Days<- exp(Train$RIDGE.Pred)
### Test Predictions
Test$LASSO.Pred <- predict(lasso.fit, X.test, s="lambda.min")[,1]
Test$LASSO.Pred.Days <- exp(Test$LASSO.Pred)
Test$RIDGE.Pred <- predict(ridge.fit, X.test, s="lambda.min")[,1]
Test$RIDGE.Pred.Days<- exp(Test$RIDGE.Pred)
### LASSO ###
### Training Errors (LASSO = Col 1)
Train.Err$MSE[1] <- MSE(Train$LASSO.Pred, dat$LnTime)
Train.Err$MSE.Days[1] <- MSE(Train$LASSO.Pred.Days, dat$Time_days)
Train.Err$MAE[1] <- MAE(Train$LASSO.Pred, dat$LnTime)
Train.Err$MAE.Days[1] <- MAE(Train$LASSO.Pred.Days, dat$Time_days)
### Test Errors (LASSO = Col 1)
Test.Err$MSE[1] <- MSE(Test$LASSO.Pred, dat.test$LnTime)
Test.Err$MSE.Days[1] <- MSE(Test$LASSO.Pred.Days, dat.test$Time_days)
Test.Err$MAE[1] <- MAE(Test$LASSO.Pred, dat.test$LnTime)
Test.Err$MAE.Days[1] <- MAE(Test$LASSO.Pred.Days, dat.test$Time_days)
### RIDGE ###
### Training Errors (RIDGE = Col 2)
Train.Err$MSE[2] <- MSE(Train$RIDGE.Pred, dat$LnTime)
Train.Err$MSE.Days[2] <- MSE(Train$RIDGE.Pred.Days, dat$Time_days)
Train.Err$MAE[2] <- MAE(Train$RIDGE.Pred, dat$LnTime)
Train.Err$MAE.Days[2] <- MAE(Train$RIDGE.Pred.Days, dat$Time_days)
### Test Errors (RIDGE = Col 2)
Test.Err$MSE[2] <- MSE(Test$RIDGE.Pred, dat.test$LnTime)
Test.Err$MSE.Days[2] <- MSE(Test$RIDGE.Pred.Days, dat.test$Time_days)
Test.Err$MAE[2] <- MAE(Test$RIDGE.Pred, dat.test$LnTime)
Test.Err$MAE.Days[2] <- MAE(Test$RIDGE.Pred.Days, dat.test$Time_days)

### Rename the donor/timepoint colnames for each model so they can be merged
colnames(DTmodel.LASSO)[6:9] <- paste("LASSO.", colnames(DTmodel.LASSO)[6:9], sep="")
colnames(DTmodel.Ridge)[6:9] <- paste("Ridge.", colnames(DTmodel.Ridge)[6:9], sep="")
### Merge these together
all.equal(DTmodel.LASSO$HoldOutSample, DTmodel.Ridge$HoldOutSample)
DT.model <- cbind(DTmodel.LASSO, DTmodel.Ridge[,6:9])

########### GBM ##########
### Load the gbm library
library(gbm) # 2.1.8
load(paste(workDir, "<redacted>", sep="")) # gbm.fit
load(paste(workDir, "<redacted>", sep="")) # DTmodel.GBM
load(paste(workDir, "<redacted>", sep="")) # DTmodel.GBM.pred

### Set up the data for prediction
X.train.df <- dat[, -which(colnames(dat) %in% c("Time_days", "Group", "Samples", "SamplesByTime", "LnTime"))]
X.test.df <- dat.test[, -which(colnames(dat.test) %in% c("Time_days", "Group", "Samples", "SamplesByTime", "LnTime"))]

### Training Predictions
Train$GBM.Pred <- predict(gbm.fit, X.train.df)
# Train$GBM.Pred <- predict(gbm.fit, X.train.df, n.trees = which.min(gbm.fit$cv.error))
Train$GBM.Pred.Days <- exp(Train$GBM.Pred)
### Test Predictions
Test$GBM.Pred <- predict(gbm.fit, X.test.df)
# Test$GBM.Pred <- predict(gbm.fit, X.test.df, n.trees = which.min(gbm.fit$cv.error))
Test$GBM.Pred.Days <- exp(Test$GBM.Pred)
### Training Errors (GBM = Col 3)
Train.Err$MSE[3] <- MSE(Train$GBM.Pred, dat$LnTime)
Train.Err$MSE.Days[3] <- MSE(Train$GBM.Pred.Days, dat$Time_days)
Train.Err$MAE[3] <- MAE(Train$GBM.Pred, dat$LnTime)
Train.Err$MAE.Days[3] <- MAE(Train$GBM.Pred.Days, dat$Time_days)
### Test Errors (GBM = Col 3)
Test.Err$MSE[3] <- MSE(Test$GBM.Pred, dat.test$LnTime)
Test.Err$MSE.Days[3] <- MSE(Test$GBM.Pred.Days, dat.test$Time_days)
Test.Err$MAE[3] <- MAE(Test$GBM.Pred, dat.test$LnTime)
Test.Err$MAE.Days[3] <- MAE(Test$GBM.Pred.Days, dat.test$Time_days)

### Rename the donor/timepoint colnames for each model so they can be merged
colnames(DTmodel.GBM)[6:9] <- paste("GBM.", colnames(DTmodel.GBM)[6:9], sep="")
### Merge these together
all.equal(DT.model$HoldOutSample, DTmodel.GBM$HoldOutSample)
DT.model <- cbind(DT.model, DTmodel.GBM[,6:9])

########### GLMM ##########
### Load the glmmLasso library
library(glmmLasso)
load(paste(workDir, "<redacted>", sep="")) # glmm.fit
load(paste(workDir, "<redacted>", sep="")) # DTmodel.GLMM
load(paste(workDir, "<redacted>", sep="")) # DTmodel.GLMM.pred

### Look at the model object for number of retained variables
length(coef(glmm.fit)) # 98
sum(coef(glmm.fit) == 0) #3

### Training Predictions
Train$GLMM.Pred <- predict(glmm.fit, dat)
Train$GLMM.Pred.Days <- exp(Train$GLMM.Pred)
### Test Predictions
Test$GLMM.Pred <- predict(glmm.fit, dat.test)
Test$GLMM.Pred.Days <- exp(Test$GLMM.Pred)
### Training Errors (GLMM = Col 4)
Train.Err$MSE[4] <- MSE(Train$GLMM.Pred, dat$LnTime)
Train.Err$MSE.Days[4] <- MSE(Train$GLMM.Pred.Days, dat$Time_days)
Train.Err$MAE[4] <- MAE(Train$GLMM.Pred, dat$LnTime)
Train.Err$MAE.Days[4] <- MAE(Train$GLMM.Pred.Days, dat$Time_days)
### Test Errors (GLMM = Col 4)
Test.Err$MSE[4] <- MSE(Test$GLMM.Pred, dat.test$LnTime)
Test.Err$MSE.Days[4] <- MSE(Test$GLMM.Pred.Days, dat.test$Time_days)
Test.Err$MAE[4] <- MAE(Test$GLMM.Pred, dat.test$LnTime)
Test.Err$MAE.Days[4] <- MAE(Test$GLMM.Pred.Days, dat.test$Time_days)

### Rename the donor/timepoint colnames for each model so they can be merged
colnames(DTmodel.GLMM)[6:9] <- paste("GLMM.", colnames(DTmodel.GLMM)[6:9], sep="")
### Merge these together
all.equal(DT.model$HoldOutSample, DTmodel.GLMM$HoldOutSample)
DT.model <- cbind(DT.model, DTmodel.GLMM[,6:9])

### Output the test set predictions ###
save(Test, file=paste(pubDir, "<redacted>", sep=""))
################################################################################
### Summarize the donor/timepoint hold-out set predictions
DT.model.summary <- data.frame(matrix(nrow=4, ncol=4))
colnames(DT.model.summary) <- c("MSE", "MSE.Days", "MAE", "MAE.Days")
rownames(DT.model.summary) <- c("LASSO", "RIDGE", "GBM", "GLMM")

dt.mse <- DT.model[, c(6,10,14,18)]
dt.mse.days <- DT.model[, c(7,11,15,19)]
dt.mae <- DT.model[, c(8,12,16,20)]
dt.mae.days <- DT.model[, c(9,13,17,21)]

DT.model.summary$MSE <- apply(dt.mse, 2, mean)
DT.model.summary$MSE.Days <- apply(dt.mse.days, 2, mean)
DT.model.summary$MAE <- apply(dt.mae, 2, mean)
DT.model.summary$MAE.Days <- apply(dt.mae.days, 2, mean)

### Output some of these for the publication
Test.Err[, c(1,3)] <- round(Test.Err[, c(1,3)], 2)
Test.Err[, c(2,4)] <- round(Test.Err[, c(2,4)], 1)
write.csv(Test.Err, file=paste(pubDir, "TestError_vBetter.csv", sep=""))

Train.Err[, c(1,3)] <- round(Train.Err[, c(1,3)], 2)
Train.Err[, c(2,4)] <- round(Train.Err[, c(2,4)], 1)
write.csv(Train.Err, file=paste(pubDir, "TrainError_vBetter.csv", sep=""))

DT.model.summary[, c(1,3)] <- round(DT.model.summary[, c(1,3)], 2)
DT.model.summary[, c(2,4)] <- round(DT.model.summary[, c(2,4)], 1)
write.csv(DT.model.summary, file=paste(pubDir, "<redacted>", sep=""))
################################################################################
### Categorize the predictions from the hold-out Test set into bins

m.names <- c("LASSO", "RIDGE", "GBM", "GLMM")
n.models <- length(m.names)
n.names <- c("True", "Pred")
c.names <- c("under7", "under30", "under60", "under90", "under120", "under180")
Test.names <- paste(rep(n.names, each = length(c.names)), c.names, sep = ".")

aa<- data.frame(matrix(nrow=dim(dat.test)[1], ncol=(2 + length(Test.names))))
colnames(aa) <- c("Time.days", "Pred.days", Test.names)
aa$Time.days <- Test$Time.Days

day.cat <- c(7, 30, 60, 90, 120, 180)
n.days <- length(day.cat)

### Run the loop for the hold-out Test set and output 4 separate dataframes
for (ii in 1:n.models){
  bb <- aa
  bb$Pred.days <- ceiling(Test[, grep(paste(m.names[ii], ".Pred.Days", sep=""), colnames(Test))])
  
  for (jj in 1:n.days){
    true.col <- grep(paste("True.under", day.cat[jj], sep=""), colnames(bb))
    pred.col <- grep(paste("Pred.under", day.cat[jj], sep=""), colnames(bb))
    bb[, true.col] <- ifelse(bb$Time.days <= day.cat[jj], 1, 0)
    bb[, pred.col] <- ifelse(bb$Pred.days <= day.cat[jj], 1, 0)
    
  }
  eval(parse(text = paste("Test.Cat.", m.names[ii], " <- bb", sep="")))
  
}

### Run the loop for the donor-timepoint holdouts and output 4 separate dataframes
m.names <- c("LASSO", "Ridge", "GBM", "GLMM")
n.models <- length(m.names)
for (ii in 1:n.models){
  eval(parse(text = paste("bb <- DTmodel.", m.names[ii], ".pred", sep="")))
  bb <- bb[, -which(colnames(bb) %in% c("LnTime", "preds"))]
  colnames(bb) <- c("Samples", "Time.Days", "SamplesByTime", "Pred.Days")
  bb$Pred.Days <- ceiling(bb$Pred.Days)
  
  for (jj in 1:n.days){
    eval(parse(text = paste("bb$True.", c.names[jj], " <- ifelse(bb$Time.Days <=", day.cat[jj], ", 1, 0)", sep="")))
    eval(parse(text = paste("bb$Pred.", c.names[jj], " <- ifelse(bb$Pred.Days <=", day.cat[jj], ", 1, 0)", sep="")))
  }
  
  eval(parse(text = paste("DT.Cat.", m.names[ii], " <- bb", sep="")))
}


### Calculate overall accuracy of the binary classifiers, both for the hold-out Test set
### and for the donor-timepoint sets within the Training set
Cat.Accuracy <- data.frame(matrix(nrow=n.days, ncol=n.models))
rownames(Cat.Accuracy) <- c.names
colnames(Cat.Accuracy) <- m.names

### These didn't end up being the same
test.names <- c("LASSO", "RIDGE", "GBM", "GLMM")
dt.names <- c("LASSO", "Ridge", "GBM", "GLMM")

### Run for the Test set
for (ii in 1:n.models){
  eval(parse(text = paste("aa <- Test.Cat.", test.names[ii], sep="")))
  for (jj in 1:n.days){
    eval(parse(text = paste("preds <- aa$True.", c.names[jj], sep="")))
    eval(parse(text = paste("trues <- aa$Pred.", c.names[jj], sep="")))
    Cat.Accuracy[jj,ii] <- sum(preds == trues)
  }
}
Cat.Accuracy.Test <- Cat.Accuracy

### Run for the DT set
for (ii in 1:n.models){
  eval(parse(text = paste("aa <- DT.Cat.", dt.names[ii], sep="")))
  for (jj in 1:n.days){
    eval(parse(text = paste("preds <- aa$True.", c.names[jj], sep="")))
    eval(parse(text = paste("trues <- aa$Pred.", c.names[jj], sep="")))
    Cat.Accuracy[jj,ii] <- sum(preds == trues)
  }
}
Cat.Accuracy.DT <- Cat.Accuracy

### Create the same tables but with proportions
n.Test <- dim(Test)[1]
n.DT <- dim(DTmodel.GBM.pred)[1]

Cat.Accuracy.Test.Prop <- round(Cat.Accuracy.Test/n.Test,3)
Cat.Accuracy.DT.Prop <- round(Cat.Accuracy.DT/n.DT,3)

### COuld be helpful to add overall row counts
Cat.Accuracy.Test.Prop$Total <-c(sum(Test.Cat.GBM$True.under7),
                                 sum(Test.Cat.GBM$True.under30),
                                 sum(Test.Cat.GBM$True.under60),
                                 sum(Test.Cat.GBM$True.under90),
                                 sum(Test.Cat.GBM$True.under120),
                                 sum(Test.Cat.GBM$True.under180))

Cat.Accuracy.DT.Prop$Total <-c(sum(DT.Cat.GBM$True.under7),
                                 sum(DT.Cat.GBM$True.under30),
                                 sum(DT.Cat.GBM$True.under60),
                                 sum(DT.Cat.GBM$True.under90),
                                 sum(DT.Cat.GBM$True.under120),
                                 sum(DT.Cat.GBM$True.under180))

write.csv(Cat.Accuracy.Test.Prop, file=paste(pubDir, "<redacted>", sep=""))
write.csv(Cat.Accuracy.DT.Prop, file=paste(pubDir, "<redacted>", sep=""))

### Re-calculate the proportions of properly classfied by donor/timpoint hold-out
### Just do this for the GLMM and GBM models
### By Donor ###
m.names <- c("GBM", "GLMM")
n.models <- length(m.names)

n.donors <- length(unique(dat$Samples))
Samples <- unique(dat$Samples)

Cat.Accuracy <- data.frame(matrix(nrow=n.donors, ncol=(1 + n.days)))
rownames(Cat.Accuracy) <- Samples
colnames(Cat.Accuracy) <- c(c.names, "Total")

for (ii in 1:n.models){
  eval(parse(text = paste("aa <- DT.Cat.", dt.names[ii], sep="")))
  for (kk in 1:n.donors){
    for (jj in 1:n.days){
      eval(parse(text = paste("preds <- aa$True.", c.names[jj],"[aa$Samples %in% '", Samples[kk], "']", sep="")))
      eval(parse(text = paste("trues <- aa$Pred.", c.names[jj],"[aa$Samples %in% '", Samples[kk], "']", sep="")))
      Cat.Accuracy[kk,jj] <- sum(preds == trues)
    }
  eval(parse(text = paste("Cat.Accuracy$Total[kk] <- sum(DT.Cat.", dt.names[ii],"$Samples %in% Samples[kk])", sep="")))
  }
  eval(parse(text = paste("Cat.Accuracy.DTbyD.", m.names[ii], " <- Cat.Accuracy", sep="" )))
}
### Put in alphabetical order, for kicks
Cat.Accuracy.DTbyD.GBM <- Cat.Accuracy.DTbyD.GBM[order(rownames(Cat.Accuracy.DTbyD.GBM)),]
Cat.Accuracy.DTbyD.GLMM <- Cat.Accuracy.DTbyD.GLMM[order(rownames(Cat.Accuracy.DTbyD.GLMM)),]
write.csv(Cat.Accuracy.DTbyD.GBM, file=paste(pubDir, "<redacted>", sep=""))
write.csv(Cat.Accuracy.DTbyD.GLMM, file=paste(pubDir, "<redacted>", sep=""))
### Convert counts to proportions
Cat.Accuracy.DTbyD.GBM[, 1:6] <- round(Cat.Accuracy.DTbyD.GBM[, 1:6]/Cat.Accuracy.DTbyD.GBM[,7], 2)
Cat.Accuracy.DTbyD.GLMM[, 1:6] <- round(Cat.Accuracy.DTbyD.GLMM[, 1:6]/Cat.Accuracy.DTbyD.GLMM[,7], 2)
write.csv(Cat.Accuracy.DTbyD.GBM, file=paste(pubDir, "<redacted>", sep=""))
write.csv(Cat.Accuracy.DTbyD.GLMM, file=paste(pubDir, "<redacted>", sep=""))

### By Donor/Timepoint)
n.dt <- length(unique(dat$SamplesByTime))
dts <- unique(dat$SamplesByTime)

Cat.Accuracy <- data.frame(matrix(nrow=n.dt, ncol=(1 + n.days)))
rownames(Cat.Accuracy) <- dts
colnames(Cat.Accuracy) <- c(c.names, "Total")

for (ii in 1:n.models){
  eval(parse(text = paste("aa <- DT.Cat.", dt.names[ii], sep="")))
  for (kk in 1:n.dt){
    for (jj in 1:n.days){
      eval(parse(text = paste("preds <- aa$True.", c.names[jj],"[aa$SamplesByTime %in% '", dts[kk], "']", sep="")))
      eval(parse(text = paste("trues <- aa$Pred.", c.names[jj],"[aa$SamplesByTime %in% '", dts[kk], "']", sep="")))
      Cat.Accuracy[kk,jj] <- sum(preds == trues)
    }
    eval(parse(text = paste("Cat.Accuracy$Total[kk] <- sum(DT.Cat.", dt.names[ii],"$SamplesByTime %in% dts[kk])", sep="")))
  }
  eval(parse(text = paste("Cat.Accuracy.DTbyDT.", m.names[ii], " <- Cat.Accuracy", sep="" )))
}
### Put in alphabetical order, for kicks
Cat.Accuracy.DTbyDT.GBM <- Cat.Accuracy.DTbyDT.GBM[order(rownames(Cat.Accuracy.DTbyDT.GBM)),]
Cat.Accuracy.DTbyDT.GLMM <- Cat.Accuracy.DTbyDT.GLMM[order(rownames(Cat.Accuracy.DTbyDT.GLMM)),]
write.csv(Cat.Accuracy.DTbyDT.GBM, file=paste(pubDir, "<redacted>", sep=""))
write.csv(Cat.Accuracy.DTbyDT.GLMM, file=paste(pubDir, "<redacted>", sep=""))
### Convert counts to proportions
Cat.Accuracy.DTbyDT.GBM[, 1:6] <- round(Cat.Accuracy.DTbyDT.GBM[, 1:6]/Cat.Accuracy.DTbyDT.GBM[,7], 2)
Cat.Accuracy.DTbyDT.GLMM[, 1:6] <- round(Cat.Accuracy.DTbyDT.GLMM[, 1:6]/Cat.Accuracy.DTbyDT.GLMM[,7], 2)
write.csv(Cat.Accuracy.DTbyDT.GBM, file=paste(pubDir, "<redacted>", sep=""))
write.csv(Cat.Accuracy.DTbyDT.GLMM, file=paste(pubDir, "<redacted>", sep=""))

