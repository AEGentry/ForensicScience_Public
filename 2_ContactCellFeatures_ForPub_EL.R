### Age of Evidence: Filtered Contact Cell Features
### Data from Chris Ehrhardt, May 2022
### Scripts by Amanda Elswick Gentry, 2022

### Re-run EL, GBM, and GLMM models for final publications analyses ###

### EL Model on Group ###

### Set directories
workDir <- "<redacted>"

### Load the test and training sets separately
load(paste(workDir, "<redacted>", sep="")) ### dat
load(paste(workDir, "<redacted>", sep="")) ###dat.test

### Load the glmnet library
library(glmnet)

### Parse the objects
y.orig <- dat$Time_days             ### Original response time in days
y.group <- dat$Group                ### Prob won't use this
y.samples <- dat$Samples            ### Sample identifier
y.samplesBytime <- dat$SamplesByTime
dim(dat) # 6414   102

### Need matrices of predictors for the test and train set
### Just need to remove the response
X.train <-  as.matrix(dat[, -which(colnames(dat) %in% c("Time_days", "Group", "Samples", "LnTime", "SamplesByTime"))])
X.test <- as.matrix(dat.test[, -which(colnames(dat.test) %in% c("Time_days", "Group", "Samples", "LnTime", "SamplesByTime"))])
X.test.df <- dat.test[, -which(colnames(dat) %in% c("Time_days", "Group", "Samples", "LnTime", "SamplesByTime"))]
### Set the primary training response
y.train <- dat$LnTime
### Paranoid? You know you are
all.equal(y.orig, exp(y.train))

### First, run the standard model for each and save the output
### Second, run a looped model with each donor/timepoint held out for a sensitivity analysis
#################### LASSO FIT #################################################
### (Use glmnet because it runs faster and has better plotting properties)
### alpha=1 (Lasso)
set.seed(8344)
lasso.fit <- cv.glmnet(x=X.train, y=y.train, family="gaussian", standardize=TRUE, seed=62822, nfolds=10, alpha=1)
save(lasso.fit, file=paste(workDir, "<redacted>", sep=""))
#################### RIDGE FIT #################################################
### alpha=0 (Ridge)
set.seed(8344)
ridge.fit <- cv.glmnet(x=X.train, y=y.train, family="gaussian", standardize=TRUE, seed=62822, nfolds=10, alpha=0)
save(ridge.fit, file=paste(workDir, "<redacted>", sep=""))

#################### Donor/Timepoint CV Models #################################
### * ### Originally, we didn't save the predictions from the DT models because
######### we summarized each run within the loop, but then I decided I wanted
######### the categorical predictions, so I reran and saved them ### * ####

SamplesIDs <- unique(dat$SamplesByTime)
n.Sids <- length(SamplesIDs)

### Create a matrix to hold the output
DTmodel.LASSO <- data.frame(matrix(nrow=n.Sids, ncol=9))
DTmodel.Ridge <- data.frame(matrix(nrow=n.Sids, ncol=9))
colnames(DTmodel.LASSO) <- colnames(DTmodel.Ridge) <-
  c("HoldOutSample", "TestSize", "TrainSize",
    "Time.days", "LnTime",
    "MSE", "MSE.days", 
    "MAE", "MAE.days")

### Run the model in a loop
for (ii in 1:n.Sids){
  cat("Hold out Sample/Timepoint ", ii, " out of ", n.Sids, "\n")
  ### Set the hold-out set for the donor/timepoint
  to.test <- which(dat$SamplesByTime %in% SamplesIDs[ii])
  dat.holdOut <- dat[to.test,]
  dat.train <- dat[-to.test,]
  
  X.train <-  as.matrix(dat.train[, -which(colnames(dat.train) %in% c("Samples", "Time_days", "Group", "LnTime", "SamplesByTime"))])
  X.test <- as.matrix(dat.holdOut[, -which(colnames(dat.holdOut) %in% c("Samples", "Time_days", "Group", "LnTime", "SamplesByTime"))])
  X.test.df <- dat.holdOut[, -which(colnames(dat.holdOut) %in% c("Samples", "Time_days", "Group", "LnTime", "SamplesByTime"))]
  
  ### Set the primary training response, for running/testing the model
  y.train <- dat.train$LnTime
  y.holdOut <- dat.holdOut$LnTime
  
  ### For the prediction table
  y.test <- dat.holdOut[, which(colnames(dat.holdOut) %in% c("Samples", "SamplesByTime", "Time_days", "LnTime"))]
  
  ### Save the relevant data for the iteration
  DTmodel.LASSO$HoldOutSample[ii] <- DTmodel.Ridge$HoldOutSample[ii] <- SamplesIDs[ii]
  DTmodel.LASSO$TestSize[ii] <- DTmodel.Ridge$TestSize[ii] <- length(to.test)
  DTmodel.LASSO$TrainSize[ii] <- DTmodel.Ridge$TrainSize[ii] <- dim(dat)[1] - length(to.test)
  
  ### Run the LASSO model
  set.seed(8344)
  L.fit <- cv.glmnet(x=X.train, y=y.train, family="gaussian", standardize=TRUE, seed=62822, nfolds=10, alpha=1)
  preds <- predict(L.fit, X.test, s="lambda.min")
  preds.days <- exp(preds)
  
  DTmodel.LASSO$MSE[ii] <- (1/length(preds)) * sum((dat.holdOut$LnTime - preds)^2)
  DTmodel.LASSO$MSE.days[ii] <- (1/length(preds)) * sum((dat.holdOut$Time_days - preds.days)^2)
  DTmodel.LASSO$MAE[ii] <- (1/length(preds)) * sum(abs(dat.holdOut$LnTime - preds))
  DTmodel.LASSO$MAE.days[ii] <- (1/length(preds)) * sum(abs(dat.holdOut$Time_days - preds.days))
  DTmodel.LASSO$LnTime[ii] <- dat.holdOut$LnTime[1]
  DTmodel.LASSO$Time.days[ii] <- dat.holdOut$Time_days[1]
  
  if (ii==1){
    DTmodel.LASSO.pred <- data.frame(cbind(y.test, preds, preds.days))
  } else {
    aa <- data.frame(cbind(y.test, preds, preds.days))
    DTmodel.LASSO.pred <- rbind(DTmodel.LASSO.pred, aa)
  }
  
  cat("Finished iteration", ii, "out of", n.Sids, "for the LASSO fit")
  
  ### Run the Ridge model
  set.seed(8344)
  R.fit <- cv.glmnet(x=X.train, y=y.train, family="gaussian", standardize=TRUE, seed=62822, nfolds=10, alpha=0)
  preds <- predict(R.fit, X.test, s="lambda.min")
  preds.days <- exp(preds)
  
  DTmodel.Ridge$MSE[ii] <- (1/length(preds)) * sum((dat.holdOut$LnTime - preds)^2)
  DTmodel.Ridge$MSE.days[ii] <- (1/length(preds)) * sum((dat.holdOut$Time_days - preds.days)^2)
  DTmodel.Ridge$MAE[ii] <- (1/length(preds)) * sum(abs(dat.holdOut$LnTime - preds))
  DTmodel.Ridge$MAE.days[ii] <- (1/length(preds)) * sum(abs(dat.holdOut$Time_days - preds.days))
  DTmodel.Ridge$LnTime[ii] <- dat.holdOut$LnTime[1]
  DTmodel.Ridge$Time.days[ii] <- dat.holdOut$Time_days[1]
  
  if (ii==1){
    DTmodel.Ridge.pred <- data.frame(cbind(y.test, preds, preds.days))
  } else {
    aa <- data.frame(cbind(y.test, preds, preds.days))
    DTmodel.Ridge.pred <- rbind(DTmodel.Ridge.pred, aa)
  }
  
  cat("Finished iteration", ii, "out of", n.Sids, "for the Ridge fit")
}

save(DTmodel.LASSO, file=paste(workDir, "<redacted>", sep=""))
save(DTmodel.LASSO.pred, file=paste(workDir, "<redacted>", sep=""))
save(DTmodel.Ridge, file=paste(workDir, "<redacted>", sep=""))
save(DTmodel.Ridge.pred, file=paste(workDir, "<redacted>", sep=""))

