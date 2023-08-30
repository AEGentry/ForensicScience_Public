### Age of Evidence: Filtered Contact Cell Features
### Data from Chris Ehrhardt, May 2022
### Scripts by Amanda Elswick Gentry, 2022

### Re-run EL, GBM, and GLMM models for final publications analyses ###

### GBM Model on Group ###

### Set directories
workDir <- "<redacted>"

### Load the test and training sets separately
load(paste(workDir, "<redacted>", sep="")) ### dat
load(paste(workDir, "<redacted>", sep="")) ###dat.test

### Load the gbm library
library(gbm) # Loaded gbm 2.1.8

### Load the hyperparameter grid previously calculated in script 3a_ContactCellFeatures_ForPub_GBM_Tuning_Step.R
load(paste(workDir, "<redacted>", sep="")) # hyper_grid
### Sort the hyperparam matrix to find the parameters that minimize the MSE
hyper_grid_sorted <- hyper_grid[order(hyper_grid$predMSE),]
### Save the hypergrid output as a table for publication reporting
write.csv(hyper_grid_sorted, file=paste(workDir, "<redacted>", sep=""), quote=FALSE)

### Set the parameters accordingly
Shrinkage <- hyper_grid_sorted$shrinkage[1]
Int.Depth <- hyper_grid_sorted$interaction.depth[1]
MinObsNode <- hyper_grid_sorted$n.minobsinnode[1]
Bag.Frac <- hyper_grid_sorted$bag.fraction[1]

####################### GBM FIT ########################
### GBM wants a single dataframe of the outcome and the predictors
X.y.df <- dat[, -which(colnames(dat) %in% c("Time_days", "Group", "Samples", "SamplesByTime"))]

# reproducibility
set.seed(123)

# fit model
gbm.fit <- gbm(
  formula = LnTime ~ .,
  distribution = "gaussian",
  data = X.y.df,
  interaction.depth = Int.Depth,
  n.trees = 10000,
  shrinkage = Shrinkage,
  n.minobsinnode = MinObsNode,
  bag.fraction = Bag.Frac,
  cv.folds=10,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

save(gbm.fit, file=paste(workDir, "<redacted>", sep=""))

#################### Donor/Timepoint CV Models #################################
### * ### Originally, we didn't save the predictions from the DT models because
######### we summarized each run within the loop, but then I decided I wanted
######### the categorical predictions, so I reran and saved them ### * ####

SamplesIDs <- unique(dat$SamplesByTime)
n.Sids <- length(SamplesIDs)

### Create a matrix to hold the output
DTmodel.GBM <- data.frame(matrix(nrow=n.Sids, ncol=9))
colnames(DTmodel.GBM) <- 
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
  
  ### For running/testing the model
  X.y.train <-  dat.train[, -which(colnames(dat.train) %in% c("Samples", "Time_days", "Group", "SamplesByTime"))]
  X.y.test <- dat.holdOut[, -which(colnames(dat.holdOut) %in% c("Samples", "Time_days", "Group", "SamplesByTime"))]

  ### For the prediction table
  y.test <- dat.holdOut[, which(colnames(dat.holdOut) %in% c("Samples", "SamplesByTime", "Time_days", "LnTime"))]
  
  ### Save the relevant data for the iteration
  DTmodel.GBM$HoldOutSample[ii] <- SamplesIDs[ii]
  DTmodel.GBM$TestSize[ii] <- length(to.test)
  DTmodel.GBM$TrainSize[ii] <- dim(dat)[1] - length(to.test)
  
  ### Run the GBM model
  set.seed(8344)
  G.fit <- gbm(
    formula = LnTime ~ .,
    distribution = "gaussian",
    data = X.y.train,
    n.trees = 10000,
    interaction.depth = Int.Depth,
    shrinkage = Shrinkage,
    n.minobsinnode = MinObsNode,
    bag.fraction = Bag.Frac,
    cv.folds=10,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  preds <-predict.gbm(G.fit, X.y.test, n.trees = which.min(G.fit$cv.error))
  preds.days <- exp(preds)
  
  DTmodel.GBM$MSE[ii] <- (1/length(preds)) * sum((dat.holdOut$LnTime - preds)^2)
  DTmodel.GBM$MSE.days[ii] <- (1/length(preds)) * sum((dat.holdOut$Time_days - preds.days)^2)
  DTmodel.GBM$MAE[ii] <- (1/length(preds)) * sum(abs(dat.holdOut$LnTime - preds))
  DTmodel.GBM$MAE.days[ii] <- (1/length(preds)) * sum(abs(dat.holdOut$Time_days - preds.days))
  DTmodel.GBM$Time.days[ii] <- dat.holdOut$Time_days[1]
  DTmodel.GBM$LnTime[ii] <- dat.holdOut$LnTime[1]
  
  if (ii==1){
    DTmodel.GBM.pred <- data.frame(cbind(y.test, preds, preds.days))
  } else {
    aa <- data.frame(cbind(y.test, preds, preds.days))
    DTmodel.GBM.pred <- rbind(DTmodel.GBM.pred, aa)
  }
  
  cat("Finished iteration", ii, "out of", n.Sids, "for the GBM fit")
  
}

save(DTmodel.GBM.pred, file=paste(workDir, "<redacted>", sep=""))
save(DTmodel.GBM, file=paste(workDir, "<redacted>", sep=""))


