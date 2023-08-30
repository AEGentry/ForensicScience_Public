### Age of Evidence: Filtered Contact Cell Features
### Data from Chris Ehrhardt, May 2022
### Scripts by Amanda Elswick Gentry, 2022

### Re-run EL, GBM, and GLMM models for final publications analyses ###

### GLMM Model on Group ###

### Set directories
workDir <- "<redacted>"

### Load the test and training sets separately
load(paste(workDir, "<redacted>", sep="")) ### dat
load(paste(workDir, "<redacted>", sep="")) ###dat.test

### Load the glmmLasso library
library(glmmLasso)

### Load the cross-validation object run previously, separately
### from script 4a_ContactCellFeatures_GLMMLasso_Lambda.R
load(paste(workDir, "<redacted>", sep="")) # object CVlam
### Find the lambda value that minimizes CV-error
MSE.means <- apply(CVlam, 2, mean)
which.min(MSE.means)
minLambda <- as.numeric(colnames(CVlam)[which.min(MSE.means)]) #90

### Make a fixed effects-only version
fix.col.names <- colnames(dat)[-which(colnames(dat) %in% 
                                        c("Time_days", "Samples", "Time_Days", "Group", "SamplesByTime", "LnTime"))]
fix.cols <- paste(fix.col.names, collapse=" + ")
dat$Samples <- as.factor(dat$Samples)

### First, run the standard model for each and save the output
### Second, run a looped model with each donor/timepoint held out for a sensitivity analysis
#################### GLMM Model #########################
eval(parse(text = paste("glmm.fit <- glmmLasso(LnTime ~ ", fix.cols, ", 
                        rnd = list(Samples=~1), 
                        lambda = ", minLambda, ", 
                        data=dat, 
                        final.re=TRUE)", sep="")))
save(glmm.fit, file=paste(workDir, "<redacted>", sep=""))

#################### Donor/Timepoint CV Models #################################
### * ### Originally, we didn't save the predictions from the DT models because
######### we summarized each run within the loop, but then I decided I wanted
######### the categorical predictions, so I reran and saved them ### * ####

SamplesIDs <- unique(dat$SamplesByTime)
n.Sids <- length(SamplesIDs)

### Create a matrix to hold the output
DTmodel.GLMM <- data.frame(matrix(nrow=n.Sids, ncol=9))
colnames(DTmodel.GLMM) <- 
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
  
  ### Set the primary training response, for running/testing the model
  y.train <- dat.train$LnTime
  y.holdOut <- dat.holdOut$LnTime
  
  ### For the prediction table
  y.test <- dat.holdOut[, which(colnames(dat.holdOut) %in% c("Samples", "SamplesByTime", "Time_days", "LnTime"))]
  
  ### Save the relevant data for the iteration
  DTmodel.GLMM$HoldOutSample[ii] <- SamplesIDs[ii]
  DTmodel.GLMM$TestSize[ii] <-length(to.test)
  DTmodel.GLMM$TrainSize[ii] <- dim(dat)[1] - length(to.test)
  
  ### Run the GLMM model
  set.seed(8344)
  eval(parse(text = paste("glmm.Fit <- glmmLasso(LnTime ~ ", fix.cols, ", 
                        rnd = list(Samples=~1), 
                        lambda = ", minLambda, ", 
                        data=dat.train, 
                        final.re=TRUE)", sep="")))
  
  preds <- predict(glmm.Fit, dat.holdOut)
  preds.days <- exp(preds)
  
  DTmodel.GLMM$MSE[ii] <- (1/length(preds)) * sum((dat.holdOut$LnTime - preds)^2)
  DTmodel.GLMM$MSE.days[ii] <- (1/length(preds)) * sum((dat.holdOut$Time_days - preds.days)^2)
  DTmodel.GLMM$MAE[ii] <- (1/length(preds)) * sum(abs(dat.holdOut$LnTime - preds))
  DTmodel.GLMM$MAE.days[ii] <- (1/length(preds)) * sum(abs(dat.holdOut$Time_days - preds.days))
  DTmodel.GLMM$LnTime[ii] <- dat.holdOut$LnTime[1]
  DTmodel.GLMM$Time.days[ii] <- dat.holdOut$Time_days[1]
  
  if (ii==1){
    DTmodel.GLMM.pred <- data.frame(cbind(y.test, preds, preds.days))
  } else {
    aa <- data.frame(cbind(y.test, preds, preds.days))
    DTmodel.GLMM.pred <- rbind(DTmodel.GLMM.pred, aa)
  }
  
  cat("Finished iteration", ii, "out of", n.Sids, "for the GLMM fit")
  
}

save(DTmodel.GLMM.pred, file=paste(workDir, "<redacted>", sep=""))
save(DTmodel.GLMM, file=paste(workDir, "<redacted>", sep=""))



