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

#################### GBM FIT #################################################
### Create a matrix of hyperparameters to try, see tutorial here:
### http://uc-r.github.io/gbm_regression

### For tuning the model, use 10-fold cross-validation for each
### of the hyperparameter scenarios
### Per KJA, use the cross-validation to estimate that error in each
### scenario

### GBM wants a single dataframe of the outcome and the predictors
X.y.df.train <- dat[, -which(colnames(dat) %in% c("Time_days", "Group", "Samples", "SamplesByTime"))]
dim(X.y.df.train) # 6414   98

### Create the matrix of hyperparameters to try
hyper_grid <- expand.grid(
  shrinkage = c(0.001, .01, .05),
  interaction.depth = c(1, 2),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = 0.5, 
  predMSE = 0
)

### The gbm() procedure will use cv inside the function to fit the best
### number of iterations (n.trees)
### Here, we will create an "outer loop" of cv to estimate error from
### each of the hyperparameter scenarios

### Make an vector of length n of indices
indices <- seq(from=1, to=dim(X.y.df.train)[1], by=1)
n.folds <- 10
n.perFold <- floor(length(indices)/n.folds)
n.perFinalFold <- length(indices) - (n.perFold * (n.folds -1))
folds.mat <- data.frame(matrix(nrow=n.folds, ncol=3))
colnames(folds.mat) <- c("Begin", "End", "N")
folds.mat$Begin <- n.perFold * seq(0,(n.folds-1)) + 1
folds.mat$End <- (n.perFold * seq(1,n.folds))
folds.mat
folds.mat$End[n.folds] <- dim(X.y.df.train)[1]
folds.mat$N <- folds.mat$End - folds.mat$Begin + 1
folds.mat

set.seed(32123)
random.indices <- sample(indices, length(indices))

for(i in 1:nrow(hyper_grid)) {
  cat("Beginning parameter set ", i, " of ", nrow(hyper_grid), "\n")
  CV.MSE <- numeric()
  
  for (j in 1:n.folds){
    fold.indices <- random.indices[folds.mat$Begin[j]:folds.mat$End[j]]
    
    X.y.df.train.cv <- X.y.df.train[-fold.indices,]
    X.df.test.cv <- X.y.df.train[fold.indices, -which(colnames(X.y.df.train.cv) %in% "LnTime")]
    y.test.cv <- X.y.df.train$LnTime[fold.indices]
    cat("Dimensions of CV fold training set ", j, " are ", dim(X.y.df.train.cv), "\n")
    cat("Dimensions of CV fold testing set ", j, " are ", dim(X.df.test.cv), "\n")
  
  # reproducibility
  set.seed(123)

  # train model
  gbm.tune <- gbm(
    formula = LnTime ~ .,
    distribution = "gaussian",
    data = X.y.df.train.cv,
    n.trees = 10000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    cv.folds=10,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  ### extract the number of iterations from the CV model
  # gbm.tune.it <- gbm.perf(gbm.tune, plot.it=FALSE, method="cv")
  # actually can extract what you need on the fly, see below
  
  tune.pred <- predict.gbm(gbm.tune, X.df.test.cv, n.trees = which.min(gbm.tune$cv.error))
  CV.MSE[j] <- 1/length(y.test.cv) * sum ((y.test.cv - tune.pred)^2)
  cat("Finished CV fold ", j, "\n")
  
  }
  
hyper_grid$predMSE[i] <- sum(CV.MSE)/n.folds
cat("Finishing parameter set ", i, " of ", nrow(hyper_grid), "\n")

}
##################################################################################################
save(hyper_grid, file=paste(workDir, "<redacted>", sep=""))
