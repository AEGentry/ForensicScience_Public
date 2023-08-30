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

### Load the glmmLasso library
library(glmmLasso)

### Make a fixed effects-only version
fix.col.names <- colnames(dat)[-which(colnames(dat) %in% 
                                        c("Time_days", "Samples", "Time_Days", "Group", "SamplesByTime", "LnTime"))]
fix.cols <- paste(fix.col.names, collapse=" + ")

###################### Step 1 ##################################################
### Use CV to find the appropriate lambda
### Set-up the glmmLasso model

### Things to know:
##### You can't use the . to pass the whole dataframe to the model statement
##### If you don't use random effects, you need "rnd = NULL" or you'll get an error
##### Make the ID variable a factor
dat$Samples <- as.factor(dat$Samples)

### There's not automated CV in this function, so you'll need to write your own loop
folds <- 10
cut.begin <- seq(1, dim(dat)[1], by=floor(dim(dat)[1]/folds))
cut.end <- cut.begin - 1
cut.begin <- cut.begin[1:(folds)]
cut.end <- cut.end[2:(folds + 1)]
cut.end[folds] <- dim(dat)[1]

### Randomize the ordering of the data
set.seed(1129)
dat <- dat[sample(1:nrow(dat)), ]
### Make a vector of lambda values
lambda <- c(seq(from=5, to=100, by=5))
nlam <- length(lambda)
### Create a matrix to store the results
CVlam <- data.frame(matrix(nrow=folds, ncol=nlam))

for (jj in 1:nlam){
  cat("Begining lamda value ", jj, " out of ", nlam, "\n")
  for (ii in 1:folds){
    dat.hold <- dat[cut.begin[ii] : cut.end[ii],]
    cat("Fold ", ii, " out of ", folds, "\n")
    cat("Dim of hold-out data is ", dim(dat.hold), "\n")
    dat.train <- dat[-seq(from=cut.begin[ii], to=cut.end[ii], by=1),]
    cat("Dim of training data is ", dim(dat.train), "\n")
    
    ### Run the model
    eval(parse(text = paste("mLas <- glmmLasso(LnTime ~ ", fix.cols, ", 
                        rnd = list(Samples=~1), 
                        lambda = ", lambda[jj], ", 
                        data=dat.train, 
                        final.re=TRUE)", sep="")))
    
    preds <- predict(mLas, dat.hold)
    MSE <- (1/length(preds)) * sum((dat.hold$LnTime - preds)^2)
    CVlam[ii,jj] <- MSE
    
  }
}

colnames(CVlam) <- as.character(lambda)
save(CVlam, file=paste(workDir, "<redacted>", sep=""))
