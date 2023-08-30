### Age of Evidence: Filtered Contact Cell Features
### Data from Chris Ehrhardt, May 2022
### Scripts by Amanda Elswick Gentry, 2022

##### Per institutional policy, path and filenames have been redacted #####

### Re-run EL, GBM, and GLMM models for final publications analyses ###

### Start by loading data filtered in ContactCellFeatures.R script
### Set directories
workDir <- "<redacted>"
pubDir <- "<redacted>"
### Data: Filtered.Contact.forML
load(file=paste(workDir, "<redacted>", sep=""))
### Keep a copy of the original data with the sample identifiers

### After the assessment of gradient RMS in the last script, choose to remove from the dataset for now
### as this measure is for calibration and is not useful for analysis
Filtered.Contact.forML <- Filtered.Contact.forML[, -grep("RMS", colnames(Filtered.Contact.forML))]
dim(Filtered.Contact.forML) # 10221   102

dat.full <- Filtered.Contact.forML
dat <- Filtered.Contact.forML

######************************************########
###### Data description
###### 97 features
###### Ln-Transformed time as the outcome
###### 15 subjects
###### 47 unique sample/timepoint observations
######************************************########

##### Assess number of donor/timpoint observations #####
########################################################
N.obs.table <- table(dat$SamplesByTime)
N.obs <- data.frame(SampleByTime=names(N.obs.table), Count=as.numeric(N.obs.table))
summary(N.obs$Count)

### Make a table for output that shows how many obs/timepoints for each donor
Data.Summary <- data.frame(matrix(nrow=length(unique(dat$SamplesByTime)), ncol=4))
colnames(Data.Summary) <- c("SamplesByTime", "Donor", "Timepoint", "Count")
Data.Summary$SamplesByTime <- unique(dat$SamplesByTime)
Data.Summary <- Data.Summary[order(Data.Summary$SamplesByTime),]
Data.Summary$Donor <- do.call("rbind", strsplit(Data.Summary$SamplesByTime, "." ,fixed=TRUE))[,1]
Data.Summary$Timepoint <- as.numeric(do.call("rbind", strsplit(Data.Summary$SamplesByTime, "." ,fixed=TRUE))[,2])
all.equal(names(N.obs.table), Data.Summary$SamplesByTime)
Data.Summary$Count <- N.obs.table
Data.Summary <- Data.Summary[, 2:4]
Data.Summary <- Data.Summary[order(Data.Summary$Donor, Data.Summary$Timepoint),]
write.csv(Data.Summary, file=paste(pubDir, "DonorTimepointCounts.csv", sep=""), row.names=FALSE)

### Given the skew of the number of observations per donor/timpoint, choose here to
### downsample to the 90% percentile IF the number of observations in the training set
### will still excede the 90th percentile after removing 
set.ceiling <- ceiling(quantile(N.obs$Count, 0.9))
set.ceiling ### 572
### Which donor/timepoints have more than that number of observations
N.obs$SampleByTime[which(N.obs$Count > set.ceiling)] ### B32.37  J72.35  J72.8   L12.152 L12.82
### For the remaining donor/timpoints, choose to hold out 20% of the data points
### UNLESS 20% of the timepoints equals less than one observation (which is obvi impossible)
### and in that case, just hold out 1 observation

##### Look at some summaries #####
########################################################
### Summarize number of observations per donor/timepoint
summary(N.obs$Count)
### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
### 3.0    36.5    78.0   217.5   202.0  2151.0
### Summarize time since deposition
summary(dat$Time_days)
### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
### 1.0     8.0    35.0    67.6   121.0   415.0

### Look at number of timepoints per donor
N.donors <- length(unique(dat$Samples))
Donors <- unique(dat$Samples)
Timepoints <- data.frame(matrix(nrow=N.donors, ncol=9))
Timepoints.Total <- matrix(nrow=N.donors, ncol=1)
colnames(Timepoints) <- as.character(seq(1:9))
rownames(Timepoints) <- rownames(Timepoints.Total) <- Donors
for (jj in 1:N.donors){
  tpts <- unique(dat$Time_days[dat$Samples %in% Donors[jj]])
  Timepoints[jj, 1:length(tpts)] <- tpts
  Timepoints.Total[jj] <- length(tpts)
}
summary(Timepoints.Total)
# Min.   :1.000  
# 1st Qu.:1.000  
# Median :2.000  
# Mean   :3.133  
# 3rd Qu.:3.500  
# Max.   :9.000  

##### Make a loop to create the hold-out test set #####
#######################################################
HoldOut.prop <- 0.2
HoldOutKey <- data.frame(matrix(nrow=dim(N.obs)[1], ncol=4))
colnames(HoldOutKey) <- c("SampleByTime", "N.Observations", "N.Hold.Out", "N.Train")

for (ii in 1:dim(N.obs)[1]){
  dt <- as.character(N.obs$SampleByTime[ii])
    HoldOutKey$SampleByTime[ii] <- dt
  dt.count <- N.obs$Count[ii]
    HoldOutKey$N.Observations[ii] <- dt.count
  cat("In loop", ii, "of", dim(N.obs)[1], "\n")
  cat("Donor/Timepoint", dt, "has", dt.count, "total observations", "\n")
  dt.indices <- which(dat$SamplesByTime %in% dt)
  
  if ((dt.count*(1-HoldOut.prop)) > set.ceiling){
    set.seed(122722 + ii)
    hold.out.indices <- sample(seq(1:dt.count), (dt.count-set.ceiling))
    hold.out.indices <- dt.indices[hold.out.indices]
    HoldOutKey$N.Hold.Out[ii] <- length(hold.out.indices)
    HoldOutKey$N.Train[ii] <- length(dt.indices) - length(hold.out.indices)
  }else{
    set.seed(122722 + ii)
    hold.out.indices <- sample(seq(1:dt.count), ceiling(HoldOut.prop * dt.count))
    hold.out.indices <- dt.indices[hold.out.indices]
    HoldOutKey$N.Hold.Out[ii] <- length(hold.out.indices)
    HoldOutKey$N.Train[ii] <- length(dt.indices) - length(hold.out.indices)
  }
  
  if (ii == 1){
    Final.Hold.Out.Indices <- hold.out.indices
  } else {
    Final.Hold.Out.Indices <- c(Final.Hold.Out.Indices, hold.out.indices)
  }
}

### See how many indices were held out:
length(Final.Hold.Out.Indices) ### 3807
### make sure they are all unique indices:
length(unique(Final.Hold.Out.Indices)) ### 3807
### It will likely be larger than 20% of the full size of the data because we had
### to downsample some of the donor/timepoints with lots of observations
length(Final.Hold.Out.Indices)/dim(dat)[1] ### 0.372

### Save the hold out key object for publication
write.csv(HoldOutKey,file=paste(workDir, "<redacted>", sep=""),
          row.names=F, quote=F)

######************************************########
###### The test set contains ~37.2% of the data
###### This includes 20% of the observations from each donor/timepoint OR
###### a single observation from each donor/timepoint, whichever is larger
###### UNLESS the number of observations in the training set exceeded the 
###### 90-percentile of observations, in which case it was trimmed to not
###### exceed that value. 3807 total observations were witheld in the test set.
######************************************########

dat.test <- dat[Final.Hold.Out.Indices, ]
dim(dat.test) ### 3807  102
dat <- dat[-Final.Hold.Out.Indices, ]
dim(dat) ### 6414  102

##### Fit the models under 2 conditions:
##### 1. Use default 10-fold cross-validation
##### 2. Within the training set, run donor/timepoint CV for sensitivity analysis
##### Probably best to run each separately for efficiency

### Save the test/train datasets separately
save(dat, file=paste(workDir, "<redacted>", sep=""))
save(dat.test, file=paste(workDir, "<redacted>", sep=""))



