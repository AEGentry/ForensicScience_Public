# ForensicScience_Public

Public repository for scripts used in analyses published in:
Preliminary assessment of three quantitative approaches for estimating 
time-since-deposition from autofluorescence and morphological profiles 
of cell populations from forensic biological samples
By Amanda Elswick Gentry, Sarah Ingram, M. Katherine Philpott, Kellie J. Archer,
and Christopher J. Ehrhardt

Currently available from bioRxiv:
https://www.biorxiv.org/content/10.1101/2023.04.19.537512v1

Workflow for TSD Publication Scripts

1. 1_ContactCellFeatures_ForPub.R
* Organize data for analysis

2. 2_ContactCellFeatures_ForPub_EL.R
* EL models

3a. 3a_ContactCellFeatures_ForPub_GBM_TuningStep.R
* Tuning the GBM model

3b. ContactCellFeatures_ForPub_GBM_Final.R
* GBM models

4a. 4a_ContactCellFeatures_ForPub_GLMM_Lambda.R
* Estimating lambda for GLMM

4b. 4b_ContactCellFeatures_ForPub_GLMM_Final.R
* GLMM models

5. 5_ContactCellFeatures_ForPub_Predictions.R
* Calculating predictions and performance

6. 6_ContactCellFeatures_ForPub_Plotting.R
* Plots