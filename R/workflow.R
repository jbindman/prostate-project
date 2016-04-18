#rm(list=ls()) #clears
#setwd("~/Desktop/JHU/Prostate/prostate-project")

# Install required packages

install.packages("lme4", repos="http://cran.rstudio.com/")
install.packages("splines", repos="http://cran.rstudio.com/") #not available R 3.2.3., supress warning
install.packages("bayesm", repos="http://cran.rstudio.com/")
install.packages("Matrix", repos="http://cran.rstudio.com/")
#packages accessed as needed through library calls




source("R/fillPatientTables.R")
source("R/RJAGSprep.R")
#dont source the files, want automatically downloaded through package use HOW DO I DO THIS

#######################
# Customize File Names

# demographic data. one record per patient
demo_data<-read.csv("julia-demo-data.csv")
# psa data. one record per PSA test per patient
psa_data<-read.csv("julia-psa-data.csv")
# bx.data. one record per biopsy per patient
bx_data<-read.csv("julia-bx-data.csv")
# tx.data. one record per treatment received per patient
tx_data<-read.csv("julia-tx-data.csv")
# .txt file that defines the model
model.file <- "UNADJ-jags-model.txt" #customize here
writeJAGSmodel(model.file)

# Organize data frames from clinical patient sources
# dataCheck (tx.data = tx_data, demo.data = demo_data, psa.data = psa_data, bx.data = bx_data)
patientDataframes <- fillPatientTables(tx.data = tx_data, demo.data = demo_data, psa.data = psa_data, bx.data = bx_data)

# Call argument prep on data frames for RJAGS function
jagsPrep <- RJAGSprep(patientDataframes, model.file) #returns list of arguments for running RJAGS



# Execute RJAGS (Test)

#library(rjags)
#inits=jagsPrep$inits # test
#ex.jags<-jags(jags_data = jagsPrep$jags_data, inits=jagsPrep$inits, parameters.to.save=jagsPrep$parameters.to.save, model.file = jagsPrep$model.file, n.chains=1, n.iter=50, n.burnin=10, n.thin=5)
#ex.out<-ex.jags$BUGSoutput
#str(ex.out$sims.list)


