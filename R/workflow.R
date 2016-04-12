#rm(list=ls()) #clears

# 0. Install Packages with Dependencies
install.packages("devtools")
devtools::install_github("jbindman/prostate-project") #install package
install.packages("lme4", repos="http://cran.rstudio.com/")
install.packages("splines", repos="http://cran.rstudio.com/") #not available R 3.2.3., supress warning
install.packages("bayesm", repos="http://cran.rstudio.com/")
install.packages("Matrix", repos="http://cran.rstudio.com/") #double colon syntax?
install.packages("rjags")

#setwd("~/Desktop/JHU/Prostate/prostate-project") #change to your working directory
source("R/fillPatientTables.R")
source("R/RJAGSprep.R")


#######################
# #1 Customize File Names

demo_data<-read.csv("julia-demo-data.csv")
psa_data<-read.csv("julia-psa-data.csv")
bx_data<-read.csv("julia-bx-data.csv")
tx_data<-read.csv("julia-tx-data.csv")
model.file <- "UNADJ-jags-model.txt"



# #2 Prep for RJAGS
patientDataframes <- fillPatientTables(tx_data, demo_data, psa_data, bx_data)
RJAGSprepfull <- RJAGSprep(patientDataframes, model.file) #returns list of arguments for running RJAGS


# #3 Execute RJAGS (call from this main workflow? Within function?)

jags_data <- RJAGSprepfull [[1]]
inits <- RJAGSprepfull [[2]]
parameters.to.save <- RJAGSprepfull [[3]]
model.file <- RJAGSprepfull [[4]] #already defined?
n.iter <- 50000; n.burnin <- 25000; n.thin <- 20; n.chains <- 1


# #4 Execute RJAGS (call from this main workflow? Within function?)

#library(rjags)
#ex.jags<-jags(data=jags_data, inits=inits, parameters.to.save=parameters.to.save, model.file="UNADJ-jags-model.txt", n.chains=1, n.iter=50, n.burnin=10, n.thin=5)
#ex.out<-ex.jags$BUGSoutput
#str(ex.out$sims.list)


