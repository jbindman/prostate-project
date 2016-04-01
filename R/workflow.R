#rm(list=ls()) #clears

#these four packages need to be installed with dependencies
install.packages("lme4", repos="http://cran.rstudio.com/")
install.packages("splines", repos="http://cran.rstudio.com/") #not available R 3.2.3., supress warning
install.packages("bayesm", repos="http://cran.rstudio.com/")
install.packages("Matrix", repos="http://cran.rstudio.com/") #necessary?

#setwd("Desktop/JHU/Prostate/prostate-project") #change to your working directory
source("R/fillPatientTables.R")
source("R/RJAGSprep.R")


#######################

demo_data<-read.csv("julia-demo-data.csv")
psa_data<-read.csv("julia-psa-data.csv")
bx_data<-read.csv("julia-bx-data.csv")
tx_data<-read.csv("julia-tx-data.csv")

all <- fillPatientTables(tx_data, demo_data, psa_data, bx_data) #builds three data frames
RJAGSreturn <- RJAGSprep(all)






