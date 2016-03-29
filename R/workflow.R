#rm(list=ls()) #clears


install.packages("lme4") #dependencies figure out
install.packages("splines") #not available R 3.2.3., supress warning
install.packages("bayesm")
install.packages("Matrix") #necessary?

#setwd("Desktop/JHU/Prostate/prostate-project") #change to your working directory
source("R/fillPatientTables.R")
source("R/RJAGSprep.R")


#######################

demo_data<-read.csv("julia-demo-data.csv")
#colnames(demo_data) <- c("X", "id", "dob") #customize (right now default)

psa_data<-read.csv("julia-psa-data.csv")
#colnames(psa_data) <- c("X", "id", "psa", "psa.date") #customizable

bx_data<-read.csv("julia-bx-data.csv")
#colnames(bx_data) <- c("X", "id", "bx.date", "RC", "vol", "dx") #customizable

tx_data<-read.csv("julia-tx-data.csv")
#colnames(tx_data) <- c("X", "id", "GS", "tx.date") #customizable

all <- fillPatientTables(tx_data, demo_data, psa_data, bx_data) #builds three data frames
RJAGSreturn <- RJAGSprep(all)






