rm(list=ls()) #clears
#TEST

#setwd("Desktop/JHU/Prostate/prostate-project") #change to your working directory
source("R/fillPatientTables.R")



#######################

demo_data<-read.csv("julia-demo-data.csv")
colnames(demo_data) <- c("X", "id", "dob") #customize (right now default)

psa_data<-read.csv("julia-psa-data.csv")
#colnames(psa_data) <- c("X", "id", "psa", "psa.date") #customizable

bx_data<-read.csv("julia-bx-data.csv")
#colnames(bx_data) <- c("X", "id", "bx.date", "RC", "vol", "dx") #customizable

tx_data<-read.csv("julia-tx-data.csv")
#colnames(tx_data) <- c("X", "id", "GS", "tx.date") #customizable

#all <- fillPatientTables(tx_data, demo_data, psa_data, bx_data) #builds three data frames







