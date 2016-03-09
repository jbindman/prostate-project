rm(list=ls()) #clears
#TEST

setwd("Desktop/JHU/Prostate/prostate-project") #change to your working directory
source("R/functions.R")



#######################

demo_data<-read.csv("julia-demo-data.csv")
psa_data<-read.csv("julia-psa-data.csv")
bx_data<-read.csv("julia-bx-data.csv")
tx_data<-read.csv("julia-tx-data.csv")

list <- fillPatientTables(tx_data, demo_data, psa_data, bx_data) #overrides, passes through specified files







