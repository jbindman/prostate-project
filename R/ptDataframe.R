rm(list=ls()) #clears
#TEST





#######################

demo_data<-read.csv("julia-demo-data.csv")
psa_data<-read.csv("julia-psa-data.csv")
bx_data<-read.csv("julia-bx-data.csv")
tx_data<-read.csv("julia-tx-data.csv")

threeDataframes <- functions(tx_data, demo_data, psa_data, bx_data) #overrides, passes through specified files







