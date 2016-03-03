rm(list=ls()) #clears
#TEST

##WORKFLOW
# 1. Load data, look at variables.
# 2. Start patient dataframe
# 3. Add relevant variables to PSA dataframe
# 4. Start biopsy dataframe
# 5. Save data



#######################

demo_data<-read.csv("julia-demo-data.csv")
psa_data<-read.csv("julia-psa-data.csv")
bx_data<-read.csv("julia-bx-data.csv")
tx_data<-read.csv("julia-tx-data.csv")

threeDataframes <- loadfiles(tx_data, demo_data, psa_data, bx_data) #overrides, passes through specified files







