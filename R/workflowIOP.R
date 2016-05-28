#' IOPworkflow.R

#rm(list=ls())
#setwd("/Users/ryc/Documents/inhealth/prediction-model/for-julia")

#This is a more complicated second pass at preparing clinical data for input to our model.
#Complexities added include: tracking when patients have surgery

##WORKFLOW
# 1. Load data, look at variables.
# 2. Start patient dataframe
# 3. Add relevant variables to PSA dataframe
# 4. Start biopsy dataframe, including biopsy and surgery decisions ###THIS IS WHERE THINGS REALLY CHANGE
# 5. Save data

##SEE new code at line...




### 1. Load data, look at variables.

#demographic data. one record per patient
demo_data<-read.csv("julia-demo-data-IOP.csv")


#psa data. one record per PSA test per patient
psa_data<-read.csv("julia-psa-data-IOP.csv")


#bx.data. one record per biopsy per patient
bx_data<-read.csv("julia-bx-data-IOP.csv")


#surg.data. one record per treatment received per patient
#this data is just for surgery
surg_data<-read.csv("julia-surg-data-IOP.csv")





### 2. Start patient dataframe
# We want the patient data to include one record per patient and true GS
# We may also put variables in this data frame to ease definition of variables in the PSA and BX data frame (date of dx, age dx, average prostate volume)
# Finally, we will order patients based on their observed GS. This is done to make estimation in JAGS easier. We assign a new sequential unique patient identifier for this ordering ("subj")

#dataCheck(surg_data, demo_data, psa_data, bx_data)
full <- fillPatientTablesIOP()



