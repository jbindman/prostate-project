rm(list=ls()) #clears
#TEST

##WORKFLOW
# 1. Load data, look at variables.
# 2. Start patient dataframe
# 3. Add relevant variables to PSA dataframe
# 4. Start biopsy dataframe
# 5. Save data

demo.data<-read.csv("julia-demo-data.csv")
names(demo.data)
(n<-dim(demo.data)[1]) #1000 patients in this data

psa.data<-read.csv("julia-psa-data.csv")
names(psa.data)
(n_psa<-dim(psa.data)[1]) #18792 PSA tests

#bx.data. one record per biopsy per patient
bx.data<-read.csv("julia-bx-data.csv")
names(bx.data)
(n_bx<-dim(bx.data)[1]) #4134 biopsy observations
#id- unique identifier
#bx.date- date of biopsy (abbreviated bx)
#RC- binary indicator or whether grade reclassification (abbreviated RC), i.e. bx gleason 7+, occurred
#vol- volume of prostate
#dx- binary indicator of diagnostic biopsy for each patient (diagnosis abbreviated dx)

tx.data<-read.csv("julia-tx-data.csv")
names(tx.data)
(n_tx<-dim(tx.data)[1]) #203 patients received treatment
#id- unique identifier
#GS- indicator of whether post-surgery Gleason score (abbreviated GS) was 7 or higher; aka "true GS"
#tx.date- date of treatment (abbreviated tx)



#######################

#FILLING DATAFRAME NOW
pt.data<-ptDataload(tx.data, demo.data, bx.data) #overrides, passes through specified files
pt.data
psa.data
psa.data <- addPSA(pt.data, psa.data) #overrides, passes through specified files
psa.data
#biopsy.data <- biopsy(pt.data, bx.data)
#biopsy.data

#save(pt.data, psa.data, bx.full,file="data-shaping-work-space.RData")



