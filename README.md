
#rm(list=ls())
#setwd("<your wd>")

# prostate-project
A single package synthesizing the work of Yates Coley

#Complexities added include: tracking when patients have surgery


1. Install package(s)

install.packages("devtools")
devtools::install_github("jbindman/prostate-project") 

2. Load in data from csv files for tx.data, demo.data, psa.data, bx.data, each of which must be saved locally. Combine these files into a more useful set of dataframes by calling fillPatientTables on the four files. The function will return three filled dataframes (pt.data, psa.data, bx.full) to the workflow in list called patientDataframes. 

#demographic data. one record per patient
demo_data<-read.csv("example-demo-data-IOP.csv")
names(demo_data)
#psa data. one record per PSA test per patient
psa_data<-read.csv("example-psa-data-IOP.csv")
names(psa_data)
#bx.data. one record per biopsy per patient
bx_data<-read.csv("example-bx-data-IOP.csv")
names(bx_data)
#surg.data. one record per treatment received per patient
surg_data<-read.csv("example-surg-data-IOP.csv")
names(surg_data)


#demo_data<-read.csv("julia-demo-data.csv")
#psa_data<-read.csv("julia-psa-data.csv")
#bx_data<-read.csv("julia-bx-data.csv")
#surg_data<-read.csv("julia-tx-data.csv")

3. Organize data frames from clinical patient sources
# We want the patient data to include one record per patient and true GS
# We may also put variables in this data frame to ease definition of variables in the PSA and BX data frame (date of dx, age dx, average prostate volume)
# Finally, we will order patients based on their observed GS. This is done to make estimation in JAGS easier. We assign a new sequential unique patient identifier for this ordering ("subj")

ptDataframes <- ProstatePackage::fillPatientTables(demo_data, psa_data, bx_data, surg_data, IOP = TRUE)


inputPatient <- ProstatePackage:::printIndividualData(198, ptDataframes)

for (i in 1:200) {
  printIndividualData(i, ptDataframes)
}


3. Prepare data and arguments for RJAGS by calling RJAGS on a required text file required for RJAGS use, as well as the formatted list patient.Dataframes.

# Return RJAGS argument prep on formatted patient dataframes
jagsPrep <- RJAGSprep(ptDataframes, TRUE)

4. Execute RJAGS 

#independently

5. Preform Distance function
closestK(inputPatient, ptDataframes)
