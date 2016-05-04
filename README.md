# prostate-project
A single package synthesizing the work of Yates Coley

1. Install packages

install.packages("devtools")
devtools::install_github("jbindman/prostate-project") 

2. Load in data from csv files for tx.data, demo.data, psa.data, bx.data.To combine these files into a more useful set of dataframes, call fillPatientTables on the four files. The function will return three filled dataframes (pt.data, psa.data, bx.full) to the workflow in list called patientDataframes. 

# demographic data. one record per patient
demo_data<-read.csv("julia-demo-data.csv")
# psa data. one record per PSA test per patient
psa_data<-read.csv("julia-psa-data.csv")
# bx.data. one record per biopsy per patient
bx_data<-read.csv("julia-bx-data.csv")
# tx.data. one record per treatment received per patient
tx_data<-read.csv("julia-tx-data.csv")
# .txt file that defines the model
model.file <- "UNADJ-jags-model.txt" #customize here
#ProstatePackage:::writeJAGSmodel(model.file)

# Organize data frames from clinical patient sources
patientDataframes <- ProstatePackage:::fillPatientTables(tx.data = tx_data, demo.data = demo_data, psa.data = psa_data, bx.data = bx_data)


3. Prepare data and arguments for RJAGS by calling RJAGS on the formatted list patientDataframes and the specific file name for the required accompanying RJAGS txt file.

# Return RJAGS argument prep on formatted patient dataframes
jagsPrep <- ProstatePackage:::RJAGSprep(patientDataframes, model.file)

4. Execute RJAGS 
