# prostate-project
A single package synthesizing the work of Yates Coley

1. Install package

install.packages("devtools")
devtools::install_github("jbindman/prostate-project") 

2. Load in data from csv files for tx.data, demo.data, psa.data, bx.data, each of which must be saved locally. Combine these files into a more useful set of dataframes by calling fillPatientTables on the four files. The function will return three filled dataframes (pt.data, psa.data, bx.full) to the workflow in list called patientDataframes. 

# demographic data. one record per patient
demo_data<-read.csv("julia-demo-data.csv")
# psa data. one record per PSA test per patient
psa_data<-read.csv("julia-psa-data.csv")
# bx.data. one record per biopsy per patient
bx_data<-read.csv("julia-bx-data.csv")
# tx.data. one record per treatment received per patient
tx_data<-read.csv("julia-tx-data.csv")


# Organize data frames from clinical patient sources
patientDataframes <- ProstatePackage:::fillPatientTables(tx.data = tx_data, demo.data = demo_data, psa.data = psa_data, bx.data = bx_data)


3. Prepare data and arguments for RJAGS by calling RJAGS on a required text file required for RJAGS use, as well as the formatted list patientDataframes.

# Return RJAGS argument prep on formatted patient dataframes
jagsPrep <- ProstatePackage:::RJAGSprep(patientDataframes, model.file)

4. Execute RJAGS 

#independently
