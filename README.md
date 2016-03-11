# prostate-project
A single package synthesizing the work of Yates Coley


1. Load in data by calling function loadfiles() and passing through csv files for tx.data, demo.data, psa.data, bx.data and three filled dataframes (pt.data, psa.data, bx.full) will be returned to the workspace. 
   
    setwd(“/path”)
    source("R/functions.R") #specifies files
    highlight and run the four lines to load in the default files 
    call fillPatientTables()
    three data frames will be returned in list and saved to workspace



    customization: fillPatientTables(data... tx_data_names=c("ID", "DATE", data.format"XX/XX/XXXX")) and describe in the readme 
