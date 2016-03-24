# prostate-project
A single package synthesizing the work of Yates Coley

1. Install package

    install.packages("devtools")
    devtools::install_github("jbindman/prostate-project") --> why doesn't this work?

2. Load in data by calling function fillPatientTables() and passing through csv files for tx.data, demo.data, psa.data, bx.data. Will return three filled dataframes (pt.data, psa.data, bx.full) to the workspace
   
    setwd(“/path”)
    source("R/fillPatientTables.R") 
    highlight and run the four lines to load in the default files.
    (optional) customize column names
    call fillPatientTables()
    three data frames will be returned in list and saved to workspace


    
