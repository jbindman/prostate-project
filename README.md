# prostate-project
A single package synthesizing the work of Yates Coley

1. Install package

    install.packages("devtools")
    devtools::install_github("jbindman/prostate-project")  

2. Load in data by calling function fillPatientTables() in workflow.R and passing through custom csv files for tx.data, demo.data, psa.data, bx.data. Will return three filled dataframes (pt.data, psa.data, bx.full) to the workflow in list called all.
   
    
3. Prepare data and arguments for RJAGS by calling RJAGS on the formatted list all.


#supress warnings
#package with dependencies
