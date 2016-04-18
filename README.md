# prostate-project
A single package synthesizing the work of Yates Coley

1. Install packages

install.packages("devtools")
devtools::install_github("jbindman/prostate-project") #install package


2. # Install other required packages

install.packages("lme4", repos="http://cran.rstudio.com/")
install.packages("splines", repos="http://cran.rstudio.com/") #not available R 3.2.3., supress warning
install.packages("bayesm", repos="http://cran.rstudio.com/")
install.packages("Matrix", repos="http://cran.rstudio.com/")
#packages accessed as needed through library calls

3. Load in data by calling function fillPatientTables() in workflow.R and passing through custom csv files for tx.data, demo.data, psa.data, bx.data. Will return three filled dataframes (pt.data, psa.data, bx.full) to the workflow in list called patientDataframes. 
   
4. Prepare data and arguments for RJAGS by calling RJAGS on the formatted list patientDataframes and the specific file name for the required accompanying RJAGS txt file.

5. Execute RJAGS 
