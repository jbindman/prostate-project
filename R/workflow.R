#rm(list=ls()) #clears
#setwd("<location of your datasets>")


# Install required packages
#install.packages("Matrix", repos="http://cran.rstudio.com/")
#install.packages("lme4", repos="http://cran.rstudio.com/")
#install.packages("splines", repos="http://cran.rstudio.com/") #not available R 3.2.3., supress warning
#install.packages("bayesm", repos="http://cran.rstudio.com/")
#install.packages("RCurl")


#packages accessed as needed through library calls


#######################
# Customize File Names

# demographic data. one record per patient
demo_data<-read.csv("julia-demo-data.csv")
# psa data. one record per PSA test per patient
psa_data<-read.csv("julia-psa-data.csv")
# bx.data. one record per biopsy per patient
bx_data<-read.csv("julia-bx-data.csv")
# tx.data. one record per treatment received per patient
tx_data<-read.csv("julia-tx-data.csv")


require(RCurl)
model.fileName <- getURL("https://raw.githubusercontent.com/jbindman/prostate-project/master/UNADJ-jags-model.txt")
model.file <- "UNADJ-jags-model.txt"
cat(model.fileName, fill=TRUE, file= model.file) #writes file named "UNADJ-jags-model.txt"

#model.file <- read.table(model.fileName, header = FALSE, fill = TRUE)


#modelFile <- (text=getURL("https://raw.githu9busercontent.com/jbindman/prostate-project/master/UNADJ-jags-model.txt"))
#x <- scan("https://raw.githubusercontent.com/jbindman/prostate-project/master/UNADJ-jags-model.txt", what = list(NULL, name = character()))


#source("R/writeRJAGSmodel.R")
#model.fileName <- "UNADJ-jags-model.txt" #file name can be changed
#ProstatePackage:::writeRJAGSmodel() #model.fileName


# Organize data frames from clinical patient sources
patientDataframes <- ProstatePackage:::fillPatientTables(tx.data = tx_data, demo.data = demo_data, psa.data = psa_data, bx.data = bx_data)

# Return RJAGS argument prep on formatted patient dataframes
jagsPrep <- ProstatePackage:::RJAGSprep(patientDataframes, model.file)


# Execute RJAGS (Test)

#library(rjags)
#ex.jags<-jags(jags_data = jagsPrep$jags_data, inits=jagsPrep$inits, parameters.to.save=jagsPrep$parameters.to.save, model.file = jagsPrep$model.file, n.chains=1, n.iter=50, n.burnin=10, n.thin=5)
#ex.out<-ex.jags$BUGSoutput
#str(ex.out$sims.list)


