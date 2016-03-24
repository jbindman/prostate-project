### NOTE to Julia:
#I've realized that I have been inconsistent with whether I use underscores or period for data frame and variable names. Have you come across anything that indicates one convention is preferable to the other?

#Next 2 lines just for me
#rm(list=ls())
#setwd("/Users/ryc/Documents/inhealth/prediction-model/for-julia")



#General description of this script: Before calling JAGS (to obtain model estimates), it is necessary to get the data into simple matrices and vectors to send to the JAGS functions.
#For each clinical measurement (PSA and biopsy data), we want to define the total number of observations, a vector that contains the outcome of every observation, a matrix of predictor variables corresponding to each observation, and a vector with patient-specific identifiers ("subj") 

#### WORKFLOW
### 0. Load packageges and  necessary data. 
### 1. Format pt-level data for JAGS run 
### 2. Format PSA data for JAGS run 
### 3. Format biopsy data for JAGS run 



### 0. Load packageges and  necessary data. 

#These packages should be loaded automatically when someone loads our package
library("lme4")
library("splines")


#(You shouldn't need to load the data in your workflow because the dataframes should already be defined by your previous function.)
load('data-shaping-work-space.RData')

ls()
#bx.full, psa.data, pt.data
#Here, I am starting with bx.full, psa.data, and pt. data created by the data-shaping-code I sent your before. You may need to adjust dataframe and variable names if you defined things differently in your earlier code 







### 1. Format pt-level data for JAGS run 

(n<-dim(pt.data)[1]) #there are 1000 patients. 
#I think you have already defined this in your data shaping code, so you wouldn't need to define it again
#(The same may be true of other variables I define below. If that is the case, no need to define them again. I won't keep putting this reminder in the code every place this may happen.)

#define vector with observed cancer state for patients with surgery
eta_data<-pt.data$true.gs[!is.na(pt.data$true.gs)] 


#number of patients with true state (eta) observed
(n_eta_known<-length(eta_data))  #203

#Pt.data should already be ordered so that those with eta=0, 1 come before patients without eta observed (true.gs=NA) and subject ids 1:n_eta_known are the ids for patients with eta observed


#I also found something that should have been included in the pt.data shaping earlier. 
pt.data$rc<-rep(0,n)
for(i in 1:n){
	if(max(bx.full$rc[bx.full$subj==i], na.rm=T)==1){pt.data$rc[i]<-1}}
#Pt.data needs to be ordered by subject to run this line as is. Otherwise, try if(max(bx.full$rc[bx.full$id==pt.data$id[i]], na.rm=T)==1){pt.data$rc[i]<-1}

table(pt.data$rc) #205 patients with grade reclassification observed




### 2. Format PSA data for JAGS run 
(n_obs_psa<-dim(psa.data)[1]) #18792 #the total number of PSA observations
Y<-psa.data$log_psa #name log-PSA observatios "Y
subj_psa<-psa.data$subj #vector with subject ids that correspond to PSA obsrevations

#matrix of covariates with "random effects" (This term is familiar to stats people, but may not be familiar to you. Make sure to keep the Z_ and X_ labels here, because stats people will understand that better.)
#here, intercept and age (standardized)
psa.data$age.std<-scale(psa.data$age) #you may want to move this line to original data shaping function
Z_data<-as.matrix( cbind(rep(1,n_obs_psa), psa.data$age.std) )
(d_Z<-dim(Z_data)[2]) #should be 2


#matrix of covariates with only fixed effects
#here, prostate volume (standardized on patient-level with mean and std of volume since it is constant within patients). no intercept needed
psa.data$vol.std<-scale(psa.data$vol.avg, center=mean(pt.data$vol.avg), scale=sd(pt.data$vol.avg)) #you may want to move this line to original data shaping
X_data<-as.matrix(cbind(psa.data$vol.std))
(d_X<-dim(X_data)[2]) #should be 1


#here, I fit a linear mixed effects regression (lmer) to get a starting value for the covariance parameter. I will use this as input for the JAGS model
#lmer fit to get starting value for covariance parameter in JAGS
mod_lmer<-lmer(log.psa~ vol.std + (1+ age.std |id), data=psa.data)
(var_vec <- apply(coef(mod_lmer)$id, 2, var)[1:d_Z])
(var_vec <- c(var_vec[2], var_vec[1])) #I want the covariance parameters ordered so that the one corresponding to the intercept is first and the one corresponding to age is second. I don't know why the model output is in a different order






### 3. Format biopsy data for JAGS run 
#Here, we define the outcome and predictors for grade reclassification on biopsy
#Later, we will add two additional models here: one for biopsy received and one for surgery
rc.data<-bx.full[bx.full$bx.here==1 & !is.na(bx.full$bx.here),] 
(n_rc<-dim(rc.data)[1]) #4134
RC<-as.numeric(rc.data$rc) #indicator of reclassificaiton in this interval
subj_rc<-rc.data$subj

#covariate matrix include an intercept, natural spline representations of time and calendar date (df=2 for each), and mean- and sd- standardized age
V_RC_data<-as.matrix(cbind(rep(1,n_rc), ns(rc.data$bx.time, 2), ns(rc.data$bx.date.num, 2), scale(rc.data$bx.age) ))
(d_V_RC<-dim(V_RC_data)[2]) #should be 6

