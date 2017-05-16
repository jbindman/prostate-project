#Yates Coley
#rycoley@gmail.com
#2017-03-24

#This script takes results from teh JAGS run and produces the risk of reclassification for a patient (or patients) at a specfied time (or times)

#Julia- you can turn this into a function that takes the following inputs: data shaping workspace, patient id, time, coefficient estimates (see those needed below)




#load packages
library(splines)

#load data data shaping workspace
#load('~/Documents/inhealth/prediction-model/for-julia/IOP/IOP-data-shaping-work-space.RData')
#load('/Users/jbindman/Desktop/JHU/Prostate/prostate-project/IOP-data-shaping-work-space.RData')

predTest <-function(id_i = 657, date_pred = c(today, next_yr), pt = ptDataframes) {

  #get spline information for time since dx, age, and calendar date
  #we have to use the same knots (or whatever scaling information) used in estimation to modify the time, date, and age we want to make predictions for
  #you can check IOP-data-prep-for-jags-code. R and see that the spline/scaling used there is the same
  rc.data<-bx.full[bx.full$bx.here==1 & !is.na(bx.full$bx.here),]

  rc.time.knots<- attributes(ns(rc.data$bx.time,2))$knots
  rc.time.bknots<- attributes(ns(rc.data$bx.time,2))$Boundary.knots

  rc.date.knots<- attributes(ns(rc.data$bx.date.num,2))$knots
  rc.date.bknots<- attributes(ns(rc.data$bx.date.num,2))$Boundary.knots

  rc.age.mean<- mean(rc.data$bx.age)
  rc.age.sd<- sd(rc.data$bx.age)


  #get results from jags estimation
  #location.of.generated.files<-"/Users/ryc/Documents/inhealth/prediction-model/for-julia/IOP/for-cluster/results-for-julia" #you will have to change this to the appropriate directory for you
  location.of.generated.files <- "/Users/jbindman/Desktop/JHU/Prostate/prostate-project/R/results-for-julia"

  gamma<-read.csv(paste0(location.of.generated.files, "/jags-prediction-IOP-gamma_RC-1.csv"))
  gamma<-as.matrix(gamma[,2:dim(gamma)[2]])
  for(i in 2:5){
    res<-read.csv(paste0(location.of.generated.files, "/jags-prediction-IOP-gamma_RC-",i,".csv"))
    gamma<-rbind(gamma,res[,2:dim(res)[2]])}
  gamma<-as.matrix(gamma)
  dim(gamma)
  #this matrix has the coefficient estimates ("posterior sample") for the association between time, date, and age and risk of reclassification from 5 runs of the jags model. each model saved 1250 posterior samples

  #define length of posterior samples
  B<-dim(gamma)[1]

  etahat<-read.csv(paste0(location.of.generated.files, "/jags-prediction-IOP-eta_hat-1.csv"))
  etahat<-as.matrix(etahat[,2:dim(etahat)[2]])
  for(i in 2:5){
    res<-read.csv(paste0(location.of.generated.files, "/jags-prediction-IOP-eta_hat-",i,".csv"))
    etahat<-rbind(etahat,res[,2:dim(res)[2]])}


  ### SIDE NOTE TO EXPLAIN THIS FILE
  dim(etahat) #6250 817
  sum(is.na(pt.data$true.gs)) #817
  #this matrix has the true state predictions for all patients who do not have eta observed after surgery from 5 independent runs of the jags model
  #rows are the posterior preedictions, columns are the patients. take the mean within a column to get that patient's predicted state

  #for example
  mean(etahat[,1]) #this patient has a 43% chance of harboring a more aggressive cancer.
  #patients are in the same order here as in the pt.data dataframe so we can use that to determine the patient subj # and id
  (n_eta_known <- sum(!is.na(pt.data$true.gs))) #183

  #so, if we want to know the true state prediction for subj #200
  mean(etahat[,(200-n_eta_known)]) #their risk of having more aggressive cancer is 18%
  #if we only knew subj #200's id number  (it's 657) we can also get the risk of more aggressive cancer
  # pt.data$id[pt.data$subj==200]
  mean(etahat[,(pt.data$subj[pt.data$id==657]-n_eta_known)])
  #you may recall that the data comes to us with an id, but we then define this subject number once we have reordered the data. physicians or a patients may have the pt's medical record number (MRN) or some other unique identifier. they wouldn't have the unique "subj" variable we created

  ### SIDE NOTE ENDED


  #define number of patients with true state known
  (n_eta_known <- sum(!is.na(pt.data$true.gs))) #183


  # define inverse logit function
  expit <- function(x)
  {return(exp(x)/(1+exp(x)))}
  #since we used a logistic regression model to estimate the coefficients, we will need to back-transform to get probabilities of reclassification

  # define boundaries for quantiles
  quants <- c(0.025, 0.5, 0.95) #this means I want the 95% CI (between 2.5 and 97.5 percentiles) and the median (50th percentile)


  #in this example, I will calculate the risk of reclassification today and in one year, but you will want to write a function that can take some arbitrary time
  today<-as.numeric(as.Date("2017-03-24"))  #I'm using the fixed date instead of the system date so that you get the same results
  next_yr<-as.numeric(as.Date("2018-03-24"))
  date_pred <- c(today, next_yr)


  ### Ideally, you can write a function that will take some patient identifier (id_i below), vector date_pred, and the results loaded from above. You can decide if quants, spline and scaling information, etc. should be defined outside of the function and given as inputs or if you will define those objects then define the function dependent on some set values

  ##define subject to make predictions for
  #I am arbitrarily getting predictions for pt id #657
  id_i <- 657
  subj_i <- pt.data$subj[pt.data$id==id_i]

  ##true state predictions for subj_i
  etahat_i <- etahat[,(subj_i - n_eta_known)]

  ##define time, date, and age for predictions
  #time since diagnosis
  time_i <- (date_pred - pt.data$dx.date.num[pt.data$subj==subj_i])/365
  #then transform using spline representation
  time_i_ns<-ns(time_i, knots=rc.time.knots, Boundary.knots=rc.time.bknots)

  #transform date using spline representation
  date_i_ns<-ns(date_pred, knots=rc.date.knots, Boundary.knots=rc.date.bknots)

  #scale age
  age_i <- pt.data$age.dx[pt.data$subj==subj_i] + time_i
  age_i_std <- scale(age_i, center=rc.age.mean, scale=rc.age.sd)


  ##length of times to make predictions for
  n_pred<-length(date_pred)

  ##define covariate matrix with time, date, and age
  V_pred <- as.matrix(cbind(rep(1,n_pred), time_i_ns, date_i_ns, age_i_std))
  dV <- dim(V_pred)[2]

  #matrix algebra!
  logit_prc_pred_mat <- tcrossprod(gamma[,1:dV], V_pred) + matrix(rep(gamma[,(dV+1)]*etahat_i, each=n_pred), nrow=B, ncol=n_pred, byrow=TRUE)
  #this makes a matrix with 6250 rows (the number of posterior samples we have) and n_pred columns. We can summarize each column to get the risk of reclassification and confidence interval for each time period of interest

  prc_est_logit <- apply(logit_prc_pred_mat, 2, quantile, p=quants)
  prc_est <- expit(prc_est_logit) #transforms predictions on logit scale to probability scale

  #Risk of reclassifying today = 12.7% with 95% (2.1%, 58%)
  # Risk of reclassifiying next year = 14% (1.8%, 63%)

}
