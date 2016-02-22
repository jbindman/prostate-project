ptDataframe <- function() {
  rm(list=ls())

  ##WORKFLOW
  # 1. Load data, look at variables. 
  # 2. Start patient dataframe
  # 3. Add relevant variables to PSA dataframe
  # 4. Start biopsy dataframe
  # 5. Save data
 
  demo.data<-read.csv("julia-demo-data.csv")
  names(demo.data)
  (n<-dim(demo.data)[1]) #1000 patients in this data
  #psa data. one record per PSA test per patient
  psa.data<-read.csv("julia-psa-data.csv")
  names(psa.data)
  #id- unique identifier
  #psa- total PSA measured
  #psa.data- date of each PSA test
  
  (n_psa<-dim(psa.data)[1]) #18792 PSA tests
  
  #bx.data. one record per biopsy per patient
  bx.data<-read.csv("julia-bx-data.csv")
  names(bx.data)
  #id- unique identifier
  #bx.date- date of biopsy (abbreviated bx)
  #RC- binary indicator or whether grade reclassification (abbreviated RC), i.e. bx gleason 7+, occurred
  #vol- volume of prostate 
  #dx- binary indicator of diagnostic biopsy for each patient (diagnosis abbreviated dx)
  
  (n_bx<-dim(bx.data)[1]) #4134 biopsy observations
  
  #tx.data. one record per treatment received per patient
  #this data is just for surgery
  tx.data<-read.csv("julia-tx-data.csv")
  names(tx.data)
  #id- unique identifier
  #GS- indicator of whether post-surgery Gleason score (abbreviated GS) was 7 or higher; aka "true GS"
  #tx.date- date of treatment (abbreviated tx) 
  
  (n_tx<-dim(tx.data)[1]) #203 patients received treatment
  
  #######################
  
  #FILLING DATAFRAME NOW
  pt.data<-as.data.frame(demo.data$id) # adds Id column 
  names(pt.data)<-"id"
  pt.data<-ptDataload(pt.data)
  pt.data
  #addPSA(pt.data, temp)
  #biopsy(pt.data)
  
  save(pt.data, psa.data, bx.full,file="data-shaping-work-space.RData")
}

ptDataload <- function(pt = pt.data, tx = tx.data, demo = demo.data, 
                       bx = bx.data) {
  #define true GS
  pt.data$true.gs<-rep(NA,n) #post-surgery true GS observation; want it to be "NA" for patients without surgery
  
  for(i in 1:n){
    if(pt$id[i]%in%tx$id){
      pt$true.gs[i]<-tx$GS[tx$id==pt$id[i]] #tx.data$GS[tx.data$id==pt.data$id[i]]
    }} #this is one place you could improve efficiency with an apply function of maybe something from dplyr
  ####
  #DPLYR
  
  #pt$true.gs<-filter(tx, GS)
  
  ###
  
  table(pt$true.gs) #90 GS 6 and 113 GS 7+
  sum(is.na(pt$true.gs)) #797 pt without surgery 
  
  
  #define DOB numerically
  #warning: date functions in R can be tricky! we will have to do some work later to make sure our function handles all date types
  as.Date(demo.data$dob[1:10])
  # ADD CUSTOMIZABLE
  pt.data$dob.num<-as.numeric(as.Date(demo$dob)) #demo.data$dob
  pt.data$dob.num[1:10]
  #as.Date(pt.data$dob.num[1:10], origin="1970-01-01") #I always check that I've defined the dates right
  #another note- here I just copied dates from demo.data to pt.data because I know the ids are in the same order. this may not always be the case
  
  
  #get diagnostic date
  #requires making bx dates numeric
  as.Date(bx$bx.date[1:10])
  bx$bx.date.num<-as.numeric(as.Date(bx$bx.date)) #bx.data$bx.date
  bx$bx.date.num[1:10]
  as.Date(bx$bx.date.num[1:10], origin="1970-01-01")
  
  pt$dx.date.num<-rep(0,n)
  for(i in 1:n){
    pt$dx.date.num[i]<-bx$bx.date.num[bx$id==pt$id[i] & bx$dx==1]}
  
  #age at diagnosis
  pt$age.dx<-(pt$dx.date.num-pt$dob.num)/365
  summary(pt$age.dx)
  
  
  #get average biopsy volume
  #We want to take volume information from the biopsy data. Since increase in prostate volume due to cancer is negligible relative to the measurement error in assessing prostate volume, we will just take the average across all available biopsies
  #We are going to use prostate volume data in the PSA model. (Great volume leads to greater PSA)
  pt$vol.avg<-vector(length=n)
  for(i in 1:n){
    pt$vol.avg[i]<-mean(bx$vol[bx$id==pt$id[i]])} #another place dplyr may be helpful
  summary(pt.data$vol.avg)
  
  
  #order data based on observed true GS
  pt<-pt.data[order(pt$true.gs),]
  pt$true.gs[1:300]
  pt$subj<-c(1:n)
  return(pt.data)
}
addPSA <- function (pt.data = pt.data, psa.data = psa.data) {
  #log-PSA
  psa.data$log.psa<-log(psa.data$psa + 0.01) #necessary to add a small number when some values are 0 (not the case in the simulated data, but will be the case in real data)
  
  #date of test
  as.Date(psa.data$psa.date[1:10])
  psa.data$psa.date.num<-as.numeric(as.Date(psa.data$psa.date))
  psa.data$psa.date.num[1:10]
  as.Date(psa.data$psa.date.num[1:10], origin="1970-01-01")
  
  #age at each test
  psa.data$age<-vector(length=n_psa)
  for(i in 1:n){
    psa.data$age[psa.data$id==i] <- (psa.data$psa.date.num[psa.data$id==i] - pt.data$dob.num[i])/365 #GIVES ME ERRORS
      }
  summary(psa.data$age) #some of these are unrealistic; this is because the data is fake
  
  #pt-level prostate volume
  psa.data$vol.avg<-vector(length=n_psa)
  for(i in 1:n){
    psa.data$vol.avg[psa.data$id==pt.data$id[i]]<-pt.data$vol.avg[i]}
  summary(psa.data$vol.avg)
  
  #subj identifier
  psa.data$subj<-vector(length=n_psa)
  for(i in 1:n){
    psa.data$subj[psa.data$id==pt.data$id[i]]<-pt.data$subj[i]}
  
}
biopsy <- function (pt.data = pt.data, bx.data = bx.data) {
  #define maximum number of follow-up years per patient
  #we will deal with censoring, treatment dates, etc. later
  pt.data$total.fup<-vector(length=n)
  for(i in 1:n){
    pt.data$total.fup[i] <- (max(max(psa.data$psa.date.num[psa.data$id==pt.data$id[i]]), max(bx.data$bx.date.num[bx.data$id==pt.data$id[i]])) - pt.data$dx.date.num[i])/365}
  summary(pt.data$total.fup)
  
  #adding time since dx and subj variable to bx.data will also help
  bx.data$subj<-vector(length=n_bx)
  for(i in 1:n){
    bx.data$subj[bx.data$id==pt.data$id[i]]<-pt.data$subj[i]}
  
  bx.data$time.since.dx<-vector(length=n_bx)
  for(i in 1:n){
    bx.data$time.since.dx[bx.data$id==pt.data$id[i]]<-(bx.data$bx.date.num[bx.data$id==pt.data$id[i]] - pt.data$dx.date.num[i])/365
    }
    #gives me issues
  summary(bx.data$time.since.dx)
  
  
  
  #start by defining one time interval for each year a pt is under surveillance
  bx.subj<-rep(pt.data$subj[1],ceiling(pt.data$total.fup[1])+1)
  bx.time.int<-c(0:ceiling(pt.data$total.fup[1]))
  for(i in 2:n){
    bx.subj<-c(bx.subj,rep(pt.data$subj[i],ceiling(pt.data$total.fup[i])+1))
    bx.time.int<-c(bx.time.int,c(0:ceiling(pt.data$total.fup[i])))}
  
  bx.full<-as.data.frame(cbind(bx.subj,bx.time.int))
  names(bx.full)<-c("subj", "time.int")
  (N<-dim(bx.full)[1]) #total number of person-years
  
  
  # Each row will have data on the time since dx, date and age, whether a biopsy was received, and, if so, its result
  bx.full$bx.time<-bx.full$bx.date.num<-bx.full$bx.age<-bx.full$bx.here<-bx.full$rc<-vector(length=N)
  
  
  for(j in 1:N){
    if(bx.full$time.int[j]==0){ #diagnostic biopsies
      bx.full$bx.time[j]<-0
      bx.full$bx.date.num[j]<-pt.data$dx.date.num[pt.data$subj==bx.full$subj[j]]
      bx.full$bx.age[j]<-pt.data$age.dx[pt.data$subj==bx.full$subj[j]]
      bx.full$bx.here[j]<-1
      bx.full$rc[j]<-0
    }
    else{ #post-dx biopsies
      bx.data$use<-rep(0,n_bx) #clearing existing values in this variable
      bx.data$use[ bx.data$subj==bx.full$subj[j] & bx.data$time.since.dx>(bx.full$time.int[j]-1) & bx.data$time.since.dx<=bx.full$time.int[j] ] <- 1 #identifying biopsies for this patient in the time interval of interest
      
      if(sum(bx.data$use) > 0){ #if this patient had any biopsies in this time interval
        bx.full$bx.time[j] <- max(bx.data$time.since.dx[bx.data$use==1]) #want to use max in case patient had multiple biopies in an interval (I don't think there are any multiples in this simulated data)
        bx.full$bx.date.num[j] <- max(bx.data$bx.date.num[bx.data$use==1])
        bx.full$bx.age[j] <- bx.full$bx.time[j] + pt.data$age.dx[pt.data$subj==bx.full$subj[j]]
        bx.full$bx.here[j] <- 1
        bx.full$rc[j] <- max(bx.data$RC[bx.data$use==1])
      }
      
      else{ #if the patient didn't get any biopsies in this interval
        bx.full$bx.time[j] <- bx.full$bx.date.num[j] <- bx.full$bx.age[j] <- bx.full$rc[j] <- NA
        bx.full$bx.here[j]<-0
      }
    }
  } #this for loop obviously takes an unreasonably long time to run
  
  table(bx.full$bx.here) #the number of biopsies should equal n_bx (since we don't have any overlaps in intervals)
  
  table(bx.full$rc) #these should also be equal 
  sum(bx.data$RC) 
  
  summary(bx.full$bx.time[bx.full$time.int==1])
  summary(bx.full$bx.time)
  summary(bx.full$bx.date.num[bx.full$time.int==1])
  summary(bx.full$bx.date.num)
  summary(bx.full$bx.age[bx.full$time.int==1])
  summary(bx.full$bx.age)
  
  
  #I have various output here in order to "sanity check" the data. Of course, we wouldn't want to have to run these every time we are shaping the data, but I wanted to give you an idea of how you could check that your code did the right thing. Also, we may want to build a couple of data checks into the function so that the user gets an error message if they try to put in problematic data.
  
}