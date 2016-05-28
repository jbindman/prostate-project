#' fillPatientTablesIOP.R
#'
#' Load dataframes from four patient data files to create a list of formatted patient dataframes.
#'
#' @param surg.data One record per treatment received per patient containing treatment date and GS
#' @param demo.data Demographic data, one record per patient ID containing DOB
#' @param psa.data PSA data, one record per PSA test per patient containing date of PSA test
#' @param bx.data One record per biopsy per patient containing reclassicfication, volume, and dx
#' @return List of three dataframes, pt.data, psa.data, bx.full
#' @export
# @details
# 1. Load data, look at variables.
# 2. Start patient dataframe
# 3. Add relevant variables to PSA dataframe
# 4. Start biopsy dataframe
# 5. Save data
fillPatientTablesIOP <- function(demo.data = demo_data, psa.data = psa_data, bx.data = bx_data, surg.data = surg_data) { #default file names

  dataCheck(surg.data, demo.data, psa.data, bx.data) #do the checks need to be different?


  names(demo.data)
  #id- unique identifier for each patient (abbreviated pt)
  #dob- date of birth, in  "%Y-%M-%D" form
  #status.rc- did this patient reclassify on biopsy
  #censor.dt.rc- if status.rc=1, date of reclassification; if status.rc=0, date of censoring
  #status.tx- did this patient have any treatment (surgery, radiation, etc.)
  #censor.dt.tx- if status.tx=1, date of treatment; if status.tx=0, date of censoring
  (n<-dim(demo.data)[1]) #1000 patients in this data

  names(psa.data)
  #id- unique identifier
  #psa- total PSA measured
  #psa.data- date of each PSA test
  (n_psa<-dim(psa.data)[1]) #15083 PSA tests

  names(bx.data)
  #id- unique identifier
  #bx.date- date of biopsy (abbreviated bx)
  #RC- binary indicator or whether grade reclassification (abbreviated RC), i.e. bx gleason 7+, occurred
  #vol- volume of prostate
  #dx- binary indicator of diagnostic biopsy for each patient (diagnosis abbreviated dx)
  (n_bx<-dim(bx.data)[1]) #3842 biopsy observations


  names(surg.data)
  #id- unique identifier
  #GS- indicator of whether post-surgery Gleason score (abbreviated GS) was 7 or higher; aka "true GS"
  #surg.date- date of surgery (abbreviated surg)
  (n_surg<-dim(surg.data)[1]) #183 patients received treatment






  # checks:
  #-each patient needs date of birth
  #-all biopsy records must have a date
  #-all PSA observations must have a date
  #-all treatment records must have a date
  #-all patients must have age at diagnosis above 0. (I actually think it should probably be above 35.)  --> check date, later




  #define data frame
  pt.data<-as.data.frame(demo.data$id)
  names(pt.data)<-"id"



  #define surgery, true GS
  pt.data$surgery<-rep(0,n)
  pt.data$true.gs<-rep(NA,n) #post-surgery true GS observation; want it to be "NA" for patients without surgery

  for(i in 1:n){
    if(pt.data$id[i]%in%surg.data$id){
      pt.data$surgery[i]<-1
      pt.data$true.gs[i]<-surg.data$GS[surg.data$id==pt.data$id[i]]
    }} #this is one place you could improve efficiency with an apply function of maybe something from dplyr

  table(pt.data$surgery) #183
  table(pt.data$true.gs) #84 GS 6 and 99 GS 7+
  sum(is.na(pt.data$true.gs)) #817 pt without surgery


  #define DOB numerically
  #warning: date functions in R can be tricky! we will have to do some work later to make sure our function handles all date types
  as.Date(demo.data$dob[1:10])
  pt.data$dob.num<-as.numeric(as.Date(demo.data$dob))
  pt.data$dob.num[1:10]
  as.Date(pt.data$dob.num[1:10], origin="1970-01-01") #I always check that I've defined the dates right
  #another note- here I just copied dates from demo.data to pt.data because I know the ids are in the same order. this may not always be the case


  #get diagnostic date
  #requires making bx dates numeric
  as.Date(bx.data$bx.date[1:10])
  bx.data$bx.date.num<-as.numeric(as.Date(bx.data$bx.date))
  bx.data$bx.date.num[1:10]
  as.Date(bx.data$bx.date.num[1:10], origin="1970-01-01")

  pt.data$dx.date.num<-rep(0,n)
  for(i in 1:n){
    pt.data$dx.date.num[i]<-bx.data$bx.date.num[bx.data$id==pt.data$id[i] & bx.data$dx==1]}

  #age at diagnosis
  pt.data$age.dx<-(pt.data$dx.date.num-pt.data$dob.num)/365
  summary(pt.data$age.dx)

  #will add to fillPatientTables function
  ###NEW
  #define reclassification, treatment outcomes and dates
  #this information just has to be moved from demo.data
  pt.data$status.rc<-demo.data$status.rc
  pt.data$censor.dt.rc.num<-as.numeric(as.Date(demo.data$censor.dt.rc))
  #this tells us how long a patient was eligible to have a biopsy and reclassify

  pt.data$status.tx<-demo.data$status.tx
  pt.data$censor.dt.tx.num<-as.numeric(as.Date(demo.data$censor.dt.tx))
  #this tells us how long a patient was eligible for surgery, treatment
  #for this simulated data set, only patients with surgery have treatment. but, in a real dataset, patients may have also left AS for radiation therapy or other treatment. For that reason, we will not treat status.tx and surgery variables as interchangeable in this code.

  #you may want to check that date transformation worked correctly


  ###NEW
  #time until treatment or censoring; total follow-up time for all patients
  pt.data$time.until.tx <- (pt.data$censor.dt.tx.num-pt.data$dx.date.num)/365
  summary(pt.data$time.until.tx)


  #get average biopsy volume
  #We want to take volume information from the biopsy data. Since increase in prostate volume due to cancer is negligible relative to the measurement error in assessing prostate volume, we will just take the average across all available biopsies
  #We are going to use prostate volume data in the PSA model. (Great volume leads to greater PSA)
  pt.data$vol.avg<-vector(length=n)
  for(i in 1:n){
    pt.data$vol.avg[i]<-mean(bx.data$vol[bx.data$id==pt.data$id[i]])} #another place dplyr may be helpful
  summary(pt.data$vol.avg)

  #standardize prostate volume, so that mean= 0 and std dev=1
  pt.data$vol.std<-scale(pt.data$vol.avg)



  #order data based on observed true GS
  pt.data<-pt.data[order(pt.data$true.gs),]
  pt.data$true.gs[1:300]
  pt.data$subj<-c(1:n)




  ### 3. Add relevant variables to PSA dataframe
  #We need to log-transform PSA observations, make PSA dates numeric, define pt age at time of PSA, add volume data and new subj identifier

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
    psa.data$age[psa.data$id==i] <- (psa.data$psa.date.num[psa.data$id==i] - pt.data$dob.num[i])/365}
  summary(psa.data$age) #some of these are unrealistic; this is because the data is fake
  #standardize age so that mean age=0, sd=1
  psa.data$age.std<-scale(psa.data$age)

  names(psa.data)

  #pt-level prostate volume
  #I've changed this from the earlier version. I am saving the standardized volume (not the average volume) in the PSA data
  psa.data$vol.std<-vector(length=n_psa)
  for(i in 1:n){
    psa.data$vol.std[psa.data$id==pt.data$id[i]]<-pt.data$vol.std[i]}
  summary(psa.data$vol.std)

  #subj identifier
  psa.data$subj<-vector(length=n_psa)
  for(i in 1:n){
    psa.data$subj[psa.data$id==pt.data$id[i]]<-pt.data$subj[i]}



  ### 4. Start biopsy dataframe
  # We want a dataframe with one row per year per patient
  # Each row will have data on the time since dx, date, whether a biopsy was received, and, if so, its result
  # (Note from first pass) This is going to seem like bit overkill initially because I know that we don't have repeated biopsies in a year (since I simulated the data myself) and in our first pass at model estimation we aren't going to use the annual intervals without biopsies. There will be more complexity added later
  # In this second pass, we have one row per year per patient for as many years as this patient is under surveillance (pt.data$time.until.tx variable above)

  #define maximum number of follow-up years per patient

  ###CODE REMOVED FROM EARLIER VERSION
  #I'm not using the 5 lines below in this code (this was in your earlier data-shaping code) #I'm doing something a bit more extensive now to deal with follow-up time and censoring
  #pt.data$time.until.tx<-vector(length=n)
  #for(i in 1:n){
  #	pt.data$time.until.tx[i] <- (max(max(psa.data$psa.date.num[psa.data$id==pt.data$id[i]]), max(bx.data$bx.date.num[bx.data$id==pt.data$id[i]])) - pt.data$dx.date.num[i])/365}
  #summary(pt.data$time.until.tx)

  #adding time since dx and subj variable to bx.data will also help
  bx.data$subj<-vector(length=n_bx)
  for(i in 1:n){
    bx.data$subj[bx.data$id==pt.data$id[i]]<-pt.data$subj[i]}

  bx.data$time.since.dx<-vector(length=n_bx)
  for(i in 1:n){
    bx.data$time.since.dx[bx.data$id==pt.data$id[i]]<-(bx.data$bx.date.num[bx.data$id==pt.data$id[i]] - pt.data$dx.date.num[i])/365}
  summary(bx.data$time.since.dx)


  ### NOTE pt.data$time.to.tx was the same variable as pt.data$total.fup in the earlier code

  #start by defining one time interval for each year a pt is under surveillance
  bx.subj<-rep(pt.data$subj[1],ceiling(pt.data$time.until.tx[1])+1)
  bx.time.int<-c(0:ceiling(pt.data$time.until.tx[1]))
  for(i in 2:n){
    bx.subj<-c(bx.subj,rep(pt.data$subj[i],ceiling(pt.data$time.until.tx[i])+1))
    bx.time.int<-c(bx.time.int,c(0:ceiling(pt.data$time.until.tx[i])))}

  bx.full<-as.data.frame(cbind(bx.subj,bx.time.int))
  names(bx.full)<-c("subj", "time.int")
  (N<-dim(bx.full)[1]) #total number of person-years #7832


  # Each row will have data on the time since dx, date and age, whether a biopsy was received, and, if so, its result
  ##NEW in previous code, we had a single variable for each of time, date, and age in an interval. specifically, we used the time since dx, date, and age at a biopsy that occurred. here, we want to also record the time, date, and age at the start of the interval to model the probability of having a biopsy and the probability of having surgery in each interval. (these aren't tied to biopsy date).

  bx.full$int.date.num<-bx.full$int.age<-vector(length=N)
  bx.full$bx.time<-bx.full$bx.date.num<-bx.full$bx.age<-vector(length=N)
  bx.full$bx.here<-bx.full$rc<-vector(length=N)

  ### NOTE I also realized a slightly faster way to do this than I did in the earlier code, so I've changed it a bit. (I am not looping through all biopsies when not necessary)

  #data at time=0
  bx.full$bx.time[bx.full$time.int==0]<-0
  bx.full$bx.here[bx.full$time.int==0]<-1
  bx.full$rc[bx.full$time.int==0]<-0


  #variable values that do not depend on patient biopsy data, just pt.data
  for(i in 1:n){
    bx.full$bx.date.num[bx.full$time.int==0 & bx.full$subj==pt.data$subj[i]] <- pt.data$dx.date.num[i]
    bx.full$bx.age[bx.full$time.int==0 & bx.full$subj==pt.data$subj[i]] <- pt.data$age.dx[i]

    bx.full$int.date.num[bx.full$subj==pt.data$subj[i]] <- pt.data$dx.date.num[i] + bx.full$time.int[bx.full$subj==pt.data$subj[i]]*365
    bx.full$int.age[bx.full$subj==pt.data$subj[i]] <- pt.data$age.dx[i] + bx.full$time.int[bx.full$subj==pt.data$subj[i]]}


  #variable values that do depend on patient biopsy data (this is similar to earlier code)
  for(j in 1:N){

    if(bx.full$time.int[j]>0){ #post-dx biopsies

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
  } #

  table(bx.full$bx.here) #the number of biopsies should equal n_bx (since we don't have any overlaps in intervals)

  table(bx.full$rc) #these should also be equal
  sum(bx.data$RC)

  summary(bx.full$bx.time[bx.full$time.int==1])
  summary(bx.full$bx.time)
  summary(bx.full$bx.date.num[bx.full$time.int==1])
  summary(bx.full$bx.date.num)
  summary(bx.full$bx.age[bx.full$time.int==1])
  summary(bx.full$bx.age)



  ###Everything below here is NEW and needed for modeling biopsy and surgery outcomes

  #number of previous biopsies at start and end of each interval
  bx.full$num.prev.bx.start<-bx.full$num.prev.bx.end<-vector(length=N)

  #did patient experience reclssification earlier (including this interval)?
  bx.full$prev.rc<-vector(length=N)

  bx.full$num.prev.bx.start[bx.full$time.int==0]<-0
  bx.full$num.prev.bx.end[bx.full$time.int==0]<-1


  for(j in 1:N){if(bx.full$time.int[j]>0){
    bx.full$num.prev.bx.start[j]<-sum(bx.full$subj==bx.full$subj[j] & bx.full$bx.here==1  & bx.full$time.int<bx.full$time.int[j])
    bx.full$num.prev.bx.end[j]<-sum(bx.full$subj==bx.full$subj[j] & bx.full$bx.here==1 & bx.full$time.int<=bx.full$time.int[j])
    bx.full$prev.rc[j]<-max(bx.full$rc[bx.full$subj==bx.full$subj[j] & bx.full$bx.here==1 & bx.full$time.int<=bx.full$time.int[j]])
  }}

  table(bx.full$num.prev.bx.start)
  table(bx.full$num.prev.bx.end)
  table(bx.full$prev.rc)

  #We need to indicate that patients aren't eligible for biopsy after reclassification. We will do this by making bx.here=NA after reclassification

  for(i in 1:n){
    if(pt.data$status.rc[i]==1){
      rc.time<-bx.full$time.int[bx.full$subj==pt.data$subj[i] & bx.full$rc==1 & !is.na(bx.full$rc)]
      bx.full$bx.here[bx.full$subj==pt.data$subj[i] & bx.full$time.int>rc.time]<-NA}}
  sum(is.na(bx.full$bx.here)) #537

  #I have various output here in order to "sanity check" the data. Of course, we wouldn't want to have to run these every time we are shaping the data, but I wanted to give you an idea of how you could check that your code did the right thing. Also, we may want to build a couple of data checks into the function so that the user gets an error message if they try to put in problematic data.


  #I'm saving the workspace here so you can see what the data frames looked like when I finished, but you will want to define these objects in some simple, easy to understand, efficient way
  #save(pt.data, psa.data, bx.full,file="IOP-data-shaping-work-space.RData")


  patientDataframes<-list(pt.data=pt.data, psa.data=psa.data, bx.full=bx.full)
  return(patientDataframes)
}


#pt.data$rc versus pt.data$status.rc?
#
#pt.data$rc<-rep(0,n)
#for(i in 1:n){
#  if(max(bx.full$rc[bx.full$subj==i], na.rm=T)==1){pt.data$rc[i]<-1}}
#Pt.data needs to be ordered by subject to run this line as is. Otherwise, try if(max(bx.full$rc[bx.full$id==pt.data$id[i]], na.rm=T)==1){pt.data$rc[i]<-1}



