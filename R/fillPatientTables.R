#' Load dataframes from four patient data files
#'
#' @param tx.data one record per treatment received per patient
#' @param demo.data demographic data, one record per patient
#' @param psa.data psa data, one record per PSA test per patient
#' @param bx.data one record per biopsy per patient
#' @return list of three dataframes, pt.data, psa.data, bx.full
#' @export
#' 1. Load data, look at variables.
#' 2. Start patient dataframe
#' 3. Add relevant variables to PSA dataframe
#' 4. Start biopsy dataframe
#' 5. Save data

fillPatientTables <- function(tx.data = tx_data, demo.data = demo_data, psa.data = psa_data, bx.data = bx_data) { #default file names

  dataCheck(tx.data, demo.data, psa.data, bx.data)


  # checks:
  #-each patient needs date of birth
  #-all biopsy records must have a date
  #-all PSA observations must have a date
  #-all treatment records must have a date
  #-all patients must have age at diagnosis above 0. (I actually think it should probably be above 35.)  --> check date, later


  pt.data<-as.data.frame(demo.data$id)
  names(pt.data)<-"id"
  pt.data$true.gs
  (n<-dim(demo_data)[1]) #1000 patients in this data
  pt.data$true.gs<-rep(NA,n)
  #check dob
  for (i in demo.data$dob) {
    if  (as.Date(i) < "1900-01-01") { #what dates?  #possible to have future values for dates
      stop ("Patients must have DOB dates in correct range")
    }
  }


  #check treatment dates
  for (i in tx.data$tx.date) {
    if  (as.Date(i) < "1970-01-01") { #what dates?
      stop ("Patients must have treatment dates in correct range")
    }
  }

  for(i in 1:n){
    if(pt.data$id[i]%in%tx.data$id){
      pt.data$true.gs[i]<-tx.data$GS[tx.data$id==pt.data$id[i]]
    }}


  pt.data$dob.num<-as.numeric(as.Date(demo.data$dob)) #demo.data$dob
  pt.data$dob.num[1:10]


  #get diagnostic date, requires making bx dates numeric
  as.Date(bx.data$bx.date[1:10])
  bx.data$bx.date.num<-as.numeric(as.Date(bx.data$bx.date))
  bx.data$bx.date.num[1:10]
  as.Date(bx.data$bx.date.num[1:10], origin="1970-01-01")
  #bx check
  for (i in bx.data$bx.date) {
    if  (as.Date(i) < "1970-01-01") {
      stop ("Patients must have BX dates in correct range ")
    }
  }


  pt.data$dx.date.num<-rep(0,n)
  for(i in 1:n){
    pt.data$dx.date.num[i]<-bx.data$bx.date.num[bx.data$id==pt.data$id[i] & bx.data$dx==1]}

  #age at diagnosis
  pt.data$age.dx<-(pt.data$dx.date.num-pt.data$dob.num)/365
  for (i in pt.data$age.dx) {
    if  (i < 35) { #what age?
      stop ("Patients must be older than 35 when they received treatment")
    }
  }
  #summary(pt.data$age.dx)



  #average biopsy volume
  pt.data$vol.avg<-vector(length=n)
  for(i in 1:n){
    pt.data$vol.avg[i]<-mean(bx.data$vol[bx.data$id==pt.data$id[i]])
  }
  #summary(pt.data$vol.avg)


  #order data based on observed true GS
  pt.data<-pt.data[order(pt.data$true.gs),]
  pt.data$true.gs[1:300]
  pt.data$subj<-c(1:n)

  #log-PSA
  psa.data$log.psa<-log(psa.data$psa + 0.01)

  #date of test
  as.Date(psa.data$psa.date[1:10])
  psa.data$psa.date.num<-as.numeric(as.Date(psa.data$psa.date))
  psa.data$psa.date.num[1:10]
  as.Date(psa.data$psa.date.num[1:10], origin="1970-01-01")

  #check psa date
  for (i in psa.data$psa.date) {
    if  (as.Date(i) < "1970-01-01") {
      stop ("Patients must have PSA dates in correct range ")
    }
  }

  #age at each test
  (n_psa<-dim(psa_data)[1]) #18792 PSA tests
  psa.data$age<-vector(length=n_psa)
  psa.data$age

  for(i in 1:n){
    psa.data$age[psa.data$id==i] <- (psa.data$psa.date.num[psa.data$id==i] - pt.data$dob.num[i])/365
  }
  summary(psa.data$age)

  #pt-level prostate volume
  psa.data$vol.avg<-vector(length=n_psa)
  for(i in 1:n){
    psa.data$vol.avg[psa.data$id==pt.data$id[i]]<-pt.data$vol.avg[i]}
  summary(psa.data$vol.avg)

  #subj identifier
  psa.data$subj<-vector(length=n_psa)
  for(i in 1:n){
    psa.data$subj[psa.data$id==pt.data$id[i]]<-pt.data$subj[i]}

  #define maximum number of follow-up years per patient
  #we will deal with censoring, treatment dates, etc. later
  pt.data$total.fup<-vector(length=n)
  for(i in 1:n){
    pt.data$total.fup[i] <- (max(max(psa.data$psa.date.num[psa.data$id==pt.data$id[i]]), max(bx.data$bx.date.num[bx.data$id==pt.data$id[i]])) - pt.data$dx.date.num[i])/365}
  summary(pt.data$total.fup)

  #adding time since dx and subj variable to bx.data will also help
  (n_bx<-dim(bx_data)[1]) #4134 biopsy observations
  bx.data$subj<-vector(length=n_bx)
  for(i in 1:n){
    bx.data$subj[bx.data$id==pt.data$id[i]]<-pt.data$subj[i]}

  bx.data$time.since.dx<-vector(length=n_bx)
  for(i in 1:n){
    bx.data$time.since.dx[bx.data$id==pt.data$id[i]]<-(bx.data$bx.date.num[bx.data$id==pt.data$id[i]] - pt.data$dx.date.num[i])/365
  }
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
    # everyone here had a biopsy and is easily assigned
    if(bx.full$time.int[j]==0){ #diagnostic biopsies
      bx.full$bx.time[j]<-0
      bx.full$bx.date.num[j]<-pt.data$dx.date.num[pt.data$subj==bx.full$subj[j]]
      bx.full$bx.age[j]<-pt.data$age.dx[pt.data$subj==bx.full$subj[j]]
      bx.full$bx.here[j]<-1
      bx.full$rc[j]<-0
    }
    # for patient i patient year j, look at biopsy data and see if there were biospies in year j "use"
    # if theres at least one biopsy, this indicator variable "use" will be created than 0. quick and dirty

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
      #if no biopsies, assign everything NA
      else{ #if the patient didn't get any biopsies in this interval
        bx.full$bx.time[j] <- bx.full$bx.date.num[j] <- bx.full$bx.age[j] <- bx.full$rc[j] <- NA
        bx.full$bx.here[j]<-0
      }
    }
  }


  table(bx.full$bx.here) #the number of biopsies should equal n_bx (since we don't have any overlaps in intervals)

  table(bx.full$rc) #these should also be equal
  sum(bx.data$RC)


  pt.data$rc<-rep(0,n)
  for(i in 1:n){
    if(max(bx.full$rc[bx.full$subj==i], na.rm=T)==1){pt.data$rc[i]<-1}}


  patientDataframes<-list(pt.data=pt.data, psa.data=psa.data, bx.full=bx.full)
  return(patientDataframes)
}





