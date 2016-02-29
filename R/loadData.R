loadData <- function(demo.data) {
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
  
  }

ptDataframe <- function(pt.data) {
  ### 2. Start patient dataframe
  
  # We want the patient data to include one record per patient and true GS
  # We may also put variables in this data frame to ease definition of variables in the PSA and BX data frame (date of dx, age dx, average prostate volume)
  # Finally, we will order patients based on their observed GS. This is done to make estimation in JAGS easier. We assign a new sequential unique patient identifier for this ordering ("subj")
  

  #define true GS
  pt.data$true.gs<-rep(NA,n) #post-surgery true GS observation; want it to be "NA" for patients without surgery
  
  #pt.data
  #tx.data
  #filter(tx.data, id%in%pt.data$id)
  #newDataframe <- merge(pt.data, tx.data, by = "id")
  #newDataframe
  
  for(i in 1:n){
    if(pt.data$id[i]%in%tx.data$id){
      pt.data$true.gs[i]<-tx.data$GS[tx.data$id==pt.data$id[i]]
    }} #this is one place you could improve efficiency with an apply function of maybe something from dplyr
  
  table(pt.data$true.gs) #90 GS 6 and 113 GS 7+
  sum(is.na(pt.data$true.gs)) #797 pt without surgery 
  
  
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
  
  
  #get average biopsy volume
  #We want to take volume information from the biopsy data. Since increase in prostate volume due to cancer is negligible relative to the measurement error in assessing prostate volume, we will just take the average across all available biopsies
  #We are going to use prostate volume data in the PSA model. (Great volume leads to greater PSA)
  pt.data$vol.avg<-vector(length=n)
  for(i in 1:n){
    pt.data$vol.avg[i]<-mean(bx.data$vol[bx.data$id==pt.data$id[i]])} #another place dplyr may be helpful
  summary(pt.data$vol.avg)
  
  
  #order data based on observed true GS
  pt.data<-pt.data[order(pt.data$true.gs),]
  pt.data$true.gs[1:300]
  pt.data$subj<-c(1:n)
  return (pt.data)
}