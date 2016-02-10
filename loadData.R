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

