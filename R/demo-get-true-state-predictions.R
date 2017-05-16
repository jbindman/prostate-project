


rm(list=ls())
my.wd<-"/Users/ryc/Documents/inhealth/prediction-model/for-julia/IOP/IS-algorithm"
setwd(my.wd)


source("functions-dynamic.R") #where is this reading from?

##description of inputs for get_eta_pred

#psa: list of psa values
#psa_age: age at time of PSA tests (likely want patients to put in birthdate and dates of tests and derive this)
#vol: average prostate volume

#BX: binary indicator of receiving a biopsy in each annual interval (starting at diagnosis); one record for diagnosis and each annual interval since
#bx_time_int: annual time interval e.g. 0, 1, 2, ... for diagnosis, 1 year after biopsy, 2 year after biopsy,...
#bx_int_date_num: numeric date at the start of each annual interval (numeric date of diagnosis + 365*bx_time_int)
#bx_int_age: patient age at the start of each annual interval (age at diagnosis + bx_time_int)
#bx_num_prev_bx_start: number of biopsies patient has received prior to the start of this interval


#RC: results of all prostate biopsies. BX=0 no upgrading vs. BX=1 upgrading. should be the same number of records as BX.
#rc_age: age at time of post-dx biopsies
#rc_time: time since dx of all biopsies
#rc_date: numberic calendar date of all biopsies
## The age, time, and date of the actual biopsy are different from time_int, int_date, and int_age above (exceptions: diagnosis and if the biopsy was performed exactly 365*time_int days from diagnosis)

## SURG: binary indicator of receiving surgery in each annual interval (less than 1 year after dx, between 1 and 2 years since dx, etc. ). this should also be the same length as BX. ***Let me know if this restriction needs to be lifted
## time_int, int_date, and int_age are the same for surgery intervals, so they don't need to be redefined again.
#surg_num_prev_bx_end: the number of biopsies patient has received at the end of this interval (so surg_num_prev_bx_end= bx_num_prev_bx_start if no biopsy in that interval and bx_num_prev_bx_start+1 if biopsy was done)
#surg_prev_rc: binary indicator of whether the patient has reclassified (in this interval or an early interval) ** this code is intended for patients who have reclassified at most once. ** this code does not use any follow-up biopsies or surgeries after reclassification




## To demonstrate the function, I will get these inputs from the dataset we currently have (It was loaded)

load("IOP-data-shaping-work-space.RData")

try.ids<- pt.data$subj[pt.data$surgery==0 & pt.data$status.rc==0]

#works from subjects, not id
# to do ids 25 and 3, use subj 223 and 1

subj_i<-try.ids[1]

subj_i <- 249

psa<- psa.data$psa[psa.data$subj==subj_i]
psa_age<- psa.data$age[psa.data$subj==subj_i]
vol<- pt.data$vol.avg[pt.data$subj==subj_i]

bx.data<-bx.full[!is.na(bx.full$bx.here),]
BX<-bx.data$bx.here[bx.data$subj==subj_i]
bx_time_int<-bx.data$time[bx.data$subj==subj_i]
bx_int_date_num<-bx.data$int.date.num[bx.data$subj==subj_i]
bx_int_age<-bx.data$int.age[bx.data$subj==subj_i] #int age vs bx age
bx_num_prev_bx_start<-bx.data$num.prev.bx.start[bx.data$subj==subj_i]

rc.data<-bx.full[bx.full$bx.here==1 & !is.na(bx.full$bx.here),]
RC<- rc.data$rc[rc.data$subj==subj_i]
rc_age<- rc.data$bx.age[rc.data$subj==subj_i]
rc_time<- rc.data$bx.time[rc.data$subj==subj_i]
rc_date<- rc.data$bx.date.num[rc.data$subj==subj_i]

SURG<-bx.full$surgery[bx.full$subj==subj_i]
surg_num_prev_bx_end<-bx.full$num.prev.bx.end[bx.full$subj==subj_i]
surg_prev_rc<-bx.full$prev.rc[bx.full$subj==subj_i]



get_eta_pred(psa, psa_age, vol,
						BX, bx_time_int, bx_int_date_num, bx_int_age, bx_num_prev_bx_start,
						RC, rc_age, rc_time, rc_date,
						SURG, surg_num_prev_bx_end, surg_prev_rc)

#for try.ids 1-5
# #0.22
# 0.091
# 0.19
# 0.059
# #0.11
