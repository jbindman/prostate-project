#' probability
#'
#' My working function for finding individual probabilities over time

probability<-function(pt.id){

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

  #pt.id <-436 #subj 415


  individualPsa <- subset(psa.data, id == pt.id)
  #individualBx.full <- subset(individualBx.full, bx.here == 1)
  individualBx <-  subset(bx.full, id == pt.id)


  first <- filter(individualBx, bx.here == 1)$bx.date.num[1] #individualBx$bx.date.num[1]



  last <- max(tail(filter(individualBx, bx.here == 1)$bx.date.num, n = 1), tail(individualPsa$psa.date.num, n= 1))

  years <- ceiling((last - first)/365) #= 6 years
  dates <- NULL
  for (i in 1:years) {
    dates <- append(dates, first+i*365)
  }

  #dates <- individualBx$int.date.num #use once you can get rid of first entry
  #dates <- c(14186, 14916, 15281, 15646, 16011, 16741, 17471) #random biopsy dates for id 260

  prediction <- NULL
  #print("hello")
  biopsyVar <- NULL

  for (i in dates) {
    #print(i)
    psa<- filter(individualPsa, psa.date.num < i)$psa
    psa_age<- filter(individualPsa, psa.date.num < i)$age
    vol<- filter(individualPsa, psa.date.num < i)$vol.avg[1] #made 1d to match Yates


    BX <- filter(individualBx, int.date.num < i)$bx.here
    bx_time_int <- filter(individualBx, int.date.num < i)$time.int
    bx_int_date_num<-filter(individualBx, int.date.num < i)$int.date.num
    bx_int_age<-filter(individualBx, int.date.num < i)$int.age
    bx_num_prev_bx_start<- filter(individualBx, int.date.num < i)$num.prev.bx.start


    RC<- filter(individualBx, bx.here == 1, int.date.num < i)$rc
    rc_age<- filter(individualBx, bx.here == 1, int.date.num < i)$bx.age
    rc_time<- filter(individualBx, bx.here == 1, int.date.num < i)$bx.time
    rc_date<- filter(individualBx, bx.here == 1, int.date.num < i)$bx.date.num

    SURG <- filter(individualBx, int.date.num < i)$surgery
    surg_num_prev_bx_end<-filter(individualBx, int.date.num < i)$num.prev.bx.end
    surg_prev_rc<-filter(individualBx, int.date.num < i)$prev.rc


    pred_time.i <- get_eta_pred(psa, psa_age, vol,
                                BX, bx_time_int, bx_int_date_num, bx_int_age, bx_num_prev_bx_start,
                                RC, rc_age, rc_time, rc_date,
                                SURG, surg_num_prev_bx_end, surg_prev_rc)


    #biopsy addition

    recBiopsy <- filter(individualBx, int.date.num < (i + 300), int.date.num > (i - 300))
    if (recBiopsy$bx.here == 1) {
      if (recBiopsy$rc[1] == 0) {
        x <- 0
      } else if (recBiopsy$rc[1] == 1) {
        x <- 1 #never returns any 1s, im filtering to only look at patients that never reclassify....
      } else {
        x <- 2
      }

    } else {
      x <- 2
    }

    biopsyVar <- append(biopsyVar, x) #if recent biopsies were had


    prediction <- append(prediction, pred_time.i)

  }


  prediction.data <- data.frame(col1=dates, col2=prediction)
  dob_pt.id <- filter(pt.data, id == pt.id)$dob.num
  prediction.data$ages <- (dates - dob_pt.id)/365
  prediction.data$recBiopsy <- biopsyVar

  return(prediction.data)

  #p <- ggplot(prediction.data, aes(x=age, y=col2)) + geom_point(aes(x=age, y=col2)) + geom_line(aes(x=age, y=col2))
  #p <- p + labs(title = "Percent Chance of Aggressive Cancer", x = "Age", y = "P(Aggressive Tumor)")

  #p <- p + scale_y_continuous(limits=c(0, 1)) + scale_x_continuous(limits=c(55, 85))



}


###






