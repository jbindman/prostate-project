#' closestK
#'
#' Find the distance between two patients
#'
#'
#' @param pt.id Integer of patient record to be displayed
#' @param K Number of nearby patients to return
#' @param D Max distance between comparable patients
#' @export
closestK <- function(ptId = 6, pt, K = 25, D = 1) {

  pt.data <-pt[[1]]
  #pt.data <- patientDataframes[[1]]
  pt.data$ptDistance <- vector(length = nrow(pt.data))

  #make a dataframe with one entry per person, containing age, length of follow up, and cal date dx
  distDataframe<-as.data.frame(pt.data$id)
  names(distDataframe) <- "id"
  distDataframe$dob <- as.Date(pt.data$dob.num,  origin="1970-01-01")
  distDataframe$ageDx <- pt.data$age.dx
  distDataframe$calDx <- as.Date(pt.data$dx.date.num, origin="1970-01-01")
  #slow, see if follow up time can be calculated faster
  for (i in distDataframe$id) {
    ptMerged <- getIndividualData(i, ptDataframes)
    distDataframe$loft[distDataframe$id==i] <- ptMerged$`Patient Age`[nrow(ptMerged)]- distDataframe$ageDx[distDataframe$id==i] #for each patient, go through their merged dataframe and subtract last test date from diagnostic date, then convert to year
    #error in simulated data? shouldnt be any negative loft

  }

  #compare distances for all patients except ptId
  for (i in 1:nrow(pt.data)) {
    if(pt.data$ptDistance[pt.data$id] != ptId) {
      pt.data$ptDistance[pt.data$id==i] <- distance(ptId, i, distDataframe)

    }
  }
  #### for certain # K patients
  ordered <- arrange(pt.data, ptDistance) #arrange in descending order
  return(ordered[1:K,]$id)

  ### OR within certain distance D
  ordered <- arrange(pt.data, ptDistance)
  filter(ordered, ptDistance < D)
  return(filter(ordered, ptDistance < 1)$id)
  #return vector of ids
  #remove first id which will always be ptId as distance = 0
}
