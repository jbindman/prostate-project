#' closestK
#'
#' Find the distance between two patients
#'
#'
#' @param pt.id Integer of patient record to be displayed
#' @param K Number of nearby patients to return
#' @param D Max distance between comparable patients
#' @export
closestK <- function(ptId = 6, K = 50, D = 1, patientDataframes, bx_data) {
  (n<-dim(demo.data)[1]) #1000 patients in this data
  pt.data <- patientDataframes[[1]]
  pt.data$ptDistance <- vector(length = nrow(pt.data))
  for (i in pt.data$id) {
    pt.data$ptDistance[i] <- distance(70, i, patientDataframes, bx_data)[1]
  }
  #### for certain # K patients
  all <- arrange(pt.data, desc(ptDistance)) #arrange in descending order
  pt.data[1:K,]

  ### within certain distance D
  filter(pt.data, ptDistance < D)
  # return certain rows of pt.data
  # use this in plotting to limit scope to patient id's included in this subset

#do i need to see RJAGS return
}
