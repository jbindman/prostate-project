#' closestK
#'
#' Find the distance between two patients
#'
#'
#' @param pt.id Integer of patient record to be displayed
#' @param K Number of nearby patients to return
#' @param D Max distance between comparable patients
#' @export
closestK <- function(pt.id = 5, K = 50, D = 1) {
  (n<-dim(demo.data)[1]) #1000 patients in this data
  pt.data$ptDistance <- vector(length=n)
  for (i in pt.data$id) {
    pt.data$ptDistance[i] <- distance(i, pt1)
  }
  #### for certain # K patients
  arrange(pt.data, desc(ptDistance)) #arrange in descending order
  pt.data[1:K,]

  ### within certain distance D
  filter(pt.data, ptDistance < D)
  # return certain rows of pt.data
  # use this in plotting to limit scope to patient id's included in this subset

#do i need to see RJAGS return
}
