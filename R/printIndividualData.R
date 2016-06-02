#' printIndividualData.R
#'
#' Print a specified patient's demographic, PSA, biopsy, and treatment results to the console
#'
#'
#' @param idInput Integer of patient record to be displayed
#' @export
printIndividualData<- function(idInput) {
  pt.data <- patientDataframes[[1]] #global variable patientDataframes put into temporary dataframes with names matching RJAGS prep
  psa.data <- patientDataframes[[2]]
  bx.data <- bx_data #uses original global dataframe, not formatted bx.full



  #figure out columns to omit
  psa.data$X <- NULL
  bx.data$X <- NULL




  #pt.data[pt.data$id == patient,] #actually works and is equiv #patientDataframes[[1]][patientDataframes[[1]]$id == patient,]
  #psa.data[psa.data$id == patient,]
  #bx.data[bx.data$id == patient,]
  library(dplyr)
  print("Demographics Data: ")
  print(filter(pt.data, id == idInput))
  print("PSA Data: ")
  print(filter(psa.data, id == idInput))
  print("Biopsy Data: ")
  print(filter(bx.data, id == idInput))


}
