#' printIndividualData.R
#'
#' Print a specified patient's demographic, PSA, biopsy, and treatment results to the console
#'
#'
#' @param idInput Integer of patient record to be displayed
#' @export
printIndividualData<- function(idInput = 3, patientDataframes = ptDataframes) {
  pt.data <- patientDataframes[[1]] #global variable patientDataframes put into temporary dataframes with names matching RJAGS prep
  psa.data <- patientDataframes[[2]]
  bx.data <- patientDataframes[[3]]

  lapply(psa.data, class)


  #pt.data[pt.data$id == patient,] #actually works and is equiv #patientDataframes[[1]][patientDataframes[[1]]$id == patient,]
  #psa.data[psa.data$id == patient,]
  #bx.data[bx.data$id == patient,]
  library(dplyr)

  print("Demographics Data: ")
  formattedDemo <- filter(pt.data, id == idInput)
  formattedDemo$subj <- NULL
  formattedDemo$total.fup <- NULL
  #change DOB
  names(formattedDemo) <- c("Patient", "Gleason Score", "DOB", "Diagnosis Date", "Age at Diagnosis", "Avg Volume", "RC")
  print(formattedDemo)

  print("PSA Data: ")
  formattedPsa <- filter(psa.data, id == idInput)
  formattedPsa <- arrange(formattedPsa, psa.date.num) #already in order but double check
  formattedPsa <- formattedPsa[c("id", "psa", "psa.date.num")]
  #names(formattedPsa) <- c("PSA Value", "Date of PSA", "psa.date.num", "Age")
  print(formattedPsa)

  print("Biopsy Data: ")
  #add id back to biopsy in fillPatientTables
  formattedBx <- filter(bx.data, id == idInput)
  formattedBx <- filter(formattedBx, bx.here == 1)
  formattedBx <- arrange(formattedBx, bx.date.num)
  formattedBx <- formattedBx[c("id", "bx.age", "bx.date.num")]
  print(formattedBx)

  #merged.data <- merge(formattedPsa, formattedBx, by.x="psa.date.num", by.y="bx.date.num", all = TRUE)
  #now format to make one id column, all relevant data, and replace NA with -
}
