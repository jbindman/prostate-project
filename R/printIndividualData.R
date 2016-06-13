#' printIndividualData.R
#'
#' Print a specified patient's demographic, PSA, biopsy, and treatment results to the console
#'
#'
#' @param idInput Integer of patient record to be displayed
#' @export
printIndividualData<- function(idInput = 10, patientDataframes = ptDataframes) {
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
  formattedPsa <- formattedPsa[c("id", "psa", "psa.date.num", "age")]#add age
  names(formattedPsa) <- c("Patient", "PSA", "Visit", "Age")
  print(formattedPsa)

  print("Biopsy Data: ")
  #add id back to biopsy in fillPatientTables
  formattedBx <- filter(bx.data, id == idInput)
  formattedBx <- filter(formattedBx, bx.here == 1)
  formattedBx <- arrange(formattedBx, bx.date.num)
  formattedBx <- formattedBx[c("id", "bx.here", "bx.date.num", "bx.age")]
  names(formattedBx) <- c("Patient", "Biopsy", "Visit", "Age")
  print(formattedBx)


  merged.data <- merge(formattedPsa, formattedBx, by=c("Patient","Visit", "Age"), all = TRUE)
  #merged.data[is.na(merged.data)] <- 0

  #all NA replaced with '-'
  for (i in 1:nrow(merged.data)) {
    if(is.na(merged.data$Biopsy[i])) {
      merged.data$Biopsy[i] <- "-"
    }
    if(is.na(merged.data$PSA[i])) {
      merged.data$PSA[i] <- "-"
    }
  }
  #for (i in 1:nrow(merged.data)) {
  #  if(merged.data$Biopsy == 1) {
  #    merged.data$Biopsy <-"Yes"
  #  }
  #}
  merged.data$Visit <- as.Date(merged.data$Visit[1:nrow(merged.data)], origin="1970-01-01")
  merged.data$Patient <- NULL #dont need to show anymore




  print(merged.data)

  #ages are messed up, thinks biopsy at later time has younger patient age

}
