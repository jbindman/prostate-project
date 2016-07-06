#' getIndividualData.R
#'
#' Print a specified patient's demographic, PSA, biopsy, and treatment results to the console
#'
#'
#' @param idInput Integer of patient record to be displayed
#' @param patientDataframes Full
#' @export
getIndividualData<- function(idInput = 3, patientDataframes, bx_data) {
  pt.data <- patientDataframes[[1]] #global variable patientDataframes put into temporary dataframes with names matching RJAGS prep
  psa.data <- patientDataframes[[2]]
  #bx.data <- patientDataframes[[3]]
  bx.data <- bx_data

  lapply(psa.data, class)


  #pt.data[pt.data$id == patient,] #actually works and is equiv #patientDataframes[[1]][patientDataframes[[1]]$id == patient,]
  #psa.data[psa.data$id == patient,]
  #bx.data[bx.data$id == patient,]
  #library(dplyr)

  #print("Demographics Data: ")
  #formattedDemo <- filter(pt.data, id == idInput)
  #formattedDemo$subj <- NULL
  #formattedDemo$total.fup <- NULL
  #change DOB
  #names(formattedDemo) <- c("Patient", "Gleason Score", "DOB", "Diagnosis Date", "Age at Diagnosis", "Avg Volume", "RC")
  #print(formattedDemo)

  #print("PSA Data: ")
  formattedPsa <- filter(psa.data, id == idInput)
  #formattedPsa <- arrange(formattedPsa, psa.date.num) #already in order but double check
  formattedPsa <- formattedPsa[c("psa", "psa.date", "age")]#add age
  names(formattedPsa) <- c("PSA", "Visit", "Age")
  #psa.data$Visit <- as.Date(psa.data$Visit[1:nrow(psa.data)], origin="1970-01-01")

  #print(formattedPsa)

  #print("Biopsy Data: ")
  #add VISIT back to biopsy in fillPatientTables
  formattedBx <- filter(bx.data, id == idInput)
  #formattedBx <- arrange(formattedBx, bx.date.num)
  formattedBx <- formattedBx[c("bx.date", "vol")]
  names(formattedBx) <- c("Visit", "Vol")
  #formattedBx$Visit <- as.Date(formattedBx$Visit[1:nrow(formattedBx)], origin="1970-01-01")
  #print(formattedBx)


  merged.data <- merge(formattedPsa, formattedBx, by=c("Visit"), all = TRUE)
  #merged.data[is.na(merged.data)] <- 0

  #all NA replaced with '-'
  for (i in 1:nrow(merged.data)) {
    #adjusting PSA to 2 decimals
    PSAval <- as.numeric(merged.data$PSA[i])
    PSAsmall <- round(PSAval, digits = 2)
    merged.data$PSA[i] <- PSAsmall
    #adjusting vol to 2 decimals
    ageVal <- as.numeric(merged.data$Age[i])
    ageSmall <- round(ageVal, digits = 1)
    merged.data$Age[i] <- ageSmall
    volVal <- as.numeric(merged.data$Vol[i])
    volSmall <- round(volVal, digits = 1)
    merged.data$Vol[i] <- volSmall

    if(is.na(merged.data$Vol[i])) {
      merged.data$Vol[i] <- "-"
    }

    #if (merged.data$RC[i] == 0) {
    #  merged.data$RC[i] <- "N"
    #}
    #else if (merged.data$RC[i] == 1) {
    #  merged.data$RC[i] <- "Y"
    #}
    if(is.na(merged.data$Age[i])) {
      merged.data$Age[i] <- "-"
    }
    #if(is.na(merged.data$Age2[i])) {
    #  merged.data$Age2[i] <- "-"
    #}
    if(is.na(merged.data$PSA[i])) {
      merged.data$PSA[i] <- "-"
    }
    #if(is.na(merged.data$Biopsy[i])) {
    #  merged.data$Biopsy[i] <- "-"
    #}

    #merged.data$PSA[i]
  }


  #for (i in 1:nrow(merged.data)) {
  #  if(merged.data$Biopsy[i] == 1) {
  #    merged.data$Biopsy[i] <-"Yes"
  #  }
  #  if(merged.data$Biopsy[i] == 0) {
  #    merged.data$Biopsy[i] <-"No"
  #  }
  #}
  #merged.data$Patient <- NULL #dont need to show anymore




  #print(merged.data)

  #ages are messed up, thinks biopsy at later time has younger patient age

  #find differences between IOP and non IOP versions and add T/F option

}
