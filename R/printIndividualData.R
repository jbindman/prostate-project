#' printIndividualData.R
#'
#' Print a specified patient's demographic, PSA, biopsy, and treatment results to the console
#'
#'
#' @param pt.id Integer of patient record to be displayed
#' @param pt Full
#' @export
printIndividualData<- function(pt.id = 5, pt) {
  pt.data <- pt[[1]] #global variable pt put into temporary dataframes with names matching RJAGS prep
  psa.data <- pt[[2]]
  #bx.data <- bx_data
  bx.data <- pt[[3]]
  #  bx.data <- bx_data


  #lapply(psa.data, class)


  #pt.data[pt.data$id == patient,] #actually works and is equiv #pt[[1]][pt[[1]]$id == patient,]
  #psa.data[psa.data$id == patient,]
  #bx.data[bx.data$id == patient,]
  #library(dplyr)

  #print("Demographics Data: ")
  formattedDemo <- filter(pt.data, id == pt.id)
  formattedDemo$dob <- as.Date(formattedDemo$dob.num, origin="1970-01-01")
  #formattedDemo$dobCheck <-as.numeric(as.Date(formattedDemo$dob))
  formattedDemo <- formattedDemo[c("id", "age.dx", "dob", "vol.avg")]
  names(formattedDemo) <- c("Patient      ", "Age Dx     ", "DOB       ", "Avg Prostate Volume")
  #print("Patient Id")
  #print(formattedDemo$Patient)
  #print("Age at Diagnosis")
  #print(formattedDemo$'Age at Dx')
  #print("DOB")
  #print(formattedDemo$DOB)
  #print("Average Prostate Volume")
  #print(formattedDemo$'Avg Vol')
  demoAge <- as.numeric(formattedDemo$`Age Dx     `)
  ageSmall <- round(demoAge, digits = 0)
  formattedDemo$`Age Dx     ` <- ageSmall
  demoVol <- as.numeric(formattedDemo$`Avg Prostate Volume`)
  volSmall <- round(demoVol, digits = 0)
  formattedDemo$`Avg Prostate Volume` <- volSmall
  print(knitr::kable(formattedDemo, align = 'c'))

  formattedPsa <- filter(psa.data, id == pt.id)
  formattedPsa$visit <- as.Date(formattedPsa$psa.date.num, origin = "1970-01-01")
  formattedPsa <- formattedPsa[c("age", "visit", "psa")]#add age
  #formattedPsa$visit <- as.Date(formattedPsa$psa.date, origin = "1970-01-01")
  names(formattedPsa) <- c("Age", "Visit", "PSA")


  formattedBx <- filter(bx.data, id == pt.id, bx.here == 1)
  #for (i in 1:nrow(formattedBx)) {
  #  bxDate <- as.Date(formattedBx$bx.date[i])
  #  formattedBx$ageNum[i] <- bxDate - formattedDemo$DOB
  #  formattedBx$age[i] <- formattedBx$ageNum[i]/365
  #}
  formattedBx$visit <- as.Date(formattedBx$bx.date.num, origin = "1970-01-01")
  formattedBx <- formattedBx[c("visit", "bx.age", "rc", "surgery")]
  names(formattedBx) <- c("Visit", "Age", "Biopsy", "Surgery")
  #formattedBxnoSurg <- formattedBx[c("visit", "bx.age", "bx.here")]
  #formattedSurg <- formattedBx[c("visit", "bx.age", "surgery")]
  #names(formattedBxnoSurg) <- c("Visit", "Age", "Biopsy")
  #names(formattedSurg) <- c("Visit", "Age", "Surgery")



  merged.data <- merge(formattedPsa, formattedBx, by=c("Visit", "Age"), all = TRUE)

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
    #volVal <- as.numeric(merged.data$Vol[i])
    #volSmall <- round(volVal, digits = 1)
    #merged.data$Vol[i] <- volSmall

    #if(is.na(merged.data$Vol[i])) {
    #  merged.data$Vol[i] <- "-"
    #}

    #if (merged.data$RC[i] == 0) {
    #  merged.data$RC[i] <- "N"
    #}
    #else if (merged.data$RC[i] == 1) {
    #  merged.data$RC[i] <- "Y"
    #}
    if(is.na(merged.data$Age[i])) {
      merged.data$Age[i] <- "-"
    }

    if(is.na(merged.data$PSA[i])) {
      merged.data$PSA[i] <- "-"
    }
    if(is.na(merged.data$Biopsy[i])) {
      merged.data$Biopsy[i] <- "-"
    }
    if (merged.data$Biopsy[i] == 1) {
      merged.data$Biopsy[i] <- "Gleason > 6"
    }
    if (merged.data$Biopsy[i] == 0) {
      merged.data$Biopsy[i] <- "Reclassification = 0"
    }
    if(is.na(merged.data$Surgery[i])) {
      merged.data$Surgery[i] <- "-"
    }
    if(merged.data$Surgery[i] == 0) {
      merged.data$Surgery[i] <- "-"
    }
    if (merged.data$Surgery[i] == 1) {
      merged.data$Surgery[i] <- "Y"
    }

    #merged.data$PSA[i]
  }



  names(merged.data) <- c("Visit Date", "Patient Age", "PSA value", "Biopsy", "Surgery")
  print(knitr::kable(merged.data, align = 'c'))

  #ages are messed up, thinks biopsy at later time has younger patient age

  #find differences between IOP and non IOP versions and add T/F option

}
