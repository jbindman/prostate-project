#' printIndividualData.R
#'
#' Print a specified patient's demographic, PSA, biopsy, and treatment results to the console
#'
#'
#' @param idInput Integer of patient record to be displayed
#' @param patientDataframes Full
#' @export
printIndividualData<- function(idInput = 5, patientDataframes, bx_data) {
  pt.data <- patientDataframes[[1]] #global variable patientDataframes put into temporary dataframes with names matching RJAGS prep
  psa.data <- patientDataframes[[2]]
  #bx.data <- bx_data
  bx.full <- patientDataframes[[3]]
  #  bx.data <- bx_data




  lapply(psa.data, class)


  #pt.data[pt.data$id == patient,] #actually works and is equiv #patientDataframes[[1]][patientDataframes[[1]]$id == patient,]
  #psa.data[psa.data$id == patient,]
  #bx.data[bx.data$id == patient,]
  #library(dplyr)

  #print("Demographics Data: ")
  formattedDemo <- filter(pt.data, id == idInput)
  formattedDemo$dob <- as.Date(formattedDemo$dob.num, origin="1970-01-01")
  #formattedDemo$dobCheck <-as.numeric(as.Date(formattedDemo$dob))
  formattedDemo <- formattedDemo[c("id", "age.dx", "vol.avg", "dob")]
  names(formattedDemo) <- c("Patient", "Age at Dx", "Avg Vol", "DOB")
  print(formattedDemo)

  formattedPsa <- filter(psa.data, id == idInput)
  formattedPsa <- formattedPsa[c("age", "psa.date", "psa")]#add age
  names(formattedPsa) <- c("Age", "Visit", "PSA")




  formattedBx <- filter(bx.data, id == idInput)
  #add age

  for (i in 1:nrow(formattedBx)) {
    bxDate <- as.Date(formattedBx$bx.date[i])
    formattedBx$ageNum[i] <- bxDate - formattedDemo$DOB
    formattedBx$age[i] <- formattedBx$ageNum[i]/365
  }
  formattedBx <- formattedBx[c("bx.date", "vol", "age")]
  names(formattedBx) <- c("Visit", "Vol", "Age")



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






#
#   #subset PSA data
#   psa.data.i<-psa.data[psa.data$id==idInput,]
#
#   #subset bx data
#   pt.subj<-pt.data$subj[pt.data$id==idInput] #if you go back and add id to the bx.full dataframe, we don't need this step
#   bx.data.i<-bx.full[bx.full$subj==idInput & bx.full$bx.here==1 & !is.na(bx.full$bx.here),]
#
#   psa.data.i <- psa.data.i[c("age", "psa.date", "psa")]
#   names(psa.data.i) <- c("Age", "Visit", "PSA")
#   print(psa.data.i)
#
#   #as.Date(psa.data.i$Visit)
#   #psa.data.i$Visitnum<-as.numeric(as.Date(psa.data.i$Visit))
#   #psa.data.i$Visitnum
#   #as.Date(psa.data.i$Visitnum, origin="1970-01-01")
#
#
#   as.Date(bx.data.i$int.date.num, origin="1970-01-01")
#   bx.data.i$Visit <- as.Date(bx.data.i$int.date.num, origin="1970-01-01")
#   bx.data.i <- bx.data.i[c("int.age", "bx.here", "Visit")]
#   names(bx.data.i) <- c("Age", "Biopsy", "Visit")
#   print(bx.data.i)
#
#   # #merged.data$Patient <- NULL #dont need to show anymore
#    merged <- merge(psa.data.i, bx.data.i, by=c("Age"), all = TRUE)
  # for (i in 1:nrow(merged)) {
  #   #adjusting PSA to 2 decimals
  #   PSAval <- as.numeric(merged$PSA[i])
  #   PSAsmall <- round(PSAval, digits = 2)
  #   merged$PSA[i] <- PSAsmall
  #   #adjusting vol to 2 decimals
  #   ageVal <- as.numeric(merged$Age[i])
  #   ageSmall <- round(ageVal, digits = 1)
  #   merged$Age[i] <- ageSmall
  #   #volVal <- as.numeric(merged.data$Vol[i])
  #   #volSmall <- round(volVal, digits = 1)
  #   #merged.data$Vol[i] <- volSmall
  #
  #   #if(is.na(merged.data$Vol[i])) {
  #   #  merged.data$Vol[i] <- "-"
  #   #}
  #
  #   #if (merged.data$RC[i] == 0) {
  #   #  merged.data$RC[i] <- "N"
  #   #}
  #   #else if (merged.data$RC[i] == 1) {
  #   #  merged.data$RC[i] <- "Y"
  #   #}
  #   if(is.na(merged$Age[i])) {
  #     merged$Age[i] <- "-"
  #   }
  #   #if(is.na(merged.data$Age2[i])) {
  #   #  merged.data$Age2[i] <- "-"
  #   #}
  #   if(is.na(merged$PSA[i])) {
  #     merged$PSA[i] <- "-"
  #   }
  #   if(is.na(merged$Biopsy[i])) {
  #     merged$Biopsy[i] <- "-"
  #   }
  #
  #   #merged.data$PSA[i]
  # }
  #
  # #for (i in 1:nrow(merged.data)) {
  # #  if(merged.data$Biopsy[i] == 1) {
  # #    merged.data$Biopsy[i] <-"Yes"
  # #  }
  # #  if(merged.data$Biopsy[i] == 0) {
  # #    merged.data$Biopsy[i] <-"No"
  # #  }
  # #}



#still need to add ages. visit date - dob = age, make new column and delete column from PSA age (but use this to test)
  names(merged.data) <- c("Visit Date", "Patient Age", "PSA value", "Volume")
  knitr::kable(merged.data, align = 'c')

  #ages are messed up, thinks biopsy at later time has younger patient age

  #find differences between IOP and non IOP versions and add T/F option

}
