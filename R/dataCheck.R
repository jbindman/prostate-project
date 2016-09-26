#' dataCheck.R
#'
#' Check if loaded patient data has values within acceptable range. See vignette for restrictions on data.
#'
#' @param demo.data Patient-level data. This dataframe should contain one record (row) per patient. Variables (columns) must include a unique identifier (ID) and valid date of birth for each patient.
#' @param psa.data PSA data. This dataframe should contain one record per PSA test (per patient). Variables must include patient ID, date of test, and total PSA observed.
#' @param bx.data Biopsy data. This dataframe should contain one record per biopsy (per patient). Variables must include patient ID, date of biopsy, indicator of grade reclassication, and prostate volume (if assessed).
#' @param surg.data Surgery data. This dataframe should contain one record per surgery performed. Variables must include patient ID, surgery date, and Gleason score (GS) assessed on entire specimen.
#' @export

dataCheck <- function (demo.data = demo_data, psa.data = psa_data, bx.data = bx_data, surg.data = surg_data) {
  #ADD NEW CHECKS
  library(dplyr)
  #checks demo.data
  for (i in demo.data$id) {
    if (i < 0) {
      stop ("Patients must have positive dimensions for ID in demo_data")
    }
  }
  #### NEW FOR IOP
  #check for demo.data: status.rc, status.tx, censor.dt.rc, censor.dt.tx
  # any other checks needed for other dataframes?



  #checks psa.data
  for (i in psa.data$id) {
    if (i < 0) {
      stop ("Patients must have positive dimensions for ID in psa_data")
    }
  }
  for (i in psa.data$psa) {
    if (i < 0) {
      stop ("Patients must have positive value for psa value in psa_data")
    }
  }

  #checks bx.data
  for (i in bx.data$id) {
    if (i < 0) {
      stop ("Patients must have positive value for id value in bx_data")
    }
  }
  for (i in bx.data$RC) {
    if (i != 0 && i !=1) {
      stop ("Patients must have positive value for id value in bx_data")
    }
  }
  for (i in bx.data$vol) {
    if (i < 0) {
      stop ("Patients must have positive value for vol in bx_data")
    }
  }

  for (i in bx.data$dx) {
    if (i < 0) {
      stop ("Patients must have 0 or positive value for dx in bx_data")
    }
  }


  #checks surg.data
  for (i in surg.data$id) {
    if (i < 0) {
      stop ("Patients must have positive value for id value in surg_data")
    }
  }
  # add back
  invalidGS <-filter(surg.data, GS != 1 & GS != 0)
  if (nrow(invalidGS) != 0) {
    stop ("Patients must have post-surgery gleason score of 0 or 1")
  }


  #-each patient (id) in demo.data must have at least one record in psa.data
  for (i in demo.data$id) {
    contains <- i %in% psa.data$id
    if (contains == FALSE) {
      stop ("Patient ID #", i ," missing psa data")
    }
  }
  #-each patient (id) in demo.data must have at least one record in bx.data
  for (i in demo.data$id) {
    contains <- i %in% bx.data$id
    if (contains == FALSE) {
      stop ("Patient ID #", i ," missing biopsy data")
    }
  }



  # will only get to return if all checks passed
  return(TRUE)
}
