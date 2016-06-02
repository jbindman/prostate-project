#' dataCheck.R
#'
#' Check if loaded patient data has values within acceptable range.
#'
#' @param surg.data One record per treatment received per patient containing treatment date and GS
#' @param demo.data Demographic data, one record per patient ID containing DOB
#' @param psa.data PSA data, one record per PSA test per patient containing date of PSA test
#' @param bx.data One record per biopsy per patient containing reclassicfication, volume, and dx
#' @export

#any additions for IOP?
dataCheck <- function (surg.data = surg_data, demo.data = demo_data, psa.data = psa_data, bx.data = bx_data) {
  #ADD NEW CHECKS
  library(dplyr)
  #checks demo.data
  for (i in demo.data$id) {
    if (i < 0) {
      stop ("Patients must have positive dimensions for ID in demo_data")
    }
  }

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
  #invalidGS <-filter(surg.data, GS != 1 & GS != 0)
  #if (nrow(invalidGS) != 0) {
  #  stop ("Patients must have post-surgery gleason score of 0 or 1")
  #}


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
