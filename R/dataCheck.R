#' Check if data has values within specified range
#'
#' @param tx.data one record per treatment received per patient
#' @param demo.data demographic data, one record per patient
#' @param psa.data psa data, one record per PSA test per patient
#' @param bx.data one record per biopsy per patient
#' @export

dataCheck <- function (tx.data, demo.data, psa.data, bx.data) {

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


  #checks tx.data
  for (i in tx.data$id) {
    if (i < 0) {
      stop ("Patients must have positive value for id value in tx_data")
    }
  }
  invalidGS <-filter(tx.data, GS != 1 & GS != 0)
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

}
