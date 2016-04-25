#' Check if data has values within specified range
#'
#' @param tx.data one record per treatment received per patient
#' @param demo.data demographic data, one record per patient
#' @param psa.data psa data, one record per PSA test per patient
#' @param bx.data one record per biopsy per patient
#' @return true if error was caught, false if not
#' @export

dataCheck <- function (tx.data, demo.data, psa.data, bx.data) {
  #some data checks in this function, some after the dataframes have been combined
  # to do later: date checks



  #-must have positive dimension for demo.data, psa.data, bx.data, and tx.data DONE
  for (i in demo.data$X) {
    if (i < 0) {
      stop ("Patients must have positive dimensions for X in demo_data")
    }
  }
  for (i in demo.data$id) {
    if (i < 0) {
      stop ("Patients must have positive dimensions for ID in demo_data")
    }
  }
  #### checking dob in demo.data after its been changed
  for (i in psa.data$X) {
    if (i < 0) {
      stop ("Patients must have positive dimensions for X in psa_data")
    }
  }
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
  #### checking dob in psa.data after its been changed
  for (i in bx.data$X) {
    if (i < 0) {
      stop ("Patients must have positive value for X value in bx_data")
    }
  }
  for (i in bx.data$id) {
    if (i < 0) {
      stop ("Patients must have positive value for id value in bx_data")
    }
  }
  #### checking dob in bx.data after its been changed
  #### how to check bx.data RC

  #-all patients must have at least one volume measurement in the biopsy data
  #for (i in bx.data$vol) {
  #  if (i < 0) {
  #    stop ("Patients must have positive value for vol in bx_data")
  #  }
  #}

  for (i in bx.data$dx) {
    if (i < 0) {
      stop ("Patients must have 0 or positive value for dx in bx_data")
    }
  }
  for (i in tx.data$X) {
    if (i < 0) {
      stop ("Patients must have positive value for X value in tx_data")
    }
  }
  for (i in tx.data$id) {
    if (i < 0) {
      stop ("Patients must have positive value for id value in tx_data")
    }
  }
  for (i in tx.data$GS) {
    if (i < 0) {
      stop ("Patients must have positive value for GS value in tx_data")
    }
  }
  #### change dates later for the rest of tx.data


  #-each patient (id) in demo.data must have at least one record in psa.data, bx.data
  for (i in demo.data$id) {
    #i <- 5000 check
    contains <- i %in% psa.data$id
    if (contains == FALSE) {
      stop ("Patient ID #", i ," missing psa data")
    }
  }
  for (i in demo.data$id) {
    #i <- 5000 check
    contains <- i %in% bx.data$id
    if (contains == FALSE) {
      stop ("Patient ID #", i ," missing biopsy data")
    }
  }



  #-all surgery patients must have post-surgery gleason score (we may lift this restriction). these must be reported at 0 or 1.
  invalidGS <-filter(tx.data, GS != 1 & GS != 0)
  if (nrow(invalidGS) != 0) {
    stop ("Patients must have post-surgery gleason score of 0 or 1")
  }

  #-each patient needs date of birth --> check date, later
  #-all biopsy records must have a date --> check date, later
  #-all patients must have diagnostic biopsy indicated in bx.data --> check date, later
  #-all patients must have age at diagnosis above 0. (I actually think it should probably be above 35.)  --> check date, later
  #-all PSA observations must have a date --> check date, later
  #-all biopsies must have reclassification information. this should be reported as 0 or 1.


  #-all treatment records must have a date -->  check date, later




}
