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
  # to do later: DOB checks



  #-must have positive dimension for demo.data, psa.data, bx.data, and tx.data
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
  #### change dates later






  #-each patient in demo.data must have at least one record in psa.data, bx.data
  for (i in demo.data$id) {
    i <- 5000
    contains <- i %in% psa.data$id
    if (contains == FALSE) {
      stop ("Patient ID #", i ," missing psa data")
    }
  }
  for (i in demo.data$id) {
    i <- 5000
    contains <- i %in% bx.data$id
    if (contains == FALSE) {
      stop ("Patient ID #", i ," missing biopsy data")
    }
  }



  #-all surgery patients must have post-surgery gleason score (we may lift this restriction). these must be reported at 0 or 1.
  #-each patient needs date of birth
  #-all biopsy records must have a date
  bad <- filter(bx_data, is.na(bx_data$bx.date))

  #-all patients must have diagnostic biopsy indicated in bx.data
  # iterate through bx make sure each has a diagnostic biopsy = 1

  #-all patients must have age at diagnosis above 0. (I actually think it should probably be above 35.)
  #-all patients must have at least one volume measurement in the biopsy data
  #-all PSA observations must be 0 or higher; none can be missing
  #-all PSA observations must have a date
  #-all biopsies must have reclassification information. this should be reported as 0 or 1.


  #-all treatment records must have a date
  if (!is.na(filter(tx_data, is.na(tx.date)))) { #not real but dyplyr example
    stop ("tx_data values must all have a date")
  }

  for (i in tx_data$tx.date) { #iteration example
    if (is.na(i)) {
      stop ("tx_data values must all have a date")
    }
  }


  # if (psa_data < 0) {
  # stop ("psa_data must be non negative")
  # }
  #
  #

}
