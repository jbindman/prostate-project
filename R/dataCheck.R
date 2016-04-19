#' Check if data has values within specified range
#'
#' @param tx.data one record per treatment received per patient
#' @param demo.data demographic data, one record per patient
#' @param psa.data psa data, one record per PSA test per patient
#' @param bx.data one record per biopsy per patient
#' @return true if error was caught, false if not
#' @export

dataCheck <- function (tx.data, demo.data, psa.data, bx.data) {
  #-must have positive dimension for demo.data, psa.data, bx.data, and tx.data

  #-each patient in demo.data must have at least one record in psa.data, bx.data

  #-all surgery patients must have post-surgery gleason score (we may lift this restriction). these must be reported at 0 or 1.


  #-each patient needs date of birth
  #-all biopsy records must have a date
  #-all patients must have diagnostic biopsy indicated in bx.data
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
