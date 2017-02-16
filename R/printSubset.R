#' printIndividualData.R
#'
#' Print a specified patient's demographic, PSA, biopsy, and treatment results to the console
#'
#'
#' @param pt.id Integer of patient record to be displayed
#' @param pt Full
#' @export
printSubset<- function(pt.id = 5, pt = ptDataframes) {
  #this version creates one large dataframe as Scott requested
  pt.data <- pt[[1]] #global variable pt put into temporary dataframes with names matching RJAGS prep
  psa.data <- pt[[2]]
  bx.data <- pt[[3]]

  library(dplyr)
  library(knitr)

  #print("Demographics Data: ")
  formattedDemo <- dplyr::filter(pt.data, id == pt.id)
  formattedDemo$dob <- as.Date(formattedDemo$dob.num, origin="1970-01-01")
  #formattedDemo$dobCheck <-as.numeric(as.Date(formattedDemo$dob))
  formattedDemo <- formattedDemo[c("id", "age.dx", "dob", "vol.avg")]



  closest100 <- c(5, 10, 20, 100, 200, 205) #should be a real function to find closest K patients to pt.id


  #print("Demographics Data: ")
  formattedPsa <- filter(psa.data, id %in% closest100)
  formattedPsa$visit <- as.Date(formattedPsa$psa.date.num, origin = "1970-01-01")
  formattedPsa <- formattedPsa[c("id", "age", "visit", "psa")]#add age
  #formattedPsa$visit <- as.Date(formattedPsa$psa.date, origin = "1970-01-01")
  names(formattedPsa) <- c("ID", "Age", "Visit", "PSA")


  formattedBx <- dplyr::filter(bx.data, id %in% closest100, bx.here == 1)
  formattedBx$visit <- as.Date(formattedBx$bx.date.num, origin = "1970-01-01")
  formattedBx <- formattedBx[c("id", "visit", "bx.age", "rc", "surgery")]
  names(formattedBx) <- c("ID", "Visit", "Age", "Biopsy", "Surgery")
  #formattedBxnoSurg <- formattedBx[c("visit", "bx.age", "bx.here")]
  #formattedSurg <- formattedBx[c("visit", "bx.age", "surgery")]
  #names(formattedBxnoSurg) <- c("Visit", "Age", "Biopsy")
  #names(formattedSurg) <- c("Visit", "Age", "Surgery")



  merged.data <- merge(formattedPsa, formattedBx, by=c("ID", "Age", "Visit"), all = TRUE)



  #print(knitr::kable(merged.data, align = 'c'))



}
