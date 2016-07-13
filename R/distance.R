#' distance
#'
#' Find the distance between two patients
#'
#'
#' @param pt1 Patient 1
#' @param pt2 Patient 2
#' @export
distance <- function(ptId = 6, comp = 45, patientDataframes, distDataframe) {


  stdev <- getSd()
  x1 <- c(distDataframe$ageDx[distDataframe$id == ptId], distDataframe$loft[distDataframe$id == ptId], (as.numeric(distDataframe$calDx[distDataframe$id == ptId])))
  x2 <- c(distDataframe$ageDx[distDataframe$id == comp], distDataframe$loft[distDataframe$id == comp], (as.numeric(distDataframe$calDx[distDataframe$id == comp])))

  dis <- getDistance(x1, x2, stdev)
  return(dis)
}

getSd <- function() {
  ageSd <- sd(distDataframe$ageDx, na.rm = FALSE)
  loftSd <- sd(distDataframe$loft, na.rm = FALSE)
  calSd <- sd(distDataframe$calDx, na.rm = FALSE)/365
  sd <- c(ageSd, loftSd, calSd)
  return (sd)
}

getDistance <- function(x1, x2, stdev) {
    dis = (x1-x2)^2/stdev
    #fix date
    dis[3] <- ((x1[3] - x2[3]) / 365)^2/stdev[3]

    return (sum(dis))
  }
