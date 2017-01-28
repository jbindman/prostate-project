#' distance
#'
#' Find the distance between two patients
#'
#' @param ptId Patient 1
#' @param comp Patient 2, to be compared to
#' @param distDataframe Dataframe of variables to be compared in distance function
#' @export
distance <- function(ptId, comp, distDataframe, stdev) {
  #pass in stdev
  x1 <- c(distDataframe$ageDx[distDataframe$id == ptId], distDataframe$loft[distDataframe$id == ptId],
          (as.numeric(distDataframe$calDx[distDataframe$id == ptId])))
  x2 <- c(distDataframe$ageDx[distDataframe$id == comp], distDataframe$loft[distDataframe$id == comp],
          (as.numeric(distDataframe$calDx[distDataframe$id == comp])))

  dis <- getDistance(x1, x2, stdev)
  return(dis)
}

getDistance <- function(x1, x2, stdev) {
    disVector <- 0
    #disVector = (x1-x2)^2/stdev
    #doesnt become normalized, calDx is still much larger contirbutor to distance
    #disVector[3] <- disVector[3]/100 #trying to adjust scale
    return (sum(disVector))
  }
getSd <- function(distDataframe) {
  ageSd <- sd(distDataframe$ageDx, na.rm = FALSE)
  loftSd <- sd(distDataframe$loft, na.rm = FALSE)
  calSd <- sd(as.numeric(distDataframe$calDx), na.rm = FALSE)
  sd <- c(ageSd, loftSd, calSd)
  return (sd)
}

getVar <- function(distDataframe) {
  ageVar <- var(distDataframe$ageDx, na.rm= FALSE)
  loftVar <- var(distDataframe$loft, na.rm = FALSE)
  calVar <- var(as.numeric(distDataframe$calDx), na.rm = FALSE)

  variance <- c(ageVar, loftVar, calVar)
  cov(distDataframe$ageDx, distDataframe$loft)
  #cov(distDataframe$calDx, distDataframe$loft)
  #cov(distDataframe$ageDx, distDataframe$calDx)
}

