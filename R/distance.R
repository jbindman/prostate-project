#' distance
#'
#' Find the distance between two patients
#'
#'
#' @param pt1 Patient 1
#' @param pt2 Patient 2
#' @export
distance <- function(ptId = 6, comp = 45, patientDataframes, bx_data) {
  pt.data <- patientDataframes[[1]]
  ageDxPtId <- filter(pt.data, id == ptId)$age.dx
  ageDxComp <- filter(pt.data, id == comp)$age.dx
  ageDiff <- ageDxPtId - ageDxComp #can be neg or pos
  stdevAge <- sd(pt.data$age.dx, na.rm = FALSE) #standard deviation of ages, look through demo.data

  ptMerged <- getIndividualData(ptId, patientDataframes, bx_data) #should make getIndividualData function
  loftPtId <- ptMerged$Visit[nrow(ptMerged)][1] # substract with dates: - ptMerged$Visit[1]
  compMerged <- getIndividualData(comp, patientDataframes, bx_data)
  #loftComp <- #subtract
  #loft <- loftPtId - loftComp #will already be in years
  #standard deviation comes from all merged dataframes, lots of computation. another way to do this, pt.data?


  calDateNumPtId <- filter(patientDataframes[[1]],id == ptId)$dx.date.num
  calDateNumComp <- filter(patientDataframes[[1]],id == comp)$dx.date.num
  calDateDiff <- calDateNumPtId - calDateNumComp #can be neg or positive
  #needs to be converted from this seemingly arbitrary integer into years --> how
  stdevCalDate <- sd(patientDataframes[[1]]$dx.date.num, na.rm = FALSE)

  dis <- (calDateDiff/stdevCalDate)^2
  return(dis)
  #to save on code, return these comparisons within a loop, but do the standarddeviation separately so it doesnt need to be calculated each time. or does this not matter
}
