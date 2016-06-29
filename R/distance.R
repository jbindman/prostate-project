#' distance
#'
#' Find the distance between two patients
#'
#'
#' @param pt1 Patient 1
#' @param pt2 Patient 2
#' @export
distance <- function(pt1 = 5, pt2 = 38) {
  age1 <- filter(pt.data, id == pt1)$age.dx
  age2 <- filter(pt.data, id == pt2)$age.dx
  gleason1 <- filter(bx_data, id == pt1, dx == 1)$RC
  gleason2 <- filter(bx_data, id == pt2, dx == 1)$RC

  for (i in 1:100) {
    g <- filter(bx_data, id == i, dx == 1)$RC #not what i want
    print(g)
  }
}
