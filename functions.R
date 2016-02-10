loadData <- function() {
  toRead <- "julia-demo-data.csv"
  demo.data<-read.csv(toRead)
  names(demo.data)
}
