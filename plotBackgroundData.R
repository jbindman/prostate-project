#' plotIndividualData
#'
#' Find the distance between two patients.
#'
#'
#' @param pt.id Patient whose data to print
#' @param what.data What clinical data source to print
#' @param log.scale T
#' @param plot.psad F
#' @export

plotBackgroundData<-function(pt.id = 100, pt = patientDataframes, what.data="both", log.scale=T, plot.psad=F){ #add patientDataframes = ptDataframes


  #pt 100

  pt.data <- pt[[1]]
  psa.data <- pt[[2]]
  bx.full <- pt[[3]]
  #bx.data <- bx_data

  closest100 <- seq(1, 100, by=1)
  #closestPatients <- closestK(pt.id, patientDataframes)
  fullPsa <- subset(psa.data, id %in% closest100)
  fullBx <- subset(bx.full, id %in% closest100)

  individualPsa <- subset(psa.data, id == pt.id)
  individualBx <- subset(bx.full, id == pt.id)

  #print background PSA
  p <- ggplot(fullPsa, aes(x = age, y = psa)) + geom_point(colour = "black") + geom_line(aes(group = id), colour="grey") + stat_quantile(quantiles = c(0.05,0.25, 0.5, 0.75, 0.95))
  p
  #print background biopsy
  q <- ggplot(fullBx, aes(x = int.age, y = rc)) + geom_point(colour = "black")
  #p <- p + geom_smooth(colour = "grey")
  q + geom_jitter(height = .25) #arbitrary jitter


  #p <- ggplot(fullBx, aes(x = int.age, y = bx.here)) + geom_smooth(colour = "grey")
  #p + geom_jitter(height = .25, colour = fullBx$rc) #arbitrary jitter
  #p + geom_jitter(fullBx, aes(x = int.age[!is.na(fullBx$rc)], y = fullBx[!is.na(fullBx$rc)]))



  #subset PSA data
  #psa.data.i<-psa.data[psa.data$id==pt.id,]

  #subset bx data
  #pt.subj<-pt.data$subj[pt.data$id==pt.id] #if you go back and add id to the bx.full dataframe, we don't need this step
  #bx.data.i<-bx.full[bx.full$subj==pt.subj & bx.full$bx.here==1 & !is.na(bx.full$bx.here),]
  p <- ggplot(fullPsa, aes(x = age, y = psa)) + geom_point(colour = "black") + geom_line(aes(group = id), colour="grey") + stat_quantile(quantiles = c(0.05,0.25, 0.5, 0.75, 0.95))+ geom_line(data = individualPsa, aes(x=age, y=psa, group=id, colour="red", show_guide = FALSE)) + geom_point(data = individualPsa, aes(x=age, y=psa, group=id, colour="red", show_guide = FALSE))
  p
  #r <- ggplot(individualPsa, aes(x = age, y = psa)) + geom_point(colour = "red") + geom_line(aes(group = pt.id), colour="red")



  }
