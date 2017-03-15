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

plotBackgroundData<-function(pt.id = 100, closest100 = seq(1, 100, by=1),  pt = patientDataframes, what.data="both", log.scale=T, plot.psad=F){ #add patientDataframes = ptDataframes


  #pt 100

  pt.data <- pt[[1]]
  psa.data <- pt[[2]]
  bx.full <- pt[[3]]
  #bx.data <- bx_data
  #library(splines)
  #library(ggplot2)

  #closest100 <- seq(1, 100, by=1)
  #closestPatients <- closestK(pt.id, patientDataframes)
  fullPsa <- subset(psa.data, id %in% closest100)
  fullBx <- subset(bx.full, id %in% closest100)

  individualPsa <- subset(psa.data, id == pt.id)
  individualBx <- subset(bx.full, id == pt.id)

  #print background PSA
  p <- ggplot(fullPsa, aes(x = age, y = psa)) + scale_y_log10() + scale_x_continuous(limits=c(55, 85)) + geom_point(colour = "black", alpha = 0.2, size = .3) +
   geom_line(aes(group = id), colour="blue", alpha = .1) +
    stat_quantile(quantiles = c(0.05,0.25, 0.5, 0.75, 0.95), formula = (y ~ ns(x,2)), color = "black", alpha = .7) +
    geom_line(data = individualPsa, aes(x=age, y=psa, group=id, colour="red", show_guide = FALSE)) +
    geom_point(data = individualPsa, aes(x=age, y=psa, group=id, colour="red", show_guide = FALSE)) + guides(colour = FALSE)


  p <- p + labs(title = "Your PSA Results", x = "Age of Visit", y = "PSA")
  p <- p + theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())

  individualBx <- subset(individualBx, bx.here == 1)
  norc <- subset(individualBx, rc == 0)
  rc <- subset(individualBx, rc == 1)
  b <- ggplot(norc, aes(x = int.age, y = c(0))) + scale_x_continuous(limits=c(55, 85)) + geom_point(color = "black", shape = 1, size = 3) + geom_point(data = rc, aes(x=int.age, y = c(0)), color = "red", fill="red", shape = 25, size = 3)
  b <- b + labs(title = "Your Biopsy Results", x = "Age of Visit", y = "RC") + coord_fixed(ratio = 1.2)
  b <- b + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  b

  multiplot(p,b)



  b <- scale_x_continuous(limits=c(45, 90))

  fullPsa$psa.avg = filter(fullPsa)
  filter(fullPsa, id == 198)
  mean(sub[,"psa"])


  p + stat_smooth(span = .1)

    fullPsa %>% group_by(id) %>%
    summarise(`25%`=quantile(fullPsa$psa, probs=0.25),
              `50%`=quantile(fullPsa$psa, probs=0.5),
              `75%`=quantile(fullPsa$psa, probs=0.75),
              avg=mean(mpg),
              n=n())

  #add individual
  #print background biopsy
  q <- ggplot(fullBx, aes(x = int.age, y = rc)) +
    geom_line(aes(group = id), colour="blue", alpha = .5) +
    geom_point(colour = "black", alpha = .01) + geom_jitter(height = .25) +
    geom_point(data = individualBx, aes(x=int.age, y=rc, group=id, colour="red", show_guide = FALSE))



  #p <- ggplot(fullBx, aes(x = int.age, y = bx.here)) + geom_smooth(colour = "grey")
  #p + geom_jitter(height = .25, colour = fullBx$rc) #arbitrary jitter
  #p + geom_jitter(fullBx, aes(x = int.age[!is.na(fullBx$rc)], y = fullBx[!is.na(fullBx$rc)]))



  #subset PSA data
  #psa.data.i<-psa.data[psa.data$id==pt.id,]

  #subset bx data
  #pt.subj<-pt.data$subj[pt.data$id==pt.id] #if you go back and add id to the bx.full dataframe, we don't need this step
  #bx.data.i<-bx.full[bx.full$subj==pt.subj & bx.full$bx.here==1 & !is.na(bx.full$bx.here),]

  #r <- ggplot(individualPsa, aes(x = age, y = psa)) + geom_point(colour = "red") + geom_line(aes(group = pt.id), colour="red")



}


