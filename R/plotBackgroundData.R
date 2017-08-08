#' plotIndividualData
#'
#' My working function for Biopsy and PSA
#'
#'
#' @param pt.id Patient whose data to print
#' @param what.data What clinical data source to print
#' @param log.scale T
#' @param plot.psad F
#' @export

plotBackgroundData<-function(pt.id = 100, closest1000 = seq(1, 1000, by=1),  pt = ptDataframes, what.data="both", log.scale=T, plot.psad=F){ #add patientDataframes = ptDataframes

  source("functions-dynamic.R") #where is this reading from?

  #pt 100

  pt.data <- pt[[1]]
  psa.data <- pt[[2]]
  bx.full <- pt[[3]]
  #bx.data <- bx_data
  library(splines)
  library(ggplot2)
  library(gridExtra)
  library(dplyr)


  #closest100 <- seq(1, 100, by=1)
  #closestPatients <- closestK(pt.id, patientDataframes)
  closest100 = seq(1, 100, by=1)
  fullPsa <- subset(psa.data, id %in% closest1000)
  fullBx <- subset(bx.full, id %in% closest1000)

  individualPsa <- subset(psa.data, id == pt.id)
  individualBx <- subset(bx.full, id == pt.id)

  closest400 = seq(1, 400, by=1)
  fullPsa <- subset(psa.data, id %in% closest400)

  first <- 60
  last <- 68
  integers <- NULL
  for (i in first:last) {
    integers <- append(integers, i)
  }




  #print background PSA trajectory
  p <- ggplot(fullPsa, aes(x = age, y = psa)) + scale_y_log10(limits=c(.6, 30)) + scale_x_continuous(limits=c(first, last), breaks = integers) + geom_point(colour = "black", alpha = 0.2, size = .01) +
   geom_line(aes(group = id), colour="gray", alpha = .3) +
    stat_quantile(quantiles = c(0.05,0.25, 0.5, 0.75, 0.95), formula = (y ~ ns(x,2)), color = "black", alpha = .7)

  #add individual Psa
  p <- p +
    geom_line(data = individualPsa, aes(x=age, y=psa, group=id, colour="red", show_guide = FALSE), size = 2) +
    geom_point(data = individualPsa, aes(x=age, y=psa, group=id, colour="red", show_guide = FALSE), size = 2) + guides(colour = FALSE)


  #add individual biopsy
  individualBx <- subset(individualBx, bx.here == 1)
  norc <- subset(individualBx, rc == 0)
  rc <- subset(individualBx, rc == 1)
  #individual bx norc
  p <- p +
    geom_point(data = norc, aes(x=int.age, y = c(1)), color = "red", shape = 1, size = 5)
  #individual bx rc
  if (nrow(rc) != 0) { #if rc has 0 rows, dont add null data
    p <- p +
      geom_point(data = rc, aes(x=int.age, y = c(1)), color = "red", fill="red", shape = 21, size = 5)

  }

  p <- p + labs(title = " Individual Data Highlighted in Cohort Trajectory", x = "Age of Visit", y = "PSA Value")
  p <- p + theme(axis.title.x=element_blank(), axis.title=element_text(size=24),
                 axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(size=22),
                 axis.text.y = element_text(size = 18), panel.background = element_rect(fill = 'white', colour = 'black'),
                 panel.grid.major = element_line(color = 'gray'), panel.grid.minor = element_line(color = 'gray'))




  ###

  prediction.data <- probability(pt.id)
  prediction.data$col2 <-   prediction.data$col2*100
  perc <- ggplot(prediction.data, aes(x=, y=col2)) + geom_point(aes(x=, y=col2), size = 2, color = "red") + geom_line(aes(x=, y=col2), size = 2, color = "red")
  perc <- perc + scale_x_continuous(limits=c(first, last), breaks = integers)
  perc <- perc + labs(title = "Probability of Aggressive Cancer", x = "Age", y = "% Likelihood")
  perc <- perc + theme(axis.title=element_text(size=20), axis.title=element_text(size=24), plot.title = element_text(size=22),
                       axis.text = element_text(size = 18), panel.background = element_rect(fill = 'white', colour = 'black'),
                       panel.grid.major = element_line(color = 'gray'), panel.grid.minor = element_line(color = 'gray'))

  try.ids <- seq(1, 100, 1)
  #try.ids <- pt.data$id[pt.data$surgery==0 & pt.data$status.rc==0][20:200]
  try.ids <- pt.data$id
  full <- data.frame(col1=NULL, col2=NULL, col3 = NULL, col4 = NULL, col5 = NULL, col6 = NULL, col7 = NULL, col8 = NULL)
  for (i in 1:length(try.ids)) {
    print(i)
    prediction.data <- probability(try.ids[i])
    temp <- data.frame(col1=prediction.data$col1, col2= prediction.data$ages, col3 = prediction.data$col2, col4 = try.ids[i], col5 = prediction.data$recBiopsy, col6 = prediction.data$recSurgery, col7 = prediction.data$ageFac, col8 = prediction.data$lastPred)
    full <- rbind(full, temp)
  }
  colnames(full) <- c("Date","Age","Probability", "ID", "Biopsy", "Surgery", "AgeFac", "LastPred")

  #lastPredGS0 <- filter(full, LastPred==1) only look at ids from surg and no surg

                        #, Surgery == -.02)$Probability
  #articifically removing duplicates, should only be one lastpred per person but could be multiple people w same num
  lastPredGS1 <- filter(full, LastPred==1, Surgery == 1.02)$Probability

  #histogram 0s and 1s

  lastPredGs0 <- filter(pt.data, true.gs == 0)$id
  probabilitiesGs0 <- filter(full, ID %in% lastPredGs0, LastPred == 1)$Probability
  hist(probabilitiesGs0)

  lastPredGs1 <- filter(pt.data, true.gs == 1)$id
  probabilitiesGs1 <- filter(full, ID %in% lastPredGs1, LastPred == 1)$Probability
  hist(probabilitiesGs1)




  lastPredGS1 <- filter(full, LastPred==1, Surgery == 1.02)$Probability



  filter(gs0, LastPred  = 1)


  ggplot(young, aes(x = Age, y = Probability)) + scale_x_continuous(limits=c(60, 78))  + #geom_point(colour = "gray", alpha = 0.5, size = .1) +
    geom_line(aes(group = ID), colour="gray", alpha = .5) + stat_quantile(quantiles = c(0.05,0.25, 0.5), formula = (y ~ ns(x,2)), color = "black", alpha = .7) +
    stat_quantile(quantiles = c(0.75, 0.95, .975), formula = (y ~ ns(x,2)), color = "red", alpha = .7)

  ggplot(full, aes(x = Age, y = Probability)) + scale_x_continuous(limits=c(60, 78))  + #geom_point(colour = "gray", alpha = 0.5, size = .1) +
    geom_line(aes(group = ID), colour="gray", alpha = .5) + stat_quantile(quantiles = c(0.05,0.25, 0.5), formula = (y ~ ns(x,2)), color = "black", alpha = .7) +
    stat_quantile(quantiles = c(0.75, 0.95, .975), formula = (y ~ ns(x,2)), color = "red", alpha = .7)

    #60
    agefac <- 1
   #60-65
     agefac <- 2
    #65-70
    agefac <- 3
    #70-75
    agefac <- 4
    #older than 75
    agefac <- 5

    young <- subset(full, AgeFac == 2)

    noBx <- subset(young, Biopsy == 2)
    norc <- subset(young, Biopsy == 0)
    rc <- subset(young, Biopsy == 1) #none in this category

  noSurg <- subset(young, Surgery == 2)
  gs0 <- subset(young, Surgery == -.02)
  gs1 <- subset(young, Surgery == 1.02)$ID



  #predictions over time by age
  a <- ggplot(young, aes(x = Age, y = Probability)) + scale_x_continuous(limits=c(60, 70))  + #geom_point(colour = "gray", alpha = 0.5, size = .1) +
    geom_line(aes(group = ID), colour="gray", alpha = .75) #+
    #stat_quantile(quantiles = c(0.05,0.25, 0.5), formula = (y ~ ns(x,2)), color = "black", alpha = .7) +
    #stat_quantile(quantiles = c(0.75, 0.95, .975), formula = (y ~ ns(x,2)), color = "red", alpha = .7)
  a <- a + geom_point(data = norc, aes(x=Age, y = Probability), color = "blue", shape = 24, size = .6, alpha = .75) +
    geom_point(data = rc, aes(x=Age, y = Probability), color = "red", fill = "red", shape = 24, size = 1, alpha = .75) #norc

  lastPredGs0 <- filter(pt.data, true.gs == 0)$id
  healthy <- filter(young, ID %in% lastPredGs0)
  lastPredGs1 <- filter(pt.data, true.gs == 1)$id
  sick <- filter(young, ID %in% lastPredGs1)

  a <- a + geom_line(data = healthy, aes(x = Age, y = Probability, group = ID), colour="blue", position=position_jitter(w=0.00, h=0.05), size = .4, alpha = .5)
  a <- a + geom_line(data = sick, aes(x = Age, y = Probability, group = ID), colour="red", position=position_jitter(w=0.00, h=0.05), size = .4, alpha = .5)


  a <- a + geom_line(data = gs1, aes(x = Age, y = Probability, group = ID), colour="red", position=position_jitter(w=0.00, h=0.05))
  a <- a + geom_line(data = gs0, aes(x = Age, y = Probability, group = ID), colour="blue", position=position_jitter(w=0.00, h=0.05))

  a <- a + labs(title = "P(Aggressive Cancer) for Sick Men Diagnosed Age 60-65 ", x = "Age of Visit", y = "Probability", size = 500)
  #initial vs full

  a + theme(axis.text=element_text(size=16),
              axis.title=element_text(size=18), plot.title = element_text(size=22))





  #why does 29 shoot directly up instead of a year after?
  t <- 29
  test <- subset(full, ID == t)
  a + geom_line(data = test, aes(x = Age, y = Probability, group = ID), colour="red")







  short <- ggplot(NULL, aes(x = Age, y = Probability)) + scale_x_continuous(limits=c(60, 78)) +
   geom_line(data = y.5, aes(group = ID), colour="blue", alpha = .15) +
    stat_quantile(data = y.5, quantiles = c(0.05,0.25, 0.5), formula = (y ~ ns(x,2)), color = "black", alpha = .7) +
        stat_quantile(data = y.5, quantiles = c(0.75, 0.95, .975), formula = (y ~ ns(x,2)), color = "blue", alpha = .7)

  long <- ggplot(NULL, aes(x = Age, y = Probability)) + scale_x_continuous(limits=c(60, 78)) +
    geom_line(data = y.10, aes(group = ID), colour="red", alpha = .15) +
    stat_quantile(data = y.5, quantiles = c(0.05,0.25, 0.5), formula = (y ~ ns(x,2)), color = "black", alpha = .7) +
        stat_quantile(data = y.10, quantiles = c(0.75, 0.95, .975), formula = (y ~ ns(x,2)), color = "red", alpha = .7)



  short <- short + labs(title = "Probability of Reclassifying, Length of Follow up Time < 5 years", x = "Age of Visit", y = "Probability")
  long <- long + labs(title = "Probability of Reclassifying, Length of Follow up Time 5-10 years", x = "Age of Visit", y = "Probability")

  ####


  #young <- filter(full, Age < 65)
  #old <- filter(full, Age > 65)

  #y <- ggplot(NULL, aes(x = yearsSince, y = Probability)) + scale_x_continuous(limits=c(0, 20)) +
  #  geom_line(data = young, aes(group = ID), colour="blue", alpha = .15) + stat_quantile(data = young, quantiles = c(0.05,0.25, 0.5), formula = (y ~ ns(x,2)), color = "black", alpha = .7) +
  #    stat_quantile(data = young, quantiles = c(0.75, 0.95, .975), formula = (y ~ ns(x,2)), color = "blue", alpha = .7) +
  #  geom_line(data = old, aes(group = ID), colour="red", alpha = .15) + stat_quantile(data = old, quantiles = c(0.05,0.25, 0.5), formula = (y ~ ns(x,2)), color = "black", alpha = .7) +
  #    stat_quantile(data = old, quantiles = c(0.75, 0.95, .975), formula = (y ~ ns(x,2)), color = "red", alpha = .7)

  #y2.5 = quantile(full$Probability, 0.025)

  #ggplot(initial, aes(x = col1, y = col2)) + geom_point(aes(x=, y=col2), size = 2, color = "red") + geom_line(aes(group = id), colour="red")
  initial <- initial[complete.cases(initial),]
  initial$age <- floor(initial$col1/2)*2

  full <- full[complete.cases(full),]
  full$age <- floor(full$col1/2)*2

  #plots all predictions over time
  g <- ggplot(full, aes(x = col1, y = col2)) + scale_x_continuous(limits=c(60, 78))  + #geom_point(colour = "gray", alpha = 0.5, size = .1) +
    geom_line(aes(group = col3), colour="gray", alpha = .5)



  #predictions over time separated by average low or high risk
  hr <- NULL
  ids <- distinct(full, col3)$col3
  for (i in ids) {
    ave <- mean(filter(full, col3 == i)$col2)
    if (ave > .25) {
      print(ave)
      hr <- append(hr, i)
    }

  }


  ids <- distinct(full, col3)$col3
  lr <- ids[!ids %in% hr]

  hr.df <- subset(full, col3 %in% hr)
  hr.df$risk <- 1
  lr.df <- subset(full, col3 %in% lr)
  lr.df$risk <- 2
  #strat <- rbind(lr.df, hr.df)


  l <- ggplot(lr.df, aes(x = col1, y = col2)) + scale_x_continuous(limits=c(60, 78))  + #geom_point(colour = "gray", alpha = 0.5, size = .1) +
    geom_line(aes(group = col3), colour="gray", alpha = .5)

  #h <- ggplot(hr.df, aes(x = col1, y = col2)) + scale_x_continuous(limits=c(60, 78))  + #geom_point(colour = "gray", alpha = 0.5, size = .1) +
   # geom_line(aes(group = col3), colour="red", alpha = .5)

  div <- ggplot(NULL, aes(x = col1, y = col2)) + scale_x_continuous(limits=c(60, 78)) +
    geom_line(data = lr.df, aes(group = col3), colour="blue", alpha = .15) +
    geom_line(data = hr.df, aes(group = col3), colour="red", alpha = .15)

  #div + geom_line(data = lowRisk, aes(x=col1, y=col2, color="low risk"), size = 1) +
   # geom_line(data = highRisk, aes(x=col1, y=col2, color="high risk"), size = 1)



  #boxplots
  stats <- NULL
  for (i in 60:77) {
    box <- filter(full, ceiling(col1) == i)
    df <- data.frame (
      x = i,
      #y0 = min(box$col2),
      y2.5 = quantile(box$col2, 0.025),
      y5 = quantile(box$col2, 0.05),
      y25 = quantile(box$col2, 0.25),
      y50 = median(box$col2),
      y75 = quantile(box$col2, 0.75),
      y90 = quantile(box$col2, 0.90),
      y95 = quantile(box$col2, 0.95),
      y97.5 = quantile(box$col2, 0.975)
      #y100 = max(box$col2)
    )
    stats <- rbind(stats, df)

  }

  c <- ggplot(stats, aes(x = x)) + geom_line(aes(y = y100), color = "red") +
    geom_line(aes(y = y50), color = "blue") +   geom_line(aes(y = y75), color = "blue") +
    geom_line(aes(y = y25), color = "blue") +   geom_line(aes(y = y95), color = "orange") +
    geom_line(aes(y = y5), color = "green") +   geom_line(aes(y = y0), color = "yellow")

  c <- c + labs(title = "Statistical Quantiles for Predictions", x = "Age of Visit", y = "P(Aggressive Cancer)")

  filter(full, col2 > .30)$col3




  ggplot(full, aes(x = age, y = col2)) +
    geom_boxplot(aes(lower = y5, middle = y50, upper = y95, ymin = y0, ymax = y100, fill = type), stat = "identity")

  prediction.data <- probability(675)
  #highRisk <- data.frame(col1=prediction.data$, col2=prediction.data$col2, col3 = 675)
  prediction.data <- probability(237)
  #lowRisk <- data.frame(col1=prediction.data$, col2=prediction.data$col2, col3 = 237)



   l <-l + geom_line(data = lowRisk, aes(x=col1, y=col2, color="low risk"), size = 1) +
    geom_line(data = highRisk, aes(x=col1, y=col2, color="high risk"), size = 1)

   g + ggplot(initial, aes(x=factor(round_any(col1,0.5)), y=col2) + geom_boxplot())


  ####

  p1 <- ggplot_gtable(ggplot_build(p)) #warnings
  p2 <- ggplot_gtable(ggplot_build(perc))
  maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3])
  p1$widths[2:3] <- maxWidth
  p2$widths[2:3] <- maxWidth
  grid.arrange(p1, p2, heights = c(3, 2))



  #needs to handle patients who never rc
  individualBx <- subset(individualBx, bx.here == 1)
  norc <- subset(individualBx, rc == 0)
  rc <- subset(individualBx, rc == 1)
  b <- ggplot(norc, aes(x = int.age, y = c(0))) + scale_x_continuous(limits=c(64, 68)) + geom_point(color = "black", shape = 1, size = 3)
  if (nrow(rc) != 0) { #if rc has 0 rows, dont add null data
    b <- b + geom_point(data = rc, aes(x=int.age, y = c(0)), color = "red", fill="red", shape = 25, size = 3)

  }
  b <- b + labs(title = "Biopsy Data", x = "Age of Visit", y = "Reclassification") + coord_fixed(ratio = 1.2)
  b <- b + theme(axis.title.x=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title=element_text(size=16))
  b



  #now add bottom plot for prediction



#some have small length of follow up time, should these plots zoom?
  ########




  #plots biopsy data for individuals and group
  bxSubset <- filter(fullBx, rc == 1 | rc == 0)
  t <- ggplot(bxSubset, aes(x = int.age, y = rc))  + binomial_smooth(formula = y ~ splines::ns(x, 2)) + scale_x_continuous(limits=c(55, 85)) #binomial smooth for logistic regression
  #add individual
  t + scale_x_continuous(limits=c(55, 85)) + coord_cartesian(ylim=c(0,.1))
  norc <- subset(individualBx, rc == 0)
  rc <- subset(individualBx, rc == 1)
  t <- t +
    geom_point(data = rc, aes(x=int.age, y = c(.075)), color = "red", fill="red", shape = 25, size = 3) +
    geom_point(data = norc, aes(x=int.age, y = c(.05)), color = "black", fill="black", shape = 18, size = 3)
  #t <- ggplot(rc, aes(x=int.age, y = c(.25)), color = "red", fill="red", shape = 25, size = 3) +
    #geom_point(aes(x=int.age, y = c(.2)), color = "red", fill="red", shape = 25, size = 3) +
    #geom_point(data = norc, aes(x=int.age, y = c(.00)), color = "black", fill="black", shape = 18, size = 3)


  #plot eta hat
  today = 15400
  date_pred <- c(today - 4*365, today - 3*365, today - 2*365, today - 365, today)
  sample <- getPredictions(id_i = pt.id, date_pred)
  ggplot(data = sample, aes(x = age_i, y = percentRC)) + geom_point(colour="blue", size = 2) +
    geom_point(data = rc, aes(x=int.age, y = c(.01)), color = "red", fill="red", shape = 25, size = 3) +
    geom_point(data = norc, aes(x=int.age, y = c(.01)), color = "black", fill="black", shape = 18, size = 3)

  binomial_smooth <- function(...) {
    geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
  }
  #not particularly useful
  t + geom_point(data = sample, aes(x = age_i, y = means), colour="blue", size = 2) +
    geom_point(data = rc, aes(x=int.age, y = c(.075)), color = "red", fill="red", shape = 25, size = 3) +
    geom_point(data = norc, aes(x=int.age, y = c(.05)), color = "black", fill="black", shape = 18, size = 3)



  ####end working

  #test area

  d <- subset(pt.data, id == pt.id)$dob.num #numeric
  dob_vector <- c(rep(d, length(sample[2,]))) #repeats vector

  x = (date_pred - dob_vector)/365 #age of visit +
  y = sample[2,] #rc likelihood
  predictions <-   as.data.frame(cbind(x,y))
  ggplot(predictions, aes(x = x, y = y)) + geom_point(colour="red", size = 3)

  #patient 1 is 67.9 at 17327, four years before should be 63.9 checks out


####





  #  biopsy info discrete
  l <- ggplot(rcData, aes(x = ageGroup, y = rc)) + scale_y_log10() +
    geom_point(colour = "black") + geom_line() +
    geom_smooth(method='lm',formula=y~x)

  l <- l + labs(title = "Odds of RC Over time", x = "Age at Visit", y = "log odds RC")

  l <- l +
    geom_point(data = rc, aes(x=int.age, y = c(.06)), color = "red", fill="red", shape = 25, size = 3) +
    geom_point(data = norc, aes(x=int.age, y = c(.02)), color = "black", fill="black", shape = 18, size = 3)


  #multiplot(p,b) #gives uneven horizontal access

  #discrete survival surve ... want continuous
  ageGroup <- seq(60, 79, by=.5) #subdivided into 3 month intervals
  rcData <- data.frame(ageGroup)
  rcData$rc <- 1
  for (i in rcData$ageGroup) {
    upper = i + .25
   lower = i - .25
   subset <- filter(bx.full, int.age > lower, int.age < upper, bx.here == 1) #fullBx subset
   num0s <- length(which(subset$rc == "0"))
   num1s <- length(which(subset$rc == "1"))
   odds <- num1s/num0s
   print(odds)
   #rcData$num0s[ageGroup == i] <-num0s
   #rcData$num1s[ageGroup == i] <-num1s
   rcData$rc[ageGroup == i] <- odds
  }

  #binomial smooth with glm
  binomial_smooth <- function(...) {
    geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
  }
  ggplot(fullBx, aes(int.age, rc))  +
    binomial_smooth() #binomial smooth for logistic regression

  ggplot(fullBx, aes(int.age, rc)) + stat_smooth(formula = (y ~ ns(x,2))) #linear model











}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

