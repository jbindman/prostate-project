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
  library(splines)
  library(ggplot2)

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



  l <- ggplot(rcData, aes(x = ageGroup, y = rc)) + scale_y_log10() +
    geom_point(colour = "black") + geom_line() +
    geom_smooth(method='lm',formula=y~x)

  l <- l + labs(title = "Odds of RC Over time", x = "Age at Visit", y = "log odds RC")

  l <- l +
    geom_point(data = rc, aes(x=int.age, y = c(.06)), color = "red", fill="red", shape = 25, size = 3) +
    geom_point(data = norc, aes(x=int.age, y = c(.02)), color = "black", fill="black", shape = 18, size = 3)





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

