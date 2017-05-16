#' probability
#'
#' My working function for finding individual probabilities over time

probability<-function(){



  for (i in 1:10) {
    print(i)
    probability(i)

  }


  perc <- ggplot(prediction.data, aes(x=ages, y=col2)) + geom_point(aes(x=ages, y=col2)) + geom_line(aes(x=ages, y=col2))
  perc <- perc + scale_y_continuous(limits=c(0, .6)) + scale_x_continuous(limits=c(62, 72)) #age range?
  perc <- perc + labs(title = "Probability of Aggressive Cancer", x = "Age", y = "% Likelihood Aggressive Cancer")



  perc <- perc + theme(axis.title=element_text(size=14))


}


###






