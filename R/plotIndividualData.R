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

plotIndividualData<-function(pt.id = 60, what.data="both", log.scale=T, plot.psad=F){ #add patientDataframes = ptDataframes



  pt.data <- patientDataframes[[1]]
  psa.data <- patientDataframes[[2]]
  bx.full <- patientDataframes[[3]]
  #bx.data <- bx_data

  closestK <- closestK(pt.id, patientDataframes = patientDataframes)
  fullPsa <- subset(psa.data, id %in% closestK)
  fullBx <- subset(bx.full, id %in% closestK) #doesn't pull right because of subj/id issue

  #print background PSA
  p <- ggplot(fullPsa, aes(x = age, y = psa)) + geom_point(colour = "grey") + geom_line(aes(group = id), colour="grey")
  p + stat_quantile(quantiles = c(0.05,0.25, 0.5, 0.75, 0.95))
  #print background biopsy
  p <- ggplot(fullBx, aes(x = int.age, y = bx.here)) + geom_point(colour = "grey")
  p + geom_jitter(height = .25) #arbitrary jitter



  #subset PSA data
  psa.data.i<-psa.data[psa.data$id==pt.id,]

  #subset bx data
  pt.subj<-pt.data$subj[pt.data$id==pt.id] #if you go back and add id to the bx.full dataframe, we don't need this step
  bx.data.i<-bx.full[bx.full$subj==pt.subj & bx.full$bx.here==1 & !is.na(bx.full$bx.here),]

  if(what.data=="both"){

    #log.scale<-T
    #plot.psad<-F

    n.psa.i <- dim(psa.data.i)[1] #number of PSA observations for pt

    if(plot.psad==T){
      psa.data.i$vol <- pt.data$vol.avg[pt.data$id==pt.id] #average prostate volume
      psa.to.plot <- ifelse(rep(log.scale==T, n.psa.i), (psa.data.i$log.psa - log(psa.data.i$vol)), psa.data.i$psa/psa.data.i$vol) } #define values to be plotted
    #note: ifelse() returns an object that is the same length as the first argument. that is why I use rep()

    if(plot.psad==F){
      psa.to.plot <- ifelse(rep(log.scale==T, n.psa.i), psa.data.i$log.psa, psa.data.i$psa)} #define values to be plotted


    #define the range of (log)PSA/PSAD values. This will be used to define the area of the plotting space and place biopsy results at the top and bottom of graph
    psa.range <- range(psa.to.plot)
    length.range <- psa.range[2] - psa.range[1]
    ylim.i <- psa.range + c(-1,1)*0.1*length.range #here, I am just trying to make the plotting window slightly larger than the range of the PSA data


    par(mar=c(3.5,3.25,2,3)) #this sets the margins outside the plotting window. We want more space on the right than given my default so we can add the biopsy result labels and tic marks

    plot(x=0, y=0, type="n", ylim=ylim.i, xlim=range(c(psa.data.i$age, bx.data.i$age)), yaxt=ifelse(log.scale==T,"n","s"), xlab="", ylab="") #plot() establishes the plotting environment. I don't actually want to plot any data yet, just define the environment, so I use x=0, y=0, type="n to supress plotting any data.
    #Then, I define the limits of the x and y-axis based on the range of the data.
    #Next, I tell R to add tic marks and axis labels to the left hand axis if we aren't using the log-scale data. For the log scale, we will place and label the tic marks manually
    #Finally, I leave the x- and y- axis labels blank


    mtext(ifelse(plot.psad==F,"PSA (ng/mL)", "PSA Density"), side=2, line=2.25)	#lable the y-axis depending on what data we are showing


    if(log.scale==T){ #manually place and label tic marks for y-axis
      length.range.nolog<- exp(psa.range[2]) - exp(psa.range)[1]

      if(plot.psad==F){ #for these, I add a different number of tic marks depending on the maximum observed PSA. (I don't want the axis to be too crowded or, alternatively, not give enough information). The code is slightly different for PSA and PSAD since PSAis larger and can be represented with whole numbers I can use the floor() and ceiling() function
        eylim.i<-exp(ylim.i)
        if(length.range.nolog<1){ Axis(side=2, at=log(seq(round(eylim.i[1],1) , round(eylim.i[2],1), 0.1)), labels= seq(round(eylim.i[1],1), round(eylim.i[2], 1), 0.1) ) }else{
          if(length.range.nolog<2){ Axis(side=2, at=log(seq(floor(eylim.i[1]) , ceiling(eylim.i[2]), 0.25)), labels= seq(floor(eylim.i[1]), ceiling(eylim.i[2]), 0.25) )}else{
            if(length.range.nolog<4){ Axis(side=2, at=log(seq(floor(eylim.i[1]) , ceiling(eylim.i[2]), 0.5)), labels= seq(floor(eylim.i[1]), ceiling(eylim.i[2]), 0.5) ) }else{
              if(length.range.nolog<11){ Axis(side=2, at=log(seq(floor(eylim.i[1]) , ceiling(eylim.i[2]))), labels= seq(floor(eylim.i[1]), ceiling(eylim.i[2]), 1) ) }else{
                Axis(side=2, at=log(c(1, seq(5,100,5))), labels=c(1, seq(5,100,5))) }}}}
      } #end plot.psad=F

      if(plot.psad==T){ # for log(PSAD)
        if(length.range.nolog<0.05){Axis(side=2, at=log(seq(0,1,0.01)), labels=seq(0,1,0.01)) }else{
          if(length.range.nolog<0.1){Axis(side=2, at=log(seq(0,1,0.02)), labels=seq(0,1,0.02))}else{
            Axis(side=2, at=seq(0,1,0.05), labels=seq(0,1,0.05))} }
      }#end plot.psad=T

    }#end log.scale=T for y-axis tic mark labels

    #Reclassification axis labels on right side axis
    Axis(side=4, at=ylim.i , labels=c("No", "Yes") )
    mtext("Biopsy Reclassification Observed", side=4, line=2)

    #Label x-axis
    mtext("Age (years)", side=1, line=2.25)

    #plot PSA data
    points(psa.to.plot~psa.data.i$age, pch=19, col="blue", cex=1.5) #points
    lines(psa.to.plot~psa.data.i$age, col="blue") #lines

    #plot BX data
    rc.x<-ifelse(bx.data.i$rc==0, ylim.i[1], ylim.i[2]) #convert no reclassification and reclassificaiton outcomes to lower and upper end of psa data range
    points(rc.x~bx.data.i$bx.age, pch=25, col="green4", bg="green", cex=2) #each triangle is one biopsy received


  } #END WHAT.DATA=BOTH

  #Julia- you can do what.data="psa" and what.data="biopsy" using the code above
  #If only PSA data, you don't need a wide margin on the right, make par(mar=c(3.5,3.25,2,1))
  #If only biopsy data, you can put the reclassification label and tic marks on the left axis (instead of the right) and again use par(mar=c(3.5,3.25,2,1))

}
