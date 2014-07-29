#require("lattice")
require("ggplot2")
require("reshape2")
require("plotrix")
require("cluster")
#require("Gmisc")
#require("zoo")

# This is the global function that runs the whole set of HCI-oriented data analyses
# It receives the root dir containing the logs/, maps/, quests/, eyetrack/ folders
# (and if not passed, it assumes it is the current one)
# It generates a number of images with graphs, visualizations, etc. in the directory from which it is executed
JDCAnalysisHCI <- function(rootDir="."){
  
  originalDir <- getwd()
  setwd(rootDir)
  rootDir <- getwd() # So that we get the full path
  # We go back to the original directory
  setwd(originalDir)
  
  #We load the different data sources
  # Basic map completion stats from video analysis
  #mapsData <- get(load(paste(rootDir,"/maps/","Maps.rda",sep="")))
  # Survey data
  #surveyData <- get(load(paste(rootDir,"/quests/","Survey.rda",sep="")))
  # TODO: Log data summary, or maybe two summaries, one global, another for the 9 groups in Act4?
  logdir <- paste(rootDir,"/logs",sep="")
  #logData <- getLogSummaryHCI(logdir,mapsData)
  
  # We plot the different questions/themes we had
  # Basic plots of children preferences
  basicPreferencePlots(surveyData, act4UsageSummary)
  act4UsageSummary <- getAct4UsageLogSummary(logdir, mapsData)
  basicPreferencePlots(surveyData, act4UsageSummary)
  
  # Bad uses of the interface
  
  # Manipulative usage
  manipulativeUsagePlots(act4UsageSummary)
  
  # Collaboration (ownership, awareness)
  goPositionPlots(logdir)
  continuousPositionPlots(logdir)
  fractionValuePlots(logdir)
  hintCardsPlots(logdir)
  
}

manipulativeUsagePlots <- function(act4UsageSummary){
  
  # TODO: Graphs about multiple usage stats in Act4
  act4UsageSummary$Relative.Usage.Single.Act4 <- act4UsageSummary$Relative.Usage.Sole.Cont.Act4 + act4UsageSummary$Relative.Usage.Sole.Disc.Act4 + act4UsageSummary$Relative.Usage.Sole.Frac.Act4
  act4UsageSummary$Relative.Usage.Multiple.Act4 <- act4UsageSummary$Relative.Usage.Cont.Disc.Act4 + act4UsageSummary$Relative.Usage.Disc.Frac.Act4 + act4UsageSummary$Relative.Usage.Frac.Cont.Act4 + act4UsageSummary$Relative.Usage.All.Act4
  data <- act4UsageSummary[!is.na(act4UsageSummary$Relative.Usage.Single.Act4),c("Group.Name","Relative.Usage.Single.Act4","Relative.Usage.Multiple.Act4","Relative.Usage.None.Act4")]
  data <- melt(data,id.vars="Group.Name")
  
  png("Single.Multiple.Manip.Usage.Act4.png",width=1280,height=1024)  
  p <- ggplot(data, aes(x=Group.Name, y=value, fill=variable)) + 
    ggtitle("Usage of single/multiple representations in Act4") + 
    geom_bar(position="fill", stat="identity")
  print(p)
  dev.off()
  
}



# This function tries to answer the following questions:
# What kind of fractions did kids PREFER (abstract vs. concrete, continuous vs. discrete)?
# What kind of fractions did kids USE MORE (abstract vs. concrete, continuous vs. discrete) in Act4?
basicPreferencePlots <- function(surveyData, act4UsageSummary){
  
  # What do kids prefer / from survey
  png("Q1-LikeApproach.Boxplots.png",width=1280,height=1024)  
  par(mfrow=c(1,3))
  boxplot(surveyData$Q1.More.Fun, ylim=c(1,5), main="Q1", sub="I prefer this way to what we do at school", xlab="Total")
  boxplot(surveyData$Q1.More.Fun ~ surveyData$Sequence, ylim=c(1,5), main="Q1", xlab="By sequence of fractions manipulative experienced",col=(c("lightgoldenrod","lightgreen")))
  boxplot(surveyData$Q1.More.Fun ~ surveyData$Sequence * surveyData$Session, ylim=c(1,5), main="Q1", xlab="By sequence AND session",col=(c("lightgoldenrod","lightgreen")))
  dev.off()
  
  png("Q2-PreferConcrete.Boxplots.png",width=1280,height=1024)  
  par(mfrow=c(1,3))
  boxplot(surveyData$Q2.Concrete.Repr, ylim=c(1,5), main="Q2", sub="I prefer concrete (vs. symbolic) fraction manip.", xlab="Total")
  boxplot(surveyData$Q2.Concrete.Repr ~ surveyData$Sequence, ylim=c(1,5), main="Q2", xlab="By sequence of fractions manipulative experienced", col=(c("lightgoldenrod","lightgreen")))
  boxplot(surveyData$Q2.Concrete.Repr ~ surveyData$Sequence * surveyData$Session, ylim=c(1,5), main="Q2", xlab="By sequence AND session", col=(c("lightgoldenrod","lightgreen")))
  dev.off()

  png("Q3-PreferContinuous.Boxplots.png",width=1280,height=1024)  
  par(mfrow=c(1,3))
  boxplot(surveyData$Q3.Continuous.Repr, ylim=c(1,5), main="Q3", sub="I prefer continuous (vs. token) fraction manip.", xlab="Total")
  boxplot(surveyData$Q3.Continuous.Repr ~ surveyData$Sequence, ylim=c(1,5), main="Q3", xlab="By sequence of fractions manipulative experienced", col=(c("lightgoldenrod","lightgreen")))
  boxplot(surveyData$Q3.Continuous.Repr ~ surveyData$Sequence * surveyData$Session, ylim=c(1,5), main="Q3", xlab="By sequence AND session", col=(c("lightgoldenrod","lightgreen")))
  dev.off()

  # How coherent are preferences within a group? range and std per group? Are there any outliers?
  agQ2sd <- aggregate(Q2.Concrete.Repr ~ Group.Number, surveyData, sd)
  agQ3sd <- aggregate(Q3.Continuous.Repr ~ Group.Number, surveyData, sd)
  
  png("Q2.Group.Coherence.SD.Plot.png",width=1280,height=1024)
  print(qplot(Group.Number, Q2.Concrete.Repr, data=agQ2sd, main="Q2 (concrete vs. abstract preference) SD",xlab="Groups"))
  dev.off()
  
  png("Q3.Group.Coherence.SD.Plot.png",width=1280,height=1024)
  print(qplot(Group.Number, Q3.Continuous.Repr, data=agQ3sd, main="Q3 (continuous vs. discrete preference) SD",xlab="Groups"))
  dev.off()

  # What do kids use when given the choice (Act4)?
  png("Relative.Usage.Manips.Act4.Boxplots.png",width=1280,height=1024)  
  par(mfrow=c(1,3))
  boxplot(act4UsageSummary$Relative.Using.Cont.Act4,
          ylim=c(0,1), main="Continuous Manipulative usage in Act4")
  boxplot(act4UsageSummary$Relative.Using.Disc.Act4,
        ylim=c(0,1), main="Discrete Manipulative usage in Act4")
  boxplot(act4UsageSummary$Relative.Using.Frac.Act4, 
        ylim=c(0,1), main="Symbolic Fraction usage in Act4")
  dev.off()
  png("Relative.Usage.Hints.Act4.Boxplots.png",width=1280,height=1024)  
  par(mfrow=c(1,5))
  boxplot(act4UsageSummary$Relative.Help.Circ.Act4,
          ylim=c(0,1), main="Circular Hint usage in Act4")
  boxplot(act4UsageSummary$Relative.Help.Rect.Act4,
          ylim=c(0,1), main="Rectangular Hint usage in Act4")
  boxplot(act4UsageSummary$Relative.Help.Disc.Act4,
          ylim=c(0,1), main="Discrete Hint usage in Act4")
  boxplot(act4UsageSummary$Relative.Help.Dec.Act4,
          ylim=c(0,1), main="Decimal Hint usage in Act4")
  boxplot(act4UsageSummary$Relative.Help.Frac.Act4, 
          ylim=c(0,1), main="Fraction Hint usage in Act4")
  dev.off()
  # TODO: We could do barplots of relative usage all the 9 groups, both for the manipulatives and for the hints, to see common patterns

  # How coherent is preference and usage when given the choice?
  # merge the log summary and the survey data
  totalData <- merge(surveyData, act4UsageSummary, by.x="Group.Number", by.y="Group.Name")
  # create a melt for plotting two variables in the same graphic
  data <- totalData[,c("Q2.Concrete.Repr","Relative.Using.Frac.Act4","Relative.Using.Concr.Act4")]
  data <- melt(data, id.vars="Q2.Concrete.Repr")

  # Q2 vs Relative.Fraction and Relative.Concrete
  png("Concrete.Symbolic.Prefs.Usage.png",width=1280,height=1024)
  p <- ggplot(data, aes(x=Q2.Concrete.Repr, y=value, colour=variable)) + 
    ggtitle("Concrete vs. Symbolic / Preferences and usage") + 
    geom_point(size=5,alpha=0.4, position=position_jitter(width=0.05, height=0.01)) + geom_smooth(method="lm") + ylim(c(0,1))
  plot(p)
  dev.off()

  # Q3 vs Relative.Cont and Relative.Disc
  data <- totalData[,c("Q3.Continuous.Repr","Relative.Using.Cont.Act4","Relative.Using.Disc.Act4")]
  data <- melt(data, id.vars="Q3.Continuous.Repr")
  png("Continuous.Discrete.Prefs.Usage.png",width=1280,height=1024)
  p <- ggplot(data, aes(x=Q3.Continuous.Repr, y=value, colour=variable)) + 
    ggtitle("Concrete vs. Symbolic / Preferences and usage") + 
    geom_point(size=5,alpha=0.4, position=position_jitter(width=0.05, height=0.01)) + geom_smooth(method="lm") + ylim(c(0,1))
  plot(p)
  dev.off()

}

# This function takes care of everything related to the Hint Cards, X-Y position, X and Y over time, etc...
# What hint card is prefered? 
hintCardsPlots <- function(logdir){
  setwd (logdir)
  
  #For each file in logdir
  for(file in list.files(path=logdir,pattern = "\\.rda$")){
    data <- get(load(file))
    
    #We get the data of the position
    logHintPosition <- getHintPositiondata(data)
    
    #Get the name of the .png file
    groupName <- basename(file)
    groupName <-  substr(groupName, 1, nchar(groupName)-4)
    
    #Draw the plots for each hint:
    XYPositionPlot(paste(groupName,"Discrete.Hint.Card.Position.png",sep=""),"Discrete Hint XY Position", groupName, 
                   logHintPosition$Position.H1.y, logHintPosition$Position.H1.x) #Discrete
    XYPositionPlot(paste(groupName,"Fraction.Hint.Card.Position.png",sep=""), "Fraction Hint XY Position",groupName,
                   logHintPosition$Position.H2.y, logHintPosition$Position.H2.x) #Fraction
    XYPositionPlot(paste(groupName,"Circular.Hint.Card.Position.png",sep=""), "Circular Hint XY Position", groupName,
                   logHintPosition$Position.H3.y, logHintPosition$Position.H3.x) #Circular
    XYPositionPlot(paste(groupName,"Rectangular.Hint.Card.Position.png",sep=""), "Rectangular Hint XY Position", groupName,
                   logHintPosition$Position.H4.y, logHintPosition$Position.H4.x) #Rectangular
    XYPositionPlot(paste(groupName,"Decimal.Hint.Card.Position.png",sep=""), "Decimal Hint XY Position", groupName,
                   logHintPosition$Position.H5.y, logHintPosition$Position.H5.x) #Decimal
    
    
    #Draw all the hints together
    positionMeanHint(logHintPosition,groupName)
    
    #We now plot the X and Y values
    hintPositionOverTime(logHintPosition,groupName)
    
    #Finally we do the Bubbles to see the proportion of each hint used
    bubblePlotHint(logHintPosition,groupName)
  }
}

# This function tries to answer the following question:
# Do the students used techniques (eg 0-1)? 
fractionValuePlots <- function(logdir){
  setwd (logdir)
  
  #For each file in logdir
  for(file in list.files(path=logdir,pattern = "\\.rda$")){
    data <- get(load(file))
    
    #We get the data of the position
    logFractionData <- getFractionValueData(data)
    
    #We need the max and min values of the Timestamp
    minTime <- min(na.omit(logFractionData$Timestamp))
    maxTime <- max(na.omit(logFractionData$Timestamp))
    
    #Get the name of the .png file
    groupName <- basename(file)
    groupName <-  substr(groupName, 1, nchar(groupName)-4)
    name <- paste(groupName,".Circular.Values.png",sep="")
        
    #Create the .png file of the position, first with the Circular Values
    png(name,width=1280,height=1024)
    plot(logFractionData$Value.C1 ~ logFractionData$Timestamp, type='l',xlim=c(minTime, maxTime),ylim = c(0,1), 
         col='green', axes=TRUE,xlab = "Time [ms]", ylab = "Fraction", main = paste(groupName," Circular  Value vs Time",sep=""))
    lines(logFractionData$Timestamp,logFractionData$Value.C2,col='blue')
    legend("topright", legend=c("C1","C2"), col=c("green","blue"),lwd=1.5)
    
    dev.off()
    
    ## Then the Rectangular values 
    name <- paste(groupName, ".Rectangular.Values.png",sep="")
    
    png(name,width=1280,height=1024)
    plot(logFractionData$Value.R1 ~ logFractionData$Timestamp, type='l',xlim=c(minTime, maxTime),ylim = c(0,1), 
         col='green', axes=TRUE,xlab = "Time [ms]", ylab = "Fraction", main = paste(groupName," Rectangular (1) Value vs Time",sep=""))
    lines(logFractionData$Timestamp,logFractionData$Value.R2,col='red')
    legend("topright", legend=c("R1","R2"), col=c("green","red"),lwd=1.5)
    dev.off()
    
    
  }
}

#This function tries to answer the following question:
#Is there any location that is preferred to play the GO card? (All the activities)
continuousPositionPlots <- function(logdir){
  setwd (logdir)
  
  #For each file in logdir
  for(file in list.files(path=logdir,pattern = "\\.rda$")){
    data <- get(load(file))
    
    #We get the data of the position
    logGoPosition <- getContinuousPositiondata(data)
    
    #We plot the diferent manipulatives (continuous)
    groupName <- basename(file)
    groupName <-  substr(groupName, 1, nchar(groupName)-4)
    
    #Circular 1
    name <- paste(groupName,".C1.Position.png",sep="")
    XYPositionPlot (name , paste(groupName," C1 position",sep="") , groupName , logGoPosition$Position.C1.y, logGoPosition$Position.C1.x)
    
    #Circular 2
    name <- paste(groupName,".C2.Position.png",sep="")
    XYPositionPlot (name , paste(groupName," C2 position",sep="") , groupName , logGoPosition$Position.C2.y, logGoPosition$Position.C2.x)
    
    #Rectangular 1
    name <- paste(groupName,".R1.Position.png",sep="")
    XYPositionPlot (name , paste(groupName," R1 position",sep="") , groupName , logGoPosition$Position.R1.y, logGoPosition$Position.R1.x)
    
    #Rectangular 2
    name <- paste(groupName,".R2.Position.png",sep="")
    XYPositionPlot (name , paste(groupName," R2 position",sep="") , groupName , logGoPosition$Position.R2.y, logGoPosition$Position.R2.x)
    

    
    ### X and Y Over time for the two continuous representations
    continuousOverTime(logGoPosition, groupName)

    ### Resume plot with the rotation ###
    name <- paste(groupName, ".C1Position.Resume.png",sep="")
    mainTitle <- paste(groupName, ".C1 Position Resume",sep="")
    plotPositionRotationResume(name,groupName,mainTitle,logGoPosition$Position.C1.y,logGoPosition$Position.C1.x, logGoPosition$Rotation.C1)

    name <- paste(groupName, ".C2Position.Resume.png",sep="")
    mainTitle <- paste(groupName, ".C2 Position Resume",sep="")
    plotPositionRotationResume(name,groupName,mainTitle,logGoPosition$Position.C2.y,logGoPosition$Position.C2.x, logGoPosition$Rotation.C2)

    name <- paste(groupName, ".R1Position.Resume.png",sep="")
    mainTitle <- paste(groupName, ".R1 Position Resume",sep="")
    plotPositionRotationResume(name,groupName,mainTitle,logGoPosition$Position.R1.y,logGoPosition$Position.R1.x, logGoPosition$Rotation.R1)
    
    name <- paste(groupName, ".R2Position.Resume.png",sep="")
    mainTitle <- paste(groupName, ".R2 Position Resume",sep="") 
    plotPositionRotationResume(name,groupName,mainTitle,logGoPosition$Position.R2.y,logGoPosition$Position.R2.x, logGoPosition$Rotation.R2)

    
    
  }
}

#This function tries to answer the following question:
#Is there any location that is preferred to play the GO card? (All the activities)
goPositionPlots <- function(logdir){
  setwd (logdir)
  
  #For each file in logdir
  for(file in list.files(path=logdir,pattern = "\\.rda$")){
    data <- get(load(file))

    #We get the data of the position
    logGoPosition <- getGoPositiondata(data)
    
    #Get the name of the .png file
    groupName <- basename(file)
    groupName <-  substr(groupName, 1, nchar(groupName)-4)
    name <- paste(groupName,".GoPosition.png",sep="")
    
    #Create the .png file of the position
    #png(name,width=1280,height=1024)
    #plot(logGoPosition$Position.Go.y ~ logGoPosition$Position.Go.x, cex = 4,pch=1,xlim=c(0, 1280),ylim = c(768,0), col=rgb(red=0.2, green=0.2, blue=1.0, alpha=0.08), axes=TRUE,xlab = "X [px]", ylab = "Y [px]", main = paste(groupName," Go card position",sep=""))
    
    #We draw the circles in each corner
    #draw.circle(0,0,384)
    #draw.circle(0,768,384)
    #draw.circle(1280,0,384)
    #draw.circle(1280,768,384)
    
    #We draw the map area, so we can see where was the Go card
    #rect(384,640,896,128, border = "blue")
    #dev.off()
    
    name <- paste(groupName, ".GoPosition.Over.Time.png",sep="")
    
    #Create the .png file of the position over time
    #png(name,width=1280,height=1024)
    #par(mfrow=c(2,1))
    #minTime <- min(logGoPosition$Timestamp)
    #maxTime <- max(logGoPosition$Timestamp)
    #plot(logGoPosition$Position.Go.x ~ logGoPosition$Timestamp,type='l', xlim = c(minTime,maxTime), ylim = c(0,1280), col='blue',axes = TRUE, xlab = "Time [ms]", ylab = "X position [px]", main = paste(groupName," Go card X position vs Time", sep = ""))
    #plot(logGoPosition$Position.Go.y ~ logGoPosition$Timestamp,type='l', xlim = c(minTime,maxTime), ylim = c(768,0), col='blue',axes = TRUE, xlab = "Time [ms]", ylab = "Y position [px]", main = paste(groupName," Go card Y position vs Time", sep = ""))
    #dev.off()
    
    ##### Resume plot with the rotation
    #name <- paste(groupName, ".GoPosition.Resume.png",sep="")
    #png(name,width=1280,height=1024)
    #plot(logGoPosition$Position.Go.y ~ logGoPosition$Position.Go.x, pch=21,cex = 4,xlim=c(0, 1280),ylim = c(768,0), col=rgb(red=1.0, green=1.0, blue=0.3, alpha=0.08),bg =rgb(red=1.0, green=1.0, blue=0.3, alpha=0.08) , axes=TRUE,xlab = "X [px]", ylab = "Y [px]", main = paste(groupName," Go card position",sep=""))
    
    #We draw the circles in each corner
    #draw.circle(0,0,384,col = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.01))
    #draw.circle(0,768,384,col = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.01))
    #draw.circle(1280,0,384,col = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.01))
    #draw.circle(1280,768,384,col = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.01))
    
    #We draw the map area, so we can see where was the Go card
    #rect(384,640,896,128, border = "black")
    
    
    #We draw the Arrows
    #logGoPosition <- getGoPositiondata(data)
    #length <- length(logGoPosition$Rotation.Go)
    #side = numeric(length)
    #mean = numeric(length)
    
    #logGoRotation <- data.frame(logGoPosition$Rotation.Go,logGoPosition$Timestamp,side,mean)
    
    #side.1 <- sum(logGoPosition$Rotation.Go < pi/8 | logGoPosition$Rotation.Go > 15*pi/8)
    #side.2 <- sum(logGoPosition$Rotation.Go < 3*pi/8 & logGoPosition$Rotation.Go > pi/8)
    #side.3 <- sum(logGoPosition$Rotation.Go < 5*pi/8 & logGoPosition$Rotation.Go > 3*pi/8)
    #side.4 <- sum(logGoPosition$Rotation.Go < 7*pi/8 & logGoPosition$Rotation.Go > 5*pi/8)
    #side.5 <- sum(logGoPosition$Rotation.Go < 9*pi/8 & logGoPosition$Rotation.Go > 7*pi/8)
    #side.6 <- sum(logGoPosition$Rotation.Go < 11*pi/8 & logGoPosition$Rotation.Go > 9*pi/8)
    #side.7 <- sum(logGoPosition$Rotation.Go < 13*pi/8 & logGoPosition$Rotation.Go > 11*pi/8)
    #side.8 <- sum(logGoPosition$Rotation.Go < 15*pi/8 & logGoPosition$Rotation.Go > 13*pi/8)
    #total <- side.1+side.2+side.3+side.4+side.5+side.6+side.7+side.8
    #side.1.mean <- side.1/total
    #side.2.mean <- side.2/total
    #side.3.mean <- side.3/total
    #side.4.mean <- side.4/total
    #side.5.mean <- side.5/total
    #side.6.mean <- side.6/total
    #side.7.mean <- side.7/total
    #side.8.mean <- side.8/total
    
    #arrows(640, 384, 640, 600 ,length = 0.5, lwd=20*side.1.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 1
    #arrows(640, 384, 856, 600 ,length = 0.5, lwd=20*side.2.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 2
    #arrows(640, 384, 856, 384 ,length = 0.5, lwd=20*side.3.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 3
    #arrows(640, 384, 856, 168 ,length = 0.5, lwd=20*side.4.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 4
    #arrows(640, 384, 640, 168 ,length = 0.5, lwd=20*side.5.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 5
    #arrows(640, 384, 424, 168 ,length = 0.5, lwd=20*side.6.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 6
    #arrows(640, 384, 424, 384 ,length = 0.5, lwd=20*side.7.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 7
    #arrows(640, 384, 424, 600 ,length = 0.5, lwd=20*side.8.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 8
    #dev.off()
    
    ##### Resume plot with the density of the points #######
    name <- paste(groupName, ".GoPosition.Mean.png",sep="")
    png(name,width=1280,height=1024)
    plot(logGoPosition$Position.Go.y ~ logGoPosition$Position.Go.x, xlim=c(0, 1280),ylim = c(768,0), axes=TRUE,xlab = "X [px]", ylab = "Y [px]", main = paste(groupName," Go card position",sep=""))
    abline(h = median(na.omit(logGoPosition$Position.Go.y)), untf = FALSE,col = rgb(red=1.0,green=0.0,blue=0.0,alpha=0.3))
    abline(v = median(na.omit(logGoPosition$Position.Go.x)), untf = FALSE,col = rgb(red=1.0,green=0.0,blue=0.0,alpha=0.3))
    abline(v = 1280/2, untf = FALSE,col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.3))
    abline(h = 768/2, untf = FALSE,col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.3))
    
    #We draw the circles in each corner
    draw.circle(0,0,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
    draw.circle(0,768,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
    draw.circle(1280,0,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
    draw.circle(1280,768,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
    
    #We draw the map area, so we can see where was the Go card
    rect(384,640,896,128, border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
    
    dev.off()
    
    ##### Resume plot with the density of each cuadrant #######
    name <- paste(groupName, ".GoPosition.Cuadrant.Distribution.png",sep="")
    png(name,width=1280,height=1024)
   
    leftX <- logGoPosition$Position.Go.x[which(logGoPosition$Position.Go.x < 1280/2)]
    leftXY <- logGoPosition$Position.Go.y[which(logGoPosition$Position.Go.x < 1280/2)]
    rightX <- logGoPosition$Position.Go.x[which(logGoPosition$Position.Go.x > 1280/2)]
    rightXY <- logGoPosition$Position.Go.y[which(logGoPosition$Position.Go.x > 1280/2)]
    
    cuadrant <- numeric(4)
  
    cuadrant[[1]] <- length(rightXY[which(rightXY < 768/2)])
    cuadrant[[2]] <- length(leftXY[which(leftXY < 768/2)])
    cuadrant[[4]] <- length(rightXY[which(rightXY > 768/2)])
    cuadrant[[3]] <- length(leftXY[which(leftXY > 768/2)])
    total <- sum(cuadrant)
    
    cuadrantProportion <- numeric(4)
    cuadrantProportion[[1]] <- cuadrant[[1]]/total
    cuadrantProportion[[2]] <- cuadrant[[2]]/total
    cuadrantProportion[[3]] <- cuadrant[[3]]/total
    cuadrantProportion[[4]] <- cuadrant[[4]]/total
    
    plot(cuadrantProportion,xlim= c(1,4),ylim = c(0,1),type="b",main = paste(groupName," Go card distribution in cuadrant",sep=""), xlab = "Cuadrant", ylab = "Proportion")
    
    dev.off()
    
    ##### Resume plot with the SD #######
    name <- paste(groupName, ".GoPosition.SD.png",sep="")
    png(name,width=1280,height=1024)
    SDX <- sd(logGoPosition$Position.Go.x)
    SDY <- sd(logGoPosition$Position.Go.y)
    plot(logGoPosition$Position.Go.y ~ logGoPosition$Position.Go.x, xlim=c(0, 1280),ylim = c(768,0), axes=TRUE,xlab = "X [px]", ylab = "Y [px]", main = paste(groupName," Go card position",sep=""))
    abline(h = median(na.omit(logGoPosition$Position.Go.y)) - SDY, untf = FALSE,col = "green")
    abline(h = median(na.omit(logGoPosition$Position.Go.y)) + SDY, untf = FALSE,col = "green")
    abline(v = median(na.omit(logGoPosition$Position.Go.x)) - SDX, untf = FALSE,col = "green")
    abline(v = median(na.omit(logGoPosition$Position.Go.x)) + SDX, untf = FALSE,col = "green")
    legend("topright", legend=c(paste("STD X axis ",SDX,sep=""), paste("STD Y axis ",SDY,sep="")), col="green", lty=1,lwd=1.5) 
    
    
    #We draw the circles in each corner
    draw.circle(0,0,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
    draw.circle(0,768,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
    draw.circle(1280,0,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
    draw.circle(1280,768,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
    
    #We draw the map area, so we can see where was the Go card
    rect(384,640,896,128, border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
    dev.off()
    
    
  }
}

# This function returns a data frame with a summary of the Log files, describing the usage that each group made 
# of the different paper elements (manipulatives, hints, etc)
getAct4UsageLogSummary <- function(logdir,mapsData){
  
  # We get the basic, overall summaries
  logSummary <- getLogSummaries(logdir)
  
  # We merge the summary with the map data
  mergedData <- merge(mapsData,logSummary,by="Group.Name",all=TRUE)
  # We get the subset of groups for which we have meaningful map performance data
  completeData <- mergedData[!is.na(mergedData$A4_D),]
  
  # Since the logs and video coding may have discrepancies in the values for the duration of the activities,
  # we approximate it by projecting the video-based durations (A1_S, A4_E) into the range of the logs we have
  # (Log.Start, Total.Duration), and we get the equivalence of Activity 4 limits in log timestamps 
  completeData$Approx.Act4.Duration <- ((completeData$A4_E - completeData$A4_S)*completeData$Total.Duration)/(completeData$A4_E - completeData$A1_S)
  completeData$Approx.Act4.Start <- (completeData$Log.Start+completeData$Total.Duration-completeData$Approx.Act4.Duration)
  completeData$Approx.Act4.End <- (completeData$Log.Start+completeData$Total.Duration)
  # We get the usage statistics taking into account only (approximate) Activity 4 time
  completeData <- getRangedLogSummaries(logdir,completeData,completeData$Approx.Act4.Start,completeData$Approx.Act4.End)
  
  # We merge back the additional Act4 data with the log summary
  data <- merge(mergedData,completeData,all=TRUE)
  data
}

# This function gets summaries of the logs (during how many log samples a certain element was on the table)
# within a concrete range of time (e.g., in Activity 4 when they had free choice of elements)
getRangedLogSummaries <- function(logdir, globaldata, starts, ends){
  
  setwd(logdir)
  
  num_groups <- length(globaldata$Group.Name)
  
  # We pre-create the vectors for the different metrics we're going to calculate
  Using.Cont.Act4 = numeric(num_groups)
  Using.Disc.Act4 = numeric(num_groups)
  Using.Frac.Act4 = numeric(num_groups)
  Using.Concr.Act4 = numeric(num_groups)
  Help.Circ.Act4 = numeric(num_groups)
  Help.Rect.Act4 = numeric(num_groups)
  Help.Disc.Act4 = numeric(num_groups)
  Help.Dec.Act4 = numeric(num_groups)
  Help.Frac.Act4 = numeric(num_groups)
  Relative.Using.Cont.Act4 = numeric(num_groups)
  Relative.Using.Disc.Act4 = numeric(num_groups)
  Relative.Using.Frac.Act4 = numeric(num_groups)
  Relative.Using.Concr.Act4 = numeric(num_groups)
  Relative.Help.Circ.Act4 = numeric(num_groups)
  Relative.Help.Rect.Act4 = numeric(num_groups)
  Relative.Help.Disc.Act4 = numeric(num_groups)
  Relative.Help.Dec.Act4 = numeric(num_groups)
  Relative.Help.Frac.Act4 = numeric(num_groups)
  
  # We add variables regarding the sole or combined usage of representations within Act 4
  Relative.Usage.None.Act4 = numeric(num_groups)
  Relative.Usage.Sole.Cont.Act4 = numeric(num_groups)
  Relative.Usage.Sole.Disc.Act4 = numeric(num_groups)
  Relative.Usage.Sole.Frac.Act4 = numeric(num_groups)
  Relative.Usage.Cont.Disc.Act4 = numeric(num_groups)
  Relative.Usage.Disc.Frac.Act4 = numeric(num_groups)
  Relative.Usage.Frac.Cont.Act4 = numeric(num_groups)
  Relative.Usage.All.Act4 = numeric(num_groups)
  
  
  for(i in 1:num_groups){
    # We open the file for each group
    data <- get(load(paste(globaldata$Group.Name[[i]],".rda",sep="")))
    # We subset the range of timestamps that we need the stats for
    data <- data[(data$timestamp>=starts[i] & data$timestamp<=ends[i]),]
    data <- addElementUsageVariables(data)
    
    Using.Cont.Act4[i] <- sum(data$UsingCont)
    Using.Disc.Act4[i] <- sum(data$UsingDisc)
    Using.Frac.Act4[i] <- sum(data$UsingFrac)
    Using.Concr.Act4[i] <- sum(data$UsingCont | data$UsingDisc)
    Help.Circ.Act4[i] <- sum(data$HelpContCirc)
    Help.Rect.Act4[i] <- sum(data$HelpContRect)
    Help.Disc.Act4[i] <- sum(data$HelpDiscrete)
    Help.Dec.Act4[i] <- sum(data$HelpDecimal)
    Help.Frac.Act4[i] <- sum(data$HelpFraction)
    
    num_samples <- length(data$timestamp)
    
    Relative.Using.Cont.Act4[i] <- Using.Cont.Act4[i]/num_samples
    Relative.Using.Disc.Act4[i] <- Using.Disc.Act4[i]/num_samples
    Relative.Using.Frac.Act4[i] <- Using.Frac.Act4[i]/num_samples
    Relative.Using.Concr.Act4[i] <- Using.Concr.Act4[i]/num_samples
    Relative.Help.Circ.Act4[i] <- Help.Circ.Act4[i]/num_samples
    Relative.Help.Rect.Act4[i] <- Help.Rect.Act4[i]/num_samples
    Relative.Help.Disc.Act4[i] <- Help.Disc.Act4[i]/num_samples
    Relative.Help.Dec.Act4[i] <- Help.Dec.Act4[i]/num_samples
    Relative.Help.Frac.Act4[i] <- Help.Frac.Act4[i]/num_samples
    
    Relative.Usage.None.Act4[i] = sum(!data$UsingCont & !data$UsingDisc & !data$UsingFrac)/num_samples
    Relative.Usage.Sole.Cont.Act4[i] = sum(data$UsingCont & !data$UsingDisc & !data$UsingFrac)/num_samples
    Relative.Usage.Sole.Disc.Act4[i] = sum(!data$UsingCont & data$UsingDisc & !data$UsingFrac)/num_samples
    Relative.Usage.Sole.Frac.Act4[i] = sum(!data$UsingCont & !data$UsingDisc & data$UsingFrac)/num_samples
    Relative.Usage.Cont.Disc.Act4[i] = sum(data$UsingCont & data$UsingDisc & !data$UsingFrac)/num_samples
    Relative.Usage.Disc.Frac.Act4[i] = sum(!data$UsingCont & data$UsingDisc & data$UsingFrac)/num_samples
    Relative.Usage.Frac.Cont.Act4[i] = sum(data$UsingCont & !data$UsingDisc & data$UsingFrac)/num_samples
    Relative.Usage.All.Act4[i] = sum(data$UsingCont & data$UsingDisc & data$UsingFrac)/num_samples
    
  }
  
  # the log data summary for each group
  logSummary <- cbind(globaldata, 
                      Using.Cont.Act4,
                      Using.Disc.Act4,
                      Using.Frac.Act4,
                      Using.Concr.Act4,
                      Help.Circ.Act4,
                      Help.Rect.Act4,
                      Help.Disc.Act4,
                      Help.Dec.Act4,
                      Help.Frac.Act4,
                      Relative.Using.Cont.Act4,
                      Relative.Using.Disc.Act4,
                      Relative.Using.Frac.Act4,
                      Relative.Using.Concr.Act4,
                      Relative.Help.Circ.Act4,
                      Relative.Help.Rect.Act4,
                      Relative.Help.Disc.Act4,
                      Relative.Help.Dec.Act4,
                      Relative.Help.Frac.Act4,
                      Relative.Usage.None.Act4,
                      Relative.Usage.Sole.Cont.Act4,
                      Relative.Usage.Sole.Disc.Act4,
                      Relative.Usage.Sole.Frac.Act4,
                      Relative.Usage.Cont.Disc.Act4,
                      Relative.Usage.Disc.Frac.Act4,
                      Relative.Usage.Frac.Cont.Act4,
                      Relative.Usage.All.Act4)
  
  logSummary
  
  
}

# Gets the data that corresponds the 
getFractionValueData <- function(data){
  
  fileLength <- length(data$timestamp)

  #We need the value of each continuous representation: C1, C2, R1 and R2
  Value.C1 = numeric(fileLength) #Circular 1
  Value.C2 = numeric(fileLength) #Circular 2
  Value.R1 = numeric(fileLength) #Rectangular 1
  Value.R2 = numeric(fileLength) #Rectangular 2
  Timestamp = numeric(fileLength) #To see the evolution on time
  
  #For each row in the file
  for(i in 1:fileLength){
    Value.C1[[i]] <- data[i,"Value.C1"]
    Value.C2[[i]] <- data[i,"Value.C2"]
    Value.R1[[i]] <- data[i,"Value.R1"]
    Value.R2[[i]] <- data[i,"Value.R2"]
    Timestamp[[i]] <- data[i,"timestamp"]
    
  }
  
  fractionValueLog <- data.frame(Value.C1,Value.C2,Value.R1,Value.R2,Timestamp)
  
  #fractionValueLogClean <- na.omit(fractionValueLog)
  fractionValueLog
}

# Gets the data of Go card: position x, y and rotation
getGoPositiondata <- function(data){
   
  fileLength <- length(data$timestamp)
  #cat(str(data))
 
  Position.Go.x = numeric(fileLength) #Go card
  Position.Go.y = numeric(fileLength) #Go card
  Timestamp = numeric(fileLength) #To see the evolution on time
  Rotation.Go = numeric(fileLength) #The rotation
  
  #For each row in the file
  for(i in 1:fileLength){
    Position.Go.x[[i]] <- data[i,"Position.Gox"]
    Position.Go.y[[i]] <- data[i,"Position.Goy"]

    Timestamp[[i]] <- data[i,"timestamp"]
    Rotation.Go[[i]] <- data[i,"Rotation.Go"]
  }
    
  act4Log <- data.frame(Position.Go.x, Position.Go.y,Timestamp,Rotation.Go)
  
  act4LogClean <- na.omit(act4Log)
  act4LogClean
}

# Gets the data of Continuous representation: position x, y and rotation
getContinuousPositiondata <- function(data){
  
  fileLength <- length(data$timestamp)
  #cat(str(data))
  
  Position.C1.x = numeric(fileLength) #C1
  Position.C1.y = numeric(fileLength) #C1
  Position.C2.x = numeric(fileLength) #C2
  Position.C2.y = numeric(fileLength) #C2
  Position.R1.x = numeric(fileLength) #R1
  Position.R1.y = numeric(fileLength) #R1
  Position.R2.x = numeric(fileLength) #R2
  Position.R2.y = numeric(fileLength) #R2
  Rotation.C1 = numeric(fileLength) #The rotation of C1
  Rotation.C2 = numeric(fileLength) #The rotation of C2
  Rotation.R1 = numeric(fileLength) #The rotation of R1
  Rotation.R2 = numeric(fileLength) #The rotation of R2
  Timestamp = numeric(fileLength) #To see the evolution on time
    
  #For each row in the file
  for(i in 1:fileLength){
    Position.C1.x[[i]] <- data[i,"Position.C1x"]
    Position.C1.y[[i]] <- data[i,"Position.C1y"]
    Position.C2.x[[i]] <- data[i,"Position.C2x"]
    Position.C2.y[[i]] <- data[i,"Position.C2y"]
    Position.R1.x[[i]] <- data[i,"Position.R1x"]
    Position.R1.y[[i]] <- data[i,"Position.R1y"]
    Position.R2.x[[i]] <- data[i,"Position.R2x"]
    Position.R2.y[[i]] <- data[i,"Position.R2y"]
    Rotation.C1[[i]] <- data[i,"Rotation.C1"]
    Rotation.C2[[i]] <- data[i,"Rotation.C2"]
    Rotation.R1[[i]] <- data[i,"Rotation.R1"]
    Rotation.R2[[i]] <- data[i,"Rotation.R2"]
       
    
    Timestamp[[i]] <- data[i,"timestamp"]
  }
  
   
  act4Log <- data.frame(Position.C1.x,Position.C1.y,Position.C2.x,Position.C2.y,Position.R1.x,Position.R1.y,Position.R2.x,Position.R2.y,Rotation.C1,Rotation.C2,Rotation.R1,Rotation.R2,Timestamp)
  
  act4LogClean <- na.omit(act4Log)
  act4LogClean
}

# This function gets basic log statistics for each group (e.g. total duration available, 
# total samples available, samples with each kind of representation on table, samples 
# with each kind of help on table, in absolute samples and percentage of existing samples)
getLogSummaries <- function(rootDir){
  setwd(rootDir)
  
  files <- list.files(pattern = "\\.rda$")
  
  # We pre-create the vectors for the different metrics
  Group.Name=character(length(files))
  Total.Duration = numeric(length(files))
  Total.Samples = numeric(length(files))
  Using.Cont = numeric(length(files))
  Using.Disc = numeric(length(files))
  Using.Frac = numeric(length(files))
  Help.Circ = numeric(length(files))
  Help.Rect = numeric(length(files))
  Help.Disc = numeric(length(files))
  Help.Dec = numeric(length(files))
  Help.Frac = numeric(length(files))
  Relative.Using.Cont = numeric(length(files))
  Relative.Using.Disc = numeric(length(files))
  Relative.Using.Frac = numeric(length(files))
  Relative.Help.Circ = numeric(length(files))
  Relative.Help.Rect = numeric(length(files))
  Relative.Help.Disc = numeric(length(files))
  Relative.Help.Dec = numeric(length(files))
  Relative.Help.Frac = numeric(length(files))
  Log.Start = numeric(length(files))
  
  for(i in 1:length(files)){
    data <- get(load(files[i]))
    data <- addElementUsageVariables(data)
    
    Group.Name[i] <- substr(files[i],1,4)
    Total.Duration[i] <- max(data$timestamp)-min(data$timestamp)
    Total.Samples[i] <- length(data$timestamp)
    Using.Cont[i] <- sum(data$UsingCont)
    Using.Disc[i] <- sum(data$UsingDisc)
    Using.Frac[i] <- sum(data$UsingFrac)
    Help.Circ[i] <- sum(data$HelpContCirc)
    Help.Rect[i] <- sum(data$HelpContRect)
    Help.Disc[i] <- sum(data$HelpDiscrete)
    Help.Dec[i] <- sum(data$HelpDecimal)
    Help.Frac[i] <- sum(data$HelpFraction)
    Relative.Using.Cont[i] <- Using.Cont[i]/Total.Samples[i]
    Relative.Using.Disc[i] <- Using.Disc[i]/Total.Samples[i]
    Relative.Using.Frac[i] <- Using.Frac[i]/Total.Samples[i]
    Relative.Help.Circ[i] <- Help.Circ[i]/Total.Samples[i]
    Relative.Help.Rect[i] <- Help.Rect[i]/Total.Samples[i]
    Relative.Help.Disc[i] <- Help.Disc[i]/Total.Samples[i]
    Relative.Help.Dec[i] <- Help.Dec[i]/Total.Samples[i]
    Relative.Help.Frac[i] <- Help.Frac[i]/Total.Samples[i]    
    Log.Start[i] <- min(data$timestamp)
  }
  
  # the log data summary for each group
  logSummary <- data.frame(Group.Name, 
                           Total.Duration, 
                           Total.Samples, 
                           Using.Cont,
                           Using.Disc,
                           Using.Frac,
                           Help.Circ,
                           Help.Rect,
                           Help.Disc,
                           Help.Dec,
                           Help.Frac,
                           Relative.Using.Cont,
                           Relative.Using.Disc,
                           Relative.Using.Frac,
                           Relative.Help.Circ,
                           Relative.Help.Rect,
                           Relative.Help.Disc,
                           Relative.Help.Dec,
                           Relative.Help.Frac,
                           Log.Start)
  
  logSummary
}

# Adds some basic usage variables (is a certain kind of element present? boolean) to a data frame
addElementUsageVariables <- function(data){
  
  # Representation usage (number of samples in which a representation is present)
  data$UsingCont <- (data$C1!="0" | data$C2!="0" | data$R1!="0" | data$R2!="0")
  data$UsingDisc <- (data$Token1!="0" | data$Token2!="0" | data$Token3!="0" | data$Token4!="0")
  data$UsingFrac <- (data$Fraction12!="0" | 
                       data$Fraction13!="0" | data$Fraction23!="0" | 
                       data$Fraction14!="0" | data$Fraction24!="0" | data$Fraction34!="0" | 
                       data$Fraction15!="0" | data$Fraction25!="0" | data$Fraction35!="0" | data$Fraction45!="0" |
                       data$Fraction16!="0" | data$Fraction26!="0" | data$Fraction36!="0" | data$Fraction46!="0" | data$Fraction56!="0" |
                       data$Fraction110!="0" | data$Fraction210!="0" | data$Fraction310!="0" | data$Fraction410!="0" | data$Fraction510!="0" | data$Fraction610!="0" | data$Fraction710!="0" | data$Fraction810!="0" | data$Fraction910!="0")
  
  data$HelpContCirc <- (data$CircularHint!="0")
  data$HelpContRect <- (data$RectangularHint!="0")
  data$HelpDiscrete <- (data$DiscreteHint!="0")
  data$HelpDecimal <- (data$DecimalHint!="0")
  data$HelpFraction <- (data$FractionHint!="0")
  
  return(data)
}

#############     General Functions      #############

# It plots yVector~xVector for the group name groupName, it's necessary to know the image name and 
# the main title of the vector. It also displays the interface of the table (e.g. corner circles,
# and map area) so it's more easy to understand the position.
XYPositionPlot <- function(imageName , mainTitle , groupName , yVector, xVector){
  
  #We create the .png file
  png(imageName,width=1280,height=1024)
  plot(yVector ~ xVector, color = "blue", xlim = c(0,1280), ylim = c(768,0), 
       axes=TRUE,xlab = "X [px]", ylab = "Y [px]", main = paste(groupName, " " ,mainTitle,sep=""))
  
  #Draw the circles of each corner
  draw.circle(0,0,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
  draw.circle(0,768,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
  draw.circle(1280,0,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
  draw.circle(1280,768,384,border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
  
  #We draw the map area, so we can see where was the card
  rect(384,640,896,128, border = rgb(red=0.0,green=0.0,blue=0.0,alpha = 0.3))
  dev.off()
}

# This function plots the resume of position (yVector~xVector) and also the rotation of
# the corresponding manipulative. It's necessary the image, main title and group name
plotPositionRotationResume <- function (imageName,groupName,mainTitle,yVector,xVector,rotationVector){
  
  # First we calculate the number of times that the rotation is in every cuadrant
  side.1 <- sum(rotationVector < pi/8 | rotationVector > 15*pi/8)
  side.2 <- sum(rotationVector < 3*pi/8 & rotationVector > pi/8)
  side.3 <- sum(rotationVector < 5*pi/8 & rotationVector > 3*pi/8)
  side.4 <- sum(rotationVector < 7*pi/8 & rotationVector > 5*pi/8)
  side.5 <- sum(rotationVector < 9*pi/8 & rotationVector > 7*pi/8)
  side.6 <- sum(rotationVector < 11*pi/8 & rotationVector > 9*pi/8)
  side.7 <- sum(rotationVector < 13*pi/8 & rotationVector > 11*pi/8)
  side.8 <- sum(rotationVector < 15*pi/8 & rotationVector > 13*pi/8)
  
  total <- side.1+side.2+side.3+side.4+side.5+side.6+side.7+side.8
  
  # In order to compare, we calculate the mean 
  side.1.mean <- side.1/total
  side.2.mean <- side.2/total
  side.3.mean <- side.3/total
  side.4.mean <- side.4/total
  side.5.mean <- side.5/total
  side.6.mean <- side.6/total
  side.7.mean <- side.7/total
  side.8.mean <- side.8/total
  
  #We set the parameters of the arrowa
  length <- length(na.omit(rotationVector))
  
  #We create the image
  png(imageName,width=1280,height=1024)
  plot(yVector ~ xVector, pch=21,cex = 4,xlim=c(0, 1280),ylim = c(768,0), 
       col=rgb(red=1.0, green=1.0, blue=0.3, alpha=0.08),bg =rgb(red=1.0, green=1.0, blue=0.3, alpha=0.08) , 
       axes=TRUE,xlab = "X [px]", ylab = "Y [px]", main = mainTitle, na.action = na.omit)

  arrows(640, 384, 640, 600 ,length = 0.5, lwd=20*side.1.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 1
  arrows(640, 384, 856, 600 ,length = 0.5, lwd=20*side.2.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 2
  arrows(640, 384, 856, 384 ,length = 0.5, lwd=20*side.3.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 3
  arrows(640, 384, 856, 168 ,length = 0.5, lwd=20*side.4.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 4
  arrows(640, 384, 640, 168 ,length = 0.5, lwd=20*side.5.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 5
  arrows(640, 384, 424, 168 ,length = 0.5, lwd=20*side.6.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 6
  arrows(640, 384, 424, 384 ,length = 0.5, lwd=20*side.7.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 7
  arrows(640, 384, 424, 600 ,length = 0.5, lwd=20*side.8.mean, code = 2, col = rgb(red=0.3,green=0.0,blue=1.0,alpha=0.8), lty = NULL, xpd = FALSE) #Side 8
  
  dev.off()
}

# It returns the average of the vector
getAvg <- function(vector){
  average <- sum(na.omit(vector))/length(na.omit(vector))
}

###  Continuous Functions   ###

continuousOverTime <- function(data, groupName){
  #We calculate the min and max time, for xlim
  minTime <- min(data$Timestamp)
  maxTime <- max(data$Timestamp)
  
  #Start with the circular ones
  name <- paste(groupName, ".Circular.Position.Over.Time.png",sep="")
  png(name,width=1280,height=1024)
  par(mfrow=c(2,1))
  
  plot(data$Position.C1.x ~ data$Timestamp,type='l', xlim = c(minTime,maxTime), ylim = c(0,1280), col='blue',axes = TRUE, 
       xlab = "Time [ms]", ylab = " Y Position [px]", main = paste(groupName," X position vs Time", sep = ""))
  points(data$Position.C2.x ~ data$Timestamp,type='l', xlim = c(minTime,maxTime), ylim = c(0,1280), col='red',axes = TRUE)
  legend("topright", legend=c("C1","C2"), col=c("blue","red"),lwd=1.5)
  
  plot (data$Position.C1.y ~ data$Timestamp,type='l', xlim = c(minTime,maxTime), ylim = c(768,0), col='blue',axes = TRUE,
        xlab = "Time [ms]", ylab = " Y Position [px]", main = paste(groupName," Y position vs Time", sep = ""))
  points(data$Position.C2.y ~ data$Timestamp,type='l', xlim = c(minTime,maxTime), ylim = c(768,0), col='red',axes = TRUE)    
  legend("topright", legend=c("C1","C2"), col=c("blue","red"),lwd=1.5)
  
  dev.off()
  
  #Create the .png file of the position over time for Rectangular
  name <- paste(groupName, ".Rectangular.Position.Over.Time.png",sep="")
  png(name,width=1280,height=1024)
  par(mfrow=c(2,1))

  plot(data$Position.R1.x ~ data$Timestamp,type='l', xlim = c(minTime,maxTime), ylim = c(0,1280), col='blue',axes = TRUE, 
       xlab = "Time [ms]", ylab = "Position [px]", main = paste(groupName," X position vs Time", sep = ""))
  points(data$Position.R2.x ~ data$Timestamp,type='l', xlim = c(minTime,maxTime), ylim = c(0,1280), col='red',axes = TRUE)
  legend("topright", legend=c("R1","R2"), col=c("blue","red"),lwd=1.5)
  

  plot(data$Position.R1.y ~ data$Timestamp,type='l', xlim = c(minTime,maxTime), ylim = c(768,0), col='red',axes = TRUE,
       xlab = "Time [ms]", ylab = "Position [px]", main = paste(groupName," Y position vs Time", sep = ""))
  points(data$Position.R2.y ~ data$Timestamp,type='l', xlim = c(minTime,maxTime), ylim = c(768,0), col='blue',axes = TRUE)    
  legend("topright", legend=c("R1","R2"), col=c("blue","red"),lwd=1.5)
  
  dev.off()
}

###   Hint Functions  ###

# This function plots the position of all the Hint cards, and also gives the mean of each one
positionMeanHint <- function(data,groupName){
  #We get the average 
  avgH1y <- getAvg(data$Position.H1.y)
  avgH1x <- getAvg(data$Position.H1.x)
  
  avgH2y <- getAvg(data$Position.H2.y)
  avgH2x <- getAvg(data$Position.H2.x)
  
  avgH3y <- getAvg(data$Position.H3.y)
  avgH3x <- getAvg(data$Position.H3.x)
  
  avgH4y <- getAvg(data$Position.H4.y)
  avgH4x <- getAvg(data$Position.H4.x)
  
  avgH5y <- getAvg(data$Position.H5.y)
  avgH5x <- getAvg(data$Position.H5.x)
  
  name <- paste(groupName,"Resume.Hint.Card.Position.Median.png",sep="")
  png(name,width=1280,height=1024)
  plot(data$Position.H1.y ~ data$Position.H1.x, col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.5), xlim = c(0,1280), ylim = c(768,0), axes=TRUE,xlab = "X [px]", ylab = "Y [px]", main = paste(groupName," Hint card position",sep=""),cex=3,pch = 1)
  points(data$Position.H2.y ~ data$Position.H2.x, col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.5), xlim = c(0,1280), ylim = c(768,0), axes=TRUE,xlab = "X [px]", ylab = "Y [px]", cex=3, pch = 2)
  points(data$Position.H3.y ~ data$Position.H3.x, col = rgb(red=1.0,green=0.0,blue=0.0,alpha=0.5), xlim = c(0,1280), ylim = c(768,0), axes=TRUE,xlab = "X [px]", ylab = "Y [px]", cex=3)
  points(data$Position.H4.y ~ data$Position.H4.x, col = rgb(red=0.0,green=1.0,blue=0.0,alpha=0.5), xlim = c(0,1280), ylim = c(768,0), axes=TRUE,xlab = "X [px]", ylab = "Y [px]", cex=3,pch = 1)
  points(data$Position.H5.y ~ data$Position.H5.x, col = rgb(red=0.0,green=1.0,blue=0.0,alpha=0.5), xlim = c(0,1280), ylim = c(768,0), axes=TRUE,xlab = "X [px]", ylab = "Y [px]", cex=3, pch = 2)
  legend("topright", legend=c("Discrete","Fraction","Circular","Rectangular","Decimal"), col=c("blue","blue","red","green","green"), lty=c(2,1,2, 1,2),lwd=1.5, pch = c(1,2,NA,1,2))
  
  abline(h = avgH1y, untf = FALSE,col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.5), lty = 2, lwd=3.5)
  abline(v = avgH1x, untf = FALSE,col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.5), lty = 2, lwd=3.5)
  abline(h = avgH2y, untf = FALSE,col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.5), lty = 1, lwd=3.5)
  abline(v = avgH2x, untf = FALSE,col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.5), lty = 1, lwd=3.5)
  abline(h = avgH3y, untf = FALSE,col = rgb(red=1.0,green=0.0,blue=0.0,alpha=0.5), lty = 2, lwd=3.5)
  abline(v = avgH3x, untf = FALSE,col = rgb(red=1.0,green=0.0,blue=0.0,alpha=0.5), lty = 2, lwd=3.5)
  abline(h = avgH4y, untf = FALSE,col = rgb(red=0.0,green=1.0,blue=0.0,alpha=0.5), lty = 1, lwd=3.5)
  abline(v = avgH4x, untf = FALSE,col = rgb(red=0.0,green=1.0,blue=0.0,alpha=0.5), lty = 1, lwd=3.5)
  abline(h = avgH5y, untf = FALSE,col = rgb(red=0.0,green=1.0,blue=0.0,alpha=0.5), lty = 2, lwd=3.5)
  abline(v = avgH5x, untf = FALSE,col = rgb(red=0.0,green=1.0,blue=0.0,alpha=0.5), lty = 2, lwd=3.5)
  
  abline(h = 768/2, untf = FALSE,col = rgb(red=0.0,green=0.0,blue=0.0,alpha=0.3))
  abline(v = 1280/2, untf = FALSE,col = rgb(red=0.0,green=0.0,blue=0.0,alpha=0.3))
  
  
  dev.off()
}

# As a resume, it plot the average position of the Hints, but taking into account the 
# proportion of time that each Hint was on the table
bubblePlotHint <- function(data,groupName){
  #First we calculate the average point of each Hint Card, we initialize the variables:
  avgX = numeric(5)
  avgY = numeric(5)
  radius = numeric(5)
  
  #We get the X and Y average positions:
  #Hint 1 - Discrete
  avgX[[1]] <- getAvg(data$Position.H1.x)
  avgY[[1]] <- getAvg(data$Position.H1.y)
  #Hint 2 - Fraction
  avgX[[2]] <- getAvg(data$Position.H2.x)
  avgY[[2]] <- getAvg(data$Position.H2.y)
  #Hint 3 - Circular
  avgX[[3]] <- getAvg(data$Position.H3.x)
  avgY[[3]] <- getAvg(data$Position.H3.y)
  #Hint 4 - Rectangular
  avgX[[4]] <- getAvg(data$Position.H4.x)
  avgY[[4]] <- getAvg(data$Position.H4.y)
  #Hint 5 - Decimal
  avgX[[5]] <- getAvg(data$Position.H5.x)
  avgY[[5]] <- getAvg(data$Position.H5.y)
  
  #Calculate the radius based on the number of appearances of each hint 
  radius[[1]] <- sqrt( length(na.omit(data$Position.H1.x))/ pi )
  radius[[2]] <- sqrt( length(na.omit(data$Position.H2.x))/ pi )
  radius[[3]] <- sqrt( length(na.omit(data$Position.H3.x))/ pi )
  radius[[4]] <- sqrt( length(na.omit(data$Position.H4.x))/ pi )
  radius[[5]] <- sqrt( length(na.omit(data$Position.H5.x))/ pi )
  
  #We create the .png file with the bubble chart
  name <- paste(groupName,"Resume.Hint.Card.Position.Resume.png",sep="")
  png(name,width=1280,height=1024)
  symbols(avgX,avgY - radius - 3,circles = radius, inches = 0.35, fg = "blue", bg = "white", 
          xlab = "X [px]", ylab = "Y [px]", xlim = c(0,1280),ylim = c(768,0), main = paste(groupName," Hint Card",sep=""))
  text(avgX, avgY, c("Discrete","Fraction","Circular","Rectangular","Decimal"), cex=0.8, col = "blue")
  abline(h = 768/2, untf = FALSE,col = rgb(red=0.0,green=0.0,blue=0.0,alpha=0.3))
  abline(v = 1280/2, untf = FALSE,col = rgb(red=0.0,green=0.0,blue=0.0,alpha=0.3))
  
  dev.off()
  
  
}

# It plots in one png both axes: X and Y over time
hintPositionOverTime <- function(data,groupName){
  
  #First we calculate the min and max values of time
  minTime <- min(data$Timestamp)
  maxTime <- max(data$Timestamp)
  
  name <- paste(groupName,"Resume.Hint.Card.Position.Over.Time.png",sep="")
  png(name,width=1280,height=1024)
  par(mfrow=c(2,1))
  plot(data$Position.H1.y ~ data$Timestamp, col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.5), xlim = c(minTime,maxTime), ylim = c(0,768), xlab = "Time [ms]", ylab = "Y [px]", main = paste(groupName," Hint card position [Y] over Time",sep=""),cex=2,pch = 1)
  points(data$Position.H2.y ~ data$Timestamp, col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.5), xlim = c(minTime,maxTime), ylim = c(0,768), cex=2, pch = 2)
  points(data$Position.H3.y ~ data$Timestamp, col = rgb(red=1.0,green=0.0,blue=0.0,alpha=0.5), xlim = c(minTime,maxTime), ylim = c(0,768), cex=2)
  points(data$Position.H4.y ~ data$Timestamp, col = rgb(red=0.0,green=1.0,blue=0.0,alpha=0.5), xlim = c(minTime,maxTime), ylim = c(0,768), cex=2,pch = 1)
  points(data$Position.H5.y ~ data$Timestamp, col = rgb(red=0.0,green=1.0,blue=0.0,alpha=0.5), xlim = c(minTime,maxTime), ylim = c(0,768), cex=2, pch = 2)
  legend("topright", legend=c("Discrete","Fraction","Circular","Rectangular","Decimal"), col=c("blue","blue","red","green","green"), lty=c(2,1,2, 1,2),lwd=1.5, pch = c(1,2,NA,1,2))
  
  plot(data$Position.H1.x ~ data$Timestamp, col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.5), xlim = c(minTime,maxTime), ylim = c(0,1280), xlab = "Time [ms]", ylab = "X [px]", main = paste(groupName," Hint card position [X] over Time",sep=""),cex=2,pch = 1)
  points(data$Position.H2.x ~ data$Timestamp, col = rgb(red=0.0,green=0.0,blue=1.0,alpha=0.5), xlim = c(minTime,maxTime), ylim = c(0,1280), cex=2, pch = 2)
  points(data$Position.H3.x ~ data$Timestamp, col = rgb(red=1.0,green=0.0,blue=0.0,alpha=0.5), xlim = c(minTime,maxTime), ylim = c(0,1280), cex=2)
  points(data$Position.H4.x ~ data$Timestamp, col = rgb(red=0.0,green=1.0,blue=0.0,alpha=0.5), xlim = c(minTime,maxTime), ylim = c(0,1280), cex=2,pch = 1)
  points(data$Position.H5.x ~ data$Timestamp, col = rgb(red=0.0,green=1.0,blue=0.0,alpha=0.5), xlim = c(minTime,maxTime), ylim = c(0,1280), cex=2, pch = 2)
  legend("topright", legend=c("Discrete","Fraction","Circular","Rectangular","Decimal"), col=c("blue","blue","red","green","green"), lty=c(2,1,2, 1,2),lwd=1.5, pch = c(1,2,NA,1,2))
  dev.off() 
}

# It returns a data frame with the data related to the Hint Cards: Position and Rotation
getHintPositiondata <- function(data){
  fileLength <- length(data$timestamp)
  
  #Hint 1 : Discrete
  #Hint 2 : Fraction
  #Hint 3 : Circular
  #Hint 4 : Rectangular
  #Hint 5 : Decimal 
  
  Timestamp = numeric(fileLength) #To see the evolution on time
  Position.H1.x = numeric(fileLength) #Hint 1 card 
  Position.H1.y = numeric(fileLength) #Hint 1 card 
  Rotation.H1 = numeric(fileLength) #Hint 1 rotation
  
  Position.H2.x = numeric(fileLength) #Hint 2 card
  Position.H2.y = numeric(fileLength) #Hint 2 card
  Rotation.H2 = numeric(fileLength) #Hint 2 rotation
  
  Position.H3.x = numeric(fileLength) #Hint 3 card
  Position.H3.y = numeric(fileLength) #Hint 3 card
  Rotation.H3 = numeric(fileLength) #Hint 3 rotation
  
  Position.H4.x = numeric(fileLength) #Hint 4 card
  Position.H4.y = numeric(fileLength) #Hint 4 card
  Rotation.H4 = numeric(fileLength) #Hint 4 rotation
  
  Position.H5.x = numeric(fileLength) #Hint 5 card
  Position.H5.y = numeric(fileLength) #Hint 5 card
  Rotation.H5 = numeric(fileLength) #Hint 5 rotation
  
  
  #For each row in the file
  for(i in 1:fileLength){
    
    Timestamp[[i]] <- data[i,"timestamp"]
    Position.H1.x[[i]] <- data[i,"Position.DiscreteHintx"] 
    Position.H1.y[[i]] <- data[i,"Position.DiscreteHinty"]
    Rotation.H1[[i]] <- data[i,"Rotation.DiscreteHint"]
    
    Position.H2.x[[i]] <- data[i,"Position.FractionHintx"]
    Position.H2.y[[i]] <- data[i,"Position.FractionHinty"]
    Rotation.H2[[i]] <- data[i,"Rotation.FractionHint"]
    
    Position.H3.x[[i]] <- data[i,"Position.CircularHintx"]
    Position.H3.y[[i]] <- data[i,"Position.CircularHinty"]
    Rotation.H3[[i]] <- data[i,"Rotation.CircularHint"]
    
    Position.H4.x[[i]] <- data[i,"Position.RectangularHintx"]
    Position.H4.y[[i]] <- data[i,"Position.RectangularHinty"]
    Rotation.H4[[i]] <- data[i,"Rotation.RectangularHint"]
    
    Position.H5.x[[i]] <- data[i,"Position.DecimalHintx"]
    Position.H5.y[[i]] <- data[i,"Position.DecimalHinty"]
    Rotation.H5[[i]] <- data[i,"Rotation.DecimalHint"]
    
  }
  
  act4Log <- data.frame(Timestamp,Position.H1.x,Position.H1.y,Rotation.H1,Position.H2.x,Position.H2.y,Rotation.H2, 
                        Position.H3.x,Position.H3.y,Rotation.H3,Position.H4.x,Position.H4.y,Rotation.H4,
                        Position.H5.x,Position.H5.y,Rotation.H5)
  
  act4Log
}
