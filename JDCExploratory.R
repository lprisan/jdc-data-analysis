library("lattice")
library("ggplot2")
library("Gmisc")

# This is the global function that runs the whole set of exploratory data analyses
JDCExplorations <- function(){
  
  setwd("/home/lprisan/workspace/jdc-data-analysis/logs")
  
  #this object will contain the summaries of log data for 
  logSummary <- data.frame()
  
  # Do the samples available, relative usage of representations and helps per group
  for(file in list.files(pattern = "\\.rda$")){
    logPointsInTime(getwd(),file)
    logSamplesElementsPresent(getwd(),file)
  }
  
  # Do a multi-graph panel for the temporal evolution of usage of each kind of card and each group, for a session
  logManipulativeGroupUsageInSession(getwd())
  logHelpGroupUsageInSession(getwd())
  
  logSummary <- getLogSummaries(getwd())
  setwd("/home/lprisan/workspace/jdc-data-analysis/quests")
  surveyData <- get(load("Survey.rda"))
  basicSurveyPlots(surveyData)
  logCrossedWithSurveys(logSummary, surveyData)
  
  setwd("/home/lprisan/workspace/jdc-data-analysis/maps")
  mapsData <- get(load("Maps.rda"))
  mapPerformancePlots()
  allCrossedDataPlots(logSummary, surveyData, mapsData)
}

allCrossedDataPlots <- function(logSummary, surveyData, mapsData){
  
  # We create a summary/average of the survey data by group
  surveySummary <- getSurveySummary(surveyData)
  
  # Same thing, but only with the complete rows, no NAs
  totalDataComplete <- merge(x=surveySummary,y=logSummary,by.x="Group.Name",by.y="Group.Name")
  totalDataComplete$Session <- as.factor(totalDataComplete$Session)
  totalDataComplete$Cont.Disc.Ratio <- totalDataComplete$Relative.Using.Cont/totalDataComplete$Relative.Using.Disc
  
  totalDataComplete <- merge(x=totalDataComplete,y=mapsData)
  
  # We calculate the maps finished (from the video analysis) vs the total time spent doing maps (in minutes, from the logs)
  totalDataComplete$Maps.perTime <- totalDataComplete$Count.Finished/(totalDataComplete$Total.Duration/60000)
  
  png("Finished.perTime.Boxplots.png",width=1280,height=1024)  
  par(mfrow=c(2,2))
  boxplot(totalDataComplete$Maps.perTime, main="Maps completed / Total Time")
  boxplot(totalDataComplete$Maps.perTime ~ totalDataComplete$Sequence, main="Maps completed / Total Time", xlab="Manipulative Sequence", col=(c("lightgoldenrod","lightgreen")))
  boxplot(totalDataComplete$Maps.perTime ~ totalDataComplete$Session, main="Maps completed / Total Time", xlab="Session")
  boxplot(totalDataComplete$Maps.perTime ~ totalDataComplete$Sequence * totalDataComplete$Session, xlab="Manipulative Sequence and Session", col=(c("lightgoldenrod","lightgreen")))
  dev.off()
  
  png("Finished.vsTotalMaps.Boxplots.png",width=1280,height=1024)  
  par(mfrow=c(2,2))
  boxplot(totalDataComplete$Proportion.Finished, main="Maps completed / Total Maps tried")
  boxplot(totalDataComplete$Proportion.Finished ~ totalDataComplete$Sequence, main="Maps completed / Total Maps tried", xlab="Manipulative Sequence", col=(c("lightgoldenrod","lightgreen")))
  boxplot(totalDataComplete$Proportion.Finished ~ totalDataComplete$Session, main="Maps completed / Total Maps tried", xlab="Session")
  boxplot(totalDataComplete$Proportion.Finished ~ totalDataComplete$Sequence * totalDataComplete$Session, xlab="Manipulative Sequence and Session", col=(c("lightgoldenrod","lightgreen")))
  dev.off()

  png("Finished.Cont.Disc.Ratio.Scatterplots.png",width=1280,height=768)  
  p1 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Maps.perTime)) + geom_smooth(method="lm") +
    geom_point(size=5,alpha=0.4) + ggtitle("Maps completed / Time vs. Ratio Cont/Discr usage")
  p2 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Proportion.Finished)) + geom_smooth(method="lm") +
    geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Ratio Cont/Discr usage")
  p3 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Maps.perTime, colour=Sequence)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Maps completed / Time vs. Ratio Cont/Discr usage")
  p4 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Proportion.Finished, colour=Sequence)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Ratio Cont/Discr usage")
  p5 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Maps.perTime, colour=Session)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Maps completed / Time vs. Ratio Cont/Discr usage")
  p6 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Proportion.Finished, colour=Session)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Ratio Cont/Discr usage")
  multiplot(p1, p2, p3, p4, p5, p6, cols=3)
  dev.off()
  
  png("Finished.Frac.Usage.Scatterplots.png",width=1280,height=768)  
  p1 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Maps.perTime)) + geom_smooth(method="lm") +
    geom_point(size=5,alpha=0.4) + ggtitle("Maps completed / Time vs. Abstract fraction usage")
  p2 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Proportion.Finished)) + geom_smooth(method="lm") +
    geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Abstract fraction usage")
  p3 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Maps.perTime, colour=Sequence)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Maps completed / Time vs. Abstract fraction usage")
  p4 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Proportion.Finished, colour=Sequence)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Abstract fraction usage")
  p5 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Maps.perTime, colour=Session)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Maps completed / Time vs. Abstract fraction usage")
  p6 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Proportion.Finished, colour=Session)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Abstract fraction usage")
  multiplot(p1, p2, p3, p4, p5, p6, cols=3)
  dev.off()
  
  
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
                           Relative.Help.Frac)

  logSummary
}

basicSurveyPlots <- function(surveyData){
  
  # Basic boxplots of the survey data, total and separated by sequence
  png("Q1.Boxplot.png",width=1280,height=1024)  
  par(mfrow=c(1,1))
  boxplot(surveyData$Q1.More.Fun, ylim=c(1,5), main="Q1", sub="I prefer this way to what we do at school", xlab="Total")
  dev.off()
  
  png("Q1.Boxplot.Sequence.Session.png",width=1280,height=1024)  
  par(mfrow=c(2,1))
  boxplot(surveyData$Q1.More.Fun ~ surveyData$Sequence, ylim=c(1,5), main="Q1", xlab="Manipulative Sequence", col=(c("lightgoldenrod","lightgreen")))
  boxplot(surveyData$Q1.More.Fun ~ surveyData$Sequence * surveyData$Session, ylim=c(1,5), sub="I prefer this way to what we do at school", xlab="Manipulative Sequence and Session", col=(c("lightgoldenrod","lightgreen")))
  dev.off()
  
  png("Q2.Boxplot.png",width=1280,height=1024)  
  par(mfrow=c(1,1))
  boxplot(surveyData$Q2.Concrete.Repr, ylim=c(1,5), main="Q2", sub="I prefer concrete representations to abstract/numerical", xlab="Total")
  dev.off()
  
  png("Q2.Boxplot.Sequence.Session.png",width=1280,height=1024)  
  par(mfrow=c(2,1),mar=c(5,2,2,2),cex.axis=0.75)
  boxplot(surveyData$Q2.Concrete.Repr ~ surveyData$Sequence, ylim=c(1,5), main="Q2", xlab="Manipulative Sequence", col=(c("lightgoldenrod","lightgreen")))
  boxplot(surveyData$Q2.Concrete.Repr ~ surveyData$Sequence * surveyData$Session, ylim=c(1,5), sub="I prefer concrete representations to abstract/numerical", xlab="Manipulative Sequence and Session", col=(c("lightgoldenrod","lightgreen")))
  dev.off()
  
  png("Q3.Boxplot.png",width=1280,height=1024)  
  par(mfrow=c(1,1))
  boxplot(surveyData$Q3.Continuous.Repr, ylim=c(1,5), main="Q3", sub="I prefer continuous tangibles to tokens", xlab="Total")
  dev.off()
  
  png("Q3.Boxplot.Sequence.Session.png",width=1280,height=1024)  
  par(mfrow=c(2,1),mar=c(5,2,2,2),cex.axis=0.75)
  boxplot(surveyData$Q3.Continuous.Repr ~ surveyData$Sequence, ylim=c(1,5), main="Q1", xlab="Manipulative Sequence", col=(c("lightgoldenrod","lightgreen")))
  boxplot(surveyData$Q3.Continuous.Repr ~ surveyData$Sequence * surveyData$Session, ylim=c(1,5), sub="I prefer continuous tangibles to tokens", xlab="Manipulative Sequence and Session", col=(c("lightgoldenrod","lightgreen")))
  dev.off()
  
}

getSurveySummary <- function(surveyData){
  
  surveySummary <- as.data.frame((aggregate(Sequence~Group.Number, data=surveyData, unique))$Group.Number)
  names(surveySummary) <- "Group.Name"
  surveySummary$Sequence <- (aggregate(Sequence~Group.Number, data=surveyData, unique))$Sequence
  surveySummary$Session <- (aggregate(Session~Group.Number, data=surveyData, unique))$Session
  surveySummary$Q1.More.Fun <- (aggregate(Q1.More.Fun~Group.Number, data=surveyData, mean))$Q1.More.Fun
  surveySummary$Q2.Concrete.Repr <- (aggregate(Q2.Concrete.Repr~Group.Number, data=surveyData, mean))$Q2.Concrete.Repr
  surveySummary$Q3.Continuous.Repr <- (aggregate(Q3.Continuous.Repr~Group.Number, data=surveyData, mean))$Q3.Continuous.Repr

  surveySummary
}


# Draws some plots about crossing the log summaries with the survey data and sequence of representations
logCrossedWithSurveys <- function(logSummary,surveyData){
  
  # We create a summary/average of the survey data by group
  surveySummary <- getSurveySummary(surveyData)
  
  # We merge both tables, will put NAs where we do not have data
  totalData <- merge(x=surveySummary,y=logSummary,by.x="Group.Name",by.y="Group.Name",all=TRUE)
  totalData$Session <- as.factor(totalData$Session)
  
  # Same thing, but only with the complete rows, no NAs
  totalDataComplete <- merge(x=surveySummary,y=logSummary,by.x="Group.Name",by.y="Group.Name")
  totalDataComplete$Session <- as.factor(totalDataComplete$Session)
  totalDataComplete$Cont.Disc.Ratio <- totalDataComplete$Relative.Using.Cont/totalDataComplete$Relative.Using.Disc
  
  
  png("Survey.Usage.Scatterplots.png",width=1280,height=1024)  
  # Scatterplots of survey answers (group-averaged) vs relative usage of different elements (3x3) colored by sequence
  p1 <- ggplot(totalDataComplete, aes(x=Relative.Using.Cont, y=Q1.More.Fun, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("Usage of Continuous vs. Amusement") + ylim(c(1,5))
  p2 <- ggplot(totalDataComplete, aes(x=Relative.Using.Cont, y=Q2.Concrete.Repr, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("Usage of Continuous vs. Preference for Concrete/Manipulative (vs. Numeric)") + ylim(c(1,5))
  p3 <- ggplot(totalDataComplete, aes(x=Relative.Using.Cont, y=Q3.Continuous.Repr, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("Usage of Continuous vs. Preference for Continuous (vs. Tokens)") + ylim(c(1,5))
  
  p4 <- ggplot(totalDataComplete, aes(x=Relative.Using.Disc, y=Q1.More.Fun, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("Usage of Discrete/Tokens vs. Amusement") + ylim(c(1,5))
  p5 <- ggplot(totalDataComplete, aes(x=Relative.Using.Disc, y=Q2.Concrete.Repr, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("Usage of Discrete/Tokens vs. Preference for Concrete/Manipulative (vs. Numeric)") + ylim(c(1,5))
  p6 <- ggplot(totalDataComplete, aes(x=Relative.Using.Disc, y=Q3.Continuous.Repr, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("Usage of Discrete/Tokens vs. Preference for Continuous (vs. Tokens)") + ylim(c(1,5))
  
  p7 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Q1.More.Fun, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("Usage of Abstract/Fractions vs. Amusement") + ylim(c(1,5))
  p8 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Q2.Concrete.Repr, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("Usage of Abstract/Fractions vs. Preference for Concrete/Manipulative (vs. Numeric)") + ylim(c(1,5))
  p9 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Q3.Continuous.Repr, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("Usage of Abstract/Fractions vs. Preference for Continuous (vs. Tokens)") + ylim(c(1,5))

  p10 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Q1.More.Fun, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("(Ratio of Continuous vs. Discrete) vs. Amusement") + ylim(c(1,5))
  p11 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Q2.Concrete.Repr, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("(Ratio of Continuous vs. Discrete) vs. Preference for Concrete/Manipulative (vs. Numeric)") + ylim(c(1,5))
  p12 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Q3.Continuous.Repr, colour=Sequence)) +
    geom_point() + geom_smooth(method="lm") + ggtitle("(Ratio of Continuous vs. Discrete) vs. Preference for Continuous (vs. Tokens)") + ylim(c(1,5))
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, cols=4)
  dev.off()  

  png("Survey.Usage.Scatterplots.Detail.png",width=1280,height=1024)  
  multiplot(p7, p8, p9, p10, p11, p12, cols=2)
  dev.off()
  
  # We plot several hierarchical clusterings of data
  # We select only the sequence, (group averaged) survey questions, ratio of usage
  totalMatrix1 <- data.matrix(totalDataComplete[,c(2,4:6,25)])
  hc <- hclust(dist(totalMatrix1)) 
  png("Hierarchical.Dendrogram.5feats.png",width=1280,height=1024)
  par(mfrow=c(1,1))
  myplclust(hc,lab=totalDataComplete$Group.Name,lab.col=unclass(totalDataComplete$Sequence),main="Cluster Dendrogram 5 features (sequence, survey, ratio of cont/discr)")
  #plot(hc)
  dev.off()

  # We select the sequence, (group averaged) survey questions, manipulative usages and ratio of usage
  totalMatrix2 <- data.matrix(totalDataComplete[,c(2,4:6,17:19,25)])
  hc <- hclust(dist(totalMatrix2)) 
  png("Hierarchical.Dendrogram.8feats.png",width=1280,height=1024)
  par(mfrow=c(1,1))
  myplclust(hc,lab=totalDataComplete$Group.Name,lab.col=unclass(totalDataComplete$Sequence),main="Cluster Dendrogram 8 features (sequence, survey, relative usage, ratio of cont/discr)")
  dev.off()
  
  # We select the sequence, (group averaged) survey questions, manipulative usages, help usages and ratio of usage
  totalMatrix3 <- data.matrix(totalDataComplete[,c(2,4:6,17:25)])
  hc <- hclust(dist(totalMatrix3)) 
  png("Hierarchical.Dendrogram.13feats.png",width=1280,height=1024)
  par(mfrow=c(1,1))
  myplclust(hc,lab=totalDataComplete$Group.Name,lab.col=unclass(totalDataComplete$Sequence),main="Cluster Dendrogram 13 features (sequence, survey, relative usage, ratio of cont/discr, relative usage of help)")
  dev.off()
  
  # TODO: Add to the dendrogram labels the number and percentage of finished maps??
  
  # We try k-means clustering with 5 clusters (it looks like the hierarchical clustering gives 5 big clusters)
  dataSubset <- cbind(as.numeric(totalDataComplete[,2]),totalDataComplete[,c(4:6,17:19,25)])
  kmeansObj <- kmeans(dataSubset, centers=5, nstart=100)
  dataSubset$clusters <- factor(kmeansObj$cluster)
  dataSubset$Group.Name <- totalDataComplete$Group.Name
  png("KMeans.8feats.5clusters.png",width=1280,height=768)
  p1 <- ggplot(dataSubset, aes(x=log10(Cont.Disc.Ratio), y=Q1.More.Fun, colour=clusters)) +
    geom_point(size=5,alpha=0.4) + ggtitle("Ratio Cont/Discr usage vs. Amusement") + ylim(c(1,5))
  p2 <- ggplot(dataSubset, aes(x=log10(Cont.Disc.Ratio), y=Q2.Concrete.Repr, colour=clusters)) +
    geom_point(size=5,alpha=0.4) + ggtitle("Ratio Cont/Discr usage vs. Preference for Concrete/Manipulative (vs. Numeric)") + ylim(c(1,5))
  p3 <- ggplot(dataSubset, aes(x=log10(Cont.Disc.Ratio), y=Q3.Continuous.Repr, colour=clusters)) +
    geom_point(size=5,alpha=0.4) + ggtitle("Ratio Cont/Discr usage vs. Preference for Continuous (vs. Tokens)") + ylim(c(1,5))
  
  multiplot(p1, p2, p3, cols=3)
  dev.off()
  
  # We do SVD
  png("Singular.Vector.Decomposition.Contributors.png",width=1280,height=1024)
  getSvdMostInfluential(totalMatrix3, 
                        quantile=.8, 
                        similarity_threshold = .9,
                        plot_threshold = .05,
                        plot_selection = TRUE)
  dev.off()
  
  # TODO: Do more clear graphs of the SVD/PCA
  
}



# Do a multi-graph panel of usage of each kind of manipulative
logManipulativeGroupUsageInSession <- function(rootDir){
  
  setwd(rootDir)
  
  files <- list.files(pattern = "\\.rda$")
  
  #We get the files for each session
  for(i in 1:6){
    sessionFiles <- files[grep(paste("S",as.character(i),sep=""),files,fixed=TRUE)]
    
    sessionData <- data.frame()
    
    # We get all the group data for the session and add it to a overall dataframe
    for(file in sessionFiles){
      data <- get(load(file))
      data <- addElementUsageVariables(data)
      data$Group <- rep(substr(file,3,4),times=length(data$timestamp))
      sessionData <- rbind(sessionData,data)
    }
    
    # Do the graph
    png(paste(paste("S",as.character(i),sep=""),".manip.usage.png",sep=""),width=1280,height=1024)  
    par(mfcol=c(3,length(sessionFiles)))
    for(j in 1:length(sessionFiles)){
      groupName <- substr(sessionFiles[j],3,4)
      with(subset(sessionData, Group==groupName),{
       plot(timestamp,UsingCont,main=groupName)
       plot(timestamp,UsingDisc,main=groupName)
       plot(timestamp,UsingFrac,main=groupName)
     }) 
    }
    dev.off()
    
  }
  
  
  
}


# Do a multi-graph panel of usage of each kind of hint card
logHelpGroupUsageInSession <- function(rootDir){
  
  setwd(rootDir)
  
  files <- list.files(pattern = "\\.rda$")
  
  #We get the files for each session
  for(i in 1:6){
    sessionFiles <- files[grep(paste("S",as.character(i),sep=""),files,fixed=TRUE)]
    
    sessionData <- data.frame()
    
    # We get all the group data for the session and add it to a overall dataframe
    for(file in sessionFiles){
      data <- get(load(file))
      data <- addElementUsageVariables(data)
      data$Group <- rep(substr(file,3,4),times=length(data$timestamp))
      sessionData <- rbind(sessionData,data)
    }
    
    # Do the graph
    png(paste(paste("S",as.character(i),sep=""),".hint.usage.png",sep=""),width=1280,height=1024)  
    par(mfcol=c(5,length(sessionFiles)))
    for(j in 1:length(sessionFiles)){
      groupName <- substr(sessionFiles[j],3,4)
      with(subset(sessionData, Group==groupName),{
        plot(timestamp,HelpContCirc,main=groupName)
        plot(timestamp,HelpContRect,main=groupName)
        plot(timestamp,HelpDiscrete,main=groupName)
        plot(timestamp,HelpDecimal,main=groupName)
        plot(timestamp,HelpFraction,main=groupName)
      }) 
    }
    dev.off()
    
  }
  
  
  
}


# Make graphics with the points in time (e.g., samples available per minute), in order 
# to see the lamps performance, crashes and gaps in our data
logPointsInTime <- function(rootDir,datafile){
  
  setwd(rootDir)
  data <- get(load(datafile))
  png(paste(datafile,".logpoints.png",sep=""))
  breaks <- seq((min(data$timestamp)-60001),(max(data$timestamp)+60001),by=60000)
  hist(data$timestamp,breaks,main=paste("Number of available log samples per minute - Group",datafile))
  rug(data$timestamp)
  dev.off()
  
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

# Make graphics with the time a kind of representation was on the table, for a group
logSamplesElementsPresent <- function(rootDir,datafile){
  
  setwd(rootDir)
  data <- get(load(datafile))
  
  data <- addElementUsageVariables(data)
  
  
  # We sum the number of samples in which each group is present
  barmanip <- apply(data[66:68],2,sum)
  barhelp <- apply(data[69:73],2,sum)

  png(paste(datafile,".manipulative.presence.png",sep=""))
  # Do the plot
  barplot(barmanip, main="Relative usage of the different manipulative types (log samples)", sub=paste("Group",datafile))
  dev.off()

  png(paste(datafile,".help.presence.png",sep=""))
  # Do the plot
  barplot(barhelp, main="Relative usage of the different help types (log samples)", sub=paste("Group",datafile))
  dev.off()
  
  
}


# Multiple plot function, from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
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

myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  ## modifiction of plclust for plotting hclust objects *in colour*!
  ## Copyright Eva KF Chan 2009
  ## Arguments:
  ##    hclust:    hclust object
  ##    lab:        a character vector of labels of the leaves of the tree
  ##    lab.col:    colour for the labels; NA=default device foreground colour
  ##    hang:     as in hclust & plclust
  ## Side effect:
  ##    A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height,2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]
  x <- x[which(x<0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )
}

# Draws some simple maps about the finished and unfinished maps for each group. 
# It assumes the csvs with the data are in the working directory!
mapPerformancePlots <- function(){
  
  #Overall groups
  ResumeMaps <- read.csv("ResumeMaps4.csv")
  table <- ResumeMaps$Count
  table <- matrix(table, ncol = 17, byrow = T)
  colnames(table) <- levels(ResumeMaps$Label)
  rownames(table) <- levels(ResumeMaps$Level)
  colors <- c("black","white")
  png("Overall.finished.and.unfinished.maps.png")
  barplot((table),beside = FALSE, col = colors, ylim = c(0,10), xlab = "Session and group", main = "Finished and Unfinished maps", legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))
  dev.off()
  
  #Per session
  Session2 <- read.csv("Session2.csv")
  Session3 <- read.csv("Session3.csv")
  Session4 <- read.csv("Session4.csv")
  Session6 <- read.csv("Session6.csv")
  
  #In session 2, we have 3 groups
  table_s2 <- Session2$Count
  table_s2 <- matrix(table_s2, ncol = 3, byrow = T)
  colnames(table_s2) <- levels(Session2$Label)
  rownames(table_s2) <- levels(Session2$Level)
  colors <- c("black","white")
  png("Session2.finished.and.unfinished.png")
  barplot((table_s2),beside = FALSE, col = colors, ylim = c(0,10), xlab = "Group", main = "Finished and Unfinished maps Session 2", legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))
  dev.off()
  
  #In session 3, we have 4 groups
  table_s3 <- Session3$Count
  table_s3 <- matrix(table_s3, ncol = 4, byrow = T)
  colnames(table_s3) <- levels(Session3$Label)
  rownames(table_s3) <- levels(Session3$Level)
  colors <- c("black","white")
  png("Session3.finished.and.unfinished.png")
  barplot((table_s3),beside = FALSE, col = colors, ylim = c(0,10), xlab = "Group", main = "Finished and Unfinished maps Session 3", legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))
  dev.off()
  
  #In session 4, we have 5 groups
  table_s4 <- Session4$Count
  table_s4 <- matrix(table_s4, ncol = 5, byrow = T)
  colnames(table_s4) <- levels(Session4$Label)
  rownames(table_s4) <- levels(Session4$Level)
  colors <- c("black","white")
  png("Session4.finished.and.unfinished.png")
  barplot((table_s4),beside = FALSE, col = colors, ylim = c(0,10), xlab = "Group", main = "Finished and Unfinished maps Session 4", legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))
  dev.off()
  
  #In session 6, we have 5 groups
  table_s6 <- Session6$Count
  table_s6 <- matrix(table_s6, ncol = 5, byrow = T)
  colnames(table_s6) <- levels(Session6$Label)
  rownames(table_s6) <- levels(Session6$Level)
  colors <- c("black","white")
  png("Session6.finished.and.unfinished.png")
  barplot((table_s6),beside = FALSE, col = colors, ylim = c(0,10), xlab = "Group", main = "Finished and Unfinished maps Session 6", legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))
  dev.off()
  
}

