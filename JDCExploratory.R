require("lattice")
require("ggplot2")
require("Gmisc")
require("zoo")

# This is the global function that runs the whole set of exploratory data analyses
# It receives the root dir containing the logs/, maps/, quests/, eyetrack/ folders
# and if not passed, it assumes it is the current one
JDCExplorations <- function(rootDir="."){
  
  originalDir <- getwd()
    
  setwd(rootDir)
  rootDir <- getwd() # So that we get the full path

  # We get basic results of map completion by the groups
  mapdir <- paste(rootDir,"/maps",sep="")
  setwd(mapdir)
  mapsData <- get(load("Maps.rda"))
  # We plot the data from the map completion data (simple graphs)
#  mapPerformancePlots()
  
  # We get the log data  
  logdir <- paste(rootDir,"/logs",sep="")
#   # Do the samples available, relative usage of representations and helps per group
#   for(file in list.files(path=logdir,pattern = "\\.rda$")){
#     logPointsInTime(logdir,file)
#     logSamplesElementsPresent(logdir,file)
#   }
#   # Do a multi-graph panel for the temporal evolution of usage of each kind of card and each group, for a session
#   logManipulativeGroupUsageInSession(logdir)
#   logHelpGroupUsageInSession(logdir)
  # We get some summaries of the logs, eliminating most of the temporal component (relative usages of different elements)
  # logSummary <- getLogSummaries(logdir)
  logMapSummary <- getLogSummariesInclAct4(logdir,mapsData)  #

  # We get the questionnaires/surveys data
  questdir <- paste(rootDir,"/quests",sep="")
  setwd(questdir)
  surveyData <- get(load("Survey.rda"))
  # We do some basic plots of survey data
  # basicSurveyPlots(surveyData)
  # We do plots crossing logs (summarized) and survey data, including some cluster analysis
  #logCrossedWithSurveys(logSummary, surveyData)


  # We do some other plots crossing element usage (from the logs), map completion and survey responses
  allCrossedDataPlots(logMapSummary, surveyData)
  
  # We get the teacher's eyetracking data
  eyedir <- paste(rootDir,"/eyetrack",sep="")
  setwd(eyedir)
  eyedata <- get(load("EyetrackerEvents.rda"))
  # We do some basic plotting of eyetracking parameters
  eyetrackerPlots(eyedata)

  # We go back to the original directory
  setwd(originalDir)
}


# This function returns a data frame with a summary of the Log files, describing the usage that each group made 
# of the different paper elements (manipulatives, hints, etc)
getLogSummariesInclAct4 <- function(logdir,mapsData){
  
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
  Help.Circ.Act4 = numeric(num_groups)
  Help.Rect.Act4 = numeric(num_groups)
  Help.Disc.Act4 = numeric(num_groups)
  Help.Dec.Act4 = numeric(num_groups)
  Help.Frac.Act4 = numeric(num_groups)
  Relative.Using.Cont.Act4 = numeric(num_groups)
  Relative.Using.Disc.Act4 = numeric(num_groups)
  Relative.Using.Frac.Act4 = numeric(num_groups)
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
    Help.Circ.Act4[i] <- sum(data$HelpContCirc)
    Help.Rect.Act4[i] <- sum(data$HelpContRect)
    Help.Disc.Act4[i] <- sum(data$HelpDiscrete)
    Help.Dec.Act4[i] <- sum(data$HelpDecimal)
    Help.Frac.Act4[i] <- sum(data$HelpFraction)
    
    num_samples <- length(data$timestamp)
    
    Relative.Using.Cont.Act4[i] <- Using.Cont.Act4[i]/num_samples
    Relative.Using.Disc.Act4[i] <- Using.Disc.Act4[i]/num_samples
    Relative.Using.Frac.Act4[i] <- Using.Frac.Act4[i]/num_samples
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
                           Help.Circ.Act4,
                           Help.Rect.Act4,
                           Help.Disc.Act4,
                           Help.Dec.Act4,
                           Help.Frac.Act4,
                           Relative.Using.Cont.Act4,
                           Relative.Using.Disc.Act4,
                           Relative.Using.Frac.Act4,
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


# This function receives a data frame with eyetracking events (pupil diameter), and it
# draws basic line plots regarding their evolution over time (using a sliding window)
# Currently, calculates mean and standard deviation over the defined windows
# The default window size is 10s (300 samples), with 10 samples window slide
eyetrackerPlots <- function(data, window=300, slide=10){
  
  # We split each session's data, and put it into a list
  data$Session <- as.factor(data$Session)
  sessionsdata <- split(data,data$Session)
  
  # We make a sliding window mean of the data
  # TODO: add the timestamp of the middle of each window, for later browsing 
  # TODO: check out the periods where both std and mean are over the average??
  for(i in 1:length(sessionsdata)){
    png(paste("Session",sessionsdata[[i]]$Session[[1]],".Pupil.Diameter.",window,"samples.slide",slide,".png",sep=""),width=1280,height=1024)  
    par(mfcol=c(2,1))
    meandata <- rollapply(sessionsdata[[i]]$L.Pupil.Diameter..mm., width = window, by = slide, FUN = mean, align = "left")
    plot(meandata, type="n", main=paste("PD mean, Session",sessionsdata[[i]]$Session[[1]]))
    lines(meandata)
    meandata2 <- rollapply(sessionsdata[[i]]$R.Pupil.Diameter..mm., width = window, by = slide, FUN = mean, align = "left")
    lines(meandata2,col="blue")
    
    stddata <- rollapply(sessionsdata[[i]]$L.Pupil.Diameter..mm., width = window, by = slide, FUN = sd, align = "left")
    plot(stddata, type="n", main=paste("PD std, Session",sessionsdata[[i]]$Session[[1]]))
    lines(stddata)
    stddata2 <- rollapply(sessionsdata[[i]]$R.Pupil.Diameter..mm., width = window, by = slide, FUN = sd, align = "left")
    lines(stddata2,col="blue")
    dev.off()
  }
  
  
  
}

# This function gets as input the data from all our sources (logs, surveys, 
# video coding of maps completed) and plots a few  crossings between them
allCrossedDataPlots <- function(logMapSummary, surveyData){
  
  # We create a summary/average of the survey data by group
  surveySummary <- getSurveySummary(surveyData)
  
  # We merge the log-map and survey data
  totalDataComplete <- merge(x=surveySummary,y=logMapSummary,by.x="Group.Name",by.y="Group.Name",all=TRUE)
  totalDataComplete$Session <- as.factor(totalDataComplete$Session)
  totalDataComplete$Cont.Disc.Ratio <- totalDataComplete$Relative.Using.Cont/totalDataComplete$Relative.Using.Disc
  totalDataComplete$Cont.Disc.Ratio.Act4 <- totalDataComplete$Relative.Using.Cont.Act4/totalDataComplete$Relative.Using.Disc.Act4
  # We calculate the maps finished (from the video analysis) vs the total time spent doing maps (in minutes, from the logs)
  totalDataComplete$Maps.perTime <- totalDataComplete$Count.Finished/(totalDataComplete$Total.Duration/60000)
  
  totalDataComplete <- addPerformanceCluster(totalDataComplete)
  
  
  # Boxplots about maps finished per unit of time, by sequence, and session
  png("Finished.perTime.Boxplots.png",width=1280,height=1024)  
  par(mfrow=c(2,2))
  boxplot(totalDataComplete$Maps.perTime, main="Maps completed / Total Time")
  boxplot(totalDataComplete$Maps.perTime ~ totalDataComplete$Sequence, main="Maps completed / Total Time", xlab="Manipulative Sequence", col=(c("lightgoldenrod","lightgreen")))
  boxplot(totalDataComplete$Maps.perTime ~ totalDataComplete$Session, main="Maps completed / Total Time", xlab="Session")
  boxplot(totalDataComplete$Maps.perTime ~ totalDataComplete$Sequence * totalDataComplete$Session, xlab="Manipulative Sequence and Session", col=(c("lightgoldenrod","lightgreen")))
  dev.off()


  # Scatterplots about maps completed per time, representation types used and preferences
  png("Finished.perTime.Representation.png",width=1280,height=1024)  
  p1 <- ggplot(totalDataComplete, aes(x=Q3.Continuous.Repr, y=Maps.perTime)) + 
    ggtitle("Maps finished vs. Preference for continuous") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  p2 <- ggplot(totalDataComplete, aes(x=(6-Q3.Continuous.Repr), y=Maps.perTime)) + 
    ggtitle("Maps finished vs. Preference for discrete") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  p3 <- ggplot(totalDataComplete, aes(x=(6-Q2.Concrete.Repr), y=Maps.perTime)) + 
    ggtitle("Maps finished vs. Preference for symbolic") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  
  p4 <- ggplot(totalDataComplete, aes(x=Relative.Using.Cont.Act4, y=Maps.perTime)) + 
    ggtitle("Maps finished vs. Usage of continuous") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  p5 <- ggplot(totalDataComplete, aes(x=Relative.Using.Disc.Act4, y=Maps.perTime)) + 
    ggtitle("Maps finished vs. Usage of discrete") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  p6 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac.Act4, y=Maps.perTime)) + 
    ggtitle("Maps finished vs. Usage of symbolic") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  
  p7 <- ggplot(totalDataComplete, aes(x=(Relative.Help.Circ.Act4+Relative.Help.Rect.Act4), y=Maps.perTime)) + 
    ggtitle("Maps finished vs. Usage of continuous hints") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  p8 <- ggplot(totalDataComplete, aes(x=Relative.Help.Disc.Act4, y=Maps.perTime)) + 
    ggtitle("Maps finished vs. Usage of discrete hints") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  p9 <- ggplot(totalDataComplete, aes(x=(Relative.Help.Frac.Act4+Relative.Help.Dec.Act4), y=Maps.perTime)) + 
    ggtitle("Maps finished vs. Usage of symbolic hints") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")

  p10 <- ggplot(totalDataComplete, aes(x=Relative.Using.Cont.Act4, y=Q3.Continuous.Repr)) + 
    ggtitle("Continuous preference vs. cont. manip. usage") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  p11 <- ggplot(totalDataComplete, aes(x=Relative.Using.Disc.Act4, y=(6-Q3.Continuous.Repr))) + 
    ggtitle("Discrete preference vs. discr. manip. usage") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  p12 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac.Act4, y=(6-Q2.Concrete.Repr))) + 
    ggtitle("Symbolic preference and symb. manip. usage") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  
  p13 <- ggplot(totalDataComplete, aes(x=(Relative.Help.Circ.Act4+Relative.Help.Rect.Act4), y=Q3.Continuous.Repr)) + 
    ggtitle("Continuous preference vs. cont. hint usage") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  p14 <- ggplot(totalDataComplete, aes(x=Relative.Help.Disc.Act4, y=(6-Q3.Continuous.Repr))) + 
    ggtitle("Discrete preference vs. disc. hint usage") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  p15 <- ggplot(totalDataComplete, aes(x=(Relative.Help.Frac.Act4+Relative.Help.Dec.Act4), y=(6-Q2.Concrete.Repr))) + 
    ggtitle("Symbolic preference vs. symb. hint usage") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + geom_smooth(method="lm") + theme(legend.position="bottom")
  
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, cols=5)
  dev.off()
  
  
  # Get the cluster centers for the different parameters
  # aggregate(totalDataComplete,by=list(totalDataComplete$Performance.Cluster),FUN=mean)
  # TODO: Find a way to make the non-clustered values appear, or maybe show the separation by the other performance metrics
  png("Performance.Cluster.Scatterplots.png",width=1280,height=768)  
  p1 <- ggplot(totalDataComplete, aes(x=as.factor(Performance.Cluster), y=Maps.perTime)) + 
    ggtitle("Maps/Time by cluster") + 
    geom_point(size=5,alpha=0.4,aes(colour=Sequence)) + theme(legend.position="bottom")
  p2 <- ggplot(totalDataComplete, aes(x=Q3.Continuous.Repr, y=Maps.perTime)) + 
    ggtitle("Maps/Time vs Continuous preference") + 
    geom_point(size=5,alpha=0.4,aes(colour=as.factor(Performance.Cluster))) + theme(legend.position="bottom") + scale_colour_brewer(palette="Paired")
  p3 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac.Act4, y=Maps.perTime)) + 
    ggtitle("Maps/Time vs Usage of fractions during free choice") + 
    geom_point(size=5,alpha=0.4,aes(colour=as.factor(Performance.Cluster))) + theme(legend.position="bottom") + scale_colour_brewer(palette="Paired")
  multiplot(p1, p2, p3, cols=3)
  dev.off()

  # We plot a hierarchical clustering of data
  # We select only the sequence, (group averaged) survey questions, performance metrics (with all the data)
  totalMatrix <- data.matrix(na.omit(totalDataComplete[,c("Sequence","Q1.More.Fun","Q2.Concrete.Repr","Q3.Continuous.Repr",
                                                  "Mentions.Fractions","Gets.Mechanic","Maps.perTime")]))
  # We get we add the Act4 performance and usage, thus with n=9
  totalMatrixComplete <- data.matrix(na.omit(totalDataComplete[,c("Sequence","Q1.More.Fun","Q2.Concrete.Repr","Q3.Continuous.Repr",
                                                          "Mentions.Fractions","Gets.Mechanic","Act4.Finished.Maps","Maps.perTime",
                                                          "Relative.Using.Cont.Act4","Relative.Using.Disc.Act4","Relative.Using.Frac.Act4",
                                                          "Relative.Help.Circ.Act4","Relative.Help.Rect.Act4","Relative.Help.Disc.Act4","Relative.Help.Dec.Act4","Relative.Help.Frac.Act4")]))
#   
#   hc <- hclust(dist(totalMatrixComplete)) 
#   png("Hierarchical.Dendrogram.16feats.png",width=1280,height=1024)
#   par(mfrow=c(1,1))
#   myplclust(hc,lab=totalDataComplete$Group.Name,lab.col=unclass(totalDataComplete$Sequence),main="Cluster Dendrogram (sequence, survey, performance, act4 usage stats)")
#   #plot(hc)
#   dev.off()
  
  # TODO: Add to the dendrogram labels the number and percentage of finished maps??
  
  # We do SVD
  png("Singular.Vector.Decomposition.Contributors.AllGroups.png",width=1280,height=1024)
  getSvdMostInfluential(totalMatrix, 
                        quantile=.8, 
                        similarity_threshold = .9,
                        plot_threshold = .05,
                        plot_selection = TRUE)
  dev.off()
  
  png("Singular.Vector.Decomposition.Contributors.Act4Groups.png",width=1280,height=1024)
  getSvdMostInfluential(totalMatrixComplete, 
                        quantile=.8, 
                        similarity_threshold = .9,
                        plot_threshold = .05,
                        plot_selection = TRUE)
  dev.off()

  
  # Basic boxplots of the survey data, total and separated by sequence
  png("Q1.Boxplot.png",width=1280,height=1024)  
  par(mfrow=c(1,1))
  boxplot(totalDataComplete$Q1.More.Fun, ylim=c(1,5), main="Q1", sub="I prefer this way to what we do at school", xlab="Total")
  dev.off()
  
  png("Q2.Q3.Boxplot.Sequence.Session.Cluster.png",width=1280,height=1024)  
  par(mfrow=c(2,3),cex.axis=0.75)
  boxplot(totalDataComplete$Q2.Concrete.Repr ~ totalDataComplete$Sequence, ylim=c(1,5), main="Q2", xlab="Manipulative Sequence", col=(c("lightgoldenrod","lightgreen")))
  boxplot(totalDataComplete$Q2.Concrete.Repr ~ totalDataComplete$Sequence * totalDataComplete$Session, ylim=c(1,5), sub="I prefer concrete representations to abstract/numerical", xlab="Manipulative Sequence and Session", col=(c("lightgoldenrod","lightgreen")))
  boxplot(totalDataComplete$Q2.Concrete.Repr ~ totalDataComplete$Performance.Cluster, ylim=c(1,5), main="Q2", xlab="Performance cluster", col=(c("red","blue")))
  
  boxplot(totalDataComplete$Q3.Continuous.Repr ~ totalDataComplete$Sequence, ylim=c(1,5), main="Q3", xlab="Manipulative Sequence", col=(c("lightgoldenrod","lightgreen")))
  boxplot(totalDataComplete$Q3.Continuous.Repr ~ totalDataComplete$Sequence * totalDataComplete$Session, ylim=c(1,5), sub="I prefer continuous tangibles to tokens", xlab="Manipulative Sequence and Session", col=(c("lightgoldenrod","lightgreen")))
  boxplot(totalDataComplete$Q3.Continuous.Repr ~ totalDataComplete$Performance.Cluster, ylim=c(1,5), main="Q3", xlab="Performance cluster", col=(c("red","blue")))
  dev.off()




  png("Total.Finished.Cont.Disc.Ratio.Scatterplots.png",width=1280,height=768)  
  p1 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Maps.perTime)) + geom_smooth(method="lm") +
    geom_point(size=5,alpha=0.4) + ggtitle("Total Maps completed / Time vs. Ratio Cont/Discr usage")
#   p2 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Proportion.Finished)) + geom_smooth(method="lm") +
#     geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Ratio Cont/Discr usage")
  p3 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Maps.perTime, colour=Sequence)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Total Maps completed / Time vs. Ratio Cont/Discr usage")
#   p4 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Proportion.Finished, colour=Sequence)) + 
#     geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Ratio Cont/Discr usage")
  p5 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Maps.perTime, colour=Session)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Total Maps completed / Time vs. Ratio Cont/Discr usage")
#   p6 <- ggplot(totalDataComplete, aes(x=log10(Cont.Disc.Ratio), y=Proportion.Finished, colour=Session)) + 
#     geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Ratio Cont/Discr usage")
  multiplot(p1, p3, p5, cols=3)
  dev.off()
  
  png("Total.Finished.Frac.Usage.Scatterplots.png",width=1280,height=768)  
  p1 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Maps.perTime)) + geom_smooth(method="lm") +
    geom_point(size=5,alpha=0.4) + ggtitle("Maps completed / Time vs. Abstract fraction usage")
#   p2 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Proportion.Finished)) + geom_smooth(method="lm") +
#     geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Abstract fraction usage")
  p3 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Maps.perTime, colour=Sequence)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Maps completed / Time vs. Abstract fraction usage")
#   p4 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Proportion.Finished, colour=Sequence)) + 
#     geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Abstract fraction usage")
  p5 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Maps.perTime, colour=Session)) + 
    geom_point(size=5,alpha=0.4) + ggtitle("Maps completed / Time vs. Abstract fraction usage")
#   p6 <- ggplot(totalDataComplete, aes(x=Relative.Using.Frac, y=Proportion.Finished, colour=Session)) + 
#     geom_point(size=5,alpha=0.4) + ggtitle("Ratio of maps completed vs. Abstract fraction usage")
  multiplot(p1, p3, p5, cols=3)
  dev.off()
  

  # TODO: Get data on the usage of elements in Act 4 (n=9)
  act4UsageData <- na.omit(totalDataComplete[,c("Group.Name", "Sequence","Q1.More.Fun","Q2.Concrete.Repr","Q3.Continuous.Repr",
                             "Mentions.Fractions","Gets.Mechanic","Act4.Finished.Maps","Maps.perTime",
                             "Relative.Help.Circ.Act4","Relative.Help.Rect.Act4","Relative.Help.Disc.Act4","Relative.Help.Dec.Act4","Relative.Help.Frac.Act4",
                             "Relative.Usage.None.Act4", "Relative.Usage.Sole.Cont.Act4", "Relative.Usage.Sole.Disc.Act4", "Relative.Usage.Sole.Frac.Act4",
                             "Relative.Usage.Cont.Disc.Act4", "Relative.Usage.Disc.Frac.Act4", "Relative.Usage.Frac.Cont.Act4", "Relative.Usage.All.Act4")])

  # Some more graph ideas
  # Create a factor variable about representation in use for each moment (pure cont, pure disc, pure symb, ), and do line graph among different levels (are there transitions, or each group only does one combination?)
  # Do histogram/barplot of groups using each kind (binary), for representations and for hints
  # Do histogram/barplot of groups using each kind, for representations and for hints (relative to Act4 time)
  # Relate the combination of each group to the performance metrics (maps completed, mention of fractions...)

  
}

# This function takes a data frame, and makes a k-means clustering (k=2) of the subjects, with regard to the four performance metrics
# Then, it adds a factor with this cluster to the data frame
addPerformanceCluster <- function(data){
  
  perf <- data[,c("Group.Name","Maps.perTime","Mentions.Fractions","Gets.Mechanic","Act4.Finished.Maps")]
  scaledData <- scale(na.omit(perf[,-1]))
  fit <- kmeans(scaledData,2,nstart=100)
  perf <- data.frame(na.omit(perf),Performance.Cluster=fit$cluster)
  
  data <- merge(data,perf,all=TRUE)
  
  # We create a factor with correct labels for the clusters, using the maps per time which is one of the main defining factors
  if(mean(data[!is.na(data$Performance.Cluster) & data$Performance.Cluster==1,"Maps.perTime"])>mean(data[!is.na(data$Performance.Cluster) & data$Performance.Cluster==2,"Maps.perTime"])){ # 1 is the high performance cluster
    data$Performance.Cluster <- factor(data$Performance.Cluster,levels=c("1","2"),labels=c("HiPerf","LoPerf"),exclude=NULL)
  } else { # 1 is the low performance
    data$Performance.Cluster <- factor(data$Performance.Cluster,levels=c("1","2"),labels=c("LoPerf","HiPerf"),exclude=NULL)
  }
  
  data
  
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

# This function plots some very basic graphs about the likert responses in the survey (likability of the system and different kinds of elements)
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

# This function gets the (individual) data from the Likert surveys and does a group-level aggregation of it
getSurveySummary <- function(surveyData){
  
  surveySummary <- as.data.frame((aggregate(Sequence~Group.Number, data=surveyData, unique))$Group.Number)
  names(surveySummary) <- "Group.Name"
  surveySummary$Sequence <- (aggregate(Sequence~Group.Number, data=surveyData, unique))$Sequence
  surveySummary$Session <- (aggregate(Session~Group.Number, data=surveyData, unique))$Session
  surveySummary$Q1.More.Fun <- (aggregate(Q1.More.Fun~Group.Number, data=surveyData, mean))$Q1.More.Fun
  surveySummary$Q2.Concrete.Repr <- (aggregate(Q2.Concrete.Repr~Group.Number, data=surveyData, mean))$Q2.Concrete.Repr
  surveySummary$Q3.Continuous.Repr <- (aggregate(Q3.Continuous.Repr~Group.Number, data=surveyData, mean))$Q3.Continuous.Repr
  surveySummary$Mentions.Fractions <- (aggregate(Mentions.Fractions~Group.Number, data=surveyData, unique))$Mentions.Fractions
  surveySummary$Gets.Mechanic <- (aggregate(Gets.Mechanic~Group.Number, data=surveyData, unique))$Gets.Mechanic
  
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

