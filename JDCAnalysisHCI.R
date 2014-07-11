#require("lattice")
require("ggplot2")
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
  mapsData <- get(load(paste(rootDir,"/maps/","Maps.rda",sep="")))
  # Survey data
  surveyData <- get(load(paste(rootDir,"/quests/","Survey.rda",sep="")))
  # TODO: Log data summary, or maybe two summaries, one global, another for the 9 groups in Act4?
  #logdir <- paste(rootDir,"/logs",sep="")
  #logData <- getLogSummaryHCI(logdir,mapsData)
  #act4UsageSummary <- getAct4UsageLogSummary(logdir)
  
  # We plot the different questions/themes we had
  # Basic plots of children preferences
  #basicPreferencePlots(surveyData, act4UsageSummary)
  basicPreferencePlots(surveyData)
  
  # Bad uses of the interface
  
  # Manipulative usage
  
  # Collaboration (ownership, awareness)
  
}


# This function tries to answer the following questions:
# What kind of fractions did kids PREFER (abstract vs. concrete, continuous vs. discrete)?
# What kind of fractions did kids USE MORE (abstract vs. concrete, continuous vs. discrete) in Act4?
basicPreferencePlots <- function(surveyData){
  
  # What do kids prefer / from survey
  png("Q1-LikeApproach.Boxplots.png",width=1280,height=1024)  
  par(mfrow=c(1,3),cex.axis=0.75)
  boxplot(surveyData$Q1.More.Fun, ylim=c(1,5), main="Q1", sub="I prefer this way to what we do at school", xlab="Total")
  boxplot(surveyData$Q1.More.Fun ~ surveyData$Sequence, ylim=c(1,5), main="Q1", xlab="By sequence of fractions manipulative experienced",col=(c("lightgoldenrod","lightgreen")))
  boxplot(surveyData$Q1.More.Fun ~ surveyData$Sequence * surveyData$Session, ylim=c(1,5), main="Q1", xlab="By sequence AND session",col=(c("lightgoldenrod","lightgreen")))
  dev.off()
  
  png("Q2-PreferConcrete.Boxplots.png",width=1280,height=1024)  
  par(mfrow=c(1,3),cex.axis=0.75)
  boxplot(surveyData$Q2.Concrete.Repr, ylim=c(1,5), main="Q2", sub="I prefer concrete (vs. symbolic) fraction manip.", xlab="Total")
  boxplot(surveyData$Q2.Concrete.Repr ~ surveyData$Sequence, ylim=c(1,5), main="Q2", xlab="By sequence of fractions manipulative experienced", col=(c("lightgoldenrod","lightgreen")))
  boxplot(surveyData$Q2.Concrete.Repr ~ surveyData$Sequence * surveyData$Session, ylim=c(1,5), main="Q2", xlab="By sequence AND session", col=(c("lightgoldenrod","lightgreen")))
  dev.off()

  png("Q3-PreferContinuous.Boxplots.png",width=1280,height=1024)  
  par(mfrow=c(1,3),cex.axis=0.75)
  boxplot(surveyData$Q3.Continuous.Repr, ylim=c(1,5), main="Q3", sub="I prefer continuous (vs. token) fraction manip.", xlab="Total")
  boxplot(surveyData$Q3.Continuous.Repr ~ surveyData$Sequence, ylim=c(1,5), main="Q3", xlab="By sequence of fractions manipulative experienced", col=(c("lightgoldenrod","lightgreen")))
  boxplot(surveyData$Q3.Continuous.Repr ~ surveyData$Sequence * surveyData$Session, ylim=c(1,5), main="Q3", xlab="By sequence AND session", col=(c("lightgoldenrod","lightgreen")))
  dev.off()

  # How coherent are preferences within a group? range and std per group? Are there any outliers?
  
  
  # What do kids use when given the choice (Act4)?
  
  
  # How coherent is preference and usage when given the choice?
  
  
}