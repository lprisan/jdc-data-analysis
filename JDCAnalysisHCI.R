#require("lattice")
require("ggplot2")
require("reshape2")
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
  logdir <- paste(rootDir,"/logs",sep="")
  #logData <- getLogSummaryHCI(logdir,mapsData)
  
  # We plot the different questions/themes we had
  # Basic plots of children preferences
  #basicPreferencePlots(surveyData, act4UsageSummary)
  act4UsageSummary <- getAct4UsageLogSummary(logdir, mapsData)
  basicPreferencePlots(surveyData, act4UsageSummary)
  
  # Bad uses of the interface
  
  # Manipulative usage
  manipulativeUsagePlots(act4UsageSummary)
  
  # Collaboration (ownership, awareness)
  
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

