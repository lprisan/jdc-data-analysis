library("lattice")
library("ggplot2")

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
  logCrossedWithSurveys(logSummary, surveyData)
}

# This function gets basic log statistics for each group (e.g. total duration available, 
# total samples available, samples with each kind of representation on table, samples 
# with each kind of help on table)
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
                           Help.Frac)

  logSummary
}

# Draws some plots about crossing the log summaries with the survey data and sequence of representations
logCrossedWithSurveys <- function(logSummary,surveyData){
  
  # We merge both tables, will put NAs where we do not have data
  totalData <- merge(x=surveyData,y=logSummary,by.x="Group.Number",by.y="Group.Name",all=TRUE)
  # Same thing, but only with the complete rows, no NAs
  totalDataComplete <- merge(x=surveyData,y=logSummary,by.x="Group.Number",by.y="Group.Name")
  
  # We plot the hierarchical clustering of data
  totalMatrix <- data.matrix(totalDataComplete[,c(1:2,4:17)])
  hc <- hclust(dist(totalMatrix)) 
  plot(hc)
  
  # We try k-means clustering with 4/5 clusters
  kmeansObj <- kmeans(totalMatrix, centers=5)
  
  # We do SVD
  matrixOrdered <- totalMatrix[hc$order,]
  svd1 <- svd(scale(matrixOrdered))
  plot(svd1$d, xlab="Col",ylab="Singular value")
  plot(svd1$d^2/sum(svd1$d^2),xlab="Col",ylab="Prop. variance explained");
  
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