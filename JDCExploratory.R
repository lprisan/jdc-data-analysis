# This is the global function that runs the whole set of exploratory data analyses
JDCExplorations <- function(){
  
  setwd("/home/lprisan/workspace/jdc-data-analysis/logs")
  
  # Do the 
  for(file in list.files(pattern = "\\.rda$")){
    logPointsInTime(getwd(),file)
    logSamplesElementsPresent(getwd(),file)
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
  dev.off()
  
}

# Make graphics with the time a kind of representation was on the table, for a group
logSamplesElementsPresent <- function(rootDir,datafile){
  
  setwd(rootDir)
  data <- get(load(datafile))
  
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