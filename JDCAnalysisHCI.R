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
  
  # We plot the different questions/themes we had
  # Basic plots of children preferences
  
  # Bad uses of the interface
  
  # Manipulative usage
  
  # Collaboration (ownership, awareness)
  
}