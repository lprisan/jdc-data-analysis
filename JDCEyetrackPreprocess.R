# JDCEyetrackPreprocess - Clean and organize the eyetracking data (get only the fields that are important for us)
# This is the overall function in charge of the preprocessing and cleaning of the eyetracker event data from the JDC experiment
# Parameters: rootDir the directory in which the logs to be merged/split are, named JDC-...-eyetracking-eventexport.txt
JDCEyetrackPreprocess <- function(rootDir="."){
  
  setwd(rootDir)
  
  
  # We get the pupil diameter from the eyetracker events
  files <- list.files(pattern = "\\-eyetracking-eventexport.txt$")
  
  pupildata <- FALSE
  
  for(file in files){
    filedata <- read.csv(file,comment.char="#")
    
    # we select the meaningful columns (for now, only pupil diameter, and kind of event)
    filedata <- filedata[c(1,6,9,30)]
    # TODO: add other measures?
    
    # We extract the session number from the filename, and create a variable for it
    index <- regexpr("Session",file,fixed=T)
    filedata$Session <- substr(file,index+7,index+7)
    
    if(length(pupildata) == 1) pupildata <- data.frame(filedata) # This is the first file
    else pupildata <- rbind(pupildata,filedata) # The global data frame exists, just add to it
    
  }
  
  # We get the fixation data from the fixation details raw file
  files <- list.files(pattern = "\\-fixationDetails.txt$")
  
  fixdata <- FALSE
  
  for(file in files){
    filedata <- read.csv(file,comment.char="#")
    
    # we select the meaningful columns (for now, only fixation times)
    filedata <- filedata[c(8,9,10)]
    # TODO: add other measures?
    
    # We extract the session number from the filename, and create a variable for it
    index <- regexpr("Session",file,fixed=T)
    filedata$Session <- substr(file,index+7,index+7)
    
    if(length(fixdata) == 1) fixdata <- data.frame(filedata) # This is the first file
    else fixdata <- rbind(fixdata,filedata) # The global data frame exists, just add to it
  }
  
  # We get the saccade data from the saccade details file
  files <- list.files(pattern = "\\-saccadeDetails.txt$")
  
  sacdata <- FALSE
  
  for(file in files){
    filedata <- read.csv(file,comment.char="#")
    
    # we select the meaningful columns (for now, only fixation times)
    filedata <- filedata[c(8,9,10,15)]
    # TODO: add other measures?
    
    # We extract the session number from the filename, and create a variable for it
    index <- regexpr("Session",file,fixed=T)
    filedata$Session <- substr(file,index+7,index+7)
    
    if(length(sacdata) == 1) sacdata <- data.frame(filedata) # This is the first file
    else sacdata <- rbind(sacdata,filedata) # The global data frame exists, just add to it
  }
  # We add the saccade speed for each saccade
  sacdata$Saccade.Speed <- sacdata$Amplitude.... / sacdata$Saccade.Duration..ms.
  
  # We split the data per session
  pupildata$Session <- as.factor(pupildata$Session)
  pupilsessions <- split(pupildata,pupildata$Session)
  fixdata$Session <- as.factor(fixdata$Session)
  fixsessions <- split(fixdata,fixdata$Session)
  sacdata$Session <- as.factor(sacdata$Session)
  sacsessions <- split(sacdata,sacdata$Session)
  
  # We add time information so that all metrics can be aligned
  for(i in 1:length(pupilsessions)){#For each session
    # We calculate the time baseline of the session
    time0 <- min(pupilsessions[[i]]$Time)
    pupilsessions[[i]]$Time.ms <- (pupilsessions[[i]]$Time - time0) / 1000
    
    fixsessions[[i]]$Time.ms <- (fixsessions[[i]]$Fixation.Start..ms. + (fixsessions[[i]]$Fixation.Duration..ms./2)) #We set the time of fixation in the middle of the fixation
    fixsessions[[i]]$Time <- time0 + (fixsessions[[i]]$Time.ms)*1000
    
    sacsessions[[i]]$Time.ms <- (sacsessions[[i]]$Saccade.Start..ms. + (sacsessions[[i]]$Saccade.Duration..ms./2)) #We set the time of saccade in the middle of the fixation
    sacsessions[[i]]$Time <- time0 + (sacsessions[[i]]$Time.ms)*1000
    
    #TODO: We might want to check the first event of a certain kind, so that we have a more correct baseline (but probably is negligible)
  }
  
  # We join the data again in a single frame containing all sessions
  f <- pupildata$Session
  pupildata <- unsplit(pupilsessions,f)
  fixdata <- unsplit(fixsessions,fixdata$Session)
  sacdata <- unsplit(sacsessions,sacdata$Session)
  
  # We save the data to respective files
  save(pupildata,file="EyetrackerEvents.rda",compress=TRUE)
  save(fixdata,file="EyetrackerFixations.rda",compress=TRUE)
  save(sacdata,file="EyetrackerSaccades.rda",compress=TRUE)
  
  
  cat ("Process finished! Check your .rda files")
  
}
