require("ggplot2")
require("zoo")


# JDCEyetrackingAnalysis - this function analyzes and creates a series of graphs using the eyetracking data from the JDC experiment.
# It receives as an optional parameter the folder where the (pre-processed) eyetracking data (Eyetracker*.rda) is
JDCEyetrackingAnalysis <- function(rootDir="."){
  originalDir <- getwd()
  setwd(rootDir)
  pupildata <- get(load("EyetrackerEvents.rda"))
  fixdata <- get(load("EyetrackerFixations.rda"))
  sacdata <- get(load("EyetrackerSaccades.rda"))
  
  setwd(originalDir)
  # We do some basic plotting of eyetracking parameters
  eyetrackerPlots(pupildata, fixdata, sacdata)
  
  
}


# countLong - returns the number of values within a vector that are greater than 500
countLong <- function(x){
  
  logical <- x > 500
  number <- sum(logical)
  number
}


# This function receives a data frame with eyetracking events (pupil diameter), and it
# draws basic line plots regarding their evolution over time (using a sliding window)
# Currently, calculates mean and standard deviation over the defined windows
# The default window size is 10s (300 samples), with 10 samples window slide
eyetrackerPlots <- function(pupildata, fixdata, sacdata, window=300, slide=10){
  
  # We split each session's data, and put it into a list
  pupildata$Session <- as.factor(pupildata$Session)
  sessionsdata <- split(pupildata,pupildata$Session)
  
  # We make a sliding window mean and std of the data
  # TODO: add the timestamp of the middle of each window, for later browsing 
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
  
  # We make a sliding window of the fixations with duration > 500ms
  fixdata$Session <- as.factor(fixdata$Session)
  sessionsdata <- split(fixdata,fixdata$Session)
  for(i in 1:length(sessionsdata)){
    # We calculate the length of the window in fixations, so that we have a window of 30s approx
    minutes <- max(sessionsdata[[i]]$Fixation.End..ms.) / 60000
    fixationspermin <- length(sessionsdata[[i]]$Session) / minutes
    fixwindow <- ceiling(fixationspermin/2)
    fixslide <- ceiling(fixwindow / 10)
    
    png(paste("Session",sessionsdata[[i]]$Session[[1]],".Num.Fixations.gt500ms.",fixwindow,"samples.slide",fixslide,".png",sep=""),width=1280,height=1024)  
    par(mfcol=c(1,1))
    longdata <- rollapply(sessionsdata[[i]]$Fixation.Duration..ms., width = fixwindow, by = fixslide, FUN = countLong, align = "left")
    plot(longdata, type="n", main=paste("Fixations >500ms, Session",sessionsdata[[i]]$Session[[1]]))
    lines(longdata)
    dev.off()
  }
  
  
  # We calculate the saccade speed of each saccade, in degrees/s
  sacdata$Speed <- sacdata$Amplitude.... / (sacdata$Saccade.Duration..ms. / 1000)
  
  sacdata$Session <- as.factor(sacdata$Session)
  sessionsdata <- split(sacdata,sacdata$Session)
  for(i in 1:length(sessionsdata)){
    # We calculate the length of the window in saccades, so that we have a window of 30s approx
    minutes <- max(sessionsdata[[i]]$Saccade.End..ms.) / 60000
    sacspermin <- length(sessionsdata[[i]]$Session) / minutes
    sacwindow <- ceiling(sacspermin/2)
    sacslide <- ceiling(sacwindow / 10)

    png(paste("Session",sessionsdata[[i]]$Session[[1]],".Saccade.Speed.",sacwindow,"samples.slide",sacslide,".png",sep=""),width=1280,height=1024)  
    par(mfcol=c(1,1))
    spddata <- rollapply(sessionsdata[[i]]$Speed, width = sacwindow, by = sacslide, FUN = mean, align = "left")
    plot(spddata,type="n", main=paste("Saccade Speed, Session",sessionsdata[[i]]$Session[[1]]))
    lines(spddata)
    dev.off()
  }
  
  # TODO: preserve all timings and map one timing to another, so that the scales are comparable
  # TODO: plot the moments a metrics is over the session's average?
  
  
  
}
