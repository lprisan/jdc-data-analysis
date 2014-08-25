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
  basicEyetrackerPlots(pupildata, fixdata, sacdata)
  
  jointEyetrackerPlots(pupildata, fixdata, sacdata, window=10, slide=5)
  
}


# countLong - returns the number of values within a vector that are greater than 500
countLong <- function(x){
  
  logical <- x > 500
  number <- sum(logical)
  number
}

# This one accepts window size in seconds, and window slide in seconds too
jointEyetrackerPlots <- function(pupildata, fixdata, sacdata, window=30, slide=5, meanormedian="median"){
 
  # We get the data for each session
  pupildata$Session <- as.factor(pupildata$Session)
  pupilsessions <- split(pupildata,pupildata$Session)
  fixdata$Session <- as.factor(fixdata$Session)
  fixsessions <- split(fixdata,fixdata$Session)
  sacdata$Session <- as.factor(sacdata$Session)
  sacsessions <- split(sacdata,sacdata$Session)
  
  
  for(i in 1:length(pupilsessions)){
    
    
    
    png(paste("Eyetrack.Session",pupilsessions[[i]]$Session[[1]],".window",window,"s.slide",slide,"s.",meanormedian,".png",sep=""),width=1280,height=1024)  
    
    # We get the PD mean over a rolling window with the parameters set when calling the function (everything in ms)
    meandata <- rollingMean(pupilsessions[[i]]$Time.ms,pupilsessions[[i]]$L.Pupil.Diameter..mm.,window*1000,slide*1000)
    meansessionavg <- 0
    if(meanormedian=="median") meansessionavg <- median(meandata$value)
    else meansessionavg <- mean(meandata$value)
    p1 <- ggplot(meandata, aes(x=time, y=value)) + 
      ggtitle(paste("Pupil diameter MEAN over ",window,"s",sep="")) + 
      geom_line() + geom_hline(yintercept=meansessionavg)

    # We get the PD standard deviation over a rolling window with the parameters set when calling the function (everything in ms)
    sddata <- rollingSd(pupilsessions[[i]]$Time.ms,pupilsessions[[i]]$L.Pupil.Diameter..mm.,window*1000,slide*1000)
    sdsessionavg <- 0
    if(meanormedian=="median") sdsessionavg <- median(sddata$value)
    else sdsessionavg <- mean(sddata$value)
    p2 <- ggplot(sddata, aes(x=time, y=value)) + 
      ggtitle(paste("Pupil diameter SD over ",window,"s",sep="")) + 
      geom_line() + geom_hline(yintercept=sdsessionavg)
    
    # We get the number of long fixations in the window
    longdata <- rollingLong(fixsessions[[i]]$Time.ms,fixsessions[[i]]$Fixation.Duration..ms.,window*1000,slide*1000)
    longsessionavg <- 0
    if(meanormedian=="median") longsessionavg <- median(longdata$value)
    else longsessionavg <- mean(longdata$value)
    p3 <- ggplot(longdata, aes(x=time, y=value)) + 
      ggtitle(paste("Fixations >500ms over ",window,"s",sep="")) + 
      geom_line() + geom_hline(yintercept=longsessionavg)

    # We get the saccade speed in the window
    sacspdata <- rollingMean(sacsessions[[i]]$Time.ms,sacsessions[[i]]$Saccade.Speed,window*1000,slide*1000)
    sacsessionavg <- 0
    if(meanormedian=="median") sacsessionavg <- median(sacspdata$value)
    else sacsessionavg <- mean(sacspdata$value)
    p4 <- ggplot(sacspdata, aes(x=time, y=value)) + 
      ggtitle(paste("Saccade speed over ",window,"s",sep="")) + 
      geom_line() + geom_hline(yintercept=sacsessionavg)

    # We try to get how many measures went over the average at a given point in time... 
    # first, we merge all data frames
    totaldata <- merge(meandata,sddata,by="time",suffixes = c(".Mean",".SD"),all=T)
    totaldata <- merge(totaldata,longdata,by="time",suffixes = c("", ".Fix"),all=T)
    totaldata <- merge(totaldata,sacspdata,by="time",suffixes = c("",".Sac"),all=T)
    names(totaldata)[[4]] <- "value.Fix"
    
    #cat(paste("All 4 metrics merged for session ",pupilsessions[[i]]$Session[[1]],". Incomplete cases: ",sum(!complete.cases(totaldata)),sep=""))
    
    totaldata$Above.Mean <- as.numeric(totaldata$value.Mean > meansessionavg)
    totaldata$Above.SD <- as.numeric(totaldata$value.SD > sdsessionavg)
    totaldata$Above.Fix <- as.numeric(totaldata$value.Fix > longsessionavg)
    totaldata$Above.Sac <- as.numeric(totaldata$value.Sac > sacsessionavg)
    #totaldata$Load <- totaldata$Above.Mean + totaldata$Above.SD + totaldata$Above.Fix + totaldata$Above.Sac
    # By now, we leav out the mean PD, as it seems to be unreliable
    totaldata$Load <- totaldata$Above.SD + totaldata$Above.Fix + totaldata$Above.Sac
    
    p5 <- ggplot(totaldata, aes(x=time, y=Load)) + 
      ggtitle(paste("Estimation of cognitive overload over ",window,"s",sep="")) + 
      geom_line() + stat_smooth(method="loess",span=0.02)
    
    #multiplot(p1, p2, p3, p4, p5, cols=1)
    multiplot(p2, p3, p4, p5, cols=1)
    dev.off()
   
    #We do run length compression onto the load data, to detect interesting episodes
    #runs <- rle(totaldata$Load)
    
    interesting <- totaldata[totaldata$Load>=max(totaldata$Load),"time"]
    
    dataToLook <- as.data.frame(interesting)
    names(dataToLook) <- "Time.ms"
    dataToLook$Time.min <- msToMinSec(interesting)
    dataToLook$Session <- pupilsessions[[i]]$Session[[1]]
    dataToLook$Load <- max(totaldata$Load)
    
    write.csv(dataToLook, file=paste("Loaded.Times.Session.",pupilsessions[[i]]$Session[[1]],".window",window,"s.slide",slide,"s.",meanormedian,".csv",sep=""))

    interesting <- totaldata[totaldata$Load==min(totaldata$Load),"time"]
    
    dataToLook <- as.data.frame(interesting)
    names(dataToLook) <- "Time.ms"
    dataToLook$Time.min <- msToMinSec(interesting)
    dataToLook$Session <- pupilsessions[[i]]$Session[[1]]
    dataToLook$Load <- min(totaldata$Load)
    
    write.csv(dataToLook, file=paste("Unloaded.Times.Session.",pupilsessions[[i]]$Session[[1]],".window",window,"s.slide",slide,"s.",meanormedian,".csv",sep=""))
    
    
    
  }
  
}


msToMinSec <- function(millis){
  mins <- floor(millis/60000)
  secs <- (millis - (mins*60000))/1000
  
  string <- paste(mins,"m",as.character(secs),"s",sep="")
  string
}

# We get the number of values over a value in a rolling window with the parameters set when calling the function (everything in ms)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingLong <- function(times,values,window,slide,threshold=500){
  
  inittime <- 0
  endtime <- window
  
  rolllong <- data.frame(time=numeric(), value=numeric())
  
  while(endtime <= max(times)){
    
    tim <- inittime + (window/2)
    
    val <- sum(times >= inittime & times <= endtime & values > threshold)
    
    if(nrow(rolllong)==0) rolllong <- data.frame(time=tim,value=val)
    else rolllong <- rbind(rolllong,c(time=tim,value=val))
    
    inittime <- inittime+slide
    endtime <- endtime+slide
    
  }
  
  rolllong
  
}


# We get the mean over a rolling window with the parameters set when calling the function (everything in ms)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingMean <- function(times,values,window,slide){
  
  inittime <- 0
  endtime <- window
  
  rollmean <- data.frame(time=numeric(), value=numeric())
  
  while(endtime <= max(times)){
    
    tim <- inittime + (window/2)
    
    val <- mean(values[times >= inittime & times <= endtime])
    
    if(nrow(rollmean)==0) rollmean <- data.frame(time=tim,value=val)
    else rollmean <- rbind(rollmean,c(time=tim,value=val))
    
    inittime <- inittime+slide
    endtime <- endtime+slide
    
  }
  
  rollmean
  
}

# We get the PD mean over a rolling window with the parameters set when calling the function (everything in ms)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingSd <- function(times,values,window,slide){
  
  inittime <- 0
  endtime <- window
  
  rollsd <- data.frame(time=numeric(), value=numeric())
  
  while(endtime <= max(times)){
    
    tim <- inittime + (window/2)
    
    val <- sd(values[times >= inittime & times <= endtime])
    
    if(nrow(rollsd)==0) rollsd <- data.frame(time=tim,value=val)
    else rollsd <- rbind(rollsd,c(time=tim,value=val))
    
    inittime <- inittime+slide
    endtime <- endtime+slide
    
  }
  
  rollsd
  
}





# This function receives a data frame with eyetracking events (pupil diameter), and it
# draws basic line plots regarding their evolution over time (using a sliding window)
# Currently, calculates mean and standard deviation over the defined windows
# The default window size is 10s (300 samples), with 10 samples window slide
basicEyetrackerPlots <- function(pupildata, fixdata, sacdata, window=300, slide=10){
  
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

