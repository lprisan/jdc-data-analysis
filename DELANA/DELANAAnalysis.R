require("ff")
require("ggplot2")
require("car")

setwd("/home/lprisan/workspace/jdc-data-analysis-eyetrack/DELANA")
source("rollingWindows.R")
#setwd("/data/tmpData")


# Loads the data from the raw text files, and gets from them only the needed fields (PD, fixation, saccade)
loadRawDELANAData <- function(){

    # List of available sessions (the data file names start with this)
    sessions <- c("20141021_Patrick_Session1",
                  "20141021_Patrick_Session2",
                  "20141021_Kshitij_Session3",
                  "20141028_Kshitij_Session4")
    
    rawdir <- "20141021-rawdata"
    
    for (session in sessions){
        # We load the raw pupil event data
        data <- read.csv(file=paste("./",rawdir,"/",session,"-events.txt",sep=""),as.is=T,comment.char="#")
        # We select the needed fields (Time, left PD, right PD [mm])
        data <- data[,c(1,6,9)]
        # We create the zero-based ms timestamp for the session
        time0 <- min(data$Time)
        data$Time.ms <- (data$Time - time0) / 1000
        data$Session <- rep(session,nrow(data))
        save(data,file=paste("./",rawdir,"/",session,"-RawEvents.Rda",sep=""))
        
        # We load the fixation data
        fixdata <- read.csv(file=paste("./",rawdir,"/",session,"-fixationDetails.txt",sep=""),sep=";",as.is=T,comment.char="#")
        # We select the relevant fields (fix. start, duration, end)
        fixdata <- fixdata[,c(8,9,10)]
        #We set the time of the fixation in the middle of the fixation
        fixdata$Time.ms <- (fixdata$Fixation.Start..ms. + (fixdata$Fixation.Duration..ms./2)) 
        # We create a Time field so that we have the time in both timestamp and ms formats
        fixdata$Time <- time0 + (fixdata$Time.ms)*1000
        fixdata$Session <- rep(session,nrow(fixdata))
        save(fixdata,file=paste("./",rawdir,"/",session,"-RawFixations.Rda",sep=""))

        # We load the fixation data
        sacdata <- read.csv(file=paste("./",rawdir,"/",session,"-saccadeDetails.txt",sep=""),sep=";",as.is=T,comment.char="#")
        # We select the relevant fields (fix. start, duration, end)
        sacdata <- sacdata[,c(8,9,10,15)]
        # We add the saccade speed for each saccade
        sacdata$Saccade.Speed <- sacdata$Amplitude.... / sacdata$Saccade.Duration..ms.
        #We set the time of saccade in the middle of the fixation
        sacdata$Time.ms <- (sacdata$Saccade.Start..ms. + (sacdata$Saccade.Duration..ms./2)) 
        # We create a Time field so that we have the time in both timestamp and ms formats
        sacdata$Time <- time0 + (sacdata$Time.ms)*1000
        sacdata$Session <- rep(session,nrow(sacdata))
        save(sacdata,file=paste("./",rawdir,"/",session,"-RawSaccades.Rda",sep=""))
    }
}

# Does the rolling window load analysis, with a window and slide of 10s and 5s (by default)
doLoadAnalyses <- function(window=10,slide=5){
    # List of available sessions (the data file names start with this)
    sessions <- c("20141021_Patrick_Session1",
                  "20141021_Patrick_Session2",
                  "20141021_Kshitij_Session3",
                  "20141028_Kshitij_Session4")
    
    rawdir <- "20141021-rawdata"
    cleandir <- "20141021-cleandata"
    
    
    for (session in sessions){
        
        totaldata <- data.frame()

        # We load the clean data, just in case we did not the previous steps
        pupildata <- get(load(paste("./",rawdir,"/",session,"-RawEvents.Rda",sep="")))
        fixdata <- get(load(paste("./",rawdir,"/",session,"-RawFixations.Rda",sep="")))
        sacdata <- get(load(paste("./",rawdir,"/",session,"-RawSaccades.Rda",sep="")))
        
        # We get the rolling window for the mean pupil diameter, and its median value for a median cut
        meandata <- rollingMean(pupildata$Time.ms,pupildata$L.Pupil.Diameter..mm.,window*1000,slide*1000,inittime=0)
        meanPDmedian <- median(meandata$value)
        meandata$above <- as.numeric(meandata$value > meanPDmedian)
        
        # We get the rolling window for the SD of pupil diameter, and its median value for a median cut
        sddata <- rollingSd(pupildata$Time.ms,pupildata$L.Pupil.Diameter..mm.,window*1000,slide*1000,inittime=0)
        sdPDmedian <- median(sddata$value)
        sddata$above <- as.numeric(sddata$value > sdPDmedian)
        
        # We get the number of long fixations in the window, and its median
        longdata <- rollingLong(fixdata$Time.ms,fixdata$Fixation.Duration..ms.,window*1000,slide*1000,inittime=0)
        longFixMedian <- median(longdata$value)
        longdata$above <- as.numeric(longdata$value > longFixMedian)
        
        # We get the saccade speed in the window
        sacspdata <- rollingMean(sacdata$Time.ms,sacdata$Saccade.Speed,window*1000,slide*1000,inittime=0)
        sacSpMedian <- median(sacspdata$value)
        sacspdata$above <- as.numeric(sacspdata$value > sacSpMedian)
        
        totaldata <- merge(meandata,sddata,by="time",suffixes = c(".Mean",".SD"),all=T)
        totaldata <- merge(totaldata,longdata,by="time",all=T)
        names(totaldata)[[6]] <- paste(names(totaldata)[[6]],"Fix",sep=".")
        names(totaldata)[[7]] <- paste(names(totaldata)[[7]],"Fix",sep=".")
        totaldata <- merge(totaldata,sacspdata,by="time",all=T)
        names(totaldata)[[8]] <- paste(names(totaldata)[[8]],"Sac",sep=".")
        names(totaldata)[[9]] <- paste(names(totaldata)[[9]],"Sac",sep=".")
        
        totaldata$Load <- totaldata$above.Mean + totaldata$above.SD + totaldata$above.Fix + totaldata$above.Sac
        totaldata$Session <- rep(session,nrow(totaldata))
        
        # We save all the (clean) window data in a file, for later use
        save(totaldata,meanPDmedian,sdPDmedian,longFixMedian,sacSpMedian,window,slide,
             file=paste("./",cleandir,"/",session,"-LoadMetrics.Rda",sep=""))
        
    }
    
    
}


extractExtremeLoadMoments <- function(){

    sessions <- c("20141021_Patrick_Session1",
                  "20141021_Patrick_Session2",
                  "20141021_Kshitij_Session3",
                  "20141028_Kshitij_Session4")
    
    cleandir <- "20141021-cleandata"

    interesting <- data.frame()

    for(session in sessions){
        load(paste("./",cleandir,"/",session,"-LoadMetrics.Rda",sep=""))
        data <- totaldata
        data <- data[data$Load==max(data$Load) | data$Load==min(data$Load),c("Session", "time")]
        data$Time.minsec <- msToMinSec(data$time)

        if(nrow(interesting)==0) interesting <- data
        else interesting<-rbind(interesting,data)
    }

    # We add the rest of the fields, empty
    # Description: short narrative description
    interesting$Description <- character(nrow(interesting))
    # Focus: FAC/student faces, BAK/student backs, TCOMP/teacher computer, SCOMP/student computer, PROJ/projector, WHIT/whiteboard
    interesting$Focus <- character(nrow(interesting)) 
    # Large.Head.Movement: YES, NO
    interesting$Large.Head.Movement <- character(nrow(interesting))
    # Activity: EXP/Explanation, REP/Repairs, of technical problems or solve student doubts , MON/Monitor, scan, awareness, QUEST/Ask questions to students and listen to answers, TDT/Task distribution or transitions
    interesting$Activity <- character(nrow(interesting))
    # Social.Plane (the plane at which the current activity is done): IND/Individual work, GRP/Pairs or small groups, CLS/Classroom-level
    interesting$Social.Plane <- character(nrow(interesting))

    write.csv(interesting, file="TimesToVideoCode.csv")

}

msToMinSec <- function(millis){
  mins <- floor(millis/60000)
  secs <- (millis - (mins*60000))/1000
  
  string <- paste(mins,"m",as.character(secs),"s",sep="")
  string
}

plotLoadGraphs <- function(){
    
    # List of available sessions (the data file names start with this)
    sessions <- c("20141021_Patrick_Session1",
                  "20141021_Patrick_Session2",
                  "20141021_Kshitij_Session3",
                  "20141028_Kshitij_Session4")
    
    rawdir <- "20141021-rawdata"
    cleandir <- "20141021-cleandata"
    graphdir <- "20141021-graphs"
    
    overalldata <- data.frame()
    
    for(session in sessions){
        
        load(paste("./",cleandir,"/",session,"-LoadMetrics.Rda",sep=""))
        
        png(filename=paste("./",graphdir,"/",session,"-LoadHistogram.png",sep=""),width=1280,height=960)
        print(qplot(totaldata$Load, binwidth=1,main=paste("Load Indexes for ",session,sep="")))
        dev.off()
        
#         p1 <- ggplot(totaldata, aes(x=time, y=value.Mean)) + 
#             ggtitle(paste("Pupil diameter MEAN over ",window,"s",sep="")) + 
#             geom_line() + geom_hline(yintercept=meanPDmedian) +
#             theme(axis.text.x = element_blank(),plot.title=element_text(size=20),axis.title=element_text(size=18))
#         print(p1)
#         
#         p2 <- ggplot(totaldata, aes(x=time, y=value.SD)) + 
#             ggtitle(paste("Pupil diameter SD over ",window,"s",sep="")) + 
#             geom_line() + geom_hline(yintercept=sdPDmedian) +
#             theme(axis.text.x = element_blank(),plot.title=element_text(size=20),axis.title=element_text(size=18))
#         print(p2)
#         
#         p3 <- ggplot(totaldata, aes(x=time, y=value.Fix)) + 
#             ggtitle(paste("Fixations >500ms over ",window,"s",sep="")) + 
#             geom_line() + geom_hline(yintercept=longFixMedian) +
#             theme(axis.text.x = element_blank(),plot.title=element_text(size=20),axis.title=element_text(size=18))
#         print(p3)
#         
#         p4 <- ggplot(totaldata, aes(x=time, y=value.Sac)) + 
#             ggtitle(paste("Saccade speed over ",window,"s",sep="")) + 
#             geom_line() + geom_hline(yintercept=sacSpMedian) +
#             theme(axis.text.x = element_blank(),plot.title=element_text(size=20),axis.title=element_text(size=18))
#         print(p4)
        
        png(filename=paste("./",graphdir,"/",session,"-LoadGraph.png",sep=""),width=1920,height=960)
        p5 <- ggplot(totaldata, aes(x=time, y=Load, col=Load)) + 
            ggtitle(paste("Load Index\n(estimation of cognitive overload over ",window,"s)\n",session,sep="")) + 
            geom_line(size=1) + stat_smooth(method="loess",span=0.1) +
            theme(axis.text.x = element_blank(),plot.title=element_text(size=20, face="bold"),axis.title=element_text(size=18),panel.background = element_rect(fill = 'white')) +
            scale_color_gradient(low="green",high="red")
        print(p5)
        dev.off()
        
        #We can see how correlated these four measurements are to each other:
        print(paste("Correlations for session",session))
        print(cor(totaldata[,c("above.Mean","above.SD","above.Fix","above.Sac")]))

        if(nrow(overalldata)==0) overalldata <- totaldata
        else overalldata <- rbind(overalldata,totaldata)
    }
    
    png(filename=paste("./",graphdir,"/OverallSessionComparison.png",sep=""),width=1280,height=960)
    boxplot(Load ~ as.factor(Session), data=overalldata, main="Load comparison by sessions")
    dev.off()

    
    
}

doVideoCodingAnalysis <- function(){
    
    cleandir <- "20141021-cleandata"

    # Load and merge load and coding data
    videocodes <- read.csv("VideoCoding.csv")
    
    loaddata <- data.frame()
    
    for(session in levels(videocodes$Session)){
        
        load <- get(load(paste("./",cleandir,"/",session,"-LoadMetrics.Rda",sep="")))

        if(nrow(loaddata)==0) loaddata <- load
        else loaddata <- rbind(loaddata,load)
    }
    
    totaldata <- merge(loaddata,videocodes,by=c("Session","time"),all=T)

    # We remove non-coded or otherwise incomplete moments
    totaldata <- totaldata[complete.cases(totaldata),]
    
    # We merge individual and group social planes, as they are largely equivalent in the context of these sessions
    levels(totaldata$Social.Plane) = c("CLS","GRPIND","GRPIND")

    # Overall analysis
    print(paste("Analyzing overall data ====================================================\n"))
    
    # We now do some tables with the video coded occurrences, and calculate some basic chi-squared tests of independence
    totaldata$Load <- as.factor(totaldata$Load)
    
    # We eliminate levels not used
    totaldata$Activity <- factor(totaldata$Activity)
    print(tabAct <- table(totaldata$Load,totaldata$Activity))
    print(chisq.test(tabAct))
    
    # We eliminate levels not used
    totaldata$Social.Plane <- factor(totaldata$Social.Plane)
    print(tabSoc <- table(totaldata$Load,totaldata$Social.Plane))
    print(chisq.test(tabSoc))
    
    # We eliminate levels not used
    totaldata$Focus <- factor(totaldata$Focus)
    print(tabFoc <- table(totaldata$Load,totaldata$Focus))
    print(chisq.test(tabFoc))
    
    # We eliminate levels not used
    totaldata$Large.Head.Movement <- factor(totaldata$Large.Head.Movement)
    print(tabCha <- table(totaldata$Load,totaldata$Large.Head.Movement))
    print(chisq.test(tabCha))
    
    
    # Analyze participant-wise
    participants <- c("Kshitij","Patrick")
    for(participant in participants){
        print(paste("Analyzing video coding for participant",participant,"==========================================\n"))
        
        partdata <- totaldata[grepl(participant,totaldata$Session),]
        
        # We now do some tables with the video coded occurrences, and calculate some basic chi-squared tests of independence
        partdata$Load <- as.factor(partdata$Load)
        
        # We eliminate levels not used in this session
        partdata$Activity <- factor(partdata$Activity)
        print(tabAct <- table(partdata$Load,partdata$Activity))
        print(chisq.test(tabAct))
        
        # We eliminate levels not used in this session
        partdata$Social.Plane <- factor(partdata$Social.Plane)
        print(tabSoc <- table(partdata$Load,partdata$Social.Plane))
        print(chisq.test(tabSoc))
        
        # We eliminate levels not used in this session
        partdata$Focus <- factor(partdata$Focus)
        print(tabFoc <- table(partdata$Load,partdata$Focus))
        print(chisq.test(tabFoc))
        
        print(tabCha <- table(partdata$Load,partdata$Large.Head.Movement))
        print(chisq.test(tabCha))
        
        
    }
    
    # Analyze session-wise
    for(session in levels(as.factor(totaldata$Session))){
        
        print(paste("Analyzing video coding for session",session,"==========================================\n"))
        
        sessiondata <- totaldata[totaldata$Session == session,]
        
        # We now do some tables with the video coded occurrences, and calculate some basic chi-squared tests of independence
        sessiondata$Load <- as.factor(sessiondata$Load)
        
        # We eliminate levels not used in this session
        sessiondata$Activity <- factor(sessiondata$Activity)
        print(tabAct <- table(sessiondata$Load,sessiondata$Activity))
        print(chisq.test(tabAct))
        
        # We eliminate levels not used in this session
        sessiondata$Social.Plane <- factor(sessiondata$Social.Plane)
        print(tabSoc <- table(sessiondata$Load,sessiondata$Social.Plane))
        print(chisq.test(tabSoc))
        
        # We eliminate levels not used in this session
        sessiondata$Focus <- factor(sessiondata$Focus)
        print(tabFoc <- table(sessiondata$Load,sessiondata$Focus))
        print(chisq.test(tabFoc))
        
        print(tabCha <- table(sessiondata$Load,sessiondata$Large.Head.Movement))
        print(chisq.test(tabCha))
        
        
    }
    
    totaldata
}