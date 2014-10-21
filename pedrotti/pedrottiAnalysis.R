require("ff")
require("ggplot2")
require("car")

setwd("/home/lprisan/workspace/jdc-data-analysis/pedrotti")

source("rollingWindows.R")

# Loads the data from the raw text files, which are long strings of numbers, one line per participant
# in different tasks and conditions (control and experimental/stressed)
loadRawPedrottiData <- function(samplingfreq=50){
    sample <- 1000/samplingfreq # length of each sample, for timestamp purposes
    #Reading the pupil data
    totaldata <- data.frame()
    for (i in 0:4){
        # We load the control data
        data <- read.csv(file=paste("t",i,"_control.txt",sep=""),header=F,sep="\t")
        controlsubj <- nrow(data) 
        for(subj in 1:controlsubj){
            timestamp <- 0
            if(nrow(totaldata)==0) totaldata <- data.frame(subjID=rep.int(subj,length(data[subj,])),
                                                           Cond=rep.int("Control",length(data[subj,])),
                                                           Task=rep.int(i,length(data[subj,])),
                                                           time.ms=seq(from=timestamp,by=sample,length.out=length(data[subj,])),
                                                           PD=as.numeric(as.vector(data[subj,])))
            else totaldata <- rbind(totaldata,data.frame(subjID=rep.int(subj,length(data[subj,])),
                                                         Cond=rep.int("Control",length(data[subj,])),
                                                         Task=rep.int(i,length(data[subj,])),
                                                         time.ms=seq(from=timestamp,by=sample,length.out=length(data[subj,])),
                                                         PD=as.numeric(as.vector(data[subj,]))))
        }
        data <- read.csv(file=paste("t",i,"_experimental.txt",sep=""),header=F,sep="\t")
        for(subj in 1:nrow(data)){
            timestamp <- 0
            if(nrow(totaldata)==0) totaldata <- data.frame(subjID=rep.int(controlsubj+subj,length(data[subj,])),
                                                           Cond=rep.int("Experimental",length(data[subj,])),
                                                           Task=rep.int(i,length(data[subj,])),
                                                           time.ms=seq(from=timestamp,by=sample,length.out=length(data[subj,])),
                                                           PD=as.numeric(as.vector(data[subj,])))
            else totaldata <- rbind(totaldata,data.frame(subjID=rep.int(controlsubj+subj,length(data[subj,])),
                                                         Cond=rep.int("Experimental",length(data[subj,])),
                                                         Task=rep.int(i,length(data[subj,])),
                                                         time.ms=seq(from=timestamp,by=sample,length.out=length(data[subj,])),
                                                         PD=as.numeric(as.vector(data[subj,]))))
        }
        
    }
    totaldata$Cond=as.factor(totaldata$Cond)
    save(totaldata,file="UnifiedRawPedrottiData.Rda")
    totaldata
}

plotPedrottiGraphs <- function(){
    
    df <- get(load("PedrottiProcessedData.Rda"))

    png("Variables.ScatterplotMatrix.png",width=1280,height=1024)  
    scatterplotMatrix(~Load+time+Cond+Task+subjID, data=df)
    dev.off()
    
    
    ag <- aggregate(. ~ Cond+time,data=df,mean)
    
    png("Load.byCond.byTime.png",width=1280,height=1024)
    print(qplot(ag$time,ag$Load,col=ag$Cond,geom=c("line","smooth")))
    dev.off()

    png("Boxplot.byCond.png",width=1280,height=1024)  
    boxplot(Load~Cond , data=df)
    dev.off()
    
    
}


preprocessPupilData <- function(pupil){
    
    tasks = unique(pupil$Task)
    subjects = unique(pupil$subjID)
    conditions = unique(pupil$Cond)
    window = 10000
    slide = 5000
    
    
    # This is the dataframe that will contain our processed dataset
    totaldata <- data.frame()
    # We process all the eyetracking and game variables for each game
    for (subj in subjects){
        subjdata <- data.frame()
        for (task in tasks){
            #We select one task and subject
            taskpupil <- pupil[pupil$Task==task & pupil$subjID==subj,]

            # We transform the timestamps so that the tasks of a subject appear on a single timeline
            taskpupil$session.time.ms = taskpupil$time.ms + (task*nrow(taskpupil)*20)
                
            
            pupilMean <- rollingMean(taskpupil$session.time.ms,taskpupil$PD,window,slide)
            pupilSD <- rollingSd(taskpupil$session.time.ms,taskpupil$PD,window,slide)
            
            data <- merge(pupilMean,pupilSD,by="time",suffixes=c(".pupilMean",".pupilSD"))
            
            data$Task <- rep(task,nrow(data))
            data$Cond <- rep(taskpupil[1,"Cond"],nrow(data))

            # We join the subject data to our global dataset
            if(length(subjdata)==0) subjdata <- data
            else subjdata <- rbind(subjdata,data)
            
            
        }
        
        # We calculate the load index
        meansessionmed <- median(subjdata$value.pupilMean)
        subjdata$Above.Mean <- as.numeric(subjdata$value.pupilMean > meansessionmed)
        sdsessionmed <- median(subjdata$value.pupilSD)
        subjdata$Above.SD <- as.numeric(subjdata$value.pupilSD > sdsessionmed)
        subjdata$Load <- subjdata$Above.Mean + subjdata$Above.SD
        
        subjdata$subjID <- rep(subj,nrow(subjdata))
        
        
        # We join the subject data to our global dataset
        if(length(totaldata)==0) totaldata <- subjdata
        else totaldata <- rbind(totaldata,subjdata)
        
        
    }
    
    save(totaldata,file="PedrottiProcessedData.Rda")
    
    
}


eyetrackingPlots <- function(data){
    
    png("Load.Gamevariables.ScatterplotMatrix.png",width=1280,height=1024)  
    scatterplotMatrix(~Load+value.numHoles+value.stackMean+value.stackVar+value.stackMax+value.stackMin, data=data)
    dev.off()

    png("LoadComponents.Gamevariables.ScatterplotMatrix.png",width=1280,height=1024)  
    scatterplotMatrix(~Load+value.pupilMean+value.pupilSD+value.longFix+value.sacSpd+value.numHoles+value.stackMean+value.stackVar+value.stackMax+value.stackMin, data=data)
    dev.off()
    
    
    png("NumHoles.byLoad.Boxplot.png",width=1280,height=1024)
    boxplot(value.numHoles~as.factor(Load),data=data,xlab="Load Index",ylab="number of holes")
    dev.off()
    
    png("StackMean.byLoad.Boxplot.png",width=1280,height=1024)
    boxplot(value.stackMean~as.factor(Load),data=data,xlab="Load Index",ylab="MEAN stack height")
    dev.off()
    
    png("StackVar.byLoad.Boxplot.png",width=1280,height=1024)
    boxplot(value.stackVar~as.factor(Load),data=data,xlab="Load Index",ylab="stack height VARIANCE")
    dev.off()
    
    png("StackMin.byLoad.Boxplot.png",width=1280,height=1024)
    boxplot(value.stackMin~as.factor(Load),data=data,xlab="Load Index",ylab="MIN stack height")
    dev.off()
    
    png("StackMax.byLoad.Boxplot.png",width=1280,height=1024)
    boxplot(value.stackMax~as.factor(Load),data=data,xlab="Load Index",ylab="MAX stack height")
    dev.off()
    
}






