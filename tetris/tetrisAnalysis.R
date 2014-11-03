require("ff")
require("ggplot2")
require("car")
require("gplots")

setwd("/home/lprisan/workspace/jdc-data-analysis/tetris")

source("rollingWindows.R")

loadRawTetrisData <- function(){
    #Reading the pupil data
    tetrisPupil <- read.csv.ffdf(file="ALLCombinedVariables_timeseriesPupilEvolution.csv",header=T,VERBOSE=T,first.rows=10000,next.rows=50000,colClasses=NA)
    
    tetrisPupil
}


preprocessTetrisData <- function(tetrisPupil){
    
    games = unique(tetrisPupil$gameID[])
    window = 10000
    slide = 5000
    
    # This is the dataframe that will contain our processed dataset
    totaldata <- data.frame()
    
    # We process all the eyetracking and game variables for each game
    for (game in games){
        
        gamepupil <- tetrisPupil[tetrisPupil$gameID[]==game,]
        
        # We get the game init time
        gameinit <- min(gamepupil$time.milliseconds.)
        
        # We filter out undesired columns, and the rows were there is no pupil measure (saccades)
        gamepupil <- gamepupil[gamepupil$Pupil.size!="saccade",-c(3,5,6,12:16)]
        gamepupil$Pupil.size <- as.numeric(as.character(gamepupil$Pupil.size))
        
        gamepupilMean <- rollingMean(gamepupil$time.milliseconds.,gamepupil$Pupil.size,window,slide)
        gamepupilSD <- rollingSd(gamepupil$time.milliseconds.,gamepupil$Pupil.size,window,slide)
        
        gamenumHoles <- rollingMean(gamepupil$time.milliseconds.,gamepupil$Number.Holes,window,slide)
        gamestackMin <- rollingMean(gamepupil$time.milliseconds.,gamepupil$Stack.height.min,window,slide)
        gamestackMax <- rollingMean(gamepupil$time.milliseconds.,gamepupil$Stack.height.max,window,slide)
        gamestackMean <- rollingMean(gamepupil$time.milliseconds.,gamepupil$Stack.height.mean,window,slide)
        gamestackVar <- rollingMean(gamepupil$time.milliseconds.,gamepupil$Stack.height.variance,window,slide)
        
        # We now try to get the fixation (and indirectly, saccade) information for the game
        gamefix <- read.csv(file=paste("./FixationDetails/ATESTvariables_",games[1],".csv",sep=""), header=T)
        gamefix$fixtime <- gamefix$start + (gamefix$end - gamefix$start)/2 # We make the Fixation time equal to the middle point of the fixation
        gamefix$duration <- gamefix$end - gamefix$start
        gamefixLong <- rollingLong(gamefix$fixtime,gamefix$duration,window,slide,inittime=gameinit)
        
        # We derive the approximate saccade measures from the fixations (the end of a fixation is the start of a saccade, and vice-versa)
        gamesac <- data.frame(start=gamefix$end[1:(length(gamefix$end)-1)])
        gamesac$end <- gamefix$start[2:(length(gamefix$end))]
        gamesac$duration <- gamesac$end - gamesac$start
        gamesac$sactime <- gamesac$start + gamesac$duration/2 # We make the Saccade time equal to the middle point of the saccade
        gamesac$amplitude <- numeric(nrow(gamesac)) # This field will contain the amplitude of the saccade, measured in game cells
        for(i in 1:nrow(gamesac)){
            gamesac$amplitude[i] <- sqrt((gamefix$row[i+1]-gamefix$row[i])^2+(gamefix$col[i+1]-gamefix$col[i])^2)
        }
        gamesac$speed <- gamesac$amplitude / gamesac$duration
        gamesacSpd <- rollingMean(gamesac$sactime,gamesac$speed,window,slide,inittime=gameinit)
        
        # We merge all this game's data
        data <- merge(gamepupilMean,gamepupilSD,by="time",suffixes=c(".pupilMean",".pupilSD"))
        data <- merge(data,gamefixLong,by="time")
        names(data)[length(data)] <- "value.longFix"
        data <- merge(data,gamesacSpd,by="time")
        names(data)[length(data)] <- "value.sacSpd"
        data <- merge(data,gamenumHoles,by="time")
        names(data)[length(data)] <- "value.numHoles"
        data <- merge(data,gamestackMin,by="time")
        names(data)[length(data)] <- "value.stackMin"
        data <- merge(data,gamestackMax,by="time")
        names(data)[length(data)] <- "value.stackMax"
        data <- merge(data,gamestackMean,by="time")
        names(data)[length(data)] <- "value.stackMean"
        data <- merge(data,gamestackVar,by="time")
        names(data)[length(data)] <- "value.stackVar"
        
        # We calculate the load index
        meansessionmed <- median(data$value.pupilMean)
        data$Above.Mean <- as.numeric(data$value.pupilMean > meansessionmed)
        sdsessionmed <- median(data$value.pupilSD)
        data$Above.SD <- as.numeric(data$value.pupilSD > sdsessionmed)
        longsessionmed <- median(data$value.longFix)
        data$Above.Fix <- as.numeric(data$value.longFix > longsessionmed)
        sacsessionmed <- median(data$value.sacSpd)
        data$Above.Sac <- as.numeric(data$value.sacSpd > sacsessionmed)
        data$Load <- data$Above.Mean + data$Above.SD + data$Above.Fix + data$Above.Sac
        
        data$gameID <- rep(game,nrow(data))
        
        save(data,file=paste("TetrisProcessedData_game",game,".Rda",sep=""))
        
        
        # We join the game data to our global dataset
        if(length(totaldata)==0) totaldata <- data
        else totaldata <- rbind(totaldata,data)
        
        
        
    }
    
    save(totaldata,file="TetrisProcessedData.Rda")
    
    
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

morePlots <- function(data){
  
  dfc <- summarySE(data,measurevar="value.stackMean",groupvar="Load",na.rm=T)
  
  png("StackMean.byLoad.Errorbars.png",width=600,height=600)
  p <- ggplot(dfc, aes(x=Load, y=value.stackMean)) + 
    geom_errorbar(aes(ymin=value.stackMean-se, ymax=value.stackMean+se), width=.1) +
    geom_line() +
    geom_point()
  print(p)
  dev.off()
  
  dfc <- summarySE(data,measurevar="value.stackVar",groupvar="Load",na.rm=T)
  
  png("StackVar.byLoad.Errorbars.png",width=600,height=600)
  p <- ggplot(dfc, aes(x=Load, y=value.stackVar)) + 
    geom_errorbar(aes(ymin=value.stackVar-se, ymax=value.stackVar+se), width=.1) +
    geom_line() +
    geom_point()
  print(p)
  dev.off()
  
  
  png("Load.byTime.Errorbars.png",width=600,height=600)
  p <- ggplot(data, aes(x=time, y=Load)) + geom_smooth()
  print(p)
  dev.off()
  
}


## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}




