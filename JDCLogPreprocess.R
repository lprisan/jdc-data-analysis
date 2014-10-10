require(jsonlite)
# require(data.table)
require(plyr)

displayWidth <- 1280
displayHeight <- 768



# preprocessJDCLogs - Clean and organize
# This is the overall function in charge of the preprocessing and cleaning of the log data from the JDC experiment
# Parameters: rootDir the directory in which the logs to be merged/split are, in folders called "lamp 1", "lamp 2"...
preprocessJDCLogs <- function(rootDir,doYAMLConversion=FALSE){
  
  cat ("Please ensure that all .log files have ending parentheses so that they are valid pseudo-yaml, and save them as .yaml files. Then press [enter] to continue")
  line <- readline()
  
  # Convert the .yaml files to JSON .json files
  if(doYAMLConversion){
    convertLogsToJson(paste(rootDir, "lamp 1", sep="/"))
    convertLogsToJson(paste(rootDir, "lamp 2", sep="/"))
    convertLogsToJson(paste(rootDir, "lamp 3", sep="/"))
    convertLogsToJson(paste(rootDir, "lamp 4", sep="/"))
    convertLogsToJson(paste(rootDir, "lamp 5", sep="/"))
  }
  
  
  # We import the groups, start and end times from a csv in the rootDir
  syncData <- read.csv(paste(rootDir,"ActivityTiming-synchronization.csv", sep="/"), stringsAsFactors=FALSE)
  
  options(digits.secs = 3)
  
  # for each group, we get and clean the data
  for(i in 1:length(syncData$Code)){
    
    # We calculate the time limits (for now, based on the expected time slots)
    start <- as.POSIXct(strptime(syncData$RealStart[i], "%Y-%m-%d %H:%M:%OS"))
    end <- as.POSIXct(strptime(syncData$RealEnd[i], "%Y-%m-%d %H:%M:%OS"))
    mergeSplitLogFiles(paste(rootDir, paste("lamp ",as.character(syncData$Lamp..[i]),sep=""), sep="/"),start,end,syncData$Code[i])
  }
  
  #We merge manually the two composite group data, and write it to a file again
  cat ("Doing manual join of groups fragmented across lamps\n")
  s2g3a <- get(load('S2G3a.rda')) 
  s2g3b <- get(load('S2G3b.rda'))
  totalData <- rbind(s2g3a,s2g3b) 
  save(totalData,file="S2G3.rda",compress=TRUE)
  
  # s2g4a <- get(load('S2G4a.rda')) # From this part, we lost the logs!
  totalData <- get(load('S2G4b.rda'))
  # totalData <- rbind(s2g4a,s2g4b) 
  save(totalData,file="S2G4.rda",compress=TRUE)
  
  cat ("Process finished! Check your .rda files")
  
}


# preprocessISLLogs - Clean and organize log files from the Fall 2014 experiments at ISL (similar to the previous one, but different structure)
# This is the overall function in charge of the preprocessing and cleaning of the log data from the JDC experiment
# Parameters: rootDir the directory in which the logs to be merged/split are
preprocessISLLogs <- function(rootDir,doYAMLConversion=FALSE){
    cat ("Please ensure that all .log files have ending parentheses so that they are valid pseudo-yaml, 
         and save them as .yaml files. Then press [enter] to continue")
    line <- readline()
    
    # Convert the .yaml files to JSON .json files
    if(doYAMLConversion){
        convertLogsToJson(rootDir)
    }
    
    # Read and clean the data, and write a test.rda file
    start <- as.POSIXct(strptime("2014-10-10 18:00:00", "%Y-%m-%d %H:%M:%OS"))
    end <- as.POSIXct(strptime("2014-10-10 23:00:00", "%Y-%m-%d %H:%M:%OS"))
    mergeSplitLogFiles2(rootDir,start,end,"test")
    
    cat ("Process finished! Check your .rda files")
    
}








convertLogsToJson <- function(rootDir){
  
  # Store the current dir, to come back to it at the end, and change wd to the rootDir
  originalDir <- getwd()
  setwd(rootDir)
  
  cat(paste("Starting pseudo-YAML to JSON conversion of directory",rootDir))
  
  logFiles <- list.files(pattern = "\\.yaml$")
  
  for(logFile in logFiles){
    cat(paste("Converting ",logFile,"..."))
    convertLogToJson(logFile)
  }
  
  cat("Conversion to json finished!")
  
  # Go back to the original current dir
  setwd(originalDir)
}


# convertLogToJson - convert pseudo-yaml log file to JSON for later parsing
convertLogToJson <- function(inputFile){
  
  con  <- file(inputFile, open = "r")
  
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    
    # We substitute = by :, ; by ,, () by []
    newstr <- gsub("=",":", x=oneLine)
    newstr <- gsub(";",",", x=newstr)
    newstr <- gsub("\\(","[", x=newstr)
    newstr <- gsub(")","]", x=newstr)
    newstr <- gsub("([0-9]*)(L)","\\1",x=newstr)
    newstr <- gsub("taglog : ","", x=newstr)
    newstr <- gsub("timestamp","\"timestamp\"", x=newstr)
    newstr <- gsub("tags","\"tags\"", x=newstr)
    newstr <- gsub("id","\"id\"", x=newstr)
    newstr <- gsub("corners","\"corners\"", x=newstr)
    newstr <- gsub("game","\"game\"", x=newstr)
    
    # HintType=-1; Map=4; Proportion1=0.5; Proportion2=0.5; Proportion3=0.6; Proportion4=0.2; 
    # BugPositionX=5; BugPositionY=7; Steps=0; StepsToGo=5; GameStarted=1; MapFinished=0; MapNew=0; 
    # Proportion1Num=5; Proportion2Num=1; Proportion3Num=3; Proportion4Num=2; Proportion1Den=10; 
    # Proportion2Den=2; Proportion3Den=5; Proportion4Den=10; WrongMove=0; P1Greater=0; P2Greater=0; P3Greater=0; P4Greater=0
    newstr <- gsub("HintType:","\"HintType\":", x=newstr)
    newstr <- gsub("GameStarted:","\"GameStarted\":", x=newstr)
    newstr <- gsub("MapFinished:","\"MapFinished\":", x=newstr)
    newstr <- gsub("MapNew:","\"MapNew\":", x=newstr)
    newstr <- gsub("WrongMove:","\"WrongMove\":", x=newstr)
    newstr <- gsub("P1Greater:","\"P1Greater\":", x=newstr)
    newstr <- gsub("P2Greater:","\"P2Greater\":", x=newstr)
    newstr <- gsub("P3Greater:","\"P3Greater\":", x=newstr)
    newstr <- gsub("P4Greater:","\"P4Greater\":", x=newstr)
    newstr <- gsub("BugPositionX:","\"BugPositionX\":", x=newstr)
    newstr <- gsub("BugPositionY:","\"BugPositionY\":", x=newstr)
    newstr <- gsub("StepsToGo:","\"StepsToGo\":", x=newstr)
    newstr <- gsub("Steps:","\"Steps\":", x=newstr)
    newstr <- gsub("Proportion1Num:","\"Proportion1Num\":", x=newstr)
    newstr <- gsub("Proportion2Num:","\"Proportion2Num\":", x=newstr)
    newstr <- gsub("Proportion3Num:","\"Proportion3Num\":", x=newstr)
    newstr <- gsub("Proportion4Num:","\"Proportion4Num\":", x=newstr)
    newstr <- gsub("Proportion1Den:","\"Proportion1Den\":", x=newstr)
    newstr <- gsub("Proportion2Den:","\"Proportion2Den\":", x=newstr)
    newstr <- gsub("Proportion3Den:","\"Proportion3Den\":", x=newstr)
    newstr <- gsub("Proportion4Den:","\"Proportion4Den\":", x=newstr)
    newstr <- gsub("Proportion1:","\"Proportion1\":", x=newstr)
    newstr <- gsub("Proportion2:","\"Proportion2\":", x=newstr)
    newstr <- gsub("Proportion3:","\"Proportion3\":", x=newstr)
    newstr <- gsub("Proportion4:","\"Proportion4\":", x=newstr)
    newstr <- gsub("Map:","\"Map\":", x=newstr)
    
    newstr <- gsub("([0-9])(, })","\\1}", x=newstr)
    
    newstr <- gsub("],","]",x=newstr)
    
    
    
    
    
    # We write a new line to the
    cat(newstr,"\n",file=paste(inputFile,"json",sep="."), sep="", append=TRUE)
    
  } 
  
  close(con)
  
}



# mergeSplitLogFiles - Merge/Split log files
# This function takes log files in JSON, loads them in R data frames, takes those times within a certain limit, and splits the tags 
# present into one observation each, including the x/y position of the center of the tag
# Parameters: directory the directory in which the logs to be merged/split are, startTime and endTime are the limits for this
# group's activities, and label is the name of the group, which will be used when exporting the file into binary form
mergeSplitLogFiles <- function(directory, startTime, endTime, label){

  cat (paste("Processing for group ",label,"...\n"))
       
  # We check that the time frontiers - beginning is earlier than end
  if(endTime<startTime) stop("Incorrect start/end times")
  
  # Store the current dir, to come back to it at the end, and change wd to the rootDir
  originalDir <- getwd()
  setwd(directory)
  
  # We loop through all the .log files in the directory
  logFiles <- list.files(pattern = "\\.json$")
  
  totalData <- data.frame(timestamp=numeric(),stringsAsFactors=TRUE)
  
  for(file in logFiles){
    cat (paste("Processing file... ",file,"\n"))
    
    
    # We import the JSON file
    jsonData <- fromJSON(file)
    
    # We turn the start/end times to milliseconds and compare with the timestamp of each entry
    startTimestamp <- as.numeric(startTime)*1000
    endTimestamp <- as.numeric(endTime)*1000
    
    # We get which entries are valid (within the time limits)
    validEntries <- jsonData[(jsonData$timestamp >= startTimestamp & jsonData$timestamp <= endTimestamp),]
    
    # if there are no valid entries, we go to the next logfile
    if(length(validEntries$timestamp)==0) next
    
    # we create the target dataframe, or append to it if it already has data
    newData <- data.frame(timestamp = validEntries$timestamp)
    
    # We iterate throughout the log entries
    for(i in 1:length(newData$timestamp)){
      
      # We get the list of tags detected
      tags <- validEntries$tags[[i]]
      
      #We get the position (x,y coordinates) of each tag group, or NA if not present
      newData$Position.C1x[[i]] <- (getPositionTagGroup(tags,316:320))[[1]] # Continuous Circular 1 set
      newData$Position.C2x[[i]] <- (getPositionTagGroup(tags,321:325))[[1]] # Continuous Circular 2 set
      newData$Position.R1x[[i]] <- (getPositionTagGroup(tags,326:330))[[1]] # Continuous Rectangular 1 set
      newData$Position.R2x[[i]] <- (getPositionTagGroup(tags,331:335))[[1]] # Continuous Rectangular 2 set
      newData$Position.C1y[[i]] <- (getPositionTagGroup(tags,316:320))[[2]] # Continuous Circular 1 set
      newData$Position.C2y[[i]] <- (getPositionTagGroup(tags,321:325))[[2]] # Continuous Circular 2 set
      newData$Position.R1y[[i]] <- (getPositionTagGroup(tags,326:330))[[2]] # Continuous Rectangular 1 set
      newData$Position.R2y[[i]] <- (getPositionTagGroup(tags,331:335))[[2]] # Continuous Rectangular 2 set
      
      newData$Position.Token1x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,1))[[1]] # Tokens in Quadrant 1
      newData$Position.Token2x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,2))[[1]] # Tokens in Quadrant 2
      newData$Position.Token3x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,3))[[1]] # Tokens in Quadrant 3
      newData$Position.Token4x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,4))[[1]] # Tokens in Quadrant 4
      newData$Position.Token1y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,1))[[2]] # Tokens in Quadrant 1
      newData$Position.Token2y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,2))[[2]] # Tokens in Quadrant 2
      newData$Position.Token3y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,3))[[2]] # Tokens in Quadrant 3
      newData$Position.Token4y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,4))[[2]] # Tokens in Quadrant 4
      
      newData$Position.Fraction12x[[i]] <- (getPositionTagGroup(tags,345))[[1]] # Fraction Card 1/2
      newData$Position.Fraction13x[[i]] <- (getPositionTagGroup(tags,346))[[1]] # Fraction Card 1/3
      newData$Position.Fraction23x[[i]] <- (getPositionTagGroup(tags,347))[[1]] # Fraction Card 2/3
      newData$Position.Fraction14x[[i]] <- (getPositionTagGroup(tags,348))[[1]] # Fraction Card 1/4
      newData$Position.Fraction24x[[i]] <- (getPositionTagGroup(tags,349))[[1]] # Fraction Card 2/4
      newData$Position.Fraction34x[[i]] <- (getPositionTagGroup(tags,350))[[1]] # Fraction Card 3/4
      newData$Position.Fraction15x[[i]] <- (getPositionTagGroup(tags,351))[[1]] # Fraction Card 1/5
      newData$Position.Fraction25x[[i]] <- (getPositionTagGroup(tags,352))[[1]] # Fraction Card 2/5
      newData$Position.Fraction35x[[i]] <- (getPositionTagGroup(tags,353))[[1]] # Fraction Card 3/5
      newData$Position.Fraction45x[[i]] <- (getPositionTagGroup(tags,354))[[1]] # Fraction Card 4/5
      newData$Position.Fraction16x[[i]] <- (getPositionTagGroup(tags,355))[[1]] # Fraction Card 1/6
      newData$Position.Fraction26x[[i]] <- (getPositionTagGroup(tags,356))[[1]] # Fraction Card 2/6
      newData$Position.Fraction36x[[i]] <- (getPositionTagGroup(tags,357))[[1]] # Fraction Card 3/6
      newData$Position.Fraction46x[[i]] <- (getPositionTagGroup(tags,358))[[1]] # Fraction Card 4/6
      newData$Position.Fraction56x[[i]] <- (getPositionTagGroup(tags,359))[[1]] # Fraction Card 5/6
      newData$Position.Fraction110x[[i]] <- (getPositionTagGroup(tags,360))[[1]] # Fraction Card 1/10
      newData$Position.Fraction210x[[i]] <- (getPositionTagGroup(tags,361))[[1]] # Fraction Card 2/10
      newData$Position.Fraction310x[[i]] <- (getPositionTagGroup(tags,362))[[1]] # Fraction Card 3/10
      newData$Position.Fraction410x[[i]] <- (getPositionTagGroup(tags,363))[[1]] # Fraction Card 4/10
      newData$Position.Fraction510x[[i]] <- (getPositionTagGroup(tags,364))[[1]] # Fraction Card 5/10
      newData$Position.Fraction610x[[i]] <- (getPositionTagGroup(tags,365))[[1]] # Fraction Card 6/10
      newData$Position.Fraction710x[[i]] <- (getPositionTagGroup(tags,366))[[1]] # Fraction Card 7/10
      newData$Position.Fraction810x[[i]] <- (getPositionTagGroup(tags,367))[[1]] # Fraction Card 8/10
      newData$Position.Fraction910x[[i]] <- (getPositionTagGroup(tags,368))[[1]] # Fraction Card 9/10
      newData$Position.Fraction12y[[i]] <- (getPositionTagGroup(tags,345))[[2]] # Fraction Card 1/2
      newData$Position.Fraction13y[[i]] <- (getPositionTagGroup(tags,346))[[2]] # Fraction Card 1/3
      newData$Position.Fraction23y[[i]] <- (getPositionTagGroup(tags,347))[[2]] # Fraction Card 2/3
      newData$Position.Fraction14y[[i]] <- (getPositionTagGroup(tags,348))[[2]] # Fraction Card 1/4
      newData$Position.Fraction24y[[i]] <- (getPositionTagGroup(tags,349))[[2]] # Fraction Card 2/4
      newData$Position.Fraction34y[[i]] <- (getPositionTagGroup(tags,350))[[2]] # Fraction Card 3/4
      newData$Position.Fraction15y[[i]] <- (getPositionTagGroup(tags,351))[[2]] # Fraction Card 1/5
      newData$Position.Fraction25y[[i]] <- (getPositionTagGroup(tags,352))[[2]] # Fraction Card 2/5
      newData$Position.Fraction35y[[i]] <- (getPositionTagGroup(tags,353))[[2]] # Fraction Card 3/5
      newData$Position.Fraction45y[[i]] <- (getPositionTagGroup(tags,354))[[2]] # Fraction Card 4/5
      newData$Position.Fraction16y[[i]] <- (getPositionTagGroup(tags,355))[[2]] # Fraction Card 1/6
      newData$Position.Fraction26y[[i]] <- (getPositionTagGroup(tags,356))[[2]] # Fraction Card 2/6
      newData$Position.Fraction36y[[i]] <- (getPositionTagGroup(tags,357))[[2]] # Fraction Card 3/6
      newData$Position.Fraction46y[[i]] <- (getPositionTagGroup(tags,358))[[2]] # Fraction Card 4/6
      newData$Position.Fraction56y[[i]] <- (getPositionTagGroup(tags,359))[[2]] # Fraction Card 5/6
      newData$Position.Fraction110y[[i]] <- (getPositionTagGroup(tags,360))[[2]] # Fraction Card 1/10
      newData$Position.Fraction210y[[i]] <- (getPositionTagGroup(tags,361))[[2]] # Fraction Card 2/10
      newData$Position.Fraction310y[[i]] <- (getPositionTagGroup(tags,362))[[2]] # Fraction Card 3/10
      newData$Position.Fraction410y[[i]] <- (getPositionTagGroup(tags,363))[[2]] # Fraction Card 4/10
      newData$Position.Fraction510y[[i]] <- (getPositionTagGroup(tags,364))[[2]] # Fraction Card 5/10
      newData$Position.Fraction610y[[i]] <- (getPositionTagGroup(tags,365))[[2]] # Fraction Card 6/10
      newData$Position.Fraction710y[[i]] <- (getPositionTagGroup(tags,366))[[2]] # Fraction Card 7/10
      newData$Position.Fraction810y[[i]] <- (getPositionTagGroup(tags,367))[[2]] # Fraction Card 8/10
      newData$Position.Fraction910y[[i]] <- (getPositionTagGroup(tags,368))[[2]] # Fraction Card 9/10
      
      newData$Position.Integer1Rx[[i]] <- (getPositionTagGroup(tags,300))[[1]] # Integer Card 1R
      newData$Position.Integer2Rx[[i]] <- (getPositionTagGroup(tags,301))[[1]] # Integer Card 2R
      newData$Position.Integer3Rx[[i]] <- (getPositionTagGroup(tags,302))[[1]] # Integer Card 3R
      newData$Position.Integer4Rx[[i]] <- (getPositionTagGroup(tags,303))[[1]] # Integer Card 4R
      newData$Position.Integer1Gx[[i]] <- (getPositionTagGroup(tags,304))[[1]] # Integer Card 1G
      newData$Position.Integer2Gx[[i]] <- (getPositionTagGroup(tags,305))[[1]] # Integer Card 2G
      newData$Position.Integer3Gx[[i]] <- (getPositionTagGroup(tags,306))[[1]] # Integer Card 3G
      newData$Position.Integer4Gx[[i]] <- (getPositionTagGroup(tags,307))[[1]] # Integer Card 4G
      newData$Position.Integer1Bx[[i]] <- (getPositionTagGroup(tags,308))[[1]] # Integer Card 1B
      newData$Position.Integer2Bx[[i]] <- (getPositionTagGroup(tags,309))[[1]] # Integer Card 2B
      newData$Position.Integer3Bx[[i]] <- (getPositionTagGroup(tags,310))[[1]] # Integer Card 3B
      newData$Position.Integer4Bx[[i]] <- (getPositionTagGroup(tags,311))[[1]] # Integer Card 4B
      newData$Position.Integer1Px[[i]] <- (getPositionTagGroup(tags,312))[[1]] # Integer Card 1P
      newData$Position.Integer2Px[[i]] <- (getPositionTagGroup(tags,313))[[1]] # Integer Card 2P
      newData$Position.Integer3Px[[i]] <- (getPositionTagGroup(tags,314))[[1]] # Integer Card 3P
      newData$Position.Integer4Px[[i]] <- (getPositionTagGroup(tags,315))[[1]] # Integer Card 4P
      newData$Position.Integer1Ry[[i]] <- (getPositionTagGroup(tags,300))[[2]] # Integer Card 1R
      newData$Position.Integer2Ry[[i]] <- (getPositionTagGroup(tags,301))[[2]] # Integer Card 2R
      newData$Position.Integer3Ry[[i]] <- (getPositionTagGroup(tags,302))[[2]] # Integer Card 3R
      newData$Position.Integer4Ry[[i]] <- (getPositionTagGroup(tags,303))[[2]] # Integer Card 4R
      newData$Position.Integer1Gy[[i]] <- (getPositionTagGroup(tags,304))[[2]] # Integer Card 1G
      newData$Position.Integer2Gy[[i]] <- (getPositionTagGroup(tags,305))[[2]] # Integer Card 2G
      newData$Position.Integer3Gy[[i]] <- (getPositionTagGroup(tags,306))[[2]] # Integer Card 3G
      newData$Position.Integer4Gy[[i]] <- (getPositionTagGroup(tags,307))[[2]] # Integer Card 4G
      newData$Position.Integer1By[[i]] <- (getPositionTagGroup(tags,308))[[2]] # Integer Card 1B
      newData$Position.Integer2By[[i]] <- (getPositionTagGroup(tags,309))[[2]] # Integer Card 2B
      newData$Position.Integer3By[[i]] <- (getPositionTagGroup(tags,310))[[2]] # Integer Card 3B
      newData$Position.Integer4By[[i]] <- (getPositionTagGroup(tags,311))[[2]] # Integer Card 4B
      newData$Position.Integer1Py[[i]] <- (getPositionTagGroup(tags,312))[[2]] # Integer Card 1P
      newData$Position.Integer2Py[[i]] <- (getPositionTagGroup(tags,313))[[2]] # Integer Card 2P
      newData$Position.Integer3Py[[i]] <- (getPositionTagGroup(tags,314))[[2]] # Integer Card 3P
      newData$Position.Integer4Py[[i]] <- (getPositionTagGroup(tags,315))[[2]] # Integer Card 4P
      
      newData$Position.Gox[[i]] <- (getPositionTagGroup(tags,341:344))[[1]] # Go! Card
      newData$Position.Goy[[i]] <- (getPositionTagGroup(tags,341:344))[[2]] # Go! Card
      
      newData$Position.DiscreteHintx[[i]] <- (getPositionTagGroup(tags,336))[[1]] # Discrete hint card
      newData$Position.FractionHintx[[i]] <- (getPositionTagGroup(tags,337))[[1]] # Fraction hint card
      newData$Position.CircularHintx[[i]] <- (getPositionTagGroup(tags,340))[[1]] # Circular hint card
      newData$Position.RectangularHintx[[i]] <- (getPositionTagGroup(tags,339))[[1]] # Rectangular hint card
      newData$Position.DecimalHintx[[i]] <- (getPositionTagGroup(tags,338))[[1]] # Decimal hint card
      newData$Position.DiscreteHinty[[i]] <- (getPositionTagGroup(tags,336))[[2]] # Discrete hint card
      newData$Position.FractionHinty[[i]] <- (getPositionTagGroup(tags,337))[[2]] # Fraction hint card
      newData$Position.CircularHinty[[i]] <- (getPositionTagGroup(tags,340))[[2]] # Circular hint card
      newData$Position.RectangularHinty[[i]] <- (getPositionTagGroup(tags,339))[[2]] # Rectangular hint card
      newData$Position.DecimalHinty[[i]] <- (getPositionTagGroup(tags,338))[[2]] # Decimal hint card
      
      newData$Position.Carte1x[[i]] <- (getPositionTagGroup(tags,409))[[1]] # Carte 1 card
      newData$Position.Carte2x[[i]] <- (getPositionTagGroup(tags,410))[[1]] # Carte 2 card
      newData$Position.Carte3x[[i]] <- (getPositionTagGroup(tags,411))[[1]] # Carte 3 card
      newData$Position.Carte4x[[i]] <- (getPositionTagGroup(tags,412))[[1]] # Carte 4 card
      newData$Position.Carte5x[[i]] <- (getPositionTagGroup(tags,413))[[1]] # Carte 5 card
      newData$Position.Carte6x[[i]] <- (getPositionTagGroup(tags,414))[[1]] # Carte 6 card
      newData$Position.Carte7x[[i]] <- (getPositionTagGroup(tags,415))[[1]] # Carte 7 card
      newData$Position.Carte8x[[i]] <- (getPositionTagGroup(tags,416))[[1]] # Carte 8 card
      newData$Position.Carte9x[[i]] <- (getPositionTagGroup(tags,417))[[1]] # Carte 9 card
      newData$Position.Carte10x[[i]] <- (getPositionTagGroup(tags,418))[[1]] # Carte 10 card
      newData$Position.Carte1y[[i]] <- (getPositionTagGroup(tags,409))[[2]] # Carte 1 card
      newData$Position.Carte2y[[i]] <- (getPositionTagGroup(tags,410))[[2]] # Carte 2 card
      newData$Position.Carte3y[[i]] <- (getPositionTagGroup(tags,411))[[2]] # Carte 3 card
      newData$Position.Carte4y[[i]] <- (getPositionTagGroup(tags,412))[[2]] # Carte 4 card
      newData$Position.Carte5y[[i]] <- (getPositionTagGroup(tags,413))[[2]] # Carte 5 card
      newData$Position.Carte6y[[i]] <- (getPositionTagGroup(tags,414))[[2]] # Carte 6 card
      newData$Position.Carte7y[[i]] <- (getPositionTagGroup(tags,415))[[2]] # Carte 7 card
      newData$Position.Carte8y[[i]] <- (getPositionTagGroup(tags,416))[[2]] # Carte 8 card
      newData$Position.Carte9y[[i]] <- (getPositionTagGroup(tags,417))[[2]] # Carte 9 card
      newData$Position.Carte10y[[i]] <- (getPositionTagGroup(tags,418))[[2]] # Carte 10 card
      
      #We get the rotation of each tag group (except the tokens), as a value in radians or NA if not present
      newData$Rotation.C1[[i]] <- getRotationTagGroup(tags,316:319) # Continuous Circular 1 set, not including the central tag
      newData$Rotation.C2[[i]] <- getRotationTagGroup(tags,321:324) # Continuous Circular 2 set, not including the central tag
      newData$Rotation.R1[[i]] <- getRotationTagGroup(tags,326:329) # Continuous Rectangular 1 set, not including the central tag
      newData$Rotation.R2[[i]] <- getRotationTagGroup(tags,331:334) # Continuous Rectangular 2 set, not including the central tag
      
      newData$Rotation.Fraction12[[i]] <- getRotationTagGroup(tags,345) # Fraction Card 1/2
      newData$Rotation.Fraction13[[i]] <- getRotationTagGroup(tags,346) # Fraction Card 1/3
      newData$Rotation.Fraction23[[i]] <- getRotationTagGroup(tags,347) # Fraction Card 2/3
      newData$Rotation.Fraction14[[i]] <- getRotationTagGroup(tags,348) # Fraction Card 1/4
      newData$Rotation.Fraction24[[i]] <- getRotationTagGroup(tags,349) # Fraction Card 2/4
      newData$Rotation.Fraction34[[i]] <- getRotationTagGroup(tags,350) # Fraction Card 3/4
      newData$Rotation.Fraction15[[i]] <- getRotationTagGroup(tags,351) # Fraction Card 1/5
      newData$Rotation.Fraction25[[i]] <- getRotationTagGroup(tags,352) # Fraction Card 2/5
      newData$Rotation.Fraction35[[i]] <- getRotationTagGroup(tags,353) # Fraction Card 3/5
      newData$Rotation.Fraction45[[i]] <- getRotationTagGroup(tags,354) # Fraction Card 4/5
      newData$Rotation.Fraction16[[i]] <- getRotationTagGroup(tags,355) # Fraction Card 1/6
      newData$Rotation.Fraction26[[i]] <- getRotationTagGroup(tags,356) # Fraction Card 2/6
      newData$Rotation.Fraction36[[i]] <- getRotationTagGroup(tags,357) # Fraction Card 3/6
      newData$Rotation.Fraction46[[i]] <- getRotationTagGroup(tags,358) # Fraction Card 4/6
      newData$Rotation.Fraction56[[i]] <- getRotationTagGroup(tags,359) # Fraction Card 5/6
      newData$Rotation.Fraction110[[i]] <- getRotationTagGroup(tags,360) # Fraction Card 1/10
      newData$Rotation.Fraction210[[i]] <- getRotationTagGroup(tags,361) # Fraction Card 2/10
      newData$Rotation.Fraction310[[i]] <- getRotationTagGroup(tags,362) # Fraction Card 3/10
      newData$Rotation.Fraction410[[i]] <- getRotationTagGroup(tags,363) # Fraction Card 4/10
      newData$Rotation.Fraction510[[i]] <- getRotationTagGroup(tags,364) # Fraction Card 5/10
      newData$Rotation.Fraction610[[i]] <- getRotationTagGroup(tags,365) # Fraction Card 6/10
      newData$Rotation.Fraction710[[i]] <- getRotationTagGroup(tags,366) # Fraction Card 7/10
      newData$Rotation.Fraction810[[i]] <- getRotationTagGroup(tags,367) # Fraction Card 8/10
      newData$Rotation.Fraction910[[i]] <- getRotationTagGroup(tags,368) # Fraction Card 9/10
      
      newData$Rotation.Integer1R[[i]] <- getRotationTagGroup(tags,300) # Integer Card 1R
      newData$Rotation.Integer2R[[i]] <- getRotationTagGroup(tags,301) # Integer Card 2R
      newData$Rotation.Integer3R[[i]] <- getRotationTagGroup(tags,302) # Integer Card 3R
      newData$Rotation.Integer4R[[i]] <- getRotationTagGroup(tags,303) # Integer Card 4R
      newData$Rotation.Integer1G[[i]] <- getRotationTagGroup(tags,304) # Integer Card 1G
      newData$Rotation.Integer2G[[i]] <- getRotationTagGroup(tags,305) # Integer Card 2G
      newData$Rotation.Integer3G[[i]] <- getRotationTagGroup(tags,306) # Integer Card 3G
      newData$Rotation.Integer4G[[i]] <- getRotationTagGroup(tags,307) # Integer Card 4G
      newData$Rotation.Integer1B[[i]] <- getRotationTagGroup(tags,308) # Integer Card 1B
      newData$Rotation.Integer2B[[i]] <- getRotationTagGroup(tags,309) # Integer Card 2B
      newData$Rotation.Integer3B[[i]] <- getRotationTagGroup(tags,310) # Integer Card 3B
      newData$Rotation.Integer4B[[i]] <- getRotationTagGroup(tags,311) # Integer Card 4B
      newData$Rotation.Integer1P[[i]] <- getRotationTagGroup(tags,312) # Integer Card 1P
      newData$Rotation.Integer2P[[i]] <- getRotationTagGroup(tags,313) # Integer Card 2P
      newData$Rotation.Integer3P[[i]] <- getRotationTagGroup(tags,314) # Integer Card 3P
      newData$Rotation.Integer4P[[i]] <- getRotationTagGroup(tags,315) # Integer Card 4P
      
      newData$Rotation.Go[[i]] <- getRotationTagGroup(tags,341:344) # Go! Card
      
      newData$Rotation.DiscreteHint[[i]] <- getRotationTagGroup(tags,336) # Discrete hint card
      newData$Rotation.FractionHint[[i]] <- getRotationTagGroup(tags,337) # Fraction hint card
      newData$Rotation.CircularHint[[i]] <- getRotationTagGroup(tags,340) # Circular hint card
      newData$Rotation.RectangularHint[[i]] <- getRotationTagGroup(tags,339) # Rectangular hint card
      newData$Rotation.DecimalHint[[i]] <- getRotationTagGroup(tags,338) # Decimal hint card
      
      newData$Rotation.Carte1[[i]] <- getRotationTagGroup(tags,409) # Carte 1 card
      newData$Rotation.Carte2[[i]] <- getRotationTagGroup(tags,410) # Carte 2 card
      newData$Rotation.Carte3[[i]] <- getRotationTagGroup(tags,411) # Carte 3 card
      newData$Rotation.Carte4[[i]] <- getRotationTagGroup(tags,412) # Carte 4 card
      newData$Rotation.Carte5[[i]] <- getRotationTagGroup(tags,413) # Carte 5 card
      newData$Rotation.Carte6[[i]] <- getRotationTagGroup(tags,414) # Carte 6 card
      newData$Rotation.Carte7[[i]] <- getRotationTagGroup(tags,415) # Carte 7 card
      newData$Rotation.Carte8[[i]] <- getRotationTagGroup(tags,416) # Carte 8 card
      newData$Rotation.Carte9[[i]] <- getRotationTagGroup(tags,417) # Carte 9 card
      newData$Rotation.Carte10[[i]] <- getRotationTagGroup(tags,418) # Carte 10 card
      
      # For token and abstract fractions, we get the numerator and denominator, or NA if not present
      newData$Num.Token1[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,1) # Tokens in Quadrant 1
      newData$Num.Token2[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,2) # Tokens in Quadrant 2
      newData$Num.Token3[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,3) # Tokens in Quadrant 3
      newData$Num.Token4[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,4) # Tokens in Quadrant 4
      newData$Den.Token1[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,1) # Tokens in Quadrant 1
      newData$Den.Token2[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,2) # Tokens in Quadrant 2
      newData$Den.Token3[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,3) # Tokens in Quadrant 3
      newData$Den.Token4[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,4) # Tokens in Quadrant 4
      
      newData$Num.Fraction12[[i]] <- getNumFraction(tags,345,1,2) # Fraction Card 1/2
      newData$Num.Fraction13[[i]] <- getNumFraction(tags,346,1,3) # Fraction Card 1/3
      newData$Num.Fraction23[[i]] <- getNumFraction(tags,347,2,3) # Fraction Card 2/3
      newData$Num.Fraction14[[i]] <- getNumFraction(tags,348,1,4) # Fraction Card 1/4
      newData$Num.Fraction24[[i]] <- getNumFraction(tags,349,2,4) # Fraction Card 2/4
      newData$Num.Fraction34[[i]] <- getNumFraction(tags,350,3,4) # Fraction Card 3/4
      newData$Num.Fraction15[[i]] <- getNumFraction(tags,351,1,5) # Fraction Card 1/5
      newData$Num.Fraction25[[i]] <- getNumFraction(tags,352,2,5) # Fraction Card 2/5
      newData$Num.Fraction35[[i]] <- getNumFraction(tags,353,3,5) # Fraction Card 3/5
      newData$Num.Fraction45[[i]] <- getNumFraction(tags,354,4,5) # Fraction Card 4/5
      newData$Num.Fraction16[[i]] <- getNumFraction(tags,355,1,6) # Fraction Card 1/6
      newData$Num.Fraction26[[i]] <- getNumFraction(tags,356,2,6) # Fraction Card 2/6
      newData$Num.Fraction36[[i]] <- getNumFraction(tags,357,3,6) # Fraction Card 3/6
      newData$Num.Fraction46[[i]] <- getNumFraction(tags,358,4,6) # Fraction Card 4/6
      newData$Num.Fraction56[[i]] <- getNumFraction(tags,359,5,6) # Fraction Card 5/6
      newData$Num.Fraction110[[i]] <- getNumFraction(tags,360,1,10) # Fraction Card 1/10
      newData$Num.Fraction210[[i]] <- getNumFraction(tags,361,2,10) # Fraction Card 2/10
      newData$Num.Fraction310[[i]] <- getNumFraction(tags,362,3,10) # Fraction Card 3/10
      newData$Num.Fraction410[[i]] <- getNumFraction(tags,363,4,10) # Fraction Card 4/10
      newData$Num.Fraction510[[i]] <- getNumFraction(tags,364,5,10) # Fraction Card 5/10
      newData$Num.Fraction610[[i]] <- getNumFraction(tags,365,6,10) # Fraction Card 6/10
      newData$Num.Fraction710[[i]] <- getNumFraction(tags,366,7,10) # Fraction Card 7/10
      newData$Num.Fraction810[[i]] <- getNumFraction(tags,367,8,10) # Fraction Card 8/10
      newData$Num.Fraction910[[i]] <- getNumFraction(tags,368,9,10) # Fraction Card 9/10

      newData$Den.Fraction12[[i]] <- getDenFraction(tags,345,1,2) # Fraction Card 1/2
      newData$Den.Fraction13[[i]] <- getDenFraction(tags,346,1,3) # Fraction Card 1/3
      newData$Den.Fraction23[[i]] <- getDenFraction(tags,347,2,3) # Fraction Card 2/3
      newData$Den.Fraction14[[i]] <- getDenFraction(tags,348,1,4) # Fraction Card 1/4
      newData$Den.Fraction24[[i]] <- getDenFraction(tags,349,2,4) # Fraction Card 2/4
      newData$Den.Fraction34[[i]] <- getDenFraction(tags,350,3,4) # Fraction Card 3/4
      newData$Den.Fraction15[[i]] <- getDenFraction(tags,351,1,5) # Fraction Card 1/5
      newData$Den.Fraction25[[i]] <- getDenFraction(tags,352,2,5) # Fraction Card 2/5
      newData$Den.Fraction35[[i]] <- getDenFraction(tags,353,3,5) # Fraction Card 3/5
      newData$Den.Fraction45[[i]] <- getDenFraction(tags,354,4,5) # Fraction Card 4/5
      newData$Den.Fraction16[[i]] <- getDenFraction(tags,355,1,6) # Fraction Card 1/6
      newData$Den.Fraction26[[i]] <- getDenFraction(tags,356,2,6) # Fraction Card 2/6
      newData$Den.Fraction36[[i]] <- getDenFraction(tags,357,3,6) # Fraction Card 3/6
      newData$Den.Fraction46[[i]] <- getDenFraction(tags,358,4,6) # Fraction Card 4/6
      newData$Den.Fraction56[[i]] <- getDenFraction(tags,359,5,6) # Fraction Card 5/6
      newData$Den.Fraction110[[i]] <- getDenFraction(tags,360,1,10) # Fraction Card 1/10
      newData$Den.Fraction210[[i]] <- getDenFraction(tags,361,2,10) # Fraction Card 2/10
      newData$Den.Fraction310[[i]] <- getDenFraction(tags,362,3,10) # Fraction Card 3/10
      newData$Den.Fraction410[[i]] <- getDenFraction(tags,363,4,10) # Fraction Card 4/10
      newData$Den.Fraction510[[i]] <- getDenFraction(tags,364,5,10) # Fraction Card 5/10
      newData$Den.Fraction610[[i]] <- getDenFraction(tags,365,6,10) # Fraction Card 6/10
      newData$Den.Fraction710[[i]] <- getDenFraction(tags,366,7,10) # Fraction Card 7/10
      newData$Den.Fraction810[[i]] <- getDenFraction(tags,367,8,10) # Fraction Card 8/10
      newData$Den.Fraction910[[i]] <- getDenFraction(tags,368,9,10) # Fraction Card 9/10
      
      
      #For continuous fractions, we get the fraction value (as a decimal number)
      newData$Value.C1[[i]] <- getContinuousValue(tags,316:320,"C") # Continuous Circular 1 set
      newData$Value.C2[[i]] <- getContinuousValue(tags,321:325,"C") # Continuous Circular 2 set
      newData$Value.R1[[i]] <- getContinuousValue(tags,326:330,"R") # Continuous Rectangular 1 set
      newData$Value.R2[[i]] <- getContinuousValue(tags,331:335,"R") # Continuous Rectangular 2 set
      
      
      #We get the quadrant of each tag group, returned as characters "0" "Q1" "Q2" "Q3" "Q4" (to be later converted into factors)
      newData$C1[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,316:320) # Continuous Circular 1 set
      newData$C2[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,321:325) # Continuous Circular 2 set
      newData$R1[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,326:330) # Continuous Rectangular 1 set
      newData$R2[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,331:335) # Continuous Rectangular 2 set

      newData$Token1[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,1) # Tokens in Quadrant 1
      newData$Token2[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,2) # Tokens in Quadrant 2
      newData$Token3[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,3) # Tokens in Quadrant 3
      newData$Token4[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,4) # Tokens in Quadrant 4
      
      newData$Fraction12[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,345) # Fraction Card 1/2
      newData$Fraction13[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,346) # Fraction Card 1/3
      newData$Fraction23[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,347) # Fraction Card 2/3
      newData$Fraction14[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,348) # Fraction Card 1/4
      newData$Fraction24[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,349) # Fraction Card 2/4
      newData$Fraction34[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,350) # Fraction Card 3/4
      newData$Fraction15[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,351) # Fraction Card 1/5
      newData$Fraction25[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,352) # Fraction Card 2/5
      newData$Fraction35[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,353) # Fraction Card 3/5
      newData$Fraction45[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,354) # Fraction Card 4/5
      newData$Fraction16[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,355) # Fraction Card 1/6
      newData$Fraction26[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,356) # Fraction Card 2/6
      newData$Fraction36[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,357) # Fraction Card 3/6
      newData$Fraction46[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,358) # Fraction Card 4/6
      newData$Fraction56[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,359) # Fraction Card 5/6
      newData$Fraction110[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,360) # Fraction Card 1/10
      newData$Fraction210[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,361) # Fraction Card 2/10
      newData$Fraction310[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,362) # Fraction Card 3/10
      newData$Fraction410[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,363) # Fraction Card 4/10
      newData$Fraction510[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,364) # Fraction Card 5/10
      newData$Fraction610[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,365) # Fraction Card 6/10
      newData$Fraction710[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,366) # Fraction Card 7/10
      newData$Fraction810[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,367) # Fraction Card 8/10
      newData$Fraction910[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,368) # Fraction Card 9/10
      
      newData$Integer1R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,300) # Integer Card 1R
      newData$Integer2R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,301) # Integer Card 2R
      newData$Integer3R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,302) # Integer Card 3R
      newData$Integer4R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,303) # Integer Card 4R
      newData$Integer1G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,304) # Integer Card 1G
      newData$Integer2G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,305) # Integer Card 2G
      newData$Integer3G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,306) # Integer Card 3G
      newData$Integer4G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,307) # Integer Card 4G
      newData$Integer1B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,308) # Integer Card 1B
      newData$Integer2B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,309) # Integer Card 2B
      newData$Integer3B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,310) # Integer Card 3B
      newData$Integer4B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,311) # Integer Card 4B
      newData$Integer1P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,312) # Integer Card 1P
      newData$Integer2P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,313) # Integer Card 2P
      newData$Integer3P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,314) # Integer Card 3P
      newData$Integer4P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,315) # Integer Card 4P
      
      newData$Go[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,341:344) # Go! Card
      
      newData$DiscreteHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,336) # Discrete hint card
      newData$FractionHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,337) # Fraction hint card
      newData$CircularHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,340) # Circular hint card
      newData$RectangularHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,339) # Rectangular hint card
      newData$DecimalHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,338) # Decimal hint card
      
      newData$Carte1[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,409) # Carte 1 card
      newData$Carte2[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,410) # Carte 2 card
      newData$Carte3[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,411) # Carte 3 card
      newData$Carte4[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,412) # Carte 4 card
      newData$Carte5[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,413) # Carte 5 card
      newData$Carte6[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,414) # Carte 6 card
      newData$Carte7[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,415) # Carte 7 card
      newData$Carte8[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,416) # Carte 8 card
      newData$Carte9[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,417) # Carte 9 card
      newData$Carte10[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,418) # Carte 10 card
      
      #TODO: Optionally, we can add some time measurement represented by this value? but then what do we do with interruptions/gaps?
    
    }

    # We add the clean logs to this group's table
    if(length(totalData$timestamp)==0) totalData <- newData
    else totalData <- rbind(totalData, newData)
  
  }
  
  #We convert the character fields to factors
  #totalData[sapply(totalData, is.character)] <- lapply(totalData[sapply(totalData, is.character)], as.factor)
  if(length(totalData$timestamp)!=0){
    
    #Modify this if we add more fields that are not factors/quadrants
    #for(i in 2:length(totalData)) totalData[i][[1]] <- factor(totalData[i][[1]],levels=c("0","Q1","Q2","Q3","Q4"))
    
    cat (paste("Finished processing for group ",label,". Writing",as.character(length(totalData$timestamp)),"cleaned log entries to file\n"))
    # Go back to the original current dir
    setwd(originalDir)
    
    # serialize() the clean object into a binary R file
    save(totalData,file=paste(label,".rda",sep=""),compress=TRUE)
  }else{
    # Go back to the original current dir
    setwd(originalDir)

    cat (paste("Finished processing for group ",label,". No log entries to write!\n"))
  }
  
  #TODO: We read and join all .rda files in a single one with the additional "Group.Name" field?
  
  
}


# mergeSplitLogFiles2 - Merge/Split log files (improved version that parses and extracts also ladybug game variables)
# This function takes log files in JSON, loads them in R data frames, takes those times within a certain limit, and splits the tags 
# present into one observation each, including the x/y position of the center of the tag
# Parameters: directory the directory in which the logs to be merged/split are, startTime and endTime are the limits for this
# group's activities, and label is the name of the group, which will be used when exporting the file into binary form
# It also gets the game variables information in that tag, and adds it to the observations
mergeSplitLogFiles2 <- function(directory, startTime, endTime, label){
    
    cat (paste("Processing for group ",label,"...\n"))
    
    # We check that the time frontiers - beginning is earlier than end
    if(endTime<startTime) stop("Incorrect start/end times")
    
    # Store the current dir, to come back to it at the end, and change wd to the rootDir
    originalDir <- getwd()
    setwd(directory)
    
    # We loop through all the .log files in the directory
    logFiles <- list.files(pattern = "\\.json$")
    
    totalData <- data.frame(timestamp=numeric(),stringsAsFactors=TRUE)
    
    for(file in logFiles){
        cat (paste("Processing file... ",file,"\n"))
        
        
        # We import the JSON file
        jsonData <- fromJSON(file)
        
        # We turn the start/end times to milliseconds and compare with the timestamp of each entry
        startTimestamp <- as.numeric(startTime)*1000
        endTimestamp <- as.numeric(endTime)*1000
        
        # We get which entries are valid (within the time limits)
        validEntries <- jsonData[(jsonData$timestamp >= startTimestamp & jsonData$timestamp <= endTimestamp),]
        
        # if there are no valid entries, we go to the next logfile
        if(length(validEntries$timestamp)==0) next
        
        # we create the target dataframe, or append to it if it already has data
        newData <- data.frame(timestamp = validEntries$timestamp)
        
        # We iterate throughout the log entries
        for(i in 1:length(newData$timestamp)){
            
            # We get the game variables
            game <- validEntries$game
            
            # The game object looks like: 
            # HintType=-1; Map=4; Proportion1=0.5; Proportion2=0.5; Proportion3=0.6; Proportion4=0.2; 
            # BugPositionX=5; BugPositionY=7; Steps=0; StepsToGo=5; GameStarted=1; MapFinished=0; MapNew=0; 
            # Proportion1Num=5; Proportion2Num=1; Proportion3Num=3; Proportion4Num=2; Proportion1Den=10; 
            # Proportion2Den=2; Proportion3Den=5; Proportion4Den=10; WrongMove=0; P1Greater=0; P2Greater=0; P3Greater=0; P4Greater=0
            newData$HintType <- game$HintType
            newData$Map <- game$Map
            newData$Proportion1 <- game$Proportion1
            newData$Proportion2 <- game$Proportion2
            newData$Proportion3 <- game$Proportion3
            newData$Proportion4 <- game$Proportion4
            newData$BugPositionX <- game$BugPositionX
            newData$BugPositionY <- game$BugPositionY
            newData$Steps <- game$Steps
            newData$StepsToGo <- game$StepsToGo
            newData$GameStarted <- game$GameStarted
            newData$MapFinished <- game$MapFinished
            newData$MapNew <- game$MapNew
            newData$Proportion1Num <- game$Proportion1Num
            newData$Proportion2Num <- game$Proportion2Num
            newData$Proportion3Num <- game$Proportion3Num
            newData$Proportion4Num <- game$Proportion4Num
            newData$Proportion1Den <- game$Proportion1Den
            newData$Proportion2Den <- game$Proportion2Den
            newData$Proportion3Den <- game$Proportion3Den
            newData$Proportion4Den <- game$Proportion4Den
            newData$WrongMove <- game$WrongMove
            newData$P1Greater <- game$P1Greater
            newData$P2Greater <- game$P2Greater
            newData$P3Greater <- game$P3Greater
            newData$P4Greater <- game$P4Greater
            
                
                
            
            
            # We get the list of tags detected
            tags <- validEntries$tags[[i]]
            
            #We get the position (x,y coordinates) of each tag group, or NA if not present
            newData$Position.C1x[[i]] <- (getPositionTagGroup(tags,316:320))[[1]] # Continuous Circular 1 set
            newData$Position.C2x[[i]] <- (getPositionTagGroup(tags,321:325))[[1]] # Continuous Circular 2 set
            newData$Position.R1x[[i]] <- (getPositionTagGroup(tags,326:330))[[1]] # Continuous Rectangular 1 set
            newData$Position.R2x[[i]] <- (getPositionTagGroup(tags,331:335))[[1]] # Continuous Rectangular 2 set
            newData$Position.C1y[[i]] <- (getPositionTagGroup(tags,316:320))[[2]] # Continuous Circular 1 set
            newData$Position.C2y[[i]] <- (getPositionTagGroup(tags,321:325))[[2]] # Continuous Circular 2 set
            newData$Position.R1y[[i]] <- (getPositionTagGroup(tags,326:330))[[2]] # Continuous Rectangular 1 set
            newData$Position.R2y[[i]] <- (getPositionTagGroup(tags,331:335))[[2]] # Continuous Rectangular 2 set
            
            newData$Position.Token1x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,1))[[1]] # Tokens in Quadrant 1
            newData$Position.Token2x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,2))[[1]] # Tokens in Quadrant 2
            newData$Position.Token3x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,3))[[1]] # Tokens in Quadrant 3
            newData$Position.Token4x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,4))[[1]] # Tokens in Quadrant 4
            newData$Position.Token1y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,1))[[2]] # Tokens in Quadrant 1
            newData$Position.Token2y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,2))[[2]] # Tokens in Quadrant 2
            newData$Position.Token3y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,3))[[2]] # Tokens in Quadrant 3
            newData$Position.Token4y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,4))[[2]] # Tokens in Quadrant 4
            
            newData$Position.Fraction12x[[i]] <- (getPositionTagGroup(tags,345))[[1]] # Fraction Card 1/2
            newData$Position.Fraction13x[[i]] <- (getPositionTagGroup(tags,346))[[1]] # Fraction Card 1/3
            newData$Position.Fraction23x[[i]] <- (getPositionTagGroup(tags,347))[[1]] # Fraction Card 2/3
            newData$Position.Fraction14x[[i]] <- (getPositionTagGroup(tags,348))[[1]] # Fraction Card 1/4
            newData$Position.Fraction24x[[i]] <- (getPositionTagGroup(tags,349))[[1]] # Fraction Card 2/4
            newData$Position.Fraction34x[[i]] <- (getPositionTagGroup(tags,350))[[1]] # Fraction Card 3/4
            newData$Position.Fraction15x[[i]] <- (getPositionTagGroup(tags,351))[[1]] # Fraction Card 1/5
            newData$Position.Fraction25x[[i]] <- (getPositionTagGroup(tags,352))[[1]] # Fraction Card 2/5
            newData$Position.Fraction35x[[i]] <- (getPositionTagGroup(tags,353))[[1]] # Fraction Card 3/5
            newData$Position.Fraction45x[[i]] <- (getPositionTagGroup(tags,354))[[1]] # Fraction Card 4/5
            newData$Position.Fraction16x[[i]] <- (getPositionTagGroup(tags,355))[[1]] # Fraction Card 1/6
            newData$Position.Fraction26x[[i]] <- (getPositionTagGroup(tags,356))[[1]] # Fraction Card 2/6
            newData$Position.Fraction36x[[i]] <- (getPositionTagGroup(tags,357))[[1]] # Fraction Card 3/6
            newData$Position.Fraction46x[[i]] <- (getPositionTagGroup(tags,358))[[1]] # Fraction Card 4/6
            newData$Position.Fraction56x[[i]] <- (getPositionTagGroup(tags,359))[[1]] # Fraction Card 5/6
            newData$Position.Fraction110x[[i]] <- (getPositionTagGroup(tags,360))[[1]] # Fraction Card 1/10
            newData$Position.Fraction210x[[i]] <- (getPositionTagGroup(tags,361))[[1]] # Fraction Card 2/10
            newData$Position.Fraction310x[[i]] <- (getPositionTagGroup(tags,362))[[1]] # Fraction Card 3/10
            newData$Position.Fraction410x[[i]] <- (getPositionTagGroup(tags,363))[[1]] # Fraction Card 4/10
            newData$Position.Fraction510x[[i]] <- (getPositionTagGroup(tags,364))[[1]] # Fraction Card 5/10
            newData$Position.Fraction610x[[i]] <- (getPositionTagGroup(tags,365))[[1]] # Fraction Card 6/10
            newData$Position.Fraction710x[[i]] <- (getPositionTagGroup(tags,366))[[1]] # Fraction Card 7/10
            newData$Position.Fraction810x[[i]] <- (getPositionTagGroup(tags,367))[[1]] # Fraction Card 8/10
            newData$Position.Fraction910x[[i]] <- (getPositionTagGroup(tags,368))[[1]] # Fraction Card 9/10
            newData$Position.Fraction12y[[i]] <- (getPositionTagGroup(tags,345))[[2]] # Fraction Card 1/2
            newData$Position.Fraction13y[[i]] <- (getPositionTagGroup(tags,346))[[2]] # Fraction Card 1/3
            newData$Position.Fraction23y[[i]] <- (getPositionTagGroup(tags,347))[[2]] # Fraction Card 2/3
            newData$Position.Fraction14y[[i]] <- (getPositionTagGroup(tags,348))[[2]] # Fraction Card 1/4
            newData$Position.Fraction24y[[i]] <- (getPositionTagGroup(tags,349))[[2]] # Fraction Card 2/4
            newData$Position.Fraction34y[[i]] <- (getPositionTagGroup(tags,350))[[2]] # Fraction Card 3/4
            newData$Position.Fraction15y[[i]] <- (getPositionTagGroup(tags,351))[[2]] # Fraction Card 1/5
            newData$Position.Fraction25y[[i]] <- (getPositionTagGroup(tags,352))[[2]] # Fraction Card 2/5
            newData$Position.Fraction35y[[i]] <- (getPositionTagGroup(tags,353))[[2]] # Fraction Card 3/5
            newData$Position.Fraction45y[[i]] <- (getPositionTagGroup(tags,354))[[2]] # Fraction Card 4/5
            newData$Position.Fraction16y[[i]] <- (getPositionTagGroup(tags,355))[[2]] # Fraction Card 1/6
            newData$Position.Fraction26y[[i]] <- (getPositionTagGroup(tags,356))[[2]] # Fraction Card 2/6
            newData$Position.Fraction36y[[i]] <- (getPositionTagGroup(tags,357))[[2]] # Fraction Card 3/6
            newData$Position.Fraction46y[[i]] <- (getPositionTagGroup(tags,358))[[2]] # Fraction Card 4/6
            newData$Position.Fraction56y[[i]] <- (getPositionTagGroup(tags,359))[[2]] # Fraction Card 5/6
            newData$Position.Fraction110y[[i]] <- (getPositionTagGroup(tags,360))[[2]] # Fraction Card 1/10
            newData$Position.Fraction210y[[i]] <- (getPositionTagGroup(tags,361))[[2]] # Fraction Card 2/10
            newData$Position.Fraction310y[[i]] <- (getPositionTagGroup(tags,362))[[2]] # Fraction Card 3/10
            newData$Position.Fraction410y[[i]] <- (getPositionTagGroup(tags,363))[[2]] # Fraction Card 4/10
            newData$Position.Fraction510y[[i]] <- (getPositionTagGroup(tags,364))[[2]] # Fraction Card 5/10
            newData$Position.Fraction610y[[i]] <- (getPositionTagGroup(tags,365))[[2]] # Fraction Card 6/10
            newData$Position.Fraction710y[[i]] <- (getPositionTagGroup(tags,366))[[2]] # Fraction Card 7/10
            newData$Position.Fraction810y[[i]] <- (getPositionTagGroup(tags,367))[[2]] # Fraction Card 8/10
            newData$Position.Fraction910y[[i]] <- (getPositionTagGroup(tags,368))[[2]] # Fraction Card 9/10
            
            newData$Position.Integer1Rx[[i]] <- (getPositionTagGroup(tags,300))[[1]] # Integer Card 1R
            newData$Position.Integer2Rx[[i]] <- (getPositionTagGroup(tags,301))[[1]] # Integer Card 2R
            newData$Position.Integer3Rx[[i]] <- (getPositionTagGroup(tags,302))[[1]] # Integer Card 3R
            newData$Position.Integer4Rx[[i]] <- (getPositionTagGroup(tags,303))[[1]] # Integer Card 4R
            newData$Position.Integer1Gx[[i]] <- (getPositionTagGroup(tags,304))[[1]] # Integer Card 1G
            newData$Position.Integer2Gx[[i]] <- (getPositionTagGroup(tags,305))[[1]] # Integer Card 2G
            newData$Position.Integer3Gx[[i]] <- (getPositionTagGroup(tags,306))[[1]] # Integer Card 3G
            newData$Position.Integer4Gx[[i]] <- (getPositionTagGroup(tags,307))[[1]] # Integer Card 4G
            newData$Position.Integer1Bx[[i]] <- (getPositionTagGroup(tags,308))[[1]] # Integer Card 1B
            newData$Position.Integer2Bx[[i]] <- (getPositionTagGroup(tags,309))[[1]] # Integer Card 2B
            newData$Position.Integer3Bx[[i]] <- (getPositionTagGroup(tags,310))[[1]] # Integer Card 3B
            newData$Position.Integer4Bx[[i]] <- (getPositionTagGroup(tags,311))[[1]] # Integer Card 4B
            newData$Position.Integer1Px[[i]] <- (getPositionTagGroup(tags,312))[[1]] # Integer Card 1P
            newData$Position.Integer2Px[[i]] <- (getPositionTagGroup(tags,313))[[1]] # Integer Card 2P
            newData$Position.Integer3Px[[i]] <- (getPositionTagGroup(tags,314))[[1]] # Integer Card 3P
            newData$Position.Integer4Px[[i]] <- (getPositionTagGroup(tags,315))[[1]] # Integer Card 4P
            newData$Position.Integer1Ry[[i]] <- (getPositionTagGroup(tags,300))[[2]] # Integer Card 1R
            newData$Position.Integer2Ry[[i]] <- (getPositionTagGroup(tags,301))[[2]] # Integer Card 2R
            newData$Position.Integer3Ry[[i]] <- (getPositionTagGroup(tags,302))[[2]] # Integer Card 3R
            newData$Position.Integer4Ry[[i]] <- (getPositionTagGroup(tags,303))[[2]] # Integer Card 4R
            newData$Position.Integer1Gy[[i]] <- (getPositionTagGroup(tags,304))[[2]] # Integer Card 1G
            newData$Position.Integer2Gy[[i]] <- (getPositionTagGroup(tags,305))[[2]] # Integer Card 2G
            newData$Position.Integer3Gy[[i]] <- (getPositionTagGroup(tags,306))[[2]] # Integer Card 3G
            newData$Position.Integer4Gy[[i]] <- (getPositionTagGroup(tags,307))[[2]] # Integer Card 4G
            newData$Position.Integer1By[[i]] <- (getPositionTagGroup(tags,308))[[2]] # Integer Card 1B
            newData$Position.Integer2By[[i]] <- (getPositionTagGroup(tags,309))[[2]] # Integer Card 2B
            newData$Position.Integer3By[[i]] <- (getPositionTagGroup(tags,310))[[2]] # Integer Card 3B
            newData$Position.Integer4By[[i]] <- (getPositionTagGroup(tags,311))[[2]] # Integer Card 4B
            newData$Position.Integer1Py[[i]] <- (getPositionTagGroup(tags,312))[[2]] # Integer Card 1P
            newData$Position.Integer2Py[[i]] <- (getPositionTagGroup(tags,313))[[2]] # Integer Card 2P
            newData$Position.Integer3Py[[i]] <- (getPositionTagGroup(tags,314))[[2]] # Integer Card 3P
            newData$Position.Integer4Py[[i]] <- (getPositionTagGroup(tags,315))[[2]] # Integer Card 4P
            
            newData$Position.Gox[[i]] <- (getPositionTagGroup(tags,341:344))[[1]] # Go! Card
            newData$Position.Goy[[i]] <- (getPositionTagGroup(tags,341:344))[[2]] # Go! Card
            
            newData$Position.DiscreteHintx[[i]] <- (getPositionTagGroup(tags,336))[[1]] # Discrete hint card
            newData$Position.FractionHintx[[i]] <- (getPositionTagGroup(tags,337))[[1]] # Fraction hint card
            newData$Position.CircularHintx[[i]] <- (getPositionTagGroup(tags,340))[[1]] # Circular hint card
            newData$Position.RectangularHintx[[i]] <- (getPositionTagGroup(tags,339))[[1]] # Rectangular hint card
            newData$Position.DecimalHintx[[i]] <- (getPositionTagGroup(tags,338))[[1]] # Decimal hint card
            newData$Position.DiscreteHinty[[i]] <- (getPositionTagGroup(tags,336))[[2]] # Discrete hint card
            newData$Position.FractionHinty[[i]] <- (getPositionTagGroup(tags,337))[[2]] # Fraction hint card
            newData$Position.CircularHinty[[i]] <- (getPositionTagGroup(tags,340))[[2]] # Circular hint card
            newData$Position.RectangularHinty[[i]] <- (getPositionTagGroup(tags,339))[[2]] # Rectangular hint card
            newData$Position.DecimalHinty[[i]] <- (getPositionTagGroup(tags,338))[[2]] # Decimal hint card
            
            newData$Position.Carte1x[[i]] <- (getPositionTagGroup(tags,409))[[1]] # Carte 1 card
            newData$Position.Carte2x[[i]] <- (getPositionTagGroup(tags,410))[[1]] # Carte 2 card
            newData$Position.Carte3x[[i]] <- (getPositionTagGroup(tags,411))[[1]] # Carte 3 card
            newData$Position.Carte4x[[i]] <- (getPositionTagGroup(tags,412))[[1]] # Carte 4 card
            newData$Position.Carte5x[[i]] <- (getPositionTagGroup(tags,413))[[1]] # Carte 5 card
            newData$Position.Carte6x[[i]] <- (getPositionTagGroup(tags,414))[[1]] # Carte 6 card
            newData$Position.Carte7x[[i]] <- (getPositionTagGroup(tags,415))[[1]] # Carte 7 card
            newData$Position.Carte8x[[i]] <- (getPositionTagGroup(tags,416))[[1]] # Carte 8 card
            newData$Position.Carte9x[[i]] <- (getPositionTagGroup(tags,417))[[1]] # Carte 9 card
            newData$Position.Carte10x[[i]] <- (getPositionTagGroup(tags,418))[[1]] # Carte 10 card
            newData$Position.Carte1y[[i]] <- (getPositionTagGroup(tags,409))[[2]] # Carte 1 card
            newData$Position.Carte2y[[i]] <- (getPositionTagGroup(tags,410))[[2]] # Carte 2 card
            newData$Position.Carte3y[[i]] <- (getPositionTagGroup(tags,411))[[2]] # Carte 3 card
            newData$Position.Carte4y[[i]] <- (getPositionTagGroup(tags,412))[[2]] # Carte 4 card
            newData$Position.Carte5y[[i]] <- (getPositionTagGroup(tags,413))[[2]] # Carte 5 card
            newData$Position.Carte6y[[i]] <- (getPositionTagGroup(tags,414))[[2]] # Carte 6 card
            newData$Position.Carte7y[[i]] <- (getPositionTagGroup(tags,415))[[2]] # Carte 7 card
            newData$Position.Carte8y[[i]] <- (getPositionTagGroup(tags,416))[[2]] # Carte 8 card
            newData$Position.Carte9y[[i]] <- (getPositionTagGroup(tags,417))[[2]] # Carte 9 card
            newData$Position.Carte10y[[i]] <- (getPositionTagGroup(tags,418))[[2]] # Carte 10 card
            
            #We get the rotation of each tag group (except the tokens), as a value in radians or NA if not present
            newData$Rotation.C1[[i]] <- getRotationTagGroup(tags,316:319) # Continuous Circular 1 set, not including the central tag
            newData$Rotation.C2[[i]] <- getRotationTagGroup(tags,321:324) # Continuous Circular 2 set, not including the central tag
            newData$Rotation.R1[[i]] <- getRotationTagGroup(tags,326:329) # Continuous Rectangular 1 set, not including the central tag
            newData$Rotation.R2[[i]] <- getRotationTagGroup(tags,331:334) # Continuous Rectangular 2 set, not including the central tag
            
            newData$Rotation.Fraction12[[i]] <- getRotationTagGroup(tags,345) # Fraction Card 1/2
            newData$Rotation.Fraction13[[i]] <- getRotationTagGroup(tags,346) # Fraction Card 1/3
            newData$Rotation.Fraction23[[i]] <- getRotationTagGroup(tags,347) # Fraction Card 2/3
            newData$Rotation.Fraction14[[i]] <- getRotationTagGroup(tags,348) # Fraction Card 1/4
            newData$Rotation.Fraction24[[i]] <- getRotationTagGroup(tags,349) # Fraction Card 2/4
            newData$Rotation.Fraction34[[i]] <- getRotationTagGroup(tags,350) # Fraction Card 3/4
            newData$Rotation.Fraction15[[i]] <- getRotationTagGroup(tags,351) # Fraction Card 1/5
            newData$Rotation.Fraction25[[i]] <- getRotationTagGroup(tags,352) # Fraction Card 2/5
            newData$Rotation.Fraction35[[i]] <- getRotationTagGroup(tags,353) # Fraction Card 3/5
            newData$Rotation.Fraction45[[i]] <- getRotationTagGroup(tags,354) # Fraction Card 4/5
            newData$Rotation.Fraction16[[i]] <- getRotationTagGroup(tags,355) # Fraction Card 1/6
            newData$Rotation.Fraction26[[i]] <- getRotationTagGroup(tags,356) # Fraction Card 2/6
            newData$Rotation.Fraction36[[i]] <- getRotationTagGroup(tags,357) # Fraction Card 3/6
            newData$Rotation.Fraction46[[i]] <- getRotationTagGroup(tags,358) # Fraction Card 4/6
            newData$Rotation.Fraction56[[i]] <- getRotationTagGroup(tags,359) # Fraction Card 5/6
            newData$Rotation.Fraction110[[i]] <- getRotationTagGroup(tags,360) # Fraction Card 1/10
            newData$Rotation.Fraction210[[i]] <- getRotationTagGroup(tags,361) # Fraction Card 2/10
            newData$Rotation.Fraction310[[i]] <- getRotationTagGroup(tags,362) # Fraction Card 3/10
            newData$Rotation.Fraction410[[i]] <- getRotationTagGroup(tags,363) # Fraction Card 4/10
            newData$Rotation.Fraction510[[i]] <- getRotationTagGroup(tags,364) # Fraction Card 5/10
            newData$Rotation.Fraction610[[i]] <- getRotationTagGroup(tags,365) # Fraction Card 6/10
            newData$Rotation.Fraction710[[i]] <- getRotationTagGroup(tags,366) # Fraction Card 7/10
            newData$Rotation.Fraction810[[i]] <- getRotationTagGroup(tags,367) # Fraction Card 8/10
            newData$Rotation.Fraction910[[i]] <- getRotationTagGroup(tags,368) # Fraction Card 9/10
            
            newData$Rotation.Integer1R[[i]] <- getRotationTagGroup(tags,300) # Integer Card 1R
            newData$Rotation.Integer2R[[i]] <- getRotationTagGroup(tags,301) # Integer Card 2R
            newData$Rotation.Integer3R[[i]] <- getRotationTagGroup(tags,302) # Integer Card 3R
            newData$Rotation.Integer4R[[i]] <- getRotationTagGroup(tags,303) # Integer Card 4R
            newData$Rotation.Integer1G[[i]] <- getRotationTagGroup(tags,304) # Integer Card 1G
            newData$Rotation.Integer2G[[i]] <- getRotationTagGroup(tags,305) # Integer Card 2G
            newData$Rotation.Integer3G[[i]] <- getRotationTagGroup(tags,306) # Integer Card 3G
            newData$Rotation.Integer4G[[i]] <- getRotationTagGroup(tags,307) # Integer Card 4G
            newData$Rotation.Integer1B[[i]] <- getRotationTagGroup(tags,308) # Integer Card 1B
            newData$Rotation.Integer2B[[i]] <- getRotationTagGroup(tags,309) # Integer Card 2B
            newData$Rotation.Integer3B[[i]] <- getRotationTagGroup(tags,310) # Integer Card 3B
            newData$Rotation.Integer4B[[i]] <- getRotationTagGroup(tags,311) # Integer Card 4B
            newData$Rotation.Integer1P[[i]] <- getRotationTagGroup(tags,312) # Integer Card 1P
            newData$Rotation.Integer2P[[i]] <- getRotationTagGroup(tags,313) # Integer Card 2P
            newData$Rotation.Integer3P[[i]] <- getRotationTagGroup(tags,314) # Integer Card 3P
            newData$Rotation.Integer4P[[i]] <- getRotationTagGroup(tags,315) # Integer Card 4P
            
            newData$Rotation.Go[[i]] <- getRotationTagGroup(tags,341:344) # Go! Card
            
            newData$Rotation.DiscreteHint[[i]] <- getRotationTagGroup(tags,336) # Discrete hint card
            newData$Rotation.FractionHint[[i]] <- getRotationTagGroup(tags,337) # Fraction hint card
            newData$Rotation.CircularHint[[i]] <- getRotationTagGroup(tags,340) # Circular hint card
            newData$Rotation.RectangularHint[[i]] <- getRotationTagGroup(tags,339) # Rectangular hint card
            newData$Rotation.DecimalHint[[i]] <- getRotationTagGroup(tags,338) # Decimal hint card
            
            newData$Rotation.Carte1[[i]] <- getRotationTagGroup(tags,409) # Carte 1 card
            newData$Rotation.Carte2[[i]] <- getRotationTagGroup(tags,410) # Carte 2 card
            newData$Rotation.Carte3[[i]] <- getRotationTagGroup(tags,411) # Carte 3 card
            newData$Rotation.Carte4[[i]] <- getRotationTagGroup(tags,412) # Carte 4 card
            newData$Rotation.Carte5[[i]] <- getRotationTagGroup(tags,413) # Carte 5 card
            newData$Rotation.Carte6[[i]] <- getRotationTagGroup(tags,414) # Carte 6 card
            newData$Rotation.Carte7[[i]] <- getRotationTagGroup(tags,415) # Carte 7 card
            newData$Rotation.Carte8[[i]] <- getRotationTagGroup(tags,416) # Carte 8 card
            newData$Rotation.Carte9[[i]] <- getRotationTagGroup(tags,417) # Carte 9 card
            newData$Rotation.Carte10[[i]] <- getRotationTagGroup(tags,418) # Carte 10 card
            
            # For token and abstract fractions, we get the numerator and denominator, or NA if not present
            newData$Num.Token1[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,1) # Tokens in Quadrant 1
            newData$Num.Token2[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,2) # Tokens in Quadrant 2
            newData$Num.Token3[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,3) # Tokens in Quadrant 3
            newData$Num.Token4[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,4) # Tokens in Quadrant 4
            newData$Den.Token1[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,1) # Tokens in Quadrant 1
            newData$Den.Token2[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,2) # Tokens in Quadrant 2
            newData$Den.Token3[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,3) # Tokens in Quadrant 3
            newData$Den.Token4[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,4) # Tokens in Quadrant 4
            
            newData$Num.Fraction12[[i]] <- getNumFraction(tags,345,1,2) # Fraction Card 1/2
            newData$Num.Fraction13[[i]] <- getNumFraction(tags,346,1,3) # Fraction Card 1/3
            newData$Num.Fraction23[[i]] <- getNumFraction(tags,347,2,3) # Fraction Card 2/3
            newData$Num.Fraction14[[i]] <- getNumFraction(tags,348,1,4) # Fraction Card 1/4
            newData$Num.Fraction24[[i]] <- getNumFraction(tags,349,2,4) # Fraction Card 2/4
            newData$Num.Fraction34[[i]] <- getNumFraction(tags,350,3,4) # Fraction Card 3/4
            newData$Num.Fraction15[[i]] <- getNumFraction(tags,351,1,5) # Fraction Card 1/5
            newData$Num.Fraction25[[i]] <- getNumFraction(tags,352,2,5) # Fraction Card 2/5
            newData$Num.Fraction35[[i]] <- getNumFraction(tags,353,3,5) # Fraction Card 3/5
            newData$Num.Fraction45[[i]] <- getNumFraction(tags,354,4,5) # Fraction Card 4/5
            newData$Num.Fraction16[[i]] <- getNumFraction(tags,355,1,6) # Fraction Card 1/6
            newData$Num.Fraction26[[i]] <- getNumFraction(tags,356,2,6) # Fraction Card 2/6
            newData$Num.Fraction36[[i]] <- getNumFraction(tags,357,3,6) # Fraction Card 3/6
            newData$Num.Fraction46[[i]] <- getNumFraction(tags,358,4,6) # Fraction Card 4/6
            newData$Num.Fraction56[[i]] <- getNumFraction(tags,359,5,6) # Fraction Card 5/6
            newData$Num.Fraction110[[i]] <- getNumFraction(tags,360,1,10) # Fraction Card 1/10
            newData$Num.Fraction210[[i]] <- getNumFraction(tags,361,2,10) # Fraction Card 2/10
            newData$Num.Fraction310[[i]] <- getNumFraction(tags,362,3,10) # Fraction Card 3/10
            newData$Num.Fraction410[[i]] <- getNumFraction(tags,363,4,10) # Fraction Card 4/10
            newData$Num.Fraction510[[i]] <- getNumFraction(tags,364,5,10) # Fraction Card 5/10
            newData$Num.Fraction610[[i]] <- getNumFraction(tags,365,6,10) # Fraction Card 6/10
            newData$Num.Fraction710[[i]] <- getNumFraction(tags,366,7,10) # Fraction Card 7/10
            newData$Num.Fraction810[[i]] <- getNumFraction(tags,367,8,10) # Fraction Card 8/10
            newData$Num.Fraction910[[i]] <- getNumFraction(tags,368,9,10) # Fraction Card 9/10
            
            newData$Den.Fraction12[[i]] <- getDenFraction(tags,345,1,2) # Fraction Card 1/2
            newData$Den.Fraction13[[i]] <- getDenFraction(tags,346,1,3) # Fraction Card 1/3
            newData$Den.Fraction23[[i]] <- getDenFraction(tags,347,2,3) # Fraction Card 2/3
            newData$Den.Fraction14[[i]] <- getDenFraction(tags,348,1,4) # Fraction Card 1/4
            newData$Den.Fraction24[[i]] <- getDenFraction(tags,349,2,4) # Fraction Card 2/4
            newData$Den.Fraction34[[i]] <- getDenFraction(tags,350,3,4) # Fraction Card 3/4
            newData$Den.Fraction15[[i]] <- getDenFraction(tags,351,1,5) # Fraction Card 1/5
            newData$Den.Fraction25[[i]] <- getDenFraction(tags,352,2,5) # Fraction Card 2/5
            newData$Den.Fraction35[[i]] <- getDenFraction(tags,353,3,5) # Fraction Card 3/5
            newData$Den.Fraction45[[i]] <- getDenFraction(tags,354,4,5) # Fraction Card 4/5
            newData$Den.Fraction16[[i]] <- getDenFraction(tags,355,1,6) # Fraction Card 1/6
            newData$Den.Fraction26[[i]] <- getDenFraction(tags,356,2,6) # Fraction Card 2/6
            newData$Den.Fraction36[[i]] <- getDenFraction(tags,357,3,6) # Fraction Card 3/6
            newData$Den.Fraction46[[i]] <- getDenFraction(tags,358,4,6) # Fraction Card 4/6
            newData$Den.Fraction56[[i]] <- getDenFraction(tags,359,5,6) # Fraction Card 5/6
            newData$Den.Fraction110[[i]] <- getDenFraction(tags,360,1,10) # Fraction Card 1/10
            newData$Den.Fraction210[[i]] <- getDenFraction(tags,361,2,10) # Fraction Card 2/10
            newData$Den.Fraction310[[i]] <- getDenFraction(tags,362,3,10) # Fraction Card 3/10
            newData$Den.Fraction410[[i]] <- getDenFraction(tags,363,4,10) # Fraction Card 4/10
            newData$Den.Fraction510[[i]] <- getDenFraction(tags,364,5,10) # Fraction Card 5/10
            newData$Den.Fraction610[[i]] <- getDenFraction(tags,365,6,10) # Fraction Card 6/10
            newData$Den.Fraction710[[i]] <- getDenFraction(tags,366,7,10) # Fraction Card 7/10
            newData$Den.Fraction810[[i]] <- getDenFraction(tags,367,8,10) # Fraction Card 8/10
            newData$Den.Fraction910[[i]] <- getDenFraction(tags,368,9,10) # Fraction Card 9/10
            
            
            #For continuous fractions, we get the fraction value (as a decimal number)
            newData$Value.C1[[i]] <- getContinuousValue(tags,316:320,"C") # Continuous Circular 1 set
            newData$Value.C2[[i]] <- getContinuousValue(tags,321:325,"C") # Continuous Circular 2 set
            newData$Value.R1[[i]] <- getContinuousValue(tags,326:330,"R") # Continuous Rectangular 1 set
            newData$Value.R2[[i]] <- getContinuousValue(tags,331:335,"R") # Continuous Rectangular 2 set
            
            
            #We get the quadrant of each tag group, returned as characters "0" "Q1" "Q2" "Q3" "Q4" (to be later converted into factors)
            newData$C1[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,316:320) # Continuous Circular 1 set
            newData$C2[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,321:325) # Continuous Circular 2 set
            newData$R1[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,326:330) # Continuous Rectangular 1 set
            newData$R2[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,331:335) # Continuous Rectangular 2 set
            
            newData$Token1[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,1) # Tokens in Quadrant 1
            newData$Token2[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,2) # Tokens in Quadrant 2
            newData$Token3[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,3) # Tokens in Quadrant 3
            newData$Token4[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,4) # Tokens in Quadrant 4
            
            newData$Fraction12[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,345) # Fraction Card 1/2
            newData$Fraction13[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,346) # Fraction Card 1/3
            newData$Fraction23[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,347) # Fraction Card 2/3
            newData$Fraction14[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,348) # Fraction Card 1/4
            newData$Fraction24[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,349) # Fraction Card 2/4
            newData$Fraction34[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,350) # Fraction Card 3/4
            newData$Fraction15[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,351) # Fraction Card 1/5
            newData$Fraction25[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,352) # Fraction Card 2/5
            newData$Fraction35[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,353) # Fraction Card 3/5
            newData$Fraction45[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,354) # Fraction Card 4/5
            newData$Fraction16[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,355) # Fraction Card 1/6
            newData$Fraction26[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,356) # Fraction Card 2/6
            newData$Fraction36[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,357) # Fraction Card 3/6
            newData$Fraction46[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,358) # Fraction Card 4/6
            newData$Fraction56[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,359) # Fraction Card 5/6
            newData$Fraction110[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,360) # Fraction Card 1/10
            newData$Fraction210[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,361) # Fraction Card 2/10
            newData$Fraction310[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,362) # Fraction Card 3/10
            newData$Fraction410[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,363) # Fraction Card 4/10
            newData$Fraction510[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,364) # Fraction Card 5/10
            newData$Fraction610[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,365) # Fraction Card 6/10
            newData$Fraction710[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,366) # Fraction Card 7/10
            newData$Fraction810[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,367) # Fraction Card 8/10
            newData$Fraction910[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,368) # Fraction Card 9/10
            
            newData$Integer1R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,300) # Integer Card 1R
            newData$Integer2R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,301) # Integer Card 2R
            newData$Integer3R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,302) # Integer Card 3R
            newData$Integer4R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,303) # Integer Card 4R
            newData$Integer1G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,304) # Integer Card 1G
            newData$Integer2G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,305) # Integer Card 2G
            newData$Integer3G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,306) # Integer Card 3G
            newData$Integer4G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,307) # Integer Card 4G
            newData$Integer1B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,308) # Integer Card 1B
            newData$Integer2B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,309) # Integer Card 2B
            newData$Integer3B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,310) # Integer Card 3B
            newData$Integer4B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,311) # Integer Card 4B
            newData$Integer1P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,312) # Integer Card 1P
            newData$Integer2P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,313) # Integer Card 2P
            newData$Integer3P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,314) # Integer Card 3P
            newData$Integer4P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,315) # Integer Card 4P
            
            newData$Go[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,341:344) # Go! Card
            
            newData$DiscreteHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,336) # Discrete hint card
            newData$FractionHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,337) # Fraction hint card
            newData$CircularHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,340) # Circular hint card
            newData$RectangularHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,339) # Rectangular hint card
            newData$DecimalHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,338) # Decimal hint card
            
            newData$Carte1[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,409) # Carte 1 card
            newData$Carte2[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,410) # Carte 2 card
            newData$Carte3[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,411) # Carte 3 card
            newData$Carte4[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,412) # Carte 4 card
            newData$Carte5[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,413) # Carte 5 card
            newData$Carte6[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,414) # Carte 6 card
            newData$Carte7[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,415) # Carte 7 card
            newData$Carte8[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,416) # Carte 8 card
            newData$Carte9[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,417) # Carte 9 card
            newData$Carte10[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,418) # Carte 10 card
            
            #TODO: Optionally, we can add some time measurement represented by this value? but then what do we do with interruptions/gaps?
            
        }
        
        # We add the clean logs to this group's table
        if(length(totalData$timestamp)==0) totalData <- newData
        else totalData <- rbind(totalData, newData)
        
    }
    
    #We convert the character fields to factors
    #totalData[sapply(totalData, is.character)] <- lapply(totalData[sapply(totalData, is.character)], as.factor)
    if(length(totalData$timestamp)!=0){
        
        #Modify this if we add more fields that are not factors/quadrants
        #for(i in 2:length(totalData)) totalData[i][[1]] <- factor(totalData[i][[1]],levels=c("0","Q1","Q2","Q3","Q4"))
        
        cat (paste("Finished processing for group ",label,". Writing",as.character(length(totalData$timestamp)),"cleaned log entries to file\n"))
        # Go back to the original current dir
        setwd(originalDir)
        
        # serialize() the clean object into a binary R file
        save(totalData,file=paste(label,".rda",sep=""),compress=TRUE)
    }else{
        # Go back to the original current dir
        setwd(originalDir)
        
        cat (paste("Finished processing for group ",label,". No log entries to write!\n"))
    }
    
    #TODO: We read and join all .rda files in a single one with the additional "Group.Name" field?
    
    
}






###################################################
## HELPER FUNCTIONS ###############################
###################################################

# Returns a vector with the numerator and denominator of a fraction card if it is present in the table, or NA if it's not present
getNumFraction <- function(tags,targetTags,numerator,denominator){
  
  # This is the variable that will store the tag group's center
  num <- 0
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  
  # if none of the tags of this tangible is present, return 0
  if(length(presentTags$id)==0) return(NA)
  else{ #Some tags are present, the pass the numerator received
    num <- as.integer(numerator)
  }
  
  num
}

getDenFraction <- function(tags,targetTags,numerator,denominator){
  
  # This is the variable that will store the tag group's center
  den <- 1
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  
  # if none of the tags of this tangible is present, return 0
  if(length(presentTags$id)==0) return(NA)
  else{ #Some tags are present, let's calculate the centroid
    den <- as.integer(denominator)
  }
  
  den
}



# Returns a vector with the numerator (number of yellow tokens) and denominator (number of tokens) of a group of tokens IN A QUADRANT, or NA if it's not present
getNumTokensInQuadrant <- function(tags,width,height,targetTags,numeratorTags,quadrantToCheck){
  
  # This is the variable that will store the tag group's center
  numerator <- 0
  #denominator <- 0
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  presentNumTags <- tags[tags$id %in% numeratorTags,]
  
  # if none of the tokens is present IN THE WHOLE TABLE, return NA
  if(length(presentTags$id)==0) return(NA)
  else if(length(presentNumTags$id)==0) return(0)
  else{ #Some yellow tags are present, let's calculate which quadrants they're on
    #centers <- sapply(presentTags$corners,getCenter) #this is an 2xn matrix with all the tokens center coordinates
    centersNum <- sapply(presentNumTags$corners,getCenter) #this is an 2xn matrix with the yellow tokens center coordinates
    # We restrict our focus to only one quadrant's centers
    if(quadrantToCheck==1){
      #centers <- as.matrix(centers[,(centers[1,] >= width/2 & centers[2,] < height/2)])
      centersNum <- as.matrix(centersNum[,(centersNum[1,] >= width/2 & centersNum[2,] < height/2)])
    }else if(quadrantToCheck==2){
      #centers <- as.matrix(centers[,(centers[1,] < width/2 & centers[2,] < height/2)])
      centersNum <- as.matrix(centersNum[,(centersNum[1,] < width/2 & centersNum[2,] < height/2)])
    }else if(quadrantToCheck==3){
      #centers <- as.matrix(centers[,(centers[1,] < width/2 & centers[2,] >= height/2)])
      centersNum <- as.matrix(centersNum[,(centersNum[1,] < width/2 & centersNum[2,] >= height/2)])
    }else if(quadrantToCheck==4){
      #centers <- as.matrix(centers[,(centers[1,] >= width/2 & centers[2,] >= height/2)])
      centersNum <- as.matrix(centersNum[,(centersNum[1,] >= width/2 & centersNum[2,] >= height/2)])
    }
  }
  
  # We Get the number of concerned tags in our quadrant
  if(length(centersNum)==0) return(0)
  else{
    #denominator <- as.integer(dim(centers)[[2]])
    numerator <- as.integer(dim(centersNum)[[2]])
  }
  
  return(numerator)
  
}

# Returns a vector with the numerator (number of yellow tokens) and denominator (number of tokens) of a group of tokens IN A QUADRANT, or NA if it's not present
getDenTokensInQuadrant <- function(tags,width,height,targetTags,numeratorTags,quadrantToCheck){
  
  # This is the variable that will store the tag group's center
  numerator <- 0
  denominator <- 0
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  #presentNumTags <- tags[tags$id %in% numeratorTags,]
  
  # if none of the tokens is present IN THE WHOLE TABLE, return NA
  if(length(presentTags$id)==0) return(NA)
  else{ #Some tags are present, let's calculate which quadrants they're on
    centers <- sapply(presentTags$corners,getCenter) #this is an 2xn matrix with all the tokens center coordinates
    #centersNum <- sapply(numeratorTags$corners,getCenter) #this is an 2xn matrix with the yellow tokens center coordinates
    # We restrict our focus to only one quadrant's centers
    if(quadrantToCheck==1){
      centers <- as.matrix(centers[,(centers[1,] >= width/2 & centers[2,] < height/2)])
      #centersNum <- as.matrix(centersNum[,(centersNum[1,] >= width/2 & centersNum[2,] < height/2)])
    }else if(quadrantToCheck==2){
      centers <- as.matrix(centers[,(centers[1,] < width/2 & centers[2,] < height/2)])
      #centersNum <- as.matrix(centersNum[,(centersNum[1,] < width/2 & centersNum[2,] < height/2)])
    }else if(quadrantToCheck==3){
      centers <- as.matrix(centers[,(centers[1,] < width/2 & centers[2,] >= height/2)])
      #centersNum <- as.matrix(centersNum[,(centersNum[1,] < width/2 & centersNum[2,] >= height/2)])
    }else if(quadrantToCheck==4){
      centers <- as.matrix(centers[,(centers[1,] >= width/2 & centers[2,] >= height/2)])
      #centersNum <- as.matrix(centersNum[,(centersNum[1,] >= width/2 & centersNum[2,] >= height/2)])
    }
  }
  
  # We Get the number of concerned tags in our quadrant
  if(length(centers)==0) return(NA)
  else{
    denominator <- as.integer(dim(centers)[[2]])
    #numerator <- as.integer(dim(centersNum)[[2]])
  }
  
  return(denominator)
  
}



# getQuadrantTagGroup - this function gets a dataframe with the present tags and their 
# coordinates in a certain point in time, and returns the centroid of the token tags
# WITHIN A CERTAIN QUADRANT (since a set of tokens can be distributed in all four quadrants),
# responding with a numeric vector (x,y) or NA if no tokens are on that quadrant
# Parameters: tags is the data.frame with the tag ids and their corners; width and height are the size of the display, 
# which define the position of the quadrants, targetTags is the group of tags that make up the 
# global token taggroup, quadrantToCheck is the quadrant on which we are focusing
getPositionTokensInQuadrant <- function(tags,width,height,targetTags,quadrantToCheck){
  
  # This is the variable that will store the tag group's center
  position <- c(0,0)
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  
  # if none of the tokens is present IN THE WHOLE TABLE, return 0
  if(length(presentTags$id)==0) return(c(NA,NA))
  else{ #Some tags are present, let's calculate which quadrants they're on
    centers <- sapply(presentTags$corners,getCenter) #this is an 2xn matrix with all the tokens center coordinates
    # We restrict our focus to only one quadrant's centers
    if(quadrantToCheck==1){
      centers <- as.matrix(centers[,(centers[1,] >= width/2 & centers[2,] < height/2)])
    }else if(quadrantToCheck==2){
      centers <- as.matrix(centers[,(centers[1,] < width/2 & centers[2,] < height/2)])
    }else if(quadrantToCheck==3){
      centers <- as.matrix(centers[,(centers[1,] < width/2 & centers[2,] >= height/2)])
    }else if(quadrantToCheck==4){
      centers <- as.matrix(centers[,(centers[1,] >= width/2 & centers[2,] >= height/2)])
    }
  }
  
  # We Get the center of the concerned tags in our quadrant
  if(length(centers)==0) return(c(NA,NA))
  else position <- rowMeans(centers)
  
  return(position)
}

# getPositionTagGroup - Given a set of detected tags (a data.frame), and a subgroup of all the tag ids to detect (a vector),
# this function returns the centroid of this tag/tag group (a vector with two named numbers, the x-position and y-position in the screen)
# or NA if the target tags are not present
getPositionTagGroup <- function(tags,targetTags){
  
  # This is the variable that will store the tag group's center
  position <- c(0,0)
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  
  # if none of the tags of this tangible is present, return 0
  if(length(presentTags$id)==0) return(c(NA,NA))
  else{ #Some tags are present, let's calculate the centroid
    position <- getListCenter(presentTags$corners)
  }
  
  position
}


# getRotationTagGroup - Given a set of detected tags (a data.frame), and a subgroup of all the tag ids to detect (a vector),
# this function returns the angle in which it is gyrated, from the vertical (a numeric value in radians)
# or NA if the target tags are not present
getRotationTagGroup <- function(tags,targetTags){
  
  # This is the variable that will store the tag group's center
  rotation <- 0
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  
  # if none of the tags of this tangible is present, return NA
  if(length(presentTags$id)==0) return(NA)
  else{ #Some tags are present, let's calculate the centroid
    rotation <- getListRotation(presentTags$corners)
  }
  
  rotation
}

# getListRotation - gets the rotation angle of a group of tags, in radians.
# It receives a list of the detected tags to check, where each element is a list of 8 values (the four corners of the tag, x1, y1, x2, y2...)
# (probably you can assume that all tags in the list will have the same rotation, or you can do an average of all of them 
# - the center in the circular one should not have been passed onto here)
getListRotation <- function(listCorners){
  
  if(length(listCorners)==0) return(NA)
  
  rotation <- 0 # Do whatever here
  
  #We take the points of the first tag of the manipulative (we assume that with the first corner is enough), we just need the first and second corner
  x1 <- listCorners[[1]][[1]]
  y1 <- listCorners[[1]][[2]]
  x2 <- listCorners[[1]][[3]]
  y2 <- listCorners[[1]][[4]]
  
  x <- x2 - x1
  y <- y2 - y1
    
  if(x > 0 && y > 0)    rotation <- atan(y/x) #Case 1: first cuadrant
  else if (x < 0 && y > 0)  rotation <- atan(-x/y) + pi/2 #Case 2: second cuadrant
  else if (x < 0 && y < 0)  rotation <- atan(y/x) + pi #Case 3: third cuadrant
  else if (x > 0 && y < 0)  rotation <- atan(-x/y) + 3*pi/2 #Case 4: fourth cuadrant
  else if (x == 0 && y > 0) rotation <- pi/2 #Case 5: Y axis (+ direction) -> 90
  else if (x < 0 && y == 0) rotation <- pi #Case 6: X axis (- direction) -> 180
  else if (x == 0 && y < 0) rotation <- 3*pi/2 #Case 7: Y axis (- direction) -> 270
  else{
    rotation <- 0 #Case 8: X axis (+ direction)
  }  
    
  return(rotation)
  
}

# getContinuousValue - this function calculates and returns the decimal value equivalent to the detected fraction manipulative (or NA if it cannot be calculated)
# It receives a list of detected tags in a certain moment (a vector with the 8 corner coordinates, x1, y1, x2, y2...), 
# the tag ids of the concerned manipulative (targetTags, the last one is the center one),
# and a character denoting the kind of continuous manipulative (C)ircular or (R)ectangular
getContinuousValue <- function(tags,targetTags,contType){
  
  # This is the variable that will store the tag group's center
  value <- 0
  
  centerTagId <- targetTags[[length(targetTags)]] # This is the tag in the center of the continuous manipulative, the last value of the vector
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  presentCenter <- tags[tags$id %in% centerTagId,]
  # Circular
  # First manipulative: tag 1 <- 316, tag 2 <- 317
  # Second manipulative: tag 1 <- 321, tag 2 <- 322
  # Rectangular
  # First manipulative: tag 1 <- 326, tag 2 <- 327
  # Second manipulative: tag 1 <- 331, tag 2 <- 332
  presentTag1 <- presentTags[presentTags$id %in% c(316,321,326,331),]
  presentTag2 <- presentTags[presentTags$id %in% c(317,322,327,332),]
  presentTag34 <- presentTags[presentTags$id %in% c(318,319,323,324,328,329,333,334),]
  
  if(length(presentTags$id)>3 && contType=="R"){
    value2 <- 3
  }
  # if the needed tags of this tangible to calculate the value are not present, return NA
  if(length(presentTags$id)==0 || length(presentCenter$id)==0 || length(presentTag1$id)==0 || length(presentTag2$id)==0 || (contType=="C" && length(presentTag34$id)==0)) return(NA)
  else{ #the tags are present, let's calculate the value

    
      if(contType=="R"){
        
          value <- 0 # Do whatever with the here to calculate the rectangular value
          
          #The corner [0] of the tag [0] of the marker rectangle_origin (is a point2d)
          originPoint <- c(x = presentTag1$corners[[1]][[1]], y =  presentTag1$corners[[1]][[2]]) 
          
          #The the corner [0] of the tag ofthe marker rectangle_control (is a point2d)
          endPoint <- c(x = presentCenter$corners[[1]][[1]], y = presentCenter$corners[[1]][[2]])
          
          #The corner [0] of the tag [1] of the marker rectangle_origin (is a point2d)
          startPoint <- c(x = presentTag2$corners[[1]][[1]], y = presentTag2$corners[[1]][[2]])
            
          #Calculate the angle
          angle <- getVertexAngle(startPoint, originPoint, endPoint)
          
          #We calculate the distance between the two tags
          totalDistance <- getDistance (originPoint, startPoint)
          
          projectedDistance <- getDistance(originPoint,endPoint)
          projectedDistance <- projectedDistance*cos(angle)
          
          value <- (projectedDistance / totalDistance)

      }
        
      else if(contType=="C"){ #We need at least 3 tags, to calculate the center of the circunference
        
          value <- 0 # Do whatever with the here to calculate the circular value
          
        
          x1 <- sum(presentTag1$corners[[1]][[1]],presentTag1$corners[[1]][[3]],presentTag1$corners[[1]][[5]],presentTag1$corners[[1]][[7]])/4 #Center (x) of the first tag
          y1 <- sum(presentTag1$corners[[1]][[2]],presentTag1$corners[[1]][[4]],presentTag1$corners[[1]][[6]],presentTag1$corners[[1]][[8]])/4 #Center (y) of the first tag
          
          x2 <- sum(presentTag2$corners[[1]][[1]],presentTag2$corners[[1]][[3]],presentTag2$corners[[1]][[5]],presentTag2$corners[[1]][[7]])/4 #Center (x) of the second tag 
          y2 <- sum(presentTag2$corners[[1]][[2]],presentTag2$corners[[1]][[4]],presentTag2$corners[[1]][[6]],presentTag2$corners[[1]][[8]])/4 #Center (y) of the second tag
          
          x3 <- sum(presentTag34$corners[[1]][[1]],presentTag34$corners[[1]][[3]],presentTag34$corners[[1]][[5]],presentTag34$corners[[1]][[7]])/4 #Center (x) of the second tag 
          y3 <- sum(presentTag34$corners[[1]][[2]],presentTag34$corners[[1]][[4]],presentTag34$corners[[1]][[6]],presentTag34$corners[[1]][[8]])/4 #Center (y) of the second tag
          
          #This is the center of the marker circular_origin (is a point2d)
          
          if(length(presentTag34$id) == 2){ #If we have the two corners identified, then we calculate the center of the 4th tag
            x4 <- sum(presentTag34$corners[[2]][[1]],presentTag34$corners[[2]][[3]],presentTag34$corners[[2]][[5]],presentTag34$corners[[2]][[7]])/4 #Center (x) of the second tag 
            y4 <- sum(presentTag34$corners[[2]][[2]],presentTag34$corners[[2]][[4]],presentTag34$corners[[2]][[6]],presentTag34$corners[[2]][[8]])/4 #Center (y) of the second tag
            
            originPoint <- c(x= sum(x1,x2,x3,x4)/4, y= sum(y1,y2,y3,y4)/4)
            
          }else{ #We have to calculate considering the three points
            
            angleBetweenTags <- getVertexAngle(c(x = x2 , y = y2), c(x = x1 , y = y1), c(x = x3, y = y3)) 
            
            if(angleBetweenTags < pi/2){#If the bottom tag is the 3th one
              originPoint.x <- sum(x1,x3)/2
            } else { #The bottom tag is the 4th one
              originPoint.x <- sum(x2,x3)/2
            }
            originPoint.y <- sum(y1,y3)/2
            
            originPoint <- c(x=originPoint.x , y = originPoint.y) #Center of the manipulative.
          }
           
          
          #This is the center of the marker circunf_control (is a point2d)  
          endPoint.x <- sum(presentCenter$corners[[1]][[1]],presentCenter$corners[[1]][[3]],presentCenter$corners[[1]][[5]],presentCenter$corners[[1]][[7]])/4
          endPoint.y <- sum(presentCenter$corners[[1]][[2]],presentCenter$corners[[1]][[4]],presentCenter$corners[[1]][[6]],presentCenter$corners[[1]][[8]])/4
          
          #endPoint <- c(x = mean(presentCenter$corners[[1]][[1]],presentCenter$corners[[1]][[3]]),y = mean(presentCenter$corners[[1]][[4]],presentCenter$corners[[1]][[6]]))
          endPoint <- c(x = endPoint.x, y = endPoint.y)
          
          #Centroid of the tag [0] and [1] of the circular_origin marker (is a point2d)
          startPoint <- c(x = sum(x1,x2)/2 , y = sum(y1,y2)/2) 
            
          angle <- getVertexAngle(startPoint, originPoint, endPoint) #Check the parameters
          
          value <- (angle/(2*pi))

      }else return(NA)
  }
  
  value
}


# getQuadrantTagGroup - this function gets a dataframe with the present tags and their 
# coordinates in a certain point in time, and returns whether the centroid of the token tags
# are or not WITHIN A CERTAIN QUADRANT (since a set of tokens can be distributed in all four quadrants),
# responding with a string:  "0" or "Qn" (to be later converted into factors)
# Parameters: tags is the data.frame with the tag ids and their corners; width and height are the size of the display, 
# which define the position of the quadrants, targetTags is the group of tags that make up the 
# global token taggroup, quadrantToCheck is the quadrant on which we are focusing
getTokensInQuadrant <- function(tags,width,height,targetTags,quadrantToCheck){
  
  # This is the variable that will store the tag group's center
  position <- c(x=0,y=0)
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]

  # if none of the tokens is present IN THE WHOLE TABLE, return 0
  if(length(presentTags$id)==0) return("0")
  else{ #Some tags are present, let's calculate which quadrants they're on
    centers <- sapply(presentTags$corners,getCenter) #this is an 2xn matrix with all the tokens center coordinates
    # We restrict our focus to only one quadrant's centers
    if(quadrantToCheck==1){
      centers <- as.matrix(centers[,(centers[1,] >= width/2 & centers[2,] < height/2)])
    }else if(quadrantToCheck==2){
      centers <- as.matrix(centers[,(centers[1,] < width/2 & centers[2,] < height/2)])
    }else if(quadrantToCheck==3){
      centers <- as.matrix(centers[,(centers[1,] < width/2 & centers[2,] >= height/2)])
    }else if(quadrantToCheck==4){
      centers <- as.matrix(centers[,(centers[1,] >= width/2 & centers[2,] >= height/2)])
    }
  }
  
  # We Get the center of the concerned tags in our quadrant
  if(length(centers)==0) return("0")
  else position <- rowMeans(centers)
  
  return(getQuadrantFromPosition(position,width,height))
  
}

# getQuadrantTagGroup - this function gets a dataframe with the present tags and their 
# coordinates in a certain point in time, and returns whether the centroid of a group of tags is present or not,
# an in which quadrant, as a string:  "0" "Q1" "Q2" "Q3" "Q4" (to be later converted into factors)
# Parameters: tags is the data.frame with the tag ids and their corners; width and height are the size of the display, 
# which define the position of the quadrants, targetTags is the group of tags that make up the taggroup
getQuadrantTagGroup <- function(tags,width,height,targetTags){
  
  # This is the variable that will store the tag group's center
  position <- c(0,0)
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  
  # if none of the tags of this tangible is present, return 0
  if(length(presentTags$id)==0) return("0")
  else{ #Some tags are present, let's calculate the centroid
      position <- getListCenter(presentTags$corners)
  }
  
  return(getQuadrantFromPosition(position,width,height))
  
}

# getListCenter - gets the centroid of a group of tags, in the form of a list with the 8 coordinates of the corners
getListCenter <- function(listCorners){
  
  if(length(listCorners)==0) return(c(x=0,y=0))
  
  centers <- sapply(listCorners,getCenter)
  
  return(c(mean(centers[1,]),mean(centers[2,])))
  
}

# getCenter - gets the average of a x,y coordinates vector (odd members are x, even members are y)
getCenter <- function(xyvector){
  
  if(length(xyvector)<2) return(c(0,0))
  
  xcoords <- xyvector[seq(1, length(xyvector), 2)]
  ycoords <- xyvector[seq(2, length(xyvector), 2)]
  
  return(c(mean(xcoords),mean(ycoords)))
  
}

# getQuadrantFromPosition - returns a string with the quadrant of a position in the display: 
# Q1 is top-right, Q2 top-left, Q3 bottom-left, Q4 bottom-right
getQuadrantFromPosition <- function(pos,width,height){
  
  if(pos[1] >= width/2 && pos[2] < height/2) return("Q1")
  else if(pos[1] >= width/2 && pos[2] >= height/2) return("Q4")
  else if(pos[1] < width/2 && pos[2] >= height/2) return("Q3")
  else return("Q2")
  
}

# getVertexAngle - gets the angle between three points x1,y1,x2,y2,x3,y3 in radians
getVertexAngle <- function(startPoint, originPoint, endPoint){ ##Put the input here
  
  #I need the x1, x2, x3, y1, y2, y3
  x1 <- startPoint[[1]] - originPoint[[1]]
  x3 <- endPoint[[1]] - originPoint[[1]]
  y1 <- startPoint[[2]] - originPoint[[2]]
  y3 <- endPoint[[2]] - originPoint[[2]]
  
  #We calculate the distance
  distance <- ((x1 * x1) + (y1 * y1))*((x3 * x3) + (y3 * y3))
  
  if(distance == 0) return (0)
  else{
    input <- (x1*x3 + y1*y3)/sqrt(distance)
  
    if(input == 1)  return (0)
    else if (input == -1) return (pi)
    else  return(acos(input))
  }
}

#getDistance - gets the distance between two points (x1,y1) and (x2,y2)
getDistance <- function(point1, point2){
  
  x = point2[[1]] - point1[[1]]
  y = point2[[2]] - point1[[2]]
  
  return (sqrt(x * x + y * y))
}



# preprocessJDCLogsAlternative - Clean and organize the logs, ACTUALLY SLOWER VARIANT!
# This is the overall function in charge of the preprocessing and cleaning of the log data from the JDC experiment
# Parameters: rootDir the directory in which the logs to be merged/split are, in folders called "lamp 1", "lamp 2"...
preprocessJDCLogsAlternative <- function(rootDir,doYAMLConversion=FALSE){
  
  if(doYAMLConversion){
    cat ("Please ensure that all .log files have ending parentheses so that they are valid pseudo-yaml, and save them as .yaml files. Then press [enter] to continue")
    line <- readline()
    # Convert the .yaml files to JSON .json files
    convertLogsToJson(paste(rootDir, "lamp 1", sep="/"))
    convertLogsToJson(paste(rootDir, "lamp 2", sep="/"))
    convertLogsToJson(paste(rootDir, "lamp 3", sep="/"))
    convertLogsToJson(paste(rootDir, "lamp 4", sep="/"))
    convertLogsToJson(paste(rootDir, "lamp 5", sep="/"))
  } 
  
  # We import the groups, start and end times from a csv in the rootDir
  syncData <- read.csv(paste(rootDir,"ActivityTiming-synchronization.csv", sep="/"), stringsAsFactors=FALSE)
  
  options(digits.secs = 3)
  
  totalData <- data.frame(timestamp=numeric())
  
  for(l in 1:5){ #For each of the five folders with json log files
    cat (paste("Processing for lamp ",l,"...\n"))
    
    timings <- syncData[syncData$Lamp.. == l,c("Lamp..","Code","RealStart","RealEnd")]
    timings$start <- as.numeric(as.POSIXct(strptime(timings$RealStart, "%Y-%m-%d %H:%M:%OS")))*1000
    timings$end <- as.numeric(as.POSIXct(strptime(timings$RealEnd, "%Y-%m-%d %H:%M:%OS")))*1000
    
    # We loop through all the .log files in the directory
    setwd(paste(rootDir,"/lamp ",l,sep=""))
    logFiles <- list.files(pattern = "\\.json$")
    
    for(file in logFiles){
      cat (paste("Processing file... ",file,"\n"))
      
      # We import the JSON file
      jsonData <- fromJSON(file)
      
      newData <- data.frame(timestamp=jsonData$timestamp)
      
      for(i in 1:nrow(jsonData)){# for each log entry
        # We calculate which group this belongs with
        newData$Group.Name[[i]] <- getGroupFromTimestamp(timings,newData$timestamp[[i]])
        
        # if the group is not NA, we set the rest of the variables
        if(!is.na(newData$Group.Name[[i]])){
          
          # We get the list of tags detected
          tags <- jsonData$tags[[i]]
          
          #We get the position (x,y coordinates) of each tag group, or NA if not present
          newData$Position.C1x[[i]] <- (getPositionTagGroup(tags,316:320))[[1]] # Continuous Circular 1 set
          newData$Position.C2x[[i]] <- (getPositionTagGroup(tags,321:325))[[1]] # Continuous Circular 2 set
          newData$Position.R1x[[i]] <- (getPositionTagGroup(tags,326:330))[[1]] # Continuous Rectangular 1 set
          newData$Position.R2x[[i]] <- (getPositionTagGroup(tags,331:335))[[1]] # Continuous Rectangular 2 set
          newData$Position.C1y[[i]] <- (getPositionTagGroup(tags,316:320))[[2]] # Continuous Circular 1 set
          newData$Position.C2y[[i]] <- (getPositionTagGroup(tags,321:325))[[2]] # Continuous Circular 2 set
          newData$Position.R1y[[i]] <- (getPositionTagGroup(tags,326:330))[[2]] # Continuous Rectangular 1 set
          newData$Position.R2y[[i]] <- (getPositionTagGroup(tags,331:335))[[2]] # Continuous Rectangular 2 set
          
          newData$Position.Token1x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,1))[[1]] # Tokens in Quadrant 1
          newData$Position.Token2x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,2))[[1]] # Tokens in Quadrant 2
          newData$Position.Token3x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,3))[[1]] # Tokens in Quadrant 3
          newData$Position.Token4x[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,4))[[1]] # Tokens in Quadrant 4
          newData$Position.Token1y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,1))[[2]] # Tokens in Quadrant 1
          newData$Position.Token2y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,2))[[2]] # Tokens in Quadrant 2
          newData$Position.Token3y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,3))[[2]] # Tokens in Quadrant 3
          newData$Position.Token4y[[i]] <- (getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,4))[[2]] # Tokens in Quadrant 4
          
          newData$Position.Fraction12x[[i]] <- (getPositionTagGroup(tags,345))[[1]] # Fraction Card 1/2
          newData$Position.Fraction13x[[i]] <- (getPositionTagGroup(tags,346))[[1]] # Fraction Card 1/3
          newData$Position.Fraction23x[[i]] <- (getPositionTagGroup(tags,347))[[1]] # Fraction Card 2/3
          newData$Position.Fraction14x[[i]] <- (getPositionTagGroup(tags,348))[[1]] # Fraction Card 1/4
          newData$Position.Fraction24x[[i]] <- (getPositionTagGroup(tags,349))[[1]] # Fraction Card 2/4
          newData$Position.Fraction34x[[i]] <- (getPositionTagGroup(tags,350))[[1]] # Fraction Card 3/4
          newData$Position.Fraction15x[[i]] <- (getPositionTagGroup(tags,351))[[1]] # Fraction Card 1/5
          newData$Position.Fraction25x[[i]] <- (getPositionTagGroup(tags,352))[[1]] # Fraction Card 2/5
          newData$Position.Fraction35x[[i]] <- (getPositionTagGroup(tags,353))[[1]] # Fraction Card 3/5
          newData$Position.Fraction45x[[i]] <- (getPositionTagGroup(tags,354))[[1]] # Fraction Card 4/5
          newData$Position.Fraction16x[[i]] <- (getPositionTagGroup(tags,355))[[1]] # Fraction Card 1/6
          newData$Position.Fraction26x[[i]] <- (getPositionTagGroup(tags,356))[[1]] # Fraction Card 2/6
          newData$Position.Fraction36x[[i]] <- (getPositionTagGroup(tags,357))[[1]] # Fraction Card 3/6
          newData$Position.Fraction46x[[i]] <- (getPositionTagGroup(tags,358))[[1]] # Fraction Card 4/6
          newData$Position.Fraction56x[[i]] <- (getPositionTagGroup(tags,359))[[1]] # Fraction Card 5/6
          newData$Position.Fraction110x[[i]] <- (getPositionTagGroup(tags,360))[[1]] # Fraction Card 1/10
          newData$Position.Fraction210x[[i]] <- (getPositionTagGroup(tags,361))[[1]] # Fraction Card 2/10
          newData$Position.Fraction310x[[i]] <- (getPositionTagGroup(tags,362))[[1]] # Fraction Card 3/10
          newData$Position.Fraction410x[[i]] <- (getPositionTagGroup(tags,363))[[1]] # Fraction Card 4/10
          newData$Position.Fraction510x[[i]] <- (getPositionTagGroup(tags,364))[[1]] # Fraction Card 5/10
          newData$Position.Fraction610x[[i]] <- (getPositionTagGroup(tags,365))[[1]] # Fraction Card 6/10
          newData$Position.Fraction710x[[i]] <- (getPositionTagGroup(tags,366))[[1]] # Fraction Card 7/10
          newData$Position.Fraction810x[[i]] <- (getPositionTagGroup(tags,367))[[1]] # Fraction Card 8/10
          newData$Position.Fraction910x[[i]] <- (getPositionTagGroup(tags,368))[[1]] # Fraction Card 9/10
          newData$Position.Fraction12y[[i]] <- (getPositionTagGroup(tags,345))[[2]] # Fraction Card 1/2
          newData$Position.Fraction13y[[i]] <- (getPositionTagGroup(tags,346))[[2]] # Fraction Card 1/3
          newData$Position.Fraction23y[[i]] <- (getPositionTagGroup(tags,347))[[2]] # Fraction Card 2/3
          newData$Position.Fraction14y[[i]] <- (getPositionTagGroup(tags,348))[[2]] # Fraction Card 1/4
          newData$Position.Fraction24y[[i]] <- (getPositionTagGroup(tags,349))[[2]] # Fraction Card 2/4
          newData$Position.Fraction34y[[i]] <- (getPositionTagGroup(tags,350))[[2]] # Fraction Card 3/4
          newData$Position.Fraction15y[[i]] <- (getPositionTagGroup(tags,351))[[2]] # Fraction Card 1/5
          newData$Position.Fraction25y[[i]] <- (getPositionTagGroup(tags,352))[[2]] # Fraction Card 2/5
          newData$Position.Fraction35y[[i]] <- (getPositionTagGroup(tags,353))[[2]] # Fraction Card 3/5
          newData$Position.Fraction45y[[i]] <- (getPositionTagGroup(tags,354))[[2]] # Fraction Card 4/5
          newData$Position.Fraction16y[[i]] <- (getPositionTagGroup(tags,355))[[2]] # Fraction Card 1/6
          newData$Position.Fraction26y[[i]] <- (getPositionTagGroup(tags,356))[[2]] # Fraction Card 2/6
          newData$Position.Fraction36y[[i]] <- (getPositionTagGroup(tags,357))[[2]] # Fraction Card 3/6
          newData$Position.Fraction46y[[i]] <- (getPositionTagGroup(tags,358))[[2]] # Fraction Card 4/6
          newData$Position.Fraction56y[[i]] <- (getPositionTagGroup(tags,359))[[2]] # Fraction Card 5/6
          newData$Position.Fraction110y[[i]] <- (getPositionTagGroup(tags,360))[[2]] # Fraction Card 1/10
          newData$Position.Fraction210y[[i]] <- (getPositionTagGroup(tags,361))[[2]] # Fraction Card 2/10
          newData$Position.Fraction310y[[i]] <- (getPositionTagGroup(tags,362))[[2]] # Fraction Card 3/10
          newData$Position.Fraction410y[[i]] <- (getPositionTagGroup(tags,363))[[2]] # Fraction Card 4/10
          newData$Position.Fraction510y[[i]] <- (getPositionTagGroup(tags,364))[[2]] # Fraction Card 5/10
          newData$Position.Fraction610y[[i]] <- (getPositionTagGroup(tags,365))[[2]] # Fraction Card 6/10
          newData$Position.Fraction710y[[i]] <- (getPositionTagGroup(tags,366))[[2]] # Fraction Card 7/10
          newData$Position.Fraction810y[[i]] <- (getPositionTagGroup(tags,367))[[2]] # Fraction Card 8/10
          newData$Position.Fraction910y[[i]] <- (getPositionTagGroup(tags,368))[[2]] # Fraction Card 9/10
          
          newData$Position.Integer1Rx[[i]] <- (getPositionTagGroup(tags,300))[[1]] # Integer Card 1R
          newData$Position.Integer2Rx[[i]] <- (getPositionTagGroup(tags,301))[[1]] # Integer Card 2R
          newData$Position.Integer3Rx[[i]] <- (getPositionTagGroup(tags,302))[[1]] # Integer Card 3R
          newData$Position.Integer4Rx[[i]] <- (getPositionTagGroup(tags,303))[[1]] # Integer Card 4R
          newData$Position.Integer1Gx[[i]] <- (getPositionTagGroup(tags,304))[[1]] # Integer Card 1G
          newData$Position.Integer2Gx[[i]] <- (getPositionTagGroup(tags,305))[[1]] # Integer Card 2G
          newData$Position.Integer3Gx[[i]] <- (getPositionTagGroup(tags,306))[[1]] # Integer Card 3G
          newData$Position.Integer4Gx[[i]] <- (getPositionTagGroup(tags,307))[[1]] # Integer Card 4G
          newData$Position.Integer1Bx[[i]] <- (getPositionTagGroup(tags,308))[[1]] # Integer Card 1B
          newData$Position.Integer2Bx[[i]] <- (getPositionTagGroup(tags,309))[[1]] # Integer Card 2B
          newData$Position.Integer3Bx[[i]] <- (getPositionTagGroup(tags,310))[[1]] # Integer Card 3B
          newData$Position.Integer4Bx[[i]] <- (getPositionTagGroup(tags,311))[[1]] # Integer Card 4B
          newData$Position.Integer1Px[[i]] <- (getPositionTagGroup(tags,312))[[1]] # Integer Card 1P
          newData$Position.Integer2Px[[i]] <- (getPositionTagGroup(tags,313))[[1]] # Integer Card 2P
          newData$Position.Integer3Px[[i]] <- (getPositionTagGroup(tags,314))[[1]] # Integer Card 3P
          newData$Position.Integer4Px[[i]] <- (getPositionTagGroup(tags,315))[[1]] # Integer Card 4P
          newData$Position.Integer1Ry[[i]] <- (getPositionTagGroup(tags,300))[[2]] # Integer Card 1R
          newData$Position.Integer2Ry[[i]] <- (getPositionTagGroup(tags,301))[[2]] # Integer Card 2R
          newData$Position.Integer3Ry[[i]] <- (getPositionTagGroup(tags,302))[[2]] # Integer Card 3R
          newData$Position.Integer4Ry[[i]] <- (getPositionTagGroup(tags,303))[[2]] # Integer Card 4R
          newData$Position.Integer1Gy[[i]] <- (getPositionTagGroup(tags,304))[[2]] # Integer Card 1G
          newData$Position.Integer2Gy[[i]] <- (getPositionTagGroup(tags,305))[[2]] # Integer Card 2G
          newData$Position.Integer3Gy[[i]] <- (getPositionTagGroup(tags,306))[[2]] # Integer Card 3G
          newData$Position.Integer4Gy[[i]] <- (getPositionTagGroup(tags,307))[[2]] # Integer Card 4G
          newData$Position.Integer1By[[i]] <- (getPositionTagGroup(tags,308))[[2]] # Integer Card 1B
          newData$Position.Integer2By[[i]] <- (getPositionTagGroup(tags,309))[[2]] # Integer Card 2B
          newData$Position.Integer3By[[i]] <- (getPositionTagGroup(tags,310))[[2]] # Integer Card 3B
          newData$Position.Integer4By[[i]] <- (getPositionTagGroup(tags,311))[[2]] # Integer Card 4B
          newData$Position.Integer1Py[[i]] <- (getPositionTagGroup(tags,312))[[2]] # Integer Card 1P
          newData$Position.Integer2Py[[i]] <- (getPositionTagGroup(tags,313))[[2]] # Integer Card 2P
          newData$Position.Integer3Py[[i]] <- (getPositionTagGroup(tags,314))[[2]] # Integer Card 3P
          newData$Position.Integer4Py[[i]] <- (getPositionTagGroup(tags,315))[[2]] # Integer Card 4P
          
          newData$Position.Gox[[i]] <- (getPositionTagGroup(tags,341:344))[[1]] # Go! Card
          newData$Position.Goy[[i]] <- (getPositionTagGroup(tags,341:344))[[2]] # Go! Card
          
          newData$Position.DiscreteHintx[[i]] <- (getPositionTagGroup(tags,336))[[1]] # Discrete hint card
          newData$Position.FractionHintx[[i]] <- (getPositionTagGroup(tags,337))[[1]] # Fraction hint card
          newData$Position.CircularHintx[[i]] <- (getPositionTagGroup(tags,340))[[1]] # Circular hint card
          newData$Position.RectangularHintx[[i]] <- (getPositionTagGroup(tags,339))[[1]] # Rectangular hint card
          newData$Position.DecimalHintx[[i]] <- (getPositionTagGroup(tags,338))[[1]] # Decimal hint card
          newData$Position.DiscreteHinty[[i]] <- (getPositionTagGroup(tags,336))[[2]] # Discrete hint card
          newData$Position.FractionHinty[[i]] <- (getPositionTagGroup(tags,337))[[2]] # Fraction hint card
          newData$Position.CircularHinty[[i]] <- (getPositionTagGroup(tags,340))[[2]] # Circular hint card
          newData$Position.RectangularHinty[[i]] <- (getPositionTagGroup(tags,339))[[2]] # Rectangular hint card
          newData$Position.DecimalHinty[[i]] <- (getPositionTagGroup(tags,338))[[2]] # Decimal hint card
          
          newData$Position.Carte1x[[i]] <- (getPositionTagGroup(tags,409))[[1]] # Carte 1 card
          newData$Position.Carte2x[[i]] <- (getPositionTagGroup(tags,410))[[1]] # Carte 2 card
          newData$Position.Carte3x[[i]] <- (getPositionTagGroup(tags,411))[[1]] # Carte 3 card
          newData$Position.Carte4x[[i]] <- (getPositionTagGroup(tags,412))[[1]] # Carte 4 card
          newData$Position.Carte5x[[i]] <- (getPositionTagGroup(tags,413))[[1]] # Carte 5 card
          newData$Position.Carte6x[[i]] <- (getPositionTagGroup(tags,414))[[1]] # Carte 6 card
          newData$Position.Carte7x[[i]] <- (getPositionTagGroup(tags,415))[[1]] # Carte 7 card
          newData$Position.Carte8x[[i]] <- (getPositionTagGroup(tags,416))[[1]] # Carte 8 card
          newData$Position.Carte9x[[i]] <- (getPositionTagGroup(tags,417))[[1]] # Carte 9 card
          newData$Position.Carte10x[[i]] <- (getPositionTagGroup(tags,418))[[1]] # Carte 10 card
          newData$Position.Carte1y[[i]] <- (getPositionTagGroup(tags,409))[[2]] # Carte 1 card
          newData$Position.Carte2y[[i]] <- (getPositionTagGroup(tags,410))[[2]] # Carte 2 card
          newData$Position.Carte3y[[i]] <- (getPositionTagGroup(tags,411))[[2]] # Carte 3 card
          newData$Position.Carte4y[[i]] <- (getPositionTagGroup(tags,412))[[2]] # Carte 4 card
          newData$Position.Carte5y[[i]] <- (getPositionTagGroup(tags,413))[[2]] # Carte 5 card
          newData$Position.Carte6y[[i]] <- (getPositionTagGroup(tags,414))[[2]] # Carte 6 card
          newData$Position.Carte7y[[i]] <- (getPositionTagGroup(tags,415))[[2]] # Carte 7 card
          newData$Position.Carte8y[[i]] <- (getPositionTagGroup(tags,416))[[2]] # Carte 8 card
          newData$Position.Carte9y[[i]] <- (getPositionTagGroup(tags,417))[[2]] # Carte 9 card
          newData$Position.Carte10y[[i]] <- (getPositionTagGroup(tags,418))[[2]] # Carte 10 card
          
          #We get the rotation of each tag group (except the tokens), as a value in radians or NA if not present
          newData$Rotation.C1[[i]] <- getRotationTagGroup(tags,316:319) # Continuous Circular 1 set, not including the central tag
          newData$Rotation.C2[[i]] <- getRotationTagGroup(tags,321:324) # Continuous Circular 2 set, not including the central tag
          newData$Rotation.R1[[i]] <- getRotationTagGroup(tags,326:329) # Continuous Rectangular 1 set, not including the central tag
          newData$Rotation.R2[[i]] <- getRotationTagGroup(tags,331:334) # Continuous Rectangular 2 set, not including the central tag
          
          newData$Rotation.Fraction12[[i]] <- getRotationTagGroup(tags,345) # Fraction Card 1/2
          newData$Rotation.Fraction13[[i]] <- getRotationTagGroup(tags,346) # Fraction Card 1/3
          newData$Rotation.Fraction23[[i]] <- getRotationTagGroup(tags,347) # Fraction Card 2/3
          newData$Rotation.Fraction14[[i]] <- getRotationTagGroup(tags,348) # Fraction Card 1/4
          newData$Rotation.Fraction24[[i]] <- getRotationTagGroup(tags,349) # Fraction Card 2/4
          newData$Rotation.Fraction34[[i]] <- getRotationTagGroup(tags,350) # Fraction Card 3/4
          newData$Rotation.Fraction15[[i]] <- getRotationTagGroup(tags,351) # Fraction Card 1/5
          newData$Rotation.Fraction25[[i]] <- getRotationTagGroup(tags,352) # Fraction Card 2/5
          newData$Rotation.Fraction35[[i]] <- getRotationTagGroup(tags,353) # Fraction Card 3/5
          newData$Rotation.Fraction45[[i]] <- getRotationTagGroup(tags,354) # Fraction Card 4/5
          newData$Rotation.Fraction16[[i]] <- getRotationTagGroup(tags,355) # Fraction Card 1/6
          newData$Rotation.Fraction26[[i]] <- getRotationTagGroup(tags,356) # Fraction Card 2/6
          newData$Rotation.Fraction36[[i]] <- getRotationTagGroup(tags,357) # Fraction Card 3/6
          newData$Rotation.Fraction46[[i]] <- getRotationTagGroup(tags,358) # Fraction Card 4/6
          newData$Rotation.Fraction56[[i]] <- getRotationTagGroup(tags,359) # Fraction Card 5/6
          newData$Rotation.Fraction110[[i]] <- getRotationTagGroup(tags,360) # Fraction Card 1/10
          newData$Rotation.Fraction210[[i]] <- getRotationTagGroup(tags,361) # Fraction Card 2/10
          newData$Rotation.Fraction310[[i]] <- getRotationTagGroup(tags,362) # Fraction Card 3/10
          newData$Rotation.Fraction410[[i]] <- getRotationTagGroup(tags,363) # Fraction Card 4/10
          newData$Rotation.Fraction510[[i]] <- getRotationTagGroup(tags,364) # Fraction Card 5/10
          newData$Rotation.Fraction610[[i]] <- getRotationTagGroup(tags,365) # Fraction Card 6/10
          newData$Rotation.Fraction710[[i]] <- getRotationTagGroup(tags,366) # Fraction Card 7/10
          newData$Rotation.Fraction810[[i]] <- getRotationTagGroup(tags,367) # Fraction Card 8/10
          newData$Rotation.Fraction910[[i]] <- getRotationTagGroup(tags,368) # Fraction Card 9/10
          
          newData$Rotation.Integer1R[[i]] <- getRotationTagGroup(tags,300) # Integer Card 1R
          newData$Rotation.Integer2R[[i]] <- getRotationTagGroup(tags,301) # Integer Card 2R
          newData$Rotation.Integer3R[[i]] <- getRotationTagGroup(tags,302) # Integer Card 3R
          newData$Rotation.Integer4R[[i]] <- getRotationTagGroup(tags,303) # Integer Card 4R
          newData$Rotation.Integer1G[[i]] <- getRotationTagGroup(tags,304) # Integer Card 1G
          newData$Rotation.Integer2G[[i]] <- getRotationTagGroup(tags,305) # Integer Card 2G
          newData$Rotation.Integer3G[[i]] <- getRotationTagGroup(tags,306) # Integer Card 3G
          newData$Rotation.Integer4G[[i]] <- getRotationTagGroup(tags,307) # Integer Card 4G
          newData$Rotation.Integer1B[[i]] <- getRotationTagGroup(tags,308) # Integer Card 1B
          newData$Rotation.Integer2B[[i]] <- getRotationTagGroup(tags,309) # Integer Card 2B
          newData$Rotation.Integer3B[[i]] <- getRotationTagGroup(tags,310) # Integer Card 3B
          newData$Rotation.Integer4B[[i]] <- getRotationTagGroup(tags,311) # Integer Card 4B
          newData$Rotation.Integer1P[[i]] <- getRotationTagGroup(tags,312) # Integer Card 1P
          newData$Rotation.Integer2P[[i]] <- getRotationTagGroup(tags,313) # Integer Card 2P
          newData$Rotation.Integer3P[[i]] <- getRotationTagGroup(tags,314) # Integer Card 3P
          newData$Rotation.Integer4P[[i]] <- getRotationTagGroup(tags,315) # Integer Card 4P
          
          newData$Rotation.Go[[i]] <- getRotationTagGroup(tags,341:344) # Go! Card
          
          newData$Rotation.DiscreteHint[[i]] <- getRotationTagGroup(tags,336) # Discrete hint card
          newData$Rotation.FractionHint[[i]] <- getRotationTagGroup(tags,337) # Fraction hint card
          newData$Rotation.CircularHint[[i]] <- getRotationTagGroup(tags,340) # Circular hint card
          newData$Rotation.RectangularHint[[i]] <- getRotationTagGroup(tags,339) # Rectangular hint card
          newData$Rotation.DecimalHint[[i]] <- getRotationTagGroup(tags,338) # Decimal hint card
          
          newData$Rotation.Carte1[[i]] <- getRotationTagGroup(tags,409) # Carte 1 card
          newData$Rotation.Carte2[[i]] <- getRotationTagGroup(tags,410) # Carte 2 card
          newData$Rotation.Carte3[[i]] <- getRotationTagGroup(tags,411) # Carte 3 card
          newData$Rotation.Carte4[[i]] <- getRotationTagGroup(tags,412) # Carte 4 card
          newData$Rotation.Carte5[[i]] <- getRotationTagGroup(tags,413) # Carte 5 card
          newData$Rotation.Carte6[[i]] <- getRotationTagGroup(tags,414) # Carte 6 card
          newData$Rotation.Carte7[[i]] <- getRotationTagGroup(tags,415) # Carte 7 card
          newData$Rotation.Carte8[[i]] <- getRotationTagGroup(tags,416) # Carte 8 card
          newData$Rotation.Carte9[[i]] <- getRotationTagGroup(tags,417) # Carte 9 card
          newData$Rotation.Carte10[[i]] <- getRotationTagGroup(tags,418) # Carte 10 card
          
          # For token and abstract fractions, we get the numerator and denominator, or NA if not present
          newData$Num.Token1[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,1) # Tokens in Quadrant 1
          newData$Num.Token2[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,2) # Tokens in Quadrant 2
          newData$Num.Token3[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,3) # Tokens in Quadrant 3
          newData$Num.Token4[[i]] <- getNumTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,4) # Tokens in Quadrant 4
          newData$Den.Token1[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,1) # Tokens in Quadrant 1
          newData$Den.Token2[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,2) # Tokens in Quadrant 2
          newData$Den.Token3[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,3) # Tokens in Quadrant 3
          newData$Den.Token4[[i]] <- getDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,4) # Tokens in Quadrant 4
          
          newData$Num.Fraction12[[i]] <- getNumFraction(tags,345,1,2) # Fraction Card 1/2
          newData$Num.Fraction13[[i]] <- getNumFraction(tags,346,1,3) # Fraction Card 1/3
          newData$Num.Fraction23[[i]] <- getNumFraction(tags,347,2,3) # Fraction Card 2/3
          newData$Num.Fraction14[[i]] <- getNumFraction(tags,348,1,4) # Fraction Card 1/4
          newData$Num.Fraction24[[i]] <- getNumFraction(tags,349,2,4) # Fraction Card 2/4
          newData$Num.Fraction34[[i]] <- getNumFraction(tags,350,3,4) # Fraction Card 3/4
          newData$Num.Fraction15[[i]] <- getNumFraction(tags,351,1,5) # Fraction Card 1/5
          newData$Num.Fraction25[[i]] <- getNumFraction(tags,352,2,5) # Fraction Card 2/5
          newData$Num.Fraction35[[i]] <- getNumFraction(tags,353,3,5) # Fraction Card 3/5
          newData$Num.Fraction45[[i]] <- getNumFraction(tags,354,4,5) # Fraction Card 4/5
          newData$Num.Fraction16[[i]] <- getNumFraction(tags,355,1,6) # Fraction Card 1/6
          newData$Num.Fraction26[[i]] <- getNumFraction(tags,356,2,6) # Fraction Card 2/6
          newData$Num.Fraction36[[i]] <- getNumFraction(tags,357,3,6) # Fraction Card 3/6
          newData$Num.Fraction46[[i]] <- getNumFraction(tags,358,4,6) # Fraction Card 4/6
          newData$Num.Fraction56[[i]] <- getNumFraction(tags,359,5,6) # Fraction Card 5/6
          newData$Num.Fraction110[[i]] <- getNumFraction(tags,360,1,10) # Fraction Card 1/10
          newData$Num.Fraction210[[i]] <- getNumFraction(tags,361,2,10) # Fraction Card 2/10
          newData$Num.Fraction310[[i]] <- getNumFraction(tags,362,3,10) # Fraction Card 3/10
          newData$Num.Fraction410[[i]] <- getNumFraction(tags,363,4,10) # Fraction Card 4/10
          newData$Num.Fraction510[[i]] <- getNumFraction(tags,364,5,10) # Fraction Card 5/10
          newData$Num.Fraction610[[i]] <- getNumFraction(tags,365,6,10) # Fraction Card 6/10
          newData$Num.Fraction710[[i]] <- getNumFraction(tags,366,7,10) # Fraction Card 7/10
          newData$Num.Fraction810[[i]] <- getNumFraction(tags,367,8,10) # Fraction Card 8/10
          newData$Num.Fraction910[[i]] <- getNumFraction(tags,368,9,10) # Fraction Card 9/10
          
          newData$Den.Fraction12[[i]] <- getDenFraction(tags,345,1,2) # Fraction Card 1/2
          newData$Den.Fraction13[[i]] <- getDenFraction(tags,346,1,3) # Fraction Card 1/3
          newData$Den.Fraction23[[i]] <- getDenFraction(tags,347,2,3) # Fraction Card 2/3
          newData$Den.Fraction14[[i]] <- getDenFraction(tags,348,1,4) # Fraction Card 1/4
          newData$Den.Fraction24[[i]] <- getDenFraction(tags,349,2,4) # Fraction Card 2/4
          newData$Den.Fraction34[[i]] <- getDenFraction(tags,350,3,4) # Fraction Card 3/4
          newData$Den.Fraction15[[i]] <- getDenFraction(tags,351,1,5) # Fraction Card 1/5
          newData$Den.Fraction25[[i]] <- getDenFraction(tags,352,2,5) # Fraction Card 2/5
          newData$Den.Fraction35[[i]] <- getDenFraction(tags,353,3,5) # Fraction Card 3/5
          newData$Den.Fraction45[[i]] <- getDenFraction(tags,354,4,5) # Fraction Card 4/5
          newData$Den.Fraction16[[i]] <- getDenFraction(tags,355,1,6) # Fraction Card 1/6
          newData$Den.Fraction26[[i]] <- getDenFraction(tags,356,2,6) # Fraction Card 2/6
          newData$Den.Fraction36[[i]] <- getDenFraction(tags,357,3,6) # Fraction Card 3/6
          newData$Den.Fraction46[[i]] <- getDenFraction(tags,358,4,6) # Fraction Card 4/6
          newData$Den.Fraction56[[i]] <- getDenFraction(tags,359,5,6) # Fraction Card 5/6
          newData$Den.Fraction110[[i]] <- getDenFraction(tags,360,1,10) # Fraction Card 1/10
          newData$Den.Fraction210[[i]] <- getDenFraction(tags,361,2,10) # Fraction Card 2/10
          newData$Den.Fraction310[[i]] <- getDenFraction(tags,362,3,10) # Fraction Card 3/10
          newData$Den.Fraction410[[i]] <- getDenFraction(tags,363,4,10) # Fraction Card 4/10
          newData$Den.Fraction510[[i]] <- getDenFraction(tags,364,5,10) # Fraction Card 5/10
          newData$Den.Fraction610[[i]] <- getDenFraction(tags,365,6,10) # Fraction Card 6/10
          newData$Den.Fraction710[[i]] <- getDenFraction(tags,366,7,10) # Fraction Card 7/10
          newData$Den.Fraction810[[i]] <- getDenFraction(tags,367,8,10) # Fraction Card 8/10
          newData$Den.Fraction910[[i]] <- getDenFraction(tags,368,9,10) # Fraction Card 9/10
          
          
          #For continuous fractions, we get the fraction value (as a decimal number)
          newData$Value.C1[[i]] <- getContinuousValue(tags,316:320,"C") # Continuous Circular 1 set
          newData$Value.C2[[i]] <- getContinuousValue(tags,321:325,"C") # Continuous Circular 2 set
          newData$Value.R1[[i]] <- getContinuousValue(tags,326:330,"R") # Continuous Rectangular 1 set
          newData$Value.R2[[i]] <- getContinuousValue(tags,331:335,"R") # Continuous Rectangular 2 set
          
          
          # We get the quadrant of each tag group, returned as characters "0" "Q1" "Q2" "Q3" "Q4" (to be later converted into factors)
          newData$C1[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,316:320) # Continuous Circular 1 set
          newData$C2[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,321:325) # Continuous Circular 2 set
          newData$R1[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,326:330) # Continuous Rectangular 1 set
          newData$R2[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,331:335) # Continuous Rectangular 2 set
          
          newData$Token1[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,1) # Tokens in Quadrant 1
          newData$Token2[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,2) # Tokens in Quadrant 2
          newData$Token3[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,3) # Tokens in Quadrant 3
          newData$Token4[[i]] <- getTokensInQuadrant(tags,displayWidth,displayHeight,369:408,4) # Tokens in Quadrant 4
          
          newData$Fraction12[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,345) # Fraction Card 1/2
          newData$Fraction13[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,346) # Fraction Card 1/3
          newData$Fraction23[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,347) # Fraction Card 2/3
          newData$Fraction14[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,348) # Fraction Card 1/4
          newData$Fraction24[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,349) # Fraction Card 2/4
          newData$Fraction34[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,350) # Fraction Card 3/4
          newData$Fraction15[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,351) # Fraction Card 1/5
          newData$Fraction25[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,352) # Fraction Card 2/5
          newData$Fraction35[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,353) # Fraction Card 3/5
          newData$Fraction45[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,354) # Fraction Card 4/5
          newData$Fraction16[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,355) # Fraction Card 1/6
          newData$Fraction26[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,356) # Fraction Card 2/6
          newData$Fraction36[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,357) # Fraction Card 3/6
          newData$Fraction46[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,358) # Fraction Card 4/6
          newData$Fraction56[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,359) # Fraction Card 5/6
          newData$Fraction110[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,360) # Fraction Card 1/10
          newData$Fraction210[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,361) # Fraction Card 2/10
          newData$Fraction310[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,362) # Fraction Card 3/10
          newData$Fraction410[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,363) # Fraction Card 4/10
          newData$Fraction510[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,364) # Fraction Card 5/10
          newData$Fraction610[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,365) # Fraction Card 6/10
          newData$Fraction710[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,366) # Fraction Card 7/10
          newData$Fraction810[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,367) # Fraction Card 8/10
          newData$Fraction910[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,368) # Fraction Card 9/10
          
          newData$Integer1R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,300) # Integer Card 1R
          newData$Integer2R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,301) # Integer Card 2R
          newData$Integer3R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,302) # Integer Card 3R
          newData$Integer4R[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,303) # Integer Card 4R
          newData$Integer1G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,304) # Integer Card 1G
          newData$Integer2G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,305) # Integer Card 2G
          newData$Integer3G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,306) # Integer Card 3G
          newData$Integer4G[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,307) # Integer Card 4G
          newData$Integer1B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,308) # Integer Card 1B
          newData$Integer2B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,309) # Integer Card 2B
          newData$Integer3B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,310) # Integer Card 3B
          newData$Integer4B[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,311) # Integer Card 4B
          newData$Integer1P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,312) # Integer Card 1P
          newData$Integer2P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,313) # Integer Card 2P
          newData$Integer3P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,314) # Integer Card 3P
          newData$Integer4P[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,315) # Integer Card 4P
          
          newData$Go[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,341:344) # Go! Card
          
          newData$DiscreteHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,336) # Discrete hint card
          newData$FractionHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,337) # Fraction hint card
          newData$CircularHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,340) # Circular hint card
          newData$RectangularHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,339) # Rectangular hint card
          newData$DecimalHint[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,338) # Decimal hint card
          
          newData$Carte1[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,409) # Carte 1 card
          newData$Carte2[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,410) # Carte 2 card
          newData$Carte3[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,411) # Carte 3 card
          newData$Carte4[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,412) # Carte 4 card
          newData$Carte5[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,413) # Carte 5 card
          newData$Carte6[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,414) # Carte 6 card
          newData$Carte7[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,415) # Carte 7 card
          newData$Carte8[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,416) # Carte 8 card
          newData$Carte9[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,417) # Carte 9 card
          newData$Carte10[[i]] <- getQuadrantTagGroup(tags,displayWidth,displayHeight,418) # Carte 10 card
          
          
          
          
          
          
        }# End if NA
        
        
      }# End log entry for
      
      if(nrow(totalData)==0) totalData <- newData
      else totalData <- rbind(totalData,newData)
      
    }# End log file for 
    
  }# End lamp dir for
  
  totalData <- totalData[!is.na(totalData$Group.Name),]
  
  #We merge manually the two composite group data, and write it to a file again
  #   cat ("Doing manual join of groups fragmented across lamps\n")
  #   s2g3a <- get(load('S2G3a.rda')) 
  #   s2g3b <- get(load('S2G3b.rda'))
  #   totalData <- rbind(s2g3a,s2g3b) 
  #   save(totalData,file="S2G3.rda",compress=TRUE)
  #   
  #   # s2g4a <- get(load('S2G4a.rda')) # From this part, we lost the logs!
  #   totalData <- get(load('S2G4b.rda'))
  #   # totalData <- rbind(s2g4a,s2g4b) 
  #   save(totalData,file="S2G4.rda",compress=TRUE)
  
  
  # Now totalData has all the logs
  setwd(rootDir)
  save(totalData,file="Logs.rda",compress=TRUE)
  
  
  cat ("Process finished! Check your .rda files")
  
}

# getGroupFromTimestamp - gets the character string for a group, given a (numeric, in ms) timestamp and a table with the timings for that lamp, 
# or NA if no group has that timestamp
getGroupFromTimestamp <- function(timings, timestamp){
  
  group <- timings[timings$start <= timestamp & timings$end >= timestamp,"Code"]
  
  if(length(group)==0) return(NA)
  else if(length(group)>1) return(NA)
  else return(group)
  
}


