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
      newData$Position.C1[[i]] <- getPositionTagGroup(tags,316:320) # Continuous Circular 1 set
      newData$Position.C2[[i]] <- getPositionTagGroup(tags,321:325) # Continuous Circular 2 set
      newData$Position.R1[[i]] <- getPositionTagGroup(tags,326:330) # Continuous Rectangular 1 set
      newData$Position.R2[[i]] <- getPositionTagGroup(tags,331:335) # Continuous Rectangular 2 set
      
      newData$Position.Token1[[i]] <- getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,1) # Tokens in Quadrant 1
      newData$Position.Token2[[i]] <- getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,2) # Tokens in Quadrant 2
      newData$Position.Token3[[i]] <- getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,3) # Tokens in Quadrant 3
      newData$Position.Token4[[i]] <- getPositionTokensInQuadrant(tags,displayWidth,displayHeight,369:408,4) # Tokens in Quadrant 4
      
      newData$Position.Fraction12[[i]] <- getPositionTagGroup(tags,345) # Fraction Card 1/2
      newData$Position.Fraction13[[i]] <- getPositionTagGroup(tags,346) # Fraction Card 1/3
      newData$Position.Fraction23[[i]] <- getPositionTagGroup(tags,347) # Fraction Card 2/3
      newData$Position.Fraction14[[i]] <- getPositionTagGroup(tags,348) # Fraction Card 1/4
      newData$Position.Fraction24[[i]] <- getPositionTagGroup(tags,349) # Fraction Card 2/4
      newData$Position.Fraction34[[i]] <- getPositionTagGroup(tags,350) # Fraction Card 3/4
      newData$Position.Fraction15[[i]] <- getPositionTagGroup(tags,351) # Fraction Card 1/5
      newData$Position.Fraction25[[i]] <- getPositionTagGroup(tags,352) # Fraction Card 2/5
      newData$Position.Fraction35[[i]] <- getPositionTagGroup(tags,353) # Fraction Card 3/5
      newData$Position.Fraction45[[i]] <- getPositionTagGroup(tags,354) # Fraction Card 4/5
      newData$Position.Fraction16[[i]] <- getPositionTagGroup(tags,355) # Fraction Card 1/6
      newData$Position.Fraction26[[i]] <- getPositionTagGroup(tags,356) # Fraction Card 2/6
      newData$Position.Fraction36[[i]] <- getPositionTagGroup(tags,357) # Fraction Card 3/6
      newData$Position.Fraction46[[i]] <- getPositionTagGroup(tags,358) # Fraction Card 4/6
      newData$Position.Fraction56[[i]] <- getPositionTagGroup(tags,359) # Fraction Card 5/6
      newData$Position.Fraction110[[i]] <- getPositionTagGroup(tags,360) # Fraction Card 1/10
      newData$Position.Fraction210[[i]] <- getPositionTagGroup(tags,361) # Fraction Card 2/10
      newData$Position.Fraction310[[i]] <- getPositionTagGroup(tags,362) # Fraction Card 3/10
      newData$Position.Fraction410[[i]] <- getPositionTagGroup(tags,363) # Fraction Card 4/10
      newData$Position.Fraction510[[i]] <- getPositionTagGroup(tags,364) # Fraction Card 5/10
      newData$Position.Fraction610[[i]] <- getPositionTagGroup(tags,365) # Fraction Card 6/10
      newData$Position.Fraction710[[i]] <- getPositionTagGroup(tags,366) # Fraction Card 7/10
      newData$Position.Fraction810[[i]] <- getPositionTagGroup(tags,367) # Fraction Card 8/10
      newData$Position.Fraction910[[i]] <- getPositionTagGroup(tags,368) # Fraction Card 9/10
      
      newData$Position.Integer1R[[i]] <- getPositionTagGroup(tags,300) # Integer Card 1R
      newData$Position.Integer2R[[i]] <- getPositionTagGroup(tags,301) # Integer Card 2R
      newData$Position.Integer3R[[i]] <- getPositionTagGroup(tags,302) # Integer Card 3R
      newData$Position.Integer4R[[i]] <- getPositionTagGroup(tags,303) # Integer Card 4R
      newData$Position.Integer1G[[i]] <- getPositionTagGroup(tags,304) # Integer Card 1G
      newData$Position.Integer2G[[i]] <- getPositionTagGroup(tags,305) # Integer Card 2G
      newData$Position.Integer3G[[i]] <- getPositionTagGroup(tags,306) # Integer Card 3G
      newData$Position.Integer4G[[i]] <- getPositionTagGroup(tags,307) # Integer Card 4G
      newData$Position.Integer1B[[i]] <- getPositionTagGroup(tags,308) # Integer Card 1B
      newData$Position.Integer2B[[i]] <- getPositionTagGroup(tags,309) # Integer Card 2B
      newData$Position.Integer3B[[i]] <- getPositionTagGroup(tags,310) # Integer Card 3B
      newData$Position.Integer4B[[i]] <- getPositionTagGroup(tags,311) # Integer Card 4B
      newData$Position.Integer1P[[i]] <- getPositionTagGroup(tags,312) # Integer Card 1P
      newData$Position.Integer2P[[i]] <- getPositionTagGroup(tags,313) # Integer Card 2P
      newData$Position.Integer3P[[i]] <- getPositionTagGroup(tags,314) # Integer Card 3P
      newData$Position.Integer4P[[i]] <- getPositionTagGroup(tags,315) # Integer Card 4P
      
      newData$Position.Go[[i]] <- getPositionTagGroup(tags,341:344) # Go! Card
      
      newData$Position.DiscreteHint[[i]] <- getPositionTagGroup(tags,336) # Discrete hint card
      newData$Position.FractionHint[[i]] <- getPositionTagGroup(tags,337) # Fraction hint card
      newData$Position.CircularHint[[i]] <- getPositionTagGroup(tags,340) # Circular hint card
      newData$Position.RectangularHint[[i]] <- getPositionTagGroup(tags,339) # Rectangular hint card
      newData$Position.DecimalHint[[i]] <- getPositionTagGroup(tags,338) # Decimal hint card
      
      newData$Position.Carte1[[i]] <- getPositionTagGroup(tags,409) # Carte 1 card
      newData$Position.Carte2[[i]] <- getPositionTagGroup(tags,410) # Carte 2 card
      newData$Position.Carte3[[i]] <- getPositionTagGroup(tags,411) # Carte 3 card
      newData$Position.Carte4[[i]] <- getPositionTagGroup(tags,412) # Carte 4 card
      newData$Position.Carte5[[i]] <- getPositionTagGroup(tags,413) # Carte 5 card
      newData$Position.Carte6[[i]] <- getPositionTagGroup(tags,414) # Carte 6 card
      newData$Position.Carte7[[i]] <- getPositionTagGroup(tags,415) # Carte 7 card
      newData$Position.Carte8[[i]] <- getPositionTagGroup(tags,416) # Carte 8 card
      newData$Position.Carte9[[i]] <- getPositionTagGroup(tags,417) # Carte 9 card
      newData$Position.Carte10[[i]] <- getPositionTagGroup(tags,418) # Carte 10 card
      
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
      
      #TODO: For token and abstract fractions, we get the numerator and denominator, or NA if not present
      newData$NumDem.Token1[[i]] <- getNumDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,1) # Tokens in Quadrant 1
      newData$NumDem.Token2[[i]] <- getNumDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,2) # Tokens in Quadrant 2
      newData$NumDem.Token3[[i]] <- getNumDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,3) # Tokens in Quadrant 3
      newData$NumDem.Token4[[i]] <- getNumDenTokensInQuadrant(tags,displayWidth,displayHeight,369:408,369:388,4) # Tokens in Quadrant 4
      
      newData$NumDem.Fraction12[[i]] <- getNumDenFraction(tags,345,1,2) # Fraction Card 1/2
      newData$NumDem.Fraction13[[i]] <- getNumDenFraction(tags,346,1,3) # Fraction Card 1/3
      newData$NumDem.Fraction23[[i]] <- getNumDenFraction(tags,347,2,3) # Fraction Card 2/3
      newData$NumDem.Fraction14[[i]] <- getNumDenFraction(tags,348,1,4) # Fraction Card 1/4
      newData$NumDem.Fraction24[[i]] <- getNumDenFraction(tags,349,2,4) # Fraction Card 2/4
      newData$NumDem.Fraction34[[i]] <- getNumDenFraction(tags,350,3,4) # Fraction Card 3/4
      newData$NumDem.Fraction15[[i]] <- getNumDenFraction(tags,351,1,5) # Fraction Card 1/5
      newData$NumDem.Fraction25[[i]] <- getNumDenFraction(tags,352,2,5) # Fraction Card 2/5
      newData$NumDem.Fraction35[[i]] <- getNumDenFraction(tags,353,3,5) # Fraction Card 3/5
      newData$NumDem.Fraction45[[i]] <- getNumDenFraction(tags,354,4,5) # Fraction Card 4/5
      newData$NumDem.Fraction16[[i]] <- getNumDenFraction(tags,355,1,6) # Fraction Card 1/6
      newData$NumDem.Fraction26[[i]] <- getNumDenFraction(tags,356,2,6) # Fraction Card 2/6
      newData$NumDem.Fraction36[[i]] <- getNumDenFraction(tags,357,3,6) # Fraction Card 3/6
      newData$NumDem.Fraction46[[i]] <- getNumDenFraction(tags,358,4,6) # Fraction Card 4/6
      newData$NumDem.Fraction56[[i]] <- getNumDenFraction(tags,359,5,6) # Fraction Card 5/6
      newData$NumDem.Fraction110[[i]] <- getNumDenFraction(tags,360,1,10) # Fraction Card 1/10
      newData$NumDem.Fraction210[[i]] <- getNumDenFraction(tags,361,2,10) # Fraction Card 2/10
      newData$NumDem.Fraction310[[i]] <- getNumDenFraction(tags,362,3,10) # Fraction Card 3/10
      newData$NumDem.Fraction410[[i]] <- getNumDenFraction(tags,363,4,10) # Fraction Card 4/10
      newData$NumDem.Fraction510[[i]] <- getNumDenFraction(tags,364,5,10) # Fraction Card 5/10
      newData$NumDem.Fraction610[[i]] <- getNumDenFraction(tags,365,6,10) # Fraction Card 6/10
      newData$NumDem.Fraction710[[i]] <- getNumDenFraction(tags,366,7,10) # Fraction Card 7/10
      newData$NumDem.Fraction810[[i]] <- getNumDenFraction(tags,367,8,10) # Fraction Card 8/10
      newData$NumDem.Fraction910[[i]] <- getNumDenFraction(tags,368,9,10) # Fraction Card 9/10
      
      
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
      
      # TODO: Optionally, we can add some time measurement represented by this value? but then what do we do with interruptions/gaps?
    
    }

    # We add the clean logs to this group's table
    if(length(totalData$timestamp)==0) totalData <- newData
    else totalData <- rbind(totalData, newData)
  
  }
  
  #We convert the character fields to factors
  #totalData[sapply(totalData, is.character)] <- lapply(totalData[sapply(totalData, is.character)], as.factor)
  if(length(totalData$timestamp)!=0){
    
    #TODO: Modify this if we add more fields that are not factors/quadrants
    for(i in 2:length(totalData)) totalData[i][[1]] <- factor(totalData[i][[1]],levels=c("0","Q1","Q2","Q3","Q4"))
    
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

# Returns a vector with the numerator and denominator of a fraction card if it is present in the table, or NA if it's not present
getNumDenFraction <- function(tags,targetTags,numerator,denominator){
  
  # This is the variable that will store the tag group's center
  fraction <- list()
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  
  # if none of the tags of this tangible is present, return 0
  if(length(presentTags$id)==0) return(NA)
  else{ #Some tags are present, let's calculate the centroid
    fraction <- list(num=integer(numerator),den=integer(denominator))
  }
  
  fraction
}

# Returns a vector with the numerator (number of yellow tokens) and denominator (number of tokens) of a group of tokens IN A QUADRANT, or NA if it's not present
getNumDenTokensInQuadrant <- function(tags,displayWidth,displayHeight,targetTags,numeratorTags,quadrantToCheck){
  
  # This is the variable that will store the tag group's center
  numerator <- 0
  denominator <- 0
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  presentNumTags <- tags[tags$id %in% numeratorTags,]
  
  # if none of the tokens is present IN THE WHOLE TABLE, return NA
  if(length(presentTags$id)==0) return(NA)
  else{ #Some tags are present, let's calculate which quadrants they're on
    centers <- sapply(presentTags$corners,getCenter) #this is an 2xn matrix with all the tokens center coordinates
    centersNum <- sapply(numeratorTags$corners,getCenter) #this is an 2xn matrix with the yellow tokens center coordinates
    # We restrict our focus to only one quadrant's centers
    if(quadrantToCheck==1){
      centers <- as.matrix(centers[,(centers[1,] >= width/2 & centers[2,] < height/2)])
      centersNum <- as.matrix(centersNum[,(centersNum[1,] >= width/2 & centersNum[2,] < height/2)])
    }else if(quadrantToCheck==2){
      centers <- as.matrix(centers[,(centers[1,] < width/2 & centers[2,] < height/2)])
      centersNum <- as.matrix(centersNum[,(centersNum[1,] < width/2 & centersNum[2,] < height/2)])
    }else if(quadrantToCheck==3){
      centers <- as.matrix(centers[,(centers[1,] < width/2 & centers[2,] >= height/2)])
      centersNum <- as.matrix(centersNum[,(centersNum[1,] < width/2 & centersNum[2,] >= height/2)])
    }else if(quadrantToCheck==4){
      centers <- as.matrix(centers[,(centers[1,] >= width/2 & centers[2,] >= height/2)])
      centersNum <- as.matrix(centersNum[,(centersNum[1,] >= width/2 & centersNum[2,] >= height/2)])
    }
  }
  
  # We Get the number of concerned tags in our quadrant
  if(length(centers)==0) return(NA)
  else{
    denominator <- integer(dim(centers)[[2]])
    numerator <- integer(dim(centersNum)[[2]])
  }
  
  return(list(num=numerator,den=denominator))
  
}


# getQuadrantTagGroup - this function gets a dataframe with the present tags and their 
# coordinates in a certain point in time, and returns the centroid of the token tags
# WITHIN A CERTAIN QUADRANT (since a set of tokens can be distributed in all four quadrants),
# responding with a numeric vector (x,y) or NA if no tokens are on that quadrant
# Parameters: tags is the data.frame with the tag ids and their corners; width and height are the size of the display, 
# which define the position of the quadrants, targetTags is the group of tags that make up the 
# global token taggroup, quadrantToCheck is the quadrant on which we are focusing
getPositionTokensInQuadrant <- function(tags,displayWidth,displayHeight,targetTags,quadrantToCheck){
  
  # This is the variable that will store the tag group's center
  position <- list()
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  
  # if none of the tokens is present IN THE WHOLE TABLE, return 0
  if(length(presentTags$id)==0) return(NA)
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
  if(length(centers)==0) return(NA)
  else position <- rowMeans(centers)
  
  return(list(x=position[[1]],y=position[[2]]))
}

# getPositionTagGroup - Given a set of detected tags (a data.frame), and a subgroup of all the tag ids to detect (a vector),
# this function returns the centroid of this tag/tag group (a vector with two named numbers, the x-position and y-position in the screen)
# or NA if the target tags are not present
getPositionTagGroup <- function(tags,targetTags){
  
  # This is the variable that will store the tag group's center
  position <- list()
  
  # Which tags from this set are present
  presentTags <- tags[tags$id %in% targetTags,]
  
  # if none of the tags of this tangible is present, return 0
  if(length(presentTags$id)==0) return(NA)
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
  x1 <- listCorners[[1]]$corners[[1]]
  y1 <- listCorners[[1]]$corners[[2]]
  x2 <- listCorners[[1]]$corners[[3]]
  y2 <- listCorners[[1]]$corners[[4]]
  
  rightVector <- c(x = x2 - x1, y = y2 - y1)
    
  if(rightVectorX > 0 && rightVectorY > 0)    rotation <- atan(y/x) #Case 1: first cuadrant
  else if (rightVector.x < 0 && rightVector.y > 0)  rotation <- atan(-x/y) + pi/2 #Case 2: second cuadrant
  else if (rightVector.x < 0 && rightVector.y < 0)  rotation <- atan(y/x) + pi #Case 3: third cuadrant
  else if (rightVector.x > 0 && rightVector.y < 0)  rotation <- atan(-x/y) + 3*pi/2 #Case 4: fourth cuadrant
  else if (rightVector.x == 0 && rightVector.y > 0) rotation <- pi/2 #Case 5: Y axis (+ direction) -> 90
  else if (rightVector.x < 0 && rightVector.y == 0) rotation <- pi #Case 6: X axis (- direction) -> 180
  else if (rightVector.x == 0 && rightVector.y < 0) rotation <- 3*pi/2 #Case 7: Y axis (- direction) -> 270
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
  
  # if none of the tags of this tangible is present, or the center is not present, return NA
  if(length(presentTags$id)==0 || length(presentCenter$id)==0) return(NA)
  else{ #Some tags are present, let's calculate the centroid

    # If we can't see the first and second tag of the manipulative, then return NA.

    tag1Id <- presentTags[[1]]$id
    tag2Id <- presentTags[[2]]$id
    
      if(contType=="R"){
        
        # First manipulative: tag 1 <- 326, tag 2 <- 327
        # Second manipulative: tag 1 <- 331, tag 2 <- 332
        if(( tag1Id == 326 || tag1Id == 331 ) && ( tag2Id == 327 || tag2Id == 332 )){
          
          value <- 0 # Do whatever with the here to calculate the rectangular value
          
          #The corner [0] of the tag [0] of the marker rectangle_origin (is a point2d)
          originPoint <- c( x = presentTags[[1]]$corners[[1]], y =  presentTags[[1]]$corners[[2]]) 
          
          #The the corner [0] of the tag ofthe marker rectangle_control (is a point2d)
          endPoint <- c(x = presentCenter$corners[[1]], y = presentCenter$corners[[2]])
          
          #The corner [0] of the tag [1] of the marker rectangle_origin (is a point2d)
          startPoint <- c(x = presentTags[[2]]$corners[[1]], y = presentTags[[2]]$corners[[2]])
            
          #Calculate the angle
          angle <- getVertexAngle(startPoint, originPoint, endPoint)
          
          #We calculate the distance between the two tags
          totalDistance <- getDistance (originPoint, startPoint)
          projectedDistance <- getDistance (originPoint, endPoint)*cos(angle)
          
          value <- projectedDistance / totalDistance
        } 
        else{
          return (NA)
        }
      }
        
      else if(contType=="C"){ #We need at least 3 tags, to calculate the center of the circunference
        
        # First manipulative: tag 1 <- 316, tag 2 <- 317
        # Second manipulative: tag 1 <- 321, tag 2 <- 322
        if(( tag1Id == 316 || tag1Id == 321 ) && ( tag2Id == 317 || tag2Id == 322 ) && length(presentTags$id) > 2){
          
          #Since we already know that tag 1 and 2 are present, the next tag will help us with the Y coordinate of the center of the marker
          value <- 0 # Do whatever with the here to calculate the circular value
          
          x1 <- mean(presentTags[[1]]$corners[[1]] , presentTags[[1]]$corners[[3]]) #Center (x) of the first tag 
          y1 <- mean(presentTags[[1]]$corners[[4]] , presentTags[[1]]$corners[[6]]) #Center (y) of the first tag
          
          x2 <- mean(presentTags[[2]]$corners[[1]] , presentTags[[2]]$corners[[3]]) #Center (x) of the second tag 
          y2 <- mean(presentTags[[2]]$corners[[4]] , presentTags[[2]]$corners[[6]]) #Center (y) of the second tag
          
          x3 <- mean(presentTags[[3]]$corners[[1]] , presentTags[[3]]$corners[[3]]) #Center (x) of the third tag 
          y3 <- mean(presentTags[[3]]$corners[[4]] , presentTags[[3]]$corners[[6]]) #Center (y) of the third tag
          
          #This is the center of the marker circular_origin (is a point2d)
          originPoint <- c(x= mean(x1,x2), y= mean(y1,y3))
          
          #This is the center of the marker circunf_control (is a point2d)  
          endPoint <- c(x = mean(presentCenter$corners[[1]],presentCenter$corners[[3]]),y = mean(presentCenter$corners[[4]],presentCenter$corners[[6]]))
          
          #Centroid of the tag [0] and [1] of the circular_origin marker (is a point2d)
          startPoint <- c(x = mean(x1,x2) , y = mean(y1,y2)) 
            
            
          angle <- getVertexAngle(startPoint, originPoint, endPoint) #Check the parameters
          
          value <- 1 - (angle/(2*pi))
        }
        else{
          return (NA)  
        }
        
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
  
  return(list(x=mean(centers[1,]),y=mean(centers[2,])))
  
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
  x1 <- startPoint.x - originPoint.x
  x3 <- endPoint.x - originPorint.x
  y1 <- startPoint.y - originPoint.y
  y3 <- endPoint.y - originPoint.y
  
  #We calculate the distance
  distance <- ((x1 * x1) + (y1 * y1))*((x3 * x3) + (y3 * y3))
  
  if(distance == 0) return (0)
  else{
    input <- (x1*x3 + y1*y3)/sqrt(dist)
  
    if(input == 1)  return (0)
    else if (input == -1) return (pi)
    else  return(acos(input))
  }
}

#getDistance - gets the distance between two points (x1,y1) and (x2,y2)
getDistance <- function(point1, point2){
  
  x = point2.x - point1.x
  y = point2.y - point2.y
  
  return (sqrt(x * x + y * y))
}