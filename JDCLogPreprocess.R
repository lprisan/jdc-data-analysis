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
      
      # Optionally, we can add some time measurement represented by this value? but then what do we do with interruptions/gaps?
    
    }

    # We add the clean logs to this group's table
    if(length(totalData$timestamp)==0) totalData <- newData
    else totalData <- rbind(totalData, newData)
  
  }
  
  #We convert the character fields to factors
  #totalData[sapply(totalData, is.character)] <- lapply(totalData[sapply(totalData, is.character)], as.factor)
  if(length(totalData$timestamp)!=0){
    for(i in 2:length(totalData)) totalData[i][[1]] <- factor(totalData[i][[1]],levels=c("0","Q1","Q2","Q3","Q4"))
    
    cat (paste("Finished processing for group ",label,". Writing",as.character(length(totalData$timestamp)),"cleaned log entries to file\n"))
    # Go back to the original current dir
    setwd(originalDir)
    
    # serialize() the clean object into a binary R file, and also into xlsx?
    save(totalData,file=paste(label,".rda",sep=""),compress=TRUE)
  }else{
    # Go back to the original current dir
    setwd(originalDir)

    cat (paste("Finished processing for group ",label,". No log entries to write!\n"))
  }
       
  
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
  position <- c(0,0)
  
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
  
  if(length(listCorners)==0) return(c(0,0))
  
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

