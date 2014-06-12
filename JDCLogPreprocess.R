# mergeSplitLogFiles - Merge/Split log files
# This function takes log files in JSON, loads them in R data frames, takes those times within a certain limit, and splits the tags 
# present into one observation each, including the x/y position of the center of the tag
# Parameters: directory the directory in which the logs to be merged/split are, startTime and endTime are the limits for this
# group's activities, and label is the name of the group, which will be used when exporting the file into binary form
mergeSplitLogFiles <- function(directory, startTime, endTime, label){
  # We check that the time frontiers - beginning is earlier than end
  if(endTime<startTime) stop("Incorrect start/end times")
  
  # Store the current dir, to come back to it at the end, and change wd to the rootDir
  originalDir <- getwd()
  setwd(directory)
  
  # We loop through all the .log files in the directory
  logFiles <- list.files(pattern = "\\.log$")
  
  for(file in logFiles){
    # We import the JSON file
    jsonData <- fromJSON(file)
    
    # We turn the timestamp into a date/time object in R
    
    # We check whether the time is within the frontiers
    
    # We split the tags and calculate the center of each tag, and create a separate row for each
    
    # Optionally, we can add further data about what each tag means (what kind of representation it is, what quadrant the position is it in)
  }
  
  # serialize() the clean objects into a binary R file, and also into xlsx?
  
  # Go back to the original current dir
  setwd(originalDir)
}

# cleanAndOrganizeJDCLogs - Clean and organize
# This function takes log files in JSON, loads them in R data frames, and splits them and exports them into binary files
# Parameters: rootDir the directory in which the logs to be merged/split are, in folders called "lamp 1", "lamp 2"...
cleanAndOrganizeJDCLogs <- function(rootDir){
  
  # Session B Lamp 1
  
  # We calculate the time limits (for now, based on the expected time slots)
  start <- as.POSIXct(strptime("2014-06-05 10:15:00", "%Y-%m-%d %H:%M:%S"))
  end <- as.POSIXct(strptime("2014-06-05 11:15:00", "%Y-%m-%d %H:%M:%S"))
  
  # We process the data
  mergeSplitLogFiles(paste(rootDir, "lamp 1", sep="/"),start,end,"B1")


  
  
  
  
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
