# preprocessJDCEyetrack - Clean and organize the eyetracking data (get only the fields that are important for us)
# This is the overall function in charge of the preprocessing and cleaning of the eyetracker event data from the JDC experiment
# Parameters: rootDir the directory in which the logs to be merged/split are, named JDC-...-eyetracking-eventexport.txt
preprocessJDCEyetrack <- function(rootDir){
  
  setwd(rootDir)
  
  files <- list.files(pattern = "\\-eyetracking-eventexport.txt$")
  
  totaldata <- FALSE
  
  for(file in files){
    filedata <- read.csv(file,comment.char="#")
    
    # we select the meaningful columns (for now, only pupil diameter)
    filedata <- filedata[c(1,6,9)]
    # TODO: add other measures?
    
    # We extract the session number from the filename, and create a variable for it
    index <- regexpr("Session",file,fixed=T)
    filedata$Session <- substr(file,index+7,index+7)
    
    if(length(totaldata) == 1) totaldata <- data.frame(filedata) # This is the first file
    else totaldata <- rbind(totaldata,filedata) # The global data frame exists, just add to it
  }
  
  save(totaldata,file="Eyetracker.rda",compress=TRUE)
  
  cat ("Process finished! Check your .rda files")
  
}
