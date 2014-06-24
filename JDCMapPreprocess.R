# preprocessJDCMaps - Clean and organize the completed maps data from the video analysis
# Parameters: rootDir the directory in which the video analysis summary data is (in the form a csv file)
preprocessJDCMaps <- function(rootDir){
  original <- getwd()
  # Change dir to the rootDir
  setwd(rootDir)
  # load the CSV
  mapSummary <- read.csv("ResumeMaps4.csv")

  # We get the subset of finished maps per group, change its Count to Count.Finished, and remove the Level column
  finSubset <- mapSummary[mapSummary$Level=="FINISH",]
  finSubset$Count.Finished <- finSubset$Count
  finSubset <- finSubset[,c(-2,-3)]

  # We do the same for the unfinished ones
  unfinSubset <- mapSummary[mapSummary$Level=="UNFINISH",]
  unfinSubset$Count.Unfinished <- unfinSubset$Count
  unfinSubset <- unfinSubset[,c(-2,-3)]
  
  # We merge both subsets, and add a Total.Maps, Proportion.Finished column
  data <- merge(finSubset,unfinSubset)
  data$Total.Maps <- (data$Count.Finished+data$Count.Unfinished)
  data$Proportion.Finished <- (data$Count.Finished/data$Total.Maps)
  names(data)[[1]] <- "Group.Name" # We change the name for the Label column, to Group.Name
  
  # save the R data frame
  save(data,file="Maps.rda",compress=TRUE)
  
  # Go back to original dir
  setwd(original)
}