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
  
  # We load the map timings and stats
  mapTimes <- read.csv("SessionTimings.csv",stringsAsFactors=FALSE)
  # We clean up some inconsistencies in the data
  mapTimes <- as.data.frame(lapply(mapTimes, function(x){replace(x, !is.na(x) & x == "UN", "UNFIN")}),stringsAsFactors=F)
  mapTimes <- as.data.frame(lapply(mapTimes, function(x){replace(x, !is.na(x) & x == "", NA)}),stringsAsFactors=F)
  mapTimes[,c(2:31,41:44)] <- lapply(mapTimes[,c(2:31,41:44)],as.numeric)
  mapTimes[,c(1,32:40)] <- lapply(mapTimes[,c(1,32:40)],as.factor)
  
  # calculate the number of finished and unfinished maps in Act4
  mapTimes <- addAct4Performance(mapTimes)
  
  # We merge the total summary data with this more detailed map performance data
  data <- merge(data,mapTimes,by.x="Group.Name",by.y="Group")
  
  # save the R data frame
  save(data,file="Maps.rda",compress=TRUE)
  
  # Go back to original dir
  setwd(original)
}

addAct4Performance <- function(mapTimes){
  # TODO: calculate the number of finished and unfinished maps in Act4
  
  mapTimes$Act4.Finished.Maps <- as.numeric(!is.na(mapTimes$M9_ST) & mapTimes$M9_ST=="FIN") + as.numeric(!is.na(mapTimes$M10_ST) & mapTimes$M10_ST=="FIN")
  mapTimes$Act4.Unfinished.Maps <- as.numeric(!is.na(mapTimes$M9_ST) & mapTimes$M9_ST=="UNFIN") + as.numeric(!is.na(mapTimes$M10_ST) & mapTimes$M10_ST=="UNFIN")
  
  # For those groups that do not have an Act4 duration, we set the performance to NA
  mapTimes[is.na(mapTimes$A4_D),c("Act4.Unfinished.Maps","Act4.Finished.Maps")] <- NA
    
  mapTimes
}