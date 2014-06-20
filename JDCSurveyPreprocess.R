# preprocessJDCSurveys - Clean and organize the questionnaire data
# This is the overall function in charge of the preprocessing and cleaning of the survey data from the JDC experiment
# Parameters: rootDir the directory in which the questionnaire data is (in the form a csv file)
preprocessJDCSurveys <- function(rootDir){
  original <- getwd()
  # Change dir to the rootDir
  setwd(rootDir)
  # load the CSV
  data <- read.csv("StudentsQuestionnaireSequence.csv")
  # generate unique student code from the group label
  data$Student.Number <- paste(data$Group.Number,"P",data$Student.Number,sep="")
  
  # Give some meaningful name to the sequence factor
  data$Sequence <- factor(data$Sequence,labels=c("Discrete.First","Continuous.First"))
  
  # save the R data frame
  save(data,file="Survey.rda",compress=TRUE)
  
  # Go back to original dir
  setwd(original)
}