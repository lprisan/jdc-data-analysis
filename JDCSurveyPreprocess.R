# preprocessJDCSurveys - Clean and organize the questionnaire data
# This is the overall function in charge of the preprocessing and cleaning of the survey data from the JDC experiment
# Parameters: rootDir the directory in which the questionnaire data is (in the form a csv file)
preprocessJDCSurveys <- function(rootDir){
  original <- getwd()
  # Change dir to the rootDir
  setwd(rootDir)
  # load the CSV
  data <- read.csv("StudentsQuestionnaireSequence.csv")
  
  # We correct group labelling due to lamp changes in session 2
  # First, we convert the group variable to character
  data$Group.Number <- as.character(data$Group.Number)
  data[data$Group.Number=="S2G4","Group.Number"] <- "S2G3"
  data[data$Group.Number=="S2G5","Group.Number"] <- "S2G4"
  data$Group.Number <- as.factor(data$Group.Number)
  
  # generate unique student code from the group label
  data$Student.Number <- paste(data$Group.Number,"P",data$Student.Number,sep="")
  
  # Give some meaningful name to the sequence factor
  data$Sequence <- factor(data$Sequence,labels=c("Discrete.First","Continuous.First"))
  
  # We import the data from the coding of the strategy question
  data2 <- read.csv("JdC-StrategyQuestion.csv")
  # We correct group labelling due to lamp changes in session 2
  # First, we convert the group variable to character
  data2$Group.Number <- as.character(data2$Code)
  data2[data2$Group.Number=="S2G4","Group.Number"] <- "S2G3"
  data2[data2$Group.Number=="S2G5","Group.Number"] <- "S2G4"
  data2$Group.Number <- as.factor(data2$Group.Number)
  
  data2$Mentions.Fractions <- data2$Does.the.group.talk.about.fractions..FINAL. == 1
  data2$Gets.Mechanic <- data2$Does.the.group.understand.the.rule.of.the.game..FINAL_Temp. == 1  
  
  data2 <- data2[,c("Group.Number","Mentions.Fractions","Gets.Mechanic")]
  
  data <- merge(data,data2,by="Group.Number")
  
  # save the R data frame
  save(data,file="Survey.rda",compress=TRUE)
  
  # Go back to original dir
  setwd(original)
}