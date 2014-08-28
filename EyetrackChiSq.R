library(ggplot2)

setwd('~/workspace/jdc-data-analysis/eyetrack/')

data0 <- read.csv("3Metrics.Load0.window10s.slide5s.csv")

data3 <- read.csv("3Metrics.Load3.window10s.slide5s.csv")

data <- rbind(data0,data3)

data$Load <- as.factor(data$Load)

print(tabAct <- table(data$Load,data$Activity))
print(chisq.test(tabAct))

print(tabSoc <- table(data$Load,data$Social))
print(chisq.test(tabSoc))

print(tabFoc <- table(data$Load,data$Focus))
print(chisq.test(tabFoc))

print(tabCha <- table(data$Load,data$Changing.visual.field.a.lot))
print(chisq.test(tabCha))

#TODO: do a simple histogram of the distribution of the different variables
barplot(table(data$Activity))
barplot(table(data$Social))
barplot(table(data$Focus))
barplot(table(data$Changing.visual.field.a.lot))

eyedata2 <- get(load("TotalEyetrackingData.Session.2.rda"))
eyedata3 <- get(load("TotalEyetrackingData.Session.3.rda"))
eyedata6 <- get(load("TotalEyetrackingData.Session.6.rda"))

eyedata <- rbind(eyedata2,eyedata3,eyedata6)

totaldata <- merge(x=data,y=eyedata,by.x=c("Session","Time.ms"), by.y=c("Session","time"),all=T)
completedata <- totaldata[complete.cases(totaldata),]