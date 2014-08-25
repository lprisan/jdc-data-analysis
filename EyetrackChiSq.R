setwd('~/workspace/jdc-data-analysis/eyetrack/')

data0 <- read.csv("3Metrics.Load0.window10s.slide5s.csv",as.is=T)

data3 <- read.csv("3Metrics.Load3.window10s.slide5s.csv",as.is=T)

data <- rbind(data0,data3)

#data <- data[data$Session==2,]
#We eliminate the only individual occurrence, as it makes the social variable strange
data <- data[data$Session==2 & data$Social!="I",]


# We only get the first option in the doubtful codes
data$Activity <- as.factor(substr(data$Activity,1,3))
data$Social <- as.factor(substr(data$Social,1,1))
data$Focus <- as.factor(substr(data$Focus,1,3))
data$Load <- as.factor(data$Load)

tabAct <- table(data$Load,data$Activity) # Apparently not significant
chisq.test(tabAct)

tabSoc <- table(data$Load,data$Social) # This one looks significant
chisq.test(tabSoc)

tabFoc <- table(data$Load,data$Focus) # This one looks significant also
chisq.test(tabFoc)

tabCha <- table(data$Load,data$Changing.visual.field.a.lot) # This one also looks significant
chisq.test(tabCha)
