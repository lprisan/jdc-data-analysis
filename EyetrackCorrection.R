library(plyr)

setwd("~/workspace/jdc-data-analysis/eyetrack")

if(file.exists("Session2.Events.filter100ms.rda")){#If we have the filtered data, we load it as it is quite slow to process it all again
  filter100ms <- get(load("Session2.Events.filter100ms.rda"))
} else{#We do a 100ms filter of the data, to match the 10fps video data we have
  pupildata <- get(load("EyetrackerEvents.rda"))
  pupildata <- pupildata[pupildata$Session=="2",]
  
  # From this data we should get only the events that match with the video timestamps (100ms increments)
  # Is this error very relevant???
  pupildata$Round.Timestamp <- round_any(pupildata$Time.ms,100)
  
  #filter100ms <- pupildata[0,]
  uniques <- unique(pupildata$Round.Timestamp)
  filter100ms <- as.data.frame(lapply(pupildata, function(x) rep.int(NA, length(uniques))))
  
  # We select only the closest sample to 100ms increments
  for(i in 1:length(uniques)){
    t <- uniques[i]
    selectedrows <- pupildata[pupildata$Round.Timestamp == t,]
    mindiff <- min(abs(t-selectedrows$Time.ms))
    newrow <- selectedrows[abs(selectedrows$Round.Timestamp-selectedrows$Time.ms)==mindiff,]
    filter100ms[i,] <- newrow[1,]
  }
  
  save(filter100ms,file="Session2.Events.filter100ms.rda",compress=TRUE)
}


# We calculate luminance using Moon & Spencer formula
filter100ms$lum.eye <- exp((1/0.4)*atanh((filter100ms$L.Pupil.Diameter..mm.-4.9)/(-3)))

# We calculate luminance using DeGroot & Gebhard
term <- (log(filter100ms$L.Pupil.Diameter..mm./7.175))/(-0.00092)
filter100ms$lum.eye.DG <- exp((sign(term) * abs(term)^(1/3))-7.597)


summary(filter100ms$lum.eye)
summary(filter100ms$lum.eye.DG)

videodata <- read.csv("session2_intensities.csv")

videodata$lum.video <- log2(videodata$Max.Intensity-videodata$Min.Intensity)

#Inspect the luminance taken from the video
summary(videodata$lum.video) # This looks pretty uniform... is it correct?

totaldata <- merge(videodata,filter100ms,by.x="Timestamp.ms",by.y="Round.Timestamp")

write.csv(totaldata,file="Luminance.Data.Session2.csv")

model <- pairwise.t.test(totaldata$lum.video,totaldata$lum.eye.DG,na.action=na.omit,alternative = "two.sided")
summary(model)
