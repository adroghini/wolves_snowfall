#we want to calculate daily average snow depth across all cameras for each ID

##calculate daily average for each day and Device_ID
library(plyr)
avgsnow<-ddply(snow_camsC,c("Device_ID","Date"),summarize,snow=mean(SnowDepth,na.rm=T),Ncams=length(Camera),diff=mean(dfs,na.rm=T)) ##what matters here is not so much average snow depth but average DIFFERENCE. 

##order by Dev ID & Date
avgsnow<-avgsnow[order(avgsnow[,"Device_ID"],avgsnow[,"Date"]),]

##calculate snow difference from one day to the next
#avgsnow$diff<-NA NO, DIFFERENCE IS CALCULATED BEFORE FROM RAW DATA CAMS OR ELSE IF YOU GET VARYING SNOW DEPTHS ACROSS CAMERAS E.G. 20, 60, AVERAGE IS 40, YOU WILL LOSE DIFFERENCE RESOLUTION
avgsnow$DTdiff<-NA

##calculate difference in snow from one day to the next & date difference just in case a day jumps - again we always do this by ID 
for(i in 2:nrow(avgsnow)) {
  if (avgsnow$Device_ID[i] == avgsnow$Device_ID[i - 1]) {
    #avgsnow$diff[i] <- avgsnow$snow[i] - avgsnow$snow[i - 1]
    avgsnow$DTdiff[i] <-
      difftime(avgsnow$Date[i],avgsnow$Date[i - 1],units = "days")
  }
  else {
    NA
  }
}
rm(i)
summary(avgsnow$DTdiff) ##one day for which day > 1 (Device ID 33669 on 2014-03-05 but doesn't matter because there is no snow in the forecast)
avgsnow<-avgsnow[,-6]
summary(avgsnow$diff) ##wow that's low

##how many cameras per ID
library(plyr)
Y<-ddply(avgsnow,~Device_ID,summarize,cams=mean(Ncams))
mean(Y$cams)
rm(Y)

##SUMMARY snowfalls
#how many unique snowfall events do we have?
storm<-subset(avgsnow,diff>=5)
length(unique(storm$Device_ID))
unique(avgsnow$Device_ID)
storm02<-storm[!duplicated(storm[,2]),] ##get # of unique storm events
##should probably remove those individuals not having any storms at all as this will add unnecessary variation in 0 speed category?
##33676, 33669, 32254, 13790



rm(storm,storm02)
