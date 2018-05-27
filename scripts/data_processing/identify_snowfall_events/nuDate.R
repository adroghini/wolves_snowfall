##redo & simplify storm asssignment via nuDate creation on snow_cams
snowdata$DT<-as.POSIXct(strptime(snowdata$DT, format="%Y-%m-%d %H:%M:%S",tz="Etc/GMT-7"))
mylt<-as.POSIXct(strptime(snowdata$DT, format="%Y-%m-%d %H:%M:%S",tz="Etc/GMT-7"))
mylt[20:30] ##because the noon hour is what we care about, needs to be 0
mylt<-mylt - 12*3600
mylt[20:30]
snowdata$mylt<-mylt
##now convert to Date only
snowdata$newDate<-as.Date.character(snowdata$mylt)
rm(mylt)
write.csv(snowdata,"snowstorms02.csv",row.names=F)
