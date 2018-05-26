##create new date column for agsnow that subtracts date by 1
##average snow date in cams data must be subtracted by one to align with new date because, if snowfall was detected @ 12PM on Jan. 24th that means it actually occurred on Jan. 23rd. newDate category reflects that but average snow currently does not. let's try it
##note that diff in avg snow is now average difference not average mean THEN difference
##avgsnow<-avgsnow[,-c(6:8)] not needed in new version
mylt<-as.Date.character(avgsnow$Date)
mylt[1:10]
mylt<-mylt-1
mylt[1:10]
avgsnow$mylt<-mylt
#View(subset(avgsnow,Device_ID==33673))
rm(mylt)

###for telemetry
##USING DATA WITHOUT 2012 FILE
####create new date column
trun<-subset(data,year!=2012)
trun$DT<-as.POSIXct(strptime(trun$DT, format="%Y-%m-%d %H:%M:%S",tz="Etc/GMT-7"))
mylt<-as.POSIXct(strptime(trun$DT, format="%Y-%m-%d %H:%M:%S",tz="Etc/GMT-7"))
mylt[20:30] ##because the noon hour is what we care about, needs to be 0
mylt<-mylt - 12*3600
mylt[20:30]
trun$mylt<-mylt
##now convert to Date only
trun$newDate<-as.Date.character(trun$mylt)
rm(mylt)
