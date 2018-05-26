##import 2013 camera depth & deployment database
cams2013<-read.csv("/Users/amanda_droghini/Desktop/University of Alberta/THESIS/Analyses/chapter2/snowmodel/camdata/2013/depth2013.csv") ##depth

##cleanup
cams2013<-cams2013[,-c(2,4:5,9:11)]

##additional clean-up
cams2013$Date<-as.POSIXct(strptime(cams2013$Date, format="%Y-%m-%d",tz="Etc/GMT-7"))
cams2013<-subset(cams2013,Date<"2013-04-01")
cams2013<-cams2013[order(cams2013[,"Camera"],cams2013[,"Date"]),]
cams2013<-subset(cams2013,!is.na(SnowDepth))

##calculate difference for each camera
cams2013$dfs<-NA
for(i in 2:nrow(cams2013)) {
  if (cams2013$Camera[i] == cams2013$Camera[i - 1]) {
    cams2013$dfs[i] <- cams2013$SnowDepth[i] - cams2013$SnowDepth[i - 1]
  }
}
