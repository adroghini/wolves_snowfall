##import cams 2014
##need daily depth
##X Y deployment file will be used in GIS to check which IDs overlap with which cameras
cams2014<-read.csv("/Users/amanda_droghini/Desktop/University of Alberta/THESIS/Analyses/chapter2/snowmodel/camdata/2014/depth2014_ID.csv") ##depth

##cleanup
names(cams2014)
cams2014<-cams2014[,-c(2,4:5,9)]

##define Date
cams2014$Date<-as.POSIXct(strptime(cams2014$Date, format="%y-%m-%d",tz="Etc/GMT-7"))
cams2014<-cams2014[order(cams2014$Camera, cams2014$Date),]

##calculate difference in snow
cams2014$dfs<-NA
for(i in 2:nrow(cams2014)) {
  if (cams2014$Camera[i] == cams2014$Camera[i - 1]) {
    cams2014$dfs[i] <- cams2014$SnowDepth[i] - cams2014$SnowDepth[i - 1]
  }
}

cams2014<-cams2014[,-6] ##remove RowID


summary(cams2014$dfs)
nrow(subset(cams2014,dfs>=5)) ##only 9 instances where difference is >=5cm; will be problematic for snowfall analysis...