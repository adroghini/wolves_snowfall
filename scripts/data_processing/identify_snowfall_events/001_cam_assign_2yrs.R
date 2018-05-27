####OBJECTIVE: for each ID/Date/camera, obtain snow depth value
#####this requires us to merge camera database with telemetry database, based on association between cams-wolves
####in qGIS, we looked at 95% wolf kernels & matched them up with the cameras that were in its territory (for both 2013 and 2014)

##wolf telemetry data
snow_cams<-subset(data,year!="2012") ##no 2012 years
##remove IDs for which we have no cameras: 33674, 33667 and 13792
snow_cams<-subset(snow_cams,Device_ID!=33674&Device_ID!=33667&Device_ID!=13792)

##merge cams2013 & cams2014
names(cams2013)
names(cams2014)
cams2014<-cams2014[,-6] ##remove RowID
##merge cams & cams, add day/month/year columns
library(lubridate)
cams<-rbind(cams2013,cams2014)
cams$year<-year(cams$Date)
cams$month<-month(cams$Date)
cams$day<-day(cams$Date)
rm(cams2013,cams2014) ##also just the cams.csv file in scripts/omit_closed

##create camID identifier that will link cameras to Device IDs
cams$camID<-NA
snow_cams$camID<-NA

unique(snow_cams$Device_ID)
unique(cams$Camera)

##unfortunately there is no quick way to do this? need to type everything out.. well i'm sure you could have spatialed this...

##2013
snow_cams$camID[(snow_cams$Device_ID==33672)]<-1 
cams$camID[(cams$Camera=="SAthClosed"|cams$Camera=="SAthOpen")]<-1 
snow_cams$camID[(snow_cams$Device_ID==33671)]<-2 
cams$camID[cams$Camera=="CL1Closed"|cams$Camera=="CL1Open"|cams$Camera=="CL3Closed"|cams$Camera=="CL3Open"|cams$Camera=="ClearOpen"|cams$Camera=="ClearClosed"]<-2
snow_cams$camID[(snow_cams$Device_ID==33669)]<-3
cams$camID[cams$Camera=="SteepbankClosed"|cams$Camera=="SteepbankOpen"]<-3
snow_cams$camID[(snow_cams$Device_ID==33670)]<-4
cams$camID[cams$Camera=="McKayOpen"]<-4
snow_cams$camID[(snow_cams$Device_ID==33673)]<-5
cams$camID[cams$Camera=="Ells1Open"|cams$Camera=="Ells2Closed"|cams$Camera=="FrostbiteClosed"|cams$Camera=="FrostbiteOpen"|cams$Camera=="BearEllsOpen"]<-5
snow_cams$camID[(snow_cams$Device_ID==33668)]<-6
cams$camID[cams$Camera=="IceBridge1Closed"|cams$Camera=="IceBridge1Open"|cams$Camera=="McElle1Open"|cams$Camera=="McElle1Closed"|cams$Camera=="McElle2Closed"|cams$Camera=="McElle2Open"]<-6
snow_cams$camID[(snow_cams$Device_ID==33675)]<-7
cams$camID[cams$Camera=="BTWNClosed"|cams$Camera=="BTWNOpen"]<-7

table(snow_cams$camID,snow_cams$Device_ID)
table(cams$camID,cams$Camera)

##2014 assignment
snow_cams$camID[(snow_cams$Device_ID==33676)]<-8 
cams$camID[cams$Camera=="Far25"|cams$Camera=="Near09"|cams$Camera=="Near11"]<-8
snow_cams$camID[(snow_cams$Device_ID==13790)]<-9
cams$camID[cams$Camera=="Far05"|cams$Camera=="Far06"|cams$Camera=="Far13"|cams$Camera=="Near05"|cams$Camera=="Near"]<-9
snow_cams$camID[(snow_cams$Device_ID==13794)]<-11
cams$camID[cams$Camera=="Near10"]<-11
snow_cams$camID[snow_cams$Device_ID==32261|snow_cams$Device_ID==32263]<-13
cams$camID[cams$Camera=="Far16"]<-13

##problem for a few cameras that apply to more than one ID
##create another dataframe and camIDA column to work through this
snow_rejects<-subset(snow_cams,Device_ID==33678|Device_ID==32254)
snow_rejects$camIDA<-NA
snow_rejects$camIDA[(snow_rejects$Device_ID==33678)]<-10
cams$camIDA<-NA
cams$camIDA[cams$Camera=="Far13"|cams$Camera=="Near05"]<-10
snow_rejects$camIDA[(snow_rejects$Device_ID==32254)]<-12
cams$camIDA[(cams$Camera=="Far25")]<-12

##ensure Date format
cams$Date<-as.Date(cams$Date,format="%Y-%m-%d",tz="Etc/GMT-7")
snow_cams$Date<-as.Date(snow_cams$Date,format="%Y-%m-%d",tz="Etc/GMT-7")
snow_rejects$Date<-as.Date(snow_rejects$Date,format="%Y-%m-%d",tz="Etc/GMT-7")
##need only 1 row of the date per Dev ID 
snow_camsA<-subset(snow_cams,Device_ID!=33678&Device_ID!=32254)
snow_camsA<-snow_camsA[!duplicated(snow_camsA[,c(2,7)]),]
snow_camsA<-snow_camsA[,c(2,7:10,25)]
snow_camsB<-snow_rejects[!duplicated(snow_rejects[,c(2,7)]),]
snow_camsB<-snow_camsB[,c(2,7:10,26)]

##merge - for every Dev ID & date, I want the snow depth as per the cam(s) in its territory
snow_camsA<-merge(snow_camsA,cams,by=c("year","month","day","camID"),all.x=FALSE,all.y=FALSE)
snow_camsA<-snow_camsA[order(snow_camsA$Device_ID),] 
snow_camsA<-snow_camsA[,-c(7,8,9,13)] ##delete extra rows
snow_camsA<-snow_camsA[,c(5:6,1:3,8,7,9,4)] ##reorder
colnames(snow_camsA)[2]<-"Date"

##do the same thing with snow_camsB and then we can merge up both docs
snow_camsB<-merge(snow_camsB,cams,by=c("year","month","day","camIDA"),all.x=FALSE,all.y=FALSE)
snow_camsB<-snow_camsB[order(snow_camsB$Device_ID),] 
snow_camsB<-snow_camsB[,-c(7,8,9,13)] 
snow_camsB<-snow_camsB[,c(5:6,1:3,8,7,9,4)] 
colnames(snow_camsB)[2]<-"Date"
colnames(snow_camsB)[9]<-"camID"

names(snow_camsA)
names(snow_camsB)

##shove them together
snow_camsC<-rbind(snow_camsA,snow_camsB)
length(unique(snow_camsC$camID))
length(unique(snow_camsC$Device_ID))
##13 even though 14 IDs because 32261 and 32263 have same camera 
rm(snow_camsA,snow_camsB,snow_rejects)
rm(snow_cams)

##snow_camsC now contains 14 ID - 7 2013, 7 2014, snow depth for each day/camera/ID 

