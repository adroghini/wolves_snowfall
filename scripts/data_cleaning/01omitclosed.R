##following stan's suggestion we would like to omit the closed cameras in 2013 & only consider the open ones
##repopulate cams data using cams2013, cams2014

##merge cams2013 & cams2014, add day/month/year columns
library(lubridate)
cams<-rbind(cams2013,cams2014)
cams$year<-year(cams$Date)
cams$month<-month(cams$Date)
cams$day<-day(cams$Date)
rm(cams2013,cams2014)

####ASSIGN 1/0 FOR CLOSED/OPEN
####if camera name contains Closed, assign 1, or else 0
cams$closed<-ifelse(grepl("Closed", cams$Camera), 1, 0)
table(cams$closed,cams$year) ##good, only in 2013
length(unique(subset(cams,closed==1)$Camera)) ##11 closed cameras, all of which have pair equivalents

##see SUMMARY_cams_graph & SUMMARY_cams_stats for open/closed comparisons

##remove closed
cams<-subset(cams,closed!="2013 Closed")
cams<-subset(cams,month!=4)
##SUMMARY_cams_graph02 for graph on snow depth over time (open only)
write.csv(cams,"/Users/amanda_droghini/Desktop/University of Alberta/THESIS/Analyses/chapter2/temp/scripts/snowfall/omit_closed/cams.csv",row.names=F)

#####run the cams_assign script - generates snow_camsC, a database which contains 1 row for each ID/day/camera combination
##dfs = snow difference calculated for each camera

##calculate average snow for each ID/day by averaging across cameras
avgsnow<-ddply(snow_camsC,c("Device_ID","Date"),summarize,snow=mean(SnowDepth,na.rm=T),Ncams=length(Camera),diff=mean(dfs,na.rm=T))

###after you have created snowcamsC file where each ID is matched up with a camera, add newDate columns to average snow & to trun (data with 2012 omitted) - 01avgsnow_newdate in repeat_analysis

###how many cams per wolf?
library(plyr)
avg.cam<-ddply(avgsnow,~Device_ID,summarize,N=mean(Ncams))
summary(avg.cam$N)
rm(avg.cam)

##merge telem with avg snow. - 02merge_avgsnow in repeat_analysis 
rm(snow_camsC,cams)
write.csv(trun,"/Users/amanda_droghini/Desktop/University of Alberta/THESIS/Analyses/chapter2/temp/scripts/snowfall/omit_closed/merge_telem_snow.csv",row.names=F)
write.csv(avgsnow,"/Users/amanda_droghini/Desktop/University of Alberta/THESIS/Analyses/chapter2/temp/scripts/snowfall/omit_closed/avgsnow.csv",row.names=F)

#rm(avgsnow)

###follow up with 03assignstorms
##04assign_createdates

###check out SUMMARY_storms for sum stats...

##when you get to recoding of 33673 & 33675:

##for 33673
storms[17:18,] ##the ones we need to change
storms$DateM1[18]<-storms$DateM1[17]
storms$DateP1[17]<-storms$DateP1[18]
storms$DateP2[17]<-storms$DateP2[18]
storms[17:18,]

##for 33675
storms[21:22,]
storms$DateP1[21]<-storms$DateP1[22]
storms$DateP2[21]<-storms$DateP2[22]
storms$DateM1[22]<-storms$DateM1[21]
storms[21:22,]

length(unique(storms$Device_ID))
length(unique(trun$Device_ID)) ##this will drop off later in merge

##CREATE -2, -3, +3, +4 DATES TO KEEP IN BACK POCKET
storms$DateP3<-as.Date(storms$newDate)+3
storms$DateP4<-as.Date(storms$newDate)+4
storms$DateM2<-as.Date(storms$newDate)-2
storms$DateM3<-as.Date(storms$newDate)-3

##move onto 05assign_catsM1P1P2
##at the end when renaming full
write.csv(full,"/Users/amanda_droghini/Desktop/University of Alberta/THESIS/Analyses/chapter2/temp/scripts/snowfall/omit_closed/storm_assignALL.csv",row.names=F)
tdat.op<-full
rm(full)

##06assign_control but change tdat to tdat.op and tdatA to tdat.opA
write.csv(tdat.opA,"/Users/amanda_droghini/Desktop/University of Alberta/THESIS/Analyses/chapter2/temp/scripts/snowfall/omit_closed/storm_assignALL.csv",row.names=F)
##this is with M2 P3 & control

##proceed with SUMMARY_storms & stats tests