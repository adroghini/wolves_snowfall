###MERGE TELEMETRY WITH AVERAGE SNOW DATA
##use new date category in snowdata telem file & avgsnow file (see avgsnow_newdate script for explanation)
trun<-merge(trun,avgsnow,by.x=c("Device_ID","newDate"),by.y=c("Device_ID","mylt"),all.x=T)

##need to reorder by RowID though because things got shuffled
trun<-trun[order(trun$RowID),]
##check it
write.csv(trun,"snowstorms02.csv",row.names=F)

##this seems to be working so newDate is the column to use along with the new difference

##save files
write.csv(trun,"/Users/amanda_droghini/Desktop/University of Alberta/THESIS/Analyses/chapter2/temp/scripts/snowfall/repeat_analysis/snowstorms02.csv",row.names=F)
write.csv(avgsnow,"/Users/amanda_droghini/Desktop/University of Alberta/THESIS/Analyses/chapter2/temp/scripts/snowfall/repeat_analysis/avgsnow.csv",row.names=F)

rm(avgsnow)
