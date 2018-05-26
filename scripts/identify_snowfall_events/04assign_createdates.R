##now things might start to get a little hairier as we assign other storm categories
##but the code we did yesterday seemed to have worked

##isolate unique day/DevID of storms
storms<-unique(subset(trun,Storm2=="D0"))
storms<-storms[!duplicated(storms[,c(1,2)]),]
storms<-storms[,c(1,2)]
storms$DateM1<-as.Date(storms$newDate)-1
storms$DateP1<-as.Date(storms$newDate)+1
storms$DateP2<-as.Date(storms$newDate)+2

##recode storms for 33673 & 33675 on March 21 & March 22nd need to redo the storm before/after dates
row.names(storms)<-1:nrow(storms)

##for 33673
storms[15:16,] ##the ones we need to change
storms$DateM1[16]<-storms$DateM1[15]
storms$DateP1[15]<-storms$DateP1[16]
storms$DateP2[15]<-storms$DateP2[16]
storms[15:16,]

##for 33675
storms[21:22,]
storms$DateP1[21]<-storms$DateP1[22]
storms$DateP2[21]<-storms$DateP2[22]
storms$DateM1[22]<-storms$DateM1[21]
storms[21:22,]

length(unique(storms$Device_ID))
length(unique(trun$Device_ID))