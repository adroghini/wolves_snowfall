##want to create a code that will take the dates in storms and assign a storm category to telem
uni=unique(storms$Device_ID)
uni=as.matrix(uni)
segs=list()

for(n in 1:length(uni)){
  k=subset(trun,Device_ID==uni[n])
  j=subset(storms,Device_ID==uni[n])
  for(i in 1:nrow(k)) {
    if (k$newDate[i] %in% j$DateP1 & k$Storm2[i]=="none") {
      k$Storm2[i] <- "P1"
    }
    else if (k$newDate[i] %in% j$DateP2 & k$Storm2[i]=="none") {
      k$Storm2[i] <- "P2"
    }
    else if (k$newDate[i] %in% j$DateM1 & k$Storm2[i]=="none") {
      k$Storm2[i] <- "M1"
    }
    else if (k$newDate[i] %in% j$DateM2 & k$Storm2[i]=="none") {
      k$Storm2[i] <- "M2" ##added the latter 2 for omit_closed analysis
    }
    else if (k$newDate[i] %in% j$DateP3 & k$Storm2[i]=="none") {
      k$Storm2[i] <- "P3"
    }
  }
  print(unique(k$Device_ID))
  print(table(k$Storm2))
  segs[[n]]=data.frame(k)
}
full=do.call("rbind",segs) ##i think we got it
length(unique(full$Device_ID)) ##back down to 10 like this, perfs

write.csv(full,"/Users/amanda_droghini/Desktop/University of Alberta/THESIS/Analyses/chapter2/temp/scripts/snowfall/repeat_analysis/storm_assignALL.csv",row.names=F)

rm(i,j,k,n,segs,uni,storms)
rm(trun)
tdat<-full
rm(full)

