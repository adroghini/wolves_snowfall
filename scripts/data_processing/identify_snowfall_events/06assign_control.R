###create controls
##tdat.op database
table(tdat.op$Storm2)

##create list of dates that CANNOT be used for controls
cont<-subset(tdat.op,Storm2!="none")
cont<-cont[!duplicated(cont[,c(1,2)]),] 
cont<-cont[,c(1,2)]

##code to randomly select dates
uni=unique(cont$Device_ID)
segs=list()
for(i in 1:length(uni)){
  k=subset(tdat.op,Device_ID==uni[i]) ##all the dates
  k<-k[!duplicated(k[,c(1,2)]),] ##all unique dates for that ID
  k<-k[,c(1:2)]
  j=subset(cont,Device_ID==uni[i]) ##those you can't pick
  k=subset(k,!(k$newDate %in% cont$newDate)) ##those you can
  random=k[sample(nrow(k), 3), ] ##select 3
  print(random)
  segs[[i]]<-random
}

segs=do.call("rbind",segs) ##3 random dates for each ID
rm(i,uni,j,k,random)

#segs$Storm2<-"control"
#segs<-segs[,c(1,13,14)]
j<-merge(tdat.op,segs,by=c("Device_ID","newDate"),all.x=F)
j$Storm2<-"control"
k<-subset(tdat.op,!(RowID %in% j$RowID ))
nrow(j)+nrow(k)==nrow(tdat.op)

tdat.opA<-rbind(j,k)
tdat.opA<-tdat.opA[order(tdat.opA$RowID),]
table(tdat.opA$Storm2)

rm(segs,j,k,cont,tdat.op)
