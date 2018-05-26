# Load required packages
library(Animal)
library(doSNOW)
library(foreach)

# Calculate step length
# Expressed in meters/minute
merge$step=NA
merge$step<-as.numeric(merge$step)
for (i in 2:nrow(merge)){
  if (merge$Device_ID[i]==merge$Device_ID[i-1]){
    xdiff<-(merge$POINT_X[i]-merge$POINT_X[i-1])^2
    ydiff<-(merge$POINT_Y[i]-merge$POINT_Y[i-1])^2
    merge$step[i]=sqrt(xdiff+ydiff)}}
write.csv(merge,"merge.csv",row.names=F)

merge$speed=NA
merge$speed<-as.numeric(merge$speed)
NumberOfCluster <- 4
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)
print(Sys.time())
merge$speed<-foreach(i = 1:nrow(merge), .combine = rbind) %dopar% {
  merge$step[i]/merge$DTdiff[i]
}
print(Sys.time())
stopCluster(cl)

#write.csv(merge,"merge.csv",row.names=F)

######WHAT IS THIS????
##repopulate LINENO - some are fucked (e.g. City pack LINENO ran out of decimals)
merge<-merge[order(merge[,"Device_ID"],merge[,"DT"]),]
###delete old LINENO column: merge<-merge[,-49]###
library(data.table)
mergeA <- as.data.table(merge)
mergeB<-mergeA[, LINENO := 0:nrow(mergeA), by = Device_ID]
merge<-mergeB
rm(mergeA,mergeB)
write.csv(merge,"merge.csv",row.names=F)

# Calculate daily distance travelled
daily.dist<- daily(W2013$step,time=W2013$Date,fun=sum,subject=W2013$Device_ID)
##merge up both docs
table.sum<-table.sum[order(table.sum[,"Device_ID"],table.sum[,"Date"]),]
daily.dist<-daily.dist[order(daily.dist[,"Subject"],daily.dist[,"Date"]),]
table.sum<-cbind(table.sum,daily.dist)
View(table.sum)
rm(daily.dist)
