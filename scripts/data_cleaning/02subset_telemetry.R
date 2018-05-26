# Subset
## Remote camera data used for analysis were only available for winter (Jan. - Mar.) 2013 and 2014

## Subset to only include time period of interest
###################################
##### BETTER WAY TO DO THIS ######
telem<-subset(telem,Season=="Winter")
##remove march 2012
telem<-subset(telem,DT_AD > as.POSIXct('2012-04-01'))
merge <- telem # name change from previous script which had the file merging with temperature data I thought I was going to use
merge<-merge[order(merge[,"Device_ID"],merge[,"DT"]),]

## Subset collars
## Some collars failed or only had 3-hour fix rates
## Maximum of 30 min. fix rate for snowfall analysis because fine-scale movement metrics are required
ID<-unique(merge$Device_ID)
pack<-unique(merge$Pack)
## 11 packs and 39 unique IDs

library(plyr)
ddply(merge,c("Device_ID","Pack"),summarise,N=length(Use))

###################################
##### BETTER WAY TO DO THIS ######

##delete F63 33678 a (only 32 observations with 3 hour fix rates and 2 days of data)
merge<-merge[!(merge$F63=="33678 a"),]

##Vectronics collars (IDs starting with 139...) were unreliable & died quickly
##collar 13794 seemed to have worked best (until end of season)
##13790, 13792, 13793 only worked through end of January

##discard collar 13791 because there are huge jumps between fixes - only 663 obs. anyway
merge<-merge[!(merge$Device_ID==13791),]

##thinking of getting rid of collar 13793 as well.. not sure yet...

##delete ID 35260 - only 13 observations & fix rate >= 24 hrs.
merge<-merge[!(merge$Device_ID==35260),]

##delete points left from cluster removal (only 4 points because the others were in summer)
merge<-merge[!(merge$Device_ID==32266 & merge$LINENO>=5172),]

##delete Device_ID 15008 and 15009 (only 8 and 12 obs. respectively)
merge<-merge[!(merge$Device_ID==15008 | merge$Device_ID==15009),]

##delete Device_ID 33674 with LINENO 0 through 3 (slow fixes until 30mins. pick-up)
merge<-merge[!(merge$Device_ID==33674 & merge$LINENO<=3),]

##########REPOP. UNIQUE ID & PACK + THAT 1 OBS FROM A COLLAR DELETE IT NOW

##delete last obs. of Device_ID 33674 on March 31 2014 (fix rate goes from 30 mins. to 1080)
merge<-merge[!(merge$Device_ID==33674 & merge$LINENO==7743),]



test <- telem %>% 
  group_by(Device) %>% 
  mutate(UID = row_number())

######MOVEMENT METRIC STUFF
# Calculate fix rate
# i.e. time difference between each consecutive fix

####i would rewrite this code using a foreach loop & parallel, because we cannot use
####i-1 notation, make column DT-1. create dataset 2:nrow(X), add to it DTdiff 1:(nrow-1)
####add to it Dev_id (n-1). condition dev-id==devid (n-1)
####reappend row #1 with DTdiff being NA.

merge<-merge[order(merge[,"Device_ID"],merge[,"DT"]),]
##recalculate DTdiff - use difftime to get consistent units
merge$DTdiff=NA
merge$DTdiff<-as.numeric(merge$DTdiff)
for (i in 2:nrow(merge)){
  if (merge$Device_ID[i]==merge$Device_ID[i-1]){
    merge$DTdiff[i]<-difftime(merge$DT[i],merge$DT[i-1],units="mins")}}

###################
###################
###################
ORIGINALLY FROM MVMT METRICS FILE
YOU HAVE TO CLEAN this
GET RID OF FIX RATES THAT DON'T MAKE SENSE
AND RAREFY DATA
##get rid of huge diffDT e.g. between March 31st and Oct. 1st
##time interval between the two is >200 000 (minutes)
Z<-subset(merge,DTdiff>200000&month_loca==10)
Z$DTdiff=NA
Z$step=NA
Z$speed=NA
Z$DTdiff=as.numeric(Z$DTdiff)
Z$step=as.numeric(Z$step)
Z$speed=as.numeric(Z$speed)
##creative negative subset
Y<-merge[!(merge$DTdiff>200000 & merge$month_loca==10),]
nrow(Y)+nrow(Z)==nrow(merge)
##merge the two
merge<-rbind(Y,Z)
rm(Y,Z)
merge<-merge[order(merge[,"Device_ID"],merge[,"DT"]),]


##THIS IS HOW I RAREFIED IN 07RAREFY_NOV
##not sure if this is true???
##I think this is for November data I never used
##SUBSET TO 30 MINS INSTEAD OF 15, GIVEN THAT ALL DEVICE IDS HAVE THEIR FIRST FIX RATE ON 00:00:00 ON THE 11TH
ffNovR<-subset(ffNov,minute_loc<5|(minute_loc>22&minute_loc<35))



#####this is how i rarefied with 08rarefy2014 scripts
##repeat plot procedures with 2014 data rarefied to 30 minutes :)
###script 08: those 10 minute fast November fixes
subset(W2014,DTdiff>35)
##13793 is screwy but c'est la vie
W2014.R<-subset(W2014,Device_ID!=13793)
W2014.R<-subset(W2014.R,DTdiff>7&DTdiff<35)


# Make data (hourly time stamps +- random noise with 30 min standard dev)
len <- 81  # number of days
devices<-c(13790,13792,13794,32254,32261,32263,33676,33678)
devices<-as.factor(device)
library(data.table)
stamps.data = data.table(device=W2014.R$Device_ID,stamp.join=W2014.R$DT)
# Now add a copy of stamp.join to the data table; necessary
# because we will lose the stamp.join column in the join
stamps.data[, closest.match:=stamp.join]
# Now, for each pet, create a data.table with the target
# times (CJ does a cartesian join of our pets and our target
# times vectors and returns a data table, this is necessary
# because we are doing a rolling join, if it was an exact
# join we wouldn't need to CJ with pets, could just use
# target stamps)

stamps.target <- CJ(devices, seq(as.POSIXct("2014-01-09 17:00:00"), by="30 mins", length.out=len*48))
setkey(stamps.data, device, stamp.join)  # join on pet and stamp.join

# Use data table to join stamps.target (midnight/noon) to stamps (hourly w/ noise)
help<-stamps.data[stamps.target, roll="nearest"][order(device)]
View(help)

W2014.R<-subset(W2014.R,minute_loc<5|(minute_loc>25&minute_loc<35))
W2014.R<-W2014.R[order(W2014.R[,"RowID"]),]
W2014.R$DT<-as.POSIXct(strptime(W2014.R$DateTimeSt, format="%m/%d/%Y %H:%M:%S",tz="Etc/GMT-7"))

W2014.R$DTdiff<-NA
W2014.R$DTdiff<-as.numeric(W2014.R$DTdiff)
W2014.R2<-W2014.R[2:23157,]
W2014.R1<-W2014.R[1:23156,]
W2014.R2$Dev1<-W2014.R1$Device_ID
W2014.R2$DT1<-W2014.R1$DT

library(doSNOW)
library(foreach)
NumberOfCluster <- 4
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)
print(Sys.time())
W2014.R2$DTdiff<-foreach(i = 1:nrow(W2014.R2), .combine = rbind) %dopar% {
  if (W2014.R2$Device_ID[i]==W2014.R2$Dev1[i]){
    difftime(W2014.R2$DT[i],W2014.R2$DT1[i],units="mins")
  }
  else{NA}}
print(Sys.time())
stopCluster(cl)

W2014.R1$DTdiff<-NA
W2014.R1$DTdiff<-as.numeric(W2014.R1$DTdiff)
W2014.R1<-W2014.R1[1,]
W2014.R2<-W2014.R2[,-(85:86)]
W2014.R3<-subset(W2014.R2,DTdiff>35)
W2014.R2<-subset(W2014.R2,DTdiff<=35)
W2014.R3$DTdiff<-NA
W2014.R3$DTdiff<-as.numeric(W2014.R3$DTdiff)
W2014.R<-rbind(W2014.R1,W2014.R2,W2014.R3)
rm(W2014.R1,W2014.R2,W2014.R3)
W2014.R<-W2014.R[order(W2014.R[,"RowID"]),]
View(W2014.R)


#recalculate step & speed
W2014.R2<-W2014.R[2:23150,]
W2014.R1<-W2014.R[1:23149,]

W2014.R2$Dev1<-W2014.R1$Device_ID
W2014.R2$PX1<-W2014.R1$POINT_X
W2014.R2$PY1<-W2014.R1$POINT_Y
W2014.R2$step<-NA
W2014.R2$step<-as.numeric(W2014.R2$step)

##STEP
NumberOfCluster <- 4
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)
print(Sys.time())
W2014.R2$step<-foreach(i = 1:nrow(W2014.R2), .combine = rbind) %dopar% {
  if (W2014.R2$Device_ID[i]==W2014.R2$Dev1[i]){
    xdiff<-(W2014.R2$POINT_X[i]-W2014.R2$PX1[i])^2
    ydiff<-(W2014.R2$POINT_Y[i]-W2014.R2$PY1[i])^2
    sqrt(xdiff+ydiff)
  }
  else{NA}}
print(Sys.time())
stopCluster(cl)

##SPEED
W2014.R2$speed<-NA
W2014.R2$speed<-as.numeric(W2014.R2$speed)
NumberOfCluster <- 4
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)
print(Sys.time())
W2014.R2$speed<-foreach(i = 1:nrow(W2014.R2), .combine = rbind) %dopar% {
  W2014.R2$step[i]/W2014.R2$DTdiff[i]}
print(Sys.time())
stopCluster(cl)

summary(W2014.R2$speed)
summary(W2014.R2$DTdiff)

W2014.R1<-W2014.R1[1,]
W2014.R1$step<-NA
W2014.R1$speed<-NA
W2014.R2<-W2014.R2[,-(85:87)]
W2014.R<-rbind(W2014.R1,W2014.R2)
rm(W2014.R1,W2014.R2,cl,NumberOfCluster,xdiff,ydiff)
W2014.R$speed<-as.numeric(W2014.R$speed)
W2014.R1<-subset(W2014.R,!is.na(speed))


