##want to create a column, "storm", where the day before a snowfall is coded 1, the day of is coded 2, and the day after is 3
snowf01<-snowfall
snowf01<-snowf01[!duplicated(snowf01[,c(2:3)]),]

snowf01$storm<-0
for (i in 2:nrow(snowf01)) {
  if (is.na(snowf01$diff[i]) == TRUE) {
    snowf01$storm[i] <- 0
  }
  else if (snowf01$diff[i] >= 5) {
    snowf01$storm[i] <- 2
  }
  else if (snowf01$Device_ID[i] == snowf01$Device_ID[i - 1] &
           snowf01$diff[i - 1] >= 5 & !is.na(snowf01$diff[i - 1])) {
    snowf01$storm[i] <- 3
  }
  else if (snowf01$Device_ID[i] == snowf01$Device_ID[i + 1] &
           snowf01$diff[i + 1] >= 5 & !is.na(snowf01$dif[i + 1])) {
    snowf01$storm[i] <- 1
  }
  else {
    snowf01$storm[i] <- 0
  }
}

table(snowf01$storm)
View(subset(snowf01,storm>0))
names(snowf01)
snowf01<-snowf01[,c(2,3,34)]
snowfall$Date<-as.character(snowfall$Date)
snowf01$Date<-as.character(snowf01$Date)
snowf02<-merge(snowfall,snowf01,by=c("Device_ID","Date"))

View(subset(snowf02,storm>0))
snowf02<-snowf02[order(snowf02$RowID),]
table(snowf02$storm) ##probably occasional overlap but at least like this both day & night are represented
View(subset(snowf02,storm==1))
rm(i,snowf01,snowfall)

##ADDENDUM
snowf01<-snow.red ##save copy
snow.red$storm[snow.red$storm==3 & snow.red$hour>12]<-4
snow.red$storm[snow.red$storm==2 & snow.red$hour>12]<-3
snow.red$storm[snow.red$storm==1 & snow.red$hour>12]<-2
table(snow.red$storm)

####
##we want to further define "pre" storm days b/c category 1 currently encompasses only the 12 hour preceding the snowfall (from noon to midnight), we want to capture from noon to noon i.e. Date -1 & hour > 12. there is some slight overlap between day preceding storm & 2 days following storm (cat. 4)..not sure how to deal with that for now.

soup<-subset(snow.red,storm==1) ##isolate pre storm dates
soup<-soup[,c(1:2)]
soup<-soup[!duplicated(soup[,c(1:2)]),]
soup$Date<-as.Date(soup$Date)-1 ##date - 1
soup$ider<-paste(soup$Device_ID,soup$Date,sep=".") ##create unique ID - as not all device ids are experiencing the same storm
snow.red$ider<-paste(snow.red$Device_ID,snow.red$Date,sep=".")
uni<-soup$ider
snow.red03<-subset(snow.red, ider %in% uni & hour>12) ##subset date/ID match & hour > 12
snow.red02<-subset(snow.red, !(ider %in% uni & hour>12)) ##negative subset

##check
nrow(snow.red02)+nrow(snow.red03)==nrow(snow.red)
uni02<-snow.red03$ider
uni02<-as.data.frame(uni02)
uni02<-as.character(uni02[!duplicated(uni02[,c(1)]),])
uni==uni02

##
snow.red03$storm<-1 ##call these category 1 - day before the storm
snow.red02<-rbind(snow.red02,snow.red03) ##merge up again
rm(snow.red03,uni,uni02,soup)
snow.red02<-snow.red02[order(snow.red02$RowID),]
snow.red02<-snow.red02[,-35]
snow.red<-snow.red[,-35]

table(snow.red$storm)
table(snow.red02$storm)

##repeat with category 4, Date + 1 - if there is override with cat. 1 I think 2 days after is more fitting than 1 day before ... ##didn't work? too much overlap
soup<-subset(snow.red02,storm==4) ##isolate post storm dates
soup<-soup[,c(1:2)]
soup<-soup[!duplicated(soup[,c(1:2)]),]
soup$Date<-as.Date(soup$Date)+1 ##date + 1
soup$ider<-paste(soup$Device_ID,soup$Date,sep=".") ##create unique ID - as not all device ids are experiencing the same storm
snow.red02$ider<-paste(snow.red02$Device_ID,snow.red02$Date,sep=".")
uni<-soup$ider
snow.red04<-subset(snow.red02, ider %in% uni & hour<12) ##subset date/ID match & hour > 12
snow.red03<-subset(snow.red02, !(ider %in% uni & hour<12)) ##negative subset

##check
nrow(snow.red03)+nrow(snow.red04)==nrow(snow.red)
##there is no clearwater march 24th so delete
uni<-uni[-14]
uni02<-snow.red04$ider
uni02<-as.data.frame(uni02)
uni02<-as.character(uni02[!duplicated(uni02[,c(1)]),])
uni==uni02

##
snow.red04$storm<-4 ##call these category 4 - 2 days after the storm
snow.red03<-rbind(snow.red03,snow.red04) ##merge up again
rm(snow.red04,uni,uni02,soup)
snow.red03<-snow.red03[order(snow.red03$RowID),]
snow.red03<-snow.red03[,-35]
snow.red02<-snow.red02[,-35]

table(snow.red$storm)
table(snow.red02$storm)
table(snow.red03$storm)
##took some from day 2?!?!? which is wrong

##get rid of 04 what is it anyway
snow.red02$storm[snow.red02$storm==4]<-0
