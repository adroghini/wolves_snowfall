####THIS CODE WILL NEED TO BE SPLIT UP BECAUSE IT CONTAINS MODELS/RESULTS


##wondering if proportion of time spent resting vs. moving is different AM/PM

###Data build
AM<-data
library(lubridate)
AM$hour<-hour(AM$DT)
AM$day<-day(AM$DT)
AM$month<-month(AM$DT)
AM$year<-year(AM$DT)
##to identify AM/PM times, will base myself on median day sunrise/sunset time in fort mc for each month
ddply(AM,c("year","month"),summarize,day=median(day))

AM$AM<-"night"
AM$AM[(AM$year=="2012" & AM$hour>=8 & AM$hour<16)|((AM$year=="2013"|(AM$year=="2014")) & AM$month==1 & AM$hour>=9 & AM$hour<17)|((AM$year=="2013"|(AM$year=="2014")) & AM$month==2 & AM$hour>=8 & AM$hour<18)|((AM$year=="2013"|(AM$year=="2014")) & AM$month==3 & AM$hour>=7 & AM$hour<19)]<-"day"
table(AM$AM)

AM$beh<-0
AM$beh[AM$behav=="move"]<-1

AM$linesON<-as.character(AM$linesON)
AM$linesON<-as.numeric(AM$linesON)

##want to add a column with # of daylight hours
AM$dayl<-NA
AM$dayl[(AM$year=="2012"|(AM$month==1))&AM$AM=="day"]<-8
AM$dayl[(AM$year=="2012"|(AM$month==1))&AM$AM=="night"]<-16
AM$dayl[AM$month==2&AM$AM=="day"]<-10
AM$dayl[AM$month==2&AM$AM=="night"]<-14
AM$dayl[AM$month==3]<-12
(table(AM$month,AM$dayl))

library(plyr)
##pretty proud of this stupid table
AMsum<-ddply(AM,c("Device_ID","Date","AM"),summarize, N=length(beh),mov=sum(beh),rest=N-mov,prom=mov/N,year=mean(year),month=mean(month),day=mean(day),dayh=mean(dayl),pdayl=prom/dayh,temp=mean(govt.temp),wind=mean(WindSpd),snow=mean(CMCdepth))
##ok "max min" N you can have is 8hrs. * 2 locs./hr = 16
nrow(subset(AMsum,N<10)) ##get rid of anything less than 10?
AMsum<-subset(AMsum,N>=10)

AMsum2<-ddply(AMsum,c("month","AM"),summarize,N=length(year),pmov=mean(prom),pday=mean(pdayl),ci=qt(0.975,df=N-1)*sd(prom)/sqrt(N),cday=qt(0.975,df=N-1)*sd(pdayl)/sqrt(N))
AMsum2
AMsum2$month<-as.factor(AMsum2$month)
AMsum2$month<-revalue(AMsum2$month, c("1"="Jan.", "2"="Feb.", "3"="Mar.","11"="Nov."))
AMsum2$month<-ordered(AMsum2$month, levels = c("Nov.","Jan.","Feb.","Mar."))

##PROPORTION OF TRAVEL, DAY VERSUS NIGHT
library(ggplot2)
ggplot(AMsum2, aes(x=month, y=pmov,fill=AM)) +
  geom_bar(aes(colour=AM),position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=pmov-ci, ymax=pmov+ci),
                width=.2,
                position=position_dodge(.9))+
  scale_y_continuous(breaks=0:35*0.1,lim=c(0,0.52))+
  ylab("Proportion of travel behaviour")+
  xlab("Month")+
  scale_fill_manual(values=c("grey78","royalblue4"))+
  scale_colour_manual(values=c("grey78","royalblue4"))+
  theme_bw()+
  theme(legend.position = "none",
                    axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))
ggsave(filename="graphs/ampm_summ.png",dpi=800,units="cm",height=10,width=15)

##PROPORTION OF TRAVEL, DAY VERSUS NIGHT, REL. TO DAYLENGTH
library(ggplot2)
ggplot(AMsum2, aes(x=month, y=pday,fill=AM)) +
  geom_bar(aes(colour=AM),position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=pday-cday, ymax=pday+cday),
                width=.2,
                position=position_dodge(.9))+
  scale_y_continuous(breaks=0:35*0.01,lim=c(0,0.064))+
  ylab("Relative proportion of travel behaviour")+
  xlab("Month")+
  scale_fill_manual(values=c("grey78","royalblue4"))+
  scale_colour_manual(values=c("grey78","royalblue4"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))
ggsave(filename="graphs/ampm_summ.png",dpi=800,units="cm",height=10,width=15)


rm(AMsum2)

###PROP. OF TRAVEL ON DIFF. LINE TYPES BY TIME OF DAY
AM_move<-subset(AM,beh==1&!(stpl2=="None"))
tab<-table(AM_move$Device_ID,AM_move$stpl2) ##these are the totals I want
tab<-as.data.frame.matrix(tab)
tab$Device_ID<-row.names(tab)
library(reshape)
tab<-melt(tab)
colnames(tab)[2:3]<-c("stpl2","freq")
tab<-tab[order(tab[,"Device_ID"],tab[,"stpl2"]),]
##also need frequency and totals for each, + speed
tab2<-ddply(AM_move,c("Device_ID","stpl2","AM"),summarize,sp=mean(speed),year=mean(year),N=length(linesON),.drop=FALSE)
tab2<-tab2[order(tab2[,"Device_ID"],tab2[,"stpl2"]),]
tab<-merge(tab,tab2,by=c("Device_ID","stpl2"),all.x = TRUE,all.y=TRUE)
rm(tab2)
##get proportion
tab$prop<-tab$N/tab$freq

##danke
AMsum2<-ddply(tab,c("stpl2","AM"),summarize,spd=mean(sp,na.rm=T),count=sum(N),tot=length(AM_move$sp),pro=mean(prop,na.rm=T),ci=qt(0.975,df=count-1)*sd(prop,na.rm=T)/sqrt(count),ciS=qt(0.975,df=count-1)*sd(sp,na.rm=T)/sqrt(count))
##other water is useless
AMsum2<-subset(AMsum2,stpl2!="Other water")

##plot...
AMsum2$stpl2<-ordered(AMsum2$stpl2, levels = c("Rivers","Oil & energy","Roads"))
ggplot(AMsum2, aes(x=stpl2, y=pro,fill=AM)) +
  geom_bar(aes(colour=AM),position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=pro-ci, ymax=pro+ci),
                width=.2,
                position=position_dodge(.9))+
  scale_y_continuous(breaks=0:105*0.2,lim=c(0,0.85))+
  ylab("Relative proportion of use")+
  #scale_x_discrete(labels=c("Day","Night"))+
  xlab("Linear feature type")+
  scale_fill_manual(values=c("grey78","royalblue4"))+
  scale_colour_manual(values=c("grey78","royalblue4"))+
  theme_bw()+theme(legend.position = "none",
                   axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))
ggsave(filename="graphs/lines_AM.png",dpi=800,units="cm",height=10,width=15)

rm(AMsum,AMsum2,tab,AM_move)

##########################
##########JUNK############
##########################

##PROPORTION OF TRAVEL STEPS ON LINES BY TIME OF DAY
##let's consider only those steps on lines rather than all travel steps
AM_move<-subset(AM,beh==1&(linesON==1))

##frequency table
tab<-table(AM_move$Device_ID,AM_move$AM) ##these are the totals I want
tab<-prop.table(tab,1)
tab<-as.data.frame.matrix(tab)
tab$Device_ID<-row.names(tab)
library(reshape)
tab<-melt(tab)
colnames(tab)[2:3]<-c("AM","prop")
tab<-tab[order(tab[,"Device_ID"],tab[,"AM"]),]
##also need frequency and totals for each, + speed
tab2<-ddply(AM_move,c("Device_ID","AM"),summarize,sp=mean(speed),year=mean(year),N=length(linesON),.drop=FALSE)
tab2<-tab2[order(tab2[,"Device_ID"],tab2[,"AM"]),]
tab<-merge(tab,tab2,by=c("Device_ID","AM"),all.x = TRUE,all.y=TRUE)
rm(tab2)
##get total rows
tab$tot<-tab$N/tab$prop

##this is so crazy complicated that we have stupid NAs still
for(i in 2:nrow(tab)){
  if(tab$Device_ID[i]==tab$Device_ID[i-1]&is.na(tab$tot[i])){
    tab$tot[i]<-tab$tot[i-1]
    tab$year[i]<-tab$year[i-1]}
  else{
    if(tab$Device_ID[i]==tab$Device_ID[i+1]&is.na(tab$tot[i])){
      tab$tot[i]<-tab$tot[i+1]
      tab$year[i]<-tab$year[i+1]}
  }}
rm(i)
##success omg
AMsum<-ddply(tab,c("AM"),summarize,spd=mean(sp,na.rm=T),count=sum(N),tot=length(AM_move$sp),pro=mean(prop),ci=qt(0.975,df=count-1)*sd(prop)/sqrt(count),ciS=qt(0.975,df=count-1)*sd(sp,na.rm=T)/sqrt(count))
rm(tab)
library(ggplot2)
ggplot(AMsum, aes(x=as.factor(AM), y=pro,fill=AM)) +
  geom_bar(aes(colour=AM),position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=pro-ci, ymax=pro+ci),
                width=.2,
                position=position_dodge(.9))+
  scale_y_continuous(breaks=0:35*0.15,lim=c(0,0.745))+
  scale_x_discrete(labels=c("Day","Night"))+
  ylab("Prop. of travel on human linear features")+
  xlab("Time of day")+
  theme_bw()+theme(legend.position = "none",
                   axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))

##DO THEY ALSO TRAVEL FASTER ON THEM?
ggplot(AMsum, aes(x=as.factor(AM), y=spd,fill=AM)) +
  geom_bar(aes(colour=AM),position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=spd-ciS, ymax=spd+ciS),
                width=.2,
                position=position_dodge(.9))+
  scale_y_continuous(breaks=0:105*10,lim=c(0,54))+
  ylab("Speed on human linear features (m/min)")+
  scale_x_discrete(labels=c("Day","Night"))+
  xlab("Time of day")+
  theme_bw()+theme(legend.position = "none",
                   axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))
