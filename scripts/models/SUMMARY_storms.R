##summary stats
##using your definition of 5cm snowstorms
##remember that graph w/ # of snowstorm events ~ # of individuals

##1. how many snowstorms events did you identify in all?
library(plyr)
Z<-subset(tdatB,Storm2=="D0")
Z1<-ddply(Z,c("Device_ID","newDate"),summarize,dif=mean(diff))
rm(Z)
##26 snowstorms | IDs, not all unique dates

Z1$Device_ID<-factor(Z1$Device_ID)

##how many unique dates?
length(unique(Z1$newDate)) ##15
length(unique(Z1$Device_ID))

##by year
library(lubridate)
Z1$year<-year(Z1$newDate)
table(Z1$year,Z1$Device_ID) ##6 IDs 2013, 4 in 2014
##average number of snowstorms per individual? 1 in 2014
##2013 avg: (3+4+2+1+6+6)/6

##how many snowstorms were shared by >1 ID?
Z2<-ddply(Z1,~newDate,summarize,N=length(Device_ID))
nrow(subset(Z2,N==1)) ##5 of the 12 only experienced by 1 ID
Z2$year<-year(Z2$newDate)
Z2$month<-month(Z2$newDate)

##1 snowstorm per every X days?
Z2$diff<-NA
for (i in 2:nrow(Z2)){
  if(Z2$year[i]==Z2$year[i-1]){
    Z2$diff[i]<-difftime(Z2$newDate[i],Z2$newDate[i-1],units=c("days"))}}
mean(subset(Z2,year==2013)$diff,na.rm=T)
mean(subset(Z2,year==2014)$diff,na.rm=T)
nrow(subset(Z2,year==2013))

Z2<-ddply(Z2,c("N","year"),summarize,days=length(newDate))
Z2<-ddply(Z2,c("month"),summarize,days=length(newDate))


##level so 2013 is on top
Z2$year<-as.factor(Z2$year)
Z2<-within(Z2, year <- relevel(year, ref = "2014"))

library(ggplot2)
ggplot(Z2, aes(x=as.factor(N), y=days,fill=as.factor(year))) +
  geom_bar(stat="identity",width=0.72)+
   geom_bar(stat="identity",width=0.72,colour="grey25",show_guide=FALSE)+
  coord_flip()+
  xlab("Number of wolves affected") +
  ylab("Number of snowstorm events") +
  scale_y_continuous(breaks=0:6*1,limit=c(0,5),expand=c(0,0))+
  #scale_x_continuous(breaks=0:6*1)+
  scale_fill_manual(values=c("grey75","tomato"),name="Year")+
  theme_bw(base_size=10)+theme(legend.justification=c(1,1), legend.position=c(0.95,0.83),
                               axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),panel.grid.minor=element_blank())
ggsave(filename="graphs/snowfall_hist.png",dpi=800,units="cm",height=10,width=15)


rm(Z1,Z2)
rm(i)