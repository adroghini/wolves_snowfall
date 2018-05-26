##SNOW DEPTH SUMMARIES
##generated cams (cams2013,cams2014) data for this

library(plyr)
cams<-subset(cams,month!=4)
Z<-ddply(cams,c("year","month"),summarize,snow=mean(SnowDepth,na.rm=T),std=sd(SnowDepth,na.rm=T),diff=mean(dfs,na.rm=T),N=length(unique(Camera)),ci=qt(0.975,df=N-1)*std/sqrt(N),se=std/N)
mean(subset(Z,year==2014)$snow)
Z <- transform(Z, month = month.abb[month])
Z$year<-as.factor(Z$year)
Z$monyr<-paste(Z$month,Z$year,sep=" ")
Z$monyr<-ordered(Z$monyr, levels = c("Jan 2013","Feb 2013","Mar 2013","Jan 2014","Feb 2014","Mar 2014"))
#Z<-subset(Z,year==2013)

library(ggplot2)
ggplot(data=Z, aes(x=monyr, y=snow, group=year,colour=year)) +
  geom_line() +
  geom_point(size=3)+
  geom_errorbar(aes(ymin=snow-ci, ymax=snow+ci),
                width=.1)+
  xlab("Time")+
  ylab("Snow depth (cm)")+
  scale_colour_manual(values=c("firebrick2","royalblue"))+
  scale_y_continuous(limit=c(35,61),breaks=0:80*5)+
  theme_bw(base_size=10)+theme(legend.position = "none",
                               axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),panel.grid.minor=element_blank())
ggsave(filename="graphs/snowcams_time.png",dpi=800,units="cm",height=10,width=15)
