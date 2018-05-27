##rerun w/o log to get raw values
library(lme4)
ModelPM<-lmer(speed ~ Storm2+(1|Device_ID),data=tdatPM,REML=T)
ranef(ModelPM)
##33668 closer to 0 - this is the one we will pick for our graph

dev94<-data.frame(Storm2=c("control","M2","M1","D0","P1","P2","P3"),Device_ID="33668")
dev94$pred<-predict(ModelPM,dev94)

##bootstrap
library(ez)
preds<-ezPredict(ModelPM,to_predict=dev94,boot=T)
preds1<-preds[[2]]
dev94

###order preds1 by ascending
preds1<-preds1[order(preds1[,"Storm2"],preds1[,"value"]),]
####your CIs are the 97.5% and 2.5% 

##find CIs
uni=as.factor(unique(preds1$Storm2))
plotdata<-data.frame()
for (i in 1:length(uni)){
  j<-subset(preds1,Storm2==uni[i])$value
  low<-sort(j)[25]
  hi<-sort(j)[975]
  mean<-mean(j)
  plotdata<-rbind(plotdata,data.frame(Storm2=uni[i],low=low,hi=hi,mean=mean))
}
rm(low,hi,mean,uni,preds,i,j,dev94)


##reorder storm cats
library(plyr)
plotdata$Storm2<-mapvalues(plotdata$Storm2, from=c("control","M2","M1","D0","P1","P2","P3"), to=c("Control", "-2","-1", "0","+1","+2","+3"), warn_missing = TRUE)
plotdata$Storm2<-ordered(plotdata$Storm2, levels = c("Control", "-2","-1", "0","+1","+2","+3"))

library(ggplot2)
plot_speedPM<-ggplot(plotdata, aes(x=as.factor(Storm2), y=mean)) + 
  geom_errorbar(aes(ymin=low, ymax=hi),
                width=.1)+
  geom_point(size=2.75,pch=21,aes(fill=Storm2, colour="black"))+
  xlab("Snowfall category")+
  geom_vline(xintercept = 1.5, colour="gray57", linetype = "longdash")+
  ylab("Travel speed (m/min)") +
  scale_y_continuous(breaks=0:40*4,limit=c(15.75,36))+
  scale_fill_manual(values=c("black","royalblue","royalblue","royalblue","royalblue","royalblue","royalblue"))+
  scale_colour_manual(values="black")+
  theme_bw(base_size=10)+theme(legend.position = "none",
                               axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))

##save
#ggsave(filename="scripts/snowfall/omit_closed/thesis-final/snowfall_PMtravel.png",dpi=800,units="cm",height=10,width=15)


rm(plotdata,preds1)


