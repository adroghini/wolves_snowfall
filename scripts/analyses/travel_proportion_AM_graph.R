##for prop test
library(lme4)

###why not set ref as 0 if that's what you're interested in?
##where's the AM subset here??
binomAM<-subset(binomdt,AM=="day")
binomAM<-within(binomAM, Storm2 <- relevel(Storm2, ref = "control"))
ModelBPM<-glmer(pmove~Storm2+(1|Device_ID),data=binomAM, family=binomial)
ranef(ModelBPM)
dev94<-data.frame(Storm2=c("control","M2","M1","D0","P1","P2","P3"),Device_ID="32263")
dev94$pred<-predict(ModelBPM,dev94)


##bootstrap
library(ez)
preds<-ezPredict(ModelBPM,to_predict=dev94,boot=T)
preds1<-preds[[2]]
preds1$V2<-exp(preds1$value)/(exp(preds1$value)+1)
#dev94

###order preds1 by ascending
preds1<-preds1[order(preds1[,"Storm2"],preds1[,"value"]),]
####your CIs are the 97.5% and 2.5%

##find CIs
uni=as.factor(unique(preds1$Storm2))
plotdata<-data.frame()
for (i in 1:length(uni)){
  j<-subset(preds1,Storm2==uni[i])$V2
  low<-sort(j)[25]
  hi<-sort(j)[975]
  mean<-mean(j)
  plotdata<-rbind(plotdata,data.frame(Storm2=uni[i],low=low,hi=hi,mean=mean))
}
rm(low,hi,mean,uni,preds,i,j,dev94)


##reorder cats
library(plyr)
plotdata$Storm2<-mapvalues(plotdata$Storm2, from=c("control","M2","M1","D0","P1","P2","P3"), to=c("Control", "-2","-1", "0","+1","+2","+3"), warn_missing = TRUE)
plotdata$Storm2<-ordered(plotdata$Storm2, levels = c("Control", "-2","-1", "0","+1","+2","+3"))

library(ggplot2)
plot_propAM<-ggplot(plotdata, aes(x=as.factor(Storm2), y=mean)) +
  geom_errorbar(aes(ymin=low, ymax=hi),
                width=.1)+
  geom_point(size=2.75,pch=21,aes(fill=Storm2, colour="black"))+
  xlab("Snowfall category")+
  geom_vline(xintercept = 1.5, colour="gray57", linetype = "longdash")+
  ylab("Proportion of time spent travelling") +
  #scale_y_continuous(breaks=0:0.5*0.05,limit=c(0,0.5))+
  scale_fill_manual(values=c("black","goldenrod","goldenrod","goldenrod","goldenrod","goldenrod","goldenrod"))+
  scale_colour_manual(values="black")+
  theme_bw(base_size=10)+theme(legend.position = "none",
                               axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),panel.grid.minor=element_blank())


#ggsave(filename="scripts/snowfall/omit_closed/thesis-final/binom_AMpropsteps.png",dpi=800,units="cm",height=10,width=15)

rm(plotdata)
