##max speed analysis
##can run quick ANOVA
##can use tdatB because max speed will obviously be contained with travel speed cat
library(plyr)
maxsp<-ddply(tdatB,c("Device_ID","Storm2","newDate"),summarize,mx=max(speed),mxl=max(logsp))
table(maxsp$Device_ID,maxsp$Storm2) ##all IDs have at least 1 obs. in each category, but i think you can probably collapse this?
maxsp<-ddply(tdatB,c("Device_ID","Storm2"),summarize,mx=max(speed),mxl=max(logsp)) 

###ASSUMPTIONS
library(car)
qqPlot(maxsp$mx,pch=20,ylab="Daily distance",xlab="Normal quantiles") ##raw values are fine
#qqPlot(maxsp$mxl,pch=20,ylab="Daily distance",xlab="Normal quantiles")

###SPHERICITY
library(ez)
maxsp$Storm2<-factor(maxsp$Storm2) ##drop unused factors as this is causing error messages
maxsp$Device_ID<-factor(maxsp$Device_ID)
ezANOVA(data=maxsp, dv=.(mx), wid=.(Device_ID), within=.(Storm2), detailed=TRUE)
##ok for spher.

options(contrasts=c("contr.sum","contr.poly"))
maxsp<-within(maxsp, Storm2 <- relevel(Storm2, ref = "control"))
library(nlme)
library(multcomp)
Model<-lme(mx ~ Storm2, random=~1|Device_ID,data=maxsp,method="REML")
anova(Model) ##marginal sig.
summary(glht(Model,linfct=mcp(Storm2="Tukey")))
#summary(Model)

ddply(maxsp,~Storm2,summarize,max=mean(mx))
max(subset(tdatB,Storm2=="M1")$speed)
max(maxsp$mx)

