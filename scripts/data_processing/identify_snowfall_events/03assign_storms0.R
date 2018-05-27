##what now?
##clean up trun and then start assigning new storm categories
trun<-subset(trun,select=-c(lineno,year,month,day,hour,Date.x,Date.y,Ncams)) 

##assign 0 storm category and this should match perfectly in all but 4 cases (3 false positives, 1 false negative detected with new difference category)
trun$Storm2<-"none"
trun$Storm2[trun$diff>=5]<-"D0" #was diff.y if using snowdata file
table(trun$Storm2)

write.csv(trun,"/Users/amanda_droghini/Desktop/University of Alberta/THESIS/Analyses/chapter2/temp/scripts/snowfall/repeat_analysis/storm0cat.csv",row.names=F)

##this is working
##quick check here i am of course concerned about stats
summary(subset(trun,behav=="move"&AM=="night"&Storm2=="D0")$speed) ##slight shift to lower speed but seems about the same??
#summary(subset(trun,behav=="move"&AM=="night"&storm=="0")$speed) no longer have this with data2013 file
