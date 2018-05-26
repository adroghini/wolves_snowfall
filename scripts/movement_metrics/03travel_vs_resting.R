##defining a breakpoint in speed distributions: slow vs. fast travel
##using methods as outlined in Melanie's thesis - segmented package
segs<-data.frame(log(0.01+test2$speed)) ##add constant or else 0 = -INF?
colnames(segs)[1]<-"log_sp"

####WHAT I ACTUALLY DID##
library(mixtools)
W<-normalmixEM(segs$log_sp, k=2, epsilon = 1e-03, fast=TRUE)
summary(W)

png("graphs/bimode_hist.png",res=800,units="cm",height=15,width=20)
plot.new()
plot.window(xlim=c(-6,6), ylim=c(0,0.3))
plot(W,which=2,add=TRUE) #xaxp  = c(0, 6, 50),xlim=c(0.48,0.5))
box(); axis(1); axis(2); title(xlab="Log of speed (m/min)", ylab="Density")
abline(v=0.5,lty=2,lwd=2,col="black")
dev.off()

##uniroot(function(x) dnorm(x, mean=-1.666274, sd=0.948155)-dnorm(x, mean=2.658749, sd=1.135727), c(0, 1)) nope doesn't work
locator()

#######################WRONG
#WRONG##################
##breakpoint value is 0.5
exp(0.5) ####BASE 10
#######OH WE KNOW THIS WRONG
## = 1.65 m/min = 0.1km/hr

##add column behav.
test2$behav<-"NA"
test2$behav[test2$speed<1.648721]<-"rest" #####FIX
test2$behav[test2$speed>=1.648721]<-"move" #####FIX
