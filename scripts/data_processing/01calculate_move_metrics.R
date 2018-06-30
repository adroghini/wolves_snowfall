# Load required libraries
library(plyr)
library(dplyr)
library(mixtools)

# Calculate step length
# Thanks to Elie Gurarie for the code
# http://faculty.washington.edu/eliezg/teaching/AniMove2014/Basics/BasicsPartI_Document.html
# Note: Speed is not calculated if FixRate > 59
tel.fixr30 <- tel.fixr30 %>% 
  arrange(Device,UID) %>%
  group_by(Device) %>%
  mutate(z=Easting+1i*Northing, 
         steps=c(NA,diff(z)), #NA for 1st step length
         steplength_m=ifelse(is.na(FixRate.NA), NA, Mod(steps))) %>%
  select(-c(z,steps))

# Step length is in meters

# Create backup file in case something gets overwritten
# backup <- tel.fixr30

# Recalculate fix rate now that 10 min data have been subset
tel.fixr30$NewFixRate <- tel.fixr30$FixRate.NA

for (i in 2:nrow(tel.fixr30)) {
  if (tel.fixr30$Device[i] == tel.fixr30$Device[i - 1]) {
    if(is.na(tel.fixr30$FixRate.NA[i])){
      tel.fixr30$NewFixRate[i]<-tel.fixr30$FixRate.NA[i]
    }
    else {
      tel.fixr30$NewFixRate[i] <-
      difftime(tel.fixr30$DateTime[i], tel.fixr30$DateTime[i - 1], units = "mins")
    }
  }
}

rm(i)

# Check what happened to the NAs
# View(subset(tel.fixr30,is.na(FixRate.NA)))

# Remove superfluous FixRate column
tel.fixr30 <- tel.fixr30 %>% 
  select(-FixRate)


# Thinning script isn't perfect at the tail ends of the sequences we generated
View(subset(tel.fixr30,NewFixRate>35))

# It also runs into problems when encountering big jumps in fixes
# Replace these with NAs

# UID = 4274 from Device = 33676. Last fix rate was at 16:10 and the one just before was at 16:00
tel.fixr30$NewFixRate[tel.fixr30$Device==33676 & tel.fixr30$UID==4274]<-NA
tel.fixr30$steplength_m[tel.fixr30$Device==33676 & tel.fixr30$UID==4274]<-NA

# UID = 2472 from Device = 32261
# Fixes jump from 15:00 and 15:10 to 23:00
tel.fixr30$NewFixRate[tel.fixr30$Device==32261 & tel.fixr30$UID==2472]<-NA
tel.fixr30$steplength_m[tel.fixr30$Device==32261 & tel.fixr30$UID==2472]<-NA

# Summarize results
tel.fixr30 %>%
  filter(!is.na(NewFixRate)) %>%
  ungroup(Device) %>%
  summarize(
    mean.fix = mean(NewFixRate),
    max.fix = max(NewFixRate),
    min.fix = min(NewFixRate),
    sd.fix = sd(NewFixRate)
  )

# Calculate speed
tel.fixr30 <- tel.fixr30 %>% 
  mutate(speed = steplength_m/NewFixRate) # in meters per minute

# Calculate resting/traveling breakpoint
# Thanks to Melanie Dickie for the idea

# segments<-data.frame(log(tel.fixr30$speed+0.01)))
# log base for R is exp(1)
# plot using base 10 - addressing reviewer comments
# re: clarification of breakpoint
segments<-data.frame(log10(tel.fixr30$speed+0.01)) ##add constant or else 0 = -INF
colnames(segments)[1]<-"log.speed"

# Remove NAs for function to work
segments <- segments %>% 
  filter(!is.na(log.speed))

breakpoint_model <- normalmixEM(segments$log.speed, k=2, epsilon = 1e-03, fast=TRUE)
range(segments$log.speed) # for plotting parameters

# Plot
# Save plot for Supplementary Information
png("figures/S2_speed_breakpoint.png",res=800,units="cm",height=16,width=20)
plot.new()
# plot.window(xlim=c(-5,5.5), ylim=c(0,0.3))
plot.window(xlim=c(-2,2.32), ylim=c(0,0.7))
plot(breakpoint_model,which=2,add=TRUE)
# box(); axis(1,at = seq(-5, 5, by = 1), ); axis(2); 
box(); axis(1,at = seq(-2, 3, by = 0.5), ); 
axis(2); 
title(xlab="Log of speed (m/min)", ylab="Density")
# abline(v=0.5,lty=2,lwd=2,col="black")
abline(v=0.228,lty=2,lwd=2,col="black")
dev.off()

# Use locator() tool to estimate breakpoint

# Read in antilog function and back-transform estimated breakpoint
# To express in original units (m/min)
# Inverse log function from:  http://r.789695.n4.nabble.com/Searching-for-antilog-function-td4721348.html
source('scripts/functions/antilog_function.R')

# breakpoint <- antilog(0.5,exp(1))
breakpoint <- antilog(0.228,10)

# Categorize behaviour based on speed
tel.fixr30$Behavior <- NA
tel.fixr30$Behavior[tel.fixr30$speed<breakpoint] <- 0 # resting
tel.fixr30$Behavior[tel.fixr30$speed>=breakpoint] <- 1 # travelling

rm(antilog, breakpoint,segments,breakpoint_model)

