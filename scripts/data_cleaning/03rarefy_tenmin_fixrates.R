# To compare apples with apples, subset 10 min. fix rates to 30 min.

# Load required packages
library(dplyr)
library(data.table)

# Identify which columns ran on 10 min fix rates
fast.fixes <- telem %>%
  group_by(Device) %>%
  filter(FixRate<=35) %>% # This is the subset that will be applied when calculating movement metrics
  summarise(
    obs = length(Device),
    start.date = min(DateTime),
    end.date = max(DateTime),
    mean.fix = mean(FixRate,na.rm=TRUE), # first row is NA (no previous fix)
    sd.fix = sd(FixRate,na.rm=TRUE),
    max.fix = max(FixRate,na.rm=TRUE), 
    min.fix = min(FixRate,na.rm=TRUE)
  ) %>% 
  arrange(mean.fix)

ten.min.id <- unique(fast.fixes %>% 
  filter(mean.fix < 29))$Device
ten.min.id<-droplevels(ten.min.id)
rm(fast.fixes)

ten.min <- telem %>% 
  filter(Device %in% ten.min.id)

rm(ten.min.id)

# Figure out start date of each collar
date.range <-ten.min %>%
  group_by(Device) %>%
  summarise(
    start.datetime = min(DateTime),
    end.datetime = max(DateTime),
    start.date = min(Date),
    end.date = max(Date),
    no_of_days = ceiling(difftime(end.datetime, start.datetime,units="days"))
  )

# date.range$Device <- droplevels(date.range$Device)

## Code adapted from:
## https://stackoverflow.com/questions/39282749/r-how-to-join-two-data-frames-by-nearest-time-date
## With insight from: https://www.r-bloggers.com/understanding-data-table-rolling-joins/

# based on collar start time, generate "perfect" time stamp distribution
# length.out = no_of_days * 48 fixes per day
# this is the number of fixes we should have every day if collars operated on 30 min fix rate

# Device ID 32261 requires a special fix because it has 10 min data in both 2013 and 2014

for (i in 1:nrow(date.range)) {
  device = as.character(date.range$Device[i])
  
  if (device == "32261") {
    start.date <-
      paste(date.range$start.date[i], "09:00:00", sep = " ")
  }
  else {
    start.date <-
      paste(date.range$start.date[i], "00:00:00", sep = " ")
    }
  
  seq.dates <-
    seq(as.POSIXct(start.date), by = "30 mins",length.out = as.numeric(date.range$no_of_days[i]) * 48)
  target <- CJ(device, seq.dates)
  
  if (device == "32261") {
    target <- target %>% 
      filter(V2 < "2013-04-01 00:00:00" | V2 >= "2014-01-10 00:00:00")
  }
  else {
  }
  
  if (i == 1) {
    target_dates <- target
  }
  else {
    target_dates <- rbind(target_dates, target)
  }
}

# Clean up workspace
rm(device,i,seq.dates,start.date,target)

# Rename columns
target_dates <- target_dates %>% 
  rename(target.id = V1, target.time = V2)
target_dates$target.id = as.factor(target_dates$target.id)

# Check to see if this worked
target_dates %>% 
  group_by(target.id) %>% 
  summarize(obs = length(target.time),
            start.date = min(target.time),
            end.date = max(target.time))

# Join idealized target_dates with ten.min telemetry subset
ten.min <- data.table(ten.min)
target_dates <- data.table(target_dates)
ten.min[, date_time:=DateTime]
target_dates[, target_time:=target.time]
setkey(ten.min, Device, date_time)
setkey(target_dates, target.id, target_time)

subset.data <- ten.min[target_dates, roll="nearest"]
# row number roughly 1/3 of ten minute data subset
# there are some NAs created here because >30 min fix rates and imprecision of seq.dates
# use Device and UID to create a unique variable that allows for removal of duplicate entries
subset.data <- subset.data %>% 
  mutate(unique_var = paste(UID,Device,sep="_")) %>% 
  distinct(unique_var,.keep_all=TRUE)

# Remove extra columns created during these operations
names(subset.data)
subset.data <- subset.data %>% 
  select(-c(date_time,target.time,unique_var))

ten.min <- select(ten.min, -date_time)

# create subset from telem file that includes everything except ten.min
# merge this subset with subset.data
telem.wo.ten <- dplyr::setdiff(telem,ten.min)
# check
nrow(telem) - nrow(ten.min) == nrow(telem.wo.ten)

subset.data <- as.data.frame(subset.data)
tel.fixr30 <- union(telem.wo.ten,subset.data)
tel.fixr30$Device<-as.factor(tel.fixr30$Device)
# nrow(telem.wo.ten) + nrow(subset.data) == nrow(tel.fixr30)
tel.fixr30 <- tel.fixr30 %>% 
  group_by(Device) %>% 
  arrange(Device,UID)

# Clean up workspace
rm(date.range, subset.data,target_dates,telem,telem.wo.ten,ten.min)
