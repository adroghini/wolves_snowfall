# Output summary statistics
# For Results section

# Load required packages
library(plyr)
library(dplyr)

# How far away are cameras from wolves?
# How many cameras is linked to each wolf?
# Use snowfall_telem dataset, which only includes data with snowfall categories

cams.summary <- snowfall_telem %>% 
  group_by(Device) %>% 
  filter(!is.na(SnowDepth)) %>% 
  summarize(mean.dist = mean(dist.from.cams), 
            no_of_cams = length(unique(Camera))) %>% 
  arrange(mean.dist)

# Device ID 33674 does not have any cameras nearby
# Mean distance is 90.1 km away

# Summary statistics omitting "outlier"
mean(cams.summary[1:16,]$mean.dist)
sd(cams.summary[1:16,]$mean.dist)
range(cams.summary[1:16,]$mean.dist)

# Summarizing deployment length
cams.summary <- cams.depth %>% 
  group_by(Camera) %>% 
  filter(!is.na(SnowDepth)) %>% 
  summarize(deploy.length = length(unique(Date)))

mean(cams.summary$deploy.length)
sd(cams.summary$deploy.length)

# Describe snowfall events
# Use only snowfall_category == "day_of_snowfall"
day_of_snow <- subset(snowfall_telem, snowfall_category == "day_of_snowfall")

# How many snowfall events were recorded?
day_of_snow %>% 
  group_by(Year) %>% 
  summarize(no_of_devices = length(unique(CamDate))) # 19 unique dates

snow.tab <- day_of_snow %>% 
  group_by(Device) %>% 
  summarize(no_of_events = length(unique(CamDate)))
colSums(snow.tab) # For a total of 56 records across 17 individuals

# How many snowfall events did each wolf experience?
snow.tab <- day_of_snow %>% 
  # filter(!(is.na(SnowAccum) & is.na (SnowDepth))) %>% 
  group_by(Year, Device) %>% 
  summarize(no_of_events = length(unique(CamDate)))
snow.tab %>% 
  group_by(Year) %>% 
  summarize(mean_events_per_ID = mean(no_of_events), min_event = min(no_of_events),max_events = max(no_of_events))

# Were snowfall events experienced by many individuals?
day_of_snow %>% 
  group_by(CamDate) %>% 
  summarize(no_of_devices = length(unique(Device))) %>% 
  # filter(no_of_devices == 1) # 9 snowfall dates only experienced by a single individual (out of 19 total)
  filter(no_of_devices == 10) # Total individuals per year = 10. Only 1 snowfall event affected all IDs.

# Was snow different between years?
# Use entire snowfall_telem dataset
snowfall_telem %>% 
  filter(!(is.na(SnowAccum) & is.na (SnowDepth))) %>% 
  group_by(Year) %>% 
  summarize(mean_depth = mean(SnowDepth), sd_depth = sd(SnowDepth), max_depth = max(SnowDepth),
    mean_accum = mean(SnowAccum), sd_accum = sd(SnowAccum), max_accum = max(SnowAccum))

# What is the daily distance travelled by wolves on days of a snowfall compared to other days?
# Does decrease in travel speed "scale up" to a decrease in daily distance travelled?

# Generate dataframe
daily.dist <- snowfall_telem %>% 
  filter(!is.na(SnowDepth) & !is.na(speed)) %>% 
  group_by(Device,CamDate) %>% 
  summarize(distance = sum(steplength_m/1000))

events_by_id <- snowfall_telem %>% 
  distinct(Device, CamDate, .keep_all = TRUE) %>% 
  select(c(Device, CamDate, snowfall_category))

daily.dist <- left_join(daily.dist,events_by_id, by=c("Device" = "Device", "CamDate" = "CamDate"))

# Summarize across all individuals
daily.dist.mean <- daily.dist %>% 
  group_by(snowfall_category) %>% 
  summarize(mean.dist = mean(distance), st.dev = sd(distance), no.obs = length(distance))


# Clean workspace
rm(cams.summary, snow.tab, day_of_snow, daily.dist, events_by_id, daily.dist.mean)


