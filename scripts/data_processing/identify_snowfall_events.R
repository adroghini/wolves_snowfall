# Load required packages
library(dplyr)
library(tidyr)

# Create snowfall categories
# If SnowAccum >= 5, we want that "CamDate" to be coded as "snowfall"
# From there, assign values to 1 and 2 days before the snowfall
# And to 1, 2, and 3 days after

# Isolate snowfall events
snowfall_events <- telem_snow %>% 
  filter(SnowAccum >= 5) %>% 
  # distinct(Camera,CamDate,.keep_all=TRUE) %>% # 35 unique snowstorms
  distinct(Device,CamDate,.keep_all=TRUE) %>% # 57 total observations
  select(Camera,CamDate,SnowAccum,SnowDepth,Device) %>% 
  dplyr::arrange(CamDate)

length(unique(snowfall_events$Device)) # Across 17 collars

# Compute summary statistics
# How many snowfall events did each wolf experience
summary <- snowfall_events %>% 
  group_by(Device) %>% 
  summarize(no_of_snowfall = length(unique(CamDate)))
mean(summary$no_of_snowfall) # x = 3.35 snowfall events per wolf 
sd(summary$no_of_snowfall) # sd = 2.17
range(summary$no_of_snowfall) # range = 1 to 7
rm(summary)

# Issues:
# Device IDs 33670, 33673, and 33675 experienced a snowfall 
# on two consecutive days: 2013-03-21 and 2013-03-22

# Snowfalls on 2013-01-18 and 2013-01-24 have overlapping dates if
# selecting from three days before to three days after

# Selecting from two days before to three days after would avoid overlap

snowfall_events <- snowfall_events %>% 
  mutate(two_before = CamDate - 2, 
         one_before = CamDate - 1, 
         one_after = CamDate + 1, two_after = CamDate + 2, 
         three_after = CamDate + 3)

# For 03-21 and 03-22, code both days as "snowfall", and start 
# working backwards from 2013-03-21 and forwards from 2013-03-22

for (i in 1:nrow(snowfall_events)) {
  if (snowfall_events$CamDate[i] == "2013-03-21"){
    snowfall_events$one_after[i] = snowfall_events$CamDate[i] + 2
    snowfall_events$two_after[i] = snowfall_events$CamDate[i] + 3 
    snowfall_events$three_after[i] = snowfall_events$CamDate[i] + 4
  }
  else if (snowfall_events$CamDate[i] == "2013-03-22"){
    snowfall_events$one_before[i] = snowfall_events$CamDate[i] - 2
    snowfall_events$two_before[i] = snowfall_events$CamDate[i] - 3
  } else {
  
}
}

rm(i)

# Switch to long format
snowfall_events <- snowfall_events %>% 
  rename(day_of_snowfall = CamDate) %>% 
  tidyr::gather(key = snowfall_category, value = category_date,
                one_before,two_before, day_of_snowfall, one_after, 
                two_after, three_after) %>% 
  group_by(Device) %>% 
  dplyr::arrange(Device,category_date)

# Create controls
# Controls are three random dates that do not belong to any snowfall_category
# N = 3 to balance number of observations

# Create list of dates that *cannot* be used for controls
snowfall_dates <- snowfall_events %>% 
  dplyr::group_by(Device) %>% 
  dplyr::distinct(category_date, .keep_all=TRUE)
# Returns fewer rows than snowfall_events because of 2013-03-21 / 03-22 
rm(snowfall_events)

random_dates <- telem_snow %>% 
  filter(!is.na(speed)) %>% 
  filter(!is.na(SnowAccum)) %>% 
  group_by(Device) %>% 
  filter(!(CamDate %in% snowfall_dates$category_date)) %>% 
  sample_n(3,replace = FALSE) %>% 
  select(Device,CamDate)
# 3 observations * 17 wolves = 51 random dates

# Subset telemetry date to only include these random dates

# Add unique ID for easy filtering
random_dates$key <- paste(random_dates$Device,random_dates$CamDate,sep="_")
test <- telem_snow %>% 
  mutate(key = paste(Device,CamDate,sep="_"))

random_telem <- test %>% 
  filter(key %in% random_dates$key) %>% 
  select(-key)

rm(test)
random_telem$snowfall_category = "control"
random_dates$snowfall_category = "control"
# Check
# max(random_telem$SnowAccum) # Should not be >= 5 cm

# Subset telem_snow to only include snowfall_dates
# Then merge with random_telem

# Left join by Device and category_date = CamDate

# Remove repeating columns
snowfall_dates <- snowfall_dates %>% 
  select(-c(Camera,SnowAccum,SnowDepth))

snowfall_telem <- right_join(telem_snow,snowfall_dates,
                            by = c("Device" = "Device",
                                   "CamDate" = "category_date"))

snowfall_telem <- plyr::rbind.fill(snowfall_telem,random_telem)
unique(snowfall_telem$snowfall_category)

# Export category dates for reference
random_dates <- random_dates %>% 
  rename(category_date = CamDate) %>% 
  select(Device,snowfall_category,category_date)

snowfall_dates <- plyr::rbind.fill(snowfall_dates,random_dates)

write.csv(snowfall_dates,'data/outputs/snowfall_category_dates.csv',
          row.names=F)

rm(random_telem,snowfall_dates,random_dates)
