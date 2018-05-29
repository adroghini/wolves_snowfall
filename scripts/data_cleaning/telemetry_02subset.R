# Load packages
library(dplyr)

# Subset to only include collars that have fix rates of 30 mins. or less
## Fast fix rates are required for fine-scale movement metrics
# The following Device IDs were programmed to have fast fix rates
fast.ids <- read.csv('data/fast_collars_schedule.csv',stringsAsFactors = FALSE)
ids.to.keep <- unique(fast.ids$Collar_ID)
# N = 18 collars with fast fix rates
# 9 in 2014
# 9 in 2013
# 3 ran for both years

# Subset to only include date/time that corresponds with our remote camera (snowfall) data
# Jan. - Mar. 2013 and 2014

# Fast fixes only start on January 10

telem <- telem %>%
  filter(Device %in% ids.to.keep & Year > 2012 & ( (Month == 1 & Day >= 10) | (Month > 1 & Month < 4)))

rm(ids.to.keep, fast.ids)

# Some collars failed or had only a few observations 
# Identify and remove these collars

# Create unique row number for each device, according to date/time
telem <- telem %>% 
  group_by(Device) %>% 
  arrange(DateTime) %>% 
  mutate(UID = row_number()) %>% 
  select(UID,everything())

telem <- arrange(telem,Device,UID)

# Calculate actual fix rate
telem$FixRate <- NA
telem$FixRate <- telem$FixRate

for (i in 2:nrow(telem)) {
  if (telem$Device[i] == telem$Device[i - 1]) {
    telem$FixRate[i] <-
      difftime(telem$DateTime[i], telem$DateTime[i - 1], units = "mins")
  }
}

rm(i)

collar.summary <- telem %>%
  group_by(Device) %>%
  summarise(
    obs = length(Device),
    start.date = min(DateTime),
    end.date = max(DateTime),
    mean.fix = mean(FixRate,na.rm=TRUE), # first row is NA (no previous fix)
    sd.fix = sd(FixRate,na.rm=TRUE),
    max.fix = max(FixRate,na.rm=TRUE), 
    min.fix = min(FixRate,na.rm=TRUE)
  )
write.csv(collar.summary,'data/collar_summary.csv',row.names=FALSE)
rm(collar.summary)

# I used results from the collar.summary table to inspect collars for failure
# and missed fixes. In general, our collars performed very well, and most of the issues I found (by subsetting, for each Device, the rows which had
# a FixRate>35) had to do with the collars ending their fast fix schedule a few days before the end of March.

# There was only one device that I decided to exclude completely because of poor performance.
# Device 13793 had a mean fix rate of 33.0 (range: 0.66 to 3749) and a standard deviation of 192.
# It also only ran for two weeks (N = 954) at the beginning of January 2014 before dying.

# Remove Device 13793
telem <- telem %>% 
  filter(Device != 13793)

### ------------------------- ###

# Summary of other issues I discovered

# Devices: 32261, 33674, 33672 have data for two years
# Causes huge jump in FixRate as you go from March 31 2013 to Jan 10 2014
# Set FixRate of first row of 2014 to NA

# Device 33668 has one observation (UID = 1976) with a FixRate of 570. 
# Device 33675 UID 2011 has a FixRate of 510.
# Device 33673 UID 1976 has a FixRate of 569.
# Device 33671 UID 1975 & UID 1995 FixRates > 500
# Device 33669 has a few (N=6) rows with FixRates > 35
# Device 33674 has N = 6 rows for which FixRates > 35 
# Device 32261 final row (UID = 14713) has FixRate of 730 - end of collar data, consistent 10 min. data up until (including) March 30
# Devices: 32254, 33678, 32263, 13794. Fast fix rate ends on 2014-03-29; fix rate falls to 720+ minutes after 03-29

### ------------------------- ###

# Based on these patterns I will ignore all data for which FixRate > 59 (N = 53)
# nrow(subset(telem,FixRate>59)) 
# Note there are only 2 observations between 30 and 59 (40 min. fix rates)

# To do this I will manually set FixRate == NA if current fix rate is > 59
# So that speed is not calculated between this & previous point
# During implementation of movement metric script

telem <- telem %>% 
  mutate(FixRate.NA = ifelse(FixRate > 59,
                             NA,FixRate))
# Check - N = 53
# nrow(subset(telem,is.na(FixRate.NA)&!is.na(FixRate)))