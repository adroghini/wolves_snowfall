# Load required packages
library(dplyr)

# Create new "Camera Day" column
# Before joining snow depth with telemetry data, need to create a common date
# We programmed cameras to take pictures every day at noon
# So if a camera detects a snowfall on e.g. January 1 at 12:00:00, it means
# that the snowfall took place sometime between December 31 at > 12:00:00 and
# January 1 <= 12:00:00

# To make sure that the movement metrics we are calculating for wolves =
# the date at which a snowfall was detected, create new CamDate column
# that will match the "camera day" resolution
tel.fixr30$CamDate <- as.Date(NA)
for (i in 1:nrow(tel.fixr30)) {
  
  if (tel.fixr30$Hour[i] >= 0 & tel.fixr30$Hour[i] < 12) {
    tel.fixr30$CamDate[i] <- tel.fixr30$Date[i]
  }   
  # If snowfall happens at or before 12:00:00, captured same day
  
  else if (tel.fixr30$Hour[i] >= 12) {
    tel.fixr30$CamDate[i] <- tel.fixr30$Date[i] + 1 
    # If snowfall happens after 12:00:00, it will only be captured next day
  }
  }

# For each wolf, obtain average location on any given Camera Day
mean.position <- tel.fixr30 %>% 
  arrange(Device,UID) %>% 
  group_by(Device,CamDate) %>% 
  summarise(mean.x=mean(Easting),mean.y=mean(Northing))

# Append pack ID
pack_to_id <- tel.fixr30 %>% 
  distinct(Device, .keep_all = TRUE) %>% 
  select(Device, Pack)

pack_to_id$Device <- as.factor(pack_to_id$Device)
mean_position$Device <- as.factor(mean_position$Device)

mean_position <- left_join(mean_position,pack_to_id)
rm(pack_to_id)
# write.csv(mean.position,'data/outputs/mean_position.csv',row.names=FALSE)

# Extract coordinates for cameras
cam.locations <- cams.deploy %>% 
  distinct(camera, .keep_all = TRUE)

# Append coordinates of camera locations to cams.depth
cams.depth <- cams.depth %>%
    left_join(cam.locations, by = c("Camera" = "camera")) %>%
    dplyr::select(-c(deploy.date, measure_type, measurement, sample_no)) %>%
  

# For each daily mean position, 
# Find cameras for which snow accumulation data are available
# Remove NAs of snow accumulation
test <- left_join(mean.position,cams.depth,by=c("CamDate"="Date")) %>% 
  filter(!is.na(SnowAccum)) %>% 
  dplyr::select(-c(Time,DateTime,RowID))

# Calculate "step length" between wolf daily position (mean.x, mean.y) and
# snow depth camera (easting, northing)
# Code from Elie
# http://faculty.washington.edu/eliezg/teaching/AniMove2014/Basics/BasicsPartI_Document.html

test <- test %>% 
  arrange(Device,CamDate) %>%
  group_by(Device) %>%
  mutate (zwolf = mean.x + 1i*mean.y, zcams = easting +1i*northing)

test$steps <- NA + 1i # must be complex number
for (i in 1:nrow(test)) {
  test$steps[i] <- diff(c(test$zwolf[i], test$zcams[i]))
}

# Take absolute value expressed in kilometers
test <- mutate(test,dist.from.cams=Mod(steps/1000)) 

# Check answers in ArcGIS
# Export camera locations
write.csv(cam.locations,'data/outputs/cam_locations.csv',row.names=FALSE)
# Export 'test' file
write.csv(test,'data/outputs/test_distances.csv',row.names=FALSE)

# Distances check out
# Cue: https://www.youtube.com/watch?v=1s1LJbzRulM

# For each wolf & date, select minimum distance
test <- test %>% 
  group_by(Device,CamDate) %>% 
  filter(dist.from.cams==min(dist.from.cams))

mean.position <- test %>% 
  dplyr::select(-c(zwolf,zcams,steps))

rm(i, test)

# Get weird uninitialized columns error
tel.fixr30 <- tel.fixr30 %>% 
  ungroup(Device)
mean.position <- as.data.frame(mean.position)
tel.fixr30 <- as.data.frame(tel.fixr30)

# Join mean.position file back into tel.fixr30
test <- left_join(tel.fixr30, mean.position, 
                  by = c("Device"="Device", 
                         "CamDate"="CamDate"))

# Snow depth is not available for each observation
# Will need to be subset
nrow(subset(test,is.na(SnowAccum)))

# Clean up workspace
telem_snow <- test
backup <- tel.fixr30
rm(test,mean.position,cams.depth,cam.locations,cams.deploy,tel.fixr30)

