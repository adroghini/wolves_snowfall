# Load required packages
library(dplyr)

# For each wolf, obtain average location on any given day
mean.position <- tel.fixr30 %>% 
  arrange(Device,UID) %>% 
  group_by(Device,Date) %>% 
  summarise(mean.x=mean(Easting),mean.y=mean(Northing))

#write.csv(mean.position,'data/outputs/mean_position.csv',row.names=FALSE)

# Extract coordinates for cameras
cam.locations <- cams.deploy %>% 
  distinct(camera, .keep_all = TRUE)

# Append coordinates of camera locations to cams.depth
cams.depth <- cams.depth %>%
    left_join(cam.locations, by = c("Camera" = "camera")) %>%
    select(-c(deploy.date, measure_type, measurement, sample_no))

# For each daily mean position, 
# Find cameras for which snow accumulation data are available
# Remove NAs of snow accumulation
test <- left_join(mean.position,cams.depth,by="Date")
test <- filter(test,!is.na(SnowAccum))

# Calculate "step length" between wolf daily position (mean.x, mean.y) and
# snow depth camera (easting, northing)
# Code from Elie
# http://faculty.washington.edu/eliezg/teaching/AniMove2014/Basics/BasicsPartI_Document.html

test <- test %>% 
  arrange(Device,Date) %>%
  group_by(Device) %>%
  mutate (zwolf = mean.x + 1i*mean.y, zcams = easting +1i*northing)

test$steps <- NA + 1i # must be complex number
for (i in 1:nrow(test)) {
          test$steps[i] <- diff(c(test$zwolf[i],test$zcams[i]))
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
  group_by(Device,Date) %>% 
  filter(dist.from.cams==min(dist.from.cams))

mean.position <- test %>% 
  select(-c(zwolf,zcams,steps))

rm(i, test)

# Join mean.position file back into tel.fixr30
test <- left_join(tel.fixr30, mean.position, 
                  by = c("Device" = "Device", "Date" = "Date"))


# Snow depth is not available for each observation
# Will need to be subset
nrow(subset(test,is.na(SnowAccum)))

# Output summary statistics
# How far away are cameras from wolves?
# How many cameras is linked to each wolf?
test %>% 
  group_by(Device) %>% 
  filter(!is.na(SnowAccum)) %>% 
  summarize(mean.dist = mean(dist.from.cams), 
            no_of_cams = length(unique(Camera)))

# Device ID 33674 does not have any cameras nearby
# Closest one is 97.0 km away
# Not sure whether to omit?
