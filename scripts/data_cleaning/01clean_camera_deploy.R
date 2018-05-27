# Load required packages
library(dplyr)
library(tidyr)
# Load camera data
deploy2013 <- read.csv('data/cameras/deploy2013.csv', stringsAsFactors = FALSE)
deploy2014 <- read.csv('data/cameras/deploy2014.csv', stringsAsFactors = FALSE)

# For deploy2013, remove cameras placed in "closed" habitats (canopy cover >30%)
deploy2013 <- deploy2013 %>% 
  filter(grepl("Close",PlotID)!=TRUE)
  
# Convert to long format 
# During initial deployment, 4 measurements for a) sinking depth ("cn") and 
# b) snow depth ("sn") were taken
# Can be used to summarize initial snow depth measurements
deploy2013 <- deploy2013 %>% 
gather(key = snow_condition, value = measurement,
       sn1,sn2,sn3,sn4,cn1,cn2,cn3,cn4) %>% 
  mutate(measure_type = substr(snow_condition,start=1,stop=2)) %>% 
  mutate(sample_no = substr(snow_condition,start=3,stop=3)) %>% 
  select(-snow_condition) %>% 
  mutate(measure_type = if_else(measure_type == "sn", "snow_depth",
                                "sinking_depth"))

# For deploy2014, restrict data to only those cameras for which a measurement
# pole was installed (allowing for snow depth readings)
# Remove camera Near3 which only had 10 pictures, all taken on the same day
# Repeat wide-to-long format conversion
# Sinking depth measurements were not taken in 2014
deploy2014 <- deploy2014 %>% 
  filter(MeasurePole=="Yes" & CameraStation!="Near3") %>% 
  gather(key = measure_type, value = measurement,
         SnowDepth1,SnowDepth2,SnowDepth3,SnowDepth4) %>% 
  mutate(sample_no = substr(measure_type,start=10,stop=10))

deploy2014$measure_type <- "snow_depth"

# Clean columns and merge into a single file
names(deploy2013)
deploy2013 <- select(deploy2013, -c(3,6:14))
names(deploy2014)
deploy2014 <- select(deploy2014, -c(1,4,7:16))

names(deploy2013)
deploy2013 <- rename(deploy2013,camera = PlotID,deploy.date=DeployDate,
                     easting=X,northing=Y)
deploy2013$deploy.date <- as.Date(deploy2013$deploy.date, 
                                  format="%y-%m-%d",tz="Etc/GMT-7")
deploy2013 <- deploy2013 %>% 
  select(camera:northing,measure_type,measurement,sample_no)

names(deploy2014)
deploy2014 <- rename(deploy2014,camera = CameraStation,
                     deploy.date=DateDeployed,
                     easting=UTMEast,northing=UTMNorth)
deploy2014$deploy.date <- as.Date(deploy2014$deploy.date, 
                                  format="%m/%d/%Y",tz="Etc/GMT-7")

cams.deploy <- rbind(deploy2013,deploy2014)

rm(deploy2013,deploy2014)