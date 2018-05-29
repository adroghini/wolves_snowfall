# Load required packages
library(suncalc)
library(dplyr)

# Calcule sunrise/sunset times
# Round to nearest .1 decimal point or else getSunlightTimes takes a while to run
# Create rounded lat / lon column
tel.fixr30 <- tel.fixr30 %>% 
  mutate(lat = round(Latitude,digits=1),lon = (round(Longitude,digits=1)*-1)) # getSunlightTimes specifies + as west?

# Obtain sunrise/sunset times
sunset.df <- tel.fixr30 %>% 
  ungroup(Device) %>% 
  select(Date,lat,lon) %>% 
  mutate(date = Date - 1) %>%
  distinct()

sunset.df$date <- as.Date(sunset.df$date, format="%Y-%m-%d",tz="Etc/GMT-7")

sunset.df <- getSunlightTimes(data = sunset.df, 
                 keep = c("sunset","sunrise"),
                 tz = "Etc/GMT-7")

sunset.df <- select(sunset.df, -date)

# Left join to append sunrise/sunset times to tel.fixr30
test <- left_join(tel.fixr30,sunset.df,by=c("Date"="Date","lat"="lat","lon"="lon"))

test$time_of_day <- "NA"
test <- test %>% 
  mutate(time_of_day=ifelse(DateTime >= sunrise & DateTime < sunset, "day", "night"))

# Check that results make sense
range(test$sunrise)
range(test$sunset)
table(test$Hour,test$time_of_day)

tel.fixr30 <- test %>% 
  select(-c(lat,lon))

rm(sunset.df, test)
