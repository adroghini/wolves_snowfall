# Set working directory
setwd('C:/Users/adroghini/Documents/wolves_snowfall/')

# Load required packages
library(dplyr)

# Load telemetry data
# This file was previously cleaned and modified by colleague Eric Neilson
telem <- read.csv("data/all_wolves_telem.csv",header=T,stringsAsFactors = FALSE)
head(telem)

# Clean up extra columns
names(telem)

# DateTimeSt column is local time
# Delete all other date/time strings

# Check if day_local, month_loca, etc. columns are the same as Day, Month, Hour, Minute, Second..
print(which(telem$Day %in% telem$day_local == FALSE))
print(which(telem$Month %in% telem$month_loca == FALSE))
print(which(telem$Hour %in% telem$hour_local == FALSE))
print(which(telem$Minute %in% telem$minute_loc == FALSE))
print(which(telem$Second %in% telem$second_loc == FALSE))

# Check if Device_ID and Device columns are the same
print(which(unique(telem$Device_ID) %in% unique(telem$Device) == FALSE))
# Device_ID is wrong. It looks like two IDs (32261 and 32263) have an extra '1' added at the end of them in the
# Device_ID column. Device is the correct one.

cols.to.delete <- c("FID","number","Device_ID","Date_Time_","Date_Time1","date_Local","day_local","month_loca",
                    "time_local","hour_local","minute_loc","second_loc","Altitude","Fix_Status","DOP",
                    "Temp_C","Main_V","Back_V","Beavon_V","Collartype","UTC_Date","UTC_Time","LMT_Date","LMT_Time",
                    "Origin","SCTS_Date","SCTS_Time","ECEFX_m","ECEFY_m","ECEFZ_m","Mort_Statu","Activity",
                    "F63","DateTime","LINENO","DTdiff","FixRate","ThreeHR","StudyArea","Season","WSCount","In99Sel",
                    "Use")


#Specify new column order
cols.order <- c("Device","Latitude","Longitude","POINT_X","POINT_Y","DateTimeSt","year_local",
               "Month","Day","Hour","Minute","Second","Pack")

# Drop unnecessary columns, reorder and rename
telem  <- telem %>%
  select(cols.order,-cols.to.delete) %>%
  rename(DateTime = DateTimeSt, Year = year_local,Easting = POINT_X, Northing = POINT_Y)

rm(cols.order,cols.to.delete)

# Specify proper column type
telem$Device <- as.factor(telem$Device)
telem$Pack <- as.factor(telem$Pack)
telem$Year <- as.integer(telem$Year)

# Create Date only column
telem$Date <- paste (telem$Year, telem$Month, telem$Day, sep = "-")

# Format Date/Time column
Sys.setenv(TZ="Etc/GMT-7") # because POSIXt is a nightmare
# Note: This is a DST-free timezone 
# Pre-processing of telemetry data applied same convention
# To avoid 1 hour DST jump in March
telem$DateTime <- as.POSIXct(strptime(telem$DateTime, format="%m/%d/%Y %H:%M:%S",tz="Etc/GMT-7"))
telem$Date <- as.Date(telem$Date, format="%Y-%m-%d",tz="Etc/GMT-7")

# Arrange by Collar ID and by DateTime for easy calculation of movement metrics
telem <- arrange(telem, Device, DateTime)
