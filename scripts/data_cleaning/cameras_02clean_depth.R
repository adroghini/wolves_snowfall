# Load required packages
library(plyr)
library(dplyr)

# Load snow depth data
depth2013 <- read.csv('data/cameras/depth2013.csv',stringsAsFactors = FALSE)
depth2014 <- read.csv('data/cameras/depth2014.csv',stringsAsFactors = FALSE)

# Merge into one file
cams.depth <- rbind.fill(depth2013,depth2014)
rm(depth2013,depth2014)

names(cams.depth)

# Fix date/time columns - create them from Image.Name
for(i in 1:nrow(cams.depth)) {
  cams.depth$Date[i] <-
    unlist(strsplit(cams.depth$Image.Name[i], ' '))[1]
  cams.depth$Time[i] <- unlist(strsplit(cams.depth$Image.Name[i], ' '))[2]
  cams.depth$Time[i] <- paste(unlist(strsplit(cams.depth$Time[i], '-')),collapse=":")
  cams.depth$DateTime[i] <- paste(cams.depth$Date[i], cams.depth$Time[i])
  }
rm(i)

# Specify date/time format
cams.depth$Date <- as.Date(cams.depth$Date, 
                                  format="%Y-%m-%d",tz="Etc/GMT-7")

cams.depth$DateTime <- as.POSIXct(strptime(cams.depth$DateTime, 
                                      format="%Y-%m-%d %H:%M:%S",tz="Etc/GMT-7"))

# Sort columns
cams.depth <- arrange(cams.depth, Camera,DateTime)

# Delete extra columns
# For each camera, create RowID that goes from 1:length of observation
cams.depth <- cams.depth %>% 
  dplyr::select(-c(Image.Name,Period,Image.Path,Temp,ImagePath,
            Moon.Phase)) %>% 
  mutate(RowID = 1:length(DateTime))

# Subset to only include cameras in deploy
cams.to.include <- unique(cams.deploy$camera)
cams.depth <- filter(cams.depth, Camera %in% cams.to.include)
# Check - Should have 27 cameras
length(unique(cams.depth$Camera))

rm(cams.to.include)


