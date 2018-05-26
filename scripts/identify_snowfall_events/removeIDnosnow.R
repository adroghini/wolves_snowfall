##did you ever get rid of those IDs for which there were snowfalls?
length(unique(snow_cams$Device_ID))
##no you didn't, please do???


##from avgsnow script:##should probably remove those individuals not having any storms at all as this will add unnecessary variation in 0 speed category?
##33676, 33669, 32254, 13790

snow_red<-subset(snow_cams,Device_ID!=33676&Device_ID!=33669&Device_ID!=32254&Device_ID!=13790)
rm(snow_cams)
