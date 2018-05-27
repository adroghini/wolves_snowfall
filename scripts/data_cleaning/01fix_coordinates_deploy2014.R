# Compare Cam Data 2014.csv to Cam Data 2014 Access.csv to see if they are the same
# This is the deployment file (only coordinates, no snow depth info)

deploy2014Acc<-read.csv("data/cameras/Cam Data 2014 Access.csv",header=T)
deploy2014<-read.csv("data/cameras/Cam Data 2014.csv", header=T)
names(deploy2014Acc)
names(deploy2014)
summary(deploy2014$Project)
summary(deploy2014Acc$ID)
unique(deploy2014Acc$PLOT.ID)
unique(deploy2014$CameraStation)

# Double-check that East/North coordinates are the same
deploy2014Acc<- deploy2014Acc[order(deploy2014Acc$EAST),]
deploy2014<- deploy2014[order(deploy2014$UTMEast),]
A<-deploy2014$UTMEast
B<-deploy2014Acc$EAST
C<-A %in% B
table(C)[TRUE]

# Check for duplicates
uni<- deploy2014[duplicated(deploy2014[,5:6]),]
uni<- deploy2014[duplicated(deploy2014[,6]),]

# Fixed issues:
# Easting coordinate for camera Near6 in deploy2014 has an extra 4. Coordinate should be 444029.
deploy2014[47, 5] = 444029
# Northing coordinate for camera Near 3 has extra digit. Should be 6306985.
deploy2014[25, 6] = 6306985
# Camera Near 28 has the wrong coordinates. Should be East 442910 North 6352662
deploy2014[7, 5] = 442910
deploy2014[7, 6] = 6352662
# Near 28 is *not* a snow depth camera. MeasurePole should be "No"
deploy2014[7, 13] = "No"

# Save new deployment file
write.csv(deploy2014,"data/cameras/deploy2014.csv",row.names=F)

rm(deploy2014Acc,A,B,C,uni)
