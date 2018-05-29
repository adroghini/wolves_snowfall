# Load required packages
library(dplyr)

# Calculate snow accumulation
# I.e. Difference in snow depth from one day to the next

# Ensure dataframe is properly sorted
cams.depth <- arrange(cams.depth, Camera,DateTime)

cams.depth$SnowAccum <- NA

for(i in 2:nrow(cams.depth)) {
  if (cams.depth$Camera[i] == cams.depth$Camera[i - 1]) {
    cams.depth$SnowAccum[i] <- 
      cams.depth$SnowDepth[i] - cams.depth$SnowDepth[i - 1]
  }
}

rm(i)

# Plot data
hist(cams.depth$SnowAccum,xlim=c(4,max(cams.depth$SnowAccum,na.rm=TRUE)),
     breaks=20,ylim=c(0,30))

# Environment Canada issues a snowfall warning in Alberta when
# >= 10 cm falls in 24 hour
nrow(subset(cams.depth,SnowAccum>=10))

# Very few such instances recorded
# Given sample size limitation, set threshold for snowfall definition to
# >= 5 cm
nrow(subset(cams.depth,SnowAccum>=5))
