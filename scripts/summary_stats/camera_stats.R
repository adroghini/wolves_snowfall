# Output summary statistics

# How far away are cameras from wolves?
# How many cameras is linked to each wolf?
# Use snowfall_telem dataset (subset to only include snowfall categories data) as written in manuscript

cams.summary <- snowfall_telem %>% 
  group_by(Device) %>% 
  filter(!is.na(SnowDepth)) %>% 
  summarize(mean.dist = mean(dist.from.cams), 
            no_of_cams = length(unique(Camera))) %>% 
  arrange(mean.dist)

# Device ID 33674 does not have any cameras nearby
# Mean distance is 90.1 km away!

# Summary statistics omitting "outlier"
mean(cams.summary[1:16,]$mean.dist)
sd(cams.summary[1:16,]$mean.dist)
range(cams.summary[1:16,]$mean.dist)

# Summarizing deployment length
cams.summary <- cams.depth %>% 
  group_by(Camera) %>% 
  filter(!is.na(SnowDepth)) %>% 
  summarize(deploy.length = length(unique(Date)))

mean(cams.summary$deploy.length)
sd(cams.summary$deploy.length)

# Clean workspace
rm(cams.summary)

