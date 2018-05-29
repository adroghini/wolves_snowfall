# Load required packages
library(dplyr)
library(nlme)
library(MuMIn)

# Create data for Model 1: travel speed
test_travel <- snowfall_telem %>% 
  filter(Behavior==1 & !is.na(SnowDepth)) %>% 
  dplyr::select(Device,speed, time_of_day,SnowDepth,snowfall_category)

# Exploratory plots
# Do continuous variables contain outliers
# Or require transformations?

# Create ordered observation number
# To mimic dotchart
test_travel$obs_no <- 1:nrow(test_travel)
## Travel speed
plot(test_travel$obs_no~test_travel$speed) 
range(test_travel$speed) 
# Likely requires strong transformation
plot(test_travel$obs_no~log10(test_travel$speed)) 
# A million times better

## Snow depth
plot(test_travel$obs_no~test_travel$SnowDepth) 
# Looks ok

# Create column for log.speed
# Code factors for categorical variables
test_travel$log.speed <- log10(test_travel$speed)
test_travel$Device <- as.factor(test_travel$Device)
test_travel$snowfall_category <- as.factor(test_travel$snowfall_category)
test_travel$time_of_day <- as.factor(test_travel$time_of_day)

test_travel$snowfall_category <- 
  relevel(test_travel$snowfall_category, 
          ref="day_of_snowfall")

# Apply linear model
# Use information theoretic approach (AIC)
options(na.action = "na.fail") # Prevent fitting models to different datasets

# Define full model
# random slope
full_model_travel <- lme(log.speed ~ snowfall_category + time_of_day + scale( SnowDepth), random = ~1|Device/snowfall_category, data=test_travel, method="ML")

# Check residuals
plot(full_model_travel)

# Model selection
all_models <- dredge(full_model_travel, beta="none", 
                     evaluate = TRUE, rank = "AIC")

# Clean up model selection table
names(all_models)

# Round to two decimal places
all_models <- all_models %>% 
  dplyr::select(-logLik) %>% 
  mutate(AIC = round(AIC,2), delta = round(delta,2), 
         weight = round(weight,2)) %>% 
  rename(K = df) 

# Export model selection table
write.csv(all_models,'data/outputs/travel_speed_model_select.csv',
          row.names=FALSE)

# Workspace clean-up
rm(all_models,full_model_travel,test_travel)

