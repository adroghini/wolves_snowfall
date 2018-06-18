# Load required packages
library(dplyr)
library(nlme)
library(MuMIn)

# Create data for Model 1: travel speed
speed.df <- snowfall_telem %>% 
  filter(Behavior==1 & !is.na(SnowDepth)) %>% 
  dplyr::select(Device,speed, time_of_day,SnowDepth,snowfall_category)

# Exploratory plots
# Do continuous variables contain outliers
# Or require transformations?

# Create ordered observation number
# To mimic dotchart
speed.df$obs_no <- 1:nrow(speed.df)
## Travel speed
plot(speed.df$obs_no~speed.df$speed) 
range(speed.df$speed) 
# Requires strong transformation
plot(speed.df$obs_no~log10(speed.df$speed)) 
# A million times better

## Snow depth
plot(speed.df$obs_no~speed.df$SnowDepth) 
# Looks ok

# Create column for log.speed
# Code factors for categorical variables
# Set day_of_snowfall as reference category
speed.df$log.speed <- log10(speed.df$speed)
speed.df$Device <- as.factor(speed.df$Device)
speed.df$snowfall_category <- as.factor(speed.df$snowfall_category)
speed.df$time_of_day <- as.factor(speed.df$time_of_day)

speed.df$snowfall_category <- 
  relevel(speed.df$snowfall_category, 
          ref="day_of_snowfall")

# Apply linear model
# Use information theoretic approach (AIC)
options(na.action = "na.fail") # Prevent fitting models to different datasets

###################################
# See if random effects are warranted

# Full model: explains travel speed as a function of 
# 1. snowfall category, 2. time of day, and 3. snow depth
# Scale snow depth - values of snow depth >> log speed
# Because speed is logged, snow depth response (if significant) is exponential

# Fixed effects only
AIC(gls(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
        data=speed.df, method="ML"))

# Random intercept only
AIC(lme(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
        random = ~ 1 | Device, data=speed.df, method="ML"))

# Random slope by time_of_day
AIC(lme(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
        random = ~ 1 + time_of_day | Device, data=speed.df, method="ML"))

# Random slope by snow depth
AIC(lme(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
        random = ~ 1 + scale(SnowDepth) | Device, data=speed.df, method="ML"))

# Random slope by snowfall_category does not converge
AIC(lme(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
        random = ~ 1 + snowfall_category | Device, data=speed.df, method="ML"))

###################################
# Final global model
# Includes random slope for Device as a function of time_of_day
full_mod_travel <- lme(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
                         random = ~ 1 + time_of_day | Device, data=speed.df, method="ML")

# Check residuals
plot(full_mod_travel)

# Model selection
all_models <- dredge(full_mod_travel, beta="none", 
                     evaluate = TRUE, rank = "AIC")

# Clean up model selection table
names(all_models)

# Round to two decimal places
all_models <- all_models %>% 
  dplyr::select(-logLik) %>% 
  mutate(AIC = round(AIC,2), delta = round(delta,2), 
         weight = round(weight,2)) %>% 
  rename(K = df) 

### Need to add null model results ###

# Export model selection table
write.csv(all_models,'data/outputs/travel_speed_model_select.csv',
          row.names=FALSE)

# Obtain summary for models with highest weight of evidence
top.mod1 <-lme(log.speed ~ snowfall_category + time_of_day, 
                 random = ~ 1 + time_of_day | Device, data=speed.df, method="REML")
top.mod2 <-lme(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
               random = ~ 1 + time_of_day | Device, data=speed.df, method="REML")

summary(top.mod1)
summary(top.mod2) # Confidence interval for SnowDepth variable overlaps zero.



# Workspace clean-up
rm(all_models,full_mod_travel,speed.df)

