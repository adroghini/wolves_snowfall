# Load required packages
library(dplyr)
library(lme4)
library(MuMIn)

# Create dataframe, speed.df, for model fitting
# Create data for Model 1: travel speed
speed.df <- snowfall_telem %>% 
  filter(Behavior==1 & !is.na(SnowDepth)) %>% 
  dplyr::select(Device,speed, time_of_day,SnowDepth,snowfall_category,
                SnowAccum)

# Exploratory plots
# Do continuous variables contain outliers
# Or require transformations?

# Create ordered observation number for plotting
speed.df$obs_no <- 1:nrow(speed.df)
## Travel speed
plot(speed.df$obs_no~speed.df$speed) 
range(speed.df$speed) 
# Requires strong transformation
plot(speed.df$obs_no~log10(speed.df$speed)) 
# A million times better

## Snow depth
plot(speed.df$obs_no~speed.df$SnowDepth) 
plot(speed.df$obs_no~speed.df$SnowAccum) 
# Looks ok

# Create column for log.speed
# Code factors for categorical variables
# Set day_of_snowfall as reference category
speed.df$log.speed <- log10(speed.df$speed)
speed.df$Device <- as.factor(speed.df$Device)
speed.df$snowfall_category <- as.factor(speed.df$snowfall_category)
speed.df$time_of_day <- as.factor(speed.df$time_of_day)

# Set reference category to day of snowfall event
speed.df$snowfall_category <- 
  relevel(speed.df$snowfall_category, 
          ref="day_of_snowfall")

# Set reference time of day to "day" (default)
speed.df$time_of_day <- 
  relevel(speed.df$time_of_day, 
          ref="day")

####### Model selection ####### 
options(na.action = "na.fail") # Prevent fitting models to different datasets

# Full model: explains travel speed as a function of 
# 1. snowfall category, 2. time of day, and 3. snow depth
# Scale snow depth because values of snow depth >> log speed
# Because speed is logged, depth response (if important) is exponential

#### Random effects structure ####
# Use REML for optimization of random effects structure

# Fixed effects only
# Fitting fixed effects with REML is discouraged...
# AIC(gls(log.speed ~ snowfall_category * time_of_day + scale(SnowDepth) + scale(SnowAccum), data=speed.df, method="REML"))

# Random intercept only
AIC(lmer(log.speed ~ snowfall_category * time_of_day + scale(SnowDepth) + (1 | Device), speed.df, REML = TRUE))

# Random slope (time_of_day) within Device
AIC(lmer(log.speed ~ snowfall_category * time_of_day + scale(SnowDepth) + (1 + time_of_day | Device), speed.df, REML = TRUE))

# Random slope (snow depth) within Device
AIC(lmer(log.speed ~ snowfall_category * time_of_day 
         + scale(SnowDepth) + (1 + scale(SnowDepth) | Device), 
         speed.df, REML = TRUE))

# Random slope by snowfall_category 
# Fails to converge when REML = FALSE
AIC(lmer(log.speed ~ snowfall_category * time_of_day + 
           scale(SnowDepth) + (1 + snowfall_category | Device), 
         speed.df, REML = TRUE))

#### Global model and candidate set #### 
# Final global model
# Includes random slope for time_of_day within Device group
full_mod_travel <- lmer(log.speed ~ snowfall_category * time_of_day + scale(SnowDepth) + (1 + time_of_day | Device), speed.df, REML = FALSE)

# Check residuals
plot(full_mod_travel)

# Create set of candidate models
# Jutification of MuMIn's dredge: It is biologically reasonable to expect every variable to occur singly or in combination
modelset.speed <- dredge(full_mod_travel, beta="none", 
                         evaluate = TRUE, rank = "AIC")

# Clean up model selection table
names(modelset.speed)

# Round to two decimal places
# Calculate deviance (log likelihood * -2)
modelset.speed <- modelset.speed %>% 
  mutate(AIC = round(AIC,2), delta = round(delta,2), 
         weight = round(weight,2), 
         Deviance = round((-2 * logLik),2)) %>% 
  rename(K = df) %>% 
  dplyr::select(-logLik) %>% 
  select(c(1:5,Deviance,everything()))

# Export model selection table
write.csv(modelset.speed,
          'data/outputs/travel_speed_model_select.csv',
          row.names=FALSE)

