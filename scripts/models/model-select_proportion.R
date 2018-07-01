# Load required packages
library(dplyr)
library(lme4)
library(MuMIn)

proportion.df <- snowfall_telem %>% 
  filter(!is.na(Behavior)&!is.na(SnowDepth)) %>% 
  mutate(travel = Behavior) %>% 
  mutate(resting = if_else(Behavior == 0,1,0)) %>% 
  dplyr::select(Device,Date,speed,snowfall_category,SnowDepth,travel,
                resting,time_of_day)

table(proportion.df$Device,proportion.df$snowfall_category)

# Specify factors
proportion.df$Device <- as.factor(proportion.df$Device)
proportion.df$time_of_day <- as.factor(proportion.df$time_of_day)
proportion.df$snowfall_category <- as.factor(proportion.df$snowfall_category)

proportion.df$snowfall_category <- 
  relevel(proportion.df$snowfall_category, 
          ref="day_of_snowfall")

# Set reference time of day to "day" (default)
proportion.df$time_of_day <- 
  relevel(proportion.df$time_of_day, 
          ref="day")

# Start with global model
options(na.action = "na.fail") # Prevent fitting models to different datasets
full_mod_prop <- glmer(cbind(travel,resting) ~ time_of_day *
                           snowfall_category + scale(SnowDepth) +
                           (1|Device), data=proportion.df, 
                         family=binomial)

# Model selection
modelset.prop <- dredge(full_mod_prop, beta="none", 
                     evaluate = TRUE, rank = "AIC")

# Clean up model selection table
# Calculate deviance (log likelihood * -2)
modelset.prop <- modelset.prop %>% 
  mutate(AIC = round(AIC,2), delta = round(delta,2), 
         weight = round(weight,2), 
         Deviance = round((-2 * logLik),2)) %>% 
  rename(K = df) %>% 
  dplyr::select(-logLik) %>% 
  select(c(1:5,Deviance,everything()))

# Export model selection table
write.csv(modelset.prop,'data/outputs/model_select_proportion.csv',
          row.names=FALSE)

# Workspace clean-up
# rm(modelset.prop, proportion.df, full_mod_prop)
