# Load required packages
library(lme4)
library(MuMIn)

proportion_model <- snowfall_telem %>% 
  filter(!is.na(Behavior)&!is.na(SnowDepth)) %>% 
  mutate(travel = Behavior) %>% 
  mutate(resting = if_else(Behavior == 0,1,0)) %>% 
  dplyr::select(Device,Date,speed,snowfall_category,SnowDepth,travel,
                resting,time_of_day)

table(proportion_model$Device,proportion_model$snowfall_category)

# Specify factors
proportion_model$Device <- as.factor(proportion_model$Device)
proportion_model$time_of_day <- as.factor(proportion_model$time_of_day)
proportion_model$snowfall_category <- as.factor(proportion_model$snowfall_category)

proportion_model$snowfall_category <- 
  relevel(proportion_model$snowfall_category, 
          ref="day_of_snowfall")

# Start with global model
options(na.action = "na.fail") # Prevent fitting models to different datasets
full_model_prop <- glmer(cbind(travel,resting) ~ time_of_day +
                           snowfall_category + scale(SnowDepth) +
                           (1|Device), data=proportion_model, 
                         family=binomial)

# Model selection
all_models_prop <- dredge(full_model_prop, beta="none", 
                     evaluate = TRUE, rank = "AIC")

# Clean up model selection table
all_models_prop <- all_models_prop %>% 
  dplyr::select(-logLik) %>% 
  mutate(AIC = round(AIC,2), delta = round(delta,2), 
         weight = round(weight,2)) %>% 
  rename(K = df) 

# Export model selection table
write.csv(all_models_prop,'data/outputs/proportion_model_select.csv',
          row.names=FALSE)

# Workspace clean-up
rm(all_models_prop, proportion_model, full_model_prop)
