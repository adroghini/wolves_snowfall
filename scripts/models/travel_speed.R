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
# Full model: explains travel speed as a function of 
# 1. snowfall category, 2. time of day, and 3. snow depth
# Scale snow depth because values of snow depth >> log speed
# Because speed is logged, snow depth response (if important) is exponential

# First, see if random effects are warranted
# Use REML for optimization of random effects structure
# Need to switch to lmer

# Fixed effects only
AIC(gls(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
        data=speed.df, method="REML"))

# Random intercept only
AIC(lme(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
        random = ~ 1 | Device, data=speed.df, method="REML"))

# Random slope by time_of_day
AIC(lme(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
        random = ~ 1 + time_of_day | Device, data=speed.df, method="REML"))

# Random slope by snow depth
AIC(lme(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
        random = ~ 1 + scale(SnowDepth) | Device, data=speed.df, method="REML"))

# Random slope by snowfall_category 
# Does not converge
AIC(lme(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
        random = ~ 1 + snowfall_category | Device, data=speed.df, method="REML"))

###################################

# Final global model
# Includes random slope for Device as a function of time_of_day
# full_mod_travel <- lme(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth), 
                        # random = ~ 1 + time_of_day | Device, data=speed.df, method="ML")

library(lme4)
full_mod_travel <- lmer(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth)
            + (time_of_day | Device), speed.df, REML = FALSE)


# Check residuals
plot(full_mod_travel)

# Model selection
modelset.speed <- dredge(full_mod_travel, beta="none", 
                     evaluate = TRUE, rank = "AIC")

# Clean up model selection table
names(modelset.speed)

# Round to two decimal places
# Calculate deviance (log likelihood * -2)
modelset.speed <- modelset.speed %>% 
  mutate(AIC = round(AIC,2), delta = round(delta,2), 
         weight = round(weight,2), Deviance = -2 * logLik) %>% 
  rename(K = df) %>% 
  dplyr::select(-logLik) %>% 
  select(c(1:5,Deviance,everything()))

# Add null model results ###

# Export model selection table
write.csv(modelset.speed,'data/outputs/travel_speed_model_select.csv',
          row.names=FALSE)

# Obtain summary for models with highest weight of evidence
# top.mod1 <-lme(log.speed ~ snowfall_category + time_of_day, 
                # random = ~ 1 + time_of_day | Device, data=speed.df, method="ML")
top.mod1 <- lmer(log.speed ~ snowfall_category + time_of_day
                   + (time_of_day | Device), speed.df, REML = FALSE)
top.mod2 <- lmer(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth)
                   + (time_of_day | Device), speed.df, REML = FALSE)

summary(top.mod1)
summary(top.mod2) # Confidence interval for SnowDepth variable overlaps zero.

# How fast do wolves travel at night versus during the day?
time_day_summary <- speed.df %>% 
  group_by(time_of_day, Device) %>% 
  summarize(mean.sp = mean(speed), sd.speed = sd(speed), no.obs = length(speed)) %>% 
  arrange(Device)

# Wolves travel faster at night...
# Write quick loop to determine for how many individuals this pattern holds true
for (i in 1:length(unique(time_day_summary$Device))) {
  x <- subset(time_day_summary, Device == unique(time_day_summary$Device)[i])
  if (subset(x, time_of_day == "night")$mean.sp > subset(x, time_of_day == "day")$mean.sp) {
    y <- 1
  } else {
    y <- 0
  }
  if (i == 1) {
    w <- y
  }
  else {
    w <- w + y
  }
}
cat(c(w,"out of", i)) # 13 out of 17
rm(i, w, y, x, time_day_summary)

# By snowfall_category
speed.df %>% 
  ungroup(speed.df) %>% 
  group_by(time_of_day,snowfall_category) %>% 
  summarize(mean.sp = mean(speed), sd.speed = sd(speed), no.of.obs = length(speed)) %>% 
  filter(time_of_day == "night") %>% 
  # filter(time_of_day == "day") %>% 
  arrange(mean.sp)


# Workspace clean-up
rm(modelset.speed,full_mod_travel,speed.df)

