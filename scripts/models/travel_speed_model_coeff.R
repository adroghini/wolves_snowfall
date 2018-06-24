# Obtain summary for models with highest weight of evidence
# top.mod1 <- lme(log.speed ~ snowfall_category + time_of_day, 
# random = ~ 1 + time_of_day | Device, data=speed.df, method="ML")
top.mod1 <- lmer(log.speed ~ snowfall_category + time_of_day
                 + (1 + time_of_day | Device),
                 speed.df,
                 REML = FALSE)

# top.mod2 <- lmer(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth) + (time_of_day | Device), speed.df, REML = FALSE)
# Second highest model: deviance only marginally different. SnowDepth is liely a "pretending" variable (Anderson 2008)

# Backtransform coefficients for interpretation
fixedef.mod1 <- as.data.frame(summary(top.mod1)$coefficients)
fixedef.mod1$category <- rownames(fixedef.mod1)
fixedef.mod1 <- fixedef.mod1 %>% 
  rename(SE = "Std. Error") %>% 
  select(-"t value")

fixedef.mod1$BT.estimates <- 10*exp(fixedef.mod1$Estimate)

#### NEED TO BOOTSTRAP CIs ####
fixedef.mod1$ApproxSE1 <- 10*exp(fixedef.mod1$Estimate-fixedef.mod1$SE)
fixedef.mod1$ApproxSE2 <- 10*exp(fixedef.mod1$Estimate+fixedef.mod1$SE)


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