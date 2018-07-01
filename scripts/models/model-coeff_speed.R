# Load required libraries
library(lme4)
library(boot)
library(plyr)
library(dplyr)

# Obtain coefficients from models with highest weight of evidence
# Use REML = FALSE to estimate coefficients using maximum likelihood
top.mod1 <- lmer(log.speed ~ snowfall_category + time_of_day
                 + (1 + time_of_day | Device),
                 speed.df,
                 REML = FALSE)

# top.mod2 <- lmer(log.speed ~ snowfall_category + time_of_day + scale(SnowDepth) + (time_of_day | Device), speed.df, REML = FALSE)
# Second highest model: deviance only marginally different. Snow depth is likely a "pretending" variable (Anderson 2008). Discussed in Results

# See results
# Coefficients need to be back-transformed because response variable is logged
summary(top.mod1)

# Create table in which to place bootstrapped betas + CIs
coef.mod1 <- as.data.frame(summary(top.mod1)$coefficients)
coef.mod1$Variable <- row.names(coef.mod1)
row.names(coef.mod1) <- c()
coef.mod1 <- coef.mod1 %>% 
  select(-"t value") %>% 
  select(Variable,Estimate, "Std. Error")

coef.mod1$CI.low <- NA
coef.mod1$CI.high <- NA

# Bootstrap confidence intervals
# With 5000 simulations
boot_par <- bootMer(x=top.mod1,FUN=fixef,nsim=5000)

boot_par[[1]]
for (i in 1:length(boot_par[[1]])) {
  boot_ci <- (boot.ci(boot_par,type="perc",conf = 0.95,index=i))
  # variable <- as.data.frame(boot_ci[[2]])
  # coef.mod1$Variable[i] <- row.names(variable) 
  # coef.mod1$Estimate[i] <- variable[1,1]
  coef.mod1$CI.low[i] <- boot_ci[[4]][4]
  coef.mod1$CI.high[i] <- boot_ci[[4]][5]
}

rm(i)

# Backtransform betas and CIs for interpretation
# Using antilog_function.R
# Inverse log function from:  http://r.789695.n4.nabble.com/Searching-for-antilog-function-td4721348.html
source('scripts/functions/antilog_function.R')
coef.mod1$bt.beta <- round(antilog(coef.mod1$Estimate,10),dig=3)
coef.mod1$bt.CIlow <- round(antilog(coef.mod1$CI.low,10),dig=3)
coef.mod1$bt.CIhigh <- round(antilog(coef.mod1$CI.high,10),dig=3)

# Export final model coefficient table
coef.mod1 <- select(coef.mod1,-c(CI.low,CI.high))
write.csv(coef.mod1,
          'data/outputs/travel_speed_final_model.csv',
          row.names=FALSE)