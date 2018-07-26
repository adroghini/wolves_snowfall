# Load required libraries
library(lme4)
library(boot)
library(plyr)
library(dplyr)

# Obtain coefficients for top-ranking model
# Use REML = FALSE to estimate coefficients using maximum likelihood
top.mod1 <- glmer(cbind(travel,resting) ~ time_of_day *
                                     snowfall_category + 
                    (1|Device), data=proportion.df, 
                                   family=binomial)

summary(top.mod1)

# Create table in which to place bootstrapped betas + CIs
coef.mod1 <- as.data.frame(summary(top.mod1)$coefficients)
coef.mod1$Variable <- row.names(coef.mod1)
row.names(coef.mod1) <- c()
coef.mod1 <- coef.mod1 %>% 
  select(Variable,Estimate, "Std. Error")

coef.mod1$CI.low <- NA
coef.mod1$CI.high <- NA

# Bootstrap confidence intervals
# With 5000 simulations
# Note: This takes a *very* long time to run (on the order of several days)

# Define function
boot.fun <- function(fit) {
  return(fixef(fit))
}

boot_par <- bootMer(x=top.mod1,FUN=boot.fun,nsim=5000)
# Save results
saveRDS(boot_par,file='data/outputs/bootstrap_prop_model.rds')

# boot_par[[1]]
for (i in 1:length(boot_par[[1]])) {
  boot_ci <- (boot.ci(boot_par,type="perc",conf = 0.95,index=i))
  # variable <- as.data.frame(boot_ci[[2]])
  # coef.mod1$Variable[i] <- row.names(variable) 
  # coef.mod1$Estimate[i] <- variable[1,1]
  coef.mod1$CI.low[i] <- boot_ci[[4]][4]
  coef.mod1$CI.high[i] <- boot_ci[[4]][5]
}

rm(i)

# Export final model coefficient table
write.csv(coef.mod1,
          'data/outputs/prop_travel_final_model.csv',
          row.names=FALSE)
