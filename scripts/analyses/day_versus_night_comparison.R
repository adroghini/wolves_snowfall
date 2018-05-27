# "Diurnal" analysis
# Do wolves travel more at night than during the day? Do they travel fast?

# Load packages
library(dplyr)

diurnal.summary <- tel.fixr30 %>% 
  filter(!is.na(NewFixRate)) %>% 
  group_by(Device,time_of_day) %>% 
  summarize(N = length(Behavior), travelling = sum(Behavior), resting = N - travelling,
            prop.move = travelling / N, prop.rest = 1 - prop.move, mean.speed = mean(speed))

plot(as.factor(diurnal.summary$time_of_day),diurnal.summary$prop.move)
plot(as.factor(diurnal.summary$time_of_day),diurnal.summary$mean.speed)