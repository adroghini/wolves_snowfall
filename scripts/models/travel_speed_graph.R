# Load required packages
library(ggplot2)

test <- test_travel %>% 
  group_by(snowfall_category,time_of_day) %>% 
  summarize(mean.speed = mean(speed),st.err=(sqrt(var(speed)/length(speed))))

test$snowfall_order <- factor(test$snowfall_category, levels = 
                                c("control", "two_before", "one_before",
"day_of_snowfall", "one_after","two_after","three_after"))

# Plot summary graph
ggplot(test,aes(x=as.factor(snowfall_order),y=mean.speed,
                color=time_of_day)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean.speed-st.err, ymax=mean.speed+st.err),
                width = 0.1) +
  scale_x_discrete(name="Time since snowfall event") +
  scale_y_continuous(lim=c(16,31.5),breaks=seq(16,32,3),
                     name="Average travel speed (m/min)") +
  scale_colour_manual(values=c("black","red")) +
  theme_classic() +
  theme(legend.position="top",legend.title = element_blank(),
        panel.grid.major = element_line(colour="grey90"))


  
         