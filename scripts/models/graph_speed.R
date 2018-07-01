# Load required packages
library(ggplot2)
library(dplyr)

# Create graph that summarizes population-level response to
# snowfall events
# Generate summary dataframe
graph_speed <- speed.df %>%
  group_by(snowfall_category, time_of_day) %>%
  summarize(mean.speed = mean(speed), st.err = (sqrt(var(speed) / length(speed))))

graph_speed$snowfall_order <- factor(
  graph_speed$snowfall_category,
  levels = c(
    "control",
    "two_before",
    "one_before",
    "day_of_snowfall",
    "one_after",
    "two_after",
    "three_after"
  )
)

# Plot summary graph
plot_speed <- ggplot(graph_speed,
       aes(
         x = as.factor(snowfall_order),
         y = mean.speed,
         color = time_of_day
       )) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = mean.speed - st.err, ymax = mean.speed + st.err),
                width = 0.12) +
  scale_x_discrete(name = "Time since snowfall event") +
  scale_y_continuous(lim = c(16, 31.5),
                     breaks = seq(16, 32, 3),
                     name = "Average travel speed (m/min)") +
  scale_colour_manual(values = c("darkorange1", "midnightblue")) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major = element_line(colour = "grey90")
  )

file_path <- "/Users/amanda_droghini/Documents/work/archived/msc_ualberta_2013_2016/thesis/analyses/wolves_snowfall/figures/"

# File specifications based on PLoS ONE figure requirements:
# http://journals.plos.org/plosone/s/figures
ggsave(paste(file_path,"fig2_travel_speed.eps",sep=""),device="eps",dpi=300,limitsize=TRUE,units="in",width=7.5,height=5.25)

rm(graph_speed,plot_speed,speed.df,top.mod1,file_path,coef.mod1,full_mod_travel)
