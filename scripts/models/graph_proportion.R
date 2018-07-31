library(dplyr)
library(ggplot2)

graph_prop <- coef.mod1[-c(1:2),]

# Use patterns within "Variable" column to
# Create column for "time_of_day" (day versus night)
# And "time_since_snowfall" column (snowfall categories)
graph_prop <- graph_prop %>% 
  mutate(
    time_of_day = 
      case_when(
    grepl("night", Variable) ~ "night",
    !grepl("night", Variable) ~ "day")) %>% 
  mutate(
   time_since_snowfall  = 
      case_when(
        grepl("control", Variable) ~ "control",
        grepl("two_before", Variable) ~ "two_before",
        grepl("one_before", Variable) ~ "one_before",
        grepl("one_after", Variable) ~ "one_after",
        grepl("two_after", Variable) ~ "two_after",
        grepl("three_after", Variable) ~ "three_after"
        )
  ) %>% 
  select(time_of_day,time_since_snowfall,everything()) %>% 
  select(-Variable,-"Std. Error")

graph_prop$time_of_day <- factor(graph_prop$time_of_day,  
                                 level=c("day", "night"),    
                                 labels=c("day", "night"))   

graph_prop$time_since_snowfall <- factor(graph_prop$time_since_snowfall, level=c("three_after", "two_after", "one_after", "one_before", "two_before","control"), labels=c("three_after", "two_after", "one_after", "one_before", "two_before","control"))

# Create plot
ggplot(data=graph_prop, aes(x=bt.beta, y=time_since_snowfall, group=time_of_day))+
  geom_errorbarh(aes(xmin=bt.CIlow, xmax=bt.CIhigh,color=time_of_day),
                height = 0.12)+
  geom_point(size=2, aes(color=time_of_day))+
  ylab("Time since snowfall event (days)")+
  scale_x_continuous(lim = c(0.5, 2.4),
                     breaks = seq(0, 3, 0.5),
                     name = "Odds ratio (proportion of travel)") +
  scale_colour_manual(values = c("darkorange1", "midnightblue")) +
  geom_vline(xintercept=1,linetype="dotted")+
  theme_classic()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")

file_path <- "/Users/amanda_droghini/Documents/work/archived/msc_ualberta_2013_2016/thesis/analyses/wolves_snowfall/figures/"

# File specifications based on PLoS ONE figure requirements:
# http://journals.plos.org/plosone/s/figures
ggsave(paste(file_path,"fig3_prop_travel.eps",sep=""),device="eps",dpi=300,limitsize=TRUE,units="in",width=7.5,height=5.25)
