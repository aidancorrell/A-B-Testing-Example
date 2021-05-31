# load packages
library(readr)
library(dplyr)
library(ggplot2)

#loading a/b testing data

ab_test <- read_csv('ab_testing_file.csv')

#inspecting a/b testing data

head(ab_test)

#defining views by utm

views_by_utm <- ab_test %>%
  group_by(utm_source) %>%
  summarize(count = n())
#views_by_utm

#defining clicks by utm

clicks_by_utm <- ab_test %>%
  group_by(utm_source, ad_clicked) %>%
  summarize(count=n())
clicks_by_utm

#defining percentage by utm

percentage_by_utm <- clicks_by_utm %>%
  group_by(utm_source) %>%
  mutate(percentage = count/sum(count)) %>%
  filter(ad_clicked == TRUE)
#percentage_by_utm

#defining experiment split by experiment group

experiment_split <- ab_test %>%
  group_by(experimental_group) %>%
  summarize(count = n())
experiment_split

#defining clicks per experiment group

clicks_by_experiment <- ab_test %>%
  group_by(experimental_group, ad_clicked) %>%
  summarize(count=n())
clicks_by_experiment

#defining clicks a

a_clicks <- ab_test %>%
  filter(experimental_group == 'A')
#a_clicks

#defining clicks b

b_clicks <- ab_test %>%
  filter(experimental_group == 'B')
#b_clicks

#defining a_clicks_by_day here:

a_clicks_by_day <- a_clicks %>%
  group_by(day, ad_clicked) %>%
  summarize(count = n())
#a_clicks_by_day

#defining a_clicks_by_day_true here:

a_clicks_by_day_true <- a_clicks %>%
  group_by(day, ad_clicked) %>%
  summarize(count = n()) %>%
  filter(ad_clicked == TRUE)
a_clicks_by_day_true



#defining b_clicks_by_day here:

b_clicks_by_day <- b_clicks %>%
  group_by(day, ad_clicked) %>%
  summarize(count = n())
#b_clicks_by_day

#defining b_clicks_by_day_true here:

b_clicks_by_day_true <- b_clicks %>%
  group_by(day, ad_clicked) %>%
  summarize(count = n()) %>%
  filter(ad_clicked == TRUE)
#b_clicks_by_day_true



#defining a_percentage_by_day here:

a_percentage_by_day <- a_clicks_by_day %>%
  group_by(day) %>%
  mutate(percentage = count/sum(count)) %>%
  filter(ad_clicked == TRUE)
#a_percentage_by_day




#defining b_percentage_by_day here:

b_percentage_by_day <- b_clicks_by_day %>%
  group_by(day) %>%
  mutate(percentage = count/sum(count)) %>%
  filter(ad_clicked == TRUE)
#b_percentage_by_day

# Barplot
#bar1 <- ggplot(a_clicks_by_day_true, aes(x=day, y=count)) + 
  #geom_bar(stat = "identity")

#bar2 <- ggplot(b_clicks_by_day_true, aes(x=day, y=count)) + 
  #geom_bar(stat = "identity")


#bar + ggtitle("Group A Ad clicks by Day of Week") +
  #xlab("Day") + ylab("Clicks")


########

ggplot(data = a_clicks_by_day_true, aes(x = ad_clicked, y = count, fill = day)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)  +
  ylim(0,25) +
  geom_text(aes(label = count), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  labs(x = "\n Day", y = "Clicks\n", title = "\n Group A Advertisement Clicks by Day \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size = 12),
        legend.title = element_text(face="bold", size = 10))

#####

ggplot(data = b_clicks_by_day_true, aes(x = ad_clicked, y = count, fill = day)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)  +
  ylim(0,25) +
  geom_text(aes(label = count), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  labs(x = "\n Day", y = "Clicks\n", title = "\n Group B Advertisement Clicks by Day \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size = 12),
        legend.title = element_text(face="bold", size = 10))

#######

ggplot(data = clicks_by_experiment, aes(x = ad_clicked, y = count, fill = ad_clicked)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  facet_grid(. ~experimental_group)  +
  ylim(0,60) +
  geom_text(aes(label = count), fontface = "bold", vjust = 1.5, colour = "white", size = 4) +
  labs(x = "\n True = Clicked False = Not Clicked", y = "Clicks\n", title = "\n Group A vs B Ad clicks \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12),
        legend.title = element_text(face="bold", size = 10),
        strip.background = element_rect(fill="lightblue", colour="black", size=1),
        strip.text = element_text(face="bold", size=rel(1.2)))

#######

ggplot(data = clicks_by_utm, aes(x = ad_clicked, y = count, fill = utm_source)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)  +
  ylim(0,25) +
  geom_text(aes(label = count), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  labs(x = "\n UTM SOURCE", y = "Count\n", title = "\n UTM Source by Ad Clicked \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="Black", size = 12),
        axis.title.y = element_text(face="bold", colour="Black", size = 12),
        legend.title = element_text(face="bold", size = 10))

