library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(janitor)
library(fmsb)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(extrafont)
library(ggradar)
library(scales)

tuesdata <- tidytuesdayR::tt_load(2020, week = 30)
animal_outcomes <- tuesdata$animal_outcomes
animal_complaints <- tuesdata$animal_complaints
brisbane_complaints <- tuesdata$brisbane_complaints

?mutate_at
animal_outcomes1 <- animal_outcomes %>%
  pivot_longer(-c(year, animal_type, outcome), names_to = "region", values_to = "count")
radargraphoutcomes <- animal_outcomes1 %>%
  filter(year == 2018,
         region == "Total", 
         animal_type == "Dogs") %>%
  pivot_wider(names_from = outcome, values_from = count) %>%
  select(-c(year, animal_type, region)) 
radargraphoutcomes <- rbind(rep(32000, 6), rep(0, 6), radargraphoutcomes)
ggradar(radargraphoutcomes)

animal_outcomes1 %>%
  filter(year == 2018,
         region == "Total") %>%
  ggplot(aes(outcome, count, group = animal_type, col))+
  geom_point()+
  coord_polar()+
  facet_wrap(~animal_type)


?rescale
radargraphoutcomescat <- animal_outcomes1 %>%
  filter(year == 2018,
         region == "Total", 
         animal_type == "Cats") %>%
  pivot_wider(names_from = outcome, values_from = count) %>%
  select(-c(year, animal_type, region)) 
radargraphoutcomescat
radargraphoutcomescat <- rbind(rep(32000, 6), rep(0, 6), radargraphoutcomescat)
radarchart(radargraphoutcomescat)




dogoutcomessegment
dogoutcomesradar
par(mfrow = c(1,2))
animal_outcomes1 %>%
  filter(year == 2018,
         region == "Total") %>%
  mutate(
    outcome = fct_reorder(outcome, count)
  ) %>%
  ggplot(aes(count, outcome))+
  geom_segment(aes(x=0,xend = count, y=outcome, yend = outcome), alpha = 0.5)+
  geom_point(aes(shape = animal_type, col = animal_type), alpha = 0.7, size = 3)





radarchart(radargraphoutcomes)
dogoutcomessegment
par(mfrow = c(2,2))
ggarrange(radarchart(radargraphoutcomes), dogoutcomessegment)


# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "physic" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)

# Check your data, it has to look like this!
head(data) 
  
radarchart(data)  
  

animal_outcomes1 %>%
  filter(
    region == "Total"
  ) %>%
  ggplot(aes(year, count))+
  geom_col(aes(fill = outcome))+
  facet_wrap((~animal_type))

outcome_levels <- c("Currently In Care", "Euthanized", "In Stock", "Other", "Reclaimed", "Rehomed", "Released", "Transferred")
animal_levels <- c("Cats", "Dogs", "Horses", "Livestock", "Other Animals", "Wildlife")
animal_outcomes$animal_type
animal_outcomes1$outcome <- parse_factor(animal_outcomes1$outcome, levels = outcome_levels)
animal_outcomes1$animal_type <- parse_factor(animal_outcomes1$animal_type, levels = animal_levels)
fct_lump_lowfreq(animal_outcomes$outcome) %>%
  table()
animal_outcomes2 %>%
  filter(animal_type == "Dogs") %>%
  ggplot(aes(year, count))+
  geom_col(aes(fill = region))+
  facet_wrap(~outcome)
  
animal_outcomes2 <- animal_outcomes %>%
  pivot_longer(-c(year, animal_type, outcome, Total), names_to = "region", values_to = "count")


animal_outcomes1
animal_outcomes1 %>%
  filter(
    region == "Total"
  ) %>%
  ggplot(aes(year, count))+
  geom_col(aes(fill = outcome)) +
  facet_wrap(~animal_type)

animal_complaints1 <- 
  animal_complaints %>%
  clean_names() 
#  parse_date_time(date_received, "m\ y")

animal_complaints1$date_received <- parse_date_time(animal_complaints1$date_received, "my")
animal_complaints1 %>%
  group_by(animal_type, complaint_type, date_received) %>%
  count() %>%
  ggplot(aes(date_received, n))+
  geom_col(aes(fill = animal_type))+
  facet_wrap(~complaint_type)
animal_complaints1