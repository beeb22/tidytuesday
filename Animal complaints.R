# load relevant packages

library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(janitor)

# load data 

tuesdata <- tidytuesdayR::tt_load(2020, week = 30)
animal_outcomes <- tuesdata$animal_outcomes
animal_complaints <- tuesdata$animal_complaints
brisbane_complaints <- tuesdata$brisbane_complaints

# clean data 

animal_complaints <- 
  animal_complaints %>%
  clean_names() %>%
  group_by(animal_type, complaint_type, date_received) %>%
  count() 
animal_complaints$date_received <- parse_date_time(animal_complaints$date_received, "my")

# draw graph

animal_complaints %>%
  filter(
    date_received >= as.Date("2014-01-01")
  ) %>%
  ggplot(aes(date_received, n))+
  geom_col(aes(fill = complaint_type), alpha = 0.6)+
  ylim(-250, 700)+
  labs(
    title = "Townsville City Council Animal Complaints since 2014",
    subtitle = "Cats get far fewer complaints than dogs; complaints about both animals peak mid-year.",
    caption = "Data: Townsville City Council Animal Complaints",
    x = "Date received", 
    y = "Number of complaints",
    fill = "Complaint Type"
  )+
  scale_x_datetime(date_breaks="1 year", date_labels="%Y")+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  theme(
    text = element_text(family = "Founders Grotesk Light"),
    axis.text.y = element_blank(),
    axis.title = element_blank()
  )+
  coord_polar()+
  facet_wrap(~animal_type)


ggsave("complaints-by-species.png")
