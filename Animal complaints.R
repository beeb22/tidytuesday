library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(janitor)

tuesdata <- tidytuesdayR::tt_load(2020, week = 30)
animal_outcomes <- tuesdata$animal_outcomes
animal_complaints <- tuesdata$animal_complaints
brisbane_complaints <- tuesdata$brisbane_complaints

animal_complaints <- 
  animal_complaints %>%
  clean_names() 

animal_complaints$date_received <- parse_date_time(animal_complaints$date_received, "my")
animal_complaints %>%
  group_by(animal_type, complaint_type, date_received) %>%
  count() %>%
  ggplot(aes(date_received, n))+
  geom_col(aes(fill = animal_type))+
  facet_wrap(~complaint_type)
animal_complaints

animal_complaints %>%
  filter(
    date_received >= as.Date("2014-01-01")
         ) %>%
  group_by(complaint_type, date_received, animal_type) %>%
  count() %>%
  ggplot(aes(date_received, n))+
  geom_col(aes(fill = complaint_type), alpha = 0.7)+
  ylim(-250, 700)+
  labs(
    title = "Townsville City Council Animal Complaints from 2014",
    caption = "Data: data.gov.au",
    x = "Date received", 
    y = "Number of complaints",
    fill = "Complaint Type"
  )+
  scale_x_datetime(date_breaks="1 year", date_labels="%Y")+
  scale_fill_brewer(palette = "Pastel1")+
  theme_minimal()+
  theme(
    text = element_text(family = "Founders Grotesk Light"),
    axis.text.y = element_blank(),
    axis.title = element_blank()
  )+
  coord_polar()+
  facet_wrap(~animal_type)

