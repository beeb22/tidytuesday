library(tidytuesdayR)
library(tidyverse)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2020, week = 39)

peaks <- tuesdata$peaks
members <- tuesdata$members
expeditions <- tuesdata$expeditions

str(peaks)
str(members)
str(expeditions)

#extremely speedy data wrangle
exp_size <- members %>%
  filter(peak_name == "Everest") %>%
  group_by(expedition_id) %>%
  summarise(
    number_onexp = n(),
    year = mean(year)
  )

#dot plot
ggplot(exp_size)+
  geom_point(aes(x = year, y = number_onexp),
             alpha = 0.2)+
  geom_point(data = subset(exp_size, number_onexp > 80),
             aes(x = year, y = number_onexp), pch = 21)+
  geom_text(data = subset(exp_size, number_onexp > 80),
            aes(x = year, y = number_onexp - 3, label = paste0("The largest expedition had ", number_onexp, " members")),
            family = "Bahnschrift", color = "gray60")+
  geom_smooth(aes(y = number_onexp, x = year),
              color = "orange", se = F, alpha = 0.01, method = "lm")+
  labs(title = "Everest expedition size over time",
       subtitle = "The maximum number of people on a single Everest expedition has grown over time, from below 50 to 99.
Average expedition sizes, though, have fallen over time, with an increasingly large number of solo expeditions.
This plot illustrates the change in the average by fitting a linear regression to the data points.",
       caption = "Source: The Himalayan Database | Visualisation: @beeboileau",
       x = "Year of expedition",
       y = "Number of expedition members") +
  theme_minimal(base_family = "Bahnschrift")+
  theme(
    plot.background = element_rect(fill = "#F0EFEB",
                                   color = "#F0EFEB")
  )
  
ggsave("expeditionsize.png", height = 8, width = 8)



members %>%
  group_by(sex, year) %>%
  filter(year > 1969,
         peak_name == "Everest") %>%
  summarise(
    n = n()
  ) %>%
  pivot_wider(names_from = sex, values_from = n) %>%
  mutate_at(c(2:3), ~replace(., is.na(.), 0)) %>%
  mutate(
    total = F + M
  ) %>%
  ggplot()+
  geom_col(aes(x = total, y = factor(year)), alpha = 0.8, size = 1.5)+
  geom_col(aes(x = F, y = factor(year)), fill = "orange", alpha = 0.8, size = 1.5)+
  labs(title = "Number of Everest climbers per year since 1970",
       subtitle = "The proportion of <b style = 'color:orange'>women</b> is slowly growing, but remains low",
       caption = "Source: The Himalayan Database | Visualisation: @beeboileau",
       x = "Total Everest climbers",
       y = "") +
  theme_minimal(base_family = "Bahnschrift")+
  theme(
    panel.grid.major.y = element_blank(),
    plot.subtitle = element_markdown(),
    panel.grid.major.x = element_line(linetype = "dotted"),
    panel.grid.minor.x = element_line(linetype = "dotted"),
    plot.background = element_rect(fill = "#F0EFEB",
                                   color = "#F0EFEB")
  )

ggsave("womenclimbers.png", width = 8, height = 8)
           