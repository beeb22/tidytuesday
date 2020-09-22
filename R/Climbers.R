library(tidytuesdayR)
library(tidyverse)
library(maps)
library(biscale)
library(cowplot)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2020, week = 39)

peaks <- tuesdata$peaks
members <- tuesdata$members
expeditions <- tuesdata$expeditions

str(peaks)
str(members)
str(expeditions)

peaks_fixed <- peaks %>%
  mutate(first_ascent_year = ifelse(peak_id == "SPH2", 2018, first_ascent_year))

peaks_fixed %>%
  ggplot(aes(first_ascent_year)) +
  geom_histogram(binwidth = 5, direction = -1, alpha = 0.8) +
  scale_x_continuous(breaks = seq(1910, 2020, 10)) +
  labs(title = "Climbers are still summitting peaks for the first time",
       subtitle = "Year of first ascent for Himalayan peaks",
       caption = "Source: The Himalayan Database",
       x = "Year of first ascent (5-year bins)",
       y = "Number of first ascents") +
  theme(text = element_text(family = "Bahnschrift"),
        panel.grid.minor = element_blank())

peaks_fixed %>%
  ggplot(aes(height_metres))+
  geom_histogram(binwidth = 100, alpha = 0.8)

peaks_fixed %>%
  ggplot()+
  geom_point(aes(x = first_ascent_year, y = height_metres),
             alpha = 0.4)+
  theme_minimal(base_family = "Bahnschrift")

#were women's outcomes different from men's?
#get proportion

members %>%
  group_by(expedition_id) %>%
  summarise(
    number_onexp = n(),
    year = mean(year)
  ) %>%
  ggplot()+
  geom_point(aes(x = year, y = number_onexp),
             alpha = 0.1)+
  geom_smooth(aes(y = number_onexp, x = year), 
              color = "orange", se = F, alpha = 0.01)+
  labs(title = "Expedition size over time",
       subtitle = "While it might appear from the fact the maximum journey has got larger over time that crew size
is an increasing function of time, the trend is actually fairly constant",
       caption = "Source: The Himalayan Database | Visualisation: @beeboileau",
       x = "Year of expedition",
       y = "Number of expedition members") +
  theme_minimal(base_family = "Bahnschrift")

ggsave("expeditionsize.png", height = 8, width = 8)

members %>%
  group_by(expedition_id, sex, year) %>%
  summarise(
    num = n(),
    year = mean(year)
  ) %>%
  filter(num > 3) %>%
  pivot_wider(names_from = sex, values_from = num) %>%
  mutate_at(c(2:4), ~replace(., is.na(.), 0)) %>%
  mutate(
    fem_prop = F/(F+M)
  ) %>%
  ggplot()+
  geom_point(aes(x = year, y = fem_prop), alpha = 0.3)+
  geom_smooth(aes(x = year, y = fem_prop), colour = "orange")+
  labs(title = "Proportion of women in expedition groups (n > 3) over time",
       subtitle = "The proportion is growing, although t",
       caption = "Source: The Himalayan Database | Visualisation: @beeboileau",
       x = "Year of expedition",
       y = "Number of expedition members") +
  theme_minimal(base_family = "Bahnschrift")

#ok lets try n do like...bars filled in (orange?)

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
    panel.grid.minor.x = element_line(linetype = "dotted")
  )

           