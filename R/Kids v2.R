library(socviz)
library(ggrepel)
library(tidyverse)
library(tidytuesdayR)

#load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 38)
kids <- tuesdata$kids

#tidy data and join to region
kids_data <-
  opiates %>%
  filter(year == 1999) %>%
  select(state, division_name, region, abbr) %>%
  left_join(kids, by = "state") %>%
  select(state, abbr, region, division_name, variable, year, inf_adj_perchild)
unique(kids_data$variable)

myvariable <- "PK12ed"

#draw
kids_data %>%
  filter(variable == myvariable,
         !is.na(division_name)) %>%
  ggplot()+
  geom_line(aes(x = year, y = inf_adj_perchild*1000, group = state),
            alpha = 0.3)+
  geom_text_repel(data = subset(kids_data,
                                year == max(year) & variable == myvariable & !is.na(division_name)),
                  aes(x = year, y = inf_adj_perchild*1000, label = abbr),
                  size = 3,
                  family = "JetBrains Mono",
                  segment.color = NA,
                  nudge_x = 30)+
  geom_smooth(mapping = aes(x = year, y = inf_adj_perchild*1000, group = division_name),
              color = "darkorange2",
              se = F)+
  scale_y_continuous(labels = scales::dollar)+
  labs(
    title = "US state spending on primary and secondary education",
    subtitle = "Inflation-adjusted expenditure per child, disaggregated by region, from 1997 to 2016",
    caption = "Data: Urban Institute | Visualisation: @beeboileau"
  )+
  theme_minimal(base_family = "Playfair Display")+
  facet_wrap(~division_name)+
  theme(
    axis.text = element_text(family = "JetBrains Mono"),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted",
                                      color = "darkgray"),
    panel.grid.minor.y = element_line(linetype = "dotted",
                                      color = "darkgrey"),
    plot.background = element_rect(fill = "#F0EFEB",
                                   color = "#F0EFEB")
  )

#save
ggsave("educationspending.png", height = 8, width = 8)
