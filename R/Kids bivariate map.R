#load libraries
library(tidytuesdayR)
library(tidyverse)
library(maps)
library(biscale)
library(cowplot)

#load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 38)
kids <- tuesdata$kids

us_states <- map_data("state")

#get map
kids_mapped <- 
  kids %>%
  mutate(
    state = tolower(state)
  ) %>%
  left_join(us_states, by = c("state" = "region")) %>%
  filter(year == 2016,
         variable == "socsec" | variable == "PK12ed") %>%
  select(state, variable, inf_adj_perchild, long, lat, group, order) %>%
  pivot_wider(names_from = variable,
              values_from = inf_adj_perchild)

data <- bi_class(kids_mapped, 
                 x = socsec,
                 y = PK12ed, 
                 dim = 3)

#draw map
map <- 
  ggplot(data)+
  geom_polygon(aes(x = long, y = lat, fill = bi_class, group = group), 
               colour = "gray80",
               size = 0.2,
               show.legend = F)+
  bi_scale_fill(pal = "DkCyan", dim = 3)+
  bi_theme()+
  labs(
    title = "US education and social security spending",
    subtitle = "State expenditure per child in 2016",
    caption = "Data: Urban Institute | Visualisation: @beeboileau"
  )+
  coord_map(projection = "albers",
            lat0 = 39, lat1 = 45)+
  theme(
    axis.title = element_blank(),
    plot.caption = element_text(size = rel(0.5),
                                hjust = 1, vjust = -1)
  )
  
#draw legend
legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "Higher social security",
                    ylab = "Higher education",
                    size = 10)

#use cowplot to put it all together and label states
ggdraw()+
  draw_plot(map, 0, 0, 1, 1)+
  draw_plot(legend, 0.1, 0.1, 0.2, 0.2)+
  geom_segment(aes(x = 0.4, y = 0.7, xend = 0.45, yend = 0.63),
               alpha = 0.8,
               col = "gray70")+
  geom_text(aes(x = 0.4, y = 0.71, label = "High education spending, low social security"),
            col = "gray60")+
  geom_segment(aes(x = 0.6, y = 0.2, xend = 0.5, yend = 0.4),
               alpha = 0.8,
               col = "gray70")+
  geom_text(aes(x = 0.6, y = 0.19, label = "High social security spending, low education"),
            col = "gray60")+
  geom_segment(aes(x = 0.7, y = 0.7, xend = 0.79, yend = 0.65),
               alpha = 0.8,
               col = "gray70")+
  geom_text(aes(x = 0.7, y = 0.71, label = "High education and social security spending"),
            col = "gray60")
  
  
ggsave("education.png", width = 12, height = 10)



