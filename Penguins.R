library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(devtools)

# load data 

tuesdata <- tidytuesdayR::tt_load(2020, week = 31)
penguins <- tuesdata$penguins
penguins_raw <- tuesdata$penguins_raw


# graph colourful depth-mass by species

pengs <- ggplot(penguins)+
  geom_point(aes(bill_depth_mm, body_mass_g , col = species), alpha = 0.4, size = 2)+
  stat_smooth(geom = "line", 
              data = penguins %>%
                filter(species == "Adelie"), 
              alpha = 0.6,
              col = "#E41A1C", 
              aes(bill_depth_mm, body_mass_g), 
              method = "lm", se = F,
              linetype = "longdash",
              size = 1, 
              na.rm = T)+
  stat_smooth(geom = "line",
              data = penguins %>%
                filter(species == "Chinstrap"),
              alpha = 0.6,
              col = "#377EB8",
              linetype = "longdash",
              aes(bill_depth_mm, body_mass_g),
              method = "lm", se = F,
              size = 1,
              na.rm = T)+
  stat_smooth(geom = "line", 
              data = penguins %>%
                filter(species == "Gentoo"),
              alpha = 0.6, 
              col = "#4DAF4A",
              linetype = "longdash",
              aes(bill_depth_mm, body_mass_g),
              method = "lm", se = F,
              size = 1,
              na.rm = T)+
  labs(
    x = "bill depth",
    y = "body mass"
  )+
  scale_color_brewer(palette = "Set1")+
  theme_minimal()+
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(family = "NYTFranklin Light"),
    axis.text = element_text(family = "NYTFranklin Light"),
    axis.title.y = element_blank(),
    legend.title = element_text(family = "NYTFranklin Light", size = rel(1.5)),
    legend.text = element_text(family = "NYTFranklin Light", size = rel(1.2)),
    strip.text = element_text(family = "NYTFranklin Light", margin = margin(t = 50, r = 0, b = 0, l = 0)),
    legend.position = c(0.8, 0.8)
  )+
  coord_fixed(1/400)

#graph grey depth-mass relationship by overall sample

pengs2 <- ggplot(penguins)+
  geom_point(aes(bill_depth_mm, body_mass_g), alpha = 0.4, size = 2)+
  stat_smooth(geom = "line", 
              data = penguins, 
              aes(bill_depth_mm, body_mass_g), 
              method = "lm", se = F, 
              alpha = 0.6,
              col = "darkgrey",
              linetype = "longdash",
              size = 2,
              na.rm = T)+
  labs(
    x = "bill depth",
    y = "body mass"
  )+
  theme_minimal()+
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_markdown(hjust = 0, vjust = 0, size = rel(1.5), family = "NYTFranklin Light", margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title = element_text(family = "NYTFranklin Light"),
    axis.text = element_text(family = "NYTFranklin Light")
  )+
  coord_fixed(1/400)

#combine the two

pengs3 <- plot_grid(pengs2, pengs)+
  labs(
    title = "Simpson's Paradox amongst Palmer Penguins",
    subtitle = "A linear regression of bill depth on body mass shows a negative relationship. <br>  But when broken down into separate penguin species -
  <b style = 'color: #E41A1C'>Adelie</b>,
  <b style='color:#377EB8'>Chinstrap</b>, and
  <b style='color:#4DAF4A'>Gentoo</b> - <br> we find
  an intra-species positive relationship between bill depth and body mass.",
    caption = "data: Gorman, Williams, and Fraser 2014 | graphic: @beeboileau"
  )+
  theme(
    plot.title = element_text(hjust = 0.1, vjust = 0, size = rel(3.5), family = "NYTFranklin Light", margin = margin(t = 20, 0, b = 0, l = 0)),
    plot.subtitle = element_markdown(hjust = 0.1, vjust = 0, size = rel(1.5), family = "NYTFranklin Light", margin = margin(t = 20, 200, b = 0, l = 0)),
    axis.title = element_text(family = "NYTFranklin Light"),
    axis.text = element_text(family = "NYTFranklin Light"),
    plot.caption = element_text(family = "NYTFranklin Light", size = rel(1), hjust = 0.99, margin = margin(0, 0, b=10, 0))
  )
pengs3

#save!

ggsave("simpsons-paradox.png", width = 20, height = 12)
