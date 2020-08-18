library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(viridis)
library(ggtext)

#load data!
tuesdata <- tidytuesdayR::tt_load(2020, week = 34)
plants <- tuesdata$plants
threats <- tuesdata$threats
actions <- tuesdata$actions

#clean data!
chr_threats <-
  threats %>%
  select_if(is.character) %>%
  colnames()

threats[,chr_threats] = lapply(threats[,chr_threats], as.factor)

threats_table <- 
threats %>%
  filter(
    !is.na(year_last_seen), 
    threatened != 0
  ) %>%
  group_by(year_last_seen) %>%
  summarise(
    threat_type = fct_lump_n(threat_type, 5)
  ) %>%
  group_by(year_last_seen) %>%
  summarise(
    fct_count(threat_type)
  ) %>%
  mutate(
    f = fct_relevel(f, "Unknown"),
    f = fct_relevel(f, "Other"),
    year_last_seen = relevel(year_last_seen, "Before 1900")
  ) 

#plot data!
ggplot(threats_table)+
  geom_bar(aes(year_last_seen, n, fill = f),
           position = "fill",
           stat = "identity")+
  scale_fill_manual(
    name = "Threat type",
    breaks = c(
      "Agriculture & Aquaculture",
      "Biological Resource Use",
      "Commercial Development",
      "Natural System Modifications",
      "Energy Production & Mining",
      "Invasive Species",
      "Climate Change",
      "Unknown",
      "Other"),
    values = c("Other" = "grey90",
               "Unknown" = "grey80",
               "Agriculture & Aquaculture" = "#440154FF",
               "Biological Resource Use"= "#443A83FF",
               "Commercial Development" = "#31688EFF",
               "Natural System Modifications" = "#21908CFF",
               "Energy Production & Mining" = "#35B779FF",
               "Invasive Species" = "#8FD744FF",
               "Climate Change" = "#FDE725FF")
  )+
  annotate("curve", x = 3, xend = 1, y = 1.1, yend = 0.75,
           arrow = arrow(length = unit(2, "mm")))+
  geom_richtext(aes(x = 3, y = 1.1, label = "Pre-1900, it was most common for the cause<br>of plants' extinction to be <b style = 'color:grey70'>unknown</b>"), 
                label.color = NA, 
                label.padding = unit(0.1, "lines"),
                family = "JetBrains Mono Light",
                size = rel(5))+
  annotate("curve", x = 8, xend = 6, y = 0.02, yend = 0.05,
           curvature = -0.3,
           arrow = arrow(length = unit(2, "mm"))
           )+
  geom_richtext(aes(x = 8, y = 0.02, label = "<b style = 'color:gold'>Climate change</b> first appeared in<br> the 5 top threats between 1980-1999"),
                label.color = NA, 
                label.padding = unit(0.1, "lines"),
                family = "JetBrains Mono Light",
                size = rel(5))+
  annotate("curve", x = 6.2, xend = 5, y = 1.1, yend = 0.8,
           curvature = -0.2,
           arrow = arrow(length = unit(2, "mm")))+
  geom_richtext(aes(x = 6.2, y = 1.1,
                    label = "The relative threat of<br> <b style = 'color:#440154FF'>agriculture & aquaculture</b> peaked in 1960-1979"),
                label.color = NA, 
                label.padding = unit(0.1, "lines"),
                family = "JetBrains Mono Light",
                size = rel(5))+
  annotate("curve", x = 8, xend = 7, y = 0.12, yend = 0.2,
           arrow = arrow(length = unit(2, "mm")))+
  geom_richtext(aes(x = 8, y = 0.12,
                    label = "<b style = 'color:#8FD744FF'>Invasive species</b> are an increasing threat"),
                label.color = NA, 
                label.padding = unit(0.1, "lines"),
                family = "JetBrains Mono Light",
                size = rel(5))+
  labs(
    title = "Evolution of the top 5 reasons for plants' extinction",
    caption = "data: IUCN | visualisation: @beeboileau"
#    subtitle = "The threat of <b style = 'color:#8FD744FF'>invasive species</b> is rising. On the other hand,<br> <b style = 'color:#21908CFF'>natural system modifications</b> are no longer among plants' top 5 threats,<br> and the threat posed by <b style = 'color:#440154FF'>agriculture & aquaculture</b> is falling."
  )+
  theme_minimal(base_family = "JetBrains Mono Light")+
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = rel(3),
                              margin = margin(20,200,20,0)),
    plot.caption = element_text(size = rel(1),
                              margin = margin(20,0,10,0)),
    legend.title = element_text(size = rel(3)),
#   plot.subtitle = element_markdown(size = rel(2),
#                                     margin = margin(0,200,20,0)),
    legend.key.size = unit(2, "line"),
    axis.text.x = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1.5)),
    plot.margin = unit(c(1,1,1,1), "cm")
  )+
  coord_cartesian(clip = "off")

#save data!
ggsave("extinction.png", width = 20, height = 10)

