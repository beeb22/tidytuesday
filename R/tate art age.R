#load libraries-----
library(scales)
library(ggtext)
library(patchwork)
library(tidyverse)
library(tidytuesdayR)

#load data-----
tuesdata <- tidytuesdayR::tt_load('2021-01-12')
artists <- tuesdata$artists
artwork <- tuesdata$artwork

#wrangle data-----
artwork_graph <- artwork %>%
  filter(!is.na(acquisitionYear),
         !is.na(year)) %>%
  mutate(
    recency = acquisitionYear - year,
    decade = acquisitionYear - (acquisitionYear %% 10)
  ) %>%
  filter(recency >= 0) %>%
  group_by(decade) %>%
  mutate(av_recency = mean(recency, na.rm = T)) %>%
  ungroup()

#plot data-----
artwork_graph %>%
  ggplot()+
  geom_point(aes(x = acquisitionYear, y = recency), alpha = 0.1, size = rel(3))+
  geom_point(data = artwork_graph %>%
               filter(recency >= 410),
             mapping = aes(x = acquisitionYear, y = recency), shape = 1, size = rel(3))+
  geom_line(aes(x = acquisitionYear, y = av_recency), col = "orange", size = rel(3))+
  annotate(geom = "text", x = 1890, y = 310, label = "The oldest painting acquired by the Tate\n('Portrait of an Unknown Lady' by Hans Eworth)\nwas acquired in 1984, when it was 419 years old.",
           colour = "black", family = "Helvetica", size = rel(5))+
  geom_curve(aes(x = 1984, y = 419, xend = 1890, yend = 340), curvature = 0.2)+
  labs(
    title = "**How new are the paintings the Tate acquires?**",
    subtitle = "Each dot represents a painting in the Tate: each dot's height represents its age at acquisition.
    <br>Paintings' <i style = 'color:orange'>average age on acquisition</i> has not risen over time, although the range of paintings' ages has increased.<br>Before 1850, no paintings more than 100 years old, and no brand new paintings, were acquired.<br>After 1950 the oldest paintings acquired were over 400 years old; large numbers of brand new paintings were acquired.",
    x = "Year acquired",
    y = "Years between creation and acquisition",
    caption = "data: Tate | visualisation: @beeboileau"
  )+
  theme_minimal(base_family = "Helvetica", base_size = 20)+
  theme(
    axis.text = element_text(family = "JetBrains Mono"),
    plot.title = element_markdown(margin = margin(15,0,15,0)),
    plot.subtitle = element_markdown(margin = margin(0,0,30,0)),
    axis.title.y = element_text(margin = margin(0,15,0,0), face = "bold"),
    axis.title.x = element_text(face = "bold"),
    plot.background = element_rect(fill = "#F0EFEB",
                                   color = "#F0EFEB")
  )

#save graph-----
ggsave("paintingsage.png", height = 12, width = 16)
