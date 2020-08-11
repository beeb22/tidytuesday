library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(waffle)
library(sf)
library(patchwork)
library(scales)
library(ggthemes)
library(viridis)
library(ggtext)
library(stringi)
library(gghighlight)
library(tvthemes)

import_avatar()

#load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 33)
avatar <- tuesdata$avatar
scene_desc <- tuesdata$scene_description

import_avatar()

#count words
avatar_bookwords <- 
  avatar %>%
  filter(
    character != "Scene Description"
  ) %>%
  mutate(
    words_per_line = stri_count_words(character_words),
    book = as.factor(book),
    character = as.factor(character)
  ) %>%
  group_by(book, character) %>%
  summarise(
    book_words = sum(words_per_line)
  ) 

avatar_bookwords_water <-
  avatar_bookwords %>%
  filter(
    book == "Water"
  ) %>%
  arrange(desc(book_words)) %>%
  mutate(
    character = fct_lump_n(character, 5, w = book_words)
  )

avatar_bookwords_fire <-
  avatar_bookwords %>%
  filter(
    book == "Fire"
  ) %>%
  arrange(desc(book_words)) %>%
  mutate(
    character = fct_lump_n(character, 5, w = book_words)
  )

avatar_bookwords_earth <-
  avatar_bookwords %>%
  filter(
    book == "Earth"
  ) %>%
  arrange(desc(book_words)) %>%
  mutate(
    character = fct_lump_n(character, 5, w = book_words)
  )

scales::viridis_pal()(6)
    
wordspercharacter<-
  rbind(avatar_bookwords_water, avatar_bookwords_earth, avatar_bookwords_fire) %>%
  ggplot()+
  geom_waffle(aes(fill = character, values = book_words/50), ncols = 5, size = 0.33, color = "white")+
  coord_equal()+
  scale_fill_manual(values = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF",  "gray90", "#FDE725FF"))+
  facet_wrap(~book, ncol = 1, strip.position = "left")+
  labs(
    title = "words per character in each book",
    subtitle = "each square represents 50 words; the top 5 characters in each book are highlighted"
  )+
  theme_minimal(base_family = "Slayer")+
  theme(
    plot.margin = margin(10,10,10,10),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    strip.text.y.left = element_text(angle=0,
                                     size = rel(1)),
    plot.title = element_text(size = rel(1),
                              margin = margin(20,0,30,0)),
    plot.subtitle = element_text(size = rel(0.8),
                                 margin = margin(0,0,30,0)),
    legend.title = element_text(size = rel(0.8)),
    legend.text = element_text(size = rel(0.6))
  )

linespercharacter <-
  avatar %>%
  filter(
    character == c("Aang", "Iroh", "Katara", "Sokka", "Zuko", "Toph")
  ) %>%
  mutate(chapter_num = case_when(
    book == "Earth" ~ chapter_num + 20,
    book == "Fire" ~ chapter_num + 41,
    TRUE ~ chapter_num
  )) %>%
  group_by(book, chapter, chapter_num, character) %>%
  summarise(
    line_count = n()
  ) %>%
  ggplot()+
  geom_area(aes(chapter_num, line_count, fill = character), alpha = 0.5, show.legend = F)+
  scale_fill_manual(values = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#FDE725FF", "#7AD151FF"))+
  labs(
    title = "lines per character as the show progresses"
  )+
  theme_minimal(base_family = "Slayer")+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size = rel(1),
                              margin = margin(50,0,50,0))
  )+
  facet_wrap(~character)


wordspercharacter / linespercharacter +
  plot_annotation(
    title = "Who talks most in Avatar: The Last Airbender?",
    caption = "data: appa package\nvisualisation: Bee Boileau",
    theme = theme(plot.title = element_text(family = "Slayer", 
                                            size = rel(2),
                                            margin = margin(20,0,20,0)),
                  plot.caption = element_text(family = "Slayer", 
                                              size = rel(0.7),
                                              margin = margin(20,20,20,20))
    )
  ) 


ggsave("avatarattempt.png", width = 12, height = 12)