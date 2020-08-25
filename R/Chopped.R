library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(stringi)
library(patchwork)
library(scales)
library(ggtext)

#-----load data!-----

tuesdata <- tidytuesdayR::tt_load(2020, week = 35)
chopped <- tuesdata$chopped 

#-----clean data-----

#sort out accent problems
chopped$judge1 <- stri_trans_general(chopped$judge1, "Latin-ASCII")
chopped$judge2 <- stri_trans_general(chopped$judge2, "Latin-ASCII")
chopped$judge3 <- stri_trans_general(chopped$judge3, "Latin-ASCII")

#separate foodstuffs
chopped <- 
  chopped %>%
  separate(appetizer, into = c("app1", "app2", "app3", "app4"), sep = ", ") %>%
  separate(entree, into = c("ent1", "ent2", "ent3", "ent4", "ent5", "ent6"), sep = ", ") %>%
  separate(dessert, into = c("des1", "des2", "des3", "des4", "des5"), sep = ", ")

#figure out top foods - 11 top foods (because of ties in the rank-ordered data!)
topfood <-
  chopped %>%
  pivot_longer(cols = c("app1", "app2", "app3", "app4",
                        "ent1", "ent2", "ent3", "ent4", "ent5", "ent6",
                        "des1", "des2", "des3", "des4", "des5"), 
               names_to = "meal_part", 
               values_to = "food") %>%
  select(series_episode, episode_rating, food) %>%
  count(food) %>%
  filter(
    !is.na(food)
  ) %>%
  arrange(desc(n)) %>%
  head(11) %>%
  left_join(chopped %>%
              pivot_longer(cols = c("app1", "app2", "app3", "app4",
                                    "ent1", "ent2", "ent3", "ent4", "ent5", "ent6",
                                    "des1", "des2", "des3", "des4", "des5"), 
                           names_to = "meal_part", 
                           values_to = "food"), by = "food") %>%
  select(series_episode, episode_rating, food, n)

#calculate required variables to draw graph
foodpositions <-
  topfood %>%
  group_by(food) %>%
  summarise(
    n = mean(n), 
    mean_IMDB = mean(episode_rating, na.rm = T) #this will allow the average horizontal line to exist for each food
  ) %>%
  arrange(desc(mean_IMDB)) %>% #make the graph in a nice order
  mutate(
    number = row_number() - 1, 
    position_max = cumsum(n) + number, #add little gap between each line to make food separation easire to see
    position_min = position_max - n
  ) %>%
  left_join(topfood) %>%
  mutate(
    rownumber = row_number() + number #make sure dot plot will line up with average food lines by adding gaps
  ) 

foodlabels <-
  foodpositions %>%
  group_by(food) %>%
  summarise(
    position = mean((position_min+position_max)/2), #in the middle of each food line - helpful fo rlabels
    mean_IMDB = mean(mean_IMDB), #this gives us an x point!
    characters = mean(stri_count_boundaries(food, type = "character")) #want to size the food label according to character count fo the food
  )

#figure out a bok choy colour !
show_col(hue_pal(l = 45)(11))


#-----draw plot-----
ggplot(foodpositions)+
  #this is the average lines
  geom_segment(aes(x = position_min, xend = position_max, y = mean_IMDB, yend = mean_IMDB, col = food),
               show.legend = F,
               alpha = 0.05,
               size = 3)+
  #this is the lines for each episode!
  geom_segment(aes(x = rownumber, xend = rownumber, y = mean_IMDB, yend = episode_rating, col = food,), alpha = 0.8,
               show.legend = F)+
  #points for each episode
  geom_point(aes(rownumber, episode_rating, col = food), alpha = 0.3,
             show.legend = F)+
  #text with foodstuff
  geom_text(data = foodlabels,
            aes(x = position,
                y = mean_IMDB + 0.05 + 0.2/characters,
                label = toupper(food),
                color = food, 
                size = 1/characters + 100), #trying to size according to no. of characters (tricky?)
            alpha = 0.5,
            family = "NYTFranklin Light",
            show.legend = F)+
  scale_size(range = c(5, 9), guide = F)+
  scale_colour_hue(l=55)+
  labs(
    y = "IMDB score per episode",
    title = "Frequent Foods and IMDB Ratings",
    subtitle = "11 most frequently used foodstuffs on Chopped, with the IMDB scores of the episodes on which they appeared",
    caption = "viz: @beeboileau | data = Kaggle  "
  )+
  ylim(c(5.3,9.5))+
  annotate("curve", x = 40, xend = 12, y = 6.2, yend = 7.1,
           curvature = -0.2,
           alpha = 0.8,
           arrow = arrow(length = unit(2, "mm")))+
  geom_richtext(aes(x = 40, y = 6.2, label = "each point represents one episode;<br>its position represents the IMDB rating"),
           alpha = 0.8,
           family = "NYTFranklin Light",
           label.color = NA,
           label.padding = unit(0.2, "lines"))+
  annotate("curve", x = 100, xend = 151, y = 6, yend = 5.4,
           curvature = 0.2,
           alpha = 0.8,
           arrow = arrow(length = unit(2, "mm")))+
  geom_richtext(aes(x = 100, y = 6, label = "the lowest-rated episode appeared on by <b style = 'color:#2B7D00'>bok choy</b><br>(and any of the most frequent foods) was 'Worst Cooks Challenge', with a rating of 5.5"),
                family = "NYTFranklin Light",
                alpha = 0.8,
                label.color = NA,
                label.padding = unit(0.2, "lines"))+
  theme_minimal(base_family = "NYTFranklin Light")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = rel(3), margin = margin(20,0,10,0)),
    plot.subtitle = element_text(size = rel(2)),
    plot.caption = element_text(size = rel(1), margin = margin(0,0,10,0)),
    axis.title.y = element_text(size = rel(1.5), margin = margin(0,10,0,20)),
    axis.text.y = element_text(size = rel(1.5))
  )

ggsave("foodstuffs.png", width = 20, height = 10)

