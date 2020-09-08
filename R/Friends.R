library(tidytuesdayR)
library(tidyverse)
library(RColorBrewer)
library(stringi)
library(gt)
library(purrr)

#-----Load data-----

tuesdata <- tidytuesdayR::tt_load(2020, week = 37)

friends <- tuesdata$friends
friends_info <- tuesdata$friends_info
friends_emotions <- tuesdata$friends_emotions


#-----Clean data-----

friends_table <- 
  friends_info %>%
  group_by(season) %>%
  summarise(
    first_aired = first(air_date),
    av_imdb = mean(imdb_rating),
    av_viewers = mean(us_views_millions)
  )

#-----Create bar chart-----

barchart1  <- 
  friends_words %>%
  filter(season == 1) %>%
  ggplot()+
  geom_col(aes(x = speaker, y = speaker_words, fill = speaker), show.legend = F)+
  geom_text(aes(x = speaker, y = 11000, label = paste0(substr(speaker, 1, 2))),
            size = 50,
            colour = "darkgray",
            family = "JetBrains Mono")+
  scale_fill_viridis_d()+
  theme_void()+
  coord_cartesian(ylim = c(6000, 13000))


plot_group <-
  function(df) {
    plot_object <- 
      ggplot(data = df)+
      geom_col(aes(x = speaker, y = speaker_words, fill = speaker), show.legend = F)+
      scale_fill_viridis_d()+
      theme_void()+
      coord_cartesian(ylim = c(6000, 13000))
    return(plot_object)
  }

tibble_plot <- 
  friends_words %>%
  group_by(season) %>%
  nest() %>%
  mutate(plot = map(data, plot_group)) %>%
  select(-data)

#-----Draw table-----

#Get palette
brewer.pal(n = 8, name = "Purples")

friendstable <- 
  friends_table %>%
  mutate(ggplot = NA) %>%
  select(season, av_imdb, av_viewers, ggplot) %>%
  gt() %>%
  tab_header(
    title = "Friends Seasons",
    subtitle = html("<em> IMDB ranking, average viewers, and verbose characters</em>")
  ) %>%
  text_transform(
    locations = cells_body(columns = vars(ggplot)),
    fn = function(x) {
      map(tibble_plot$plot, ggplot_image, height = px(50), aspect_ratio = 5)
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = 4,
                           rows = 1),
    fn = function(x) {
      ggplot_image(barchart1, height = px(50), aspect_ratio = 5)
    }
  ) %>%
  cols_label(
    av_imdb = "Average IMDB score",
    av_viewers = "Average viewers",
    season = "Season",
    ggplot = "Words per character each season"
  ) %>%
  fmt_number(
    columns = vars(season),
    decimals = 0
  ) %>%
  fmt_number(
    columns = vars(av_imdb, av_viewers),
    decimals = 2
  ) %>%
  tab_footnote(
    footnote = "US, millions of viewers",
    locations = cells_column_labels(
      vars(av_viewers)
    )
  ) %>%
  data_color(
    columns = vars(av_imdb),
    colors = scales::col_numeric(palette = c("#C7E9C0",
                                             "#A1D99B",
                                             "#74C476",
                                             "#41AB5D",
                                             "#238B45",
                                             "#005A32"), domain = NULL)
  ) %>%
  data_color(
    columns = vars(av_viewers),
    colors = scales::col_numeric(palette = c("#DADAEB",
                                             "#BCBDDC",
                                             "#9E9AC8",
                                             "#807DBA",
                                             "#6A51A3",
                                             "#4A1486"), domain = NULL)
  ) %>%
  summary_rows(
    columns = vars(av_imdb, av_viewers),
    fns = list(
      Average = ~mean(.)
    )
  ) %>%
  cols_align(align = "center",
             columns = 2
  ) %>%
  tab_style(
    style = cell_text(font = "JetBrains Mono"),
    locations = list(cells_body(), cells_grand_summary())
  ) %>%
  tab_options(
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.width = px(3)
  ) %>%
  tab_source_note(source_note = html("<b>Data</b>: {friends} package | <b>Table</b>: @beeboileau"))

#-----Save table-----

gtsave(friendstable, "friendstable.png")

