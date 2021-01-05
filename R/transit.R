#-----load libraries-----
library(tidyverse)
library(countrycode)
library(gt)
library(paletteer)

#-----load and clean data-----
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

transit_cost_clean <- 
  transit_cost %>%
  mutate(
    continent = case_when(
      country != "UK" ~ countrycode(country, origin = 'genc2c', destination = 'continent'),
      country == "UK" ~ "Europe"
    ),
    start_year = as.double(start_year),
    end_year = as.double(start_year),
    continent = factor(continent)
  ) %>%
  filter(!is.na(country))

topbottom5 <- transit_cost_clean %>%
  filter(dense_rank(cost_km_millions) <= 5 | dense_rank(desc(cost_km_millions)) <= 5) %>%
  arrange(-cost_km_millions) 

#-----make table-----

#set up a gt theme
my_theme <- function(data) {
  tab_options(
    data = data,
    heading.subtitle.font.size = 14,
    heading.align = "left",
    row_group.border.top.width = px(10),
    row_group.border.top.color = alpha("white", 0),
    row_group.border.bottom.width = px(3),
    row_group.border.bottom.color = "lightgrey",
    table.border.top.color = "white",
    table.border.top.width = px(5),
    table.border.bottom.color = "white",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1),
    column_labels.border.top.width = px(10),
    column_labels.border.top.color = "white"
  )
}

costcolours <- c("lightblue", "white", "thistle2")
scales_costscale <- scales::col_bin(costcolours, 
                                         domain = c(min(topbottom5$cost_km_millions),
                                                    max(topbottom5$cost_km_millions)),
                                    bins = c(0, 10, 20, 30, 40, 50, 1500, 1800, 2100, 2400, 2700, 3000,
                                             3300, 3600, 3900, 4200))

table <- 
  topbottom5 %>%
  mutate(
    cost_km_millions = round(cost_km_millions, 2),
    length = round(length, 1)
  ) %>%
  select(
    City = city,
    "Start year" = start_year,
    Line = line,
    "Length (km)" = length,
    "Cost (millions) per km" = cost_km_millions
  ) %>%
  gt() %>%
  data_color(
    columns = 5,
    colors = scales_costscale
  ) %>%
  tab_row_group(
    group = "Five least expensive",
    rows = 6:10
  ) %>%
  tab_row_group(
    group = "Five most expensive transit projects",
    rows = 1:5
  ) %>%
  tab_header(
    title = md("**The five most expensive transit projects are all from New York**"),
    subtitle = md("**whereas none of the five least expensive are in America**")
  ) %>%
  tab_source_note("Data: Transit Costs Project | Visualisation: @beeboileau") %>%
  tab_footnote(footnote = "'Expensive' here means real cost per kilometre",
               locations = cells_title(
                 groups = "title"
               )
  ) %>% 
  tab_style(
    style = cell_text(font = "JetBrains Mono"),
    locations = cells_body(columns = c(2, 4:5))
  ) %>%
  tab_style(
    style = cell_text(font = "Arial"),
    locations = cells_body(columns = c(1, 3))
  ) %>%
    tab_style(
      style = cell_text(font = "Arial"),
      locations = cells_column_labels(columns = everything())
    ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(gt::everything())
  ) %>%
  cols_align(
    align = "left",
    columns = 2
  ) %>%
  fmt_currency(
    columns = 5,
    rows = c(1, 6)
  ) %>%
  my_theme() %>%
  cols_width(vars(Line) ~ px(210),
             5 ~ px(190),
             c(1:2, 4) ~ px(120))

gtsave(table, "table.png", expand = 10)
