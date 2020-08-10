library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(waffle)
library(sf)
library(rnaturalearth)
library(patchwork)
library(scales)
library(ggthemes)
library(viridis)
library(ggtext)

#load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 32)
energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals


europe_geom <- rnaturalearth::ne_countries(continent = "europe", 
                                           returnclass = "sf") %>%
  select(name_long, geometry)

#clean up
country_totals <- country_totals %>%
  pivot_longer(
    cols = c(`2016`, `2017`, `2018`),
    names_to = "year",
    values_to = "totals"
  ) 

#figure out which countries are missing
country_totals %>%
  filter(
    type == "Total net production",
    year == 2018
  ) %>%
  anti_join(europe_geom, by = c("country_name" = "name_long"))

#clean up country totals
country_totals_clean <-
  country_totals %>%
  mutate(
    country_name = case_when(
      country == "CZ" ~ "Czech Republic",
      country == "CY" ~ "Cyprus",
      country == "MT" ~ "Malta",
      country == "UK" ~ "United Kingdom",
      country == "MK" ~ "Macedonia",
      country == "TR" ~ "Turkey",
      country == "BA" ~ "Bosnia and Herzegovina",
      country == "GE" ~ "Georgia",
      TRUE ~ country_name
    )
  ) 

#clean up energy types
energy_types_clean <-
  energy_types %>%
  mutate(
    country_name = case_when(
      country == "CZ" ~ "Czech Republic",
      country == "CY" ~ "Cyprus",
      country == "MT" ~ "Malta",
      country == "UK" ~ "UK",
      country == "MK" ~ "Macedonia",
      country == "TR" ~ "Turkey",
      country == "BA" ~ "Bosnia and Herzegovina",
      country == "GE" ~ "Georgia",
      TRUE ~ country_name)
  ) %>%
  mutate(
    country_name = as.factor(country_name),
    type = as.factor(type)
  )

#-----map plot-----

#join cleaned country totals to europe geometry
europe_map <- country_totals_clean %>%
  left_join(europe_geom, by = c("country_name" = "name_long")) %>%
  st_as_sf()    

#heat-map of production in 2018
production_map <- 
  europe_map %>%
  filter(
    year == 2018,
    type == "Total net production"
  ) %>%
  ggplot()+
  geom_sf(aes(fill = totals),
          alpha = 0.5,
          col = "white")+
  stat_sf_coordinates(data = europe_map %>%
                        filter(
                          year == 2018,
                          type == "Total net production"
                        ) %>%
                        arrange(desc(totals)) %>%
                        slice(1:11),
                      alpha = 0.5)+
  scale_fill_steps2(
    high = "red2",
    labels = scales::comma,
    name = "GWh"
  )+
  theme_void()+
  theme(
    plot.subtitle = element_text(family = "NYTFranklin Light",
                                 colour = "red2",
                                 margin = margin(20, 0, 0, 0),
                                 size = rel(1.5)),
    legend.text = element_text(family = "NYTFranklin Light",
                               size = rel(0.8)),
    legend.title = element_text(family = "NYTFranklin Light",
                                size = rel(1),
                                colour = "red2"),
    legend.position = c(0.2, 0.7),
    legend.key.size = unit(1, "line")
  )+
  coord_sf(xlim = c(2500000, 6500000),
           ylim = c(1550000, 6500000),
           crs = 3035)

#-----waffle plot-----

#find exact hues used
scales::viridis_pal()(4)

#draw waffle plot
energy_composition <- 
  energy_types_clean %>%
  filter(level == "Level 1") %>%
  mutate(
    energy_type = fct_collapse(type, 
                               renewable = c("Wind", "Hydro", "Solar", "Geothermal"),
                               nuclear = "Nuclear",
                               conventional_thermal = "Conventional thermal",
                               other = "Other"
    )
  ) %>%
  count(country_name, energy_type, wt = `2018`) %>%
  group_by(country_name) %>%
  summarise(
    total = sum(n),
    energy_type,
    country_name,
    n
  ) %>%
  ungroup() %>%
  mutate(
    n = n/300
  ) %>%
  arrange(
    desc(total),
    desc(country_name)
  ) %>%
  slice(1:40) %>%
  ggplot(aes(fill = energy_type,
             values = n))+
  geom_waffle(n_rows = 10, size = 0.33, color = "white", show.legend = F)+
  coord_equal()+
  scale_fill_manual(values = c("#31688EFF", "#35B779FF", "#FDE725FF", "#440154FF"))+
  theme_enhance_waffle()+
  facet_wrap(~fct_reorder(country_name, desc(total)), ncol = 1, strip.position = "left")+
  theme_minimal(base_family = "NYTFranklin Light")+
  theme(
    plot.margin = margin(10,10,10,10),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.title = element_blank(),
    strip.text.y.left = element_text(angle=0,
                                     size = rel(2.5))
  )


                   

layout <- c(
  area(t = 1, l = 1, b = 10, r = 10),
  area(t = 5, l = 5, b = 10, r = 10)
)
waffleenergy <- 
  energy_composition + production_map+
  plot_layout(design = layout) &
  plot_annotation(
    title = "The 10 European countries using most energy in 2018",
    subtitle = "each box represents 1/300 giga-watt hour; <br>
    colours demarcate <b style = 'color:#31688EFF'>conventional thermal</b>,
    <b style = 'color:#35B779FF'>renewable</b>, and
    <b style = 'color:#FDE725FF'>nuclear</b> energy.",
    caption = "data: Eurostat\nviz: @beeboileau"
  ) &
  theme(
    plot.title = element_text(family = "NYTFranklin Light",
                              size = rel(2.5),
                              margin = margin(20, 0, 20, 200)
                              ),
    plot.subtitle = element_markdown(family = "NYTFranklin Light",
                                     size = rel(2), 
                                     margin = margin(20,0,20,200)),
    plot.caption = element_text(family = "NYTFranklin Light", 
                                margin = margin(10,10,20,10),
                                size = rel(0.9))
  )

waffleenergy
?
ggsave("waffleenergy.png", width = 15, height = 10)
