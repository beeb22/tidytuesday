library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(viridis)
library(ggtext)
library(maps)
library(sf)
library(rnaturalearth)
library(ggthemes)
library(hrbrthemes)
library(transformr)
library(RColorBrewer)
library(devtools)
install_github("dgrtwo/gganimate", ref = "26ec501")

tuesdata <- tidytuesdayR::tt_load(2020, week = 36)

arableland <- tuesdata$arable_land_pin
cerealcrop_vs_fertiliser <- tuesdata$cereal_crop_yield_vs_fertilizer_application
cerealyields_vs_tractor <- tuesdata$cereal_yields_vs_tractor_inputs_in_agriculture
cropyields <- tuesdata$key_crop_yields
landuse_vs_yieldchange <- tuesdata$land_use_vs_yield_change_in_cereal_production

worldmap <- ne_countries(scale = "medium", returnclass = "sf")

landuse <- 
  landuse_vs_yieldchange %>%
  select(
    entity = Entity,
    year = Year,
    code = Code,
    cereal_yield = `Cereal yield index`,
    land_change = `Change to land area used for cereal production since 1961`,
    population = `Total population (Gapminder)`
  ) %>%
  mutate(
    entity = case_when(
      entity == "Timor" ~ "Timor-Leste",
      entity == "North Korea" ~ "Dem. Rep. Korea",
      entity == "South Korea" ~ "Republic of Korea",
      entity == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
      entity == "Congo" ~ "Republic of Congo",
      entity == "Brunei" ~ "Brunei Darussalam",
      entity == "Cote d'Ivoire" ~ "Côte d'Ivoire",
      entity == "Czechoslovakia" ~ "Czech Republic",
      entity == "Sao Tome and Principe" ~ "São Tomé and Principe",
      entity == "Russia" ~ "Russian Federation",
      entity == "Gambia" ~ "The Gambia",
      entity == "Laos" ~ "Lao PDR",
      entity == "Micronesia (country)" ~ "Federated States of Micronesia",
      TRUE ~ entity),
    year = as.numeric(year)
    ) %>%
  filter(year > 1959)

landuse_map <-
  landuse %>%
  left_join(worldmap, by = c("entity" = "name_long")) %>%
  select(entity, year, cereal_yield, land_change, population, continent, geometry, region_un) %>%
  st_as_sf()

landuse_map %>%
  filter(!is.na(geometry),
         !is.na(continent)) %>%
  ggplot()+
  geom_line(aes(x = year, y = land_change, group = entity), alpha = 0.3)+
  facet_wrap(~region_un)

landuse_map %>%
  group_by(continent) %>%
  summarise(
    topchange = max(land_change, na.rm = T)
  )

# cereal yield per population ?


crops <-
  cropyields %>%
  select(
    entity = Entity,
    code = Code,
    year = Year,
    peas = `Peas (tonnes per hectare)`,
    maize = `Maize (tonnes per hectare)`,
    soybeans = `Soybeans (tonnes per hectare)`,
    potatoes = `Potatoes (tonnes per hectare)`,
    beans = `Beans (tonnes per hectare)`,
    rice = `Rice (tonnes per hectare)`,
    cassava = `Cassava (tonnes per hectare)`,
    barley = `Barley (tonnes per hectare)`,
    cocoa = `Cocoa beans (tonnes per hectare)`,
    bananas = `Bananas (tonnes per hectare)`,
    wheat = `Wheat (tonnes per hectare)`
  ) %>%
  mutate(
    entity = case_when(
      entity == "Timor" ~ "Timor-Leste",
      entity == "North Korea" ~ "Dem. Rep. Korea",
      entity == "South Korea" ~ "Republic of Korea",
      entity == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
      entity == "Congo" ~ "Republic of Congo",
      entity == "Brunei" ~ "Brunei Darussalam",
      entity == "Cote d'Ivoire" ~ "Côte d'Ivoire",
      entity == "Czechoslovakia" ~ "Czech Republic",
      entity == "Sao Tome and Principe" ~ "São Tomé and Principe",
      entity == "Russia" ~ "Russian Federation",
      entity == "Gambia" ~ "The Gambia",
      entity == "Laos" ~ "Lao PDR",
      entity == "Micronesia (country)" ~ "Federated States of Micronesia",
      TRUE ~ entity)
  ) %>%
  filter(year > 1999)


crops_map <-
  crops %>%
  left_join(worldmap, by = c("entity" = "name_long")) %>%
  pivot_longer(cols = c("peas",
                        "maize",
                        "soybeans", 
                        "potatoes",
                        "beans",
                        "rice", 
                        "cassava",
                        "barley", 
                        "cocoa",
                        "bananas", 
                        "wheat"),
               names_to = "crop", 
               values_to = "yield") %>%
  st_as_sf()

#maize, potatoes ?

#highest yielding crops
crops %>%
  filter(year == 2018) %>%
  pivot_longer(cols = c("peas",
                        "maize",
                        "soybeans", 
                        "potatoes",
                        "beans",
                        "rice", 
                        "cassava",
                        "barley", 
                        "cocoa",
                        "bananas", 
                        "wheat"),
               names_to = "crop", 
               values_to = "yield") %>%
  group_by(crop) %>%
  summarise(
    yield = sum(yield, na.rm = T)
  ) %>%
  arrange(desc(yield))

topcrops <-
  crops_map %>%
  filter(crop == "potatoes",
         !is.na(code)) %>%
    group_by(year) %>%
    slice_max(order_by = yield, n = 5)

potatoes <- 
  crops_map %>%
  filter(crop == "potatoes") %>%
  ggplot()+
  geom_sf(aes(fill = yield, frame = year), color = "gray90", size = 0.05)+
  stat_sf_coordinates(data = topcrops,
                      geom = "text",
                      aes(label = "★",
                          frame = year),
                      family = "Apple Symbols",
                      size = 10,
                      color = "white",
                      alpha = 0.7)+
  coord_sf(crs = "+proj=robin")+
  theme_map(base_family = "NYTFranklin Light")+
  scale_fill_steps(n.breaks = 10, 
                   low = "#E5F5E0",
                   high = "#31A354",
                   na.value = "white",
                   name = "Potato yield (tonnes per hectare)")+
  theme(legend.position = "bottom",
        legend.justification = "centre",
        legend.key.size = unit(2, "cm"), 
        legend.background = element_rect(fill = "darkgray", color = "darkgray"),
        plot.background = element_rect(fill = "darkgray", color = "darkgray"))+
  labs(title = "Where were potatoes produced most efficiently between 2000-2018?",
       subtitle = "The top 5 countries in each year are starred",
       caption = "data: OWID | graphic: @beeboileau")+
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5))

gg_animate(potatoes, "world.gif", title_frame = T, ani.width=1600, ani.height=820)

