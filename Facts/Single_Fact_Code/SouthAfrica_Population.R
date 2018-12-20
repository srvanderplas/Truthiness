opt <- "SouthAfrica_Population"

## ----setup, include=FALSE, echo = F, warning = F, message = F------------
knitr::opts_chunk$set(echo = F, warning = F, message = F, dpi = 300)

# source(here::here("worldfactbook.R"))
load(here::here("Facts/Data/factbook.Rdata"))
export_col_theme <- sample(scales::hue_pal()(15), size = 15)

library(ggthemes)
library(ggmap)
library(ggrepel)
library(ggalt)

library(rworldmap)
library(sp)
library(proj4)
library(rgdal)
library(RgoogleMaps)

library(tidyverse)

world <- readOGR(here::here("Facts/Data/countries.geo.json"), "OGRGeoJSON", stringsAsFactors = FALSE)
world_data <- data_frame(
  name = as.character(world@data$name),
  id = rownames(world@data)
)

world_map <- fortify(world) %>%
  left_join(world_data)
rm(world_data)
world <- map_data("world")
world2 <- map_data("world2")

## ---- out.width = "60%"--------------------------------------------------
country <- "South Africa"
filter(borders, name == country) %>%
  unnest() %>%
  bind_rows(data_frame(name = country, country = "Coast", length = unique(.$coast))) %>%
  arrange(desc(length)) %>%
  select(country, length) %>%
  mutate(country = factor(country, levels = country, ordered = T)) %>%
  ggplot() + 
  geom_bar(aes(x = country, y = length, fill = country), stat = "identity") + 
  xlab("") + 
  ylab("Length (km)") + 
  scale_fill_brewer("Border With:", type = "qual", palette = "Dark2", guide = F) + 
  ggtitle(sprintf("%s's Border Regions", country))

ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_rel_topic_unrel_nonprobative.png", opt), 
       width = 5, height = 5, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
country <- "France"
filter(borders2, name == country) %>%
  unnest() %>%
  bind_rows(data_frame(name = country, country = "Coast", length = 3427)) %>%
  arrange(desc(length)) %>%
  select(country, length) %>%
  mutate(country = factor(country, levels = country, ordered = T)) %>%
  ggplot() + 
  geom_bar(aes(x = country, y = length, fill = country), stat = "identity") + 
  xlab("") + 
  ylab("Length (km)") + 
  scale_fill_brewer("Border With:", type = "qual", palette = "Paired", guide = F) + 
  ggtitle(sprintf("%s's Border Regions", country))

ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_unrel_topic_unrel_nonprobative.png", opt), 
       width = 7, height = 4, dpi = 300)


## ---- out.width = "60%"--------------------------------------------------
region <- "(Southern|Southwestern|Southeastern) Africa"
country <- "South Africa"
filter(location, str_detect(desc, region)) %>%
  select(name, label_lat, label_long) %>%
  left_join(population, by = 'name') %>%
  arrange(desc(label_lat), age) %>%
  mutate(name = factor(name, levels = unique(name)),
         age = factor(age, levels = rev(levels(age)))) %>%
  filter(!is.na(Pct)) %>%
  ggplot() + 
  geom_col(aes(x = name, y = Pct, fill = age), color = "black", position = "stack") + 
  scale_fill_brewer("Age", type = "qual", palette = "Greens") + 
  ggtitle("Age Distribution of Countries in Southern Africa") + 
  ylab("% Population") + 
  theme(axis.title.y = element_blank()) + 
  coord_flip()
ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_rel_topic_rel_probative.png", opt), 
       width = 6, height = 5, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
region <- "(North)?(South)?[Ww]estern Europe"
country <- "France"
filter(location, str_detect(desc, region)) %>%
  select(name, label_lat, label_long) %>%
  left_join(population, by = 'name') %>%
  arrange(desc(label_lat), age) %>%
  mutate(name = factor(name, levels = unique(name)),
         age = factor(age, levels = rev(levels(age)))) %>%
  filter(!is.na(Pct)) %>%
  ggplot() + 
  geom_col(aes(x = name, y = Pct, fill = age), color = "black", position = "stack") + 
  scale_fill_brewer("Age", type = "qual", palette = "Greens") + 
  ggtitle("Age Distribution of Countries in Western Europe") + 
  ylab("% Population") + 
  theme(axis.title.y = element_blank()) + 
  coord_flip()
ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_unrel_topic_rel_nonprobative.png", opt), 
       width = 6, height = 5, dpi = 300)

## ---- out.width = "60%", include = F-------------------------------------
x <- filter(location, name == "South Africa")
# newmap <- GetMap(center = c(x$label_lat, x$label_long), zoom = 5, destfile = "SouthAfricaMap.png")
if (!file.exists(here::here("Facts/Data/SouthAfricaMapTiles.Rdata"))) {
  SouthAfricamaptiles <- get_googlemap(center = c( x$label_long, x$label_lat), zoom = 5, maptype = "roadmap") 
  save(SouthAfricamaptiles, file = here::here("Facts/Data/SouthAfricaMapTiles.Rdata"))
} else {
  load(here::here("Facts/Data/SouthAfricaMapTiles.Rdata"))
}
SouthAfricamaptiles %>% ggmap() + 
  theme_map() 
ggsave(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_unrel_nonprobative.png", opt), 
       width = 5, height = 5, dpi = 300)

## ---- out.width = "60%", include = F-------------------------------------
x <- filter(location, name == "France")
# newmap <- GetMap(center = c(x$label_lat+1, x$label_long), zoom = 6, destfile = "FranceMap.png")
if (!file.exists(here::here("Facts/Data/FranceMapTiles.Rdata"))) {
  Francemaptiles <- get_googlemap(center = c( x$label_long, x$label_lat), zoom = 6, maptype = "roadmap") 
  save(Francemaptiles, file = here::here("Facts/Data/FranceMapTiles.Rdata"))
} else {
  load(here::here("Facts/Data/FranceMapTiles.Rdata"))
}
Francemaptiles %>% ggmap() + 
  theme_map() 
ggsave(sprintf("Facts/Pictures_all/%s-map_subj_unrel_topic_unrel_nonprobative.png", opt), 
       width = 5, height = 5, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
region <- "(Southern|Southwestern|Southeastern|Central|Eastern) Africa"
region2 <- "(Southern|Southwestern|Southeastern) Africa"
tmp <- filter(location, str_detect(desc, region)) %>%
  select(name) %>%
  left_join(population, by = 'name') %>%
  mutate(name = str_replace_all(name, c("Eswatini" = "Swaziland", "Congo, Democratic Republic of the" = "Democratic Republic of the Congo"))) %>%
  filter(age == "25-54") %>%
  filter(!is.na(Pct)) %>%
  select(name, pct_working_age = Pct) %>%
  full_join(world, by = c("name" = "region"))

lims <- tmp %>%
  filter(is.na(subregion)) %>%
  right_join(filter(location, str_detect(desc, region2))) %>%
  select(lat, long) %>%
  summarize_all(.funs = funs(min, max), na.rm = T)


tmp %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = pct_working_age, group = group), color = "black") + 
  coord_quickmap(xlim = c(lims$long_min, lims$long_max), ylim = c(lims$lat_min, -10), expand = T) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) + 
  scale_fill_gradient("% Population\n25-54", low = "white", high = "darkgreen", limits = c(25, 75)) + 
  ggtitle("Southern Africa Working-Age Population") + 
  theme(legend.position = c(1, 0), legend.justification = c(1, 0))

ggsave(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_rel_nonprobative.png", opt), 
       width = 7, height = 4, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
region <- "Europe"
region2 <- "(North)?(South)?[Ww]estern Europe"
tmp <- filter(location, str_detect(simple, region)) %>%
  select(name) %>%
  left_join(population, by = 'name') %>%
  mutate(name = str_replace_all(name, c("United Kingdom" = "UK", "Congo, Democratic Republic of the" = "Democratic Republic of the Congo"))) %>%
  filter(age == "0-14") %>%
  filter(!is.na(Pct)) %>%
  select(name, pct_working_age = Pct) %>%
  full_join(world, by = c("name" = "region"))

lims <- tmp %>%
  filter(is.na(subregion)) %>%
  right_join(filter(location, str_detect(desc, region2))) %>%
  select(lat, long) %>%
  summarize_all(.funs = funs(min, max), na.rm = T)


tmp %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = pct_working_age, group = group), color = "black") + 
  coord_quickmap(xlim = c(lims$long_min, lims$long_max), ylim = c(lims$lat_min, lims$lat_max), expand = T) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) + 
  scale_fill_gradient("% Population\n0-14", low = "white", high = "darkgreen", limits = c(10, 30)) + 
  ggtitle("Population under 15 years old") + 
  theme(legend.position = c(0, .6), legend.justification = c(0, .6), legend.direction = "vertical",
        legend.background = element_rect(fill = "white"))

ggsave(sprintf("Facts/Pictures_all/%s-map_subj_unrel_topic_rel_nonprobative.png", opt), 
       width = 4, height = 6.5, dpi = 300)

## ---- out.width = "60%", message = F, warning = F------------------------
region <- "(Southern|Southwestern|Southeastern|Central|Eastern) Africa"
region2 <- "(Southern|Southwestern|Southeastern) Africa"
tmp <- filter(location, str_detect(desc, region)) %>%
  select(name) %>%
  left_join(population, by = 'name') %>%
  mutate(name = str_replace_all(name, c("Eswatini" = "Swaziland", "Congo, Democratic Republic of the" = "Democratic Republic of the Congo"))) %>%
  filter(age == "0-14") %>%
  filter(!is.na(Pct)) %>%
  select(name, pct_working_age = Pct) %>%
  full_join(world, by = c("name" = "region"))

lims <- tmp %>%
  filter(is.na(subregion)) %>%
  right_join(filter(location, str_detect(desc, region2))) %>%
  select(lat, long) %>%
  summarize_all(.funs = funs(min, max), na.rm = T)


tmp %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = pct_working_age, group = group), color = "black") + 
  coord_quickmap(xlim = c(lims$long_min, lims$long_max), ylim = c(lims$lat_min, -10), expand = T) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.position = c(1, 0), legend.direction = "horizontal", legend.justification = c(1, 0), legend.background = element_rect(fill = "transparent")) + 
  scale_fill_gradient("% Population\n0-14", low = "white", high = "darkgreen", limits = c(15, 50)) + 
  ggtitle("Population under 15 years old")

ggsave(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_rel_probative.png", opt), 
       width = 7, height = 4, dpi = 300)

