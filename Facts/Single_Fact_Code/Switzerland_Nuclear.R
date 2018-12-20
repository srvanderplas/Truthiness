opt <- "Switzerland_Nuclear"

## ----setup, include=FALSE, echo = F, warning = F, message = F------------
knitr::opts_chunk$set(echo = F, warning = F, message = F, dpi = 300)
# source(here::here("worldfactbook.R"))
load(here::here("Facts/Data/factbook.Rdata"))
# 
library(ggthemes)
library(ggmap)

library(tidyverse)

world <- map_data("world")

nuclear <- read_csv(here::here("Facts/Data/energy-pop-exposure-nuclear-plants-locations_reactors.csv")) %>%
  mutate(Country = str_replace_all(Country, c("UNITED STATES OF AMERICA" = "UNITED STATES",
                                              "CZECH REPUBLIC" = "CZECHIA", 
                                              "IRAN(.*)" = "IRAN",
                                              "KOREA, REPUBLIC OF" = "KOREA, SOUTH", 
                                              "RUSSIAN FEDERATION" = "RUSSIA", 
                                              "SLOVAK REPUBLIC" = "SLOVAKIA",
                                              "TAIWAN, CHINA" = "TAIWAN"))) %>%
  left_join(select(location, "name") %>% mutate(Country = str_to_upper(name))) %>%
  filter(Status == "Operational")


## ---- out.width = "60%"--------------------------------------------------
filter(population, name == "Switzerland") %>%
  select(name, age, Female, Male) %>%
  gather(key = gender, value = value, -name, -age) %>%
  mutate(value = as.numeric(value)) %>%
ggplot() + 
  geom_col(aes(x = age, y = value/1e3, fill = gender), position = "dodge", color = "black") + 
  ggtitle("Population of Switzerland") + 
  xlab("Age") + 
  scale_y_continuous("Population (Thousands)") + 
  scale_fill_discrete("Gender") + 
  theme(legend.position = c(1, 1), legend.justification = c(1,1), legend.background = element_rect(fill = "transparent"))

ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_rel_topic_unrel_nonprobative.png", opt), 
       width = 5, height = 5, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
filter(population, name == "United States") %>%
  select(name, age, Female, Male) %>%
  gather(key = gender, value = value, -name, -age) %>%
  mutate(value = as.numeric(value)) %>%
ggplot() + 
  geom_col(aes(x = age, y = value/1e6, fill = gender), position = "dodge", color = "black") + 
  ggtitle("Population of the United States") + 
  xlab("Age") + 
  scale_y_continuous("Population (Millions)") + 
  scale_fill_discrete("Gender") + 
  theme(legend.position = c(1, 1), legend.justification = c(1,1), legend.background = element_rect(fill = "transparent"))
ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_unrel_topic_unrel_nonprobative.png", opt), 
       width = 5, height = 5, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
electricity_all %>%
  right_join(
    filter(location, simple == "Europe"),
    by = c("abbr", "name")
  ) %>%
  filter(!name %in% c("World", "European Union", "Antarctica") & ! str_detect(name, "Ocean")) %>%
  select(name, Fossil = FossilFuels, Hydroelectric = HydroelectricPlants, Nuclear = NuclearFuels, Renewables = OtherRenewableSources) %>%
  arrange(desc(Nuclear)) %>%
  filter(row_number() <= 12) %>%
  arrange(desc(Nuclear + Renewables + Hydroelectric)) %>%
  mutate(Other = 0) %>%
  mutate(name = factor(name, levels = name, labels = str_replace(name, " ", "\n"))) %>%
  gather(key = "Type", value = value, -name) %>%
  mutate(Type = factor(Type, levels = rev(c("Nuclear", "Renewables", "Hydroelectric", "Other", "Fossil")))) %>%
  group_by(name) %>%
  mutate(value = ifelse(Type == "Other", 100 - sum(value), value),
         value = ifelse(Type == "Other", pmax(0, value), value),
         value = value/sum(value)*100) %>%
  ggplot() + 
  geom_col(aes(x = name, y = value, fill = Type), color = "black") + 
  xlab("") + 
  scale_fill_manual("Type", values = c("Fossil" = "grey40", "Hydroelectric" = "#4292c6", "Nuclear" = "#ec7014", "Renewables" = "#41ab5d", "Other" = "#f768a1")) + 
  scale_y_continuous("Percent of Electrical Generation", breaks = c(0, 20, 40, 60, 80, 100), minor_breaks = seq(0, 100, by = 10)) + 
  theme(legend.position = "bottom", axis.title.x = element_blank()) + 
  ggtitle("Electrical Generation in Europe")

ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_rel_topic_rel_probative.png", opt), 
       width = 7.5, height = 5, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
electricity_all %>%
  right_join(
    filter(location, simple == "North America"),
    by = c("abbr", "name")
  ) %>%
  filter(!name %in% c("World", "European Union", "Antarctica", "Greenland") & ! str_detect(name, "Ocean")) %>%
  select(name, Fossil = FossilFuels, Hydroelectric = HydroelectricPlants, Nuclear = NuclearFuels, Renewables = OtherRenewableSources) %>%
  arrange(desc(Nuclear)) %>%
  mutate(Other = 0) %>%
  mutate(name = factor(name, levels = name, labels = str_replace_all(name, c(" " = "\n", "Saint\\nPierre\\nand\\nMiquelon" = "Saint Pierre\nand Miquelon")))) %>%
  gather(key = "Type", value = value, -name) %>%
  mutate(Type = factor(Type, levels = c("Nuclear", "Renewables", "Hydroelectric", "Other", "Fossil"))) %>%
  group_by(name) %>%
  mutate(value = ifelse(Type == "Other", 100 - sum(value), value),
         value = ifelse(Type == "Other", pmax(0, value), value),
         value = value/sum(value)*100) %>%
  ggplot() + 
  geom_col(aes(x = name, y = value, fill = Type), color = "black") + 
  xlab("") + 
  ylab("Percent of Electrical Generation") + 
  scale_fill_manual("Type", values = c("Fossil" = "grey40", "Hydroelectric" = "#4292c6", "Nuclear" = "#ec7014", "Renewables" = "#41ab5d", "Other" = "#f768a1")) + 
  theme(legend.position = "bottom", axis.title.x = element_blank()) + 
  ggtitle("Electrical Generation in North America")
ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_unrel_topic_rel_nonprobative.png", opt), 
       width = 6, height = 5, dpi = 300)

## ---- out.width = "60%", include = T-------------------------------------
# europemap <- GetMap(center = c(50, 10), zoom = 4, destfile = "EuropeMap.png")

if (!file.exists(here::here("Facts/Data/europeMapTiles.Rdata"))) {
  europemaptiles <- get_googlemap(center = c(10, 50), zoom = 5, maptype = "terrain") 
  save(europemaptiles, file = here::here("Facts/Data/europeMapTiles.Rdata"))
} else {
  load(here::here("Facts/Data/europeMapTiles.Rdata"))
}
europemaptiles %>% ggmap() + 
  theme_map() + 
  ggtitle("Europe")
ggsave(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_unrel_nonprobative.png", opt), 
       width = 5, height = 5, dpi = 300)


## ---- out.width = "60%", include = T-------------------------------------
# usmap <- GetMap(center = c(40, -96), zoom = 4, destfile = "USMap.png")
if (!file.exists(here::here("Facts/Data/USMapTiles.Rdata"))) {
  USmaptiles <- get_googlemap(center = c(-96, 40), zoom = 4, maptype = "terrain") 
  save(USmaptiles, file = here::here("Facts/Data/USMapTiles.Rdata"))
} else {
  load(here::here("Facts/Data/USMapTiles.Rdata"))
}
USmaptiles %>% ggmap() + 
  theme_map() + 
  ggtitle("United States")

ggsave(sprintf("Facts/Pictures_all/%s-map_subj_unrel_topic_unrel_nonprobative.png", opt), 
       width = 5, height = 5, dpi = 300)

## ---- out.width = "80%"--------------------------------------------------
plants <- filter(nuclear, str_detect(Region, "Europe")) %>%
  filter(!is.na(Latitude)) %>%
  select(name, Plant, NumReactor, Latitude, Longitude) %>%
  mutate(Latitude = jitter(Latitude, amount = .1), Longitude = jitter(Longitude, amount = .1))

if (!file.exists(here::here("Facts/Data/europeMapTiles.Rdata"))) {
  europemaptiles <- get_googlemap(center = c(10, 50), zoom = 5, maptype = "terrain") 
  save(europemaptiles, file = here::here("Facts/Data/europeMapTiles.Rdata"))
} else {
  load(here::here("Facts/Data/europeMapTiles.Rdata"))
}
ggmap(europemaptiles) +
  theme_void() + 
  geom_point(aes(x = Longitude, y = Latitude), shape = 17, alpha = .75, color = "blue", data = plants) + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background = element_rect(fill = "transparent")) +
  ggtitle("Nuclear Power Plants in Europe")

ggsave(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_rel_nonprobative.png", opt), 
       width = 5, height = 5, dpi = 300)

## ---- out.width = "80%"--------------------------------------------------
plants <- filter(nuclear, str_detect(Region, "America - (North|Latin)")) %>%
  filter(!is.na(Latitude)) %>%
  select(name, Plant, NumReactor, Latitude, Longitude) %>%
  mutate(Latitude = jitter(Latitude, amount = .1), Longitude = jitter(Longitude, amount = .1))

if (!file.exists(here::here("Facts/Data/USMapTiles.Rdata"))) {
  USmaptiles <- get_googlemap(center = c(-96, 40), zoom = 4, maptype = "terrain") 
  save(USmaptiles, file = here::here("Facts/Data/USMapTiles.Rdata"))
} else {
  load(here::here("Facts/Data/USMapTiles.Rdata"))
}

load(here::here("Facts/Data/USMapTiles.Rdata"))
ggmap(USmaptiles) + 
  theme_void() + 
  geom_point(aes(x = Longitude, y = Latitude), shape = 17, alpha = .75, color = "blue", data = plants) + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.background = element_rect(fill = "transparent")) +
  ggtitle("Nuclear Power Plants in North America")
ggsave(sprintf("Facts/Pictures_all/%s-map_subj_unrel_topic_rel_nonprobative.png", opt), 
       width = 5, height = 5, dpi = 300)


## ---- out.width = "60%", message = F, warning = F------------------------
europe_nuke <- filter(location, simple == "Europe" | name == "Russia") %>%
  select(name) %>%
  left_join(select(areas, name, total)) %>%
  left_join(select(electricity_all, name, NuclearFuels)) %>%
  mutate(name = str_replace(name, "United Kingdom", "UK") %>%
           str_replace("Czechia", "Czech Republic")) %>%
  filter(name != "Iceland") %>%
  arrange(desc(NuclearFuels)) %>%
  mutate(Nuke = cut(NuclearFuels, breaks = c(0, 5, 10, 15, 20, 25, 30, 40, 50), right = F, include.lowest = F) %>%
           as.character(),
         Nuke = ifelse(NuclearFuels == 0, "0", Nuke) %>%
           factor(levels = c("0", "[0,5)", "[5,10)", "[10,15)", "[15,20)", "[20,25)", "[25,30)", "[30,40)", "[40,50)")))

regions <- filter(world, long > -15, long < 25, lat < 70, lat > 37)

europe_nuke_map <- filter(world, region %in% regions$region) %>%
  filter(region != "Iceland") %>%
  left_join(europe_nuke, by = c("region" = "name"))

europe_nuke_labels <- filter(location, name %in% c(head(europe_nuke, 10)$name, "Spain", "Germany", "Italy", "Czechia", "Austria", "Poland")) %>%
  filter(!name %in% c("Slovenia", "Belgium")) %>%
  mutate(label_lat = ifelse(name == "Finland", label_lat - 2, label_lat),
         label_long = ifelse(name == "Finland", label_long - 2.5, label_long))

colorpal <- c("#FFFFFF", RColorBrewer::brewer.pal(name = "Paired", n = 10))[c(1:5,8:9,6:7)]
ggplot(data = europe_nuke_map) + 
  geom_polygon(aes(x = long, y = lat, fill = Nuke, group = group), color = "black") +
  coord_map(xlim = c(-16, 26), ylim = c(36, 63)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
        legend.position = c(0, 1), legend.justification = c(0, 1), legend.background = element_rect(fill = "transparent")) + 
  scale_fill_manual("% Electricity\nfrom Nuclear", values = colorpal, na.value = "grey", drop = F) + 
  geom_text(aes(x = label_long, y = label_lat, label = name), data = europe_nuke_labels, hjust = .5, vjust = .5, size = 2.75)

ggsave(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_rel_probative.png", opt), 
       width = 6, height = 6, dpi = 300)
