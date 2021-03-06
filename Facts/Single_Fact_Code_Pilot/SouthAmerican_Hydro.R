opt <- "SouthAmerican_Hydro"

## ----setup, include=FALSE, echo = F, warning = F, message = F------------
knitr::opts_chunk$set(echo = F, warning = F, message = F, dpi = 300)

# source(here::here("worldfactbook.R"))
load(here::here("Facts/Data/factbook.Rdata"))
load(here::here("Facts/Data/hydro.Rdata"))
library(ggthemes)
library(ggrepel)
# 
# library(rworldmap)
# library(sp)
# library(proj4)
# library(rgdal)
# library(RgoogleMaps)

library(tidyverse)
world <- map_data("world")
world2 <- map_data("world2")


## ---- out.width = "60%"--------------------------------------------------
population %>% 
  right_join(
    filter(location, simple == "South America"), by = c("name", "abbr")
  ) %>%
  group_by(name) %>%
  summarize(Pop = sum(Female + Male)) %>%
  arrange(desc(Pop)) %>%
  filter(!is.na(Pop)) %>%
  mutate(name = factor(name, levels = name)) %>%
ggplot() + 
  geom_col(aes(x = name, y = Pop/1000000, fill = name)) + 
  ggtitle("Population of South American Nations") + 
  scale_fill_brewer(guide = F, type = "qual", palette = "Paired") +
  xlab("") + 
  scale_y_continuous("Population (Millions)") + 
  coord_flip()

ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_rel_topic_unrel_nonprobative.png", opt), 
       width = 5, height = 6, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
population %>% 
  right_join(
    filter(location, simple == "Europe"), by = c("name", "abbr")
  ) %>%
  filter(name != "European Union") %>%
  group_by(name) %>%
  summarize(Pop = sum(Female + Male)) %>%
  arrange(desc(Pop)) %>%
  filter(!is.na(Pop)) %>%
  filter(row_number() <= 12) %>%
  mutate(name = factor(name, levels = name)) %>%
ggplot() + 
  geom_col(aes(x = name, y = Pop/1000000, fill = name)) + 
  ggtitle("Population of Largest European Nations") + 
  scale_fill_brewer(guide = F, type = "qual", palette = "Paired") +
  xlab("") + 
  scale_y_continuous("Population (Millions)") + 
  coord_flip()

ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_unrel_topic_unrel_nonprobative.png", opt), 
       width = 5, height = 6, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
electricity_all %>% 
  right_join(
    filter(location, simple == "South America") %>% select(name), by = c("name")
  ) %>%
  select(name, Fossil = FossilFuels, Hydroelectric = HydroelectricPlants, Nuclear = NuclearFuels, Renewables = OtherRenewableSources) %>%
  filter(!is.na(Hydroelectric)) %>%
  filter(name != "Falkland Islands (Islas Malvinas)") %>%
  filter(!name %in% c("Suriname", "Guyana")) %>%
  arrange(desc(Hydroelectric)) %>%
  mutate(Other = 0) %>%
  mutate(name = factor(name, levels = name)) %>%
  gather(key = "Type", value = value, -name) %>%
  mutate(Type = factor(Type, levels = c("Fossil", "Other", "Renewables", "Nuclear", "Hydroelectric"))) %>%
  group_by(name) %>%
  mutate(value = ifelse(Type == "Other", 100 - sum(value), value),
         value = ifelse(Type == "Other", pmax(0, value), value),
         value = value/sum(value)*100) %>%
ggplot() + 
  geom_col(aes(x = name, y = value, fill = Type), color = "black") + 
  ggtitle("Electrical Generation in South America") + 
  scale_fill_manual("Type", values = c("Fossil" = "grey30", "Hydroelectric" = "#4292c6", "Nuclear" = "#ec7014", "Renewables" = "#41ab5d", "Other" = "grey50")) + 
  xlab("") + 
  scale_y_continuous("Percent Electric Generation") + 
  theme(legend.position = "bottom")

ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_rel_topic_rel_probative.png", opt), 
       width = 6, height = 5, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
electricity_all %>% 
  right_join(
    filter(location, simple == "Europe") %>% select(name), by = c("name")
  ) %>%
  left_join(select(areas, name, total), by = "name") %>%
  filter(name != "European Union") %>%
  mutate(name = str_replace(name, "United Kingdom", "UK")) %>%
  arrange(desc(total)) %>%
  filter(row_number() <= 12) %>%
  select(name, Fossil = FossilFuels, Hydroelectric = HydroelectricPlants, Nuclear = NuclearFuels, Renewables = OtherRenewableSources) %>%
  arrange(desc(Hydroelectric)) %>%
  mutate(Other = 0) %>%
  mutate(name = factor(name, levels = name)) %>%
  gather(key = "Type", value = value, -name) %>%
  mutate(Type = factor(Type, levels = c("Fossil", "Other", "Renewables", "Nuclear", "Hydroelectric"))) %>%
  group_by(name) %>%
  mutate(value = ifelse(Type == "Other", 100 - sum(value), value),
         value = ifelse(Type == "Other", pmax(0, value), value),
         value = value/sum(value)*100) %>%
ggplot() + 
  geom_col(aes(x = name, y = value, fill = Type), color = "black") + 
  ggtitle("Electrical Generation in Large European Nations") + 
  scale_fill_manual("Type", values = c("Fossil" = "grey30", "Hydroelectric" = "#4292c6", "Nuclear" = "#ec7014", "Renewables" = "#41ab5d", "Other" = "grey50")) + 
  xlab("") + 
  scale_y_continuous("Percent Electric Generation") + 
  theme(legend.position = "bottom")

ggsave(sprintf("Facts/Pictures_all/%s-chart_subj_unrel_topic_rel_nonprobative.png", opt), 
       width = 6, height = 5, dpi = 300)

## ---- out.width = "60%", include = T, fig.width = 5, fig.height = 6------
datasubset <- population %>% 
  right_join(
    filter(location, simple == "South America"), by = c("name", "abbr")
  ) %>%
  mutate(name = str_replace(name, "South Georgia(.*)", "South Georgia") %>%
           str_replace("Falkland Islands(.*)", "Falkland Islands")) %>%
  group_by(name) %>%
  summarize(Pop = sum(Female + Male)/1e6) %>%
  arrange(Pop) %>%
  bind_rows(data_frame(name = "French Guiana", Pop = .250337))

lims <- filter(world, region %in% datasubset$name) %>%
  filter(is.na(subregion )) %>%
  summarize(long_min = min(long), long_max = max(long), lat_min = min(lat), lat_max = max(lat))

submap <- filter(world,
                 long > lims$long_min*1.01, 
                 long < lims$long_max*.98, 
                 lat > lims$lat_min*1.01, 
                 lat < lims$lat_max*1.05)

mapsubset <- filter(world, region %in% submap$region & group %in% submap$group) %>%
  left_join(datasubset, by = c("region" = "name")) %>%
  arrange(group, order)

mapsubset_avg <- submap %>%
  filter(region %in% datasubset$name) %>%
  unique() %>%
  group_by(region) %>%
  summarize(long = median(long), lat = median(lat))%>%
  mutate(region = str_replace_all(region, " ", "\n")) %>%
  filter(region %in% c("Brazil", "Bolivia", "Peru", "Colombia", "Chile", "Argentina", "Venezuela")) %>%
  mutate(long = ifelse(region == "Peru", long - 5, long),
         lat = ifelse(region == "Colombia", lat + 5, lat),
         lat = ifelse(region == "Bolivia", lat + 1, lat),
         lat = ifelse(region == "Paraguay", lat - 2, lat),
         lat = ifelse(region == "Peru", lat - 3, lat),
         long = ifelse(region == "Chile", long - 4, long))

ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Pop), color = "black", data = mapsubset) + 
  geom_label_repel(aes(x = long, y = lat, label = region), data = mapsubset_avg, point.padding = .35) +
  scale_fill_gradient("Population\n(millions)", low = "#deebf7", high = "#08306b", trans = "log10", breaks = c(.2, .5, 2, 5, 20, 50, 200), limits = c(.1, 250), na.value = "grey40") + 
  coord_map(xlim = c(lims$long_min*1.01, lims$long_max*.98), ylim = c(lims$lat_min*1.01, lims$lat_max*1.05)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), 
        legend.position = c(1, 0), legend.justification = c(1, 0), legend.direction = "vertical", legend.background = element_rect(fill = "transparent")) + 
  ggtitle("Population of South America")

ggsave(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_unrel_nonprobative.png", opt), 
       width = 4, height = 6, dpi = 300)


## ---- out.width = "60%", include = T-------------------------------------
datasubset <- population %>% 
  right_join(
    filter(location, simple == "Europe"  | name == "Russia"), by = c("name", "abbr")
  ) %>%
  filter(name != "European Union") %>%
  group_by(name) %>%
  summarize(Pop = sum(Female + Male)/1e6) %>%
  mutate(name = str_replace(name, "United Kingdom", "UK") %>%
           str_replace("Czechia", "Czech Republic")) %>%
  filter(name != "Iceland") %>%
  arrange(Pop)

lims <- filter(world, region %in% datasubset$name) %>%
  summarize(long_min = min(long), long_max = max(long), lat_min = min(lat), lat_max = max(lat))

submap <- filter(world, long > -15, long < 25, lat < 70, lat > 37)

mapsubset <- filter(world, region %in% submap$region & group %in% submap$group) %>%
  left_join(datasubset, by = c("region" = "name")) %>%
  arrange(group, order)

mapsubset_avg <- submap %>%
  filter(region %in% datasubset$name) %>%
  unique() %>%
  group_by(region) %>%
  summarize(long = median(long), lat = median(lat))%>%
  mutate(region = str_replace_all(region, " ", "\n")) %>%
  filter(region %in% c("UK", "Ireland", "France", "Spain", "Germany", "Poland", "Italy")) %>%
  mutate(lat = ifelse(region == "Spain", lat - 2, lat),
         lat = ifelse(region == "France", lat + 1, lat),
         long = ifelse(region == "UK", long + 1, long))

ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Pop), color = "black", data = mapsubset) + 
  geom_label_repel(aes(x = long, y = lat, label = region), data = mapsubset_avg) +
  scale_fill_gradient("Population\n(millions)", low = "#deebf7", high = "#08306b", trans = "sqrt", breaks = c(.5, 5, 20, 50, 100)) + 
  coord_map(xlim = c(-12, 25), ylim = c(36, 60)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title = element_blank(), axis.ticks = element_blank(), 
        legend.position = "right", legend.direction = "vertical") + 
  ggtitle("Population of Europe")

ggsave(sprintf("Facts/Pictures_all/%s-map_subj_unrel_topic_unrel_nonprobative.png", opt), 
       width = 6, height = 5, dpi = 300)

## ---- out.width = "60%", fig.width = 5, fig.height = 6-------------------
riverdata <- 

datasubset <- filter(location, simple == "South America") %>%
  mutate(name = str_replace(name, "South Georgia(.*)", "South Georgia") %>%
           str_replace("Falkland Islands(.*)", "Falkland Islands"))

lims <- filter(world, region %in% datasubset$name) %>%
  filter(is.na(subregion )) %>%
  summarize(long_min = min(long), long_max = max(long), lat_min = min(lat), lat_max = max(lat))

submap <- filter(world,
                 long > lims$long_min*1.01, 
                 long < lims$long_max*.98, 
                 lat > lims$lat_min*1.01, 
                 lat < lims$lat_max*1.05)

hydro_sub <- filter(hydro_plants, 
                    long > lims$long_min*1.01, 
                    long < lims$long_max*.98, 
                    lat > lims$lat_min*1.01, 
                    lat < lims$lat_max*1.05)

mapsubset <- filter(world, region %in% submap$region & group %in% submap$group) %>%
  arrange(group, order)

mapsubset_avg <- submap %>%
  filter(region %in% datasubset$name) %>%
  unique() %>%
  group_by(region) %>%
  summarize(long = median(long), lat = median(lat))%>%
  mutate(region = str_replace_all(region, " ", "\n"))

ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "grey50", data = mapsubset) +
  geom_point(aes(x = long, y = lat, size = capacity_MW), color = "blue", alpha = .5, data = hydro_sub, shape = 1) + 
  coord_map(xlim = c(lims$long_min*1.01, lims$long_max*.98), ylim = c(lims$lat_min*1.01, lims$lat_max*1.05)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), 
        legend.position = c(1, 0), legend.justification = c(1, 0), legend.direction = "vertical", legend.background = element_rect(fill = "white")) + 
  scale_size_continuous("Capacity(MW)", breaks = c(10, 100, 1000), trans = "log10", range = c(.1, 3)) + 
  ggtitle("Hydroelectric power plants")

ggsave(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_rel_nonprobative.png", opt), 
       width = 4, height = 6, dpi = 300)


## ---- out.width = "60%"--------------------------------------------------
# datasubset <- filter(location, simple == "Europe" | name == "Russia") %>%
#   mutate(name = str_replace(name, "United Kingdom", "UK") %>%
#            str_replace("Czechia", "Czech Republic"))
# 
# lims <- filter(world, region %in% datasubset$name) %>%
#   filter(is.na(subregion )) %>%
#   summarize(long_min = min(long), long_max = max(long), lat_min = min(lat), lat_max = max(lat))
# 
# submap <- filter(world, long > -15, long < 25, lat < 70, lat > 37)
# 
# mapsubset <- filter(world, region %in% submap$region & group %in% submap$group) %>%
#   arrange(group, order)
# 
# mapsubset_avg <- submap %>%
#   filter(region %in% datasubset$name) %>%
#   unique() %>%
#   group_by(region) %>%
#   summarize(long = median(long), lat = median(lat))%>%
#   mutate(region = str_replace_all(region, " ", "\n"))
# 
# hydro_sub <- filter(hydro_plants, long > -12, long < 25, lat > 36, lat < 60)
# 
# ggplot() + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "grey50", data = mapsubset) + 
#   geom_point(aes(x = long, y = lat, size = capacity_MW), color = "blue", alpha = .5, data = hydro_sub, shape = 1) + 
#   coord_map(xlim = c(-12, 25), ylim = c(36, 60)) +
#   theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) + 
#   scale_size_continuous("Capacity(MW)") + 
#   ggtitle("Hydroelectric power plants")
datasubset <- electricity_all %>% 
  right_join(
    filter(location, simple == "Europe" | name == "Russia") %>% select(name), by = c("name")
  ) %>%
  filter(name != "European Union") %>%
  select(name, Hydroelectric = HydroelectricPlants) %>%
  arrange(desc(Hydroelectric)) %>%
  mutate(name = str_replace(name, "United Kingdom", "UK") %>%
           str_replace("Czechia", "Czech Republic")) %>%
  filter(name != "Iceland") %>%
  mutate(name = factor(name, levels = name))

lims <- filter(world, region %in% datasubset$name) %>%
  summarize(long_min = min(long), long_max = max(long), lat_min = min(lat), lat_max = max(lat))

submap <- filter(world, long > -15, long < 25, lat < 70, lat > 37)

mapsubset <- filter(world, region %in% submap$region & group %in% submap$group) %>%
  left_join(datasubset, by = c("region" = "name")) %>%
  arrange(group, order) %>%
  mutate(hydro = cut(Hydroelectric, breaks = c(0, 20, 40, 60, 80, 100)))

mapsubset_avg <- submap %>%
  filter(region %in% datasubset$name) %>%
  unique() %>%
  group_by(region) %>%
  summarize(long = median(long), lat = median(lat))%>%
  mutate(region = str_replace_all(region, " ", "\n")) %>%
  filter(region %in% c("UK", "Ireland", "France", "Spain", "Germany", "Poland", "Italy")) %>%
  mutate(lat = ifelse(region == "Spain", lat - 2, lat),
         lat = ifelse(region == "France", lat + 1, lat),
         long = ifelse(region == "UK", long + 1, long))

ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = hydro), color = "black", data = mapsubset) + 
  coord_map(xlim = c(-12, 25), ylim = c(36, 60)) +
  scale_fill_brewer("% Hydro\nPower", palette = "YlGnBu", na.value = "grey50") + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), 
        legend.position = c(0, .4), legend.justification = c(0, .4), legend.direction = "vertical", legend.background = element_rect(fill = "white")) + 
  ggtitle("Hydroelectric Power Generation in Europe")

ggsave(sprintf("Facts/Pictures_all/%s-map_subj_unrel_topic_rel_nonprobative.png", opt), 
       width = 5, height = 5, dpi = 300)


## ---- out.width = "60%", message = F, warning = F, fig.width = 5, fig.height = 6----
datasubset <- electricity_all %>% 
  right_join(
    filter(location, simple == "South America") %>% select(name), by = c("name")
  ) %>%
  select(name, Hydroelectric = HydroelectricPlants) %>%
  arrange(desc(Hydroelectric)) %>%
  mutate(name = factor(name, levels = name)) %>%
  mutate(name = str_replace(name, "South Georgia(.*)", "South Georgia") %>%
           str_replace("Falkland Islands(.*)", "Falkland Islands"))

lims <- filter(world, region %in% datasubset$name) %>%
  filter(is.na(subregion )) %>%
  summarize(long_min = min(long), long_max = max(long), lat_min = min(lat), lat_max = max(lat))

submap <- filter(world,
                 long > lims$long_min*1.01, 
                 long < lims$long_max*.98, 
                 lat > lims$lat_min*1.01, 
                 lat < lims$lat_max*1.05)

mapsubset <- filter(world, region %in% submap$region & group %in% submap$group) %>%
  arrange(group, order) %>%
  left_join(datasubset, by = c("region" = "name")) %>%
  mutate(hydro = cut(Hydroelectric, breaks = c(0, 25, 37.5, 50, 62.5, 75, 100)))

mapsubset_avg <- submap %>%
  filter(region %in% datasubset$name) %>%
  unique() %>%
  group_by(region) %>%
  summarize(long = median(long), lat = median(lat))%>%
  mutate(region = str_replace_all(region, " ", "\n"))

ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = hydro), color = "black", data = mapsubset) + 
  coord_map(xlim = c(lims$long_min*1.01, lims$long_max*.98), ylim = c(lims$lat_min*1.01, lims$lat_max*1.05)) +
  scale_fill_brewer("% Hydro\nPower", palette = "YlGnBu", na.value = "grey50") + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), 
        legend.position = c(1, 0), legend.justification = c(1, 0), legend.direction = "vertical", legend.background = element_rect(fill = "transparent")) + 
  ggtitle("Hydroelectric Power Generation\nin South America")


ggsave(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_rel_probative.png", opt), 
       width = 4, height = 6, dpi = 300)