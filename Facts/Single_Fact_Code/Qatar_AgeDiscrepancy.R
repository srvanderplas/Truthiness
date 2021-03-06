## ----setup, include=FALSE, echo = F, warning = F, message = F------------
opt <- "Qatar_AgeDiscrepancy"
knitr::opts_chunk$set(echo = F, warning = F, message = F, dpi = 300)

# source(here::here("worldfactbook.R"))
load(here::here("Facts/Data/factbook.Rdata"))

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

world <- readOGR(here::here("Facts/Data/countries.geo.json"), "countries.geo", stringsAsFactors=FALSE)
world_data <- data_frame(
  name = as.character(world@data$name),
  id = rownames(world@data)
)

world_map <- fortify(world) %>%
  left_join(world_data)
rm(world_data)
world <- map_data("world")
world2 <- map_data("world2")

load(here::here("Facts/Data/JapanQatarImports.Rdata"))
export_col_theme <- sample(scales::hue_pal()(15), size = 15)

## ----echo = FALSE, out.width="60%"---------------------------------------
tmp  <- file.copy(
  here::here(paste0("Facts/Single_Fact_Files/", opt, "/", "picture_subject_related.jpg")),
  here::here(sprintf("Facts/Pictures_all/%s-picture_subject_related.jpg", opt)), 
  overwrite = T)
knitr::include_graphics("picture_subject_related.jpg")

## ----echo = FALSE, out.width="60%"---------------------------------------
tmp  <- file.copy(
  here::here(paste0("Facts/Single_Fact_Files/", opt, "/", "picture_subject_unrelated.jpg")),
  here::here(sprintf("Facts/Pictures_all/%s-picture_subject_unrelated.jpg", opt)), 
  overwrite = T)
knitr::include_graphics("picture_subject_unrelated.jpg")

## ---- out.width = "60%"--------------------------------------------------
ggplot(data = qatar_imports) + 
  geom_col(aes(x = ProductCategory, y = TotalTradeMil/1000, fill = ProductCategory)) + 
  ggtitle("Qatar's Imports (2016)") + 
  scale_fill_manual(guide = F, values = export_col_theme) +
  xlab("") + 
  scale_y_continuous("Value (Billions, USD)") + 
  coord_flip()
ggsave(here::here(sprintf("Facts/Pictures_all/%s-chart_subj_rel_topic_unrel_nonprobative.png", opt)), 
       width = 4, height = 6, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
ggplot(data = japan_imports) + 
  geom_col(aes(x = ProductCategory, y = TotalTradeMil/1000, fill = ProductCategory)) + 
  ggtitle("Japan's Imports (2017)") + 
  scale_fill_manual(guide = F, values = export_col_theme) +
  xlab("") + 
  scale_y_continuous("Value (Billions, USD)") + 
  coord_flip()
ggsave(here::here(sprintf("Facts/Pictures_all/%s-chart_subj_unrel_topic_unrel_nonprobative.png", opt)), 
       width = 4, height = 6, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
population %>%
  mutate(Female = as.numeric(Female), Male = as.numeric(Male), ratio = Female/Male) %>%
  arrange(ratio) %>%
  filter(name %in% c("Qatar", "Oman", "Yemen", "Saudi Arabia", "United Arab Emirates")) %>%
  gather(key = Gender, value = value, -abbr, -name, -age, -Pct, -ratio) %>%
  group_by(name) %>%
  mutate(value = value/sum(value)*100) %>%
  ggplot() + 
  geom_bar(aes(x = age, y = value, fill = Gender), stat = "identity", position = "dodge") + 
  scale_fill_manual("Gender", values = c("Female" = "#B2182B", "Male" = "#2166AC")) + 
  facet_wrap(~name) + 
  ylab("% Population") + 
  xlab("Age Group") + 
  ggtitle("Population Demographics - Lower Arabian Peninsula") + 
  theme(legend.position = c(1, 0), legend.justification = c(1, 0), legend.background = element_rect(fill = "transparent"))
ggsave(here::here(sprintf("Facts/Pictures_all/%s-chart_subj_rel_topic_rel_probative.png", opt)), 
       width = 6, height = 4, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
population %>%
  mutate(Female = as.numeric(Female), Male = as.numeric(Male), ratio = Female/Male) %>%
  arrange(ratio) %>%
  filter(name %in% c("Japan", "Korea, North", "Korea, South", "China", "Russia")) %>%
  mutate(name = str_replace(name, "Korea, (.*)", "\\1 Korea")) %>%
  gather(key = Gender, value = value, -abbr, -name, -age, -Pct, -ratio) %>%
  group_by(name) %>%
  mutate(value = value/sum(value)*100) %>%
  ggplot() + 
  geom_bar(aes(x = age, y = value, fill = Gender), stat = "identity", position = "dodge") + 
  scale_fill_manual("Gender", values = c("Female" = "#B2182B", "Male" = "#2166AC")) + 
  facet_wrap(~name) + 
  ylab("% Population") + 
  xlab("Age Group") + 
  ggtitle("Population Demographics - Eastern Asia") + 
  theme(legend.position = c(1, 0), legend.justification = c(1, 0), legend.background = element_rect(fill = "transparent"))
ggsave(here::here(sprintf("Facts/Pictures_all/%s-chart_subj_unrel_topic_rel_nonprobative.png", opt)), 
       width = 6, height = 4, dpi = 300)


## ---- out.width = "60%"--------------------------------------------------
regionlist <- c("Qatar", "Oman", "Yemen", "Saudi Arabia", "United Arab Emirates")
lims <- filter(world2, region %in% regionlist) %>%
  summarize(long_min = min(long), long_max = max(long), lat_min = min(lat), lat_max = max(lat))

submap <- filter(world2, 
                 long > lims$long_min*.98, 
                 long < lims$long_max*1.01, 
                 lat > lims$lat_min*.98, 
                 lat < lims$lat_max*1.01)

datasubset <- electricity_all %>%
  filter(name %in% submap$region) %>%
  select(region = name, total_elec)

mapsubset <- filter(world2, region %in% submap$region & group %in% submap$group) %>%
  left_join(datasubset, by = "region")

mapsubset_avg <- submap %>%
  filter(region %in% regionlist) %>%
  unique() %>%
  group_by(region) %>%
  summarize(long = median(long), lat = median(lat))%>%
  mutate(region = str_replace_all(region, " ", "\n")) 

ggplot(data = arrange(mapsubset, group, order)) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = total_elec), color = "black") + 
  geom_label_repel(aes(x = long, y = lat, label = region), data = mapsubset_avg, point.padding = .35) + 
  scale_fill_gradient("% Population\nwith Electricity", low = "#F7fcf5", high = "#00441b", limits = c(0, 100)) + 
  coord_map(xlim = c(lims$long_min, lims$long_max) * c(.98, 1.01),
            ylim = c(lims$lat_min, lims$lat_max) * c(.98, 1.01)) + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), 
        legend.position = c(1, 1), legend.justification = c(1, 1), legend.direction = "horizontal", legend.background = element_rect(fill = "white")) + 
  ggtitle("Electrification in the Middle East")
rm(lims, submap, mapsubset)
ggsave(here::here(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_unrel_nonprobative.png", opt)), 
       width = 6, height = 5, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
country <- "Japan"
lims <- filter(world2, region == country) %>%
  summarize(long_min = min(long), long_max = max(long), lat_min = min(lat), lat_max = max(lat))

submap <- filter(world2, 
                 long > lims$long_min*.9, 
                 long < lims$long_max*1.001, 
                 lat > lims$lat_min*.9, 
                 lat < lims$lat_max*1.001)

datasubset <- electricity_all %>%
  mutate(region = str_replace(name, "Korea, (.*)", "\\1 Korea")) %>%
  filter(region %in% c("Russia", "China", "North Korea", "South Korea", "Japan", "Taiwan", "Mongolia")) %>%
  select(region, total_elec) %>%
  mutate(total_elec = ifelse(region == "Taiwan", 100, total_elec))

mapsubset <- filter(world2, region %in% submap$region & group %in% submap$group) %>% 
  left_join(datasubset, by = "region")

mapsubset_avg <- submap %>%
  unique() %>%
  group_by(region) %>%
  summarize(long = median(long), lat = median(lat)) %>%
  mutate(long = ifelse(region == "Mongolia", long - 3, long),
         long = ifelse(region == "Taiwan", long + 1.5, long),
         long = ifelse(region == "North Korea", long + 2, long),
         long = ifelse(region == "South Korea", long - 2, long),
         long = ifelse(region == "Russia", long + 2, long),
         lat = ifelse(region == "China", lat + 5, lat))

ggplot(data = arrange(mapsubset, group, order)) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = total_elec), color = "black") + 
  geom_label(aes(x = long, y = lat, label = region), data = mapsubset_avg) + 
  scale_fill_gradient("% Population\nwith Electricity", low = "#F7fcf5", high = "#00441b", limits = c(0, 100)) + 
  coord_map(xlim = c(lims$long_min, lims$long_max) * c(.85, 1.01),
            ylim = c(lims$lat_min, lims$lat_max) * c(.85, 1.01)) + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
        legend.position = c(1, 0), legend.justification = c(1, 0), legend.background = element_rect(fill = "transparent")) + 
  ggtitle("Electrification in East Asia")
ggsave(here::here(sprintf("Facts/Pictures_all/%s-map_subj_unrel_topic_unrel_nonprobative.png", opt)), 
       width = 6, height = 5, dpi = 300)

rm(lims, submap, mapsubset)

## ---- out.width = "60%"--------------------------------------------------
regionlist <- c("Qatar", "Oman", "Yemen", "Saudi Arabia", "United Arab Emirates")
lims <- filter(world2, region %in% regionlist) %>%
  summarize(long_min = min(long), long_max = max(long), lat_min = min(lat), lat_max = max(lat))

submap <- filter(world2, 
                 long > lims$long_min*.98, 
                 long < lims$long_max*1.01, 
                 lat > lims$lat_min*.98, 
                 lat < lims$lat_max*1.01)

datasubset <- population %>%
  filter(name %in% submap$region, age == "25-54") %>%
  select(region = name, Pct2554 = Pct) %>%
  mutate(Pct2554 = as.numeric(Pct2554))

mapsubset <- filter(world2, region %in% submap$region & group %in% submap$group) %>%
  left_join(datasubset, by = "region") %>%
  arrange(group, order)

mapsubset_avg <- submap %>%
  filter(region %in% regionlist) %>%
  unique() %>%
  group_by(region) %>%
  summarize(long = median(long), lat = median(lat))%>%
  mutate(region = str_replace_all(region, " ", "\n")) 

ggplot(data = arrange(mapsubset, group, order)) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Pct2554), color = "black") + 
  geom_label_repel(aes(x = long, y = lat, label = region), data = mapsubset_avg, point.padding = .35) + 
  scale_fill_gradient("% Population\n25-54", low = "#F7fcf5", high = "#00441b") +
  coord_map(xlim = c(lims$long_min, lims$long_max) * c(.98, 1.01),
            ylim = c(lims$lat_min, lims$lat_max) * c(.98, 1.01)) + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), 
        legend.position = c(1, 1), legend.justification = c(1, 1), legend.direction = "horizontal", legend.background = element_rect(fill = "white")) + 
  ggtitle("Middle East Population, % Age 25-54")

ggsave(here::here(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_rel_nonprobative.png", opt)), 
       width = 6, height = 5, dpi = 300)

## ---- out.width = "60%"--------------------------------------------------
country <- "Japan"
lims <- filter(world2, region == country) %>%
  summarize(long_min = min(long), long_max = max(long), lat_min = min(lat), lat_max = max(lat))

submap <- filter(world2, 
                 long > lims$long_min*.9, 
                 long < lims$long_max*1.001, 
                 lat > lims$lat_min*.9, 
                 lat < lims$lat_max*1.001)

datasubset <- population %>%
  mutate(region = str_replace(name, "Korea, (.*)", "\\1 Korea")) %>%
  filter(region %in% c("Russia", "China", "North Korea", "South Korea", "Japan", "Taiwan", "Mongolia")) %>%
  mutate(Female = as.numeric(Female), Male = as.numeric(Male), ratio = Female/Male) %>%
  filter(age %in% c("15-24", "25-54")) %>%
  select(region, Male, Female) %>%
  group_by(region) %>%
  summarize(ratio = sum(Male)/sum(Male + Female))

mapsubset <- filter(world2, region %in% submap$region & group %in% submap$group) %>%
  left_join(datasubset, by = "region") %>%
  arrange(group, order)

mapsubset_avg <- submap %>%
  unique() %>%
  group_by(region) %>%
  summarize(long = median(long), lat = median(lat)) %>%
  mutate(long = ifelse(region == "Mongolia", long - 3, long),
         long = ifelse(region == "Taiwan", long + 1.5, long),
         long = ifelse(region == "North Korea", long + 2, long),
         long = ifelse(region == "South Korea", long - 2, long),
         long = ifelse(region == "Russia", long + 2, long),
         lat = ifelse(region == "China", lat + 5, lat))

ggplot(data = arrange(mapsubset, group, order)) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = ratio), color = "black") + 
  geom_label(aes(x = long, y = lat, label = region), data = mapsubset_avg) + 
  scale_fill_gradient2("Male Proportion\nof population,\n15-54", low = "#b2182b", mid = "#FFFFFF", midpoint = .5, high = "#2166ac", limits = c(.45, .55)) + 
  coord_map(xlim = c(lims$long_min, lims$long_max) * c(.85, 1.01),
            ylim = c(lims$lat_min, lims$lat_max) * c(.85, 1.01)) + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
        legend.position = c(1, 0), legend.justification = c(1, 0), legend.background = element_rect(fill = "transparent")) + 
  ggtitle("Working-Age Population Gender Balance")

ggsave(here::here(sprintf("Facts/Pictures_all/%s-map_subj_unrel_topic_rel_nonprobative.png", opt)), 
       width = 6, height = 5, dpi = 300)

# rm(lims, submap, mapsubset)


## ---- out.width = "60%"--------------------------------------------------
regionlist <- c("Qatar", "Oman", "Yemen", "Saudi Arabia", "United Arab Emirates")
lims <- filter(world2, region %in% regionlist) %>%
  summarize(long_min = min(long), long_max = max(long), lat_min = min(lat), lat_max = max(lat))

submap <- filter(world2, 
                 long > lims$long_min*.98, 
                 long < lims$long_max*1.01, 
                 lat > lims$lat_min*.98, 
                 lat < lims$lat_max*1.01)

datasubset <- population %>%
  mutate(region = name) %>%
  filter(region %in% submap$region) %>%
  mutate(Female = as.numeric(Female), Male = as.numeric(Male), ratio = Female/Male) %>%
  filter(age %in% c("15-24", "25-54")) %>%
  select(region, Male, Female) %>%
  group_by(region) %>%
  summarize(ratio = sum(Male)/sum(Male + Female))

mapsubset <- filter(world2, region %in% submap$region & group %in% submap$group) %>%
  left_join(datasubset, by = "region") %>%
  arrange(group, order)

mapsubset_avg <- submap %>%
  filter(region %in% regionlist) %>%
  unique() %>%
  group_by(region) %>%
  summarize(long = median(long), lat = median(lat))%>%
  mutate(region = str_replace_all(region, " ", "\n"))

ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = ratio), color = "black", data = mapsubset) + 
  geom_label_repel(aes(x = long, y = lat, label = region), data = mapsubset_avg, point.padding = .35) +
  scale_fill_gradient2("Male Proportion\nof population,\n15-54", low = "#b2182b", mid = "#FFFFFF", midpoint = .5, high = "#2166ac", limits = c(.1, .9)) + 
  coord_map(xlim = c(lims$long_min, lims$long_max) * c(.98, 1.01),
            ylim = c(lims$lat_min, lims$lat_max) * c(.98, 1.01)) + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), 
        legend.position = c(1, 1), legend.justification = c(1, 1), legend.direction = "horizontal", legend.background = element_rect(fill = "white")) + 
  ggtitle("Working-Age Population Gender Balance")

ggsave(here::here(sprintf("Facts/Pictures_all/%s-map_subj_rel_topic_rel_probative.png", opt)), 
       width = 5, height = 5, dpi = 300)
rm(lims, submap, mapsubset)


