library(rgdal)
library(tidyverse)
library(sp)
library(sf)

worldborders <- st_read("Data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
waterborders <- st_read("Data/World_Seas_IHO_v2/World_Seas_IHO_v2.shp")

BlackSea <- filter(waterborders, NAME == "Black Sea") %>%
  select(Name = NAME, area = Sph_Area, geometry)
RedSea <- filter(waterborders, str_detect(NAME, "Red Sea")) %>%
  select(Name = NAME, area = Sph_Area, geometry)

bs_boundaries <- worldborders %>%
  mutate(NAME = as.character(NAME)) %>%
  split(.$NAME) %>%
  map_df(function(x) data_frame(name = x$NAME, length = st_intersection(x, st_boundary(BlackSea)) %>% st_length()))

rs_boundaries <- worldborders %>%
  mutate(NAME = as.character(NAME)) %>%
  split(.$NAME) %>%
  map_df(function(x) data_frame(name = x$NAME, length = st_intersection(x, st_boundary(RedSea)) %>% st_length()))

save(bs_boundaries, rs_boundaries, file = "Data/SeaBorders.Rdata")
