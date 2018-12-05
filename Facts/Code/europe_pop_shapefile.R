library(sf)

euro_sf <- st_read(here::here("Data/Version 2_0_1/GEOSTATReferenceGrid/Grid_ETRS89_LAEA_1K-ref_GEOSTAT_POP_2011_V2_0_1.shp"))

euro_pop <- read_csv(here::here("Data/Version 2_0_1/GEOSTAT_grid_POP_1K_2011_V2_0_1.csv"))

euro_sf <- left_join(euro_sf, euro_pop)
st_crs(euro_sf$geometry) <- 3035
euro_sf <- euro_sf %>%
  mutate(geometry = st_transform(geometry, "+proj=longlat"))

belgium <- filter(euro_sf, CNTR_CODE == "BE")
italy <- filter(euro_sf, CNTR_CODE == "IT")

save(belgium, italy, file = here::here("Data/BelgiumItalyPopShapes.Rdata"))
save(belgium, file = here::here("Data/BelgiumPopShapes.Rdata"))
# https://cran.r-project.org/web/packages/sf/vignettes/sf2.html

# ggplot(data = belgium) + 
#   geom_sf(aes(fill = TOT_P))

