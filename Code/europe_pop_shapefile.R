library(sf)

euro_sf <- st_read("Data/Version 2_0_1/GEOSTATReferenceGrid/Grid_ETRS89_LAEA_1K-ref_GEOSTAT_POP_2011_V2_0_1.shp")

euro_pop <- read_csv("Data/Version 2_0_1/JRC-GHSL_AIT-grid-POP_1K_2011.csv")

euro_sf <- left_join(euro_sf, euro_pop)

# https://cran.r-project.org/web/packages/sf/vignettes/sf2.html