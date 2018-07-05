library(rvest)

url <- "http://globalenergyobservatory.org/list.php?db=PowerPlants&type=Hydro"

plants <- url %>%
  read_html() %>%
  xml_find_all("//table/tr/td/a")

read_table_info <- function(x) {
  data_frame(
    name = xml_children(x)[[1]] %>% xml_text(),
    capacity_MW = xml_children(x)[[2]] %>% xml_text(),
    country = xml_children(x)[[3]] %>% xml_text(),
    state = xml_children(x)[[4]] %>% xml_text(),
    link = xml_children(x)[[1]] %>% xml_children() %>% xml_attr("href")
  ) %>%
    mutate(
      link = ifelse(is.na(link), NA, url_absolute(link, base = "http://globalenergyobservatory.org"))
    )
}

read_plant_info <- function(link) {
  pg <- try(read_html(link))
  if ("try-error" %in% class(pg)) {
    return(data_frame())
  }
  
  data_frame(
    link = link,
    lat = xml_find_first(pg, "//input[@id='Latitude_Start']") %>%
      xml_attr("value") %>% as.numeric(),
    long = xml_find_first(pg, "//input[@id='Longitude_Start']") %>%
      xml_attr("value") %>% as.numeric(),
    status = xml_find_first(pg, "//select[@id='Status_of_Plant_enumfield_itf']/option[@selected]") %>%
      xml_attr("value"),
    design_capacity = xml_find_first(pg, "//input[@id='Design_Capacity_(MWe)_nbr']") %>%
      xml_attr("value") %>% as.numeric(),
    firm_capacity = xml_find_first(pg, "//input[@id='Firm_Capacity_(MWe)_rng1_nbr']") %>%
      xml_attr("value") %>% as.numeric(),
    river = xml_node(pg, xpath = '//*[@id="River/Lake/Canal_Dammed"]') %>% xml_attr("value"),
    year_comissioned = xml_find_first(pg, "//input[@id='Year_Project_Commissioned']") %>%
      xml_attr("value"),
    capacity_reservoir_FRL_MMcum = xml_find_first(pg, "//input[@id='Capacity_of_Main_Reservoir_(MM_cum)_rng1_at_FRL_nbr']") %>%
      xml_attr("value"),
    capacity_reservoir_useful_MMcum = xml_find_first(pg, "//input[@id='CMR_(MM_cum)_rng2_Useful/Live_nbr']") %>%
      xml_attr("value"),
    dam = xml_find_first(pg, "//input[@id='Name_of_Dam']") %>% xml_attr("value"),
    dam_height_m = xml_find_first(pg, "//input[@id='Height_of_the_Dam_(m)_nbr_rng1']") %>% xml_attr("value"),
    dam_crest_length_m = xml_find_first(pg, "//input[@id='Crest_Length_of_the_Dam_(m)_nbr']") %>% xml_attr("value"),
    dam_type = xml_find_first(pg, "//select[@id='Type_of_Dam_enumfield_rng1']/option[@selected]") %>%
      xml_text()
  )
  
}

plant_info <- url %>%
  read_html() %>%
  xml_find_all("//table") %>%
  xml_children() %>%
  map_df(read_table_info) %>%
  filter(!is.na(link))

plant_detail_info <- plant_info$link %>%
  map_df(read_plant_info)

hydro_plants <- plant_info %>% left_join(plant_detail_info)

save(hydro_plants, file =  "Data/hydro.Rdata")
