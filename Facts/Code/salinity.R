library(xml2)
library(magrittr)
library(rvest)
library(tidyverse)
library(lubridate)

salinity <- read_html( "https://en.wikipedia.org/wiki/List_of_bodies_of_water_by_salinity") %>%
  xml_nodes(".wikitable tr") %>%
  purrr::map_df(function(x) {
    y <- xml_children(x)
    if (length(y) > 3) {
      data_frame(
        salinity = xml_text(y[1]),
        salinity_num = salinity %>%
          str_replace(" \\d{1,}$", "") %>%
          str_split("–", simplify = T) %>% 
          as.numeric() %>% mean(),
        name = xml_text(y[2]),
        type = xml_text(y[3]),
        region = xml_text(y[4])
      )
    } else {
      data_frame()
    }
  }) %>%
  filter(name != "Name") 

save(salinity, file = "Data/Salinity.Rdata")
