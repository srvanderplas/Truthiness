library(xml2)
library(magrittr)
library(tidyverse)
library(lubridate)

epidemics <- read_html( "https://en.wikipedia.org/wiki/List_of_epidemics") %>%
  xml_nodes(".wikitable tr") %>%
  purrr::map_df(function(x) {
    y <- xml_children(x)
    
    fatalities <- xml_find_all(y[1], "span/text()") %>% xml_text()
    if (length(fatalities) == 0) {
      fatalities <- xml_text(y[1])
      fatal_sort <- xml_attr(y[1], "data-sort-value")
      fatal_sort2 <- xml_text(y[1])
      if(is.na(fatal_sort) & !is.na(fatal_sort2)) {
        fatal_sort <- fatal_sort2
      }
      fatalities <- c(fatal_sort, fatalities)
    }
    if (length(fatalities) > 0) {
      fatalities_sort <- str_replace(fatalities[1], "[[:punct:]]([\\d,]{1,}).*", "\\1") %>% parse_number()
      fatalities_txt <- rev(fatalities)[1]
    } else {
      fatalities_sort <- NA
      fatalities_txt <- NA
    }
    
    dates <- xml_find_all(y[3], "span/text()") %>% xml_text()
    if (length(dates) == 0) {
      dates <- xml_text(y[3])
      dates <- c(str_extract(dates, "^\\d{1,}"), dates)
    }
    if (length(dates) > 0) {
      dates_sort <- str_replace_all(dates[1], "\\D", "") %>% parse_number()
      dates_txt <- rev(dates)[1]
    } else {
      dates_sort <- NA
      dates_txt <- NA
    }
    
    dates_tmp <- dates_txt %>%
      str_replace("present", as.character(year(today())))
    dates_start <- dates_tmp %>%
      str_replace("(\\d{1,}).(\\d{1,}) BC", "-\\1") %>%
      str_replace("(.?\\d{1,})(.\\d{1,})?", "\\1")
    
    dates_end <- dates_tmp %>%
      str_replace("(\\d{1,})–(\\d{1,}) BC", "-\\1--\\2") %>%
      str_replace("^(-?\\d{1,})?.?(\\d*)?", "\\2")
    
    article <- xml_children(y[4]) %>% xml_attr("href")
    article <- ifelse(length(article) > 0, article[1], NA)

    
    data_frame(
      fatalities_sort = fatalities_sort,
      fatalities = fatalities_txt,
      location = xml_text(y[2]),
      date_sort = dates_sort,
      date = dates_txt,
      date_start = dates_start,
      date_end = dates_end,
      article = article,
      disease = xml_text(y[5])
    )
  }) %>%
  filter(location != "Location") %>%
  mutate(date_end = ifelse(date_end == "", date_start, date_end))
