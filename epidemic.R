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
      fatalities_tmp <- str_replace_all(fatalities, "[\\d\\.-]*\\% of the population", "")
      fatalities_tmp <- str_replace_all(fatalities_tmp, ",(\\d{3})", "\\1")
      fatalities_tmp <- str_replace_all(fatalities_tmp, "[\\(\\[].*[\\)\\]]", "")
      
      fatalities_sort <- str_replace(fatalities_tmp[1], "(?:[\\d,\\.]{1,})?[[:punct:]]([\\d,\\.]{1,}).*?", "\\1") %>% parse_number()
      fatalities_txt <- rev(fatalities)[1]
    } else {
      fatalities_sort <- NA
      fatalities_txt <- NA
    }
    
    if (str_detect(fatalities_txt, "million")) {
      fatalities_sort <- fatalities_sort * 1000000
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
    
    if (str_detect(dates_tmp, "\\d{2,4}s")) {
      z <- as.numeric(str_extract(dates_tmp, "\\d{2,4}"))
      
      dates_tmp <- paste0(z, "-", z+10)
    }
    
    dates_start <- dates_tmp %>%
      str_replace("(\\d{1,}).(\\d{1,}) BC", "-\\1") %>%
      str_replace("(.?\\d{1,})(.\\d{1,})?", "\\1") %>%
      as.numeric()
    
    dates_end <- dates_tmp %>%
      str_replace("(\\d{1,})–(\\d{1,}) BC", "-\\1--\\2") %>%
      str_replace("^(-?\\d{1,})?.?(\\d*)?", "\\2") %>%
      as.numeric()
    
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
  mutate(date_end = ifelse(is.na(date_end), date_start, date_end),
         date_end = ifelse(date_end - date_start > 100, date_end - 100, date_end)) %>%
  mutate(cause = disease %>%
           str_replace("(.*) possibly (.*)", "\\2") %>%
           str_replace("(.*) similar to (.*)", "\\2") %>%
           str_replace("[Bb]ubonic ", "") %>%
           str_replace("^(.*?)[Ii]nfluenza(.*)$", "influenza") %>%
           str_replace("epidemic ", "") %>%
           str_replace_all("[[:punct:][:space:]]{1,}", " ") %>%
           str_replace(" Spanish Flu Virus", "") %>%
           str_replace(" virus", "") %>%
           str_replace("Ebola(.*)", "Ebola") %>%
           # str_replace("^(.*)([Uu]nknown|ambiguous)(.*)$", "unknown") %>%
           str_replace("(.*)[Hh]epatitis(.*)$", "hepatitis") %>%
           str_replace("(.*)plague(.*)", "plague") %>%
           str_replace("(unknown cause)|(fever ambiguous)|(relapsing fever)", "unknown") %>%
           str_trim() %>%
           str_to_upper()
  ) %>%
  group_by(cause) %>%
  add_tally() %>%
  ungroup() %>%
  mutate(cause = ifelse(n > 5, cause, "OTHER")) %>%
  select(-n) %>%
  group_by(cause) %>%
  mutate(dt_group = floor(pmax(date_start, date_end)/pmax(5, max(date_end - date_start)))) %>%
  ungroup() %>%
  group_by(dt_group, cause) %>%
  mutate(idx = 0:(n()-1) * pmin(1/5, .75/n())) %>%
  ungroup()
