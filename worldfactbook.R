library(rvest)
library(tidyverse)

pg <- read_html("https://www.cia.gov/library/publications/the-world-factbook/fields/2010.html#ar")

cts <- xml_nodes(pg, "#fieldListing") %>%
  xml_children() %>%
  xml_children()

population <- data_frame(
  abbr = xml_attr(cts, "id")[-1],
  name = xml_find_first(cts, "td")[-1] %>%
    xml_text(),
  facts = xml_find_all(cts, "td[@class='fieldData']") %>%
    xml_text() %>%
    str_replace_all("\\n", " ") %>%
    str_trim()
  )

vars <- expand.grid(var = c("Age", "Pct", "Male", "Female"), age = c("0.14", "14.24", "25.54", "55.64", "65")) %>%
  mutate(varname = paste(var, age, sep = ""))

population <- population %>%
  extract(facts, into = vars$varname, 
          regex = "(\\d{1,}-\\d{1,}) years.*?: ([\\.\\d]{1,}). \\(male ([\\d,]{1,})/female ([\\d,]{1,})\\)(\\d{1,}-\\d{1,}) years.*?: ([\\.\\d]{1,}). \\(male ([\\d,]{1,})/female ([\\d,]{1,})\\)(\\d{1,}-\\d{1,}) years.*?: ([\\.\\d]{1,}). \\(male ([\\d,]{1,})/female ([\\d,]{1,})\\)(\\d{1,}-\\d{1,}) years.*?: ([\\.\\d]{1,}). \\(male ([\\d,]{1,})/female ([\\d,]{1,})\\)(\\d{1,}) years and over: ([\\.\\d]{1,}). \\(male ([\\d,]{1,})/female ([\\d,]{1,})\\)") %>%
  mutate_at(.vars = vars(matches("Male|Female")), parse_number) %>%
  select(-matches("Age")) %>%
  gather(key = var, value = value, -abbr, -name) %>%
  mutate(
    age = str_extract(var, "\\d{1,}(\\.\\d{1,})?"), 
    var = str_replace(var, age, "")
  ) %>%
  spread(key = var, value = value) %>%
  mutate(age = factor(age, levels = c("0.14", "14.24", "25.54", "55.64", "65"), 
                      labels = c("0-14", "15-24", "25-54", "55-64", "65+"), ordered = T))


#function definition
html_text_collapse <- function(x, trim = FALSE, collapse = "\n"){
  UseMethod("html_text_collapse")
}

html_text_collapse.xml_nodeset <- function(x, trim = FALSE, collapse = "\n"){
  vapply(x, html_text_collapse.xml_node, character(1), trim = trim, collapse = collapse)
}

html_text_collapse.xml_node <- function(x, trim = FALSE, collapse = "\n"){
  paste(xml2::xml_find_all(x, ".//text()"), collapse = collapse)
}


pg <- read_html("https://www.cia.gov/library/publications/the-world-factbook/fields/2147.html")
cts <- xml_nodes(pg, "#fieldListing") %>%
  xml_children() %>%
  xml_children()

areas <- data_frame(
  abbr = xml_attr(cts, "id")[-1],
  name = xml_find_first(cts, "td")[-1] %>%
    xml_text(),
  facts = xml_find_all(cts, "td[@class='fieldData']") %>%
    lapply(., function(x) {
      z <- xml_contents(x) %>%
        as.character() %>%
        paste(collapse = " ") %>%
        str_split(pattern = " <br> ?", simplify = T) %>%
        # str_replace_all("\\n", " ") %>%
        str_trim()
      z[nchar(z) > 0]
    })
) %>%
  unnest() %>%
  extract(facts, into = c("key", "value"), "<strong>(.*?): </strong> (.*)", remove = F) %>%
  mutate(value = str_replace_all(value, c(" ?\\(.*\\)" = "", ";.*$" = "", " sq km ?" = ""))) %>%
  filter(key %in% c("total", "land", "water")) %>%
  mutate(million = grepl("million", value),
         value = str_replace(value, " million|less than|ca\\. ", "") %>%
           str_replace("negligible", "0")) %>%
  mutate(value = parse_number(value),
         value = ifelse(million, value*1000000, value)) %>%
  select(-million, -facts) %>%
  spread(key = key, value = value)

pg <- read_html("https://www.cia.gov/library/publications/the-world-factbook/fields/2060.html")
cts <- xml_nodes(pg, "#fieldListing") %>%
  xml_children() %>%
  xml_children()

coast <- data_frame(
  abbr = xml_attr(cts, "id")[-1],
  name = xml_find_first(cts, "td")[-1] %>%
    xml_text(),
  facts = xml_find_all(cts, "td[@class='fieldData']") %>%
    lapply(., function(x) {
      z <- xml_contents(x) %>%
        as.character() %>%
        paste(collapse = " ") %>%
        str_split(pattern = " <br> ?", simplify = T) %>%
        # str_replace_all("\\n", " ") %>%
        str_trim()
      z[nchar(z) > 0]
    })
) %>%
  unnest() %>%
  filter(!str_detect(facts, "^.strong.note:")) %>%
  mutate(coast = str_replace(facts, "(.*?)([\\d\\.,]{1,})(.*)", "\\2")) %>%
  mutate(coast = parse_number(coast)) %>%
  select(-facts) %>%
  group_by(name, abbr) %>%
  summarize(coast = sum(coast, na.rm = T))


pg <- read_html("https://www.cia.gov/library/publications/the-world-factbook/fields/2096.html")

cts <- xml_nodes(pg, "#fieldListing") %>%
  xml_children() %>%
  xml_children()

na_fill <- function(x) {
  if (sum(!is.na(x)) == 0) {
    NA
  } else if (sum(!is.na(x)) == 1) {
    x[!is.na(x)]
  } else {
    warning("multiple not-NA values.")
    if (is.numeric(x)) {
      sum(x, na.rm = T)
    } else {
      x[!is.na(x)][1]
    }
  }
}

borders <- data_frame(
  abbr = xml_attr(cts, "id")[-1],
  name = xml_find_first(cts, "td")[-1] %>%
    xml_text(),
  facts = xml_find_all(cts, "td[@class='fieldData']") %>%
    lapply(., function(x) {
      z <- xml_contents(x) %>%
        as.character() %>%
        paste(collapse = " ") %>%
        str_split(pattern = " <br> ?", simplify = T) %>%
        # str_replace_all("\\n", " ") %>%
        str_trim()
      z[nchar(z) > 0]
    })
) %>%
  unnest() %>%
  extract(facts, into = c("key", "n", "value"), "<strong>(.*?)\\(?(\\d{1,})?\\)?: </strong> (.*)", remove = F) %>%
  mutate(key = ifelse(facts == "0 km", "total", key),
         value = ifelse(facts == "0 km", "0 km", value)) %>%
  mutate(name = ifelse(grepl(" - total", key), str_extract(key, "^([A-z ]{1,})"), name),
         key = str_replace(key, name, "") %>% str_replace(" ?- ", ""),
         name = ifelse(name == "France", lag(name, 1), name),
         name = str_replace(name, "^metropolitan ", "")) %>%
  select(-facts) %>%
  filter(!is.na(key) & !is.na(value)) %>%
  group_by(abbr, name) %>%
  mutate(n = as.numeric(n), 
         n = na_fill(n), 
         key = str_trim(key) %>% str_replace(".*border.*", "border")) %>%
  group_by(abbr, name, key) %>%
  summarize(value = paste(value, collapse = ", "), n = unique(n)) %>%
  spread(key = key, value = value) %>%
  mutate(border = str_split(border, ", ")) %>%
  unnest() %>%
  extract(border, into = c("bordercountry", "borderlength"), "(.*?) ([\\d,\\.]{1,}) km") %>%
  mutate(total = str_replace(total, " km", "") %>% parse_number(),
         borderlength = parse_number(borderlength),
         n = ifelse(is.na(n), 1, n)) %>%
  mutate(bordercountry = str_replace(bordercountry, " \\(.*\\)", "")) %>%
  group_by(abbr, name, bordercountry) %>%
  summarize(borderlength = sum(borderlength, na.rm = T), n = unique(n), total = unique(total)) %>%
  ungroup() %>%
  filter(name != "World" & name != "Antarctica")

borders <- borders %>%
  rename(country = bordercountry, length = borderlength, land.border = total, ncountries = n) %>%
  full_join(coast) %>%
  nest(-abbr, -name, -land.border, -coast)

rm(cts, pg, na_fill, vars)

library(ggplot2)
borders %>%
  unnest() %>%
  select(name, ncountries, land.border) %>% 
  unique() %>%
  filter(land.border > 0) %>%
ggplot(data = .) + 
  geom_jitter(aes(x = land.border, y = ncountries)) + 
  scale_x_log10("Border Length (km)", breaks = c(10, 100, 1000, 5000, 20000)) + 
  scale_y_continuous("Number of bordering countries and regions")

library(ggrepel)
borders %>%
  filter(!grepl("Ocean", name)) %>%
  filter(name != "European Union") %>%
  filter(land.border > 0 & coast > 0) %>%
  mutate(ratio = land.border/(land.border + coast)) %>%
  mutate(label = land.border < 10 | coast < 10 | coast > 50000 | land.border > 20000) %>%
ggplot(data = .) + 
  geom_jitter(aes(x = land.border, y = coast)) + 
  geom_label_repel(aes(x = land.border, y = coast, label = ifelse(label, name, NA))) + 
  scale_x_log10("Land Border Length (km)", breaks = c(10, 100, 1000, 5000, 20000, 100000)) + 
  scale_y_log10("Coast Length (km)", breaks = c(10, 100, 1000, 5000, 20000, 100000)) + 
  theme(axis.text.y = element_text(angle = 90, hjust = .5, vjust = .5))



