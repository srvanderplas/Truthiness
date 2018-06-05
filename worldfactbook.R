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



pg <- read_html("https://www.cia.gov/library/publications/the-world-factbook/fields/2096.html#uv")

cts <- xml_nodes(pg, "#fieldListing") %>%
  xml_children() %>%
  xml_children()

borders <- data_frame(
  xml_nodes()
)