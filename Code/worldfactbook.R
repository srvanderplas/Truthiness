library(rvest)
library(measurements)
library(magrittr)
library(tidyverse)

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
  tidyr::extract(facts, into = vars$varname, 
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
  tidyr::extract(facts, into = c("key", "value"), "<strong>(.*?): </strong> (.*)", remove = F) %>%
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
  tidyr::extract(facts, into = c("key", "n", "value"), "<strong>(.*?)\\(?(\\d{1,})?\\)?: </strong> (.*)", remove = F) %>%
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
  tidyr::extract(border, into = c("bordercountry", "borderlength"), "(.*?) ([\\d,\\.]{1,}) km") %>%
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

rm(cts, pg, vars)


pgs <- paste0("https://www.cia.gov/library/publications/the-world-factbook/fields/", 2237:2240, ".html")
pg <- purrr::map(pgs, read_html)

cts <- purrr::map(pg, function(x) {
  xml_nodes(x, "#fieldListing") %>%
  xml_children() %>%
  xml_children()
})

cts2 <- purrr::map(pg, function(x) {
  xml_nodes(x, ".fieldHeading th:nth-of-type(2)") %>%
    xml_text() %>%
    str_replace("ELECTRICITY - FROM (.*?)\\(.*\\)$", "\\1")
})

pg3 <- read_html("https://www.cia.gov/library/publications/the-world-factbook/fields/2268.html")
cts3 <- pg3 %>%
    xml_nodes("#fieldListing") %>%
      xml_children() %>% 
      xml_children() %>%
      (function(x) {
        data_frame(
          abbr = xml_attr(x, "id")[-1],
          name = xml_find_first(x, "td")[-1] %>%
            xml_text(),
          facts = xml_find_all(x, "td[@class='fieldData']") %>%
            xml_text()
        ) %>%
          tidyr::extract(facts, keep = T, into = c("pop_no_elec", "total_elec", "urban_elec", "rural_elec"), 
                  regex = "\\s{0,}(?:population without electricity: ([\\d,]{1,}))?\\s?(?:electrification - total population: ([\\d,\\.]{1,})%)?\\s?(?:electrification - urban areas: ([\\d,\\.]{1,})%)?\\s?(?:electrification - rural areas: ([\\d,\\.]{1,})%)?\\s?(?:\\(\\d{4}\\))?") %>%
          mutate_at(.vars = vars(pop_no_elec, total_elec, urban_elec, rural_elec), 
                    .funs = parse_number)
        
      }) %>%
  mutate(
    pop_no_elec = ifelse(total_elec == 100, 0, pop_no_elec),
    urban_elec = ifelse(total_elec == 100, 100, urban_elec),
    rural_elec = ifelse(total_elec == 100, 100, rural_elec)
  )


electricity <- cts %>%
  purrr::map(
    .f = function(x) {
      data_frame(
        abbr = xml_attr(x, "id")[-1],
        name = xml_find_first(x, "td")[-1] %>%
          xml_text(),
        value = xml_find_all(x, "td[@class='fieldData']") %>%
          xml_text() %>%
          str_replace_all(c("(?:\\n){0,}([\\d\\.]{1,})% of total installed capacity ?\\(.*\\)\\s?(note:.*)?$" = "\\1")) %>%
          parse_number()
      )
    }
  ) 

electricity_all <- map2_df(cts2, electricity, .f = function(x, y) {
  y$Type = x
  y
})  %>%
  mutate(Type = str_to_title(Type) %>% str_replace_all(" ", "")) %>%
  spread(Type, value) %>%
  left_join(cts3)

rm(cts, cts2, cts3, pg, pg3, pgs, electricity)

location <- "https://www.cia.gov/library/publications/the-world-factbook/fields/2144.html" %>%
  read_html() %>%
  xml_nodes("#fieldListing") %>%
  xml_children() %>% 
  xml_children() %>%
  map_df(.f = function(x) {
    data_frame(
      abbr = xml_attr(x, "id"),
      name = xml_find_first(x, "td") %>%
        xml_text(),
      desc = xml_find_all(x, "td[@class='fieldData']/text()") %>%
        xml_text() %>%
        str_replace_all("\\n", "") %>%
        str_trim() %>%
        (function(xx) {
          nc <- nchar(xx) > 0
          if (sum(nc) > 0) return(xx[nc][1])
          return("")
        }),
      simple = str_replace(desc, "^(.*?)(?:\\(.*\\))?[;,](.*)$", "\\1") %>%
        str_replace(".* in the (.*)", "\\1") %>%
        str_replace(".*?((?:South America)|(?:Central America)|(?:North America)|Europe|Antarctica?|(?:Southern Ocean)|(?:Atlantic Ocean)|(?:Pacific Ocean)|(?:Indian Ocean)|(?:Middle East)|Asia|Africa|Oceania|Mediterranean).*", "\\1") %>%
        str_replace("Antarctic$", "Southern Ocean") %>%
        str_replace("west and Russia", "Europe") %>%
        str_replace("Middle America", "Pacific Ocean") %>%
        str_trim()
    ) 
  }) %>%
  filter(!str_detect(desc, "body of water")) %>%
  filter(!is.na(abbr))

conv_unit_dir <- function(x) {
  sgn <- ifelse(str_detect(x, "[SW]"), -1, 1)
  str_replace(x, " [NSEW]$", "") %>%
    conv_unit("deg_dec_min", "dec_deg") %>%
    as.numeric() %>%
    multiply_by(sgn)
}

location_center <- "https://www.cia.gov/library/publications/the-world-factbook/fields/2011.html" %>%
  read_html() %>%
  xml_nodes("#fieldListing") %>%
  xml_children() %>% 
  xml_children() %>%
  map_df(.f = function(x) {
    data_frame(
      abbr = xml_attr(x, "id"),
      name = xml_find_first(x, "td") %>%
        xml_text(),
      loc = xml_find_all(x, "td[@class='fieldData']/text()") %>%
        xml_text() %>%
        str_replace_all("\\n", "") %>%
        str_trim() %>%
        (function(xx) {
          nc <- nchar(xx) > 0
          if (sum(nc) > 0) return(xx[nc][1])
          return("")
        }),
      label_lat = str_replace(loc, "^((?:\\d{1,} ){2}[NSEW]{1}), ((?:\\d{1,} ){2}[NSEW]{1})(.*?)$", "\\1") %>% conv_unit_dir,
      label_long = str_replace(loc, "^((?:\\d{1,} ){2}[NSEW]{1}), ((?:\\d{1,} ){2}[NSEW]{1})(.*?)$", "\\2") %>% conv_unit_dir
    ) 
  }) %>%
  filter(!is.na(abbr))

urbanization <- "https://www.cia.gov/library/publications/the-world-factbook/fields/2212.html" %>%
  read_html() %>%
  xml_nodes("#fieldListing") %>%
  xml_children() %>% 
  xml_children() %>%
  map_df(.f = function(x) {
    data_frame(
      abbr = xml_attr(x, "id"),
      name = xml_find_first(x, "td") %>%
        xml_text(),
      urbanPct = xml_find_all(x, "td[@class='fieldData']/text()") %>%
        xml_text() %>%
        str_replace_all("\\n", "") %>%
        str_trim() %>%
        (function(xx) {
          nc <- nchar(xx) > 0
          if (sum(nc) > 0) return(xx[nc][1])
          return("")
        }) %>%
        str_replace("% of total population.*$", "") %>%
        parse_number()
    ) 
  }) %>%
  filter(!is.na(abbr))

location <- left_join(location, select(location_center, -loc)) %>%
  left_join(urbanization)

rm(location_center)

remove_empty_str <- function(x) {
  x[nchar(x) != 0]
}

# roads <- "https://www.cia.gov/library/publications/the-world-factbook/fields/2085.html" %>%
#   read_html() %>%
#   xml_nodes("#fieldListing") %>%
#   xml_children() %>%
#   xml_children() %>%
#   map_df(.f = function(x) {
#     data_frame(
#       abbr = xml_attr(x, "id"),
#       name = xml_find_first(x, "td") %>%
#         xml_text(),
#       key = xml_find_all(x, "td[@class='fieldData']/strong/text()") %>%
#         xml_text() %>%
#         str_replace_all("\\n", "") %>%
#         str_trim(),
#       value = xml_find_all(x, "td[@class='fieldData']/node()[not(self::strong or self::br)]") %>%
#         xml_text() %>%
#         str_replace_all("\\n", "") %>%
#         str_trim() %>%
#         remove_empty_str()
#     )
#   }) %>%
#   filter(key != "note:") %>%
#   mutate(value = str_replace(value, "([\\d,\\.]{1,}) ?\\(?.*\\)?", "\\1") %>% parse_number,
#          key = str_replace_all(key, c("^paved(.*):$" = "paved/urban",
#                                       "private and forest roads:" = "unpaved/rural",
#                                       "unpaved(.*)" = "unpaved/rural",
#                                       "non-urban:" =  "unpaved/rural",
#                                       "^urban:" = "paved/urban",
#                                       "highways:" = "paved/urban",
#                                       "total:" = "total",
#                                       "urban roads:" = "paved/urban"))) %>%
#   group_by(key, name, abbr) %>%
#   summarize(value = sum(value)) %>%
#   filter(key %in% c("total", "paved/urban", "unpaved/rural")) %>%
#   spread(key = key, value = value)
  

religion <- "https://www.cia.gov/library/publications/the-world-factbook/fields/2122.html" %>%
  read_html() %>%
  xml_nodes("#fieldListing") %>%
  xml_children() %>%
  xml_children() %>%
  map_df(.f = function(x) {
    data_frame(
      abbr = xml_attr(x, "id"),
      name = xml_find_first(x, "td") %>%
        xml_text(),
      value = xml_find_all(x, "td[@class='fieldData']/node()[not(self::strong or self::br)]") %>%
        xml_text() %>%
        str_replace_all("\\n", "") %>%
        str_trim() %>%
        remove_empty_str()
    )
  }) %>%
  group_by(name) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(short = str_replace_all(value, " \\(.*?\\)", "")) %>%
  mutate(denom = str_split(short, ", ")) %>%
  unnest() %>%
  mutate(pct = str_extract(denom, "[\\.\\d]{1,}") %>% as.numeric(),
         denom_name = str_extract(denom, "^[A-z '-]{1,}") %>%
           str_trim()) %>%
  filter(!is.na(pct)) %>%
  mutate(denom_name = denom_name %>%
           str_replace_all(c("nominally " = "", 
                             "more than" = "",
                             "none(.*)" = "none",
                             "(not religious or atheist)|(not stated or unidentified)" = "none",
                             "no (religion|response)" = "none",
                             "objected to answering" = "none",
                             "^unknown|undeclared|unspecified|unaff?ill?iated$" = "none",
                             "^Orthodox$" = "Orthodox Christian", 
                             "other( and unspecified)?( and none)?( religion)?" = "other",
                             "other Catholic" = "Catholic",
                             "other Christian" = "Christian", 
                             "other (non|or none|or unspecified)" = "other",
                             "none or unknown" = "none",
                             "(more than )?one religion" = "other",
                             "other-Christian" = "Protestant",
                             "other Protestant" = "Protestant",
                             "other Evangelical Churches" = "Protestant",
                             "(.*)(traditional|customary)(.*)" = "Traditional",
                             "Seventh$" = "Seventh Day Adventist",
                             "(.*)folk religion(.*)" =  "folk religion",
                             "Roman Catholic" = "Catholic", 
                             "Christianity" = "Christian", 
                             "Evangelical( and Pentecostal)?( or Protestant)?( Reformist)?" = "Protestant",
                             "Hindus" = "Hindu",
                             "[Aa]nimist" = "Animist",
                             "atheist (and|or) agnostic" = "Atheist",
                             "atheist|agnostic" = "Atheist",
                             "Buddhism" = "Buddhist",
                             "don't know" = "none",
                             "Kimbanguiste" = "Kimbanguist",
                             "non-believers?" = "none",
                             "non-Catholic Christians" = "Protestant",
                             "Protestant and .*" = "Protestant",
                             "Protestant Reformed" = "Reformed",
                             "nondenominational" = "Protestant",
                             "Seventh-Day" = "Seventh Day",
                             "Witnesses" = "Witness",
                             "Iglesia ni Kristo" = "Church of Christ",
                             "Calvinist" = "Reformed",
                             "others" = "other",
                             "Vodoun|vodou" = "Vodou",
                             "indigenous(.*)" = "Traditional",
                             "other" = "Other",
                             "none" = "None")) %>%
           str_trim()) %>%
  select(abbr, name, denom = denom_name, pct)


ethnicity <- "https://www.cia.gov/library/publications/the-world-factbook/fields/2075.html" %>%
  read_html() %>%
  xml_nodes("#fieldListing") %>%
  xml_children() %>%
  xml_children() %>%
  map_df(.f = function(x) {
    data_frame(
      abbr = xml_attr(x, "id"),
      name = xml_find_first(x, "td") %>%
        xml_text(),
      value = xml_find_all(x, "td[@class='fieldData']/node()[not(self::strong or self::br)]") %>%
        xml_text() %>%
        str_replace_all("\\n", "") %>%
        str_trim() %>%
        remove_empty_str()
    )
  }) %>%
  group_by(name) %>%
  filter(row_number() == 1) %>%
  filter(str_detect(value, "%")) %>%
  ungroup() %>%
  mutate(short = str_replace_all(value, c("\\(.*?\\(.*?\\).*?\\)" = "",
                                          "\\(.*?\\)" = "",
                                          "(.*?), .*?:" = "",
                                          "^([A-z ]*):" = "",
                                          " at least" = "", 
                                          "less than " = "",
                                          "more than " = "",
                                          "predominantly " = "",
                                          "population: " = "",
                                          " and related" = "",
                                          ",(\\d{3})" = "\\1",
                                          "((over)|(more than)) \\d{2,}(.*) ethnic groups" = "",
                                          "; \\d{1,}%[A-z ]{1,},[A-z ]{1,}$" = ""))) %>%
  mutate(group = str_split(short, ", ")) %>%
  unnest() %>%
  mutate(pct = str_extract(group, "[\\.\\d]{1,}%") %>% str_replace("%", ""),
         group_name = str_replace(group, pct, "") %>%
           str_replace("%", "") %>%
           str_trim()) %>%
  filter(!is.na(pct)) %>%
  mutate(group_name = str_replace_all(group_name, 
                                      c("mainland - " = "",
                                        "(local|metropolitan) " = "",
                                        "mestizo|mestico" = "mixed",
                                        "mulatto(.*)" = "mixed",
                                        "(other/unavailable|other/unspecified|none|not declared|undeclared|unknown|no answer|unspecified|unidentified|not stated)" = "other",
                                        "other (and|or) other" = "other",
                                        "other/other" = "other",
                                        "other" = "Other"
                                        )),
         pct = as.numeric(pct)) %>%
  group_by(name, group_name) %>%
  summarize(pct = sum(pct)) %>%
  ungroup() %>%
  select(name, ethnicity = group_name, pct = pct)


save(areas, borders, electricity_all, location, religion, population, ethnicity, file = "Data/factbook.Rdata")
