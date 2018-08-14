library(tidyverse)
library(DBI)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQLite3};", 
                 timeout = 10)
tables <- dbListTables(con)

# ---- Prep table of pictures --------------------------------------------------
fact <- c(
  "Egypt has approximately equal land and coastal borders",
  "Brazil is the 5th largest country",
  "Paraguay is landlocked",
  "Russia has the longest coastal border of any country",
  "Eswatini (Swaziland) only borders one country",
  "Seven countries border the black sea",
  "Qatar has the largest discrepancy between the proportion of working-age men and women compared to neighboring countries",
  "Three South American countries get more than 50% of their electricity from hydroelectric generation",
  "Over 95% of Belgium’s population lives in Urban areas",
  "Less than 85 percent of the population of Belize has access to electricity",
  "Switzerland is one of three countries who get more than 30% of their power from nuclear power plants",
  "South Africa has the lowest proportion of the population under 15 of all African nations"
)

factCode <- c(
  "Egypt_Borders", "Brazil_Size", "Paraguay_Landlocked", 
  "Russia_Coast", "Swaziland_Border", "Black_sea", 
  "Qatar_AgeDiscrepancy", "SouthAmerican_Hydro", "Belgium_Urban",
  "Belize_Electricity", "Switzerland_Nuclear", "SouthAfrica_Population")

factAnswer <- c(T, T, T, F, F, F, T, T, T, F, F, F)

filepath <- list.files(here::here("Pictures_all/"), full.names = T)

trialTypeNum <- 0:11
trialType <- c("fact_alone", 
               "picture_subject_related", 
               "picture_subject_unrelated",
               "chart_subj_rel_topic_unrel_nonprobative", 
               "chart_subj_unrel_topic_unrel_nonprobative",
               "chart_subj_rel_topic_rel_probative",
               "chart_subj_unrel_topic_rel_nonprobative",
               "map_subj_rel_topic_unrel_nonprobative",
               "map_subj_unrel_topic_unrel_nonprobative",
               "map_subj_rel_topic_rel_nonprobative",
               "map_subj_unrel_topic_rel_nonprobative",
               "map_subj_rel_topic_rel_probative"
)

facts <- data_frame(factID = 1:length(fact), fact = fact, factCode = factCode, 
                    correctAnswer = factAnswer)
trials <- data_frame(trialTypeID = trialTypeNum, trialType = trialType)

picture_db_table <- merge(facts, trials) %>%
  mutate(path = ifelse(trialType == "fact_alone", NA, 
                       paste0(factCode, "-", trialType))) %>%
  arrange(path) %>%
  mutate(imgfilename = c(filepath, rep(NA, 12))) %>%
  mutate(match = str_replace(basename(imgfilename), "\\.png$|\\.jpg$", "") %>%
           all.equal(., path)) %>%
  arrange(factID, trialTypeID) %>%
  mutate(picID = row_number())

if (sum(!picture_db_table$match) > 0) {
  warning("Not all questions match!")
}

picture_db_table <- picture_db_table %>%
  select(picID, factID, fact, factCode, correctAnswer, trialTypeID, trialType, path)
# ------------------------------------------------------------------------------

# ---- Options for user demographics -------------------------------------------

age <- c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+") %>%
  factor(x = ., levels = ., ordered = T)
education <- c("Some High School", "High School Degree (or equivalent)", 
               "Some College", "Associate degree", "Bachelor's Degree",
               "Master's Degree", "Professional Degree", "Doctorate") %>%
  factor(x = ., levels = ., ordered = T)
study <- c("Humanities", "Social Sciences", "Natural Sciences", 
           "Mathematics and Computer Science", "Engineering",
           "Medicine and Health", "Other") %>%
  factor(x = ., levels = .)
colorblind <- c("I do not have impaired color vision", 
                "I don't know whether I have impaired color vision", 
                "I have impaired color vision") %>%
  factor(x = ., levels = ., ordered = T)

useroptions <- list(age = age, education = education, study = study, 
                    colorblind = colorblind)

# ------------------------------------------------------------------------------

if (!"picture" %in% tables) {
  dbWriteTable(con, 'pictures', picture_db_table, overwrite = T)
}

if (!"trial" %in% tables) {
  sampleTrial <- data_frame(
    trialID = 1:3,
    picID = 1:3,
    userID = 1:3,
    answer = TRUE,
    startTime = Sys.time() - lubridate::seconds(25),
    submitTime = Sys.time()
  )
  dbCreateTable(con, 'trial', sampleTrial)
}


if (!"user" %in% tables) {
  sampleUser <- data_frame(
    userID = 1:10,
    browserFP = "",
    age = sample(useroptions$age, 10, replace = T),
    education = sample(useroptions$education, 10, replace = T),
    study = sample(useroptions$study, 10, replace = T),
    colorblind = sample(useroptions$colorblind, 10, replace = T, prob = c(.9, .05, .05))
  ) %>%
    mutate(browserFP = map_chr(paste0(userID, age, education, study, colorblind), 
                               digest::digest))
  dbCreateTable(con, 'user', sampleUser)
}
