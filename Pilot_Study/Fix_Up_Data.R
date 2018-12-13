
# library(tidyverse)
# library(here) # working directory simplicity
# library(DBI) # Databases
# library(pool) # Shiny pooled connections

# file.copy(from = file.path(here("Pilot_Study/Data"), 
#                            "Driver={SQLite3};dbname=truthinessStudy.db"),
#           to = file.path(here("./Pilot_Study")), overwrite = T)
# con <- dbConnect(odbc::odbc(), dbname = "truthinessStudy.db", 
#                  .connection_string = "Driver={SQLite3};", 
#                  timeout = 10)
# tables <- dbListTables(con)
# on.exit(dbDisconnect(con))

if ("trials" %in% tables) {
  trials <- tbl(con, "trials") %>% collect()
  db_drop_table(con, "trials")
}

trial <- tbl(con, "trial") %>% 
  filter(startTime >= "2018-11-12 09:00:00") %>% 
  collect() 

dbWriteTable(con, "trial", trial, overwrite = T)

user <- tbl(con, "user") %>%
  filter(userID %in% trial$userID) %>%
  collect()

user <- user %>% 
  group_by(userID) %>%
  summarize(
    browserFP = unique(browserFP),
    userIP = unique(userIP),
    age = unique(age),
    education = unique(education),
    study = paste(study, collapse = ", "),
    colorblind = unique(colorblind),
    consent = unique(consent)
  )
dbWriteTable(con, "user", user, overwrite = T)
