# Pre-generate pilot data talk-aloud trial distribution to ensure even sample allocation

library(tidyverse)
# sample_trial_types <- function() {
#   c(c(0, 0, 1, 2, 3, 4, 5, 8, 9, 11), 
#     sample(c(1, 2, 3, 4, 5, 8, 9, 11), size = 2, replace = F)) %>%
#     sample(size = 12, replace = FALSE) %>% 
#     as.integer()
# }
# 
# sample_trial_order <- function() {
#   sample(1:12, 12)
# }
# 
# x <- NA
# y <- 1
# N_participants <- 50
# while(is.na(x) | y <= 2) {
#   trials <- purrr::map_df(1:N_participants, function(x) {
#     data_frame(participant = x, 
#                img_type = sample_trial_types(),
#                fact_id = sample_trial_order())
#   }) 
#   
#   trial_spread <- trials %>% group_by(img_type, fact_id) %>%
#     tally() %>%
#     spread(key = img_type, value = n)
#   
#   x <- sum(trial_spread[,-1], na.rm = F)
#   y <- min(trial_spread[,-1], na.rm = T)
# }

sample_trial_types <- c(0, 1, 2, 3, 4, 5, 8, 9, 11, 0, NA, NA)
sample_trial_vec <- c(sample_trial_types, sample_trial_types)

{
  trials <- purrr::map_df(36:100, function(x) {
  y <- x %% 12
  data_frame(participant = x, fact = 1:12, chart_type = sample_trial_vec[(y + 1):(y + 12)])
}) %>%
  mutate(chart_type = ifelse(is.na(chart_type), 
                             sample(c(3:5, 8, 9, 11), 
                                    size = n(), replace = T), 
                             chart_type)) 
  trials %>% 
  group_by(fact, chart_type) %>% 
  count() %>% 
  filter(n <= 5)
}  


trials <- trials %>%
  group_by(participant) %>%
  mutate(trial_order = sample(1:12, 12))

library(DBI)
wd <- setwd(here::here("Data"))
conn <- dbConnect(drv = odbc::odbc(), dbname = "truthinessStudy.db",
                  .connection_string = "Driver={SQLite3};", timeout = 10)
dbWriteTable(conn, "initial_trials", value = trials, append = F, overwrite = T)

it <- dbReadTable(conn, "initial_trials") %>%
  group_by(participant) %>%
  summarize(used = F)

itp <- dbReadTable(conn, "initial_trials_participant")
it <- filter(it, !participant %in% itp$participant)
if (nrow(it) > 0) {
  dbWriteTable(conn, "initial_trials_participant", it, append = T)
}
dbDisconnect(conn)
setwd(wd)
