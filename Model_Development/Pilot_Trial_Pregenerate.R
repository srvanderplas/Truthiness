# Pre-generate pilot data talk-aloud trial distribution to ensure even sample allocation

library(tidyverse)
sample_trial_types <- function() {
  c(c(0, 0, 1, 2, 3, 4, 5, 8, 9, 11), 
    sample(c(1, 2, 3, 4, 5, 8, 9, 11), size = 2, replace = F)) %>%
    sample(size = 12, replace = FALSE) %>% 
    as.integer()
}

sample_trial_order <- function() {
  sample(1:12, 12)
}

x <- NA
y <- 1
N_participants <- 50
while(is.na(x) | y <= 2) {
  trials <- purrr::map_df(1:N_participants, function(x) {
    data_frame(participant = x, 
               img_type = sample_trial_types(),
               fact_id = sample_trial_order())
  }) 
  
  trial_spread <- trials %>% group_by(img_type, fact_id) %>%
    tally() %>%
    spread(key = img_type, value = n)
  
  x <- sum(trial_spread[,-1], na.rm = F)
  y <- min(trial_spread[,-1], na.rm = T)
}
