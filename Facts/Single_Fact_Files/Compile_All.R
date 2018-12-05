library(tidyverse)
library(rmarkdown)
library(furrr)

rmdfiles <- list.files(here::here("Single_Fact_Files"), ".Rmd", recursive = T, full.names = T)

plan(multiprocess, workers = 12)
furrr::future_map(rmdfiles, function(x) {
  rmarkdown::render(x, output_format = "pdf_document", output_dir = here::here("Single_Fact_Files/Final"))
})
