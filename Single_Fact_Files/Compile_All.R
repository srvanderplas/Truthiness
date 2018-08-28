library(tidyverse)
library(rmarkdown)

rmdfiles <- list.files(here::here("Single_Fact_Files"), ".Rmd", recursive = T, full.names = T)

purrr::walk(rmdfiles, function(x) {
  rmarkdown::render(x, output_format = "pdf_document", output_dir = here::here("Single_Fact_Files/Final"))
})
