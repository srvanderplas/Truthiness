library(tidyverse)
library(rmarkdown)
library(furrr)

rmdfiles <- list.files(here::here("Facts/Single_Fact_Files"), ".Rmd", recursive = T, full.names = T)

compile_doc <- function(x) {
  try(rmarkdown::render(x, output_format = "pdf_document", output_dir = here::here("Facts/Single_Fact_Files/Final")))
}

plan(multiprocess, workers = 12)
res <- furrr::future_map(rmdfiles, compile_doc)

re_do <- which(sapply(res, class) != "character")

if (length(re_do) > 0) {
  res[re_do] <- furrr::future_map(rmdfiles[re_do], compile_doc)
}

# Create one giant pdf
list.files(here::here("Facts/Single_Fact_Files/Final"), "*.pdf", full.names = T) %>%
  staplr::staple_pdf(input_files = ., output_filepath = here::here("Facts/Single_Fact_Files/All_Facts.pdf"))

# Run as system command
system("ps2pdf /home/srvander/Projects/CSAFE/Truthiness/Facts/Single_Fact_Files/All_Facts.pdf /home/srvander/Projects/CSAFE/Truthiness/Facts/Single_Fact_Files/All_Facts_Small.pdf")
