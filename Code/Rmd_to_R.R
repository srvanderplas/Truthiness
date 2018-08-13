library(tidyverse)

rmdfiles <- list.files(path = ".", pattern = ".Rmd", full.names = T, recursive = T)
rmdfiles <- rmdfiles[!str_detect(rmdfiles, "Truthiness|Facts")]

purl_in_folder <- function(x) {
  dir <- dirname(x)
  indicname <- str_replace_all(dir, "[\\./]", "")
  Rfile <- file.path("Single_Fact_Code", str_replace(basename(x), ".Rmd", ".R"))
  knitr::purl(x, output = Rfile)
  imgs <- list.files(dir, pattern = "png$|jpg$", full.names = T)
  imgnewnames <- file.path("Pictures_all", paste0(indicname, "-", basename(imgs)))
  file.copy(imgs, imgnewnames)
}

lapply(rmdfiles, purl_in_folder)

setwd("Single_Fact_Code")
# find ./ -type f -exec sed -i 's!\(\"\.\./\)\(.*\)\(\"\)!here::here("\2")!g' {} \;
system("find ./ -type f -exec sed -i 's!\\(\\\"\\.\\./\\)\\(.*\\)\\(\\\"\\)!here::here(\\\"\\2\\\")!g' {} \\;")
