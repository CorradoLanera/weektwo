## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
# install.package("devtools")
# devtools::install_github("CorradoLanera/weektwo")
library(weektwo)

## ------------------------------------------------------------------------
file_path  <- system.file("sample-data", package = "weektwo")
fars_files <- list.files(file_path, "\\.bz2$")

example_fars <- file.path(file_path, fars_files[[1]])

example_fars

fars_read(example_fars)

## ------------------------------------------------------------------------
list_of_faers <- fars_read_years(c(2013, 2014), path = file_path)
list_of_faers

## ------------------------------------------------------------------------
by_index <- list_of_faers[[1]]
by_name <- list_of_faers[["2013"]]

identical(by_index, by_name)

by_name

## ------------------------------------------------------------------------
fars_summarize_years(c(2013, 2014), path = file_path)

## ------------------------------------------------------------------------
fars_map_state(
    state = 1,
    year  = 2013,
    path  = file_path
)

