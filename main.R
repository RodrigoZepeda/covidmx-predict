#!/usr/bin/env Rscript
pacman::p_load(bsts, tidyverse, lubridate, ggtext, glue, cli)

args <- commandArgs(trailingOnly = TRUE)
if (length(args)==0) {
  cli::cli_abort("Call has no args")
} else {
  file_casos      <- args[1]
  file_encoding   <- args[2]
  days_to_predict <- as.numeric(args[3])
  tipo            <- args[4]
}

if (args[5] == "auto"){
  auto_update <- TRUE
} else {
  auto_update <- FALSE
}

for (file in list.files("R", full.names = TRUE)) source(file)

#Check updates
version <- check_updates(update_model = auto_update)

#Run model
modelo_itt(datapath        = glue::glue("data/{file_casos}"),
           encoding        = file_encoding,
           days_to_predict = days_to_predict, tipo = tipo,
           version         = paste0(version, collapse = "."))
