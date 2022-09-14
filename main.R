#Sourcing
rm(list = ls())

for (file in list.files("R", full.names = TRUE)) source(file)
pacman::p_load(bsts, tidyverse, lubridate, ggtext, glue, cli)

version <- check_updates()

modelo_itt(datapath = "data/casos_ejemplo.csv", version = paste0(version, collapse = "."))
