rm(list = ls())

library(covidmx) #remotes::install_github("RodrigoZepeda/covidmx")
library(tidyverse)
library(lubridate)

#Elaboración de datos de ejemplo para el modelo
datos_covid <- descarga_datos_abiertos()
datos_covid <- datos_covid |> casos()
datos_covid$casos <- datos_covid$casos |>
  rename(Entidad = ENTIDAD_FEDERATIVA) |>
  rename(`Total de casos` = n) |>
  mutate(Fecha = as.Date(FECHA_SINTOMAS)) |>
  select(`Fecha`, `Total de casos`, Entidad)

datos_covid$disconnect()

datos_covid$casos |>
  filter(`Fecha` < max(`Fecha`) - weeks(4)) |>
  write_excel_csv("data/casos_ejemplo.csv")
