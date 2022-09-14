#' @title Procesamiento de los datos para modelo de incidencia
#' 
#' 
#' @details 
#' * Ver version.txt para saber qué versión se está trabajando. 
#' * El archivo Modelo.md contiene la descripción matemática del mismo
#' @param datapath Dirección a un archivo `.csv` como `casos_ejemplo.csv`con la siguiente estructura (ver `casos_ejemplo.csv`)
#'
#'| Semana epidemiológica   | Año epidemiológico | Total de casos | Entidad |
#'  |-----------------------|--------------------|----------------|---------|
#'  |         1             |         2020       |       12       |  AGS    |
#'  |         1             |         2020       |       84       |  BC    |
#'  |         2             |         2020       |       0        |  AGS    |
#'  |         2             |         2020       |       5        |  BC    |
#'  
#' @note Semanas donde no se observaron casos deben contener un cero.
#' @author Rodrigo Zepeda-Tello <rodrigo.zepeda€imss.gob.mx>  

data_preprocess <- function(datapath, encoding = "UTF-8"){
  
  #Read the dataset
  data_as_is <- readr::read_csv(datapath, locale = locale(encoding = encoding),
                                col_types = cols(
                                  .default = col_integer(),
                                  Entidad  = col_character(),
                                  Fecha    = col_date()
                                ))
  
  #Check columns
  if (ncol(data_as_is) != 3){
    cli::cli_abort("La base de datos debe estar conformada por 3 columnas en el siguiente orden:
                    {.strong `Fecha`, `Total de casos`, `Entidad`}")
  }
  
  #Check data specification
  if (!stringr::str_detect(colnames(data_as_is)[1], "\\b[Ff]echa\\b")){
    cli::cli_abort("La primer columna de los datos debe llamarse {.strong `Fecha`}")
  }
  
  if (!stringr::str_detect(colnames(data_as_is)[2], "\\b[Tt]otal de [Cc]asos\\b")){
    cli::cli_abort("La segunda columna de los datos debe llamarse {.strong `Total de casos`}")
  }
  
  if (!stringr::str_detect(colnames(data_as_is)[3], "\\b[Ee]ntidad\\b")){
    cli::cli_abort("La tercer columna de los datos debe llamarse {.strong `Entidad`}")
  }
  
  #Names just in case
  colnames(data_as_is) <- c("Fecha", "Total de casos", "Entidad")
  
  return(data_as_is)
  
}

