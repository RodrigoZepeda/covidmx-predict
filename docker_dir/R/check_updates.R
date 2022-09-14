#' Función para verificar actualizaciones al modelo
#' 
#' @description La función se conecta a 
#' [RodrigoZepeda/covidmx-predict](https://github.com/RodrigoZepeda/covidmx-predict) y
#' verifica si se ha actualizado el modelo en cuyo caso descarga la versión más nueva. 
#' 
#' @export

check_updates <- function(local_version = "version.txt", update_model = TRUE){
  
  #Get local version
  version_local <- readLines(local_version) |>
    gsub("v","", x = _) |>
    strsplit(split = "\\.") |>
    unlist() |>
    as.numeric()
  
  #Get updated version
  url         <- "https://raw.githubusercontent.com/RodrigoZepeda/covidmx-predict/main/version.txt"
  
  version_url <- tryCatch({
    RCurl::getURL(url) |> 
      gsub("v","", x = _) |>
      strsplit(split = "\\.") |>
      unlist() |>
      as.numeric()
    }, error = function(e){
      cli::cli_alert_danger(
        "No hay conexión a Internet. Usando modelo local v{paste0(version_local, collapse = '.')}."
      )
      NULL
    }, warning = function(e){
      cli::cli_alert_danger(
        c("Tuve un problema para descargar actualizaciones. ", 
          "Usando modelo local v{paste0(version_local, collapse = '.')}.")
      )
      NULL
    })
  
  message_danger <- c("La versión en línea es v{paste0(version_url, collapse = '.')} ", 
                      "la cual es menor que la local ",
                      "v{paste0(version_local, collapse = '.')}. Usaré la versión local.")
  
  message_warn   <- c("La versión en línea es v{paste0(version_url, collapse = '.')} ", 
                      "la cual es mayor que la local ",
                      "v{paste0(version_local, collapse = '.')}. Intentaré actualizar...")
  
  
  version <- version_local
  
  #Chequeo de versiones-----
  if (!is.null(version_url) && version_url[1] == version_local[1]){
    
    if (version_url[2] == version_local[2]){
      cli::cli_alert_success("Estás usando la versión más actual.") 
      version <- version_local
    } else if (version_url[2] > version_local[2]){
      cli::cli_alert_warning(message_warn);
      version <- get_updates(version_local, version_url, update_model)
    } else {
      cli::cli_alert_danger(message_danger)
      version <- version_local
    }
    
  } else if (!is.null(version_url) && version_url[1] < version_local[1]) {
    cli::cli_alert_danger(message_danger)
    version <- version_local
    
  } else if (!is.null(version_url) && version_url[1] > version_local[1]) {
    cli::cli_alert_warning(message_warn)
    version <- get_updates(version_local, version_url, update_model)
    
  } 
  
  return(version)
}

#' @rdname check_updates
#' @export
get_updates <- function(version_local, version_url, update_model = TRUE){
  
  version <- version_local
  
  if (update_model){
    m_url   <- "https://raw.githubusercontent.com/RodrigoZepeda/covidmx-predict/main/R/modelo.R"
    
    donefile <- tryCatch({
      
      #Download
      gfile <- RCurl::getURL(m_url)
      
      
      #Write
      fileConn <- file("R/modelo_temp.R")
      writeLines(gfile, fileConn)
      close(fileConn)
      
      TRUE
      
    }, error = function(e){
      cli::cli_alert_danger("No se pudo descargar el modelo Usando default.")
      FALSE
    }, warning = function(e){
      cli::cli_alert_danger("No se pudo actualizar el modelo Usando default.")
      FALSE
    })
    
    actualizacion <- FALSE; #Bandera para saber si se actualizó la info
    if (donefile){
      
      #Intento de renombrar. Respaldamos versión anterior
      copia <- file.copy(from = "R/modelo.R", to = "R/old_modelo_temp.R", 
                         overwrite = TRUE)
      
      if (copia){
        #Si existe el respaldo reescribimos el nuevo modelo
        actualizacion <- file.copy(from = "R/modelo_temp.R", to = "R/modelo.R", 
                                   overwrite = TRUE)
        version <- version_url
        cli::cli_alert_success("Modelo actualizado a la version v{paste0(version, collapse = '.')}")
      } else {
        cli::cli_alert_danger("No pude actualizar. Usaré la version anterior: v{paste0(version, collapse = '.')}")
      }
      
      #Double check to delete previous iteration
      if (actualizacion & copia){
        file.remove(c("R/old_modelo_temp.R","R/modelo_temp.R"))
      }
    }
  } else {
    cli::cli_alert_warning(
      "Descargas desactivadas. Usaré la versión local del modelo: v{paste0(version, collapse = '.')}"
    )
  }
  
  return(version)
}
