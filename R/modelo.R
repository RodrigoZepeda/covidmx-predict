#!/usr/bin/env r
#'
#' @title Modelo de incidencia de COVID-19
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
#' Por ahora no hay otro tipo más que COVID
modelo_itt <- function(datapath, encoding = "UTF-8", days_to_predict = 180, tipo = "covid",
                       version = "0.0.0"){

  #Get current date
  tudei <- lubrodate::today() #jejeje . This is to add the same date to everything if run too late

  data_model <- data_preprocess(datapath = datapath, encoding = "UTF-8")
  cli::cli_alert_info("Procesando datos de {.file {datapath}}")

  colores_gob <- c("#1e4236", "#c8a66a","#651d31", "#98989A")

  NStates  <- length(unique(data_model$Entidad))
  NTime    <- length(unique(data_model$Fecha))

  casos_model <- data_model |>
    pivot_wider(names_from = Entidad, values_from = `Total de casos`, values_fill = 0) |>
    arrange(Fecha)

  estados     <- colnames(casos_model)[2:ncol(casos_model)]

  myfolder    <- glue::glue("covid_itt_{tudei}")
  if (dir.exists(myfolder)){
    unlink(myfolder, recursive = T, force = T)
  }
  dir.create(myfolder)

  cli::cli_progress_bar("Calculando entidad", total = NStates)
  for (col in estados){

    cli::cli_progress_message(col)

    #Obtenemos el tibble de casos
    estado_cases <- casos_model[, c("Fecha", col)]
    colnames(estado_cases) <- c("Fecha", "Casos")

    #Normalización
    mu  <- mean(estado_cases$Casos)
    sdy <- sd(estado_cases$Casos)
    estado_cases$log_casos <- (sqrt(estado_cases$Casos) - mu)/sdy

    #Creamos el objeto bsts
    model_components <- list()
    model_components <- model_components |>
      bsts::AddSemilocalLinearTrend(y = estado_cases$log_casos) |>
      bsts::AddAutoAr(y = estado_cases$log_casos, lags = 30) |>
      bsts::AddSeasonal(y = estado_cases$log_casos, nseasons = 7) |>
      bsts::AddSeasonal(y = estado_cases$log_casos, nseasons = 52, season.duration = 7) |>
      bsts::AddTrig(y = estado_cases$log_casos, period = 365, frequencies = 2)

    fit   <- bsts::bsts(estado_cases$log_casos, model_components, niter = 500, ping = 0)
    pred  <- predict(fit, horizon = days_to_predict, burn = 250)
    pred  <- dplyr::tibble(Mean     = (pred$mean*sdy + mu)^2 ,
                    Lower_CI = (pmax(0, pred$interval[1,]*sdy + mu))^2,
                    Fecha    = seq(max(casos_model$Fecha) + 1,
                                   max(casos_model$Fecha) + days_to_predict,
                                   by = "1 day"),
                    Entidad   = col) |>
      mutate(Upper_CI = pmin((pred$interval[2,]*sdy + mu)^2, 2*Mean - Lower_CI))

    data_model <- data_model |> dplyr::full_join(pred)

    fechas <- dplyr::tibble(Fecha = seq(min(data_model$Fecha) - 7, max(pred$Fecha) + 7, by = "1 day"))
    fechas <- fechas |>
      dplyr::mutate(epiweek = lubridate::epiweek(Fecha)) |>
      dplyr::mutate(epiyear = lubridate::epiyear(Fecha)) |>
      dplyr::arrange(Fecha) |>
      dplyr::group_by(epiweek, epiyear) |>
      dplyr::mutate(val = 1:n()) |>
      dplyr::filter(val == 1) |>
      dplyr::select(-val)

    plot_estado <- data_model |>
      dplyr::mutate(epiweek = epiweek(Fecha)) |>
      dplyr::mutate(epiyear = epiyear(Fecha)) |>
      dplyr::group_by(epiweek, epiyear, Entidad) |>
      dplyr::summarise(Mean = sum(Mean, na.rm = T),
                Lower_CI = sum(Lower_CI, na.rm = T),
                Upper_CI = sum(Upper_CI, na.rm = T),
                `Total de casos` = sum(`Total de casos`, na.rm = T),
                .groups = "keep") |>
      dplyr::filter(Entidad == col) |>
      dplyr::left_join(fechas, by = c("epiweek", "epiyear")) |>
      ggplot2::ggplot(aes(x = Fecha)) +
      ggplot2::geom_col(aes(y = Mean, fill = "Proyección")) +
      ggplot2::geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.25, fill = colores_gob[4]) +
      ggplot2::geom_col(aes(y = `Total de casos`, fill = "Observado")) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        x = "Fecha",
        y = "Incapacidades Temporales en el Trabajo",
        title = glue::glue("Incapacidades Temporales en el Trabajo en {col}"),
        caption  = glue::glue("Modelo Bayesiano versión {version} elaborado el {tudei}")
      ) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::scale_fill_manual("Datos", values = c(colores_gob[1],colores_gob[3])) +
      ggplot2::theme(legend.position = "bottom")
    ggplot2::ggsave(glue::glue("{myfolder}/{col}.pdf"), plot_estado, width = 8, height = 4)

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  plot_nacional <- data_model |>
    dplyr::mutate(epiweek = lubridate::epiweek(Fecha)) |>
    dplyr::mutate(epiyear = lubridate::epiyear(Fecha)) |>
    dplyr::group_by(epiweek, epiyear, Entidad) |>
    dplyr::summarise(Mean = sum(Mean, na.rm = T),
              Lower_CI = sum(Lower_CI, na.rm = T),
              Upper_CI = sum(Upper_CI, na.rm = T),
              `Total de casos` = sum(`Total de casos`, na.rm = T),
              .groups = "keep") |>
    dplyr::left_join(fechas, by = c("epiweek", "epiyear")) |>
    ggplot2::ggplot(aes(x = Fecha)) +
    ggplot2::geom_col(aes(y = Mean, fill = "Proyección")) +
    ggplot2::geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.25, fill = colores_gob[4]) +
    ggplot2::geom_col(aes(y = `Total de casos`, fill = "Observado")) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~Entidad, scales = "free_y", nrow = 8) +
    ggplot2::labs(
      x = "Fecha",
      y = "Incapacidades Temporales en el Trabajo",
      title    = glue::glue("Incapacidades Temporales en el Trabajo"),
      subtitle = glue::glue("_Actualización al {tudei}_"),
      caption  = glue::glue("Modelo Bayesiano versión {version} elaborado el {tudei}")
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_fill_manual("Datos", values = c(colores_gob[1],colores_gob[3])) +
    ggplot2::theme(legend.position = "bottom",
          plot.title = ggtext::element_markdown(size = 25,
                                        padding = margin(5, 0, 0, 0)),
          plot.caption = ggtext::element_markdown(),
          plot.subtitle = ggtext::element_markdown(size = 12, color = colores_gob[4]))
  ggplot2::ggsave(glue::glue("{myfolder}/nacional.pdf"), plot_nacional, width = 9, height = 12)

  data_model |>
    dplyr::mutate(epiweek = epiweek(Fecha)) |>
    dplyr::mutate(epiyear = epiyear(Fecha)) |>
    dplyr::group_by(epiweek, epiyear, Entidad) |>
    dplyr::summarise(Mean = sum(Mean, na.rm = T),
              Lower_CI = sum(Lower_CI, na.rm = T),
              Upper_CI = sum(Upper_CI, na.rm = T),
              `Total de casos` = sum(`Total de casos`, na.rm = T),
              .groups = "keep") |>
    dplyr::mutate(Version = version) |>
    readr::write_excel_csv(glue::glue("{myfolder}/modelo_semanas.csv"))

  data_model |>
    dplyr::mutate(epiweek = epiweek(Fecha)) |>
    dplyr::mutate(epiyear = epiyear(Fecha)) |>
    dplyr::mutate(Version = version) |>
    dplyr::write_excel_csv(glue::glue("{myfolder}/modelo_dias.csv"))


  minimo_date <- data_model |>
    dplyr::filter(Fecha < ymd("2023/01/01") & Fecha >= tudei) |>
    dplyr::mutate(epiweek = epiweek(Fecha)) |>
    dplyr::mutate(epiyear = epiyear(Fecha)) |>
    dplyr::group_by(epiweek, epiyear) |>
    dplyr::summarise(Mean = sum(Mean, na.rm = T),
              Lower_CI = sum(Lower_CI, na.rm = T),
              Upper_CI = sum(Upper_CI, na.rm = T),
              `Total de casos` = sum(`Total de casos`, na.rm = T),
              .groups = "keep") |>
    dplyr::left_join(fechas, by = c("epiweek", "epiyear")) |>
    dplyr::ungroup() |>
    dplyr::filter(Mean == min(Mean) | Lower_CI == min(Lower_CI) | Upper_CI == min(Upper_CI)) |>
    dplyr::arrange(Fecha)

  plot_sum <- data_model |>
    dplyr::mutate(epiweek = epiweek(Fecha)) |>
    dplyr::mutate(epiyear = epiyear(Fecha)) |>
    dplyr::group_by(epiweek, epiyear) |>
    dplyr::summarise(Mean = sum(Mean, na.rm = T),
              Lower_CI = sum(Lower_CI, na.rm = T),
              Upper_CI = sum(Upper_CI, na.rm = T),
              `Total de casos` = sum(`Total de casos`, na.rm = T),
              .groups = "keep") |>
    dplyr::left_join(fechas, by = c("epiweek", "epiyear")) |>
    #mutate(Mean = if_else(Fecha == minimo_date$Fecha[6], 0.95*Mean, Mean)) |>
    ggplot2::ggplot(aes(x = Fecha)) +
    ggplot2::geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = colores_gob[4], alpha = 0.25) +
    ggplot2::geom_col(aes(y = Mean, fill = "Proyección")) +
    ggplot2::geom_col(aes(y = `Total de casos`, fill = "Observado")) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Fecha",
      y = "Incapacidades Temporales en el Trabajo",
      title = glue::glue("Incapacidades Temporales en el Trabajo"),
      caption  = glue::glue("Modelo Bayesiano versión {version} elaborado el {tudei}")
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma, breaks = (0:5)*1.e5) +
    ggplot2::scale_fill_manual("Datos", values = c(colores_gob[1],colores_gob[3])) +
    ggplot2::theme(legend.position = "right",
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 7)) +
    ggplot2::geom_vline(aes(xintercept = minimo_date$Fecha[6]), linetype = "dotted") +
    ggplot2::annotate("label", x = minimo_date$Fecha[6], y = 400000,
             label = glue::glue("Fecha aproximada\ndel máximo:\n{minimo_date$Fecha[7]}\n[± 2 semanas]"),
             size = 3)  +
    ggplot2::coord_cartesian(xlim = c(ymd("2020/01/01"),ymd("2023/01/01")))
  ggplot2::ggsave(glue::glue("{myfolder}/Nacional_sumados.pdf"), plot_sum, width = 10, height = 5)
  ggplot2::ggsave(glue::glue("{myfolder}/Nacional_sumados.png"), plot_sum, width = 10, height = 5, dpi = 750)

  cli::cli_alert_success("Elaborado modelo para el {tudei}")
}

