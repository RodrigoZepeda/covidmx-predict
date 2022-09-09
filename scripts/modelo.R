rm(list = ls())
setwd("~/Dropbox/ITT_v2022")
pacman::p_load(clusterGeneration, emulator, tidyverse, cmdstanr, bayestestR,
               lubridate, posterior, glue, bsts, gobmx, ggtext)

#Calculamos el máximo
colores_gob <- c("#1e4236", "#c8a66a","#651d31", gobmx_palette("Federal")[3])


#Lectura de la base
data_as_is <- read_csv("CASOS_POR_RP.zip", locale = locale(encoding = "WINDOWS-1252")) %>%
  mutate(Fecha = dmy(str_sub(COLAPS_FECHA_CARGA, end = 9)))

casos_rp   <- data_as_is %>%
  dplyr::select(-COLAPS_FECHA_CARGA) %>%
  mutate(Epiweek = epiweek(Fecha)) %>%
  mutate(Epiyear = epiyear(Fecha)) %>%
  group_by(Fecha, Epiweek, Epiyear, CATEGORIA, DELEGACION_ADS) %>%
  summarise(casos = sum(casos), .groups = "keep") %>%
  ungroup() %>%
  identity()

#Completar con ceros los días sin reporte
fechas_incompletas <- tibble(Fecha = seq(min(data_as_is$Fecha), max(data_as_is$Fecha),
                                         by = "1 day")) %>%
  mutate(Epiweek = epiweek(Fecha)) %>%
  mutate(Epiyear = epiyear(Fecha))

#Checar que la última fecha esté y si no ponerla
if (max(data_as_is$Fecha) > max(fechas_incompletas$Fecha)){
  fechas_incompletas <- fechas_incompletas %>%
    bind_rows(tibble(Fecha = max(data_as_is$Fecha),
                     Epiyear = epiyear(max(data_as_is$Fecha)),
                     Epiweek = epiweek(max(data_as_is$Fecha))))
}

fechas_incompletas <- fechas_incompletas %>%
  expand_grid(tibble(CATEGORIA = unique(casos_rp$CATEGORIA))) %>%
  expand_grid(tibble(DELEGACION_ADS = unique(casos_rp$DELEGACION_ADS)))

casos_rp <- casos_rp %>%
  full_join(fechas_incompletas,  by = c("CATEGORIA", "DELEGACION_ADS", "Epiweek", "Epiyear", "Fecha")) %>%
  mutate(casos = replace_na(casos, 0)) %>%
  dplyr::select(CATEGORIA, DELEGACION_ADS, casos, Fecha)

casos_rp <- casos_rp %>%
  mutate(Estado = case_when(
    str_detect(DELEGACION_ADS, "DF N|DF S") ~ "CDMX",
    str_detect(DELEGACION_ADS, "VR N|VR S") ~ "VCZ",
    str_detect(DELEGACION_ADS, "MEX O|MEX P") ~ "EDOMEX",
    TRUE ~ DELEGACION_ADS
  )) %>%
  dplyr::select(-DELEGACION_ADS)

casos_rp <- casos_rp %>%
  group_by(Fecha, Estado) %>%
  summarise(casos = sum(casos), .groups = "keep") %>%
  ungroup()

# casos_rp <- casos_rp %>%
#   mutate(Categoria_numerica = as.numeric(as.factor(CATEGORIA))) %>%
#   mutate(Estado_numerico = as.numeric(as.factor(Estado)))
#
# casos_model <- casos_rp %>%
#   dplyr::select(Fecha, Categoria_numerica, CATEGORIA, Estado, casos) %>%
#   pivot_wider(names_from = Estado,
#               values_from = casos)

NStates  <- length(unique(casos_rp$Estado))
NTime    <- length(unique(casos_rp$Fecha))

casos_model <- casos_rp %>%
  pivot_wider(names_from = Estado, values_from = casos) %>%
  arrange(Fecha)

estados     <- colnames(casos_model)[2:ncol(casos_model)]
period_pred <- 180 #Days in the future to predict
casos_pred  <- casos_rp
myfolder    <- glue("waldo_{today()}")
if (dir.exists(myfolder)){
  unlink(myfolder, recursive = T, force = T)
}
dir.create(myfolder)
for (col in estados){
  
  #Obtenemos el tibble de casos
  estado_cases <- casos_model[, c("Fecha", col)]
  colnames(estado_cases) <- c("Fecha", "Casos")
  
  #Noralozamos
  mu  <- mean(estado_cases$Casos)
  sdy <- sd(estado_cases$Casos)
  estado_cases$log_casos <- (sqrt(estado_cases$Casos) - mu)/sdy
  
  #Creamos el objeto bsts
  model_components <- list()
  model_components <- model_components %>%
    AddSemilocalLinearTrend(y = estado_cases$log_casos) %>%
    AddAutoAr(y = estado_cases$log_casos, lags = 30) %>%
    AddSeasonal(y = estado_cases$log_casos, nseasons = 7) %>%
    AddSeasonal(y = estado_cases$log_casos, nseasons = 52, season.duration = 7) %>%
    AddTrig(y = estado_cases$log_casos, period = 365, frequencies = 2)
  
  fit   <- bsts(estado_cases$log_casos, model_components, niter = 500)
  pred  <- predict(fit, horizon = period_pred, burn = 250)
  pred  <- tibble(Mean     = (pred$mean*sdy + mu)^2 ,
                  Lower_CI = (pmax(0, pred$interval[1,]*sdy + mu))^2,
                  Fecha    = seq(max(casos_model$Fecha) + 1,
                                 max(casos_model$Fecha) + period_pred,
                                 by = "1 day"),
                  Estado   = col) %>%
    mutate(Upper_CI = pmin((pred$interval[2,]*sdy + mu)^2, 2*Mean - Lower_CI))
  
  casos_pred <- casos_pred %>% full_join(pred)
  
  fechas <- tibble(Fecha = seq(min(casos_rp$Fecha) - 7, max(pred$Fecha) + 7, by = "1 day"))
  fechas <- fechas %>%
    mutate(epiweek = epiweek(Fecha)) %>%
    mutate(epiyear = epiyear(Fecha)) %>%
    arrange(Fecha) %>%
    group_by(epiweek, epiyear) %>%
    mutate(val = 1:n()) %>%
    filter(val == 1) %>%
    dplyr::select(-val)
  
  
  plot_estado <- casos_pred %>%
    mutate(epiweek = epiweek(Fecha)) %>%
    mutate(epiyear = epiyear(Fecha)) %>%
    group_by(epiweek, epiyear, Estado) %>%
    summarise(Mean = sum(Mean, na.rm = T),
              Lower_CI = sum(Lower_CI, na.rm = T),
              Upper_CI = sum(Upper_CI, na.rm = T),
              casos = sum(casos, na.rm = T),
              .groups = "keep") %>%
    filter(Estado == col) %>%
    left_join(fechas, by = c("epiweek", "epiyear")) %>%
    ggplot(aes(x = Fecha)) +
    geom_col(aes(y = Mean, fill = "Proyección")) +
    geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.25, fill = colores_gob[4]) +
    geom_col(aes(y = casos, fill = "Observado")) +
    theme_classic() +
    labs(
      x = "Fecha",
      y = "Incapacidades Temporales en el Trabajo",
      title = glue("Incapacidades Temporales en el Trabajo en {col}"),
      caption = glue("Modelo elaborado el {today()}")
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual("Datos", values = c(colores_gob[1],colores_gob[3])) +
    theme(legend.position = "bottom")
  ggsave(glue("{myfolder}/{col}.pdf"), plot_estado, width = 8, height = 4)
  
}

plot_nacional <- casos_pred %>%
  mutate(epiweek = epiweek(Fecha)) %>%
  mutate(epiyear = epiyear(Fecha)) %>%
  group_by(epiweek, epiyear, Estado) %>%
  summarise(Mean = sum(Mean, na.rm = T),
            Lower_CI = sum(Lower_CI, na.rm = T),
            Upper_CI = sum(Upper_CI, na.rm = T),
            casos = sum(casos, na.rm = T),
            .groups = "keep") %>%
  left_join(fechas, by = c("epiweek", "epiyear")) %>%
  ggplot(aes(x = Fecha)) +
  geom_col(aes(y = Mean, fill = "Proyección")) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.25, fill = colores_gob[4]) +
  geom_col(aes(y = casos, fill = "Observado")) +
  theme_minimal() +
  facet_wrap(~Estado, scales = "free_y", nrow = 8) +
  labs(
    x = "Fecha",
    y = "Incapacidades Temporales en el Trabajo",
    title = glue("Incapacidades Temporales en el Trabajo"),
    subtitle = glue("_Actualización al {today()}_"),
    caption = "Modelo Bayesiano de Series de Tiempo Estructurales"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual("Datos", values = c(colores_gob[1],colores_gob[3])) +
  theme(legend.position = "bottom",
        plot.title = element_markdown(size = 25,
                                      padding = margin(5, 0, 0, 0)),
        plot.caption = element_markdown(),
        plot.subtitle = element_markdown(size = 12, color = colores_gob[4]))
ggsave(glue("{myfolder}/nacional.pdf"), plot_nacional, width = 9, height = 12)

casos_pred %>%
  mutate(epiweek = epiweek(Fecha)) %>%
  mutate(epiyear = epiyear(Fecha)) %>%
  group_by(epiweek, epiyear, Estado) %>%
  summarise(Mean = sum(Mean, na.rm = T),
            Lower_CI = sum(Lower_CI, na.rm = T),
            Upper_CI = sum(Upper_CI, na.rm = T),
            casos = sum(casos, na.rm = T),
            .groups = "keep") %>%
  write_excel_csv(glue("{myfolder}/modelo_semanas.csv"))

casos_pred %>%
  mutate(epiweek = epiweek(Fecha)) %>%
  mutate(epiyear = epiyear(Fecha)) %>%
  write_excel_csv(glue("{myfolder}/modelo_dias.csv"))


minimo_date <- casos_pred %>%
  filter(Fecha < ymd("2023/01/01") & Fecha >= today()) %>%
  mutate(epiweek = epiweek(Fecha)) %>%
  mutate(epiyear = epiyear(Fecha)) %>%
  group_by(epiweek, epiyear) %>%
  summarise(Mean = sum(Mean, na.rm = T),
            Lower_CI = sum(Lower_CI, na.rm = T),
            Upper_CI = sum(Upper_CI, na.rm = T),
            casos = sum(casos, na.rm = T),
            .groups = "keep") %>%
  left_join(fechas, by = c("epiweek", "epiyear")) %>%
  ungroup() %>%
  filter(Mean == min(Mean) | Lower_CI == min(Lower_CI) | Upper_CI == min(Upper_CI)) %>%
  arrange(Fecha)

plot_sum <- casos_pred %>%
  mutate(epiweek = epiweek(Fecha)) %>%
  mutate(epiyear = epiyear(Fecha)) %>%
  group_by(epiweek, epiyear) %>%
  summarise(Mean = sum(Mean, na.rm = T),
            Lower_CI = sum(Lower_CI, na.rm = T),
            Upper_CI = sum(Upper_CI, na.rm = T),
            casos = sum(casos, na.rm = T),
            .groups = "keep") %>%
  left_join(fechas, by = c("epiweek", "epiyear")) %>%
  #mutate(Mean = if_else(Fecha == minimo_date$Fecha[6], 0.95*Mean, Mean)) %>%
  ggplot(aes(x = Fecha)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = colores_gob[4], alpha = 0.25) +
  geom_col(aes(y = Mean, fill = "Proyección")) +
  geom_col(aes(y = casos, fill = "Observado")) +
  theme_classic() +
  labs(
    x = "Fecha",
    y = "Incapacidades Temporales en el Trabajo",
    title = glue("Incapacidades Temporales en el Trabajo"),
    caption = glue("Modelo elaborado el {today()}")
  ) +
  scale_y_continuous(labels = scales::comma, breaks = (0:5)*1.e5) +
  scale_fill_manual("Datos", values = c(colores_gob[1],colores_gob[3])) +
  theme(legend.position = "right",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7)) +
  geom_vline(aes(xintercept = minimo_date$Fecha[6]), linetype = "dotted") +
  annotate("label", x = minimo_date$Fecha[6], y = 400000,
           label = glue("Fecha aproximada\ndel mínimo:\n{minimo_date$Fecha[7]}\n[± 2 semanas]"),
           size = 3)  +
  coord_cartesian(xlim = c(ymd("2020/01/01"),ymd("2023/01/01")))
ggsave(glue("{myfolder}/Nacional_sumados.pdf"), plot_sum, width = 10, height = 5)
ggsave(glue("{myfolder}/Nacional_sumados.png"), plot_sum, width = 10, height = 5, dpi = 750)
