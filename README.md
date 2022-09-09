# covidmx-predict

Docker con modelo de predicción de COVID-19 (incapacidades o casos). 

> **Nota** Para un funcionamiento óptimo requiere conexión a Internet.

## Datos

Los datos deben representarse en un `.csv` con la siguiente estructura (ver `casos_ejemplo.csv`)

| Semana epidemiológica | Año epidemiológico | Total de casos | Entidad |
|-----------------------|--------------------|----------------|---------|
|         1             |         2020       |       12       |  AGS    |
|         1             |         2020       |       84       |  NAL    |
|         2             |         2020       |       0        |  AGS    |
|         2             |         2020       |       5        |  NAL    |

En caso de que se esté trabajando sólo una entidad se puede omitir la última columna. 

## Actualización

De manera automática el Docker verifica que la versión existente del modelo en el **Github** en `version.txt` y si el archivo es más reciente descarga el modelo de **Github** y corre ese. 
