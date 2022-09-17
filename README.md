# covidmx-predict

Docker con modelo de predicción de COVID-19 (incapacidades o casos). 

## Ejecución

El orden de la ejecución es como sigue: 
```{bash}
docker run -it --mount src=`pwd`,target=/data,type=bind rodrigozepeda/covidmx "archivo_casos.csv" "encoding" "dias a predecir" "tipo de modelo" "¿actualizar modelo?"
```

Como ejemplo el siguiente lee `casos_ejemplo` y predice los próximos 180 días bajo el modelo de `covid` el cual se actualiza `auto`(máticamente).  

```{bash}
 docker run -it --mount src=`pwd`,target=/data,type=bind rodrigozepeda/covidmx "casos_ejemplo.csv" "UTF-8" "180" "covid" "auto
```

donde 

+ `casos_ejemplo.csv` es el archivo diario agregado por casos, 
+ `UTF-8` es el encoding (para [readr::read_csv()]()) (otra opción es `WINDOWS-1252`)
+ `180` son los días a predecir (futuro), `covid` es los casos a predecir del modelo (opciones a desarrollar **futuras**: `dengue`), 
+ `auto` es si actualizar el modelo automáticamente desde internet (la otra opción es `"no"`). 

> **Nota** Para un funcionamiento óptimo requiere conexión a Internet si se permiten actualizaciones mediante `auto`.

## Datos

Los datos deben representarse en un `.csv` (encoding `UTF-8`) con la siguiente estructura (ver `casos_ejemplo.csv`)

|    Fecha   |  Total de casos | Entidad |
|------------|-----------------|---------|
| 2020/01/01 |        12       |  AGS    |
| 2020/01/01 |        84       |  BC     |
| 2020/01/02 |        0        |  AGS    |
| 2020/01/02 |        5        |  BC     |

+ Hay **32** entidades.
+ El nacional se calcula como la suma de los casos por entidad para la misma Fecha. 
+ La fecha es en formato `ymd`. 
+ Los datos son diarios. 

## Especificaciones adicionales IMSS:

+ El modelo comienza en 2020. 

+ Los códigos CIE-10 que se incluyen son:  ICD-10 (J01, J04-J06, J20, y J21), gripa (J10/ J11), neumonía (J12, J18), COVID-19 (U07.1, U07I, y U07S), y cualquier diagnóstico con la palabra COVID-19 o su sospecha. 

+ Se excluyen trabajadores IMSS. 

## Actualización

De manera automática el Docker verifica que la versión existente del modelo en el **Github** en `version.txt` y si el archivo es más reciente descarga el modelo de **Github** y corre ese. 

