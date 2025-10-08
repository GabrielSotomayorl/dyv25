---
title: "Descripción de Variables Categóricas: Frecuencias y Gráficos de Barras en R"
linktitle: "8: Descripción de Variables Categóricas"
date: "2025-10-13"
menu:
  example:
    parent: Ejemplos
    weight: 8
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---

## 0. Objetivos del Práctico

El objetivo de este práctico es aplicar los conceptos de descripción de variables categóricas vistos en clase, utilizando R para el análisis. Al finalizar, serás capaz de:

*   Importar una base de datos en formato Stata (`.dta`).
*   Generar e interpretar tablas de frecuencias (absolutas y porcentuales) usando R base y `dplyr`.
*   Crear tablas de presentación estéticas con el paquete `knitr`.
*   Visualizar la distribución de una variable categórica usando gráficos de barras, tanto con R base como con `ggplot2`.

## 1. Contexto: La Encuesta Nacional de Empleo (ENE)

La **Encuesta Nacional de Empleo (ENE)**, realizada por el Instituto Nacional de Estadísticas (INE), es el principal instrumento para medir el mercado laboral en Chile. Sus datos se publican mensualmente y son fundamentales para el diagnóstico económico y social del país.

Hoy trabajaremos con los datos del trimestre móvil **Junio-Julio-Agosto de 2025**, y nos enfocaremos en la variable **`activ`**, que mide la condición de actividad de las personas.

## 2. Preparación del Entorno y Carga de Datos

### 2.1 Configuración del Proyecto y Descarga de Datos

1.  **Crea un Proyecto de RStudio** y una carpeta `datos` dentro de él.
2.  **Descarga la base de datos** desde el sitio del INE. El archivo está en formato Stata (`.dta`).
    *   [Enlace de descarga: ENE Junio-Julio-Agosto 2025](https://www.ine.gob.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2025/stata/ene-2025-07-jja.dta?sfvrsn=3f9c9348_5&download=true)
3.  **Guarda el archivo:** Guarda el archivo `ene-2025-07-jja.dta` dentro de tu carpeta `datos`.

### 2.2 Carga de Paquetes


```r
library(haven)
library(tidyverse)
```

### 2.3 Carga de la Base de Datos

Vamos a cargar los datos usando `read_dta()` del paquete `haven`, ya que es un archivo de Stata.


```r
# Cargar la base de datos de la ENE desde tu carpeta local
ene <- haven::read_dta("datos/ene-2025-07-jja.dta")
```

---
*Nota sobre la reproducibilidad:* Para que este práctico funcione de manera autocontenida, a continuación se incluye el código que realiza la descarga y carga de forma automática.


```r
# Este código cargará automáticamente los datos desde la web
temp <- tempfile(fileext = ".dta")
download.file("https://www.ine.gob.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2025/stata/ene-2025-07-jja.dta?sfvrsn=3f9c9348_5&download=true", temp, mode="wb")
ene <- haven::read_dta(temp)
unlink(temp); remove(temp)
```

## 3. Tablas de Frecuencia

El primer paso para describir una variable categórica es crear una tabla de frecuencias. Veremos tres formas de hacerlo en R.

### 3.1 Con R Base: `table()` y `prop.table()`

La función `table()` es la forma más rápida de obtener un conteo de **frecuencias absolutas**.


```r
# Primero, convertimos la variable 'activ' a factor para que sea legible
ene$condicion_actividad <- as_factor(ene$activ)

# Ahora creamos la tabla
frec_abs <- table(ene$condicion_actividad)
frec_abs
```

```
## 
##                   Ocupados/as                Desocupados/as 
##                         42467                          3918 
## Fuera de la fuerza de trabajo 
##                         36286
```

Para obtener **frecuencias relativas (porcentajes)**, podemos aplicar la función `prop.table()` al resultado de `table()`.


```r
# El resultado es una proporción, por lo que multiplicamos por 100
frec_rel <- prop.table(frec_abs) * 100
frec_rel
```

```
## 
##                   Ocupados/as                Desocupados/as 
##                     51.368678                      4.739268 
## Fuera de la fuerza de trabajo 
##                     43.892054
```

**Interpretación:** Los outputs nos muestran el conteo de personas y luego el porcentaje en cada una de las tres categorías. Es un método rápido, pero requiere dos pasos y el resultado es menos prolijo que con otras alternativas.

### 3.2 Una Alternativa Útil: `sjmisc::frq()`

Existen paquetes que ofrecen funciones más completas. Por ejemplo, la función `frq()` del paquete `sjmisc` crea una tabla de frecuencias muy informativa con una sola línea de código, incluyendo porcentajes válidos y acumulados.
*(Nota: No la usaremos en detalle hoy, pero es bueno que conozcas su existencia).*

### 3.3 Con `dplyr`: El Flujo de Trabajo del Tidyverse

Para mantener la coherencia con el curso, usaremos el flujo de `dplyr`. Un paso importante es filtrar primero la base para quedarnos solo con la **población en edad de trabajar (15 años o más)**, ya que la condición de actividad solo se define para este grupo.


```r
# Explicación del código:
# 1. Filtramos la base para incluir solo a personas con edad >= 15.
# 2. count() cuenta el número de casos para cada categoría de 'condicion_actividad'.
# 3. mutate() crea una nueva columna 'Porcentaje'. Para calcularla:
#    - Dividimos la frecuencia de cada fila (Frecuencia_Absoluta) por el total de casos (sum(Frecuencia_Absoluta)).
#    - Multiplicamos por 100 para obtener el porcentaje.

tabla_frec <- ene %>%
  filter(edad >= 15) %>%
  count(condicion_actividad, name = "Frecuencia_Absoluta") %>%
  mutate(
    Porcentaje = (Frecuencia_Absoluta / sum(Frecuencia_Absoluta)) * 100
  )

tabla_frec
```

```
## # A tibble: 3 × 3
##   condicion_actividad           Frecuencia_Absoluta Porcentaje
##   <fct>                                       <int>      <dbl>
## 1 Ocupados/as                                 42467      51.4 
## 2 Desocupados/as                               3918       4.74
## 3 Fuera de la fuerza de trabajo               36286      43.9
```

## 4. Presentando Tablas con `knitr::kable()`

Las tablas que R muestra en la consola son funcionales, pero no muy estéticas. Para crear tablas de presentación (para informes o publicaciones), podemos usar la función `kable()` del paquete `knitr`.


```r
knitr::kable(
  tabla_frec,
  # Le damos formato a los números: 0 decimales para Frecuencia, 1 para Porcentaje
  digits = c(0, 0, 1),
  # Le damos nombres más claros a las columnas
  col.names = c("Condición de Actividad", "Frecuencia", "Porcentaje (%)"),
  # Añadimos un título
  caption = "Distribución de la Condición de Actividad (Población 15 años o más)"
)
```



Table: <span id="tab:table-kable"></span>Table 1: Distribución de la Condición de Actividad (Población 15 años o más)

|Condición de Actividad        | Frecuencia| Porcentaje (%)|
|:-----------------------------|----------:|--------------:|
|Ocupados/as                   |      42467|           51.4|
|Desocupados/as                |       3918|            4.7|
|Fuera de la fuerza de trabajo |      36286|           43.9|

**Interpretación:** La tabla es ahora mucho más clara y profesional. Vemos que la **moda** es "Ocupados/as" (51.4%) y que un 43.9% de la población en edad de trabajar se encuentra fuera de la fuerza de trabajo.

## 5. Visualización: Gráficos de Barras

Ahora, vamos a visualizar la información de nuestra tabla.

### 5.1 Gráfico de Barras con R Base

La función `barplot()` de R base crea gráficos de barras. Requiere que le entreguemos una tabla de frecuencias ya calculada. Es una herramienta rápida para una exploración inicial.


```r
# 1. Creamos la tabla de frecuencias que vamos a graficar, filtrando por edad
frecuencias_pet <- table(ene$condicion_actividad[ene$edad >= 15])

# 2. Creamos el gráfico de barras, explicando sus argumentos
barplot(
  height = frecuencias_pet, # El argumento principal son las alturas de las barras
  main = "Distribución de la Condición de Actividad", # Título del gráfico
  xlab = "Condición de Actividad", # Etiqueta del eje X
  ylab = "Frecuencia Absoluta", # Etiqueta del eje Y
  col = "#D76D77", # Color de las barras
  ylim = c(0, 60000) # Límite del eje Y para dar más espacio arriba
)
```

<img src="/example/08-practico_files/figure-html/barplot-base-1.png" width="672" />
**Análisis:** El gráfico nos da una idea visual rápida de las magnitudes. Su principal ventaja es la simplicidad y la velocidad, pero su personalización es menos intuitiva que con `ggplot2`.

### 5.2 Gráfico de Barras con `ggplot2`

`ggplot2` nos permite construir gráficos por capas, dándonos un control total. `geom_bar()` puede calcular las frecuencias por nosotros directamente desde los datos.


```r
# Explicación del código:
# 1. ggplot(data, aes(x)): Iniciamos el gráfico, definimos los datos y el mapeo estético principal (qué va en el eje x).
# 2. geom_bar(): Añadimos la capa geométrica. Al no especificar un eje 'y', `geom_bar` cuenta automáticamente los casos por cada categoría de 'x'.
# 3. labs(): Añadimos las etiquetas, títulos y fuentes.

ene %>%
  filter(edad >= 15) %>%
  ggplot(aes(x = condicion_actividad)) +
  geom_bar(fill = "#3A1C71") +
  labs(
    title = "Distribución de la Condición de Actividad",
    subtitle = "Trimestre Junio-Julio-Agosto 2025",
    x = "Condición de Actividad",
    y = "Frecuencia Absoluta",
    caption = "Fuente: ENE"
  ) +
  theme_minimal()
```

<img src="/example/08-practico_files/figure-html/barplot-ggplot-simple-1.png" width="672" />
**Análisis:** Este gráfico es más estético y fácil de etiquetar. Sin embargo, como vimos en clase, podemos mejorarlo aún más, por ejemplo, ordenando las barras y usando porcentajes.

## 6. Actividad de Desafío

1.  **Crear una tabla de frecuencias completa:** Usando el flujo de `dplyr` y `knitr::kable()`, crea una tabla de frecuencias para la variable `region` de la encuesta ENE. Asegúrate de incluir frecuencias absolutas y porcentajes.
2.  **Crear un gráfico de barras:** Usando `ggplot2`, crea un gráfico de barras que muestre la distribución de personas por `region`.



```r
# Escribe tu código para la tabla aquí


# Escribe tu código para el gráfico aquí
```
