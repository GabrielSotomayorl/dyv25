---
title: "Manipulación de Datos con dplyr: Respondiendo Preguntas con datos sociales"
linktitle: "6: Manipulación de Datos con dplyr"
date: "2025-09-22"
menu:
  example:
    parent: Ejemplos
    weight: 6
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---

## 0. Objetivos del Práctico

El objetivo de este práctico es aprender a manipular y transformar datos en R utilizando la "gramática" del paquete `dplyr`. Al finalizar, serás capaz de:

*   Seleccionar y filtrar datos usando tanto R base como `dplyr`, comprendiendo sus diferencias.
*   Entender y aplicar el operador *pipe* (`%>%`) para crear flujos de trabajo legibles.
*   Utilizar los cinco verbos principales de `dplyr` (`select`, `filter`, `arrange`, `mutate`, `summarise`).
*   Responder preguntas de investigación simples mediante la combinación de `group_by()` y `summarise()`.

## 1. Preparación del Entorno (Recordatorio)

Un flujo de trabajo reproducible comienza con una buena organización. **No olvides realizar estos pasos cada vez que inicies un nuevo análisis.**

### 1.1 Crear un Proyecto de RStudio

Siempre trabaja dentro de un **Proyecto (`.Rproj`)**. Esto asegura que tus rutas a los archivos sean relativas y que tu código funcione en cualquier computador.

**Acción:** Si no lo has hecho, crea un Proyecto de RStudio para esta unidad. Guarda este archivo `.qmd` y la base de datos CASEN en carpetas dentro de tu proyecto.

### 1.2 Cargar Paquetes

Recuerda la regla de oro: los paquetes **se instalan una vez** con `install.packages()`, pero **se cargan siempre** al inicio de cada sesión con `library()`.


```r
# Cargar los paquetes que usaremos hoy
library(haven)
library(tidyverse)
```

### 1.3 Cargar la Base de Datos

Vamos a cargar la Encuesta CASEN 2022. Para ello, seguiremos el método estándar de trabajo: descargar el archivo y cargarlo en R desde nuestro proyecto.

**Paso 1: Descargar los datos**

Primero, debes descargar la base de datos oficial. Puedes encontrarla en el sitio del **Observatorio Social del Ministerio de Desarrollo Social y Familia**.

-   **Enlace de descarga:** [Encuesta CASEN 2022 - Base de Datos SPSS](https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2022)
-   **Acción:** Descarga el archivo `.zip` que contiene la base de datos en formato SPSS (`.sav`).

**Paso 2: Organizar tus archivos**

Una vez descargado, descomprime el archivo `.zip` y guarda el archivo `.sav` (que se llama "Base de datos Casen 2022 SPSS_18 marzo 2024.sav") en una carpeta llamada `datos` dentro de tu Proyecto de RStudio.

**Paso 3: Cargar los datos en R**

Ahora que el archivo está en su lugar, usamos la función `read_sav()` del paquete `haven` para importarlo a nuestro entorno de R. Le asignamos el nombre `casen` a nuestra base de datos.


```r
# Usamos una ruta relativa porque el archivo está dentro de nuestro proyecto
casen <- haven::read_sav("datos/Base de datos Casen 2022 SPSS_18 marzo 2024.sav")
```

---

*Nota sobre la reproducibilidad:* Para que este práctico y el sitio web del curso funcionen de manera autocontenida, a continuación se incluye un código que realiza la descarga y carga de forma automática. **No necesitas ejecutar este código si ya cargaste la base manualmente**, pero es una buena práctica conocerlo.


```r
# Código automático para cargar los datos
temp <- tempfile() 
download.file("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2022/Base%20de%20datos%20Casen%202022%20SPSS.sav.zip", temp)
casen <- haven::read_sav(unz(temp, "Base de datos Casen 2022 SPSS.sav"))
unlink(temp); remove(temp)
```

---

Ahora que tenemos nuestra base `casen` cargada, crearemos una submuestra más pequeña para trabajar de manera más cómoda y eficiente.


```r
# Creamos una submuestra con las variables que usaremos hoy
casen_sub <- casen[, c("folio", "pco1", "region", "sexo", "edad", "esc", "ytotcorh")]
```

## 2. R Base vs. `dplyr`: Un Contraste

Antes de sumergirnos en `dplyr`, comparemos cómo se realizan dos tareas básicas en R base para apreciar las ventajas de la nueva gramática.

### 2.1 Seleccionar Variables

**Objetivo:** Crear un `data.frame` solo con `edad`, `sexo` y `esc`.

**Con R Base:**

```r
casen_base_select <- casen_sub[, c("edad", "sexo", "esc")]
head(casen_base_select)
```

```
## # A tibble: 6 × 3
##    edad sexo            esc
##   <dbl> <dbl+lbl>     <dbl>
## 1    72 2 [2. Mujer]      1
## 2    67 1 [1. Hombre]     4
## 3    40 2 [2. Mujer]     15
## 4    56 1 [1. Hombre]    NA
## 5    25 2 [2. Mujer]     12
## 6     2 1 [1. Hombre]    NA
```
Usamos la notación de corchetes `[filas, columnas]`. Dejamos el espacio de las filas en blanco para indicar que queremos *todas las filas*, y le pasamos un vector de nombres de columnas `c("edad", "sexo", "esc")` para seleccionar solo esas variables.

**Con `dplyr`:**

```r
casen_dplyr_select <- select(casen_sub, edad, sexo, esc)
head(casen_dplyr_select)
```

```
## # A tibble: 6 × 3
##    edad sexo            esc
##   <dbl> <dbl+lbl>     <dbl>
## 1    72 2 [2. Mujer]      1
## 2    67 1 [1. Hombre]     4
## 3    40 2 [2. Mujer]     15
## 4    56 1 [1. Hombre]    NA
## 5    25 2 [2. Mujer]     12
## 6     2 1 [1. Hombre]    NA
```
La función `select()` es más explícita. El primer argumento es la base de datos, y los siguientes son los nombres de las variables que queremos mantener, sin comillas.

### 2.2 Filtrar Casos

**Objetivo:** Quedarnos solo con las personas mayores de 65 años.

**Con R Base:**

```r
casen_base_filter <- casen_sub[casen_sub$edad > 65, ]
dim(casen_base_filter)
```

```
## [1] 30809     7
```
Aquí, la condición lógica `casen_sub$edad > 65` se aplica en el espacio de las filas. R evalúa esta condición para cada fila y se queda solo con aquellas donde el resultado es `TRUE`.

**Con `dplyr`:**

```r
casen_dplyr_filter <- filter(casen_sub, edad > 65)
dim(casen_dplyr_filter)
```

```
## [1] 30809     7
```
El verbo `filter()` hace la misma operación, pero de una forma más legible: "Filtra la base `casen_sub` donde la edad sea mayor a 65".

## 3. La Gramática de `dplyr` y el Poder del *Pipe* `%>%`

### 3.1 La Lógica de las Funciones de `dplyr`

El paquete `dplyr` es tan poderoso porque sus funciones ("verbos") siguen una **gramática simple y consistente**:

1.  **El primer argumento es siempre un `data.frame`**.
2.  Los argumentos siguientes describen qué hacer, usando los nombres de las variables **sin comillas**.
3.  **El resultado es siempre un nuevo `data.frame`**. `dplyr` nunca modifica tus datos originales.

### 3.2 ¿Por qué funciona el *Pipe*?

El operador *pipe* (`%>%`) toma lo que está a su izquierda y lo pone como **primer argumento** de la función a su derecha.

Esta es la razón por la que `dplyr` y el *pipe* son la combinación perfecta: como todos los verbos de `dplyr` esperan un `data.frame` como primer argumento, podemos encadenarlos fluidamente.

**Sin el *pipe* (funciones anidadas):**
El código se lee "de adentro hacia afuera", lo que es poco intuitivo.

```r
casen_anidado <- filter(
  select(casen_sub, region, edad, ytotcorh),
  region == 13 # Usamos el código numérico de la RM
)
head(casen_anidado)
```

```
## # A tibble: 6 × 3
##   region                                 edad ytotcorh
##   <dbl+lbl>                             <dbl>    <dbl>
## 1 13 [Región Metropolitana de Santiago]    54  2945500
## 2 13 [Región Metropolitana de Santiago]    28  2945500
## 3 13 [Región Metropolitana de Santiago]    27  2945500
## 4 13 [Región Metropolitana de Santiago]    19  2945500
## 5 13 [Región Metropolitana de Santiago]    56  2945500
## 6 13 [Región Metropolitana de Santiago]    81   963997
```

**Con el *pipe* (`%>%`):**
El código se lee de izquierda a derecha. ¡Mucho más intuitivo!

```r
# Atajo para el pipe: Ctrl + Shift + M
casen_pipe <- casen_sub %>%
  select(region, edad, ytotcorh) %>%
  filter(region == 13)

head(casen_pipe)
```

```
## # A tibble: 6 × 3
##   region                                 edad ytotcorh
##   <dbl+lbl>                             <dbl>    <dbl>
## 1 13 [Región Metropolitana de Santiago]    54  2945500
## 2 13 [Región Metropolitana de Santiago]    28  2945500
## 3 13 [Región Metropolitana de Santiago]    27  2945500
## 4 13 [Región Metropolitana de Santiago]    19  2945500
## 5 13 [Región Metropolitana de Santiago]    56  2945500
## 6 13 [Región Metropolitana de Santiago]    81   963997
```
**A partir de ahora, usaremos el *pipe* para todas nuestras operaciones.**

## 4. Aplicando los Verbos de `dplyr`

Ahora, usemos esta nueva gramática para responder preguntas de investigación.

### 4.1 `arrange()`: Ordenando los datos

**Pregunta:** ¿Cuáles son los 5 hogares con mayor ingreso (`ytotcorh`) en nuestra submuestra?


```r
# Usamos arrange() con desc() para ordenar de mayor a menor
casen_sub %>%
  filter(pco1 == 1) %>% # 1. Filtramos solo jefes de hogar
  arrange(desc(ytotcorh)) %>%
  head(5) # Usamos head() para ver solo los primeros 5 resultados
```

```
## # A tibble: 5 × 7
##       folio pco1                     region         sexo     edad   esc ytotcorh
##       <dbl> <dbl+lbl>                <dbl+lbl>      <dbl+l> <dbl> <dbl>    <dbl>
## 1 295720201 1 [1. Jefatura de Hogar] 13 [Región Me… 1 [1. …    44    17 77300000
## 2 246360701 1 [1. Jefatura de Hogar] 14 [Región de… 1 [1. …    72    17 65167000
## 3 391140701 1 [1. Jefatura de Hogar]  3 [Región de… 1 [1. …    45    13 56833333
## 4 306270201 1 [1. Jefatura de Hogar]  4 [Región de… 1 [1. …    54    15 53783334
## 5 320530301 1 [1. Jefatura de Hogar] 14 [Región de… 1 [1. …    44    12 35143333
```
**Interpretación del output:** La tabla muestra las 5 filas que corresponden a los jefes de los hogares con los ingresos más altos en toda la muestra.

### 4.2 `mutate()`: Creando nuevas variables

`mutate()` nos permite crear nuevas columnas. La **buena práctica** es crear nuevas columnas con nombres diferentes (ej. `sexo_factor`) para no perder la información original (ej. `sexo`).

**Objetivo:** Crear una variable con la edad en meses y convertir `sexo` y `region` a factores para que sean más legibles.


```r
casen_procesada <- casen_sub %>%
  mutate(
    edad_meses = edad * 12, # Un cálculo aritmético simple
    sexo_factor = as_factor(sexo), # Creamos una nueva variable factor
    region_factor = as_factor(region) # Creamos otra variable factor
  )

# Veamos las nuevas columnas (al final a la derecha)
head(casen_procesada)
```

```
## # A tibble: 6 × 10
##      folio pco1     region   sexo     edad   esc ytotcorh edad_meses sexo_factor
##      <dbl> <dbl+lb> <dbl+lb> <dbl+l> <dbl> <dbl>    <dbl>      <dbl> <fct>      
## 1   1.00e8  2 [2. … 16 [Reg… 2 [2. …    72     1  1010894        864 2. Mujer   
## 2   1.00e8  1 [1. … 16 [Reg… 1 [1. …    67     4  1010894        804 1. Hombre  
## 3   1.00e8  4 [4. … 16 [Reg… 2 [2. …    40    15  1010894        480 2. Mujer   
## 4   1.00e8  2 [2. … 16 [Reg… 1 [1. …    56    NA   418192        672 1. Hombre  
## 5   1.00e8  4 [4. … 16 [Reg… 2 [2. …    25    12   418192        300 2. Mujer   
## 6   1.00e8 10 [10.… 16 [Reg… 1 [1. …     2    NA   418192         24 1. Hombre  
## # ℹ 1 more variable: region_factor <fct>
```
**Interpretación del output:** Nuestra nueva base de datos `casen_procesada` ahora tiene 10 columnas. Las tres últimas son las que acabamos de crear. `sexo_factor` y `region_factor` ahora contienen texto legible en lugar de códigos numéricos.

### 4.3 `group_by()` y `summarise()`: El corazón del análisis descriptivo

**Pregunta 1:** ¿Cuál es el promedio de años de escolaridad (`esc`) por sexo?

Ahora podemos usar nuestra nueva variable `sexo_factor` para que el resultado sea más claro.


```r
casen_procesada %>%
  group_by(sexo_factor) %>% # Agrupamos los datos por la variable factor
  summarise(
    escolaridad_promedio = mean(esc, na.rm = TRUE), # Calculamos la media para cada grupo
    n_casos = n() # n() es una función especial que cuenta los casos por grupo
  )
```

```
## # A tibble: 2 × 3
##   sexo_factor escolaridad_promedio n_casos
##   <fct>                      <dbl>   <int>
## 1 1. Hombre                   11.2   95656
## 2 2. Mujer                    11.1  106575
```
**Interpretación del output:** La tabla resultante muestra que, en promedio, los hombres en la muestra tienen 11.2 años de escolaridad, mientras que las mujeres tienen 11.1. La columna `n_casos` nos informa sobre el número de personas en cada grupo.

**Pregunta 2 (más compleja):** ¿Cuál es el ingreso promedio del hogar (`ytotcorh`) en cada región, pero considerando solo a los jefes de hogar (`pco1 == 1`)? Ordena los resultados de mayor a menor.


```r
casen %>% # Volvemos a la base original porque necesitamos pco1
  mutate(region_factor = as_factor(region)) %>% # Creamos el factor de región
  filter(pco1 == 1) %>% # 1. Filtramos solo jefes de hogar
  group_by(region_factor) %>% # 2. Agrupamos por la región (factor)
  summarise(
    ingreso_hog_promedio = mean(ytotcorh, na.rm = TRUE) # 3. Calculamos la media
  ) %>%
  arrange(desc(ingreso_hog_promedio)) # 4. Ordenamos el resultado
```

```
## # A tibble: 16 × 2
##    region_factor                                     ingreso_hog_promedio
##    <fct>                                                            <dbl>
##  1 Región Metropolitana de Santiago                              1863656.
##  2 Región de Magallanes y de la Antártica Chilena                1603080.
##  3 Región de Aysén del Gral. Carlos Ibáñez del Campo             1550975.
##  4 Región de Antofagasta                                         1502477.
##  5 Región de Tarapacá                                            1334223.
##  6 Región de Arica y Parinacota                                  1273240.
##  7 Región de Valparaíso                                          1255938.
##  8 Región de Atacama                                             1226856.
##  9 Región del Libertador Gral. Bernardo O'Higgins                1214506.
## 10 Región de Coquimbo                                            1204635.
## 11 Región de Los Ríos                                            1203132.
## 12 Región de Los Lagos                                           1158288.
## 13 Región del Biobío                                             1140827.
## 14 Región del Maule                                              1000071.
## 15 Región de La Araucanía                                         996879.
## 16 Región de Ñuble                                                959050.
```
**Interpretación del output:** Esta tabla nos muestra un ranking de las regiones según el ingreso promedio de sus hogares. Podemos ver claramente que la Región Metropolitana tiene el promedio más alto, mientras que la Región de Ñuble tiene el más bajo, revelando patrones de desigualdad territorial.

## 5. Actividad de Desafío

Ahora te toca a ti. Usando la base `casen_procesada` y la gramática de `dplyr`, escribe el código necesario para responder la siguiente pregunta de investigación:

**Pregunta:** ¿Cuál es la **edad promedio** y el **promedio de años de escolaridad** para cada **región**? Asegúrate de que los resultados estén ordenados de mayor a menor años de escolaridad promedio.


```r
# 
```

## 6. Reflexiones Finales

En este práctico, hemos aprendido a usar la poderosa gramática de `dplyr` para manipular datos de forma eficiente y reproducible. Hemos visto que con unos pocos verbos (`select`, `filter`, `arrange`, `mutate`, `summarise`) y el operador `%>%`, podemos pasar de una base de datos cruda a una tabla de resultados que responde a nuestras preguntas de investigación.

En la próxima clase, profundizaremos en la creación de variables más complejas, como índices, usando `mutate()` y `case_when()`.
