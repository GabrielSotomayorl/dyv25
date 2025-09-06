---
title: "Exploración Inicial de Datos en R: Una Radiografía a la Encuesta CASEN 2022"
linktitle: "5: La Estructura de los Datos en R"
date: "2025-09-08"
menu:
  example:
    parent: Ejemplos
    weight: 5
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---

## 0. Objetivos del Práctico

El propósito de este práctico es aplicar los conceptos de la clase teórica en un entorno de R real. Al finalizar, serás capaz de:

*   Establecer un flujo de trabajo reproducible usando un Proyecto de RStudio.
*   Importar una base de datos en formato SPSS (`.sav`) usando el paquete `haven`.
*   Realizar un "diagnóstico inicial" sistemático de una base de datos, interpretando los resultados de las funciones de exploración.
*   Manejar la gran cantidad de variables de una encuesta real mediante la **subselección de datos**.
*   Utilizar la función `haven::as_factor()` para trabajar con las etiquetas de las variables.
*   Explorar variables individuales, tanto categóricas como numéricas.

## 1. Contexto: ¿Qué es la Encuesta CASEN?

La Encuesta de Caracterización Socioeconómica Nacional (CASEN) es el principal instrumento de medición socioeconómica en Chile, a cargo del Ministerio de Desarrollo Social y Familia. Su objetivo es conocer la situación de los hogares en áreas como pobreza, ingresos, educación, salud, trabajo y vivienda.

Los datos de CASEN son fundamentales para el diseño y evaluación de las políticas sociales del país. En este práctico, daremos nuestros primeros pasos para analizar esta rica fuente de información.

## 2. Preparación del Entorno de Trabajo

### 2.1 Proyectos de RStudio y Rutas Relativas

La base de un trabajo de análisis reproducible es la organización. La mejor forma de hacerlo es usando **Proyectos de RStudio (`.Rproj`)**. Un proyecto agrupa todos los archivos de una investigación (datos, scripts, informes) en una sola carpeta, y establece la raíz del directorio de trabajo en esa carpeta.

Esto nos permite usar **rutas relativas** (ej. `"datos/casen.sav"`) en lugar de rutas absolutas (ej. `"C:/Users/Gabriel/Documentos/Curso/datos/casen.sav"`), lo que asegura que nuestro código funcione en cualquier computador.

**Acción:** Antes de continuar, asegúrate de haber creado un Proyecto de RStudio para esta clase.

### 2.2 Paquetes: Ampliando las capacidades de R

R base tiene muchas funciones, pero su verdadero poder reside en los **paquetes**: colecciones de funciones, datos y documentación creadas por la comunidad de usuarios.

**Funcionamiento de los paquetes:**

1.  **Se instalan UNA SOLA VEZ** con `install.packages("nombre_del_paquete")`.
2.  **Se cargan CADA VEZ que inicias una nueva sesión de R** con `library(nombre_del_paquete)`.

Para este práctico, usaremos `haven` para importar datos y `tidyverse` para buenas prácticas generales.


```r
# Cargar paquetes necesarios para la sesión
# Si no los tienes, debes instalarlos primero ejecutando en la consola:
# install.packages("haven")
# install.packages("tidyverse")

library(haven)
library(tidyverse)
```

### 2.3 Importación de la Base de Datos CASEN 2022

**Forma 1: El método recomendado**

Este es el método estándar para trabajar. Implica descargar el archivo y guardarlo localmente en tu proyecto.

1.  Descarga la base de datos desde el sitio del [Observatorio Social del Ministerio de Desarrollo Social y Familia](https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2022). Necesitarás la base en formato SPSS (`.sav`).
2.  Descomprime el archivo `.zip` y guarda el archivo `.sav` en una carpeta dentro de tu Proyecto de RStudio (ej. una carpeta llamada `datos`).
3.  Carga los datos usando la función `read_sav()` del paquete `haven` y una ruta relativa.


```r
# Ejemplo de cómo cargarías el archivo si lo guardaste en una carpeta 'datos'
casen <- haven::read_sav("datos/Base de datos Casen 2022 SPSS_18 marzo 2024.sav")
```

**Forma 2: Un método automático**

Para fines de reproducibilidad total (por ejemplo, para que este sitio web funcione), a veces se utiliza un código que descarga y carga los datos automáticamente. No es necesario que lo uses para tus tareas, pero es una buena práctica conocerlo.


```r
# Nota: Este código es necesario para el funcionamiento del sitio del curso.
# Para tu trabajo, recomendamos usar la Forma 1.
temp <- tempfile() 
download.file("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2022/Base%20de%20datos%20Casen%202022%20SPSS.sav.zip", temp)
casen <- haven::read_sav(unz(temp, "Base de datos Casen 2022 SPSS.sav"))
unlink(temp); remove(temp)
```

## 3. Primer Contacto y Subselección de Datos

### 3.1 El Problema: ¡Demasiadas Variables!

Veamos las dimensiones de nuestra base de datos recién cargada.


```r
dim(casen)
```

```
## [1] 202231    917
```

Como pueden ver, la CASEN 2022 tiene **202,231 casos (filas)** y **917 variables (columnas)**. Trabajar con una base de datos tan grande es ineficiente y confuso. El primer paso en cualquier análisis real es **seleccionar solo las variables que necesitamos**.

### 3.2 Subselección con R Base

Usaremos la sintaxis de corchetes de R base `[filas, columnas]` para crear un nuevo `data.frame` más pequeño y manejable. Dejamos el espacio de las filas en blanco para seleccionar **TODAS** las filas, y en el espacio de las columnas, le damos un vector con los nombres de las variables que queremos conservar.


```r
# Creamos un vector con los nombres de las variables de interés
variables_seleccionadas <- c("region", "sexo", "edad", "esc", "ind_hacina", "ytotcorh")

# Creamos un nuevo objeto 'casen_sub' con solo esas columnas
casen_sub <- casen[, variables_seleccionadas]

# Verificamos las dimensiones de nuestra nueva base de datos
dim(casen_sub)
```

```
## [1] 202231      6
```

¡Mucho mejor! Ahora trabajaremos con este objeto `casen_sub` que es más liviano y enfocado.

## 4. La "Radiografía" Inicial de Nuestros Datos

Ahora que tenemos un conjunto de datos manejable, apliquemos el protocolo de exploración.

### 4.1 Estructura y el Problema de las Etiquetas (`<dbl+lbl>`)

Usemos `str()` para ver la estructura de nuestro nuevo objeto.


```r
str(casen_sub)
```

```
## tibble [202,231 × 6] (S3: tbl_df/tbl/data.frame)
##  $ region    : dbl+lbl [1:202231] 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,...
##    ..@ label      : chr "Región"
##    ..@ format.spss: chr "F2.0"
##    ..@ labels     : Named num [1:16] 1 2 3 4 5 6 7 8 9 10 ...
##    .. ..- attr(*, "names")= chr [1:16] "Región de Tarapacá" "Región de Antofagasta" "Región de Atacama" "Región de Coquimbo" ...
##  $ sexo      : dbl+lbl [1:202231] 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 2, 2, 1, 2, 2, 2, ...
##    ..@ label        : chr "Sexo"
##    ..@ format.spss  : chr "F1.0"
##    ..@ display_width: int 6
##    ..@ labels       : Named num [1:2] 1 2
##    .. ..- attr(*, "names")= chr [1:2] "1. Hombre" "2. Mujer"
##  $ edad      : num [1:202231] 72 67 40 56 25 2 60 84 67 30 ...
##   ..- attr(*, "label")= chr "Edad"
##   ..- attr(*, "format.spss")= chr "F3.0"
##   ..- attr(*, "display_width")= int 6
##  $ esc       : num [1:202231] 1 4 15 NA 12 NA 10 4 8 12 ...
##   ..- attr(*, "label")= chr "Años de escolaridad (edad >= 15)"
##   ..- attr(*, "format.spss")= chr "F2.0"
##   ..- attr(*, "display_width")= int 5
##  $ ind_hacina: dbl+lbl [1:202231] 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
##    ..@ label        : chr "Índice de hacinamiento"
##    ..@ format.spss  : chr "F3.0"
##    ..@ display_width: int 12
##    ..@ labels       : Named num [1:5] -88 1 2 3 4
##    .. ..- attr(*, "names")= chr [1:5] "No sabe" "Sin hacinamiento (menos de 2,5 personas por dormitorio)" "Hacinamiento medio (entre 2,5 y 3,49 personas por dormitorio)" "Hacinamiento alto (entre 3,5 y 4,99 personas por dormitorio)" ...
##  $ ytotcorh  : num [1:202231] 1010894 1010894 1010894 418192 418192 ...
##   ..- attr(*, "label")= chr "Ingreso total corregido del hogar"
##   ..- attr(*, "format.spss")= chr "F8.0"
##   ..- attr(*, "display_width")= int 10
```

**Interpretación:** `haven` es inteligente. Ha importado las variables numéricas de SPSS (ej. `sexo` donde 1="Hombre", 2="Mujer") y también ha guardado las **etiquetas de texto** asociadas. Por eso vemos `dbl+lbl` o `haven_labelled`. Por defecto, R las trata como números, pero guarda las etiquetas. Para que nuestro análisis sea más claro, necesitamos "activar" esas etiquetas.

### 4.2 Trabajando con Etiquetas: `haven::as_factor()`

La función `as_factor()` del paquete `haven` convierte estas variables etiquetadas en **factores**, que es el tipo de dato que R usa para variables categóricas. Esta función reemplaza los números por sus correspondientes etiquetas de texto.


```r
# Convirtamos la variable de hacinamiento a factor para ver sus etiquetas
casen_sub$hacinamiento_factor <- as_factor(casen_sub$ind_hacina)

# Ahora comparemos los tipos de dato
class(casen_sub$ind_hacina)
```

```
## [1] "haven_labelled" "vctrs_vctr"     "double"
```

```r
class(casen_sub$hacinamiento_factor)
```

```
## [1] "factor"
```

### 4.3 Resumen General y Detección de Problemas

`summary()` nos da un resumen estadístico de cada variable, ideal para un control de calidad.


```r
summary(casen_sub)
```

```
##      region            sexo            edad             esc       
##  Min.   : 1.000   Min.   :1.000   Min.   :  0.00   Min.   : 0.00  
##  1st Qu.: 5.000   1st Qu.:1.000   1st Qu.: 20.00   1st Qu.: 8.00  
##  Median : 8.000   Median :2.000   Median : 38.00   Median :12.00  
##  Mean   : 8.761   Mean   :1.527   Mean   : 39.32   Mean   :11.17  
##  3rd Qu.:13.000   3rd Qu.:2.000   3rd Qu.: 58.00   3rd Qu.:14.00  
##  Max.   :16.000   Max.   :2.000   Max.   :120.00   Max.   :29.00  
##                                                    NA's   :36983  
##    ind_hacina         ytotcorh       
##  Min.   :-88.000   Min.   :       0  
##  1st Qu.:  1.000   1st Qu.:  750000  
##  Median :  1.000   Median : 1121667  
##  Mean   :  1.014   Mean   : 1476989  
##  3rd Qu.:  1.000   3rd Qu.: 1728370  
##  Max.   :  4.000   Max.   :77300000  
##                    NA's   :120       
##                                                                                       hacinamiento_factor
##  No sabe                                                                                        :   150  
##  Sin hacinamiento (menos de 2,5 personas por dormitorio)                                        :189513  
##  Hacinamiento medio (entre 2,5 y 3,49 personas por dormitorio)                                  :  9682  
##  Hacinamiento alto (entre 3,5 y 4,99 personas por dormitorio)                                   :  2079  
##  Hacinamiento crítico (5 y más personas por dormitorio u hogar sin dormitorios de uso exclusivo):   807  
##                                                                                                          
## 
```

**Actividad de Interpretación:** Examina el `summary`. ¿Detectas algún valor que te parezca extraño o problemático?
*   En `ind_hacina`, vemos un `Min.` de -88. Esto es un código para "No sabe", no un valor real.
*   En `esc`, hay 36,983 `NA's` (valores perdidos). Esto es esperable, ya que la pregunta solo aplica a personas mayores de cierta edad.
*   En `ytotcorh`, el `Max.` es muy alto. Esto sugiere una distribución de ingresos muy asimétrica (desigual).

## 5. Explorando Variables Individuales

### 5.1 Variable Categórica: Hacinamiento (`ind_hacina`)

Veamos la diferencia al tabular la variable numérica original versus la versión convertida a factor.


```r
# Tabla con la variable numérica (menos informativa, solo vemos códigos)
table(casen_sub$ind_hacina)
```

```
## 
##    -88      1      2      3      4 
##    150 189513   9682   2079    807
```

```r
# Tabla con la variable convertida a factor (mucho más clara, vemos las categorías)
table(casen_sub$hacinamiento_factor)
```

```
## 
##                                                                                         No sabe 
##                                                                                             150 
##                                         Sin hacinamiento (menos de 2,5 personas por dormitorio) 
##                                                                                          189513 
##                                   Hacinamiento medio (entre 2,5 y 3,49 personas por dormitorio) 
##                                                                                            9682 
##                                    Hacinamiento alto (entre 3,5 y 4,99 personas por dormitorio) 
##                                                                                            2079 
## Hacinamiento crítico (5 y más personas por dormitorio u hogar sin dormitorios de uso exclusivo) 
##                                                                                             807
```

Para obtener los porcentajes, usamos `prop.table()`.


```r
# Calculamos la proporción y multiplicamos por 100 para obtener el porcentaje
prop.table(table(casen_sub$hacinamiento_factor)) * 100
```

```
## 
##                                                                                         No sabe 
##                                                                                       0.0741726 
##                                         Sin hacinamiento (menos de 2,5 personas por dormitorio) 
##                                                                                      93.7111521 
##                                   Hacinamiento medio (entre 2,5 y 3,49 personas por dormitorio) 
##                                                                                       4.7875944 
##                                    Hacinamiento alto (entre 3,5 y 4,99 personas por dormitorio) 
##                                                                                       1.0280323 
## Hacinamiento crítico (5 y más personas por dormitorio u hogar sin dormitorios de uso exclusivo) 
##                                                                                       0.3990486
```

**Actividad de Interpretación:** Basado en la tabla de frecuencias porcentuales, ¿qué porcentaje de los hogares en la muestra vive en condiciones de hacinamiento (sumando las categorías de hacinamiento medio, alto y crítico)?

### 5.2 Variable Numérica: Edad (`edad`)

Para las variables numéricas, nos interesan las medidas de tendencia central y dispersión. El principal desafío son los valores perdidos (`NA`).


```r
# La variable edad en CASEN no tiene valores perdidos, por lo que podemos calcularla directamente
mean(casen_sub$edad)
```

```
## [1] 39.32292
```

```r
# Sin embargo, la buena práctica es usar siempre na.rm = TRUE
# por si trabajamos con otras variables que sí tengan NAs.
edad_promedio <- mean(casen_sub$edad, na.rm = TRUE)
desv_est_edad <- sd(casen_sub$edad, na.rm = TRUE)

# Imprimimos los resultados formateados
print(paste("La edad promedio de la muestra es:", round(edad_promedio, 1), "años"))
```

```
## [1] "La edad promedio de la muestra es: 39.3 años"
```

```r
print(paste("La desviación estándar de la edad es:", round(desv_est_edad, 1), "años"))
```

```
## [1] "La desviación estándar de la edad es: 23 años"
```

**Actividad Aplicada (Desafío):** Calcula e interpreta el promedio de años de escolaridad (`esc`) y el ingreso total del hogar (`ytotcorh`). No olvides usar `na.rm = TRUE`. ¿Qué te llama la atención de estos valores?

## 6. Reflexiones Finales y Próximos Pasos

**Resumen del Práctico:** En esta sesión, realizamos un diagnóstico completo de una subselección de variables de la Encuesta CASEN. Aprendimos a importar datos con `haven`, a subseleccionar variables, a interpretar la estructura de los datos, a convertir variables etiquetadas en factores con `as_factor()`, y a calcular nuestras primeras estadísticas descriptivas. Este protocolo es el primer paso fundamental en cualquier análisis de datos.

**Conexión con la Próxima Clase:** Hemos visto que para responder preguntas más complejas (ej. ‘¿Cuál es el ingreso promedio *por región*?’) necesitamos herramientas más potentes para manipular y agrupar los datos. Eso es exactamente lo que veremos en la próxima clase, cuando aprendamos la “gramática de la manipulación de datos” con `dplyr`.
