---
title: "Análisis Categórico Bivariado: Tablas de Contingencia y Control"
linktitle: "13: Tablas de Contingencia y Control"
date: "2025-11-17"
menu:
  example:
    parent: Ejemplos
    weight: 13
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---

## 0. Objetivos del Práctico

El objetivo de este práctico es dominar las herramientas para analizar la relación entre dos variables categóricas. Al finalizar, serás capaz de:

*   **Construir e interpretar** tablas de contingencia ponderadas usando el paquete `janitor`.
*   **Aplicar** el procedimiento de análisis: calcular porcentajes en la dirección de la variable explicativa y comparar en la opuesta.
*   **Visualizar** relaciones categóricas bivariadas con `ggplot2`.
*   **Comparar** dos flujos de trabajo para la visualización: graficar desde datos tabulados vs. graficar directamente.
*   **Implementar** el análisis de control **estratificando** por una tercera variable.

## 1. Contexto y Preparación de Datos

### 1.1 Contexto del Práctico

Hoy nos sumergiremos en el **Caso 2 (Categórica ➞ Categórica)** del análisis bivariado. Nuestra herramienta principal será la **tabla de contingencia**, que nos permitirá analizar cómo la distribución de una variable categórica (Y) cambia a través de las categorías de otra (X). Utilizaremos la **Encuesta Nacional de Uso del Tiempo (ENUT) 2023** para explorar las desigualdades en la distribución del trabajo y el bienestar.

### 1.2 Preparación del Entorno y Datos

**Paso 1: Organiza tu entorno**
Como siempre, asegúrate de estar trabajando dentro de tu **Proyecto de RStudio** y de tener una carpeta `datos`.

**Paso 2: Descarga la Base de Datos**
Descarga la base de datos de la ENUT 2023 desde el siguiente enlace y guarda el archivo `.zip` en tu carpeta `datos`:
-   **Enlace de descarga:** [Base de datos ENUT 2023 (formato R)](https://www.ine.gob.cl/docs/default-source/uso-del-tiempo-tiempo-libre/bbdd/ii-enut/250403-ii-enut-bdd-r-v2.zip?sfvrsn=87682f16_7)

**Paso 3: Carga de Paquetes y Datos**

``` r
# Cargar paquetes
library(tidyverse)
library(haven)
library(rio) # Para importar datos fácilmente
library(janitor) # Para crear tablas de contingencia
```


``` r
# Carga Manual (Método recomendado)
enut <- rio::import("datos/250403-ii-enut-bdd-r-v2.zip", which = "250403-ii-enut-bdd-r-v2.RDS")
```


El siguiente bloque carga los datos automáticamente para la reproducibilidad del documento. **No necesitas ejecutarlo** si ya cargaste la base manualmente.

``` r
# Carga automática de datos
enut <- rio::import("https://www.ine.gob.cl/docs/default-source/uso-del-tiempo-tiempo-libre/bbdd/ii-enut/250403-ii-enut-bdd-r-v2.zip?sfvrsn=87682f16_7", which = "250403-ii-enut-bdd-r-v2.RDS")
```


### 1.3 Limpieza y Preparación de Variables

Para facilitar nuestro análisis, crearemos un único `dataframe` llamado `enut_practico` que contenga todas las variables que usaremos, ya limpias y transformadas a formato factorial.


``` r
enut_practico <- enut %>%
  # Nos quedamos solo con quienes contestaron el diario de tiempo
  filter(tiempo == 1) %>%
  mutate(
    # Crear variables factoriales para análisis
    sexo_factor = as_factor(sexo),
    p_tdnr_dt_factor = as_factor(p_tdnr_dt),
    p_tcnr_dt_factor = as_factor(p_tcnr_dt),
    cae_factor = as_factor(cae),
    bs2_factor = as_factor(bs2),
    nivel_educ_factor = fct_recode(as_factor(nivel_educ),
                                   "Básica (in)completa" = "Sin educación primaria o primaria incompleta",
                                   "Básica (in)completa" = "Primaria completa",
                                   "Media" = "Secundaria completa",
                                   "Técnica" = "Técnica o postsecundaria no terciaria",
                                   "Universitaria" = "Universitaria completa"),
    # Agrupamos la edad para el desafío final
    grupo_edad = case_when(
      edad <= 29 ~ "Jóvenes (12-29)",
      edad <= 59 ~ "Adultos (30-59)",
      edad >= 60 ~ "Adultos Mayores (60+)",
      TRUE ~ NA_character_
    )
  ) %>%
  # Filtramos para quedarnos solo con las categorías que nos interesan en algunas variables
  filter(
    cae_factor %in% c("Persona ocupada", "Persona desocupada", "Personas fuera de la fuerza de trabajo"),
    !is.na(grupo_edad)
  )
```

## 2. Creando Tablas de Contingencia con `janitor`

### 2.1 La Anatomía de una Tabla con `janitor`

`janitor` es un paquete del ecosistema `tidyverse` especializado en crear tablas de resumen de forma rápida y publicable. Su función principal es `tabyl()`, que podemos "adornar" con capas adicionales.

**Paso 1: `tabyl()` - La Tabla de Frecuencias Ponderadas**
`tabyl()` crea la tabla base. Le indicamos la variable de fila, la de columna y la variable de ponderación (`wt`).

**Paso 2: `adorn_percentages("col")` - Añadiendo Porcentajes**
Añade los porcentajes de columna, siguiendo el procedimiento visto en clase (calcular en la dirección de la variable explicativa).

**Paso 3: `adorn_pct_formatting()` y `adorn_ns()` - Formato de Publicación**
Formatea los porcentajes y añade los conteos (N) originales entre paréntesis.


``` r
# Flujo completo para crear una tabla de publicación
enut_practico %>%
  tabyl(cae_factor, sexo_factor, wt = fe_cut) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()
```

```
##                              cae_factor        Hombre         Mujer
##                      Menores de 15 años  0.0%     (0)  0.0%     (0)
##                         Persona ocupada 66.9% (7,551) 50.2% (8,068)
##                      Persona desocupada  4.3%   (480)  3.4%   (554)
##  Personas fuera de la fuerza de trabajo 28.9% (3,258) 46.4% (7,461)
##  Valor Perdido
##          - (0)
##          - (0)
##          - (0)
##          - (0)
```

### 2.2 Actividad Intercalada 1
**Pregunta:** Usando el flujo completo de `janitor`, crea e interpreta una tabla ponderada que muestre la relación entre el **nivel educacional (`nivel_educ_factor`)** (X) y la **condición de actividad (`cae_factor`)** (Y). ¿Qué nivel educativo presenta el mayor porcentaje de 'Personas Ocupadas'?


``` r
# Escribe aquí tu código para la Actividad 1
```

## 3. Visualizando Relaciones Categóricas

Hay dos formas principales de crear un gráfico de barras bivariado: graficar desde los datos crudos (más directo) o tabular primero y luego graficar (más control).

### 3.1 Flujo 1: Graficar Directamente (`position = "fill"`)

Usaremos `geom_bar(position = "fill")` para que `ggplot2` calcule automáticamente los porcentajes y cree un gráfico 100% apilado.


``` r
enut_practico %>%
  filter(!bs2_factor %in% c("No Aplica", "Valor Perdido")) %>%
  ggplot(aes(x = sexo_factor, fill = fct_rev(bs2_factor), weight = fe_cut)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Satisfacción con el Reparto de Tareas por Sexo",
    x = "Sexo", y = "Porcentaje de Respuestas", fill = "Nivel de Satisfacción"
  ) +
  theme_minimal()
```

<img src="/example/13-practico_files/figure-html/vis-direct-1.png" width="672" />

### 3.2 Flujo 2: Pre-tabular con `dplyr` y Graficar (`geom_col`)
Este método nos da más control y es útil para gráficos más complejos.


``` r
# Paso 1: Crear una tabla de resumen con los porcentajes
tabla_para_grafico <- enut_practico %>%
  filter(!bs2_factor %in% c("No Aplica", "Valor Perdido")) %>%
  count(sexo_factor, bs2_factor, wt = fe_cut) %>%
  group_by(sexo_factor) %>%
  mutate(porcentaje = n / sum(n))

# Paso 2: Usar geom_col() para graficar los valores pre-calculados
ggplot(tabla_para_grafico, aes(x = sexo_factor, y = porcentaje, fill = fct_rev(bs2_factor))) +
  geom_col() + # Usamos geom_col, no geom_bar
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Satisfacción con el Reparto de Tareas por Sexo",
    x = "Sexo", y = "Porcentaje de Respuestas", fill = "Nivel de Satisfacción"
  ) +
  theme_minimal()
```

<img src="/example/13-practico_files/figure-html/vis-tabular-1.png" width="672" />
**Conclusión:** Ambos flujos producen el mismo gráfico. El primero es más rápido, el segundo es más flexible.

### 3.3 Actividad Intercalada 2
**Pregunta:** Crea un gráfico de barras apiladas al 100% que visualice la relación de la **Actividad 1** (nivel educacional vs. condición de actividad). Elige cualquiera de los dos flujos de trabajo. ¿Qué patrón visual confirma lo que viste en la tabla?


``` r
# Escribe aquí tu código para la Actividad 2
```

## 4. Análisis de Control por Tercera Variable en R

### 4.1 Control en Tablas con `tabyl()` de tres vías
Para controlar por una tercera variable, simplemente la añadimos como un tercer argumento a `tabyl()`. Esto creará una *lista* de tablas, una para cada categoría de la variable de control.


``` r
# Analizamos la relación entre participación en cuidados (p_tcnr_dt) y condición de actividad (cae),
# controlando por sexo.
tablas_parciales <- enut_practico %>%
  tabyl(p_tcnr_dt_factor, cae_factor, sexo_factor, wt = fe_cut) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

# Vemos la tabla para Hombres
tablas_parciales$Hombre
```

```
##  p_tcnr_dt_factor Menores de 15 años Persona ocupada Persona desocupada
##                No              - (0)   66.4% (5,011)        70.4% (338)
##                Sí              - (0)   33.6% (2,540)        29.6% (142)
##     Valor Perdido              - (0)    0.0%     (0)         0.0%   (0)
##  Personas fuera de la fuerza de trabajo
##                           80.7% (2,628)
##                           19.3%   (630)
##                            0.0%     (0)
```

``` r
# Vemos la tabla para Mujeres
tablas_parciales$Mujer
```

```
##  p_tcnr_dt_factor Menores de 15 años Persona ocupada Persona desocupada
##                No              - (0)   53.2% (4,290)        51.6% (286)
##                Sí              - (0)   46.8% (3,778)        48.4% (268)
##     Valor Perdido              - (0)    0.0%     (0)         0.0%   (0)
##  Personas fuera de la fuerza de trabajo
##                           64.8% (4,832)
##                           35.2% (2,629)
##                            0.0%     (0)
```

### 4.2 Control en Gráficos con `facet_wrap()`
`facet_wrap()` es la contraparte visual de la tabla de tres vías, creando un panel para cada categoría de la variable de control.


``` r
enut_practico %>%
  ggplot(aes(x = cae_factor, fill = p_tcnr_dt_factor, weight = fe_cut)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Participación en Cuidados por Condición de Actividad, según Sexo",
    x = "Condición de Actividad", y = "Porcentaje", fill = "Participa en Cuidados"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ sexo_factor) # La variable de control define los paneles
```

<img src="/example/13-practico_files/figure-html/control-facet-1.png" width="672" />

## 5. Actividad de Desafío Final (Integradora)

**Pregunta de Investigación:** Se argumenta que la percepción de "falta de tiempo para el descanso" (`bs2`) está influenciada por la "doble jornada". Queremos investigar si la relación entre el **sexo** (X) y la **satisfacción con el tiempo de descanso** (Y) es en realidad un efecto de la **condición de actividad (`cae_factor`)** (Z).

1.  **Análisis Bivariado:** Crea una tabla de contingencia ponderada con porcentajes de columna para la relación entre `sexo_factor` (X) y `bs2_factor` (Y). Interprétala brevemente.
2.  **Análisis Estratificado:** Ahora, controla por `cae_factor`. Crea las tablas parciales (una para 'Persona ocupada', otra para 'Personas fuera de la fuerza de trabajo', etc.).
3.  **Conclusión:** Compara los porcentajes en las tablas parciales. ¿La brecha de género en la satisfacción con el descanso se mantiene, desaparece o cambia dentro de cada grupo de actividad? Escribe un párrafo de conclusión: ¿La relación original era espuria, o se trata de una interacción/especificación?


``` r
# 1. Código para el análisis bivariado


# 2. Código para el análisis estratificado


# 3. Párrafo de conclusión (como comentario)
```
