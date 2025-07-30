---
title: "Análisis de conglomerados no jerárquicos"
linktitle: "12: Análisis de conglomerados no jerárquicos"
date: "2024-11-04"
menu:
  example:
    parent: Ejemplos
    weight: 12
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---
<script src="/rmarkdown-libs/kePrint/kePrint.js"></script>
<link href="/rmarkdown-libs/lightable/lightable.css" rel="stylesheet" />

## 0. Objetivo del Práctico

El objetivo de este práctico es realizar un análisis de conglomerados no jerárquicos sobre una serie de opiniones ideológicas de los individuos encuestados en la sexta ola del estudio ELSOC. Utilizaremos el método de k-medias para identificar grupos que compartan perfiles ideológicos similares con base en una serie de preguntas sobre temas sociales y políticos. Al final del ejercicio, los estudiantes deberían comprender cómo esta técnica permite descubrir agrupamientos significativos dentro de los datos, permitiendo caracterizar diferentes perfiles ideológicos sin necesidad de una hipótesis a priori.


## 1. Carga y Preparación de los Datos


``` r
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr,factoextra , corrplot, knitr, kableExtra)

temp <- tempfile()
download.file("https://github.com/GabrielSotomayorl/aadi2024/raw/refs/heads/main/content/example/input/data/ELSOC_Long_2016_2022_v1.00.RData", temp)
load(temp)
unlink(temp) 

elsoc<- elsoc_long_2016_2022.2 %>% 
  filter(ola == 6)  %>%
  mutate(across(c(c37_01:c37_09), ~ ifelse(. < 0, NA, .))) %>% 
  filter(!(is.na(c37_01)|is.na(c37_02)|is.na(c37_03)|is.na(c37_04)|is.na(c37_05)|is.na(c37_06)|is.na(c37_07)|is.na(c37_08)))


medias <- elsoc %>% 
  summarise(across(c(c37_01:c37_08), mean, na.rm = TRUE))

preg<- c(
    "Las parejas homosexuales deberían poder adoptar hijos",
    "El aborto debe ser legal bajo cualquier circunstancia",
    "El Estado de Chile, más que los privados, debería ser el principal proveedor de educación",
    "Cada persona debiera asegurarse por sí misma su futura pensión para la tercera edad",
    "Chile debería tomar medidas más drásticas para impedir el ingreso de inmigrantes al país",
    "La educación sexual de los niños debería ser responsabilidad exclusiva de los padres",
    "Se deberían clausurar empresas contaminantes, incluso si esto implica un aumento en el desempleo",
    "El gasto social debe destinarse únicamente a los más pobres y vulnerables")

# Crear la tabla con los resultados y las descripciones
resultados <- data.frame(
  Descripción = preg, 
  Media = as.numeric(medias)
)

# Mostrar la tabla con kable
kable(resultados, caption = "Medias de Opiniones en Encuesta ELSOC", format = "html", align = "l", col.names = c("Descripción", "Media"))
```

<table>
<caption><span id="tab:unnamed-chunk-1"></span>Table 1: Medias de Opiniones en Encuesta ELSOC</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Descripción </th>
   <th style="text-align:left;"> Media </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Las parejas homosexuales deberían poder adoptar hijos </td>
   <td style="text-align:left;"> 3.203502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> El aborto debe ser legal bajo cualquier circunstancia </td>
   <td style="text-align:left;"> 2.684179 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> El Estado de Chile, más que los privados, debería ser el principal proveedor de educación </td>
   <td style="text-align:left;"> 4.074879 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cada persona debiera asegurarse por sí misma su futura pensión para la tercera edad </td>
   <td style="text-align:left;"> 3.049517 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Chile debería tomar medidas más drásticas para impedir el ingreso de inmigrantes al país </td>
   <td style="text-align:left;"> 4.186594 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> La educación sexual de los niños debería ser responsabilidad exclusiva de los padres </td>
   <td style="text-align:left;"> 3.184179 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Se deberían clausurar empresas contaminantes, incluso si esto implica un aumento en el desempleo </td>
   <td style="text-align:left;"> 3.681763 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> El gasto social debe destinarse únicamente a los más pobres y vulnerables </td>
   <td style="text-align:left;"> 3.170290 </td>
  </tr>
</tbody>
</table>

Las variables corresponden a escalas de acuerdo (¿Cuán de acuerdo o en desacuerdo está usted con cada una de las siguientes afirmaciones?) donde 1 inidca menor acuerdo y 5 mayor acuerdo. 

##  2. Análisis de Correlación entre Variables

Antes de proceder al análisis de conglomerados, es importante entender las relaciones entre nuestras variables mediante un análisis de correlación.


``` r
# Calcular la matriz de correlación
corr_matrix <- cor(elsoc %>% select(c37_01:c37_08), use = "complete.obs")

# Visualizar la matriz de correlación con números y colores
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8, title = "Matriz de Correlación", addCoef.col = "black", number.cex = 0.7)
```

<img src="/example/12-practico_files/figure-html/correlation-plot-1.png" width="672" />

## 3. Estandarización de las Variables

Para asegurar que todas las variables contribuyan equitativamente al análisis, es fundamental estandarizarlas, cuando las variables tienen diferentes rangos. En este caso, dado que todas tienen la misma escala optaremos por no estandarizar. 


``` r
# Estandarizar las variables
elsoc_vars <- elsoc %>%
  select(c37_01:c37_08)  #%>%
  #scale() 

# Verificar los datos estandarizados
head(elsoc_vars)
```

```
##   c37_01 c37_02 c37_03 c37_04 c37_05 c37_06 c37_07 c37_08
## 1      4      2      5      2      5      2      4      4
## 2      3      2      5      4      5      4      3      4
## 3      1      4      5      4      5      2      4      4
## 4      3      1      4      2      5      2      4      2
## 5      1      5      5      4      5      1      4      4
## 6      3      4      3      4      4      2      4      3
```

## 4. Determinación del Número Óptimo de Conglomerados

Para determinar el número óptimo de conglomerados, utilizaremos el método del codo.


``` r
# Método del codo para determinar el número óptimo de conglomerados
fviz_nbclust(elsoc_vars, kmeans, method = "wss") +
  labs(title = "Determinación del Número Óptimo de Conglomerados - Método del Codo")
```

<img src="/example/12-practico_files/figure-html/optimal-clusters-1.png" width="672" />


## 5. Análisis de Conglomerados k-medias

El siguiente paso consiste en realizar el análisis de conglomerados utilizando el método de k-medias para identificar los perfiles ideológicos. Considerando la información del método del codo haremos un análisis con 4 conglomerados. 


``` r
set.seed(123)  # Para reproducibilidad

# Aplicar k-medias con 3 conglomerados
kmeans_result <- kmeans(elsoc_vars, centers = 4, nstart = 25)

# Agregar los conglomerados al dataframe original
elsoc$conglomerado <- kmeans_result$cluster

# Ver la asignación de los primeros registros
head(elsoc %>% select(c37_01:c37_08, conglomerado))
```

```
##   c37_01 c37_02 c37_03 c37_04 c37_05 c37_06 c37_07 c37_08 conglomerado
## 1      4      2      5      2      5      2      4      4            1
## 2      3      2      5      4      5      4      3      4            2
## 3      1      4      5      4      5      2      4      4            4
## 4      3      1      4      2      5      2      4      2            1
## 5      1      5      5      4      5      1      4      4            3
## 6      3      4      3      4      4      2      4      3            3
```


## 6. Caracterización de los Conglomerados

Ahora caracterizaremos los conglomerados obtenidos, calculando los promedios de cada variable por conglomerado para identificar los perfiles ideológicos.


``` r
caracterizacion <- elsoc %>%
  group_by(conglomerado) %>%
  summarise(
    across(c(c37_01:c37_08), mean, na.rm = TRUE),
    cantidad_individuos = n()
  )

# Crear un tibble para presentar los resultados con nombres más claros
caracterizacion_tidy <- caracterizacion %>%
  rename(
    "Conglomerado" = conglomerado,
    "Adopción Homoparental" = c37_01,
    "Aborto" = c37_02,
    "Rol del Estado en Educación" = c37_03,
    "Capitalización Individual Pensiones" = c37_04,
    "Restricciones Migratorias" = c37_05,
    "Educación Sexual (Padres)" = c37_06,
    "Restricción a Empresas Contaminantes" = c37_07,
    "Gasto Social Focalizado" = c37_08,
    "Cantidad de Individuos" = cantidad_individuos
  )

# Mostrar la caracterización con una tabla kable bonita
caracterizacion_tidy %>%
  kbl(caption = "Caracterización de los Conglomerados Ideológicos") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption><span id="tab:characterize-clusters"></span>Table 2: Caracterización de los Conglomerados Ideológicos</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Conglomerado </th>
   <th style="text-align:right;"> Adopción Homoparental </th>
   <th style="text-align:right;"> Aborto </th>
   <th style="text-align:right;"> Rol del Estado en Educación </th>
   <th style="text-align:right;"> Capitalización Individual Pensiones </th>
   <th style="text-align:right;"> Restricciones Migratorias </th>
   <th style="text-align:right;"> Educación Sexual (Padres) </th>
   <th style="text-align:right;"> Restricción a Empresas Contaminantes </th>
   <th style="text-align:right;"> Gasto Social Focalizado </th>
   <th style="text-align:right;"> Cantidad de Individuos </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3.365796 </td>
   <td style="text-align:right;"> 2.000000 </td>
   <td style="text-align:right;"> 4.004751 </td>
   <td style="text-align:right;"> 2.546318 </td>
   <td style="text-align:right;"> 4.237530 </td>
   <td style="text-align:right;"> 2.382423 </td>
   <td style="text-align:right;"> 3.543943 </td>
   <td style="text-align:right;"> 2.703088 </td>
   <td style="text-align:right;"> 421 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3.893424 </td>
   <td style="text-align:right;"> 2.909297 </td>
   <td style="text-align:right;"> 4.074830 </td>
   <td style="text-align:right;"> 3.682540 </td>
   <td style="text-align:right;"> 4.281179 </td>
   <td style="text-align:right;"> 3.977324 </td>
   <td style="text-align:right;"> 3.782313 </td>
   <td style="text-align:right;"> 3.698413 </td>
   <td style="text-align:right;"> 441 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3.920981 </td>
   <td style="text-align:right;"> 4.152589 </td>
   <td style="text-align:right;"> 4.305177 </td>
   <td style="text-align:right;"> 2.629428 </td>
   <td style="text-align:right;"> 3.901907 </td>
   <td style="text-align:right;"> 2.258856 </td>
   <td style="text-align:right;"> 3.822888 </td>
   <td style="text-align:right;"> 2.795640 </td>
   <td style="text-align:right;"> 367 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.714286 </td>
   <td style="text-align:right;"> 1.864169 </td>
   <td style="text-align:right;"> 3.946136 </td>
   <td style="text-align:right;"> 3.252927 </td>
   <td style="text-align:right;"> 4.283372 </td>
   <td style="text-align:right;"> 3.950820 </td>
   <td style="text-align:right;"> 3.592506 </td>
   <td style="text-align:right;"> 3.407494 </td>
   <td style="text-align:right;"> 427 </td>
  </tr>
</tbody>
</table>

**Conglomerado 1**: 
Este grupo tiene un perfil ideológico más conservador en aspectos como la **adopción homoparental** (promedio bajo de 3.37) y el **aborto** (2.00), lo cual sugiere una postura más tradicional en temas sociales. De igual forma, muestran un apoyo alto a la **capitalización individual de pensiones** (4.00). Este grupo está compuesto por **421 individuos**.

**Conglomerado 2**:
Este conglomerado se caracteriza por posturas moderadamente favorables hacia la **adopción homoparental** (3.89) y el **aborto** (2.91). Tienen un perfil más intervencionista en temas del **rol del Estado en la educación** (4.07) y **restricciones migratorias** (4.28). Este grupo incluye **441 individuos**, lo cual sugiere que representa un perfil relativamente común en la muestra.

**Conglomerado 3**:
El tercer grupo tiene posturas más progresistas, con altos niveles de acuerdo en el **aborto** (4.15) y en el **rol del Estado en la educación** (4.31). Del mismo modo apoyan menos la **capitalización individual de pensiones** (2.63). En cuanto a **restricciones migratorias** (3.90), su postura es más favorable, pero menos extrema comparado con otros conglomerados. Este grupo contiene **367 individuos**.

**Conglomerado 4**:
Este grupo presenta el perfil más conservador entre todos los conglomerados. Los individuos de este conglomerado tienen niveles bajos de acuerdo en **adopción homoparental** (1.71), **aborto** (1.86). Muestran una mayor aceptación hacia la **capitalización individual de pensiones** (3.25) y el **gasto social focalizado** (3.41), sugiriendo una postura conservadora y pro-mercado. Este grupo está compuesto por **427 individuos**.

## 7. Análisis de las variables más influyentes

En esta sección, realizamos un análisis de varianza (ANOVA) para cada una de las variables ideológicas con el fin de evaluar su influencia en la diferenciación de los conglomerados resultantes. El ANOVA nos permite determinar si existen diferencias significativas entre los grupos formados por el método de k-medias respecto a cada una de las opiniones encuestadas. De esta manera, podemos identificar cuáles variables juegan un papel clave en la definición de los perfiles ideológicos.

El análisis se realizó individualmente para cada una de las ocho variables, y se presenta la tabla ANOVA con el valor F y el valor p para cada una de ellas. Un valor F alto y un valor p menor a 0.05 indican que existen diferencias significativas entre los conglomerados para la variable analizada, lo cual significa que esa variable es útil para distinguir entre los diferentes perfiles ideológicos.


``` r
# ANOVA para la variable Adopción Homoparental
anova_adopcion <- aov(c37_01 ~ conglomerado, data = elsoc)
summary(anova_adopcion)
```

```
##                Df Sum Sq Mean Sq F value Pr(>F)    
## conglomerado    1  549.7   549.7   482.9 <2e-16 ***
## Residuals    1654 1882.8     1.1                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
# ANOVA para la variable Aborto
anova_aborto <- aov(c37_02 ~ conglomerado, data = elsoc)
summary(anova_aborto)
```

```
##                Df Sum Sq Mean Sq F value Pr(>F)  
## conglomerado    1    7.6   7.605    5.43 0.0199 *
## Residuals    1654 2316.2   1.400                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
# ANOVA para la variable Rol del Estado en Educación
anova_educacion <- aov(c37_03 ~ conglomerado, data = elsoc)
summary(anova_educacion)
```

```
##                Df Sum Sq Mean Sq F value Pr(>F)
## conglomerado    1      0  0.0080   0.012  0.913
## Residuals    1654   1109  0.6703
```

``` r
# ANOVA para la variable Capitalización Individual Pensiones
anova_pensiones <- aov(c37_04 ~ conglomerado, data = elsoc)
summary(anova_pensiones)
```

```
##                Df Sum Sq Mean Sq F value   Pr(>F)    
## conglomerado    1   25.4  25.380   20.41 6.69e-06 ***
## Residuals    1654 2056.6   1.243                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
# ANOVA para la variable Restricciones Migratorias
anova_migrantes <- aov(c37_05 ~ conglomerado, data = elsoc)
summary(anova_migrantes)
```

```
##                Df Sum Sq Mean Sq F value Pr(>F)
## conglomerado    1    0.9  0.8878    1.32  0.251
## Residuals    1654 1112.5  0.6726
```

``` r
# ANOVA para la variable Educación Sexual (Padres)
anova_sexual <- aov(c37_06 ~ conglomerado, data = elsoc)
summary(anova_sexual)
```

```
##                Df Sum Sq Mean Sq F value Pr(>F)    
## conglomerado    1  201.9  201.92   172.1 <2e-16 ***
## Residuals    1654 1940.9    1.17                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
# ANOVA para la variable Restricción a Empresas Contaminantes
anova_contaminacion <- aov(c37_07 ~ conglomerado, data = elsoc)
summary(anova_contaminacion)
```

```
##                Df Sum Sq Mean Sq F value Pr(>F)
## conglomerado    1    0.5  0.5348   0.738   0.39
## Residuals    1654 1198.8  0.7248
```

``` r
# ANOVA para la variable Gasto Social Focalizado
anova_gasto <- aov(c37_08 ~ conglomerado, data = elsoc)
summary(anova_gasto)
```

```
##                Df Sum Sq Mean Sq F value   Pr(>F)    
## conglomerado    1   32.5   32.48   29.99 5.01e-08 ***
## Residuals    1654 1791.5    1.08                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

e acuerdo con los resultados del ANOVA, las variables que muestran diferencias significativas entre los conglomerados son **Adopción Homoparental**, **Aborto**, **Capitalización Individual de Pensiones**, **Educación Sexual (Padres)** y **Gasto Social Focalizado**. Estas variables parecen jugar un papel clave en la diferenciación de los perfiles ideológicos dentro de la muestra. En cambio, las variables **Rol del Estado en Educación**, **Restricciones Migratorias**, y **Restricción a Empresas Contaminantes** no presentan diferencias significativas entre los conglomerados, lo que sugiere que estas opiniones no contribuyen de manera relevante a la definición de los grupos ideológicos identificados.

## 8. Visualización de Conglomerados

Finalmente, visualizaremos los conglomerados en un gráfico de dispersión utilizando dos componentes principales para entender mejor la formación de los grupos.


``` r
# Visualización de los conglomerados utilizando PCA
fviz_cluster(kmeans_result, data = elsoc_vars, geom = "point", main = "Visualización de Conglomerados - K-Means")
```

<img src="/example/12-practico_files/figure-html/visualize-clusters-1.png" width="672" />


## Gráfico interactivo de cluster

A continuación se presenta una herramienta que podrán usar para visualizar de mejor manera como se generan clusters a partir de k-medias.

Para acceder de mejor manera a la aplicación pueden acceder a este enlace: <https://gabriel-sotomayor.shinyapps.io/Cluster/>.

<iframe src="https://gabriel-sotomayor.shinyapps.io/Cluster/" 
        width="100%" 
        height="600px" 
        frameborder="0">

</iframe>

