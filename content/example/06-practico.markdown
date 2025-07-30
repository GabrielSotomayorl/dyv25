---
title: "Análisis de regresión lineal múltiple II"
linktitle: "6: Análisis de regresión lineal múltiple II"
date: "2023-09-30"
menu:
  example:
    parent: Ejemplos
    weight: 6
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---



## 0. Objetivo del práctico

En este práctico aprenderemos a aplicar la **regresión lineal múltiple** en R, utilizando variables continuas, **dicotómicas y categóricas**, para entender cómo influencian una variable dependiente. También profundizaremos en el proceso de **parcialización** para controlar el efecto de las variables independientes y aislar el efecto de cada predictor.


## 1. Cargar los datos de la encuesta CASEN

Primero, cargamos los datos de la CASEN y seleccionamos las variables que nos interesan: los ingresos del trabajo, los años de escolaridad, la edad, el sexo y el nivel educativo.



```r
if (!require(pacman)) {
  install.packages("pacman")
}
```

```
## Loading required package: pacman
```

```r
pacman::p_load(haven, dplyr, texreg, corrplot)

temp <- tempfile() #Creamos un archivo temporal
download.file("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2022/Base%20de%20datos%20Casen%202022%20SPSS.sav.zip",temp) #descargamos los datos
casen <- haven::read_sav(unz(temp, "Base de datos Casen 2022 SPSS.sav")) #cargamos los datos
unlink(temp); remove(temp) #eliminamos el archivo temporal

casen2 <- casen %>% 
  select(esc, sexo, ytrabajocor, edad, educ) 
```

## 2. Relación entre los ingresos la escolaridad y la edad

Primero, analizamos cómo los años de escolaridad y la edad afectan los ingresos del trabajo. Empezamos con una exploración de las correlaciones entre estas variables y realizamos regresiones lineales simples y múltiples.


```r
# Seleccionamos las variables del modelo
variables_modelo <- casen2 %>% 
  select(ytrabajocor, esc, edad)

# Calculamos la matriz de correlación
matriz_correlacion <- cor(variables_modelo, use = "complete.obs")

# Generamos el corrplot
corrplot(matriz_correlacion, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, 
         cl.cex = 0.8, tl.cex = 0.8, mar = c(0,0,1,0),
         title = "Matriz de Correlación")
```

<img src="/example/06-practico_files/figure-html/unnamed-chunk-2-1.png" width="672" />

Ahora realizamos regresiones para analizar el efecto de la escolaridad y la edad sobre los ingresos, primero de forma separada y luego conjuntamente a fin de observar al diferencia en la relación de las variables al introducir controles.


```r
modelo1 <- lm(ytrabajocor ~ esc , data = casen2)
modelo2 <- lm(ytrabajocor ~ edad, data = casen2)
modelo3 <- lm(ytrabajocor ~ esc + edad, data = casen2)

htmlreg(list(modelo1,modelo2,modelo3))
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 1</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 2</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 3</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">(Intercept)</td>
<td style="padding-left: 5px;padding-right: 5px;">-272321.52<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">729366.06<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-742740.49<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(8405.83)</td>
<td style="padding-left: 5px;padding-right: 5px;">(8757.98)</td>
<td style="padding-left: 5px;padding-right: 5px;">(14303.97)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">esc</td>
<td style="padding-left: 5px;padding-right: 5px;">77114.51<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">88022.20<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(656.62)</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(704.29)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">edad</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">-1403.43<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">7681.28<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(189.32)</td>
<td style="padding-left: 5px;padding-right: 5px;">(189.88)</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.13</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.15</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Adj. R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.13</td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.15</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">88391</td>
<td style="padding-left: 5px;padding-right: 5px;">88976</td>
<td style="padding-left: 5px;padding-right: 5px;">88391</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="4"><sup>&#42;&#42;&#42;</sup>p &lt; 0.001; <sup>&#42;&#42;</sup>p &lt; 0.01; <sup>&#42;</sup>p &lt; 0.05</td>
</tr>
</tfoot>
</table>

```r
#screenreg(list(modelo1,modelo2,modelo3))
```
 

**Modelo 1 (Escolaridad):**
El coeficiente para escolaridad es 77,114. Esto significa que por cada año adicional de escolaridad, los ingresos del trabajo aumentan en promedio 77,114 unidades monetarias.


**Modelo 2 (Edad):**
El coeficiente para edad es -1,403. Esto sugiere que, por cada año adicional de edad, los ingresos disminuyen en promedio 1,403 unidades, lo que podría reflejar efectos de antigüedad o envejecimiento en el mercado laboral.

**Modelo 3 (Escolaridad y Edad):**

Al considerar ambos predictores juntos, el coeficiente de escolaridad aumenta a 88,022, lo que sugiere que parte del impacto de la escolaridad estaba siendo "ocultado" por la edad. El coeficiente de edad cambia a 7,681, lo que indica que, al controlar por la escolaridad, la edad tiene un efecto positivo en los ingresos.


## 3. Incluir predictores dicotómicos en el modelo


El sexo es una variable dicotómica en este modelo. Las variables dicótomicas deben ser codificadas como 0 y 1 para integrarlas apropiadamente al modelo, y poder interpretar el beta como la diferencia promedio entre los grupos, controlando por las demás variables del modelo. R hace automáticamente esto si la variable introducida es de tipo factor (o le decimos que la trate como tal con la función factor())


```r
modelo1 <- lm(ytrabajocor ~ esc , data = casen2)
modelo2 <- lm(ytrabajocor ~ factor(sexo), data = casen2)
modelo3 <- lm(ytrabajocor ~ esc + factor(sexo), data = casen2)

htmlreg(list(modelo1,modelo2,modelo3))
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 1</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 2</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 3</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">(Intercept)</td>
<td style="padding-left: 5px;padding-right: 5px;">-272321.52<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">741995.12<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-207194.28<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(8405.83)</td>
<td style="padding-left: 5px;padding-right: 5px;">(3648.82)</td>
<td style="padding-left: 5px;padding-right: 5px;">(8426.08)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">esc</td>
<td style="padding-left: 5px;padding-right: 5px;">77114.51<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">80356.85<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(656.62)</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(652.68)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">factor(sexo)2</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">-167442.22<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-235612.34<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(5477.45)</td>
<td style="padding-left: 5px;padding-right: 5px;">(5113.14)</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.13</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.16</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Adj. R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.13</td>
<td style="padding-left: 5px;padding-right: 5px;">0.01</td>
<td style="padding-left: 5px;padding-right: 5px;">0.16</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">88391</td>
<td style="padding-left: 5px;padding-right: 5px;">88976</td>
<td style="padding-left: 5px;padding-right: 5px;">88391</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="4"><sup>&#42;&#42;&#42;</sup>p &lt; 0.001; <sup>&#42;&#42;</sup>p &lt; 0.01; <sup>&#42;</sup>p &lt; 0.05</td>
</tr>
</tfoot>
</table>

```r
#screenreg(list(modelo1,modelo2,modelo3))
```

**Modelo 2 (Sexo):**
El coeficiente para sexo es -167,442. Esto significa que los hombres ganan en promedio 167,442 unidades más que las mujeres, cuando no se controla por ninguna otra variable.

**Modelo 3 (Escolaridad y Sexo):**
Al incluir tanto la escolaridad como el sexo, los hombres ganan 235,612 unidades más que las mujeres en promedio, lo que sugiere que la brecha salarial de género se mantiene incluso después de controlar por los años de escolaridad.


## 4. Inclusión de variables categóricas politómicas en RLM

Vamos a transformar la variable de nivel educativo en un conjunto de variables dummies para incluirlas en el modelo. Esto nos permitirá comparar distintos niveles educativos entre sí.


```r
casen2 <- casen2 %>% 
  mutate(educ_simple = case_when(
    educ %in% c(0, 1, 2, 3, 4) ~ 1,  # Sin educación o Básica
    educ %in% c(5, 6) ~ 2,           # Media completa o incompleta
    educ %in% c(7, 8) ~ 3,           # Técnico nivel superior
    educ %in% c(9, 10, 11, 12) ~ 4   # Profesional o Posgrado
  ))   %>%
  mutate(
    dummy_media_completa = ifelse(educ_simple == 2, 1, 0),
    dummy_superior_tecnica = ifelse(educ_simple == 3, 1, 0),
    dummy_superior_profesional = ifelse(educ_simple == 4, 1, 0)
  ) %>%
  mutate(educ_simple_factor = factor(educ_simple,
                                     levels = c(1, 2, 3, 4),
                                     labels = c("Menos que Media", "Media Completa", 
                                                "Superior Técnica", "Superior Profesional")))
```

| educ_simple_factor    | dummy_media_completa | dummy_superior_tecnica | dummy_superior_profesional |
|-----------------------|----------------------|------------------------|----------------------------|
| Menos que Media        | 0                    | 0                      | 0                          |
| Media Completa         | 1                    | 0                      | 0                          |
| Superior Técnica       | 0                    | 1                      | 0                          |
| Superior Profesional   | 0                    | 0                      | 1                          |




```r
modelo1 <- lm(ytrabajocor ~ edad, data = casen2)
modelo2 <- lm(ytrabajocor ~ factor(educ_simple_factor), data = casen2)
modelo2.2 <- lm(ytrabajocor ~ dummy_media_completa+ dummy_superior_tecnica + dummy_superior_profesional, data = casen2)
modelo3 <- lm(ytrabajocor ~ edad + factor(educ_simple_factor), data = casen2)

htmlreg(list(modelo1,modelo2,modelo2.2,modelo3), custom.coef.names = c("Intercepto", "Edad","Media completa (ref. menos que media)",
                                                             "Superior Técnica", "Superior Profesional" ,"Media completa (ref. menos que media)",
                                                             "Superior Técnica", "Superior Profesional"))
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 1</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 2</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 3</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 4</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Intercepto</td>
<td style="padding-left: 5px;padding-right: 5px;">729366.06<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">396916.32<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">396916.32<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">123364.01<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(8757.98)</td>
<td style="padding-left: 5px;padding-right: 5px;">(4767.94)</td>
<td style="padding-left: 5px;padding-right: 5px;">(4767.94)</td>
<td style="padding-left: 5px;padding-right: 5px;">(11171.52)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Edad</td>
<td style="padding-left: 5px;padding-right: 5px;">-1403.43<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">5178.45<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(189.32)</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(191.43)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Media completa (ref. menos que media)</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">128948.37<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">128948.37<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">185782.88<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(6428.87)</td>
<td style="padding-left: 5px;padding-right: 5px;">(6428.87)</td>
<td style="padding-left: 5px;padding-right: 5px;">(6738.36)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Superior Técnica</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">271608.14<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">271608.14<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">344560.44<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(8629.17)</td>
<td style="padding-left: 5px;padding-right: 5px;">(8629.17)</td>
<td style="padding-left: 5px;padding-right: 5px;">(9006.92)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Superior Profesional</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">797327.92<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">797327.92<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">868457.15<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(7042.71)</td>
<td style="padding-left: 5px;padding-right: 5px;">(7042.71)</td>
<td style="padding-left: 5px;padding-right: 5px;">(7490.44)</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.14</td>
<td style="padding-left: 5px;padding-right: 5px;">0.14</td>
<td style="padding-left: 5px;padding-right: 5px;">0.15</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Adj. R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.00</td>
<td style="padding-left: 5px;padding-right: 5px;">0.14</td>
<td style="padding-left: 5px;padding-right: 5px;">0.14</td>
<td style="padding-left: 5px;padding-right: 5px;">0.15</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">88976</td>
<td style="padding-left: 5px;padding-right: 5px;">88406</td>
<td style="padding-left: 5px;padding-right: 5px;">88406</td>
<td style="padding-left: 5px;padding-right: 5px;">88406</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="5"><sup>&#42;&#42;&#42;</sup>p &lt; 0.001; <sup>&#42;&#42;</sup>p &lt; 0.01; <sup>&#42;</sup>p &lt; 0.05</td>
</tr>
</tfoot>
</table>

### Interpretación de los coeficientes (Modelo 4)

1. **Intercepto**: 
   - El intercepto de **123,364** indica el ingreso promedio estimado para una persona con **menos que educación media** y cuya **edad es 0**. Aunque este valor no tiene un significado práctico directo (ya que la edad 0 no es realista), es necesario para el ajuste del modelo.

2. **Edad**:
   - El coeficiente para la edad es **5,178.45**. Esto indica que por cada año adicional de edad, los ingresos aumentan en promedio en **5,178.45** unidades, **controlando** por el nivel educativo. Es decir, si mantenemos constante el nivel educativo, las personas más viejas tienden a ganar más.

3. **Media Completa (referencia: menos que media)**:
   - El coeficiente de **185,782.88** sugiere que las personas que completaron la **educación media** ganan en promedio **185,782.88** unidades más que aquellas con **menos que educación media**, manteniendo constante la edad.

4. **Superior Técnica**:
   - El coeficiente de **344,560.44** indica que las personas con **educación técnica superior** ganan en promedio **344,560.44** unidades más que aquellas con **menos que media**, ajustando por edad.

5. **Superior Profesional**:
   - El coeficiente de **868,457.15** indica que las personas con **educación superior profesional** ganan en promedio **868,457.15** unidades más que aquellas con **menos que media**, manteniendo constante la edad.

### Interpretación de R² y R² ajustado

- **R² = 0.15**: Esto significa que el **15% de la variabilidad** en los ingresos del trabajo está siendo explicada por el modelo, que incluye la edad y el nivel educativo.
  
- **R² ajustado = 0.15**: El valor ajustado penaliza el número de predictores incluidos en el modelo, pero como es igual a R², indica que la inclusión de los predictores educativos y de edad es válida para explicar la variabilidad en los ingresos.

En resumen, el modelo sugiere que tanto la **edad** como los **niveles educativos superiores** están asociados con mayores ingresos, y estos efectos son moderadamente explicativos (15%) de la variabilidad en los ingresos laborales.


## 5. Proceso de parcialización
### ¿Qué es la parcialización?
Cuando tenemos varias variables independientes relacionadas entre sí, como edad y escolaridad, el efecto de cada una puede estar influenciado por la otra. La parcialización consiste en ajustar una variable predictora eliminando el efecto de las otras.

### Crear residuos parciales
Podemos ilustrar el proceso de parcialización al ajustar cada variable por las otras y obtener sus residuos.

```r
casen2p<-casen2 %>% 
  select(ytrabajocor, edad, esc) %>% 
  filter(!is.na(ytrabajocor) & !is.na(esc) & !is.na(edad))

mod_esc <- lm(esc ~ edad, data = casen2p)
mod_edad <- lm(edad ~ esc, data = casen2p)

casen2pp<-data.frame(casen2p,  mod_esc$residuals, mod_edad$residuals)
head(casen2pp)
```

```
##   ytrabajocor edad esc mod_esc.residuals mod_edad.residuals
## 1      411242   40  15         2.3994976         0.05827002
## 2      590000   64   5        -5.1232380         9.85793176
## 3      520000   34  12        -1.2198185       -10.20183146
## 4      450000   30  12        -1.6326959       -14.20183146
## 5      160000   68  10         0.2896394        20.95810089
## 6      580000   56   8        -2.9489928         6.11803324
```

Ahora obtenemos la regresión lineal múltiple y las regresiones lineales pero utilizando las variables parcializadas, es decir, los residuos de la construcción de los modelos anteriores.
 

```r
htmlreg(list(lm(ytrabajocor ~ mod_esc.residuals  , data = casen2pp),
             lm(ytrabajocor ~ mod_edad.residuals  , data = casen2pp),
             lm(ytrabajocor ~  esc + edad , data = casen2p)))
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 1</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 2</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 3</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">(Intercept)</td>
<td style="padding-left: 5px;padding-right: 5px;">668123.79<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">668123.79<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">-742740.49<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(2533.54)</td>
<td style="padding-left: 5px;padding-right: 5px;">(2726.46)</td>
<td style="padding-left: 5px;padding-right: 5px;">(14303.97)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">mod_esc.residuals</td>
<td style="padding-left: 5px;padding-right: 5px;">88022.20<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(704.54)</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">mod_edad.residuals</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">7681.28<sup>&#42;&#42;&#42;</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(204.41)</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">esc</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">88022.20<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(704.29)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">edad</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">7681.28<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(189.88)</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.15</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">0.15</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Adj. R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.15</td>
<td style="padding-left: 5px;padding-right: 5px;">0.02</td>
<td style="padding-left: 5px;padding-right: 5px;">0.15</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">88391</td>
<td style="padding-left: 5px;padding-right: 5px;">88391</td>
<td style="padding-left: 5px;padding-right: 5px;">88391</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="4"><sup>&#42;&#42;&#42;</sup>p &lt; 0.001; <sup>&#42;&#42;</sup>p &lt; 0.01; <sup>&#42;</sup>p &lt; 0.05</td>
</tr>
</tfoot>
</table>
 
