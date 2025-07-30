---
title: "Análisis de regresión lineal múltiple III"
linktitle: "7: Análisis de regresión lineal múltiple III"
date: "2024-10-07"
menu:
  example:
    parent: Ejemplos
    weight: 7
type: docs
toc: true
editor_options: 
  chunk_output_type: console
---



## 0. Objetivo del práctico

En este práctico aprenderemos a aplicar la **regresión lineal múltiple** en R, utilizando variables continuas para entender cómo influencian una variable dependiente. Además, aprenderemos a calcular manualmente los **estadísticos de significancia** y el **R²** y **R² ajustado**, para profundizar en la comprensión del modelo y la interpretación de los coeficientes.

## 1. Cargar los datos de la encuesta CASEN

Primero, cargamos los datos de la CASEN y seleccionamos las variables que nos interesan: los ingresos del trabajo, los años de escolaridad y la edad.


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
  select(esc, ytrabajocor, edad) 
```

## 2. Cálculo manual de `\(R^2\)` y `\(R^2\)` ajustado

Primero realizamos una regresión lineal múltiple para estimar el efecto de la escolaridad y la edad sobre los ingresos del trabajo.


```r
# Realizamos la regresión
modelo3 <- lm(ytrabajocor ~ esc + edad, data = casen2)
summary(modelo3)
```

```
## 
## Call:
## lm(formula = ytrabajocor ~ esc + edad, data = casen2)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1742504  -328602  -111787   151201 49974286 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -742740.5    14304.0  -51.92   <2e-16 ***
## esc           88022.2      704.3  124.98   <2e-16 ***
## edad           7681.3      189.9   40.45   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 753000 on 88388 degrees of freedom
##   (113840 observations deleted due to missingness)
## Multiple R-squared:  0.1507,	Adjusted R-squared:  0.1507 
## F-statistic:  7842 on 2 and 88388 DF,  p-value: < 2.2e-16
```

A continuación, calculamos manualmente el `\(R^2\)` y el `\(R^2\)` ajustado. Recordemos que el `\(R^2\)` mide la proporción de la variabilidad explicada por el modelo, mientras que el `\(R^2\)` ajustado penaliza por el número de predictores para evitar el sobreajuste.


```r
# Calculo manual de R^2 y R^2 ajustado
SSR <- sum((fitted(modelo3) - mean(casen2$ytrabajocor, na.rm = TRUE))^2)  # Suma de cuadrados explicada
SST <- sum((casen2$ytrabajocor - mean(casen2$ytrabajocor, na.rm = TRUE))^2, na.rm = TRUE) # Suma total de cuadrados
R2 <- SSR / SST

n <- nrow(casen2 %>% filter(!is.na(ytrabajocor)))  # Número de observaciones
k <- length(coef(modelo3)) - 1  # Número de predictores
R2_ajustado <- 1 - ((1 - R2) * (n - 1) / (n - k - 1))

cat("R^2: ", R2, "\n")
```

```
## R^2:  0.1500966
```

```r
cat("R^2 Ajustado: ", R2_ajustado, "\n")
```

```
## R^2 Ajustado:  0.1500775
```

### Interpretación de `\(R^2\)` no ajustado
El valor de `\(R^2\)` obtenido fue 0.150, lo que indica que el 15% de la variabilidad en los ingresos del trabajo es explicada por las variables independientes del modelo: escolaridad y edad. Este valor nos da una idea de la capacidad del modelo para explicar la variabilidad de los ingresos, aunque no tiene en cuenta el número de predictores, lo que puede llevar a una sobreestimación si se añaden muchas variables sin una contribución sustancial.


## 3. Cálculo manual de los estadísticos de significancia

Para evaluar la significancia de los coeficientes de la regresión, calculamos manualmente el valor t y el valor p asociado para cada coeficiente.


```r
# Extraemos los coeficientes y errores estándar
betas <- coef(modelo3)
SE <- summary(modelo3)$coefficients[, "Std. Error"]

# Calculamos los valores t y p
valores_t <- betas / SE
valores_p <- 2 * pt(-abs(valores_t), df = n - k - 1)

# Imprimimos los resultados
coeficientes <- data.frame(Coefficient = betas, Std_Error = SE, t_value = valores_t, p_value = valores_p)
print(coeficientes)
```

```
##             Coefficient  Std_Error   t_value p_value
## (Intercept) -742740.492 14303.9664 -51.92549       0
## esc           88022.195   704.2920 124.97969       0
## edad           7681.283   189.8821  40.45291       0
```

## 4. Presentación de la tabla del modelo

Utilizamos `texreg` para presentar la tabla del modelo de manera más amigable.


```r
htmlreg(list(modelo3))
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>Statistical models</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
<th style="padding-left: 5px;padding-right: 5px;">Model 1</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">(Intercept)</td>
<td style="padding-left: 5px;padding-right: 5px;">-742740.49<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(14303.97)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">esc</td>
<td style="padding-left: 5px;padding-right: 5px;">88022.20<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(704.29)</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">edad</td>
<td style="padding-left: 5px;padding-right: 5px;">7681.28<sup>&#42;&#42;&#42;</sup></td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
<td style="padding-left: 5px;padding-right: 5px;">(189.88)</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.15</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">Adj. R<sup>2</sup></td>
<td style="padding-left: 5px;padding-right: 5px;">0.15</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
<td style="padding-left: 5px;padding-right: 5px;">88391</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="2"><sup>&#42;&#42;&#42;</sup>p &lt; 0.001; <sup>&#42;&#42;</sup>p &lt; 0.01; <sup>&#42;</sup>p &lt; 0.05</td>
</tr>
</tfoot>
</table>

## 5. Interpretación de los coeficientes

**Interpretación del Modelo de Regresión**

Este modelo examina cómo la escolaridad y la edad afectan los ingresos del trabajo. A continuación se presentan las interpretaciones de los coeficientes principales:

### Coeficientes del Modelo

**Intercepto ($b_0$)**:  
El intercepto de -742,740.49 indica el valor esperado de los ingresos cuando tanto la escolaridad como la edad son cero, lo cual no tiene una interpretación práctica debido a la falta de sentido de esta situación en el contexto real. Este valor es estadísticamente significativo ($p < 0.001$).

**Escolaridad ($b_1$)**:  
El coeficiente para escolaridad es 88,022.20, indicando que, por cada año adicional de escolaridad, los ingresos del trabajo aumentan en promedio 88,022.20 unidades monetarias, manteniendo constante la edad. Este coeficiente es altamente significativo ($p < 0.001$).

**Edad ($b_2$)**:  
El coeficiente para la edad es 7,681.28, lo que implica que cada año adicional de edad se asocia con un aumento promedio de 7,681.28 unidades en los ingresos, manteniendo constante la escolaridad. Este coeficiente también es significativo ($p < 0.001$), pero su tamaño del efecto es menor que el de la escolaridad.

**$R^2$ y `\(R^2\)` Ajustado**:  
El valor de `\(R^2\)` de 0.15 indica que el 15% de la variabilidad en los ingresos del trabajo es explicada por la edad y la escolaridad.

### Consideraciones sobre Significancia y Tamaño del Efecto

- **Significancia Estadística**: Ambos coeficientes son significativos ($p < 0.001$), lo cual sugiere que sus efectos sobre los ingresos no son atribuibles al azar.
- **Tamaño del Efecto**: Aunque ambos coeficientes son significativos, el tamaño del efecto de la escolaridad es considerablemente mayor que el de la edad, lo que implica que la educación tiene un mayor impacto en los ingresos.


## 6. Ejercicio de simulación: Variabilidad de los coeficientes

Para ilustrar la **variabilidad de los coeficientes** debido al muestreo aleatorio, realizaremos un ejercicio de simulación donde generaremos una población con un valor conocido de `\(β = 2\)`, y luego extraeremos múltiples muestras para observar cómo varían los coeficientes estimados.


```r
set.seed(123)  # Fijamos la semilla para reproducibilidad

# Generamos una población de 100,000 observaciones
n_poblacion <- 100000
x_poblacion <- rnorm(n_poblacion, mean = 50, sd = 10)
error_poblacion <- rnorm(n_poblacion, mean = 0, sd = 5)

y_poblacion <- 5 + 2 * x_poblacion + error_poblacion

# Tomamos una muestra de la población y realizamos la regresión
n_muestra <- 100
muestra <- sample(1:n_poblacion, n_muestra)
x_muestra <- x_poblacion[muestra]
y_muestra <- y_poblacion[muestra]

modelo_muestra <- lm(y_muestra ~ x_muestra)
summary(modelo_muestra)
```

```
## 
## Call:
## lm(formula = y_muestra ~ x_muestra)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.4124  -2.9333   0.4454   3.4283   9.2603 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.84860    2.09127   3.753 0.000296 ***
## x_muestra    1.95414    0.03992  48.956  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.524 on 98 degrees of freedom
## Multiple R-squared:  0.9607,	Adjusted R-squared:  0.9603 
## F-statistic:  2397 on 1 and 98 DF,  p-value: < 2.2e-16
```

```r
# Repetimos el proceso para 1000 muestras y almacenamos los coeficientes
n_replicas <- 1000
betas_muestras <- numeric(n_replicas)

for (i in 1:n_replicas) {
  muestra <- sample(1:n_poblacion, n_muestra)
  x_muestra <- x_poblacion[muestra]
  y_muestra <- y_poblacion[muestra]
  modelo <- lm(y_muestra ~ x_muestra)
  betas_muestras[i] <- coef(modelo)[2]
}

# Graficamos el histograma de los coeficientes beta obtenidos
hist(betas_muestras, breaks = 30, col = "skyblue", main = "Distribución de los coeficientes beta",
     xlab = "Valores de beta", ylab = "Frecuencia")
abline(v = 2, col = "red", lwd = 2, lty = 2)  # Línea indicando el valor verdadero de beta
```

<img src="/example/07-practico_files/figure-html/unnamed-chunk-6-1.png" width="672" />

En este ejercicio, hemos generado una población donde el valor verdadero de `\(β\)` es 2. Luego, tomamos muestras aleatorias y estimamos el coeficiente beta para cada muestra. El histograma muestra cómo varían los valores estimados de beta, ilustrando la **variabilidad muestral**. La línea roja indica el valor verdadero de `\(β\)`, y podemos observar cómo los valores de las muestras se distribuyen alrededor de este valor, reflejando la variabilidad inherente al proceso de muestreo.
