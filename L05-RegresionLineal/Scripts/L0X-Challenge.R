library(tidyverse)
library(corrplot)
library(broom)

# Get working directory para saber en dónde estamos
getwd()

# Set working directory (Pon el tuyo)
setwd("TU CÓDIGO AQUÍ")

# Carga el archivo "US_change_full.csv" dentro de la variable uschange
uschange <- read.csv("TU CÓDIGO AQUÍ")

# Mostrar los primeros 10 datos
head(uschange,10)
# Mostrar un resumen de lo que se incluye en el dataframe
summary(uschange)

# Grafica la matriz de correlaciones en un mapa de calor
# Utilizando la función corrplot de la librería con el mismo nombre
# TU CÓDIGO AQUÍ

# Ajusta un modelo de regresión lineal (simple) Y~X
# Y : Variable respuesta Consumption
# X : Variable explicativa (la que tú elijas)
# Y guardarla dentro de una variable llamada "reg"

reg <- # TU CÓDIGO AQUÍ
  
# Analiza los resultados usando la función summary a la variable reg
summary(reg) 

# Elabora una gráfica de dispersión de Consumo (eje y)
# VS Variable explicativa (eje x)
# Incorpora una línea que indique el modelo nulo (promedio de consumo)
# Y la línea de regresión

# TU CÓDIGO AQUÍ

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones

# TU CÓDIGO AQUÍ

# Elabora el gráfico de residuales VS predicciones
# Incluyendo la línea de regresión

# TU CÓDIGO AQUÍ

# Regresión múltiple
# Elabora un modelo de regresión múltiple
# Cuya variable (Y) dependiente sea Consumo
# Utilizando dos variables explicativas (las que sean de tu elección)

reg_mult <- # TU CÓDIGO AQUÍ
  
# Analiza los resultados utilizando la función summary
summary(reg_mult)

# Guarda las predicciones y residuales dentro del DF
# Y elabora una regresión de residuales ~ predicciones
# Para el modelo múltiple

# TU CÓDIGO AQUÍ

# Elabora un Gráfico de residuales contra predicciones para el modelo múltiple

# TU CÓDIGO AQUÍ

# Realiza una comparación de ambos modelos 
# Utilizando criterios de información

# TU CÓDIGO AQUÍ

# Tras el análisis propuesto, ¿cuál de los dos modelos elegirías?
# ¿Por qué?