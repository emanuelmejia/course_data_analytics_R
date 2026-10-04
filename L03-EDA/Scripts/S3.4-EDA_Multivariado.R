# Tidiverse incluye varios paquetes útiles para datos
# ggplot2 para visualización
# dplyr para manipulación de datos
# tidyr para organizar datos
# readr para importar datos 
# cpurrr para programación funcional 
# tibble para marcos de datos mejorados. 
library(tidyverse)
library(corrplot) # librería de gráficos de correlaciones
library(skimr)

# Shortcut CTRL+SHIFT+H
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L03-EDA/data")

vino <- read.csv("vino.csv")

# ==========================================
# PERFIL DE DATOS COMPLETOS
head(vino, 10)
summary(vino)
skim(vino)

# Remover NAs
vino <- vino %>% na.omit()
summary(vino)
skim(vino)

# ==========================================
# CORRELACION

# Varianza individual
var(vino$alcohol)

# Ojo seleccionar únicamente variables numéricas
vinoNum <- vino %>% select(where(is.numeric))

# Matriz de Varianzas/Covarianzas
cov(vinoNum)
# Matriz de correlaciones
cor(vinoNum)

# Graficamos los datos numéricos
cor(vinoNum) %>% corrplot(method = "square")

# ==========================================
# GRAFICO DE DISPERSION
plot(x = vinoNum$alcohol,
     y = vinoNum$muertes,                               # Coordenadas
     col = c("orangered1"),                             # De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                                          # Tipo de punto que se va a utilizar
     main = "Vino VS Muertes",                          # Título del gráfico
     xlab = "Alcohol consumido en vino per cápita (L)", # Nombre del eje x
     ylab = "Muertes por cada 100,000 hab")             # Nombre del eje y

# ==========================================
# CHALLENGE - REALIZAR EL MISMO EJERCICIO CON EL DATASET mtcars
# GENERAR AL MENOS DOS GRÁFICOS DE DISPERSIÓN DISTINTOS
# PUEDE ELEGIRSE CUALQUIER VARIABLE PARA LA VARIANZA Y PARA EL GRAFICO
# mtcars es un dataset integrado en R, por lo que omitimos el read.csv()
# Columnas:
#
# mpg  - Millas por galón (EE. UU.)
# cyl  - Número de cilindros
# disp - Desplazamiento del motor (pulgadas cúbicas)
# hp   - Caballos de fuerza brutos
# drat - Relación del eje trasero
# wt   - Peso del vehículo (en 1,000 libras)
# qsec - Tiempo en el cuarto de milla (en segundos)
# vs   - Configuración del motor (0 = motor en V, 1 = motor en línea)
# am   - Tipo de transmisión (0 = automática, 1 = manual)
# gear - Número de velocidades (marchas adelante)
# carb - Número de carburadores

head(mtcars, 10)