library(tidyverse)
library(skimr)
library(performance)

# ==========================================
# PERFIL DE DATOS COMPLETOS
head(diamonds)
summary(diamonds)
skim(diamonds)

# Descripciones de columnas:
#
# price   - Precio en dólares estadounidenses ($326 a $18,823)
# carat   - Peso del diamante en quilates (0.2 a 5.01)
# cut     - Calidad del corte (Fair [Regular], Good [Bueno], Very Good [Muy Bueno], Premium, Ideal)
# color   - Color del diamante, desde J (peor) hasta D (mejor)
# clarity - Nivel de claridad del diamante (I1 (peor), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (mejor))
# x       - Longitud en milímetros (0 a 10.74)
# y       - Anchura en milímetros (0 a 58.9)
# z       - Profundidad en milímetros (0 a 31.8)
# depth   - Porcentaje de profundidad total; calculado como z / mean(x, y) 
# table   - Anchura de la parte superior (mesa) relativa al punto más ancho del diamante


# ==========================================
# HISTOGRAMA PRECIOS

# Generar el gráfico de distribución en histograma
ggplot(diamonds, aes(x = price)) +
  geom_histogram(fill = "seagreen2", color = "white", alpha = 0.9, bins = 30) + 
  # Etiquetas y títulos descriptivos
  labs(
    title = "Distribución de Precios",
    subtitle = "Precios de diamante en USD",
    x = "Precio de Venta (USD)",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  # Personalización estética
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "seagreen3"),
    plot.subtitle = element_text(color = "#777777", face = "italic"),
    panel.grid.minor = element_blank(),  # Elimina líneas de cuadrícula intermedias
    panel.grid.major.x = element_blank() # Elimina líneas verticales
  )

# ==========================================
# BOXPLOT PRECIOS

boxplot(diamonds$price,
        main = "Boxplot de precio", 
        ylab = "precio (USD)", 
        col = "seagreen2")

priceBox <- boxplot.stats(diamonds$price)$stats
priceBox

# Boxplot dividiendo por categorías
ggplot(diamonds, aes(x = color, y = price, fill = color)) +
  geom_boxplot()

diamonds %>% 
  filter(price > last(priceBox))

# Select only the numeric columns
numeric_diamonds <- diamonds[, sapply(diamonds, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_diamonds)
cor_matrix

library(corrplot)
corrplot(cor_matrix, method = "square", type = "upper", tl.col = "black")

price99 <- quantile(diamonds$price, probs = 0.99)
price99

diamonds %>% 
  filter(price > price99)

# ==========================================
# BOXPLOT DE DIMENSIONES (largo, ancho, profundidad)

boxplot(diamonds[,c("x","y","z")],
        main = "Boxplots de dimensión", 
        ylab = "milimetros", 
        col = "turquoise")

# ==========================================
# OUTLIERS POR CONTEXTO
skim(diamonds[,c("x","y","z")])
summary(diamonds[,c("x","y","z")])
diamonds %>% filter(x==0 |y==0 | z==0)

# ==========================================
# MÉTODO 1: TRIM
diamTrim <- diamonds %>% filter(x!=0 &y!=0 & z!=0)
skim(diamTrim[,c("x","y","z")])
summary(diamTrim[,c("x","y","z")])

# ==========================================
# MÉTODO 2: IMPUTAR MEDIA
mean_x <- mean(diamonds$x)
mean_y <- mean(diamonds$y)
mean_z <- mean(diamonds$z)

diamMean <- diamonds %>% 
  mutate(x=ifelse(x==0, mean_x, x),
         y=ifelse(y==0, mean_y, y),
         z=ifelse(z==0, mean_z, z))

skim(diamMean[,c("x","y","z")])
summary(diamMean[,c("x","y","z")])

# ==========================================
# MÉTODO 3: IMPUTAR MEDIANA
median_x <- median(diamonds$x)
median_y <- median(diamonds$y)
median_z <- median(diamonds$z)

diamMedian <- diamonds %>% 
  mutate(x=ifelse(x==0, median_x, x),
         y=ifelse(y==0, median_y, y),
         z=ifelse(z==0, median_z, z))

skim(diamMedian[,c("x","y","z")])
summary(diamMedian[,c("x","y","z")])

# ==========================================
# MÉTODO 4: WINSORIZACION USANDO MINIMO EXCLUYENDO 0
min_x <- min(diamonds$x[diamonds$x > 0])
min_y <- min(diamonds$y[diamonds$y > 0])
min_z <- min(diamonds$z[diamonds$z > 0])

diamMin <- diamonds %>% 
  mutate(x=ifelse(x==0, min_x, x),
         y=ifelse(y==0, min_y, y),
         z=ifelse(z==0, min_z, z))

skim(diamMin[,c("x","y","z")])
summary(diamMin[,c("x","y","z")])

# ==========================================
# MÉTODO 5: WINSORIZACION USANDO PERCENTIL 1
p01_x <- quantile(diamonds$x, probs = 0.01)
p01_y <- quantile(diamonds$y, probs = 0.01)
p01_z <- quantile(diamonds$z, probs = 0.01)

diamP1 <- diamonds %>% 
  mutate(x=ifelse(x==0, p01_x, x),
         y=ifelse(y==0, p01_y, y),
         z=ifelse(z==0, p01_z, z))

skim(diamP1[,c("x","y","z")])
summary(diamP1[,c("x","y","z")])

# ==========================================
# SELECCION DE METODO
diamondsClean <- diamMin

summary(diamondsClean$y)

boxplot(diamondsClean$y,
        main = "Boxplot de ancho", 
        ylab = "milimetros", 
        col = "turquoise")

# ==========================================
# ANÁLISIS Y
# Obtener datos de boxplot Y
outlier_results <- check_outliers(diamondsClean$y)
print(outlier_results)

yBox <- boxplot.stats(diamondsClean$y)$stats
maxYBox <- last(yBox)

# Observar outliers
diamondsClean %>% 
  filter(y > maxYBox)

# ==========================================
# MÉTODO 1: TRIM
diamDimTrim <- diamondsClean %>% filter(y <= maxYBox)
skim(diamDimTrim[,c("x","y","z")])
summary(diamDimTrim[,c("x","y","z")])

# ==========================================
# MÉTODO 2: IMPUTAR MEDIA
diamDimMean <- diamondsClean %>% 
  mutate(y=ifelse(y > maxYBox, mean_y, y))

skim(diamDimMean[,c("x","y","z")])
summary(diamDimMean[,c("x","y","z")])

# ==========================================
# MÉTODO 3: IMPUTAR MEDIANA
diamDimMedian <- diamondsClean %>% 
  mutate(y=ifelse(y > maxYBox, median_y, y))

skim(diamDimMedian[,c("x","y","z")])
summary(diamDimMedian[,c("x","y","z")])

# ==========================================
# MÉTODO 4: WINSORIZACION USANDO MAXIMO DE CAJA
diamDimMax <- diamondsClean %>% 
  mutate(y=ifelse(y > maxYBox, maxYBox, y))

skim(diamDimMax[,c("x","y","z")])
summary(diamDimMax[,c("x","y","z")])

# ==========================================
# MÉTODO 5: WINSORIZACION USANDO PERCENTIL 99
p99_y <- quantile(diamonds$y, probs = 0.99)

diamDimP99 <- diamondsClean %>% 
  mutate(y=ifelse(y > maxYBox, p99_y, y))

skim(diamDimP99[,c("x","y","z")])
summary(diamDimP99[,c("x","y","z")])

# ==========================================
# SELECCION DE METODO
diamondsDimClean <- diamDimP99

summary(diamondsDimClean$y)

boxplot(diamondsDimClean$y,
        main = "Boxplot de ancho", 
        ylab = "milimetros", 
        col = "turquoise")

# ==========================================
# CHALLENGE - REMOVER OUTLIERS DE TODAS LAS DIMENSIONES

# Modificar el código anterior o escribir líneas adicionales
# para remover outliers de todas las dimensiones (x, y, z).

# Al finalizar correr nuevamente el boxplot de las tres dimensiones para verificar que no hay outliers.
# Puede ser necesario cambiar el nombre diamondsClean a otro nombre en caso de haberlo modificado

boxplot(diamondsDimClean[,c("x","y","z")],
        main = "Boxplots de dimensión", 
        ylab = "milimetros", 
        col = "turquoise")