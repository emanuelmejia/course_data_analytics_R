# ==============================================================================
# SCRIPT COMPLETO DE ANALISIS Y VISUALIZACION DE VENTAS EN R
# Autor: Experto en R y dplyr
# Descripción: Flujo completo de carga, agrupación, cálculo de estadísticas,
#              modelado de regresión lineal y visualización avanzada (ggplot2).
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. INSTALACIÓN Y CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------
# Descomenta las siguientes líneas si no tienes instalados los paquetes:
# install.packages("dplyr")
# install.packages("readr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("scales")

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# 1. CARGA DE DATOS
# ------------------------------------------------------------------------------
# Se carga el archivo 'ventas.csv' desde el directorio actual.
# 'read_csv' lee el archivo de forma eficiente y lo almacena como un tibble.
if (file.exists("ventas.csv")) {
  df_ventas <- read_csv("ventas.csv")
} else {
  stop("El archivo 'ventas.csv' no se encuentra en el directorio actual. Verifique con getwd().")
}

# ------------------------------------------------------------------------------
# 2. AGRUPACIÓN Y SUMA TOTAL DE VENTAS (POR REGIÓN Y FECHA)
# ------------------------------------------------------------------------------
resumen_ventas <- df_ventas %>%
  # Agrupamos por las columnas 'Region' y 'Fecha'
  group_by(Region, Fecha) %>%
  # Calculamos la suma total de la columna numérica 'Ventas'
  summarise(
    Ventas_Totales = sum(Ventas, na.rm = TRUE), 
    .groups = "drop" # Desagrupamos para evitar advertencias en futuras operaciones
  )

cat("--- Primeras filas del resumen de ventas por Región y Fecha ---\n")
print(head(resumen_ventas))
cat("\n")

# ------------------------------------------------------------------------------
# 3. CÁLCULO DE ESTADÍSTICAS DESCRIPTIVAS POR REGIÓN
# ------------------------------------------------------------------------------
# Usando el resultado anterior, calculamos la media, mediana y desviación estándar
estadisticas_region <- resumen_ventas %>%
  group_by(Region) %>%
  summarise(
    Media_Ventas    = mean(Ventas_Totales, na.rm = TRUE),
    Mediana_Ventas  = median(Ventas_Totales, na.rm = TRUE),
    Desv_Estandar   = sd(Ventas_Totales, na.rm = TRUE),
    .groups = "drop"
  )

cat("--- Estadísticas Descriptivas por Región ---\n")
print(estadisticas_region)
cat("\n")

# ------------------------------------------------------------------------------
# 4. GRÁFICO DE BARRAS AGRUPADAS (REGIONAL Y POR CATEGORÍA)
# ------------------------------------------------------------------------------
# Preparar datos uniendo ventas por Región y Categoría
df_barras <- df_ventas %>%
  group_by(Region, Categoria) %>%
  summarise(
    Ventas_Totales = sum(Ventas, na.rm = TRUE),
    .groups = "drop"
  )

# Generar gráfico de barras agrupadas lado a lado (position = "dodge")
grafico_barras <- ggplot(df_barras, aes(x = Region, y = Ventas_Totales, fill = Categoria)) +
  geom_col(position = "dodge", color = "white", lwd = 0.3) + 
  labs(
    title = "Ventas Totales por Región y Categoría",
    subtitle = "Comparativa agrupada por regiones",
    x = "Región",
    y = "Ventas Totales",
    fill = "Categoría"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Descomenta la siguiente línea para visualizar el gráfico de barras:
# print(grafico_barras)

# ------------------------------------------------------------------------------
# 5. PREPARACIÓN TEMPORAL Y REGRESIÓN LINEAL (VENTAS MENSUALES)
# ------------------------------------------------------------------------------
df_mensual <- df_ventas %>%
  # dmy() convierte texto "Día-Mes-Año" a clase Date
  mutate(Fecha_Real = dmy(Fecha),
         # floor_date() trunca la fecha al primer día del mes para agrupar mensualmente
         Mes_Anio = floor_date(Fecha_Real, "month")) %>%
  group_by(Mes_Anio) %>%
  summarise(
    Ventas_Totales = sum(Ventas, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Ordenamos cronológicamente y creamos un índice numérico para la regresión
  arrange(Mes_Anio) %>%
  mutate(Mes_Num = row_number())

# Ajustar el Modelo de Regresión Lineal
# Variable dependiente: Ventas_Totales | Variable independiente: Mes_Num
modelo_lineal <- lm(Ventas_Totales ~ Mes_Num, data = df_mensual)

cat("--- Resumen Estadístico del Modelo de Regresión Lineal ---\n")
print(summary(modelo_lineal))
cat("\n")

# ------------------------------------------------------------------------------
# 6. GRÁFICO TEMPORAL AVANZADO (TEMA OSCURO Y LÍNEA DE TENDENCIA)
# ------------------------------------------------------------------------------
grafico_final <- ggplot(df_mensual, aes(x = Mes_Anio, y = Ventas_Totales)) +
  # Línea de ventas y puntos en color cornflowerblue
  geom_line(color = "cornflowerblue", lwd = 1) +
  geom_point(color = "cornflowerblue", size = 2) +
  
  # Línea de regresión estimada en color coral y discontinua
  geom_smooth(method = "lm", se = FALSE, color = "coral", linetype = "dashed", lwd = 1.2) +
  
  # Formato del eje X temporal (Mes-Año) con saltos mensuales
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  
  # Formato del eje Y a Moneda usando la librería 'scales'
  scale_y_continuous(labels = label_dollar(prefix = "$")) +
  
  # Etiquetas de textos y títulos
  labs(
    title = "Evolución de Ventas con Línea de Tendencia Lineal",
    subtitle = "La línea coral punteada representa el modelo de regresión",
    x = "Fecha (Mes-Año)",
    y = "Ventas Totales"
  ) +
  
  # Aplicación del tema oscuro base
  theme_dark() +
  
  # Personalización estética detallada según requerimientos
  theme(
    # Ajustes de colores de fondo para modo oscuro real
    plot.background = element_rect(fill = "#2E2E2E", color = NA),
    panel.background = element_rect(fill = "#1E1E1E", color = NA),
    
    # Título: Negrita y cornflowerblue
    plot.title = element_text(face = "bold", color = "cornflowerblue", size = 14),
    
    # Subtítulo: Gris y cursiva
    plot.subtitle = element_text(color = "gray", face = "italic"),
    
    # Ejes y ticks en color gris
    axis.title = element_text(color = "gray"),
    axis.text.y = element_text(color = "gray"),
    axis.text.x = element_text(color = "gray", angle = 45, hjust = 1),
    
    # Eliminación estricta de las líneas de cuadrícula en el eje X
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    # Atenuación de las líneas de cuadrícula del eje Y para que armonicen con el fondo
    panel.grid.major.y = element_line(color = "#444444"),
    panel.grid.minor.y = element_line(color = "#333333")
  )

# Mostrar el gráfico final modificado en pantalla
print(grafico_final)
