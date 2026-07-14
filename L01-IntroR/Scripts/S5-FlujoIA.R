# Get working directory para saber en dónde estamos
getwd()

# Carga Básica de Archivos -------------------------------------

# Set working directory

#### OJO!!!!!!!!!!!!!!!!!!!!!!!!!!!
#### PONER AQUÍ ABAJO LA UBICACIÓN EN SU COMPUTADORA
#### EN DONDE SE ENCUENTRAN LOS DATOS QUE QUEREMOS CARGAR
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L01-IntroR/data")

# Puede seleccionarse manualmente usando el Shortcut CTRL+SHIFT+H
# Pero entonces se tendrá que hacer cada vez que se abra el archivo

# Ejemplos de Prompts para generar código:

#############################
# Eres un experto en R y en la librería dplyr. 
# Escribe un script que cargue el archivo 'ventas.csv' 
# desde el directorio actual en un dataframe. 
# A continuación agrupa los datos por 'Region' y 'Fecha', 
# y calcula la suma total de ventas para cada combinación

# Instalar los paquetes si no los tienes previamente instalados
# install.packages("dplyr")
# install.packages("readr")

# Cargar las librerías necesarias
library(dplyr)
library(readr) # Usamos readr porque read_csv es más rápido y eficiente que read.csv de R base

# 1. Cargar el archivo 'ventas.csv' desde el directorio actual
# read_csv lo carga automáticamente como un tibble (un dataframe moderno)
df_ventas <- read_csv("ventas.csv")

# 2. Agrupar los datos y calcular la suma total de ventas
resumen_ventas <- df_ventas %>%
  # Agrupamos por las columnas 'Region' y 'Fecha'
  group_by(Region, Fecha) %>%
  # Calculamos la suma total. 
  # Asumimos que la columna numérica de ventas se llama 'Ventas'.
  summarise(
    Ventas_Totales = sum(Ventas, na.rm = TRUE), 
    .groups = "drop" # Usamos 'drop' para desagrupar el dataframe resultante y evitar advertencias
  )

# Mostrar las primeras filas del resultado
print(resumen_ventas)

#############################
# Usando el resultado del paso anterior calcula
# la media, la mediana y la desviación estándar por región de ventas.
# Usando el dataframe 'resumen_ventas' del paso anterior

estadisticas_region <- resumen_ventas %>%
  # Agrupamos solo por Region
  group_by(Region) %>%
  # Calculamos las métricas solicitadas usando las Ventas_Totales
  summarise(
    Media_Ventas    = mean(Ventas_Totales, na.rm = TRUE),
    Mediana_Ventas  = median(Ventas_Totales, na.rm = TRUE),
    Desv_Estandar   = sd(Ventas_Totales, na.rm = TRUE),
    .groups = "drop"
  )

# Mostrar los resultados
print(estadisticas_region)

#############################
# Usando los datos cargados y la librería ggplot2
# genera un gráfico de barras. 
# El eje X debe ser la Región, el eje Y las Ventas Totales,
# El color de relleno (fill) debe ser la Categoría. 
# Que las barras aparezcan lado a lado (agrupadas), no apiladas. 
# Usa un tema minimalista.
# Instalar ggplot2 si no lo tienes
# install.packages("ggplot2")

# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Preparar los datos (calculamos las ventas totales por Región y Categoría)
# Usamos 'df_ventas', que es el dataframe original cargado en el primer paso
df_grafico <- df_ventas %>%
  group_by(Region, Categoria) %>%
  summarise(
    Ventas_Totales = sum(Ventas, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Generar el gráfico de barras agrupadas
ggplot(df_grafico, aes(x = Region, y = Ventas_Totales, fill = Categoria)) +
  
  # Usamos geom_col() porque ya calculamos el valor exacto de Y (Ventas_Totales).
  # position = "dodge" hace que las barras se coloquen lado a lado en lugar de apilarse.
  geom_col(position = "dodge", color = "white", lwd = 0.3) + 
  
  # Personalizar los títulos y etiquetas de los ejes
  labs(
    title = "Ventas Totales por Región y Categoría",
    subtitle = "Comparativa agrupada por regiones",
    x = "Región",
    y = "Ventas Totales",
    fill = "Categoría"
  ) +
  
  # Aplicar el tema minimalista solicitado
  theme_minimal() +
  
  # Opcional: Ajustes estéticos adicionales para que se vea aún más limpio
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(), # Quita las líneas de cuadrícula secundarias
    legend.position = "bottom"          # Mueve la leyenda abajo para dar más espacio al gráfico
  )


#############################
# Usando los datos cargados, la librería lubridate y ggplot2
# Genera un gráfico de línea mostrando las ventas mensuales totales. 
# El eje X debe ser la Fecha en formato "Mes-Año", el eje Y las Ventas Totales.
# La fecha original está en formato Día-Mes-Año.
# Usa un tema minimalista. 
# Instalar lubridate si no lo tienes
# install.packages("lubridate")

# Cargar las librerías
library(dplyr)
library(ggplot2)
library(lubridate)

# 1. Preparar los datos
df_mensual <- df_ventas %>%
  # dmy() de lubridate convierte el texto "Día-Mes-Año" a un formato de Fecha válido en R
  mutate(Fecha_Real = dmy(Fecha),
         # floor_date() redondea la fecha al primer día del mes para poder agrupar
         Mes_Anio = floor_date(Fecha_Real, "month")) %>%
  # Agrupamos por el nuevo campo mensual
  group_by(Mes_Anio) %>%
  summarise(
    Ventas_Totales = sum(Ventas, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Generar el gráfico de línea
ggplot(df_mensual, aes(x = Mes_Anio, y = Ventas_Totales)) +
  
  # geom_line() crea el gráfico de línea. lwd ajusta el grosor de la línea
  geom_line(color = "steelblue", lwd = 1) +
  
  # Opcional pero recomendado: geom_point() añade un punto en cada mes para mayor claridad
  geom_point(color = "darkblue", size = 2) +
  
  # Formatear el eje X para que muestre explícitamente "Mes-Año" (ej. "01-2023")
  # date_breaks = "1 month" asegura que haya una marca por cada mes
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  
  # Personalizar etiquetas
  labs(
    title = "Evolución de Ventas Totales Mensuales",
    x = "Fecha (Mes-Año)",
    y = "Ventas Totales"
  ) +
  
  # Aplicar el tema minimalista solicitado
  theme_minimal() +
  
  # Ajustar el ángulo del texto en el eje X para que no se encimen las fechas
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

############################
# A partir de los datos de ventas mensuales, 
# Calcula un modelo de regresión lineal 
# donde la variable dependiente sean las Ventas_Totales 
# y la independiente sea el mes. Muestra la tabla de resumen.
# Después, usando ggplot2, actualiza el gráfico de líneas temporal 
# superponiendo la recta de regresión de este modelo (usa geom_smooth). Haz que la recta de tendencia destaque visualmente (por ejemplo, en color rojo y punteada) y mantén el tema minimalista."
# Cargar las librerías necesarias (asumiendo que df_mensual ya existe del paso anterior)
library(dplyr)
library(ggplot2)

# 1. Preparar los datos para la regresión
# Ordenamos cronológicamente y creamos una variable numérica 'Mes_Num'
df_regresion <- df_mensual %>%
  arrange(Mes_Anio) %>%
  mutate(Mes_Num = row_number()) # 1 para el primer mes, 2 para el segundo, etc.

# 2. Calcular el modelo de regresión lineal (lm)
# Ventas_Totales es la variable dependiente (Y), Mes_Num es la independiente (X)
modelo_lineal <- lm(Ventas_Totales ~ Mes_Num, data = df_regresion)

# Muestra la tabla de resumen estadístico en la consola
# Aquí verás los coeficientes, el R-cuadrado y los p-valores
summary(modelo_lineal)

# 3. Actualizar el gráfico de líneas superponiendo la recta de regresión
ggplot(df_regresion, aes(x = Mes_Anio, y = Ventas_Totales)) +
  
  # Gráfico de línea y puntos original (color azul)
  geom_line(color = "steelblue", lwd = 1) +
  geom_point(color = "darkblue", size = 2) +
  
  # Superponer la recta de regresión lineal usando geom_smooth
  # method = "lm" le indica que use un modelo lineal
  # se = FALSE remueve la banda sombreada del intervalo de confianza (opcional)
  # linetype = "dashed" hace la línea punteada
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", lwd = 1.2) +
  
  # Formatear el eje X para que muestre "Mes-Año"
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  
  # Personalizar etiquetas e incluir anotaciones informativas
  labs(
    title = "Evolución de Ventas con Línea de Tendencia Lineal",
    subtitle = "La línea roja punteada representa el modelo de regresión",
    x = "Fecha (Mes-Año)",
    y = "Ventas Totales"
  ) +
  
  # Mantener el tema minimalista solicitado
  theme_minimal() +
  
  # Ajustar el ángulo del texto en el eje X para evitar solapamientos
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

############################
# Modifica únicamente el código del gráfico final 
# aplicando las siguientes características visuales:
# Utiliza un tema oscuro general y elimina las líneas de enrejado 
# (grid) únicamente del eje X.
# Dibuja la línea de ventas en color cornflowerblue.
# Dibuja la línea de regresión en color coral.
# Título: Formato en negrita y color cornflowerblue.
# Subtítulo: Color gris y letras cursivas
# Etiquetas y ticks de ambos ejes (X e Y): Color gris
# Ticks del eje Y con formato de moneda utilizando librería scales
# Asegúrate de tener instalada y cargada la librería scales
# install.packages("scales")
library(scales)
library(ggplot2)

# Gráfico final con personalización avanzada
ggplot(df_regresion, aes(x = Mes_Anio, y = Ventas_Totales)) +
  
  # Línea de ventas y puntos en color cornflowerblue
  geom_line(color = "cornflowerblue", lwd = 1) +
  geom_point(color = "cornflowerblue", size = 2) +
  
  # Línea de regresión en color coral
  geom_smooth(method = "lm", se = FALSE, color = "coral", linetype = "dashed", lwd = 1.2) +
  
  # Formato del eje X (Fechas)
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  
  # Formato del eje Y: Moneda usando la librería scales
  scale_y_continuous(labels = label_dollar(prefix = "$")) +
  
  # Textos del gráfico
  labs(
    title = "Evolución de Ventas con Línea de Tendencia Lineal",
    subtitle = "La línea coral punteada representa el modelo de regresión",
    x = "Fecha (Mes-Año)",
    y = "Ventas Totales"
  ) +
  
  # Aplicar un tema oscuro base
  theme_dark() +
  
  # Sobrescribir elementos específicos del tema según tus instrucciones
  theme(
    # Oscurecer el fondo para un verdadero "modo oscuro" (el theme_dark por defecto es un poco gris)
    plot.background = element_rect(fill = "#2E2E2E", color = NA),
    panel.background = element_rect(fill = "#1E1E1E", color = NA),
    
    # Título: Negrita y color cornflowerblue
    plot.title = element_text(face = "bold", color = "cornflowerblue", size = 14),
    
    # Subtítulo: Gris y cursiva (italic)
    plot.subtitle = element_text(color = "gray", face = "italic"),
    
    # Etiquetas (títulos) y ticks (textos de los valores) de los ejes en gris
    axis.title = element_text(color = "gray"),
    axis.text.y = element_text(color = "gray"),
    axis.text.x = element_text(color = "gray", angle = 45, hjust = 1),
    
    # Eliminar líneas de grid ÚNICAMENTE del eje X
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    # Opcional: Mantener el grid del eje Y con un tono sutil para no desentonar con el fondo
    panel.grid.major.y = element_line(color = "#444444"),
    panel.grid.minor.y = element_line(color = "#333333")
  )