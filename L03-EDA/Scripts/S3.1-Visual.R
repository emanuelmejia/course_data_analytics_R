library(skimr)
library(tidyverse)
library(tidyr)
library(scales)
library(lubridate)

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L03-EDA/data")

# Cargar los datos de ventas desde el archivo CSV generado anteriormente
ventas <- read.csv("Fventas.csv")

ventas <- ventas %>%
  mutate(
    Fecha = ymd_hms(Fecha),  # ymd_hms convierte el texto (Año-Mes-Día Hora:Minuto:Segundo) a tipo datetime
  )

# ==========================================
# PERFILADO INICIAL
head(ventas)
skim(ventas)

# ==========================================
# PROBLEMA - DETERMINAR EL NÚMERO DE VENTAS POR DÍA DE LA SEMANA
ventas_diasem <- ventas %>%
  mutate(
    Fecha = ymd_hms(Fecha),  # ymd_hms convierte el texto (Año-Mes-Día Hora:Minuto:Segundo) a tipo datetime
    Dia_Semana = wday(Fecha, label = TRUE,
                      abbr=F, week_start = 7) # wday() extrae el día de la semana (en formato de etiqueta)
  )

# Nuevo dataset
head(ventas_diasem)

# ==========================================
# PRIMER APROXIMACIÓN - GRÁFICO DE PASTEL

# Resumir los datos directamente: Contar cuántas ventas hubo por cada día
ventas_resumen <- ventas_diasem %>%
  group_by(Dia_Semana) %>%
  summarise(numVentas = n(), .groups = "drop")

# Generar un gráfico de pastel
ggplot(ventas_resumen, aes(x = "", y = numVentas, fill = Dia_Semana)) +
  geom_col(width = 1, color = "white", alpha = 0.9) + # geom_col genera un gráfico de barras de ancho variable porque el eje x es vacío
  coord_polar("y", start = 0) +                       # coord_polar transforma la barra en pastel
  
  # Agregar las etiquetas de texto
  geom_text(
    aes(label = numVentas),
    position = position_stack(vjust = 0.5), # Centra el texto dentro de la rebanada
    color = "#333333",
    fontface = "bold",
    size = 4
  ) +
  
  scale_fill_brewer(palette = "Set3") +                # Paleta de colores
  
  labs(
    title = "Distribución de Transacciones por Día de la Semana",
    subtitle = "Cada transacción independientemente del monto",
    fill = "Día de la Semana"                          # Etiqueta de la leyenda
  ) +
  
  # theme_void() es la mejor opción para pasteles porque elimina los ejes y la cuadrícula
  theme_void() + 
  
  # Personalización estética
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#333333", hjust = 0.5),
    plot.subtitle = element_text(color = "#777777", face = "italic", hjust = 0.5),
    legend.position = "right",
    plot.margin = margin(20, 20, 20, 20) 
  )

# ==========================================
# SEGUNDA APROXIMACIÓN - HISTOGRAMA

# Generar el gráfico de distribución en histograma
ggplot(ventas_diasem, aes(x = Dia_Semana)) +
  geom_bar(fill = "coral", color = "white", alpha = 0.9) + # geom_bar genera el histograma de frecuencias
  # Etiquetas y títulos descriptivos
  labs(
    title = "Distribución de Transacciones por Día de la Semana",
    subtitle = "Cada transacción independientemente del monto",
    x = "Día de la semana",
    y = "Número de Ventas (Frecuencia)"
  ) +
  theme_minimal() +
  # Personalización estética
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#333333"),
    plot.subtitle = element_text(color = "#777777", face = "italic"),
    panel.grid.minor = element_blank(),  # Elimina líneas de cuadrícula intermedias
    panel.grid.major.x = element_blank() # Elimina líneas verticales
  )

# ==========================================
# PROMPT EJEMPLO

# Actúa como un experto en visualización de datos con R. Escribe un script
# utilizando dplyr, lubridate y ggplot2, asumiendo que ya existe un 
# dataframe llamado `ventas`.
#
# 1. PROBLEMA - DETERMINAR EL NÚMERO DE VENTAS POR DÍA DE LA SEMANA:
# - Crea el dataframe `ventas_diasem` a partir de `ventas`.
# - Muta `Fecha` a datetime usando `ymd_hms(Fecha)`.
# - Crea la columna `Dia_Semana` usando wday como etiqueta sin abreviaturas
#
# HISTOGRAMA:
# - Genera un histograma de ggplot, usando `ventas_diasem`
# - Estética principal: `aes(x = Dia_Semana)`.
# - Usa `geom_bar(fill = "coral", color = "white", alpha = 0.9)`.
# - En `labs()` define: title = "Distribución de Transacciones por Día 
#   de la Semana", subtitle = "Cada transacción independientemente del 
#   monto", x = "Día de la semana", y = "Número de Ventas (Frecuencia)".
# - Agrega `theme_minimal()`.
# - Personaliza `theme()` con: plot.title(face = "bold", size = 14, 
#   color = "#333333"), plot.subtitle(color = "#777777", face = "italic"), 
#   panel.grid.minor = element_blank(), 
#   y panel.grid.major.x = element_blank().

# ==========================================
# PROBLEMA - DETERMINAR EL NÚMERO DE VENTAS POR HORA

# Preparar los datos extrayendo la hora
ventas_horas <- ventas %>%
  mutate(
    Hora_Venta = hour(Fecha) # hour() extrae únicamente la hora (en formato 0-23)
  )

# Perfil del nuevo dataset
head(ventas_horas)

# Función para generar descripción sencilla de una sola columna
summary(ventas_horas$Hora_Venta)

# Generar el gráfico de distribución
ggplot(ventas_horas, aes(x = Hora_Venta)) +
  geom_bar(fill = "cornflowerblue", color = "white", alpha = 0.9) + # geom_bar genera el histograma de frecuencias
  scale_x_continuous(breaks = 8:20) + # Forzar el eje X para que muestre exactamente los números enteros de las horas de operación
  
  # Etiquetas y títulos descriptivos
  labs(
    title = "Distribución de Transacciones por Hora del Día",
    subtitle = "Cada transacción independientemente del monto",
    x = "Hora del Día (Formato 24h)",
    y = "Número de Ventas (Frecuencia)"
  ) +
  theme_minimal() +
  # Personalización estética
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#333333"),
    plot.subtitle = element_text(color = "#777777", face = "italic"),
    panel.grid.minor = element_blank(),  # Elimina líneas de cuadrícula intermedias
    panel.grid.major.x = element_blank() # Elimina líneas verticales
  )

# ==========================================
# PROMPT EJEMPLO

# Actúa como un experto en visualización de datos con R. Escribe un script
# utilizando dplyr, lubridate y ggplot2, asumiendo que ya existe un 
# dataframe llamado `ventas`.
#
# 1. PROBLEMA - DETERMINAR EL NÚMERO DE VENTAS POR HORA:
# - Crea el dataframe `ventas_horas` a partir de `ventas`.
# - Muta una nueva columna llamada `Hora_Venta` usando la función
#   `hour(Fecha)` para extraer la hora (en formato 0-23).
#
# 2. EXPLORACIÓN DEL NUEVO DATASET:
# - Muestra los primeros registros usando `head(ventas_horas)`.
# - Aplica la función `summary()` únicamente a la columna `Hora_Venta`
#
# 3. GENERAR EL GRÁFICO DE DISTRIBUCIÓN:
# - Genera un histograma usando ggplot con el dataset `ventas_horas`.
# - Estética principal: `aes(x = Hora_Venta)`.
# - Usa `geom_bar(fill = "cornflowerblue", color = "white", alpha = 0.9)`.
# - Agrega `scale_x_continuous(breaks = 8:20)` para forzar que el eje X
#   muestre exactamente los números enteros de las horas de operación.
# - En `labs()` define: title = "Distribución de Transacciones por Hora 
#   del Día", subtitle = "Cada transacción independientemente del monto", 
#   x = "Hora del Día (Formato 24h)", y = "Número de Ventas (Frecuencia)".
# - Agrega `theme_minimal()`.
# - Personaliza `theme()` con: plot.title(face = "bold", size = 14, 
#   color = "#333333"), plot.subtitle(color = "#777777", face = "italic"), 
#   panel.grid.minor = element_blank(), 
#   y panel.grid.major.x = element_blank().


# ==========================================
# PROBLEMA - DETERMINAR EL NÚMERO DE VENTAS POR HORA PROMEDIO AL DÍA

# 1. Extraer la fecha (sin la hora) para poder agrupar por día específico
ventas_horas <- ventas_horas %>%
  mutate(
    Dia_Venta = date(Fecha) # date() extrae únicamente la fecha (Año-Mes-Día)
  )

# 2. Contar el total de ventas para cada hora en cada día individual
ventas_por_dia_hora <- ventas_horas %>%
  group_by(Dia_Venta, Hora_Venta) %>%
  summarise(numVentas = n(), .groups = "drop") # n() cuenta las filas (transacciones)

# 3. Calcular el promedio de esas ventas para cada hora a lo largo de todos los días
promedio_por_hora <- ventas_por_dia_hora %>%
  group_by(Hora_Venta) %>%
  summarise(Promedio_Ventas = mean(numVentas), .groups = "drop")

# Ver la tabla resultante
head(promedio_por_hora)

# 4. Generar el gráfico del promedio
ggplot(promedio_por_hora, aes(x = Hora_Venta, y = Promedio_Ventas)) +
  # Usamos geom_col() en lugar de geom_bar() porque ya tenemos el valor Y (el promedio calculado)
  geom_col(fill = "mediumseagreen", color = "white", alpha = 0.9) + 
  scale_x_continuous(breaks = 8:20) + 
  
  # Etiquetas y títulos descriptivos
  labs(
    title = "Promedio de Transacciones por Hora del Día",
    subtitle = "Volumen promedio de ventas en un día típico",
    x = "Hora del Día (Formato 24h)",
    y = "Promedio de Ventas"
  ) +
  theme_minimal() +
  
  # Personalización estética
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#333333"),
    plot.subtitle = element_text(color = "#777777", face = "italic"),
    panel.grid.minor = element_blank(),  
    panel.grid.major.x = element_blank() 
  )

# ¿Notamos alguna inconsistencia?
# Observar el df antes de promediar
head(ventas_por_dia_hora, 10)

# 2. Modificar el paso 2: Contar el total de ventas, agrupar y RELLENAR los ceros faltantes
ventas_por_dia_hora <- ventas_horas %>%
  group_by(Dia_Venta, Hora_Venta) %>%
  summarise(numVentas = n(), .groups = "drop") %>%
  # La siguiente línea cruza todos los días existentes con las horas del 8 al 20. 
  # Si la combinación no existe, le asigna un 0 en numVentas.
  complete(Dia_Venta, Hora_Venta = 8:20, fill = list(numVentas = 0))

# Actúa como un experto en análisis de datos con R. Escribe un script
# utilizando dplyr, tidyr, lubridate y ggplot2, asumiendo que ya existe 
# un dataframe llamado `ventas_horas`.
#
# 1. PROBLEMA - DETERMINAR EL NÚMERO DE VENTAS POR HORA PROMEDIO AL DÍA:
# - Sobrescribe el dataframe `ventas_horas` usando `mutate()` para crear 
#   la columna `Dia_Venta` usando `date(Fecha)` para extraer solo la fecha.
#
# 2. CONTAR Y RELLENAR CEROS FALTANTES:
# - Crea el dataframe `ventas_por_dia_hora` a partir de `ventas_horas`.
# - Agrupa por `Dia_Venta` y `Hora_Venta`.
# - Resume usando `numVentas = n()` y `.groups = "drop"`.
# - INSTRUCCIÓN ESTRICTA: En el mismo pipeline, usa la función `complete()`
#   para cruzar todos los días con las horas del 8 al 20. 
#
# 3. CALCULAR EL PROMEDIO DE VENTAS:
# - Crea `promedio_por_hora` a partir de `ventas_por_dia_hora`.
# - Agrupa por `Hora_Venta`.
# - Resume usando `Promedio_Ventas = mean(numVentas)` y `.groups = "drop"`.
#
# 4. GENERAR EL GRÁFICO DEL PROMEDIO:
# - Genera un histograma usando ggplot con el dataset `promedio_por_hora`.
# - Estética principal: `aes(x = Hora_Venta, y = Promedio_Ventas)`.
# - Usa `geom_col(fill = "mediumseagreen", color = "white", alpha = 0.9)`. 
#   Agrega un comentario explicando que usamos geom_col() porque ya 
#   tenemos el valor Y (el promedio).
# - Agrega `scale_x_continuous(breaks = 8:20)`.
# - En `labs()` define: title = "Promedio de Transacciones por Hora del 
#   Día", subtitle = "Volumen promedio de ventas en un día típico", 
#   x = "Hora del Día (Formato 24h)", y = "Promedio de Ventas".
# - Agrega `theme_minimal()`.
# - Personaliza `theme()` con: plot.title(face = "bold", size = 14, 
#   color = "#333333"), plot.subtitle(color = "#777777", face = "italic"), 
#   panel.grid.minor = element_blank(), 
#   y panel.grid.major.x = element_blank().

# ==========================================
# CHALLENGE 1 - DETERMINAR EL MONTO TOTAL DE VENTAS POR MES

# Pista: Será necesario extraer el mes de la columna 'Fecha' 
# y luego agrupar por ese mes para sumar los totales de ventas.

# Escribe tu interpretación del gráfico o los datos y envíala por correo
# O agrégala como comentario al finalizar tu ejercicio

# ==========================================
# CHALLENGE 2 - GENERAR UN GRÁFICO QUE NOS MUESTRE LA DISTRIBUCIÓN DEL MONTO DE VENTAS

# Esto nos ayudará a identificar los montos más y menos usuales de venta, 
# si hay montos extremos o si la mayoría de las ventas se concentran en un rango específico.

# Pista: Para esto, podemos usar un histograma que nos muestre la frecuencia de 
# los Totales de las transacciones. 

# Escribe una interpretación del gráfico o los datos y envíala por correo
# O agrégala como comentario al finalizar tu ejercicio