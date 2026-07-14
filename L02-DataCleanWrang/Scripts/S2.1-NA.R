# Instalación de paquete (solo la primera vez)
# install.packages("skimr") # Cambiar el nombre si es otra librería la que hace falta
library(skimr)
library(tidyverse)
library(zoo)

# CTRL + Shift + H para seleccionar carpeta con datos
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L02-DataCleanWrang/data")

# ==========================================
# DATASET VEHÍCULOS
# Lectura de df
df_veh <- read_csv("vehiculos_2025.csv")
df_veh

# ==========================================
# MANIPULACION INICIAL

# Renombrar columnas
df_veh <- df_veh %>%
  rename(
    Siniestro = `Número de Siniestro`,
    Tel = Teléfono,
    Monto = Indemnización
  )

df_veh <- df_veh %>%
  mutate(
    # ymd() asegura que R entienda el texto como fecha
    Fecha = ymd(Fecha)
  )

# La función skim() genera un reporte de perfilado
perfilado_inicial_veh <- skim(df_veh)

# Mostrar el reporte
print(perfilado_inicial_veh)

# ===============================================
# TAREA - OBSERVAR LA CANTIDAD MENSUAL DE SINIESTROS

# Eliminamos columnas completas con NAs si no las requeriremos
df_siniestros <- df_veh %>% select(-c(Nombre, Tel))

# replace_na busca los NAs en la columna y los cambia por el valor que le indiques
df_siniestros <- df_siniestros %>%
  mutate(
    Modelo = replace_na(Modelo, "Desconocido"),
    Color  = replace_na(Color, "Desconocido")
  )

df_siniestros

# La función skim() genera un reporte de perfilado
perfilado_siniestros <- skim(df_siniestros)

# Mostrar el reporte
print(perfilado_siniestros)

# Transformar los datos: Extraer el mes y hacer el conteo

df_conteo_mes <- df_siniestros %>%
  mutate(
    # month(..., label = TRUE) convierte "01" en "Enero", "02" en "Febrero", etc.
    Mes = month(Fecha, label = TRUE, abbr = FALSE) 
  ) %>%
  count(Mes, name = "Total_Siniestros")

df_conteo_mes

# Generar un gráfico de barras
grafico_meses <- ggplot(data = df_conteo_mes, aes(x = Mes, y = Total_Siniestros)) +
  geom_col(fill = "dodgerblue", alpha = 0.9) +
  # Agregamos las etiquetas de datos justo adentro de cada barra
  geom_text(aes(label = Total_Siniestros), vjust = 1, size = 4, fontface = "bold", color = "white") +
  # Títulos y etiquetas descriptivas
  labs(
    title = "Total de Siniestros por Mes (2025)",
    subtitle = "Concentración de accidentes vehiculares a lo largo del año",
    x = "Mes de Ocurrencia",
    y = "Cantidad de Siniestros"
  ) +
  # Tema limpio y ajustes de texto
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11, face = "bold"), # Etiquetas de meses
    panel.grid.major.x = element_blank() # Quitamos las líneas verticales de fondo para mayor limpieza
  )

# Mostrar el gráfico final
print(grafico_meses)

# ===============================================
# TAREA - ESTIMAR UN MONTO POR SINIESTROS EN EL AÑO

# Extraemos la columna directamente y la sumamos removiendo NAs actuales
total_monto <- sum(df_siniestros$Monto, na.rm = TRUE)
total_monto

# ¿Pero realmente queremos quitar los NAs?

# Obtener el promedio removiendo los NAs actuales
promedio_monto <- mean(df_siniestros$Monto, na.rm = TRUE)

cat("El monto promedio de siniestro:", round(promedio_monto, 2), "\n")

# Rellenamos los NAs con el promedio calculado
df_siniestros_promedio <- df_siniestros %>%
  mutate(
    Monto = ifelse(is.na(Monto), promedio_monto, Monto)
  )

# Validamos que ya no queden NAs
sum(is.na(df_siniestros_promedio))

# Calculamos nuevamente el monto total estimado
estimado_monto <- sum(df_siniestros_promedio$Monto, na.rm = TRUE)
estimado_monto

# ==========================================
# PROMPT EJEMPLO

# Eres un experto en R. Escribe un script completo, utilizando el archivo de entrada CSV llamado "vehiculos_2025.csv"
# para realizar estrictamente los siguientes pasos en orden. 
# Incluye mensajes en la consola (usando cat()) para poder auditar el proceso al ejecutarlo:


# 1. Carga el archivo CSV en un dataframe llamado df_veh.
# 2. Renombra las siguientes columnas: de Número de Siniestro a Siniestro, de Teléfono a Tel, y de Indemnización a Monto.
# 3. Convierte la columna Fecha a formato fecha utilizando la función ymd().
# 4. Genera e imprime un reporte de perfilado inicial del dataframe utilizando la función skim().
# 5. Crea un nuevo dataframe llamado df_siniestros a partir de df_veh, eliminando las columnas Nombre y Tel.
# 6. Reemplaza los valores NA de las columnas Modelo y Color con la palabra "Desconocido". Muestra el dataframe resultante.
# 7. Genera e imprime un nuevo reporte de perfilado para este dataframe utilizando skim().
# 8. Calcula e imprime la suma total de la columna Monto de df_siniestros removiendo los NAs en una variable total_monto.
# 9. Calcula el promedio de la columna Monto removiendo los NAs en una variable promedio_monto. Imprime este promedio usando la función cat(), redondeado a 2 decimales con el texto: "El monto promedio de siniestro: [valor]".
# 10.Crea un nuevo dataframe df_siniestros_promedio rellenando los valores NA de la columna Monto con el promedio_monto calculado (usa ifelse).
# 11.Valida que ya no queden NAs en todo el dataframe usando sum(is.na()).
# 12.Calcula y muestra una variable final llamada estimado_monto con la suma de la columna Monto del nuevo dataframe.

# OPCIONALES (Gráfico):
# 13.Crea un dataframe df_conteo_mes que extraiga el mes de la Fecha (con el nombre completo del mes, no abreviado) y cuenta la cantidad de siniestros por mes en una columna llamada Total_Siniestros. Muestra el resultado.
# 14.Crea un gráfico de barras con ggplot2 usando df_conteo_mes con el siguiente formato:
#   - Las barras deben ser color "dodgerblue" con una transparencia (alpha) de 0.9.
#   - Agrega etiquetas de datos en texto color blanco, en negritas, tamaño 4, colocadas justo adentro del tope de cada barra (vjust = 1).
#   - Título: "Total de Siniestros por Mes (2025)".
#   - Subtítulo: "Concentración de accidentes vehiculares a lo largo del año".
#   - Eje X: "Mes de Ocurrencia". Eje Y: "Cantidad de Siniestros".
#   - Usa theme_minimal(). 
#   - Modifica el tema para que el título esté en negritas y tamaño 16. El texto del eje X debe estar rotado 45 grados, alineado a la derecha, en negritas y tamaño 11. Elimina las líneas verticales de fondo (panel grid major x). 
# 15.Imprime el gráfico.

# ==========================================
# DATASET CO2

# Lectura de df
df_co2 <- read_csv("co2atm.csv")
df_co2

# ==========================================
# MANIPULACION INICIAL

# Convertir la columna fecha de string a date
df_co2 <- df_co2 %>%
  mutate(fecha = ym(fecha)) # ym = Year, Month

# Generar un gráfico inicial
grafico_co2 <- ggplot(data = df_co2, aes(x = fecha, y = ppm)) +
  geom_line(color = "blueviolet", linewidth = 0.8)

print(grafico_co2)

# La función skim() genera un reporte de perfilado
perfilado_inicial_co2 <- skim(df_co2)

# Mostrar el reporte
print(perfilado_inicial_co2)

# ==========================================
# TRATAMIENTO INICIAL DE NA

df_co2 <- df_co2 %>%
  filter(                                    # Filtrar únicamente datos válidos
    row_number() >= min(which(!is.na(ppm))), # Filtra desde el primer dato que no contenga NA en PPM
    row_number() <= max(which(!is.na(ppm)))  # Filtra hasta el último dato que no contenga NA en PPM
  )

# ==========================================
# PROMEDIO DIRECTO

# Calculamos el promedio removiendo los NAs actuales
promedio_ppm <- mean(df_co2$ppm, na.rm = TRUE)

cat("El valor promedio de ppm de CO2 en la atmósfera es:", round(promedio_ppm, 2), "\n")

# Rellenamos los NAs con el promedio calculado
df_co2_promedio <- df_co2 %>%
  mutate(
    ppm = ifelse(is.na(ppm), promedio_ppm, ppm)
  )

# Validamos que ya no queden NAs
sum(is.na(df_co2_promedio))

# Generar el gráfico resultante
grafico_promedio <- ggplot(data = df_co2_promedio, aes(x = fecha, y = ppm)) +
  geom_line(color = "blueviolet", linewidth = 0.8)

print(grafico_promedio)

# ==========================================
# INTERPOLACIÓN USANDO PUNTO VÁLIDO ANTERIOR Y SIGUIENTE

# na.approx busca el último valor válido antes del NA y el primero después, 
# y traza una línea recta (promedio) entre ellos.
df_co2_interpolado <- df_co2 %>%
  mutate(
    ppm = na.approx(ppm, na.rm = FALSE) 
  )

# Validamos que ya no queden NAs
sum(is.na(df_co2_interpolado))

# Generar el gráfico resultante
grafico_interpolado <- ggplot(data = df_co2_interpolado, aes(x = fecha, y = ppm)) +
  geom_line(color = "blueviolet", linewidth = 0.8)

print(grafico_interpolado)

# ==========================================
# PROMPT EJEMPLO

# Eres un experto en R. Escribe un script completo, utilizando el archivo de entrada CSV llamado "co2atm.csv"
# para realizar estrictamente los siguientes pasos en orden. 
# Incluye mensajes en la consola (usando cat()) para poder auditar el proceso al ejecutarlo:
#
# 1. Lee el archivo CSV en un dataframe llamado df_co2 y muéstralo.
# 2. Convierte la columna fecha de texto a formato fecha utilizando la función ym() (año y mes) de lubridate.
# 3. Genera y muestra un gráfico de líneas llamado grafico_co2 usando ggplot2. Mapea fecha en el eje X y ppm en el eje Y. La línea debe tener el color "blueviolet" y un grosor (linewidth) de 0.8.
# 4. Genera e imprime un reporte de perfilado del dataframe utilizando la función skim(), guardándolo en la variable perfilado_inicial_co2.
# 5. Filtra el dataframe df_co2 para eliminar los valores NA que estén al principio y al final de la serie de datos.
# 6. Calcula el promedio de la columna ppm removiendo los NAs y guárdalo en la variable promedio_ppm.
# 7. Imprime este promedio redondeado a 2 decimales usando la función cat()
# 8. Crea un nuevo dataframe llamado df_co2_promedio mutando la columna ppm para rellenar sus NAs con el promedio_ppm calculado (usa ifelse).
# 9. Valida e imprime que ya no queden NAs usando sum(is.na()) sobre el nuevo dataframe.
# 10.Genera y muestra un gráfico llamado grafico_promedio con este nuevo dataframe, usando exactamente la misma estética del gráfico anterior (línea color "blueviolet", linewidth = 0.8).
# 11.A partir del df_co2 original (el que filtramos al inicio), crea un nuevo dataframe llamado df_co2_interpolado.
# 12.Muta la columna ppm para rellenar los NAs utilizando interpolación lineal con la función na.approx() de la librería zoo (usar na.rm = FALSE).
# 13.Valida e imprime que ya no queden NAs usando sum(is.na()).
# 14.Genera y muestra un último gráfico con este dataframe interpolado, manteniendo la misma estética (línea color "blueviolet", linewidth = 0.8).