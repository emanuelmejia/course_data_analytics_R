library(tidyverse)
library(lubridate)
library(skimr)

# CTRL + Shift + H para seleccionar carpeta con datos
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L02-DataCleanWrang/data")

# ==========================================
# TRANSFORMACIONES POR CALENDARIO

# Leemos desde web
www <- "https://raw.githubusercontent.com/ricardoscr/UW-Data-Science-Certificate/master/02-Methods/CADairyProduction.csv"

# Guardamos en una variable la lectura
prod <- read.csv(www, header = T)
head(prod)

# ==========================================
# PERFIL INICIAL

# La función skim() genera un reporte de perfilado
perfilado_inicial_prod <- skim(prod)

# Mostrar el reporte
print(perfilado_inicial_prod)

# ==========================================
# MANIPULACION INICIAL

prod <- prod %>%
  mutate(
    # Une las columnas Year y Month para que quede una única columna de fecha
    fecha = ym(str_c(Year, Month, sep = "-"))
  )
head(prod)

# Seleccionamos solo columnas requeridas
df_leche <- prod %>% select(c(fecha, Milk.Prod))

# Gráfico Inicial
plot(prod$Milk.Prod, 
     main='Producción de Leche (L)', 
     ylab='Producción de Leche',
     type = "l", col = "blue")

# ==========================================
# TRANSFORMACIÓN PROMEDIO DIARIO
df_leche_transf <- df_leche %>%
  transmute(
    fecha      = fecha,
    Mensual    = Milk.Prod,
    PromDiario = Milk.Prod / days_in_month(fecha)
  )

head(df_leche_transf)

# Gráfico Final
plot(df_leche_transf$PromDiario, 
     main='Producción Diaria Promedio de Leche (L)', 
     ylab='Producción de Leche',
     type = "l", col = "blue")

# ==========================================
# PROMPT EJEMPLO

# Eres un experto en R. Escribe un script completo, para realizar estrictamente los siguientes pasos en orden. 
# Incluye mensajes en la consola (usando cat()) para poder auditar el proceso al ejecutarlo:
#
# 1. Guarda la URL "[https://raw.githubusercontent.com/ricardoscr/UW-Data-Science-Certificate/master/02-Methods/CADairyProduction.csv](https://raw.githubusercontent.com/ricardoscr/UW-Data-Science-Certificate/master/02-Methods/CADairyProduction.csv)" en una variable www.
# 2. Lee los datos usando la función base read.csv() con header = T. Guarda el resultado en un dataframe llamado prod.
# 3. Muestra las primeras filas del dataframe prod usando head().
# 4. Genera un reporte de perfilado del dataframe prod usando la función skim() y guarda el resultado en una variable llamada perfilado_inicial_prod y muéstralo
# 5. Agrega una nueva columna llamada fecha al dataframe prod, que combine las columnas Year y Month en un formato de fecha usando la función ym() de lubridate. Muestra las primeras filas del dataframe modificado.
# 6. Selecciona únicamente las columnas fecha y Milk.Prod del dataframe prod y guárdalo en un nuevo dataframe llamado df_leche. Muestra las primeras filas de df_leche.
# 7. Crea un gráfico de línea de la columna Milk.Prod del dataframe prod, con la función base plot(), con el título 'Producción de Leche (L)' y el eje y etiquetado como 'Producción de Leche'. Usa color azul para la línea.
# 8. Crea un nuevo dataframe llamado df_leche_transf que contenga las columnas fecha, Mensual (que es la columna Milk.Prod) y PromDiario (que es la columna Milk.Prod dividida por el número de días en el mes correspondiente usando days_in_month()). Muestra las primeras filas.
# 9. Crea un gráfico de línea de la columna PromDiario del dataframe df_leche_transf, con título 'Producción Diaria Promedio de Leche (L)' y el eje y etiquetado como 'Producción de Leche'. Usa color azul para la línea.

# ==========================================
# TRANSFORMACIONES POR POBLACIÓN
econ <- read.csv("global_economy.csv", header = T)
head(econ)

# La función skim() genera un reporte de perfilado
perfilado_inicial_econ <- skim(econ)

# Mostrar el reporte
print(perfilado_inicial_econ)

# ==========================================
# MANIPULACION INICIAL

# Seleccionamos los datos únicamente de Qatar
econQat <- econ[econ$unique_id == "Qatar",]
head(econQat)

# verificar perfil
skim(econQat)

# Remover NAs en la columna de PIB  
econQat <- econQat %>%
  filter(!is.na(GDP))

# verificar perfil limpio
skim(econQat)

# Gráfico Inicial
plot(econQat$GDP, 
     main='PIB Anual de Qatar', 
     ylab='PIB',
     type = "l", col = "darkgreen")

# ==========================================
# TRANSFORMACIÓN PER CÁPITA

econQat_transf <- econQat %>%
  transmute(
    fecha      = ds,
    PIB        = GDP,
    Poblacion  = Population,
    PIBxCapita = GDP / Population
  )

head(econQat_transf)

# Gráfico Final
plot(econQat_transf$PIBxCapita, 
     main='PIB per Cápita Anual de Qatar', 
     ylab='PIB per Cápita',
     type = "l", col = "darkgreen")

# ==========================================
# PROMPT EJEMPLO

# Eres un experto en R. Escribe un script completo, utilizando el archivo de entrada CSV llamado "global_economy.csv"
# para realizar estrictamente los siguientes pasos en orden. 
# Incluye mensajes en la consola (usando cat()) para poder auditar el proceso al ejecutarlo:
#
# 1. Lee los datos usando la función base read.csv() con header = T. Guarda el resultado en un dataframe llamado econ. Muestra las primeras filas.
# 2. Genera un reporte de perfilado del dataframe econ usando la función skim() y guarda el resultado en una variable llamada perfilado_inicial_econ. Muestra el reporte.
# 3. Filtra los datos para obtener únicamente las filas donde la columna unique_id sea igual a "Qatar"
# 4. Verifica el perfil de los datos filtrados usando skim() y muestra el resultado.
# 5. Remueve las filas donde la columna GDP sea NA usando filter() y muestra
# 6. Verifica nuevamente el perfil de los datos filtrados y limpios usando skim() y muestra el resultado.
# 7. Crea un gráfico de línea de la columna GDP del dataframe filtrado y limpio, con título 'PIB Anual de Qatar' y el eje y etiquetado como 'PIB'. Usa color verde oscuro para la línea.
# 8. Crea un nuevo dataframe llamado econQat_transf que contenga las siguientes columnas:
#   - fecha: que es la columna ds
#   - PIB: que es la columna GDP
#   - Poblacion: que es la columna Population
#   - PIBxCapita: que es la columna GDP dividida por la columna Population
# 9. Muestra las primeras filas del nuevo dataframe econQat_trans
# 10.Crea un gráfico de línea de la columna PIBxCapita del dataframe econQat_transf, con título 'PIB per Cápita Anual de Qatar' y el eje y etiquetado como 'PIB per Cápita'. Usa color verde oscuro para la línea

# ==========================================
# TRANSFORMACIONES FINANCIERAS

ret <- read.csv("aus_retail.csv", header = T)
head(ret)

# La función skim() genera un reporte de perfilado
perfilado_inicial_ret <- skim(ret)

# Mostrar el reporte
print(perfilado_inicial_ret)

# ==========================================
# MANIPULACION INICIAL

# Observando un solo tipo de industria
retIndustry <- ret[ret$Industry == "Newspaper and book retailing",]

# Convertimos la fecha en tipo fecha
retIndustry$Month <- ymd(retIndustry$Month)

# Agrupamos sumando por año
turnAus <- retIndustry %>% 
  group_by(ds = lubridate::year(ymd(Month))) %>% 
  summarize(Turnover = sum(Turnover)) %>% 
  as.data.frame()
head(turnAus)

# Gráfico Inicial
plot(turnAus$Turnover, 
     main='Facturación Retail Libros y Periódicos', 
     ylab='Facturación (AUD)',
     type = "l", col = "coral")

# ==========================================
# FUSIÓN DE TABLAS

# Obtenemos INPC de Australia desde otro csv
cpiAus <- econ[econ$unique_id == "Australia", c("ds", "CPI")]
head(cpiAus)

# Unimos ambas tablas por fecha (Inner Join)
econAus <- merge(turnAus, cpiAus, by = "ds")
head(econAus)
tail(econAus)

skim(econAus)

econAus[econAus$ds == 2010,]

# ==========================================
# TRANSFORMACIÓN AJUSTE INFLACIONARIO

econAus_transf <- econAus %>%
  transmute(
    fecha         = ds,
    Facturacion   = Turnover,
    FactAjust     = Turnover / (CPI / 100),
  )

head(econAus_transf)

# Gráfico Final
plot(econAus_transf$FactAjust, 
     main='Facturación Ajustada por Inflación', 
     ylab='Facturación Ajustada',
     type = "l", col = "coral")

# ==========================================
# PROMPT EJEMPLO

# Eres un experto en R. Escribe un script completo, utilizando el archivo de entrada CSV llamado "aus_retail.csv"
# para realizar estrictamente los siguientes pasos en orden. 
# Incluye mensajes en la consola (usando cat()) para poder auditar el proceso al ejecutarlo:
#
# 1. Lee los datos usando la función base read.csv() con header = T. Guarda el resultado en un dataframe llamado ret. Muestra las primeras filas.
# 2. Genera un reporte de perfilado del dataframe ret usando la función skim().
# 3. Filtra los datos para obtener únicamente las filas donde la columna Industry sea igual a "Newspaper and book retailing" y guarda en retIndustry.
# 4. Convierte la columna Month de retIndustry a tipo fecha usando ymd().
# 5. Agrupa los datos por año y suma la columna Turnover, guardando en turnAus. Muestra los primeros registros.
# 6. Crea un gráfico de línea de la columna Turnover del dataframe turnAus, con título 'Facturación Retail Libros y Periódicos' y el eje y etiquetado como 'Facturación (AUD)'. Usa color coral para la línea.
# 7. Lee los datos de inflación de Australia desde el dataframe econ, filtrando únicamente las filas donde unique_id sea igual a "Australia" y seleccionando las columnas ds y CPI. Guarda en cpiAus.
# 8. Une las tablas turnAus y cpiAus por la columna ds usando merge. Muestra los primeros registros.
# 9. Crea un nuevo dataframe llamado econAus_transf que contenga las siguientes columnas:
#  - fecha: que es la columna ds
#  - Facturacion: que es la columna Turnover
#  - FactAjust: que es la columna Turnover dividida por la columna CPI
# 10.Muestra las primeras filas del nuevo dataframe econAus_trans
# 11.Crea un gráfico de línea de la columna FactAjust del dataframe econAus_transf, con título 'Facturación Ajustada por Inflación' y el eje y etiquetado como 'Facturación Ajustada'. Usa color coral para la línea

# ==========================================
# TRANSFORMACIÓN LOGARÍTMICA

# Cargamos datos de pasajeros
pas <- as.vector(AirPassengers)

# Creamos tabla con datos originales y transformados
pas_df <- as.data.frame(
  cbind(Pasajeros = pas,
        logPasajeros = log(pas)))

# Gráfico Final
plot(pas_df$Pasajeros, 
     main='Reservaciones de Pasajeros en Vuelos Internacionales', 
     ylab='Pasajeros',
     type = "l", col = "deeppink")

# Gráfico Final
plot(pas_df$logPasajeros, 
     main='Reservaciones de Pasajeros - Ajuste Logarítmico', 
     ylab='log(Pasajeros)',
     type = "l", col = "deeppink")

# ==========================================
# PROMPT EJEMPLO

# Eres un experto en R. Escribe un script completo, utilizando el dataset integrado de R llamado AirPassengers
# para realizar estrictamente los siguientes pasos en orden. 
# Incluye mensajes en la consola (usando cat()) para poder auditar el proceso al ejecutarlo:

# 1. Carga los datos del dataset AirPassengers en un vector llamado pas.
# 2. Crea un dataframe llamado pas_df que contenga dos columnas:
#    - Pasajeros: que es el vector pas
#    - logPasajeros: que es el logaritmo natural del vector pas
# 3. Crea un gráfico de línea de la columna Pasajeros del dataframe pas con título 'Reservaciones de Pasajeros en Vuelos Internacionales' y el eje y etiquetado como 'Pasajeros'. Usa color deeppink para la línea
# 4. Crea un gráfico de línea de la columna logPasajeros del dataframe pas_df, con título 'Reservaciones de Pasajeros - Ajuste Logarítmico' y el eje y etiquetado como 'log(Pasajeros)'. Usa color deeppink para la línea