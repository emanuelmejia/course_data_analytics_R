# Instalación del paquete (solo la primera vez)
# install.packages("skimr")
library(skimr)
library(tidyverse)
library(stringi)

# CTRL + Shift + H para seleccionar carpeta con datos
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L02-DataCleanWrang/data")

# Lectura de df
df_veh <- read_csv("vehiculos_2025.csv")
df_veh

# La función skim() genera un reporte de perfilado
perfilado_inicial <- skim(df_veh)

# Mostrar el reporte
print(perfilado_inicial)

df_telefonos_largos <- df_veh %>%
  # str_count con la expresión regular "\\d" cuenta ESTRICTAMENTE los números,
  # ignorando si el texto tiene espacios, guiones o paréntesis.
  filter(str_count(Teléfono, "\\d") > 10) %>%
  
  # Seleccionamos solo las columnas de interés para visualizarlo más limpio
  select(Indemnización, Nombre, Teléfono)

# Mostrar cuántos registros con error existen
cat("Total de teléfonos excedidos encontrados:", nrow(df_telefonos_largos), "\n\n")

# Mostrar una muestra de los resultados en la consola
print(head(df_telefonos_largos, 15))

df_veh <- df_veh %>%
  mutate(
    # str_replace_all() elimina TODO lo que no sea un número (\\D). Código universal Regex 
    # Esto elimina guiones, paréntesis, espacios y letras.
    # Solo deja los dígitos puros.
    Tel_Limpio = str_replace_all(Teléfono, "\\D", ""),
    
    # str_sub() extrae desde el primer caracter (1) hasta el décimo (10).
    # Si la cadena ya medía 10, se queda igual. Si medía 13, le quita los 3 finales.
    Tel_Recortado = str_sub(Tel_Limpio, start = 1, end = 10)
  )

skim(df_veh)

print(
  df_veh %>% 
    # Filtramos para ver solo aquellos que originalmente tenían más de 10 caracteres numéricos
    filter(str_count(Tel_Recortado, "\\d") < 10) %>%
    select(Original = Teléfono, Limpio = Tel_Limpio, Resultado_Final = Tel_Recortado) %>% 
    head(10)
)

df_veh <- df_veh %>%
  mutate(
    # str_replace_all busca la letra "O" y la sustituye por el número "0"
    Tel_Corregido = str_replace_all(Teléfono, "O", "0"),
    Tel_Limpio = str_replace_all(Tel_Corregido, "\\D", ""),
    Tel_Recortado = str_sub(Tel_Limpio, start = 1, end = 10)
  )

skim(df_veh)

# Sobrescribimos el dataframe excluyendo las 3 columnas que no se usarán de teléfono
df_veh <- df_veh %>%
  select(-c(Teléfono, Tel_Limpio, Tel_Corregido)) %>%
  rename(Tel = Tel_Recortado)

skim(df_veh)

head(df_veh, 10)

df_veh <- df_veh %>%
  mutate(
    # str_remove_all(Nombre, "\\d") borra cualquier dígito (0-9)
    # str_squish() limpia los espacios dobles o residuales que queden
    Nombre = str_squish(str_remove_all(Nombre, "\\d"))
  )

df_veh

# Filtramos los registros que contengan al menos un caracter extraño
df_nombres_sucios <- df_veh %>%
  # La expresión regular busca cualquier cosa que NO (^) sea una letra o un espacio
  filter(str_detect(Nombre, "[^A-Za-záéíóúÁÉÍÓÚñÑüÜ ]")) %>%
  select(Nombre)

# Mostrar cuántos registros tienen caracteres raros
cat("Nombres con caracteres no permitidos encontrados:", nrow(df_nombres_sucios), "\n\n")

# Ver una muestra de los nombres que tienen el problema
print(head(df_nombres_sucios, 15))

df_veh <- df_veh %>%
  mutate(
    # Reemplazamos cualquier cosa que NO sea letra (incluyendo acentos/ñ) o espacio con "" (nada)
    Nombre = str_squish(str_replace_all(Nombre, "[^A-Za-záéíóúÁÉÍÓÚñÑüÜ ]", ""))
  )

df_veh

# Comprobamos si quedó algún registro con errores
errores_finales <- df_veh %>%
  filter(str_detect(Nombre, "[^A-Za-záéíóúÁÉÍÓÚñÑüÜ ]"))

errores_finales

# Si queremos eliminar cualquier acento, tilde o diéresis de cualquier idioma
df_veh <- df_veh %>%
  mutate(
    # Esto borra automáticamente CUALQUIER acento, tilde o diéresis de cualquier idioma
    # Así como cualquier otro caracter internacional
    Nombre = stri_trans_general(Nombre, id = "Latin-ASCII")
  )
