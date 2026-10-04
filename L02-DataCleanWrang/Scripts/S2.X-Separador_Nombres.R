library(tidyverse)
library(stringr)

# ==========================================
# FUNCIÓN PARA SEPARAR NOMBRES EN COLUMNAS

separar_nombres <- function(vector_nombres) {
  
  # Lista de conectores comunes (Agregar más si es necesario)
  conectores <- c("de", "del", "la", "las", "el", "los", "y", "san", "santa", "mc", "mac", "van", "von")
  
  # Función interna para procesar un solo nombre
  procesar_un_nombre <- function(nombre_completo) {
    
    
    nombre_limpio <- str_squish(nombre_completo) # Limpiar espacios extra
    partes <- str_split(nombre_limpio, " ")[[1]] # Dividir por espacios simples
    
    # Si viene vacío, regresar NA
    if (length(partes) == 0 || (length(partes) == 1 && partes[1] == "")) {
      return(tibble(PrimerNombre = NA, NombreMedio = NA, ApellidoPaterno = NA, ApellidoMaterno = NA))
    }
    
    # 2. Agrupar conectores con la palabra que les sigue
    partes_agrupadas <- c()
    i <- 1
    
    while (i <= length(partes)) {
      actual <- partes[i]
      
      # Mientras la palabra actual sea un conector y no sea la última palabra del string
      while (tolower(partes[i]) %in% conectores && i < length(partes)) {
        i <- i + 1
        actual <- paste(actual, partes[i], sep = " ")
      }
      
      partes_agrupadas <- c(partes_agrupadas, actual)
      i <- i + 1
    }
    
    n <- length(partes_agrupadas)
    
    # 3. Asignar las partes agrupadas a las columnas según cuántas partes hay
    if (n == 1) {
      tibble(PrimerNombre = partes_agrupadas[1], NombreMedio = NA, 
             ApellidoPaterno = NA, ApellidoMaterno = NA)
      
    } else if (n == 2) {
      # Ejemplo: "José Pérez"
      tibble(PrimerNombre = partes_agrupadas[1], NombreMedio = NA, 
             ApellidoPaterno = partes_agrupadas[2], ApellidoMaterno = NA)
      
    } else if (n == 3) {
      # Ejemplo: "José Pérez Gómez"
      tibble(PrimerNombre = partes_agrupadas[1], NombreMedio = NA, 
             ApellidoPaterno = partes_agrupadas[2], ApellidoMaterno = partes_agrupadas[3])
      
    } else if (n == 4) {
      # Ejemplo: "José de Jesús Montes de Oca"
      tibble(PrimerNombre = partes_agrupadas[1], NombreMedio = partes_agrupadas[2], 
             ApellidoPaterno = partes_agrupadas[3], ApellidoMaterno = partes_agrupadas[4])
      
    } else {
      # Para 5 o más, agrupamos todos los nombres del medio juntos
      # Ejemplo: "María de los Ángeles de la Cruz del Toro" (Aquí n=4 gracias a los conectores, pero si fueran más...)
      tibble(
        PrimerNombre = partes_agrupadas[1],
        NombreMedio = paste(partes_agrupadas[2:(n-2)], collapse = " "),
        ApellidoPaterno = partes_agrupadas[n-1],
        ApellidoMaterno = partes_agrupadas[n]
      )
    }
  }
  
  # Aplicar la función a cada elemento del vector y unir los resultados en un DataFrame
  map_dfr(vector_nombres, procesar_un_nombre)
}

# ==========================================
# APLICACIÓN EJEMPLO

# Directorio de trabajo en el que se encuentra el archivo
# Shortcut para cambiarlo Ctrl+ Shift + H
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L02-DataCleanWrang/data")

# Leer el archivo CSV generado
df_nombres <- read_csv("nombres.csv")

# Extraer el vector de nombres y aplicarle nuestra función
resultados_separados <- separar_nombres(df_nombres$Nombre)

# Unir la tabla original con las nuevas columnas separadas
df_final <- bind_cols(df_nombres, resultados_separados)

# Explorar los resultados

# Ver los primeros 10 casos
print("Primeros 10 casos (Regulares):")
print(head(df_final, 10))

# Ver los últimos 10 casos
print("Últimos 10 casos (Extremos y sucios):")
print(tail(df_final, 10))

# Opcional: Exportar los resultados a un nuevo archivo CSV
write_excel_csv(df_final, "nombres_separados.csv")
