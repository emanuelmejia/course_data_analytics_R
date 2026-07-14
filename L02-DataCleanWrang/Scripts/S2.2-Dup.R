# Instalación del paquete (solo la primera vez)
# install.packages("skimr")
library(skimr)
library(tidyverse)

# CTRL + Shift + H para seleccionar carpeta con datos
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L02-DataCleanWrang/data")

# ==========================================
# REPORTE INICIAL

# Lectura de df
df <- read_csv("ventasB126.csv")
df

# La función skim() genera un reporte de perfilado
perfilado_inicial <- skim(df)

# Mostrar el reporte
print(perfilado_inicial)

# ==========================================
# DUPLICADOS EXACTOS

# Identificar duplicados exactos (Clones 100% idénticos)
duplicados_exactos <- df %>%                            # Cambiar nombres de esta sección si es necesario
  mutate(Email = tolower(Email),                        # Normalizar los textos (ej. pasar a minúsculas)
         Nombre_Cliente = tolower(Nombre_Cliente)) %>%  # Para evitar diferenciadores falsos
  group_by(across(everything())) %>%                    # Agrupar filas con valores idénticos en una o varias columnas y convertirlas en una sola fila de resumen
  filter(n() > 1) %>%                                   # Filtrar únicamente los que se repitan
  ungroup() %>%                                         # Desagrupar para evitar problemas en pasos posteriores
  arrange(ID_Transaccion)                               # Acomodarlos por ID de transacción para que aparezcan juntos

duplicados_exactos

cat("-> Se han detectado", nrow(duplicados_exactos), "filas involucradas en duplicidad exacta.\n")

# Limpieza de duplicados exactos
df_sin_clones <- df %>%                                    # Cambiar nombres de esta sección si es necesario
  mutate(Email = tolower(Email),                        # Normalizar los textos (ej. pasar a minúsculas)
         Nombre_Cliente = tolower(Nombre_Cliente)) %>%  # Para evitar diferenciadores falsos
  distinct()                                            # La función distinct() borra filas donde todos los valores sean idénticos

df_sin_clones

cat("Total de filas restantes después de eliminar duplicados exactos:", nrow(df_sin_clones), "\n\n")

# ==========================================
# DUPLICADOS PARCIALES

# Identificar duplicados parciales (Errores de captura o fusión)
# Ejemplo: Cuando coincide Cliente, Fecha y Monto, pero varía el ID de Transacción
duplicados_parciales <- df_sin_clones %>%                    # Cambiar nombres de esta sección si es necesario
  group_by(ID_Cliente, Fecha_Transaccion, Monto_MXN) %>%  # Agrupamos únicamente por las columnas que definen la lógica
  filter(n() > 1) %>%                                     # Filtrar únicamente los que se repitan
  ungroup() %>%                                           # Desagrupar
  arrange(ID_Cliente, Fecha_Transaccion)                  # Acomodarlos por ID de para que aparezcan juntos

duplicados_parciales

cat("-> Se han detectado", nrow(duplicados_parciales), "filas que son duplicados parciales.\n\n")   

# Limpieza de duplicados Parciales
df_sin_dup <- df_sin_clones %>%                             # Cambiar nombres de esta sección si es necesario
  distinct(ID_Cliente, Fecha_Transaccion, Monto_MXN,    # Borra filas donde los valores de las columnas seleccionadas sean idénticos 
           .keep_all = TRUE)                            # Retener la primera ocurrencia de cada caso

cat("Total de filas restantes después de limpiar:", nrow(df_sin_dup))

# ==========================================
# DUPLICADOS CON RESTRICCIONES ESPECÍFICAS

skim(df_sin_dup)

clientes_unicos <- df_sin_dup %>%
  summarise(Unicos = n_distinct(Nombre_Cliente))

ID_unicos <- df_sin_dup %>%
  summarise(Unicos = n_distinct(ID_Cliente))

cat("Número total de nombres de clientes únicos:", clientes_unicos$Unicos, "\n")

cat("Número total de IDs de clientes únicos:", ID_unicos$Unicos, "\n")

# Verifiquemos el primer caso de inconsistencia: un mismo ID de cliente para diferentes nombres
clientes_inconsistentes <- df_sin_dup %>%
  group_by(ID_Cliente) %>%                    # Agrupamos por la columna específica
  filter(n_distinct(Nombre_Cliente) > 1) %>%  # Filtramos los grupos donde haya más de un nombre distinto
  select(ID_Cliente, Nombre_Cliente) %>%      # Nos quedamos solo con las columnas de interés para visualizar el problema
  distinct() %>%                              # Usamos distinct() para ver las variaciones exactas de los nombres sin repetir cada transacción
  arrange(ID_Cliente)

clientes_inconsistentes

# Corregimos los casos de inconsistencia manualmente, asignando un ID único a los clientes observados
df_actualizado <- df_sin_dup %>%
  mutate(ID_Cliente = case_when(
    Nombre_Cliente == "carlos mendoza" ~ "CLI-1234",   # Cliente 1: Si es carlos mendoza, asignar CLI-1234
    Nombre_Cliente == "ricardo soto"   ~ "CLI-9876",   # Cliente 2: Si es ricardo soto, asignar CLI-9876
    TRUE                               ~ ID_Cliente    # Caso por defecto: Para cualquier otro cliente, mantener su ID original
  ))

# Validamos que la corrección haya sido efectiva
validar_clientes <- df_actualizado %>%
  group_by(ID_Cliente) %>%                    # Agrupamos por la columna específica
  filter(n_distinct(Nombre_Cliente) > 1) %>%  # Filtramos los grupos donde haya más de un nombre distinto
  select(ID_Cliente, Nombre_Cliente) %>%      # Nos quedamos solo con las columnas de interés para visualizar el problema
  distinct() %>%                              # Usamos distinct() para ver las variaciones exactas de los nombres sin repetir cada transacción
  arrange(ID_Cliente)

validar_clientes

# Verifiquemos el segundo caso de inconsistencia: un mismo nombre de cliente para diferentes IDs
clientes_multID <- df_actualizado %>%
  group_by(Nombre_Cliente, Email) %>%           # Agrupamos por los campos que definen de forma única a la persona física
  filter(n_distinct(ID_Cliente) > 1) %>%        # Filtramos si esa misma persona tiene más de un ID_Cliente distinto asignado
  select(Nombre_Cliente, Email, ID_Cliente) %>% # Seleccionamos las columnas para visualizar el problema
  distinct() %>%                                # Eliminamos repeticiones transaccionales para ver solo el mapeo de IDs
  arrange(Nombre_Cliente)

clientes_multID

# Corregimos los casos de inconsistencia para todo el df, asignando un ID único a los clientes observados
df_validado <- df_actualizado %>%
  group_by(Nombre_Cliente, Email) %>%
  mutate(ID_Cliente = first(ID_Cliente)) %>%    # Sobrescribimos el ID_Cliente con el primer valor que aparezca para este grupo
  ungroup()                                     # Desagrupamos para evitar problemas en pasos posteriores

# Validamos que la corrección haya sido efectiva
validacion_final <- df_validado %>%
  group_by(Nombre_Cliente, Email) %>%
  filter(n_distinct(ID_Cliente) > 1) %>%
  ungroup()

validacion_final

# La función skim() genera un reporte de perfilado
perfilado_final <- skim(df_validado)

# Mostrar el reporte
print(perfilado_final)

df_validado

# Algo más que nos esté haciendo falta?
df_limpio <- df_validado %>%
  mutate(Nombre_Cliente = str_to_title(Nombre_Cliente))  #

df_limpio

# Guardado final en CSV si se va a requerir llevarlo a otro sistema
# write_csv(df_final, "ventasB126_limpio.csv")

# ==========================================
# PROMPT EJEMPLO PARA ESTE CUADERNO DE TRABAJO

# Eres un experto en R. Escribe un script completo, utilizando el archivo de entrada CSV llamado "ventasB126.csv"
# para realizar estrictamente los siguientes pasos en orden. 
# Incluye mensajes en la consola (usando cat()) para poder auditar el proceso al ejecutarlo:


# 1. Carga los datos y utiliza la función skim() de skimr para mostrar un resumen del dataset original. 
# 2. Normaliza los textos de las columnas Nombre_Cliente y Email a minúsculas para evitar falsos duplicados.
# 3. Identifica las filas que son 100% idénticas (clones) y elimínalas del dataset.
# 4. Identifica y elimina duplicados lógicos (asume una colisión donde coinciden el ID_Cliente, la Fecha_Transaccion y el Monto_MXN, ignorando diferencias en las demás columnas). Conserva solo la primera aparición.
# 5. Identifica si existen casos donde un mismo ID_Cliente tiene asociados distintos Nombre_Cliente y muestra una lista de ellos
# 6. Identifica si un mismo cliente físico (basado en la coincidencia exacta de Nombre_Cliente y Email) tiene asignados múltiples ID_Cliente. Corrige esto forzando a que todos sus registros adopten el primer ID de cliente que se le asignó.
# 7. Ejecuta nuevamente skim() sobre el dataframe completamente limpio para validar.
# 8. Guarda el dataframe resultante en un nuevo archivo llamado "ventasB126_limpio.csv" utilizando write_csv().

