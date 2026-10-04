library(tidyverse)
library(skimr)

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L02-DataCleanWrang/data")

# Cargar archivos
ventas1 <- read.csv("Fventas2T.csv")
ventas2 <- read.csv("FventasJul.csv")
pagos <- read.csv("Fpagos.csv")
clientes <- read.csv("Fclientes.csv")

# ==========================================
# CONSOLIDAR TABLAS CON MISMA ESTRUCTURA
head(ventas1)
skim(ventas1)

head(ventas2)
skim(ventas2)

# Estandarizar la primera tabla
ventas1_std <- ventas1 %>%
  mutate(
    # La fecha original está en formato "YYYY-MM-DD HH:MM:SS"
    Fecha = ymd_hms(Fecha)
  )

# Procesar y estandarizar la segunda tabla
ventas2_std <- ventas2 %>%
  mutate(
    Fecha_Completa = paste(Fecha, Hora), # Unir Fecha (dd-mm-aaaa) y Hora en una sola cadena de texto
    Fecha = dmy_hms(Fecha_Completa),     # Convertir a formato de fecha y hora (datetime) usando dmy_hms
    Total = parse_number(Monto)          # Limpiar el monto con parse number. lo renombramos a "Total" para que coincida exactamente con la primera tabla
  ) %>%
  select(Fecha, Numero_factura, IDCliente, Total) # Seleccionar únicamente las columnas para hacer match en ambas tablas

# Observemos los resultados
head(ventas1_std)
skim(ventas1_std)

head(ventas2_std)
skim(ventas2_std)

# Ahora sí podemos proceder a unir ambas tablas en el orden deseado
ventas <- bind_rows(ventas1_std, ventas2_std)

# Observemos los resultados
head(ventas)
skim(ventas)

# Exportar el CSV consolidado
write.csv(ventas, "Fventas.csv", row.names = FALSE)

# ==========================================
# UNIR DOS TABLAS DE DATOS CON INFORMACIÓN DISTINTA
head(pagos, 10)
skim(pagos)

# CONVERSIÓN CLAVE: Transformar el número de factura de ventas a texto
ventas <- ventas %>%
  mutate(Numero_factura = as.character(Numero_factura))

# Desglosar la columna que contiene facturas múltiples
pagos_desglosados <- pagos %>%
  separate_rows(Factura_pagada, sep = " ") %>%
  rename(Numero_factura = Factura_pagada) %>%
  mutate(Numero_factura = as.character(Numero_factura)) # Asegurarnos de que sea también texto

# Ahora generamos la unión de ambas tablas
estado_cuenta <- ventas %>%
  left_join(pagos_desglosados, by = c("IDCliente", "Numero_factura")) %>%   # La tabla que manda es ventas
  rename(                                                                   # Tenemos dos columnas de fecha
    Fecha_Venta = Fecha.x,
    Fecha_Pago  = Fecha.y
  ) %>%
  mutate(
    Estatus = ifelse(is.na(Fecha_Pago), "Pendiente", "Pagado"),      # Agregamos una nueva columna para saber su estatus
    Saldo = ifelse(is.na(Fecha_Pago), Total, 0)                      # Saldo pendiente de pago
  ) %>%
  select(IDCliente, Numero_factura, Fecha_Venta, Total, Fecha_Pago, Estatus, Saldo)

# Mostrar las primeras filas del resultado
print(head(estado_cuenta, 10))

skim(estado_cuenta)

# Comprobar cuántas facturas quedaron pendientes y el monto total de deuda
resumen_saldos <- estado_cuenta %>%
  group_by(Estatus) %>%   # Agrupamos por estado de factura
  summarise(
    Facturas = n(),       # Conteo de facturas
    Saldo = sum(Saldo)    # Suma del Saldo Pendiente
  )

print(resumen_saldos)

# ==========================================
# UNIR UNA NUEVA TABLA PARA GENERAR NUEVOS CÁLCULOS
head(clientes)
skim(clientes)

estado_cuenta_final <- estado_cuenta %>%
  left_join(
    clientes %>% select(ID, Credito, Nombre), # Únicamente buscaremos unir las columnas de nuestro interés
    by = c("IDCliente" = "ID")                # Si las dos columnas tienen nombres distintos en las tablas
    ) %>% 
  rename(Dias_Credito = Credito) %>%
  mutate(
    Fecha_Venta_Date = as.Date(Fecha_Venta),  # Convertir las fechas a formato Date (auxiliar)
    Fecha_Pago = mdy(Fecha_Pago),             # Convertir las fechas a formato Date (permanente)
    Fecha_Corte_Calculo = coalesce(Fecha_Pago,                      # Si Fecha_Pago_Date tiene un valor, la deja igual.
                                   max(Fecha_Venta_Date, na.rm = TRUE)), # Si es NA (no pagada), inserta la fecha máxima (última) de la columna Fecha_Venta_Date 
    Dias_Transcurridos_Pago = as.numeric(Fecha_Corte_Calculo - Fecha_Venta_Date), # Calcular los días transcurridos usando la nueva fecha de corte
    Dias_Vencido = Dias_Transcurridos_Pago - Dias_Credito                         # Calcular los días de vencimiento (morosidad)
  ) %>%
  select(                                     # Organizar las columnas, quitando las auxiliares para dejar el dataframe limpio
    IDCliente, 
    Numero_factura, 
    Fecha_Venta, 
    Total, 
    Dias_Credito, 
    Fecha_Pago, 
    Estatus, 
    Saldo, 
    Dias_Transcurridos_Pago,
    Dias_Vencido
  )

# Mostrar las primeras filas
print(
  estado_cuenta_final %>% 
    head(10)
)

# Mostrar las primeras filas pendientes
print(
  estado_cuenta_final %>% 
    filter(Estatus == "Pendiente") %>% 
    head(10)
)

# Guardar el estado de cuenta en CSV
# write.csv(estado_cuenta_final, "Festado_cuenta.csv", row.names = FALSE)

# ==========================================
# GUARDAR NUEVOS CÁLCULOS AGRUPANDO EN OTRA TABLA

# Calcular el promedio de días de vencimiento por cliente a partir del nuevo estado de cuenta
promedios_clientes <- estado_cuenta_final %>%
  group_by(IDCliente) %>%
  summarise(
    Promedio_Dias_Vencido = mean(Dias_Vencido, na.rm = TRUE),   # Calculamos la media. na.rm = TRUE previene errores si hubiera algún NA suelto
    .groups = "drop"
  )

# 2. Actualizar la tabla de clientes uniéndola con los promedios
clientes_actualizado <- clientes %>%
  left_join(promedios_clientes, by = c("ID" = "IDCliente"))

# Mostrar las primeras filas para comprobar que la columna se agregó correctamente
cat("--- Tabla de Clientes Actualizada ---\n")
print(head(clientes_actualizado))

skim(promedios_clientes)


ventas_totales_cliente <- estado_cuenta_final %>%
  group_by(IDCliente) %>%
  summarise(Monto_Total_Vendido = sum(Total, na.rm = TRUE), .groups = "drop")

# 1. Unir estos nuevos datos a la tabla de clientes_actualizada
clientes_actualizado <- clientes_actualizado %>%
  left_join(ventas_totales_cliente, by = c("ID" = "IDCliente")) %>%  # Cruzar con los totales de venta
  arrange(desc(Promedio_Dias_Vencido))                               # Ordenar de mayor a menor morosidad

clientes_actualizado

# Obtener los datos de mejor y peor pagador
extremos_pagadores <- clientes_actualizado %>%
  slice(1, n()) %>%             # slice(1) trae el primero (el peor) y n() trae el último (el mejor)
  mutate(
    Perfil = c("Peor Pagador (Mayor Morosidad)", 
               "Mejor Pagador (Mayor Anticipo)")) %>%
  select(                                              # Seleccionar y ordenar las columnas para la salida
    Perfil,
    ID, 
    Nombre, 
    Zona, 
    Credito, 
    Vencimiento_Prom = Promedio_Dias_Vencido, 
    Monto_Vendido = Monto_Total_Vendido
  )

# Mostrar el resultado en consola
print(extremos_pagadores)

# ==========================================
# UNIR NUEVAMENTE VENTAS Y PAGOS CON UN PROPÓSITO DISTINTO

# 1. Resumen diario de Ventas
ventas_diarias <- ventas %>%
  mutate(Fecha_Dia = as.Date(Fecha)) %>%   # Convertir quitando las horas usando as.Date()
  group_by(Fecha_Dia) %>%
  summarise(
    Ventas = sum(Total, na.rm = TRUE),
    .groups = "drop"
  )

# Resumen diario de Pagos
pagos_diarios <- estado_cuenta_final %>%
  filter(Estatus == "Pagado") %>%          # Filtrar solo las facturas que sí tienen pago
  mutate(Fecha_Dia = Fecha_Pago) %>%  # Convertir la fecha de pago a formato Date (estaba en Mes-Día-Año)
  group_by(Fecha_Dia) %>%
  summarise(
    Pagos = sum(Total, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Unir ambas tablas por día
flujo_diario <- ventas_diarias %>%
  full_join(pagos_diarios, by = "Fecha_Dia") %>%   # Usamos full_join para mantener todos los días (con ventas, con pagos o ambos)
  mutate(                                          # Rellenar con 0 los días que tienen NA (ej. día que hubo ventas pero 0 pagos)
    Ventas = coalesce(Ventas, 0),
    Pagos = coalesce(Pagos, 0)
  ) %>%
  arrange(Fecha_Dia)

# Mostrar las primeras filas de la tabla combinada
cat("--- Tabla de Flujo Diario (Ventas vs Pagos) ---\n")
print(head(flujo_diario, 10))

# ==========================================
# PROMPT EJEMPLO
#
# Actúa como un experto en análisis de datos con R. Escribe un script
# utilizando las librerías `tidyverse` y `skimr` que realice los siguientes pasos estructurados. 
# Asegúrate de incluir los comentarios separadores (ej. # ==============================)
# y los comentarios explicativos indicados.
#
# 1. Carga de archivos:
# - Lee los siguientes 4 archivos CSV usando `read.csv()` y guárdalos en
#   variables: "Fventas2T.csv" en `ventas1`, "FventasJul.csv" en `ventas2`,
#   "Fpagos.csv" en `pagos`, y "Fclientes.csv" en `clientes`.
#
# 2. Consolidar tablas con misma estructura:
# - Muestra `head()` y `skim()` de `ventas1` y `ventas2`.
# - Estandariza la primera tabla creando `ventas1_std`: muta la columna
#   `Fecha` usando la función `ymd_hms()`.
# - Estandariza la segunda tabla creando `ventas2_std` usando un
#   pipeline (`%>%`):
#   a) Usa `mutate` para crear `Fecha_Completa` pegando `Fecha` y `Hora`
#      con `paste()`.
#   b) Convierte `Fecha` a formato datetime usando `dmy_hms(Fecha_Completa)`.
#   c) Crea una columna `Total` limpiando el monto con la función
#      `parse_number(Monto)`.
#   d) Agrega un `select()` para conservar únicamente `Fecha`,
#      `Numero_factura`, `IDCliente`, y `Total`.
# - Muestra `head()` y `skim()` de `ventas1_std` y `ventas2_std`.
# - Une ambas tablas estandarizadas usando `bind_rows()` y guárdalo en `ventas`.
# - Muestra `head()` y `skim()` de `ventas`.
#
# 3. Unir dos tablas de datos con información distinta:
# - Muestra `head(pagos, 10)` y `skim(pagos)`.
# - Muta el dataframe `ventas` para convertir `Numero_factura` a texto
#   usando `as.character()`.
# - Crea `pagos_desglosados` a partir de `pagos`: usa `separate_rows()` en la
#   columna `Factura_pagada` con el separador " ", renombra esa columna a
#   `Numero_factura`, y asegúrate de mutarla a texto con `as.character()`.
# - Crea `estado_cuenta` uniendo `ventas` con `pagos_desglosados` usando un
#   `left_join` basado en `c("IDCliente", "Numero_factura")`. En el mismo
#   pipeline:
#   a) Renombra `Fecha.x` a `Fecha_Venta` y `Fecha.y` a `Fecha_Pago`.
#   b) Muta una columna `Estatus`: usa `ifelse` para que sea "Pendiente"
#      si `Fecha_Pago` es NA, y "Pagado" si no lo es.
#   c) Muta una columna `Saldo`: usa `ifelse` para que sea el `Total`
#      si `Fecha_Pago` es NA, y 0 si no lo es.
#   d) Finaliza con un `select()` de las columnas: `IDCliente`,
#      `Numero_factura`, `Fecha_Venta`, `Total`, `Fecha_Pago`, `Estatus`,
#      y `Saldo`.
# - Muestra `print(head(estado_cuenta, 10))` y `skim(estado_cuenta)`.
# - Crea `resumen_saldos` agrupando `estado_cuenta` por `Estatus` y
#   resumiendo (`summarise`) el número de facturas usando `n()` y la suma
#   del `Saldo`. Imprímelo.
#
# 4. Unir una nueva tabla para generar nuevos cálculos:
# - Muestra `head()` y `skim()` de `clientes`.
# - Crea `estado_cuenta_final` a partir de `estado_cuenta`:
#   1) Haz un `left_join` pero pasa `clientes %>% select(ID, Credito, Nombre)`
#      usando `by = c("IDCliente" = "ID")`.
#   2) Renombra `Credito` a `Dias_Credito`.
#   3) Muta `Fecha_Venta_Date` usando `as.Date(Fecha_Venta)`.
#   4) Muta `Fecha_Pago` usando `mdy(Fecha_Pago)`.
#   5) Muta `Fecha_Corte_Calculo` usando
#      `coalesce(Fecha_Pago, max(Fecha_Venta_Date, na.rm = TRUE))`.
#   6) Muta `Dias_Transcurridos_Pago` restando `Fecha_Venta_Date` a
#      `Fecha_Corte_Calculo` y convirtiendo el resultado con `as.numeric()`.
#   7) Muta `Dias_Vencido` restando `Dias_Credito` a `Dias_Transcurridos_Pago`.
#   8) Selecciona las columnas en este orden: `IDCliente`, `Numero_factura`,
#      `Fecha_Venta`, `Total`, `Dias_Credito`, `Fecha_Pago`, `Estatus`,
#      `Saldo`, `Dias_Transcurridos_Pago`, `Dias_Vencido`.
# - Imprime `head(10)` de `estado_cuenta_final`. Luego imprime otro `head(10)`
#   filtrando solo las que tengan Estatus "Pendiente".
#
# 5. Guardar nuevos cálculos agrupando en otra tabla:
# - Crea `promedios_clientes` agrupando `estado_cuenta_final` por `IDCliente`
#   y resumiendo con la media de `Dias_Vencido` (ignora NAs) en una variable
#   llamada `Promedio_Dias_Vencido`. Usa `.groups = "drop"`.
# - Crea `clientes_actualizado` uniendo `clientes` y `promedios_clientes`
#   mediante `left_join` con `by = c("ID" = "IDCliente")`.
# - Usa `cat("--- Tabla de Clientes Actualizada ---\n")` e imprime el `head()`
#   de `clientes_actualizado`. Ejecuta `skim(promedios_clientes)`.
# - Crea `ventas_totales_cliente` agrupando `estado_cuenta_final` por
#   `IDCliente` y sumando el `Total` (ignora NAs) en `Monto_Total_Vendido`.
#   Usa `.groups = "drop"`.
# - Sobrescribe `clientes_actualizado`: hazle un `left_join` con
#   `ventas_totales_cliente` y ordénalo descendente por
#   `Promedio_Dias_Vencido` usando `arrange(desc())`. Imprímelo.
# - Crea `extremos_pagadores` a partir de `clientes_actualizado`:
#   1) Usa `slice(1, n())` para obtener la primera y última fila.
#   2) Muta una nueva columna `Perfil` asignándole este vector exacto:
#      `c("Peor Pagador (Mayor Morosidad)", "Mejor Pagador (Mayor Anticipo)")`.
#   3) Usa `select()` para quedarte con `Perfil`, `ID`, `Nombre`, `Zona`,
#      `Credito`, renombra al vuelo `Promedio_Dias_Vencido` como
#      `Vencimiento_Prom` y `Monto_Total_Vendido` como `Monto_Vendido`.
# - Imprime `extremos_pagadores`.
#
# 6. Unir nuevamente ventas y pagos con un propósito distinto:
# - Crea `ventas_diarias` a partir de `ventas`: muta `Fecha_Dia` usando
#   `as.Date(Fecha)`, agrupa por `Fecha_Dia` y resume la suma de `Total` en
#   `Ventas`. Usa `.groups = "drop"`.
# - Crea `pagos_diarios` a partir de `estado_cuenta_final`: filtra por
#   Estatus "Pagado", muta `Fecha_Dia = Fecha_Pago`, agrupa por `Fecha_Dia`
#   y resume la suma de `Total` en `Pagos`. Usa `.groups = "drop"`.
# - Crea `flujo_diario` usando un `full_join` entre `ventas_diarias` y
#   `pagos_diarios` por "Fecha_Dia". Muta `Ventas` y `Pagos` utilizando la
#   función `coalesce()` para reemplazar los NAs por 0, y ordena por
#   `Fecha_Dia` usando `arrange()`.
# - Usa `cat("--- Tabla de Flujo Diario (Ventas vs Pagos) ---\n")` e imprime
#   `head(flujo_diario, 10)`.

