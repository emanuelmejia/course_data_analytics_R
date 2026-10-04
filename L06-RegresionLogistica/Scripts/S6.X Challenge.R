# ==============================================================================
# CONFIGURACIÓN INICIAL Y CARGA DE LIBRERÍAS
# ==============================================================================
library(tidyverse)
library(skimr)
library(corrplot)
library(broom)
library(caret)
library(fastDummies)

# Fijar la semilla
set.seed(123)

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L06-RegresionLogistica/data")
# Cargar el dataset de fraudes
seguros <- read.csv("segDanos.csv")

# ==============================================================================
# GLOSARIO DE VARIABLES - DATASET DE SEGUROS
# ==============================================================================
# monto: (Numérica) Costo total en dinero exigido a la aseguradora por el siniestro. Es la variable objetivo para el pronóstico lineal.
# madrugada: (Binaria) Indica el momento del accidente. Toma valor 1 si ocurrió en la madrugada y 0 en otro horario.
# demora: (Numérica Entera) Cantidad de días transcurridos entre la ocurrencia del siniestro y la notificación formal a la aseguradora.
# gravedad: (Categórica Ordinal) Clasificación cualitativa del impacto físico o daño material del vehículo. Niveles: Leve, Moderado, Pérdida Total.
# cobertura: (Categórica Ordinal) Nivel de protección contratado en la póliza del cliente. Niveles: Responsabilidad Civil, Limitada, Amplia.
# temperatura: (Numérica Continua - Ruido) Registro ambiental en grados Celsius el día del siniestro. No posee impacto estadístico real sobre el fraude.
# colorAuto: (Categórica Nominal - Ruido) Tonalidad estética del vehículo asegurado (Rojo, Azul, Blanco, Negro). No influye en el costo ni en el riesgo.
# antiguedad: (Numérica Entera) Tiempo total en meses que el cliente lleva asegurado en la compañía. Actúa habitualmente como factor protector.
# siniPrevios: (Numérica Entera) Historial o conteo de reclamaciones por choques o siniestros registrados por el mismo cliente en el pasado.
# cambioCobert: (Dicotómica / Binaria) Indica si el cliente modificó o elevó el límite de sus pólizas recientemente (1: Sí, 0: No) antes del reporte.
# atrasos: (Dicotómica / Binaria) Registro de comportamiento financiero. Indica si el asegurado cuenta con historial de primas vencidas (1: Sí, 0: No).
# fraude: (Dicotómica / Binaria) Variable objetivo para el modelo logístico. Indica si la reclamación fue dictaminada como legítima (0) o fraudulenta (1).
# ==============================================================================


# ==============================================================================
# PERFILAMIENTO INICIAL Y CORRELACION INICIAL
# ==============================================================================
print("--- Primeros 5 registros ---")
head(seguros, 5)

# Omitir valores nulos por seguridad y perfilar variables
seguros <- seguros %>% na.omit()
skim(seguros)

# Seleccionar únicamente variables numéricas para la matriz de correlación
segurosNum <- seguros %>% select(where(is.numeric))

# Gráfico de correlación para comprobar el comportamiento inyectado del monto
cor(segurosNum) %>% corrplot(method = "square", tl.col = "black", tl.srt = 45)

# ==============================================================================
# CORRELACIÓN CON VARIABLES CATEGÓRICAS
# ==============================================================================

# Crear una copia del dataset para no alterar los tipos de datos originales
seguros_corr <- seguros

# Conversión de variables Ordinales (Asignación manual de orden lógico)
seguros_corr$gravedadNum <- factor(seguros_corr$gravedad, 
                                              levels = c("Leve", "Moderado", "Pérdida Total")) %>% 
  as.numeric()

seguros_corr$coberturaNum <- factor(seguros_corr$cobertura, 
                                          levels = c("Responsabilidad Civil", "Limitada", "Amplia")) %>% 
  as.numeric()

# Conversión de variables Nominales (Asignación automática por orden alfabético)
seguros_corr$colorAutoNum <- factor(seguros_corr$colorAuto) %>% 
  as.numeric()

# ==============================================================================
# SELECCIÓN Y NUEVA MATRIZ DE CORRELACIÓN
# ==============================================================================

# Seleccionar solo las variables numéricas base y las nuevas transformadas
segurosNum <- seguros_corr %>% 
  select(
    monto, 
    madrugada, 
    demora, 
    temperatura, 
    gravedadNum, 
    coberturaNum, 
    colorAutoNum,
    antiguedad,
    sinPrevios,
    cambioCobert,
    atrasos,
    fraude
  )

# Generar y pintar la matriz de correlación completa
matriz_completa <- cor(segurosNum)

# Imprimir los valores de correlación específicos con el Monto de Reclamo
print("--- Correlaciones numéricas exactas con monto ---")
print(sort(matriz_completa["monto", ], decreasing = TRUE))

# Dibujar el gráfico corrplot con todas las variables incluidas
cor(segurosNum) %>% corrplot(
         method = "square", 
         type = "upper",        # Muestra solo la mitad superior para que sea más limpio
         tl.col = "black", 
         tl.srt = 45, 
         addCoef.col = "black", # Agrega los números exactos de correlación dentro del gráfico
         number.cex = 0.7)      # Tamaño del texto de los números

# ==============================================================================
# DIVISIÓN DATOS DE ENTRENAMIENTO Y PRUEBA
# ==============================================================================
# # Crear índices estratificados (p = 0.9 es la proporción de entrenamiento)
# # Proporción de 0s y 1s sea igual a la distribución original
# indices_entrenamiento <- createDataPartition(seguros$fraude, p = 0.9, list = FALSE)
# 
# # Dividir los datos
# segurosTrain <- seguros[indices_entrenamiento, ]
# segurosTest  <- seguros[-indices_entrenamiento, ]

# Calcular el número de filas para el 0% de los datos
tamano_entrenamiento <- floor(0.9 * nrow(seguros))

# Crear índices de forma completamente aleatoria
indices_entrenamiento <- sample(seq_len(nrow(seguros)), size = tamano_entrenamiento)

# Dividir los datos
segurosTrain <- seguros[indices_entrenamiento, ]
segurosTest  <- seguros[-indices_entrenamiento, ]

# ==============================================================================
# MODELO LINEAL SIMPLE (Base)
# ==============================================================================
# Buscamos explicar el monto usando únicamente los días de demora
reg_base <- lm(monto ~ gravedad, data = segurosTrain)
summary(reg_base)

# ==============================================================================
# MODELO MÚLTIPLE (Con variables de control de fraude y ruido numérico)
# ==============================================================================
# Agregamos la hora del siniestro y la temperatura del clima (variable de ruido)
reg_control <- lm(
  monto ~ madrugada + demora + gravedad + cobertura + fraude, 
  data = segurosTrain
)
summary(reg_control)

# ==============================================================================
# MODELO SATURADO (Incluyendo las categóricas fuertes y de ruido)
# ==============================================================================
# R maneja automáticamente las categóricas creando variables dummy bajo el capó
reg_full <- lm(
  monto ~ .,
  data = segurosTrain
)
summary(reg_full)

# ==============================================================================
# COMPARACIÓN DE MODELOS (Selección de Variables)
# ==============================================================================
# Construcción de la tabla comparativa de criterios de información
tabla_modelos <- bind_rows(
  "Modelo Base" = glance(reg_base),
  "Modelo Control" = glance(reg_control),
  "Modelo Full" = glance(reg_full),
  .id = "Modelo"
) %>% 
  select(Modelo, r.squared, adj.r.squared, AIC, BIC)

print("--- Tabla Comparativa Inicial de Modelos ---")
print(tabla_modelos)

# ==============================================================================
# MODELO AJUSTADO (Eliminando las variables sin significancia estadística)
# ==============================================================================
reg_ajuste <- lm(
  monto ~ madrugada + demora + gravedad + cobertura + 
    sinPrevios + cambioCobert + fraude,
  data = segurosTrain
)
summary(reg_ajuste)

# Actualizar la tabla comparativa de rendimiento
tabla_modelos <- tabla_modelos %>%
  bind_rows(
    glance(reg_ajuste) %>% 
      mutate(Modelo = "Modelo Ajuste") %>% 
      select(Modelo, r.squared, adj.r.squared, AIC, BIC)
  )

print("--- Tabla Final de Modelos Ajustados ---")
print(tabla_modelos)

# ==============================================================================
# MODELO FINAL (Último ajuste)
# ==============================================================================
reg_final <- lm(
  monto ~ madrugada + demora + gravedad + cobertura + 
    sinPrevios + fraude,
  data = segurosTrain
)
summary(reg_final)

# Actualizar la tabla comparativa de rendimiento
tabla_modelos <- tabla_modelos %>%
  bind_rows(
    glance(reg_ajuste) %>% 
      mutate(Modelo = "Modelo Final") %>% 
      select(Modelo, r.squared, adj.r.squared, AIC, BIC)
  )

print("--- Tabla Final de Modelos Ajustados ---")
print(tabla_modelos)


# ==============================================================================
# PREDICCIÓN SOBRE EL CONJUNTO DE PRUEBA (TEST SET)
# ==============================================================================
# 1. Generar predicciones utilizando el modelo final en los datos que no ha visto
segurosTest$pred_monto <- predict(reg_final, newdata = segurosTest)

# 2. Calcular los residuales (Monto real menos el Monto predicho)
segurosTest$res_monto <- segurosTest$monto - segurosTest$pred_monto

# ==============================================================================
# EVALUACIÓN DE MÉTRICAS DE PRECISIÓN
# ==============================================================================
# Calcular métricas estándar de regresión lineal en datos de prueba
rmse_test <- RMSE(segurosTest$pred_monto, segurosTest$monto)
mae_test  <- MAE(segurosTest$pred_monto, segurosTest$monto)

# Calcular R-cuadrado fuera de muestra de forma matemática directa
r2_test   <- cor(segurosTest$monto, segurosTest$pred_monto)^2

# Mostrar los resultados de precisión en consola
cat("--- Métricas de Precisión en el Conjunto de Prueba (Test Set) ---\n")
cat("Root Mean Squared Error (RMSE): ", round(rmse_test, 2), "\n")
cat("Mean Absolute Error (MAE):      ", round(mae_test, 2), "\n")
cat("R-squared (R2 Test):            ", round(r2_test, 4), "\n")
cat("======================================================================\n")

# ==============================================================================
# GRÁFICO DE RESIDUALES EN EL TEST SET (ggplot2)
# ==============================================================================
# Ajustamos un modelo lineal simple interino para dibujar la tendencia del error
reg_res_test <- lm(res_monto ~ pred_monto, data = segurosTest)
summary(reg_res_test)

# Desplegar el gráfico de dispersión de residuales frente a predicciones
segurosTest %>% ggplot(aes(x = pred_monto, y = res_monto)) + 
  geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_test)[1], 
              slope = coef(reg_res_test)[2], 
              color = "#0099F8", linewidth = 1.5) +
  labs(title = "Predicciones VS Residuales (Conjunto de Prueba)",
       subtitle = "Monto ~ Madrugada + Demora + Gravedad + Cobertura + sinPrevios + Fraude",
       x = "Predicción de Monto del Reclamo ($)",
       y = "Residuales (Error en Test Set)") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8", size = 15, face = "bold"),
    plot.subtitle = element_text(color = "#969696", size = 11, face = "italic"),
    axis.title = element_text(color = "#969696", size = 10, face = "bold"),
    axis.text = element_text(color = "#969696", size = 9),
    axis.line = element_line(color = "#969696")
  )