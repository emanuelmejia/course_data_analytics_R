# ==============================================================================
# CONFIGURACIÓN INICIAL Y CARGA DE LIBRERÍAS
# ==============================================================================
library(tidyverse)
library(dbscan)
library(caret)
library(skimr)
library(corrplot)

set.seed(123) # Para reproducibilidad

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L07-Clustering/data")

# Cargar el dataset de fraudes
df_banco <- read.csv("transacciones.csv")

# ==============================================================================
# GLOSARIO DE VARIABLES - DATASET DE df_banco
# ==============================================================================
# monto_usd: Cantidad de dinero en la transacción
# distancia_hogar_km: Distancia desde la ubicación del cliente.
# hora_dia: Hora en la que ocurrió (formato 24h).
# score_historial: Puntuación interna de riesgo del comercio (0 a 100, donde más alto es más peligroso).
# tipo_dispositivo: Si se usó una App Móvil (1) o un Navegador Web (2).
# ingreso_manual: Si los datos de la tarjeta se digitaron manualmente (1) o se usó chip/contactless (0).
# tipo_transaccion: Preetiquetado del tipo de transacción
#   A: Gastos diarios comunes (Gran densidad)
#   B: Compras grandes legítimas o recurrentes (Densidad media)
#   C: Fraudes / Anomalías 

# ==============================================================================
# PERFILAMIENTO INICIAL Y CORRELACION INICIAL
# ==============================================================================
head(df_banco, 5)

# Omitir valores nulos por seguridad y perfilar variables
df_banco <- df_banco %>% na.omit()
skim(df_banco)

table(df_banco$tipo_transaccion)

# Convertimos el tipo de transacción a número
df_banco$tipoNum <- factor(df_banco$tipo_transaccion, 
                                   levels = c("A", "B", "C")) %>% 
  as.numeric()

# Seleccionar únicamente variables numéricas para la matriz de correlación
df_bancoNum <- df_banco %>% select(where(is.numeric))

# Gráfico de correlación para comprobar el comportamiento inyectado del monto
cor(df_bancoNum) %>% corrplot(method = "square", tl.col = "black", tl.srt = 45)

# Quitemos la columna temporal
df_banco <- df_banco %>% select(-tipoNum)

# ==============================================================================
# DISTANCIA MAHALANOBIS
# ==============================================================================

# Nota: Mahalanobis requiere matrices puramente numéricas
df_bancoNum <- df_banco %>% select(where(is.numeric))

# Calcular el centro (medias de las columnas)
centro_datos <- colMeans(df_bancoNum)
# Matriz de covarianza de los datos
matriz_cov   <- cov(df_bancoNum)

# Calcular la Distancia de Mahalanobis para cada fila
# OJO! Retorna la distancia al cuadrado (D^2)
df_banco$mahalanobis <- mahalanobis(df_bancoNum, 
                                    center = centro_datos, 
                                    cov = matriz_cov)

df_banco$mahalanobis <- round(df_banco$mahalanobis, 2)

# Definir un umbral estadístico para detectar fraudes
# Usamos la distribución Chi-cuadrado con grados de libertad = número de variables
# Un nivel de confianza de 0.99 marca como fraude lo extremadamente atípico
grados_libertad <- ncol(df_bancoNum)
umbral_critico <- qchisq(0.99, 
                         df = grados_libertad)

df_banco$fraude <- ifelse(df_banco$mahalanobis > umbral_critico, 
                                        "Fraude", 
                                        "Regular")

table(df_banco$fraude)

# Evaluar efectividad mediante una matriz de confusión
tabla_validacion <- table(df_banco$tipo_transaccion, df_banco$fraude)
print(tabla_validacion)

# ==============================================================================
# KNN
# ==============================================================================

# Convertir la columna objetivo en Factor (necesario para clasificación en caret)
df_banco$tipo_transaccion <- as.factor(df_banco$tipo_transaccion)

# Partición de Datos (80% Entrenamiento, 20% Prueba)

indices_entrenamiento <- createDataPartition(df_banco$tipo_transaccion, p = 0.8, list = FALSE)
datos_entreno <- df_banco[indices_entrenamiento, ]
datos_prueba  <- df_banco[-indices_entrenamiento, ]

# Configurar el entrenamiento con Validación Cruzada (Cross-Validation)
# Divide la información en partes iguales (folds)
# Entrena en casi todos los grupos y guarda uno para probar
# Repite hasta que cada grupo pasa la prueba
# Saca un "promedio" final de los resultados obtenidos
control_entreno <- trainControl(method = "cv", number = 5)

# Entrenar el modelo k-NN
# preProcess = c("center", "scale") normaliza los datos numéricos de forma automática antes de medir distancias
modelo_knn <- train(
  tipo_transaccion ~ monto_usd + distancia_hogar_km + hora_dia + 
    score_historial + tipo_dispositivo + ingreso_manual,
  data = datos_entreno,
  method = "knn",
  tuneGrid = expand.grid(k = c(3, 5, 7, 9)), # R evaluará cuál de estos valores de 'k' funciona mejor
  trControl = control_entreno,
  preProcess = c("center", "scale")
)

# Resultados del modelo
# NOTA: Kappa cerca de 1 modelo es excelente y acierta por verdadera relación
# Kappa 0 o negativo, modelo acierta igual o peor que si adivinaras al azar
modelo_knn

# Realizar predicciones sobre el conjunto de prueba
predicciones <- predict(modelo_knn, 
                        newdata = datos_prueba)

# Evaluar efectividad con una Matriz de Confusión detallada
matriz_confusion <- confusionMatrix(predicciones, datos_prueba$tipo_transaccion)
matriz_confusion$table

# Precisión global
round(matriz_confusion$overall['Accuracy'] * 100, 2)

# Agregar las predicciones finales a todo el conjunto original
df_banco$prediccion_knn <- predict(modelo_knn, newdata = df_banco)


# ==============================================================================
# DBSCAN
# ==============================================================================

# Preprocesamiento: Escalar solo columnas numéricas
df_escalado <- scale(df_banco[, 1:6])

# Seleccionar mínimo de puntos (podemos empezar con 2 * numColumnas)
# Si base de datos es grande o tiene mucho ruido, incrementarlo
# A mayor número min de puntos, más estrictos son los grupos
# Menos ruido se convertirá en cluster
puntos <- 2 * ncol(df_escalado)

# Para épsilon podemos calcular la distancia de cada fila al cero
# Épsilon muy pequeño, muchos puntos se considerarán ruido
# Épsilon muy grande, grupos separados se fusionen
distancia <- sqrt(rowSums(df_escalado^2))
epsilon <- round(mean(distancia),1)

# Aplicar el algoritmo DBSCAN
modelo_dbscan <- dbscan(df_escalado, 
                        eps = epsilon, 
                        minPts = puntos)

# Guardar el resultado del cluster
df_banco$cluster_detectado <- modelo_dbscan$cluster

# Revisar clusters conformados
# Clúster 0 = anomalías
# Verificar también clústers pequeños!
freqCluster <- table(df_banco$cluster_detectado)
print(freqCluster)

# Evaluar efectividad
tabla_validacion <- table(df_banco$tipo_transaccion, df_banco$cluster_detectado)
print(tabla_validacion)

# Pruebas con diferentes valores
puntos <- 20
epsilon <- 1.8

# Aplicar el algoritmo DBSCAN
modelo_dbscan <- dbscan(df_escalado, 
                        eps = epsilon, 
                        minPts = puntos)

# Sobreescribir el resultado del cluster
df_banco$cluster_detectado <- modelo_dbscan$cluster

# Revisar clusters conformados
freqCluster <- table(df_banco$cluster_detectado)
print(freqCluster)

# Evaluar efectividad
tabla_validacion <- table(df_banco$tipo_transaccion, df_banco$cluster_detectado)
print(tabla_validacion)
