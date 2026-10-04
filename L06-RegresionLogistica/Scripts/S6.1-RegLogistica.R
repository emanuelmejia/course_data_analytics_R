library(tidyverse)
library(skimr)
library(corrplot)
library(fastDummies)
library(caret)

# Fijar la semilla (random_state=0)
set.seed(0)

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L06-RegresionLogistica/data")
bank <- read.csv("banking.csv")

# ==========================================
# PERFIL INICIAL
head(bank,10)
bank <- bank %>% na.omit()
skim(bank)

# Reducir la categoría educación
unique(bank$education)

bank <- bank %>%
  mutate(
    education = if_else(education %in% c('basic.9y', 'basic.6y', 'basic.4y'), 
                        'Basic', 
                        education)
    )

# Observar resultado
unique(bank$education)

# ==========================================
# ANALISIS EXPLORATORIO VARIABLE RESPUESTA

# Generar un conteo
bank %>% count(y)

# Gráfico de la variable respuesta
ggplot(bank, aes(x = factor(y), fill = factor(y))) +
  geom_bar() + 
  theme_minimal() +               
  theme(legend.position = "none")

# Medias por cada grupo de la variable dependiente
bank %>%
  group_by(y) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Ojo seleccionar únicamente variables numéricas
bankNum <- bank %>% select(where(is.numeric))

# Graficamos los datos numéricos
cor(bankNum) %>% corrplot(method = "square")

# ==========================================
# División de datos de entrenamiento y prueba

# Crear índices estratificados (p = 0.7 es la proporción de entrenamiento)
# Proporción de 0s y 1s sea igual a la distribución original
indices_entrenamiento <- createDataPartition(bank$y, p = 0.7, list = FALSE)

# Dividir los datos
bankTrain <- bank[indices_entrenamiento, ]
bankTest  <- bank[-indices_entrenamiento, ]

# ==========================================
# Modelo inicial

# Ajustar el modelo
# family = "binomial" es lo que le indica a glm() que haga una Regresión Logística
regInicial <- glm(y ~ pdays + previous + emp_var_rate + euribor3m + nr_employed, 
              data = bankTrain, family = "binomial")

summary(regInicial)

ajuste <- data.frame(
  Modelo = "Inicial",
  Dev = regInicial$deviance,
  AIC = regInicial$aic
)

# Visualizar el dataframe guardado
print(ajuste)

# ==========================================
# Modelo con variable categórica no significativa

# Explorar estado civil
ggplot(bank, aes(x = marital, fill = factor(y))) +
  geom_bar(position = "fill") +
  labs(title = 'Conversión por estado civil',
       x = 'Estado Civil',
       y = 'Proporción de clientes',
       fill = 'y') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Ajustar el modelo usando la variable marital
regMarital <- glm(y ~ pdays + previous + emp_var_rate + euribor3m + nr_employed + marital, 
                  data = bankTrain, family = "binomial")

summary(regMarital)

# ==========================================
# Modelo con variables categóricas socioeconómicas

ggplot(bank, aes(x = education, fill = factor(y))) +
  geom_bar(position = "fill") +
  labs(title = 'Conversión por nivel educativo',
       x = 'Nivel Educativo',
       y = 'Proporción de clientes',
       fill = 'y') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(bank, aes(x = job, fill = factor(y))) +
  geom_bar(position = "dodge") +
  labs(title = 'Frecuencia de compra por tipo de trabajo',
       x = 'Trabajo',
       y = 'Frecuencia de compra',
       fill = 'y') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Ajustar el modelo usando las variables socioeconómicas
regSoc <- glm(y ~ pdays + previous + emp_var_rate + euribor3m + nr_employed + education + job, 
                  data = bankTrain, family = "binomial")
summary(regSoc)

# Establecer casos base deseados
bankTrain$education <- relevel(factor(bankTrain$education), ref = "illiterate")
bankTrain$job <- relevel(factor(bankTrain$job), ref = "student")

# Ajustar nuevamente el modelo usando los nuevos casos base
regSoc <- glm(y ~ pdays + previous + emp_var_rate + euribor3m + nr_employed + education + job, 
              data = bankTrain, family = "binomial")
summary(regSoc)

# Agregar nuevos datos de bondad de ajuste
nuevo_ajuste <- data.frame(
  Modelo = "Socioeconómico",
  Dev = regSoc$deviance,
  AIC = regSoc$aic
)

ajuste <- rbind(ajuste, nuevo_ajuste)
print(ajuste)

# ==========================================
# Modelo con variables temporales

# Gráfico de conversión por día semana
ggplot(bank, aes(x = day_of_week, fill = factor(y))) +
  geom_bar(position = "fill") +
  labs(title = 'Inscripción por Día de la semana',
       x = 'Día de la semana',
       y = 'Proporción de clientes',
       fill = 'y') +
  theme_minimal()

# Gráfico de conversión por mes
# Ordenar el factor de mes para que aparezca en orden cronológico
lvl_mes <- c("mar", "apr", "may", "jun", 
             "jul", "aug", "sep", "oct", 
             "nov", "dec")
bank$month <- factor(bank$month, 
                     levels = lvl_mes, 
                     ordered = FALSE)

ggplot(bank, aes(x = month, fill = factor(y))) +
  geom_bar(position = "fill") +
  labs(title = 'Inscripción por Mes',
       x = 'Mes',
       y = 'Proporción de clientes',
       fill = 'y') +
  theme_minimal()


bankTrain$month <- factor(bankTrain$month, 
                          levels = lvl_mes, 
                          ordered = FALSE)

# Ajustar el modelo usando las variables temporales
regTemp <- glm(y ~ pdays + previous + emp_var_rate + euribor3m + nr_employed + 
                 education + job + month, 
               data = bankTrain, family = "binomial")
summary(regTemp)

# Agregar nuevos datos de bondad de ajuste
nuevo_ajuste <- data.frame(
  Modelo = "Temporal",
  Dev = regTemp$deviance,
  AIC = regTemp$aic
)

ajuste <- rbind(ajuste, nuevo_ajuste)
print(ajuste)

# ==========================================
# Modelo con contactos previos

# Gráfico de barras agrupadas (position = "dodge" pone las barras lado a lado)
ggplot(bank, aes(x = poutcome, fill = factor(y))) +
  geom_bar(position = "dodge") +
  labs(title = 'Conversión por Intentos Previos',
       x = 'Resultado Anterior',
       y = 'Frequencia de compra',
       fill = 'y') +
  theme_minimal()
# ¿Cuál grupo tiene mayor conversión?

ggplot(bank, aes(x = poutcome, fill = factor(y))) +
  geom_bar(position = "fill") +
  labs(title = 'Conversión por Intentos Previos',
       x = 'Resultado Anterior',
       y = 'Proporción de clientes',
       fill = 'y') +
  theme_minimal()

# Ajustar el modelo usando las variables temporales
regPrev <- glm(y ~ pdays + previous + emp_var_rate + euribor3m + nr_employed + 
                 education + job + month + poutcome, 
               data = bankTrain, family = "binomial")
summary(regPrev)

# Agregar nuevos datos de bondad de ajuste
nuevo_ajuste <- data.frame(
  Modelo = "IntPrevio",
  Dev = regPrev$deviance,
  AIC = regPrev$aic
)

ajuste <- rbind(ajuste, nuevo_ajuste)
print(ajuste)

# ==========================================
# Modelo Saturado
regSat <- glm(y ~ ., data = bank, family = "binomial")
summary(regSat)

# Agregar nuevos datos de bondad de ajuste
nuevo_ajuste <- data.frame(
  Modelo = "Saturado",
  Dev = regSat$deviance,
  AIC = regSat$aic
)

ajuste <- rbind(ajuste, nuevo_ajuste)
print(ajuste)

# ==========================================
# Remover Estratégicamente
# Quitamos la variable que pierde significancia al agregar poutcome
regStrateg <- glm(y ~ pdays + emp_var_rate + euribor3m + nr_employed + 
                     education + job + month + poutcome, 
                   data = bankTrain, family = "binomial")
summary(regStrateg)

# Agregar nuevos datos de bondad de ajuste
nuevo_ajuste <- data.frame(
  Modelo = "Estratégico",
  Dev = regStrateg$deviance,
  AIC = regStrateg$aic
)

ajuste <- rbind(ajuste, nuevo_ajuste)
print(ajuste)

# ==========================================
# Ajuste Personalizado

# Crear Variables Dummy

# Lista de columnas categóricas
cat_cols <- c('job', 'marital', 'education', 
              'default', 'housing', 'loan', 
              'contact', 'month', 'day_of_week', 'poutcome')


# Crear las variables dummy para las columnas especificadas y unirlas al dataframe
# (Esto reemplaza por completo el bucle 'for' y el 'data.join' de Python)
bankDummy <- dummy_cols(bank, 
                        select_columns = cat_cols,
                        remove_selected_columns = FALSE,
                        # remove_first_dummy = TRUE
                        )

# Observar nuestro mejor modelo:
summary(regStrateg)

# Dividir los datos nuevamente
bankTrain <- bankDummy[indices_entrenamiento, ]
bankTest  <- bankDummy[-indices_entrenamiento, ]


# Seleccionar las columnas que ingresaremos al modelo
columns_final <- c("y", "pdays", "emp_var_rate", "euribor3m", "nr_employed", 
                   "job_blue-collar", "job_student", "job_services", "job_unemployed",
                   "month_apr", "month_may", "month_jun", "month_jul", "month_aug", 
                   "month_sep", "month_oct", "month_nov", "month_dec", "poutcome_nonexistent", 
                   "poutcome_success")

# Filtrar el dataframe para quedarnos solo con esas columnas (la coma al principio significa "todas las filas")
bankSelectTrain <- bankTrain %>% select(all_of(columns_final))


# 2. Ajustar el modelo
# La sintaxis y ~ . significa "predecir 'y' en función de todas las demás columnas (.)"
# family = "binomial" es lo que le indica a glm() que haga una Regresión Logística
regCustom <- glm(y ~ ., data = bankSelectTrain, family = "binomial")

# Para ver los coeficientes y qué variables son estadísticamente significativas
# (Algo que scikit-learn no muestra fácilmente, pero R sí)
summary(regCustom)

# Agregar nuevos datos de bondad de ajuste
nuevo_ajuste <- data.frame(
  Modelo = "Custom",
  Dev = regCustom$deviance,
  AIC = regCustom$aic
)

ajuste <- rbind(ajuste, nuevo_ajuste)
print(ajuste)

# ==========================================
# Predicciones y métricas de Evaluación

# Generar las predicciones en forma de probabilidades (de 0 a 1)
probabilidades <- predict(regCustom, newdata = bankTest, type = "response")

# Convertir las probabilidades a clases (0 o 1) usando 0.5 como umbral
yPred <- ifelse(probabilidades > 0.5, 1, 0)

# Crear la matriz de confusión
# Se asignan nombres ("Real" y "Prediccion") para saber exactamente qué son las filas y columnas
matriz_confusion <- table(Real = bankTest$y, 
                          Prediccion = yPred)

# Imprimir la matriz
print(matriz_confusion)

# Es obligatorio que tanto las predicciones como los datos reales sean factores (categorías)
# y que tengan los mismos niveles (0 y 1)
yTestFactor <- factor(bankTest$y, levels = c("0", "1"))
yPredF <- factor(yPred, levels = c("0", "1"))

# Generar el reporte con enfoque en Precision, Recall y F1
reporteInicial0 <- confusionMatrix(data = yPredF, 
                                   reference = yTestFactor, 
                                   mode = "prec_recall",
                                   positive = "0")


# Generar el reporte con enfoque en Precision, Recall y F1
reporteInicial1 <- confusionMatrix(data = yPredF, 
                                   reference = yTestFactor, 
                                   mode = "prec_recall",
                                   positive = "1")

# Imprimir reporte completo
print(reporteInicial1)

# Accuracy: proporción de veces que la predicción coincide con la realidadd
print(reporteInicial1$overall["Accuracy"])

# Si solo quieres ver la tabla específica de métricas (similar al output de sklearn)
print(reporteInicial0$byClass[c("Precision", "Recall", "F1")])
print(reporteInicial1$byClass[c("Precision", "Recall", "F1")])