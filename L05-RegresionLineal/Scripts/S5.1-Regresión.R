# Tidiverse incluye varios paquetes útiles para datos
# ggplot2 para visualización
# dplyr para manipulación de datos
# tidyr para organizar datos
# readr para importar datos 
# cpurrr para programación funcional 
# tibble para marcos de datos mejorados. 
library(tidyverse)
library(skimr)
library(corrplot)
library(broom)

setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L05-RegresionLineal/data")
vino <- read.csv("vino.csv")

# ==========================================
# PERFIL INICIAL
head(vino,10)
vino <- vino %>% na.omit()
skim(vino)

# Ojo seleccionar únicamente variables numéricas
vinoNum <- vino %>% select(where(is.numeric))

# Graficamos los datos numéricos
cor(vinoNum) %>% corrplot(method = "square")

# ==========================================
# GRAFICO DE DISPERSION
plot(x = vinoNum$alcohol,
     y = vinoNum$cardio,                               # Coordenadas
     col = c("orangered1"),                             # De qué color (puede ser más de uno e incluso ponerle "colors()")
     pch = 18,                                          # Tipo de punto que se va a utilizar
     main = "Vino VS Muertes Cardio",                          # Título del gráfico
     xlab = "Alcohol consumido en vino per cápita (L)", # Nombre del eje x
     ylab = "Muertes por cardiopatia (c/100,000 hab)")             # Nombre del eje y

# abline pinta sobre el gráfico actual una recta del tipo y = a + bx

# Pintemos el MODELO NULO, es decir la media de y
abline(a = mean(vino$cardio), b = 0, col = "blue", lwd = 3)

# Intentos dse encontrar una recta que se ajuste a los datos
abline(a = 50, b = 30, col = "gray50", lwd = 2)
abline(a = 300, b = -50, col = "gray50", lwd = 2)

# ==========================================
# MODELO SIMPLE

?lm # Ajusta un modelo lineal Y~X

# Y : Variable respuesta
# X : Variable explicativa

# Busco encontrar coeficientes b0 y b1 en Y = b0 + b1*X
# Que mejor se ajusten a mis datos

# Modelo base incluyendo solo una variable
reg_base <- lm(vino$cardio ~ vino$alcohol) # "~" Regresión de muertes cardio con base en consumo alcohol
reg_base

# summary a una regresión para ver más datos 
# verificar asteriscos de coeficientes (basados en p-value)
summary(reg_base)

# Podemos extraer los coeficientes
coefs <- coef(reg_base)
# Pintemos ahora una línea con base en estos coeficientes
abline(a = coefs[1], b = coefs[2], col = "orangered1", lwd = 4)

# Guardemos las predicciones y residuales dentro del DF
vino$pred_card <- reg_base$fitted.values
vino$res_card <- reg_base$residuals

View(vino)

# Regresión de residuales
reg_res_card <- lm(vino$res_card~vino$pred_card) # "~" Regresión de los residuales con base en predicciones
reg_res_card
summary(reg_res_card)

# Gráfico de residuales usando ggplot!
vino %>% ggplot(aes(x = vino$pred_card, y = vino$res_card)) + 
  geom_point(alpha = 0.6, color = "#001F82") +
  labs(title = "Predicciones VS Residuales",
       subtitle = "Muertes Cardio ~ Alcohol",
       x = "Predicción Muertes",
       y = "Residuales") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8",
                              size = 17,
                              face = "bold"),
    plot.subtitle = element_text(color = "#969696", 
                                 size = 13, 
                                 face = "italic"),
    axis.title = element_text(color = "#969696",
                              size = 10,
                              face = "bold"),
    axis.text = element_text(color = "#969696", 
                             size = 10),
    axis.line = element_line(color = "#969696")
  )

# Hagamos nuevamente el gráfico con la línea de regresión
vino %>% ggplot(aes(x = vino$pred_card, y = vino$res_card)) + 
  geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_card)[1], 
              slope = coef(reg_res_card)[2], 
              color = "#0099F8", linewidth = 1.5)+
  # geom_text(
  #   label= vino$pais,
  #   nudge_x = 0, nudge_y = 15,
  #   check_overlap = T
  # ) +
  # geom_label(
  #   data = vino %>% filter(pred_card < 150), # Filtramos datos
  #   aes(label = pais,
  #       x = pred_card,
  #       y = res_card),
  #   nudge_x = 0, nudge_y = 16,
  #   label.size = 0.3,
  #   label.padding = unit(0.15, "lines"),
  #   fill="lightblue") +
  # labs(title = "Predicciones VS Residuales",
  #    subtitle = "Muertes Cardio ~ Alcohol",
  #    x = "Predicción Muertes Cardio",
  #    y = "Residuales") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8",
                              size = 17,
                              face = "bold"),
    plot.subtitle = element_text(color = "#969696", size = 13, face = "italic"),
    axis.title = element_text(color = "#969696",
                              size = 10,
                              face = "bold"),
    axis.text = element_text(color = "#969696", size = 10),
    axis.line = element_line(color = "#969696")
  )

# ==========================================
# MODELO MULTIPLE

# Agreguemos una variable adicional
reg_control <- lm(vino$cardio~vino$alcohol + vino$muertes) 
reg_control
summary(reg_control)

# ==========================================
# MODELO SATURADO

# Modelo saturado
reg_full <- lm(
  vino$cardio ~ vino$alcohol + vino$muertes + vino$hepatic
  )
reg_full
summary(reg_full)

# Comparación por medio de criterios de información 
# para selección de variables
glance(reg_base)

tabla_modelos <- bind_rows(
  "Modelo Base" = glance(reg_base),
  "Modelo Control" = glance(reg_control),
  "Modelo Full" = glance(reg_full),
  .id = "Modelo" # Crea una columna con los nombres que le dimos arriba
) %>% 
  select(Modelo, r.squared, adj.r.squared, AIC, BIC)

# Ver la tabla resultante
tabla_modelos

# ==========================================
# MODELO AJUSTADO

# ¿Y si ajustamos a quitar la variable 
# sin significancia estadística?
reg_ajuste <- lm(
  vino$cardio ~ vino$muertes + vino$hepatic
)
reg_ajuste
summary(reg_ajuste)

# Comparemos estos últimos dos
# Pegar el nuevo modelo al final de la tabla existente
tabla_modelos <- tabla_modelos %>%
  bind_rows(
    glance(reg_ajuste) %>% 
      mutate(Modelo = "Modelo Ajuste") %>% 
      select(Modelo, r.squared, adj.r.squared, AIC, BIC)
  )

# Ver la tabla actualizada
tabla_modelos

# Revisión de residuales
vino$pred_car <- reg_ajuste$fitted.values
vino$res_car <- reg_ajuste$residuals

reg_res_car <- lm(vino$res_car~vino$pred_car) # "~" Regresión de los residuales con base en predicciones
reg_res_car
summary(reg_res_car)

# Gráfico de residuales contra predicciones
vino %>% ggplot(aes(x = vino$pred_car, y = vino$res_car)) + 
  geom_point(alpha = 0.6, color = "#001F82") + 
  geom_abline(intercept = coef(reg_res_car)[1], 
              slope = coef(reg_res_car)[2], 
              color = "#0099F8", linewidth = 1.5)+
  labs(title = "Predicciones VS Residuales",
       subtitle = "Muertes Cardio ~ Muertes + Muertes Hepatic",
       x = "Predicción Muertes por Cardiopatía",
       y = "Residuales") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8",
                              size = 17,
                              face = "bold"),
    plot.subtitle = element_text(color = "#969696", size = 13, face = "italic"),
    axis.title = element_text(color = "#969696",
                              size = 10,
                              face = "bold"),
    axis.text = element_text(color = "#969696", size = 10),
    axis.line = element_line(color = "#969696")
  )

### OJO CON INTERPRETACIONES!!!!


# ==========================================
# VARIABLES CATEGORICAS
library(fastDummies)

# ¿Y si quisiéramos usar una variable categórica?
reg_cat <- lm(
  cardio ~ muertes + hepatic + continente,
  data = vino
)
reg_cat
summary(reg_cat)

# ¿Cómo funciona?
unique(vino$continente)

vinoDummy <- dummy_cols(vino, 
                        select_columns = "continente",
                        remove_selected_columns = FALSE,
                        remove_first_dummy = TRUE
)

# ¿Y si quisiéramos cambiar el caso base?
vino$continente <- relevel(factor(vino$continente), ref = "Europa")

# Corremos nuevamente el modelo
reg_cat <- lm(
  cardio ~ muertes + hepatic + continente,
  data = vino
)
reg_cat
summary(reg_cat)
