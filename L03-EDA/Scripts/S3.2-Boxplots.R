library(skimr)

data(airquality) # Guardar el dataset en memoria

# ==========================================
# PERFILADO INICIAL

head(airquality)
perfil <- skim(airquality)
perfil

# ==========================================
# GRÁFICOS BÁSICOS TEMPERATURA

# Gráfico de línea de temperatura  
plot(airquality$Temp, 
     type = "l",
     main = "Niveles de Temperatura", 
     xlab = "Index", 
     ylab = "Temperatura (Farenheit)", 
     col = "red")

# Gráfico de caja de temperatura
boxplot(airquality$Temp,
        main = "Boxplot de Temperatura", 
        ylab = "Temperatura (Farenheit)", 
        col = "salmon")

# Comparar con perfil
perfil

# ==========================================
# GRÁFICO ESTILIZADO TEMPERATURA

# Extraer las estadísticas de la caja (Mínimo, Q1, Mediana, Q3, Máximo)
stats <- boxplot.stats(airquality$Temp)$stats

# Crear un dataframe auxiliar para las etiquetas
df_etiquetas <- data.frame(
  x = 0.15,  # Posición en el eje X (a la derecha de la caja)
  y = stats,
  texto = paste0(c("Mín", "Q1", "Mediana", "Q3", "Máx"), ": ", stats, " °F")
)

# Generación del Gráfico
ggplot(airquality, aes(x = 0, y = Temp)) +
  # Caja principal con color cyan neón y bordes blancos
  geom_boxplot(fill = "coral", color = "white", width = 0.2, 
               outlier.color = "#FF007F", outlier.size = 3) +
  
  # Agregar las etiquetas usando el dataframe auxiliar
  geom_text(data = df_etiquetas, aes(x = x, y = y, label = texto), 
            color = "white", fontface = "bold", hjust = 0) +
  
  # Ampliar los límites del eje X para que el texto no se corte
  scale_x_continuous(limits = c(-0.2, 0.6)) +
  
  # Títulos y subtítulos
  labs(
    title = "Boxplot Temperatura",
    subtitle = "@ LaGuardia Airport",
    y = "Temperatura (Fahrenheit)",
    x = NULL
  ) +
  
  # Tema base minimalista
  theme_minimal() +
  
  # Personalización extrema para el tema oscuro
  theme(
    plot.background = element_rect(fill = "#121212", color = NA),
    panel.background = element_rect(fill = "#121212", color = NA),
    text = element_text(color = "white"),
    axis.text.y = element_text(color = "gray80", size = 10),
    axis.text.x = element_blank(), # Ocultamos el texto del eje X numérico
    panel.grid.major.y = element_line(color = "#333333"), # Cuadrícula horizontal tenue
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 18, color = "coral"),
    plot.subtitle = element_text(face = "italic", size = 12, color = "gray70", margin = margin(b = 15)),
    axis.title.y = element_text(margin = margin(r = 15), face = "bold")
  )

# Comparar con perfil
perfil

# ==========================================
# GRÁFICOS BÁSICOS VIENTO

# Gráfico de línea de temperatura  
plot(airquality$Wind, 
     type = "l",
     main = "Velocidad Promedio del Viento", 
     xlab = "Index", 
     ylab = "Velocidad del Viento (mph)", 
     col = "blue")

# Gráfico de caja de temperatura
boxplot(airquality$Wind,
        main = "Boxplot de Viento", 
        ylab = "Velocidad del Viento (mph)", 
        col = "lightblue")

# Comparar con perfil
perfil

# ==========================================
# GRÁFICO ESTILIZADO VIENTO

# Extraer las estadísticas de la caja (Mínimo, Q1, Mediana, Q3, Máximo)
stats <- boxplot.stats(airquality$Wind)$stats

# Crear un dataframe auxiliar para las etiquetas
df_etiquetas <- data.frame(
  x = 0.15,  # Posición en el eje X (a la derecha de la caja)
  y = stats,
  texto = paste0(c("MínIQ", "Q1", "Mediana", "Q3", "MáxIQ"), ": ", stats, " mph")
)

# Generación del Gráfico
ggplot(airquality, aes(x = 0, y = Wind)) +
  # Caja principal con color cyan neón y bordes blancos
  geom_boxplot(fill = "#00E5FF", color = "white", width = 0.2, 
               outlier.color = "#FF007F", outlier.size = 3) +
  
  # Agregar las etiquetas usando el dataframe auxiliar
  geom_text(data = df_etiquetas, aes(x = x, y = y, label = texto), 
            color = "white", fontface = "bold", hjust = 0) +
  
  # Ampliar los límites del eje X para que el texto no se corte
  scale_x_continuous(limits = c(-0.2, 0.6)) +
  
  # Títulos y subtítulos
  labs(
    title = "Boxplot Viento",
    subtitle = "@ LaGuardia Airport",
    y = "Viento (mph)",
    x = NULL
  ) +
  
  # Tema base minimalista
  theme_minimal() +
  
  # Personalización extrema para el tema oscuro
  theme(
    plot.background = element_rect(fill = "#121212", color = NA),
    panel.background = element_rect(fill = "#121212", color = NA),
    text = element_text(color = "white"),
    axis.text.y = element_text(color = "gray80", size = 10),
    axis.text.x = element_blank(), # Ocultamos el texto del eje X numérico
    panel.grid.major.y = element_line(color = "#333333"), # Cuadrícula horizontal tenue
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 18, color = "#00E5FF"),
    plot.subtitle = element_text(face = "italic", size = 12, color = "gray70", margin = margin(b = 15)),
    axis.title.y = element_text(margin = margin(r = 15), face = "bold")
  )

# Comparar con perfil
perfil

# ==========================================
# CHALLENGE - OZONO
# Concentración de Ozono Promedio en partes por billon
# Medida de las 13:00 a 15:00 hrs en Roosevelt Island

# Elaborar los gráficos de línea y caja para la variable Ozone, 
# Escribe una interpretación del gráfico o los datos y envíala por correo
# O agrégala como comentario al finalizar tu ejercicio

# Tip: Ojo con los NAs!

# Gráfico de línea de temperatura
