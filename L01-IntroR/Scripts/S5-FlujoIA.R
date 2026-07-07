# Get working directory para saber en dónde estamos
getwd()

# Carga Básica de Archivos -------------------------------------

# Set working directory

#### OJO!!!!!!!!!!!!!!!!!!!!!!!!!!!
#### PONER AQUÍ ABAJO LA UBICACIÓN EN SU COMPUTADORA
#### EN DONDE SE ENCUENTRAN LOS DATOS QUE QUEREMOS CARGAR
setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/course_data_analytics_R/L01-IntroR/data")

# Puede seleccionarse manualmente usando el Shortcut CTRL+SHIFT+H
# Pero entonces se tendrá que hacer cada vez que se abra el archivo

# Ejemplos de Prompts para generar código:

#############################
# Eres un experto en R y en la librería dplyr. 
# Escribe un script que cargue el archivo 'ventas.csv' 
# desde el directorio actual en un dataframe. 
# A continuación agrupa los datos por 'Region' y 'Fecha', 
# y calcula la suma total de ventas para cada combinación

#############################
# Usando el resultado del paso anterior calcula
# la media, la mediana y la desviación estándar por región de ventas.

#############################
# Usando los datos cargados y la librería ggplot2
# genera un gráfico de barras. 
# El eje X debe ser la Región, el eje Y las Ventas Totales,
# El color de relleno (fill) debe ser la Categoría. 
# Que las barras aparezcan lado a lado (agrupadas), no apiladas. 
# Usa un tema minimalista.


#############################
# Usando los datos cargados, la librería lubridate y ggplot2
# Genera un gráfico de línea mostrando las ventas mensuales totales. 
# El eje X debe ser la Fecha en formato "Mes-Año", el eje Y las Ventas Totales.
# La fecha original está en formato Día-Mes-Año.
# Usa un tema minimalista. 


############################
# A partir de los datos de ventas mensuales, 
# Calcula un modelo de regresión lineal 
# donde la variable dependiente sean las Ventas_Totales 
# y la independiente sea el mes. Muestra la tabla de resumen.
# Después, usando ggplot2, actualiza el gráfico de líneas temporal 
# superponiendo la recta de regresión de este modelo (usa geom_smooth). Haz que la recta de tendencia destaque visualmente (por ejemplo, en color rojo y punteada) y mantén el tema minimalista."


############################
# Modifica únicamente el código del gráfico final 
# aplicando las siguientes características visuales:
# Utiliza un tema oscuro general y elimina las líneas de enrejado 
# (grid) únicamente del eje X.
# Dibuja la línea de ventas en color cornflowerblue.
# Dibuja la línea de regresión en color coral.
# Título: Formato en negrita y color cornflowerblue.
# Subtítulo: Color gris y letras cursivas
# Etiquetas y ticks de ambos ejes (X e Y): Color gris
# Ticks del eje Y con formato de moneda utilizando librería scales
