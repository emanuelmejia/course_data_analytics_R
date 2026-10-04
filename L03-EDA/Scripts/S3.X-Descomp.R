library(tidyverse)

# Cargamos datos
pas <- AirPassengers
plot(pas)

# Función para descomponer la serie
pas.decom <- decompose(AirPassengers, type="mult")

# Gráfico de la descomposición
plot(pas.decom)

# Extraer componente de tendencia y estacionalidad
tendPas <- pas.decom$trend
estPas <- pas.decom$seasonal
ranPas <- pas.decom$random

# Gráfico de los componentes
ts.plot(cbind(pas, tendPas, tendPas * estPas), 
        lty=1:3,
        lwd = c(2,1.5,1.5),
        col = c("gray","orange","blue")
)
legend("topleft", 
       legend=c("Serie", "Tendencia", "Tendencia x Estacion"),
       col=c("gray","orange","blue"), lty=1:3)