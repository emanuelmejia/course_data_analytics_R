
# Factores ----------------------------------------------------------------

# Los factores son un tipo de dato nominal.

# Nivel de Estudios
# 1. Licenciatura.
# 2. Maestría.
# 3. Doctorado.

1+1 # ¿licenciado + licenciado?

info <- c(1,2,2,3,2,1,1,1)
info 
# Yo podría operar con este vector
info + 10

# Para construir un factor
f_info <- factor(info)
class(f_info) # Este es un tipo de dato factor.

f_info
f_info + 10 # No tiene sentido operar algebráicamente los factores

levels(f_info) 
levels(f_info) <- c("Licenciatura","Maestría","Doctorado") # Codificando los números

f_info

# ¿Qué pasa cuando hago un resumen?
summary(info)
summary(f_info)

# ¿Cómo se ordenan?
f_info[3] > f_info[1]

# Factores ordenados.

# ¿Qué tan rápido conduces?

# 1. Lento
# 2. Normal
# 3. Rápido
# 4. Toretto

set.seed(10)
# sample: crea una muestra aleatoria
?sample
info <- sample(x = 1:4,                   # Con este vector
               size = 100,                # Tamaño de muestra
               replace = TRUE,            # Con reemplazo
               prob = c(0.3,0.4,0.1,0.1)) # Con esta probabilidad (respectivamente) 
summary(info)
table(info)

# Categorizar
f_info <- factor(info)
summary(f_info)

# Codificación de los números
levels(f_info) <- c("Lento","Normal","Rápido","Toretto")
f_info
summary(f_info)

# Ordenar los factores
?ordered
o_info <- ordered(f_info, # Con este vector de factores
                  levels=c("Lento","Normal","Rápido","Toretto")) # Ordenando de menor a mayor.

o_info

o_info[4] < o_info[85]

# Ordenar un objeto de menor a mayor
sort(o_info) # Orden creciente
sort(o_info,decreasing = TRUE) # Orden decreciente

# Listas ------------------------------------------------------------------

# Las listas me permiten guardar distintos tipos de dato en un mismo objeto.

# Para crear una lista
L <- list(4,       # Primera entrada de la lista
          c(T,F,T),# Segunda entrada de la lista...
          matrix(1:9,3,3)) # Tercera: una matriz

L

# Mi objetivo es sumarle al '4' de la lista el número 5. 
L[1] + 5 # Esto no funciona

class(L)

L[[1]] # En el caso de listas, para seleccionar un elemento, usamos [[]]
L[[1]] + 5
class(L[1])   # Aquí es una lista
class(L[[1]]) # Aquí es un número

L[3]
L[[3]] # Seleccionando a la matriz

# Mi objetivo es seleccionar la segunda columna de la matriz, en los renglones 1 y 3.
L
L[[3]][c(1,3),2] # Seleccionar elementos de una matriz dentro de una lista.

# Doctores.
Dra.Michelle <- list(Nombre = "Michelle",
                     Edad = 44,
                     Especialidad = c("Neuróloga", "Pediatra"),
                     Salario = 125000)
# Estamos nombrando a las entradas de la lista.
Dra.Michelle

# Cuando la lista tiene nombres, podemos usar la notación '$'
Dra.Michelle[[1]]
Dra.Michelle$Nombre
class(Dra.Michelle$Nombre)

names(Dra.Michelle)[3] <- "Especialidades"
Dra.Michelle

# Para usar elementos dentro de la lista
Dra.Michelle[[3]][2]
Dra.Michelle$Especialidades[2]

# Agregar un elemento a la lista.
Dra.Michelle$Hospital <- "Pediatría General"
Dra.Michelle

# Data frames -------------------------------------------------------------

# Un data.frame es una tabla rectangular de datos 
# Lista cuyos elementos son vectores de la misma longitud.
?NA

nombre <- c("Rubén","Regina","Jocelyn","Ángel","Carlos","Sara","Edgar")
edades <- c(20,18,22,21,29,NA,24)
carrera <- c("Actuaría","Actuaría","Actuaría","Finanzas","Economista","Biología","Actuaría")
genero   <- factor(c("M","F","F","M","M","F","M"))

nombre
edades
carrera
genero

library(dplyr)
# Lo que quiero es que todos tienen la misma longitud.
c(edades %>% length(),carrera %>% length(),genero %>% length()) %in% length(nombre)

# Otro ejemplo... chafa.
length(nombre) %in% c(edades %>% length(),
                      carrera %>% length(),
                      genero %>% length())

df <- data.frame(Nombre = nombre,
                 Edad = edades,
                 Carrera = carrera,
                 Genero = genero)

# Ejemplo de tabla:
df
View(df)

# En R existen data.frame ya creados por default.
?iris
iris
View(iris) # Sirve para ver data.frames de forma más cómoda.

# Un data.frame se puede tratar como listas o como matrices
# pero las listas van sobre las columnas del data.frame.
df$Nombre
df[1,1]
df[,1]

# Quiero los nombres de las personas que tienen menos de 25 años.
df
df$Edad<25

df$Nombre[df$Edad<25] 

# Operador lógico útil. ~~~~~~~~~~~~~~~~~~~~~
# x %in% y : Verifica, entrada por entrada del objeto 'x' si se encuentra en el objeto 'y'.
# Vamos a ver entrada por entrada si los elementos de 'x' pertenecen a 'y'.
# Ejemplo 1:
resultado = c(1,3) %in% 1:10 # Todos los elementos {1,3} pertenecen al conjunto {1,2,...,10}
as.logical(resultado)
# Ejemplo 2:
c("Hola","Amigos","jajaja") %in% c("Hola","Buenas","Tardes") 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Otro operador súper útil ~~~~~~~~~~~~
library(magrittr) ; library(dplyr)
# %>% (pipe) : Sirve para evitar confusiones con los paréntesis.
# objeto %>% funcion(): Lo que hace es meter el objeto, en la función.
# Esto es lo mismo que hacer funcion(objeto).

# NOTA: Para escribir rápido %>% usa el shortcut CTRL+SHIFT+M ~~~~

# Ejemplo 1:
5 %>% factorial() # = factorial(5)
# Ejemplo 2:
10 %>% log(base = 10) # = log(10,base=10)
# Ejemplo 3:
3 %>% factorial() %>% log(base = 6) %>% exp()
exp(log(factorial(3),base=6))