library(dplyr)
# Estructuras de Control --------------------------------------------------

# if, else, ifelse

# Sintaxis de un if en R

x <- 45

if(x %% 2 == 0){
  print("Es par")
}else{ # Si la condición del if es "TRUE" R ya no ejecuta lo que está en el else
  print("Es impar")
}

# Ejemplo: if - else if - else

x <- runif(1, min = -1, max = 1)
x

if(x<0){
  z = paste(x,"es negativo")
}else if(x>0){
  z = paste(x,"es positivo")
}else{
  z = "Tu número es cero"
}

show(z)


# Ciclos ------------------------------------------------------------------

# for , while, repeat

# Ciclo For
# Usar ciclo for para sumar los primeros cien números

aux <- 0
for (i in 1:100) {
  aux <- aux + i
}

aux

# Ciclo While
# Ejemplo WARNING

# while (1>0){
#   print("Te pasas de lanza")
# }

# La condición establece el momento de paro
# Ejemplo

i = 1

while (i<=10){
  print(i)
  i = i + 1 # Esto es lo que va a detenerlo
}

# While es un ciclo que termina hasta que la condición se rompe

# Ciclo for: Corre un índice sobre un vector

vector <- c("Edgar","Emanuel","Carlos","Regina")

for(nom in vector){
  w <- paste("Hola",nom)
  print(w)
}

# Ciclo While
# Verificar que un número es primo

n <- 81
i = 2

while( n %% i != 0){
  # print(i) para mostrar i
  # Si el residuo es diferente de 0
  # es porque el número no lo divide
  i <- i+1
}

ifelse(i==n,
       paste("El número",n,"es primo"),
       paste("El número",n,"no es primo y es múltiplo de",i))

# Repeat

# Ejemplo que no se debe correr

# repeat {
#   print ("Hola")
# }
# El ciclo se repetirá indefinidamente


# Ejemplo más amigable
j = 1
repeat{
  print(j)
  j = j+1
  if (j==5){break} # Tienes que poner algo que rompa el ciclo (break)
}


# Ciclos para data.frames -------------------------------------------------

iris
View(iris)

# Ejercicio 1
# Sumar por columnas el data frame

i <- 1
resultados <- c()

repeat{
  resultados[i] <- sum(iris[,i])
  names(resultados)[i] <- colnames(iris)[i] # Para ponerle el mismo nombre
  i <- i + 1
  # La condición debe ser sobre todas las columnas menos la última.
  # En este caso la última ya no entraría al ciclo Cuando i sea 5 ya no va a a 
  if(i==ncol(iris)){break}
}

resultados

# Forma vectorial
apply(iris[,1:4],2,sum) # De iris, las columnas 1 a 4 hará la operación suma por columnas (el número 2 indica que es por columnas)

# Función tapply
tapply(iris$Sepal.Length, iris$Species, mean)

# tapply puede usarse con diferentes funciones
tapply(iris$Sepal.Length, # Toma este vector
       iris$Species,      # Con base en este índice/bandera/etiqueta
       summary)           # Obtén esta operación


# Funciones en R ----------------------------------------------------------

h <- function(x){
  return(x + 10)
}

h(5)

# Si una función no tiene un return, entonces va a regresar el último objeto que encuentre

# Ejemplo:
h2 <- function(x){
  x - 25
  x + 10
}

h2(9)

# Si quieres que muestre la operación intermedia:

h2_b <- function(x){
  print(x - 25)
  x + 10
}

h2_b(7)

y <- 10 + 5

# Aunque haya prints SOLO HAY UN RETURN. Ejemplo:

y <- h2_b(7)
y

# Cuando la función encuentra un return() regresa lo que está dentro y TERMINA

# OJO con los parámetros/argumentos que recibe la función

r <- function(x){
  if(is.numeric(x)){
    return(x*10)
  }
  
  return("¿Qué estás haciendo?")
  # Revisar que no se necesita un else al tener el return.
  # Si entra al if, el return lo va a sacar de la función
  # Si no fuera una función si se necesitaría un else
}

r(78)
r("jejeje")

# Opción con warning

r2 <- function(x){
  if(is.numeric(x)){
    return(x*10)
  }
  
  warning("No te pases ¿Qué estás haciendo?")
}

r2("jejeje")

# El caso extremo stop

r3 <- function(x){
  if(is.numeric(x)){
    return(x*10)
  }
  
  stop("No te pases ¿Qué estás haciendo?")
  paste(x, "te pasas")
}

r3("jejeje")

# Pueden tener más de un argumento

resta <- function(x,y){
  z <- x-y # Los objetos creados dentro de una función no se guardan en el ambiente
  return(z)
}

resta(5,6)
resta(y=6,x=5) # Se pueden definir los valores con un =, como en cualquier otra función


# Ejemplo 4

# Función verificadora de primos

primo <- function(n){
  # n := Es el número que queremos saber si es primo o no
  i = 2
  while(n%%i!=0){
    i <- i+1
  }
  
  ifelse(i==n,
         paste("El número",n,"es primo"),
         paste("El número",n,"no es primo y es múltiplo de",i)) %>% print()
  
  return(i)
}


x <- primo(7)
y <- primo(81)
x
y

# Una función regresa uno y solo UN OBJETO

# Ejemplo
# Tomar un data.frame y regresar promedio de los valores, el máximo y el mínimo de la primer columna

fun <- function(x){
  # x := Es una matriz
  ls <- list()
  # Promedio
  ls$Promedio = mean(x[,1])
  # Para el máximo
  ls$Máximo = max(x[,1])
  # Para el mínimo
  ls$Mínimo = min(x[,1])
  
  return(ls)
}

fun(iris)

unlist(fun(iris)) # Para hacerlo vector