# Tipos de Datos Básicos --------------------------------------------------

# Datos Numéricos ---------------------------------------------------------

class(1.1)

# Suma

5 + 5

# Resta

11 - 18

# Multiplicación

7 * 7

# División

19 / 5

# Elevar a una potencia

2 ^ 6
2 ** 6  #Esta es otra opción
exp(1) #Elevar el número de Euler a una potencia (e^x)

# Raíz cuadrada

sqrt(64)
64 ^ 0.5 #Otra opción

# Operación Módulo

19 %% 5

# Logaritmo (Por default es logaritmo natural)

?log #Pedir ayuda de una función
help(log) #Otra forma de pedir ayuda

log(10)
log(1)
log(exp(1))
log(0) 

# Logaritmos con otras bases
log(125,5) # El segundo número es la base, default ln (base e)
log(125,base = 5) # Otra opción es escribir también "base="
log(base = 5, 125) # Otra opción, si se especifica el argumento podemos ponerlo en desorden (las que no se especifiquen se toman en el orden de la función)

# Algunos tipos comunes de logaritmos tienen su propia función
log10(100) #Logaritmo base 10
log2(32) #Logaritmo Binario


# Factorial
factorial(4)
factorial(6+5)

# Funciones Trigonométricas (están en radianes)

sin(pi) # Por capacidad del computador hace la mejor aproximación que puede
sin(0)
sin(3*pi/2)

cos(0)
cos(pi)
cos(pi/2)

# Revisar estos dos ejemplos para entender cómo funciona la memoria de decimales en R
sin(pi)+100-100 # sin embargo actúa como un "cero numérico" y al sumarle 100 se pierden los últimos decimales
100-100+sin(pi) # pero por esto mismo la suma con "ceros numéricos no es conmutativa exactamente en R

# PEDMAS
# Paréntesis
# Exponentes
# División y Multiplicación
# Adición y Sustracción
6 / 2 * 1 + 2
6 / 2 * (1 + 2)
6 / (2 * (1 + 2))

# Variables --------------------------------------------

# ¿Cómo asignamos un valor a una variable?

x <- 5 # Asignar
x + 10 # Utilizar

z = 6 # Asignar (otra forma)
z + 10 # Utilizar

z = z + 5 # Guarda lo que tenía z anteriormente, le suma 5 y lo vuelve a guardar en z
z

12 -> w # Asignar (única forma en que asignación va de izquierda a derecha)

# Datos Caracteres / Character --------------------------------------------

y <- "Hola" # Asignar una cadena a la variable con comillas

class(y)

f <- 'Hola' # Otra forma (con comillas sencillas)
# f <- Hola # Error, porque piensa que Hola es una variable

nombre <- "Emanuel"
apellido = 'Mejía'

oracion = "'Había una vez'" # única opción para poner comillas en un texto es con comilla simple
oracion

# paste = función para "concatenar"
paste ("Hola",", buenos días") # con textos
paste (nombre,apellido) # con variables
paste (y,"mi nombre","es",nombre,apellido) # Mixto
paste (nombre,apellido,sep = "-") # "sep =" para elegir el separador (default es un espacio)

nombre_completo <- paste (nombre,apellido) # asignar a un objeto el resultado de una operación

edad = 34

paste(nombre_completo,"tiene",edad,"años.")

# función para borrar la asignación que tiene un objeto
rm(x)
rm(oracion,f) # Puede borrarse más de un objeto

# función para pasar todo a mayúsculas
toupper(nombre_completo) # el objeto se queda intacto

# función para pasar todo a minúsculas
tolower(nombre_completo)


# Invocar una paquetería instalada
library(stringr) # se necesita este package para usar la función en cuestión
# función para pasar todo a "nombre propio" (primera letra en mayúsculas)
str_to_title("EMANUEL MEJIA") # si no se tiene la librería mandará un error

# Lógicos / Booleanos -----------------------------------------------------

# Igualdad / Diferente

5 == 2+3 # es igual a
5 != 5 # es diferente de

class(TRUE)
class(T) # También se puede colocar solo la primera letra mayúscula

# Mayor / Menor

x = 3
y <- 10

x < y
x > y

x < 3

# Mayor o igual / Menor o igual

x <= 3
10 >= y

# ¿Qué pasa con caracteres?

"Hola" == 'Hola'

"a" > "b"
"a" < "b"

# Revisar este siguiente ejemplo
"Hombre" > "Mujer"
"Hombre" < "Mujer"

# Esto sucede porque R toma como mayor a aquel que esté más adelante en el alfabeto
"10" == 10 # Esto sucede por un Cast: Es cuando R convierte un objeto a otro de un tipo diferente para poder compararlos
"10" == as.character(10) # Esto es lo que R hace en el fondo
"10 " == 10 # Por eso mismo al añadir un espacio ya lo considera Falso

"10 " != 10

class( x != y ) # Estas operaciones son de tipo lógico

# Funciones para Booleanos

( 0 < x ) & ( x < y )
0 < x & x < y # También funciona sin paréntesis, aunque se usa para dar claridad

# Tablas de verdad

# AND / Y / CONJUNCIÓN / INTERSECCIÓN
T & T
T & F
F & T
F & F

# OR / O / DISYUNCIÓN / UNIÓN

T|T # Se hace con el caracter pipe
T|F
F|T
F|F

# OR Exclusivo. O una o la otra, pero no ambas

xor(T,T)
xor(T,F)
xor(F,T)
xor(F,F)

# Coerción (Convertir en otro tipo de dato)---------------------------------------------

class(edad)
edad + 5

class (nombre)
# nombre + 5 # error si se maneja un tipo de objeto que no es adecuado para la función
# "5" + 10 # será el mismo error

# Caracter - Número
as.character(10)
as.numeric("HOla")
as.numeric("10")

# Caracter - Lógico

as.logical("HOLA")
as.character(TRUE)
as.logical("TRUE")

# Números - Lógicos

as.numeric(TRUE) # TRUE se toma como 1 en numérico, revisar ejemplo siguiente
as.numeric(FALSE) # FALSE es equivalente a 0

TRUE + TRUE
1 == TRUE

as.logical(0)
as.logical(1)

# Detalle al convertir números a lógicos: CUALQUIER otro número que no sea 0, será tomado como TRUE en lógico

as.logical(7)
as.logical(-pi)

sqrt(-4) # Tienes que definirlo como complejo. Dos alternarivas:

sqrt(-4 + 0i)
sqrt(as.complex(-4))

( 2 + 5i ) / ( 0 + 4i ) # Es necesario poner la parte entera y la parte imaginaria para que se hagan las operaciones, aunque estas sean 0

