# Vectores ----------------------------------------------------------------

Z <- c(1,2,3,4,5) # Para crear un vector, revisar que la función es c minúscula
Z

# Seleccionar entradas del vector

vector <- c("Hola",10,TRUE,0)
vector # Un vector solo guarda un tipo de dato a la vez, y depende de la jerarqu?a, en este caso todos son character

vect <- c(10,TRUE,0)
vect # En este caso se guardan como números

vecnum <- c(3,6,9,10,1,0)

vecnum * 2

# Regla de oro: R TRABAJA TODO VECTORIALMENTE, todo lo piensa como si fuese un vector y las siguientes operaciones las hace entrada por entrada
# Aunque en álgebra lineal no tengan solución, en R lo toma entrada por entrada. Ejemplos:
vecnum + 10
exp (vecnum)
log (vecnum)

# Vectores de caracteres

v1 <- c("Hola",10,TRUE,F) ; v2 <- c("Adios",20,FALSE,T)
v1 ; v2

v1 + v2 # error por no ser numérico

paste(v1,v2) # Regla de oro nuevamente, todo lo hace entrada por entrada!

# Hablando de números

w1 <- 5 * c(2,3,5,10) ; w2 <- 3:6 # Otra forma de definir un vector es con un rango

w1 + w2
w1 * w2

# Entradas de vectores ----------------------------------------------------

# Trabajar con las entradas de un vector

r <- 100 : 200
r
length(r)

k <- 200 : 100
k

#Seleccionar una entrada

r[10] # nos muestra la entrada 10 del vector r

# Se puede seleccionar más de una entrada

r[c(1,101,3)] # Nos devuelve la entrada 1, 101 y 3 del vector r en ese orden
r[1:10] # Nos devuelve las entradas del 1 al 10
r[
  c(
    1:10,
    (length(r)-10):length(r)
    )
  ] # seleccionar los primeros 10 y los ultimos 10

# Selección Booleana

# Devuelve los TRUE y no devuelve los FALSE

r[c(T,F)] # Selecciona uno si y otro no. Pivotea cada 2
r[c(F,T)] # Selecciona uno no y otro si. Pivotea cada 2
r[c(T,T,F)] #Selecciona dos si y uno no. Pivotea cada 2

# Condicionales en vectores

r < 150 # Devuelve un vector lógico de las entradas los que son menores a 140

r[r<150] # Para saber cuáles son específicamente
r[c(r<150)] # Otra forma

# ¿Cuántas entradas son? 

length(r[r<150])
sum(r<150) # Otra forma. Suma los 1 y 0 de la propuesta lógica


# Modificar entradas de un vector dado

V <- 1:5
V

V[3] <- 100 # Simplemente asignar el valor a la entrada
V

V[c(1,length(V))] <- c(15,20) # La primera y última entrada las sustituimos con 15 y 20
V

V[c(T,F)] <- "s"
V

# Matrices ----------------------------------------------------------------

?matrix

M <- matrix(1:4, nrow = 2, ncol = 2) # Especificar vector, número de filas y n?mero de columnas.
M # Por default se rellena por columnas

# Para rellenar por filas
N <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
N

# Entradas de Matrices ----------------------------------------------------

# Crear una matriz con 'n' columnas y 'm' renglones

n = 5 ; m = 3

MAT <- matrix(data = 1 : (n*m), # Estos son los datos que introducimos a la matriz
              ncol = n, nrow = m) # Tamaño n x m
MAT

# Seleccionar entradas de una matriz

MAT[2,2]

MAT[2,4]

# Modificar entradas de una matriz
MAT[3,3] <- 100
MAT

# Seleccionar "varias" entradas de una matriz

MAT[1,] # Selecciona la primer fila completa
MAT[,4] # Selecciona la cuarta columna completa.
# Ojo! El resultado arroja los datos como un vector

x <- MAT[,3]
x
x[3]


matrix(MAT[,3] , nrow = 3 , ncol = 1) # Para que no deje de ser matriz

# Una matriz solo puede guardar un tipo de dato

MAT[2:3,] # Selecciona el segundo y tercer renglón
mat <- MAT[c(1,3),]
mat

# Para ver las dimensiones la matriz

dim(MAT)
dim(mat)

# Vamos a extraer una submatriz

MAT

y <- MAT[c(1,3),c(1,3)]
y

# Paréntesis Importante
# Semilla para números aleatorios
set.seed(2000)
MX <- matrix(c(1:100, runif(100)), nrow = 100 , ncol = 2)
MX

MX[-c(100,97,94),] # Selecciona todo menos los renglones 100, 97 y 94

MX[,-1]
MX[,-2]

# Seleccionar bajo una condición

cond = MX[,2] <= 0.75 # Identificar todos los renglones cuya segunda columna tenga un valor <= 0.75

MX[cond,]
MX[cond,2] # Devuelve la segunda columna de aquellos que cumplen la condición

summary(MX[cond,2]) # resumen estadístico seguna columna con condición
summary(MX[,2]) # Sin condición

# Operación de Matrices

A <- matrix (1:4,2,2)
B <- matrix (c(1,2,1,3),2,2)

# Transponer una matriz

t(A)
t(B)

# Obtener diagonal de una matriz

a <- diag(A)
a[2]

b <- diag(B)
b

# A veces depende de qué reciba una función, los resultados pueden ser diferentes:

diag(A) # A es una matriz, la función me lanza la diagonal de la matriz
diag(a) # a es un vector, la función me lanza una matriz diagonal

summary(A) # Summary de una matriz
summary(a) # Summary de un vector

# Multiplicación de matrices

A
B
A*B # Multiplica entrada por entrada, esto no es una multiplicación algebráica de matrices

A%*%B # Multiplicación de matrices algebráica
B%*%A

# Inversa de una matriz

A^-1 # Tomó entrada por entrada y calculó cada valor ^-1

solve(A) # Esta es la función para calcular la inversa

# Comprobando:

solve(A) %*% A # Nos da la matriz identidad

# Arreglos ----------------------------------------------------------------

# Matrices de dimensiones que nosotros queramos

arr <- array (data = 1:27, dim = c(3,3,3))
arr

arr[2,2,] # Los valores 2,2 de todas las caras
arr[,1,] # La columna 1 de todas las caras

class(arr)
class(A)
