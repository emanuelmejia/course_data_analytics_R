# Ingresar los datos
oracion <- readline("Ingresa palabras separadas por espacios: ")

# Dividimos la oración en una lista de palabras
palabras <- unlist(strsplit(oracion, " "))

# Verificando la diferencia entre ambas variables
print(oracion)    # Char
print(palabras)   # Vector

# Paso 1
elegida <- ""                                 

# Paso 2 y 4
for (i in 1:length(palabras)){
  # Paso 3
  if(nchar(palabras[i]) > nchar(elegida)){
    elegida <- palabras[i]
  }
}

# Paso 5
print(elegida)
