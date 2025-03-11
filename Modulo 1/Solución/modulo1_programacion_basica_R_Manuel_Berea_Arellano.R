# Databricks notebook source
# MAGIC %md
# MAGIC # Evalución 2025 - Módulo 1 - Programación básica
# MAGIC Manuel Berea Arellano
# MAGIC En el siguiente cuaderno estarán todos los ejercicios pedidos en R para el apartado de programación básica

# COMMAND ----------

R.version.string

# COMMAND ----------

library(testthat)

# COMMAND ----------

# MAGIC %md
# MAGIC ## Cuestión 1
# MAGIC Se pide crear dos vectores de misma longitud con valores entre 1 y 50, siguiendo una distribución uniforme, para conseguir numeros aleatorios producidos por una distribución uniforme, utilizaremos la función de R base runif, utilizaremos como minimo el valor 1 y máximo 50.

# COMMAND ----------

# Definimos la longitud del vector y la semilla para poder reproducir el resultado aleatorio.
n <- 10     #Longitud
seed <- 17  #Semilla
m <- 1      #Min
M <- 50     #Max

# Generamos los dos vectores pedidos
set.seed(seed)
vector1 <- runif(n, min=m, max=M)
vector2 <- runif(n, min=m, max=M)

print("Tenemos los dos vectores:")
cat("\n")
print("Vector1:")
print(vector1)
cat("\n")
print("Vector2:")
print(vector2)


# COMMAND ----------

# MAGIC %md
# MAGIC Una vez generados los dos vectores se pide:
# MAGIC   - La suma de sus elementos
# MAGIC   - Productor escalar de los dos vectores
# MAGIC   - Correlación entre ambos

# COMMAND ----------

## Suma de sus elementos:
vector3 <- vector1 + vector2
print("La suma de ambos resulta en un vector 3:")
print(vector3)

# COMMAND ----------

## Producto escalar
# El producto escalar es la suma del producto de ambos vectores.
productoEscalar <- sum(vector1 * vector2)
print("Producto escalar:")
print(productoEscalar)

# COMMAND ----------

## Correlacion 
# Podemos utilizar la función cor() de R base.
correlacion <- cor(vector1,vector2)
print("Correlacion:")
print(correlacion)

# COMMAND ----------

# MAGIC %md
# MAGIC ## Cuestión 2
# MAGIC Se pide crear una función que dadas dos listas se encuestre los elementos comunes.
# MAGIC Utilizaremos como apoyo la function intersect() de R base.

# COMMAND ----------

interseccion <- function(list1, list2){
  # Esta funcion toma dos listas y devuelve su intersección.
  #
  # INPUT:
  #   list1 una lista
  #   list2 una lista
  #
  # Return:
  #   Intersección entre list1 y list2
  #
  # Ejemplos:
  #   interseccion(list(1,2,3,4,5),list(2,4))
  #   interseccion(list("a","b","c"),list("c","e"))
  
  # Comprobamos Inputs
  checkCuestion2(list1)
  checkCuestion2(list2)
  
  # Pasamos la lista a vector
  vec1 <- unlist(list1)
  vec2 <- unlist(list2)
  
  # Calculamos la intersección
  list3 <- intersect(vec1, vec2)
  return(list3)
}

checkCuestion2 <- function(lista){
  # Esta función comprueba que el input es correcto.
  if (!is.list(lista)) {
    stop("Las entradas deben ser listas.")
  }
  
  # Verifica si la lista está vacía
  if (length(lista) == 0) {
    stop("La lista debe contener elementos.")
  }
  
}

# COMMAND ----------

test_that("interseccion_lista funciona correctamente", {
  # Prueba con números
  expect_equal(interseccion(list(1, 2, 3, 4), list(3, 4, 5, 6)), c(3, 4))
  
  # Prueba con caracteres
  expect_equal(interseccion(list("a", "b", "c"), list("b", "c", "d")), c("b", "c"))
  
  # Prueba con valores repetidos
  expect_equal(interseccion(list(1, 1, 2, 3), list(1, 3, 4)), c(1, 3))
  
  # Prueba con listas sin intersección
  expect_equal(interseccion(list(10, 20, 30), list(40, 50, 60)), numeric(0))
  
  # Prueba con listas vacías
  expect_error(interseccion(list(), list(1, 2, 3)), "La lista debe contener elementos.")
  expect_error(interseccion(list(), list()), "La lista debe contener elementos.")
  
  # Prueba elementos que no son listas
  expect_error(interseccion("a", list("a","B","C")), "Las entradas deben ser listas.")
  expect_error(interseccion(list(1,2,3), 10), "Las entradas deben ser listas.")
  
})


# COMMAND ----------

# MAGIC %md
# MAGIC ## Cuestión 3
# MAGIC Se pide crear una funcion con dos entradas, una lista de palabras y un numero entero. Debe devolver las palabras que sean iguales o mayor en longitud al numero dado

# COMMAND ----------

filtradoPalabras <- function(lista, n){
  # Esta funcion toma una lista de palabras y un numero entero y devuelve las palabras cuya longitud sea mayor o igual al numero dado.
  #
  # INPUT:
  #   lista una lista de palabras
  #   n un numero entero
  #
  # Return:
  #   lista de palabras
  #
  # Ejemplos:
  #   filtradoPalabras(list("Hola","Sol","Si"),3)
  #   filtradoPalabras(list("No","Luna","funcion"),5)
  
  # Comprobamos Inputs
  checkCuestion3(lista,n)
  
  solucion <- list()
  l <- 1
  
  for(elemento in lista){
    if(nchar(elemento)>=n){
      solucion[[l]] <- elemento
      l <- l+1
    }
  }
  return(solucion)
  
}

checkCuestion3 <- function(lista,n){
  # Esta función comprueba que el input es correcto.
  if (!is.list(lista)) {
    stop("La primera entrada debe ser una lista de palabras.")
  }
  
  if (!all(sapply(lista,is.character))) {
    stop("La primera entrada debe ser una lista de palabras.")
  }
  
  if (!is.numeric(n)) {
    stop("La segunda entrada debe ser un numero entero.")
  }
  
  if (n != floor(n)) {
    stop("La segunda entrada debe ser un numero entero.")
  }
  
}

# COMMAND ----------

test_that("interseccion_lista funciona correctamente", {
  expect_equal(filtradoPalabras(list("Hola","Sol","Si"),3), list("Hola","Sol"))
  expect_equal(filtradoPalabras(list("No","Luna","funcion"),5), list("funcion"))
  expect_true(identical(filtradoPalabras(list("Hola", "Sol"), 10), list()))
  
  # Prueba con elementos que no son lista
  expect_error(filtradoPalabras(c("Hola","Como"), 2), "La primera entrada debe ser una lista de palabras.")
  expect_error(filtradoPalabras(10, 2), "La primera entrada debe ser una lista de palabras.")
  expect_error(filtradoPalabras(list("Hola",3,"Que"), 4), "La primera entrada debe ser una lista de palabras.")
  
  # Prueba elementos que no son numero entero
  expect_error(filtradoPalabras(list("Hola","Que","Tal"), 5.2), "La segunda entrada debe ser un numero entero.")
  expect_error(filtradoPalabras(list("Hola","Que","Tal"), "5"), "La segunda entrada debe ser un numero entero.")
  expect_error(filtradoPalabras(list("Hola","Que","Tal"), TRUE), "La segunda entrada debe ser un numero entero.")
  
})


# COMMAND ----------

# MAGIC %md
# MAGIC ## Cuestión 4
# MAGIC Se pide crear una funcion que devuelva la suma de los elementos divisibles por un numero n entre 1 y 50.

# COMMAND ----------

divisibles <- function(n){
  # Esta funcion toma un numero entero n y devuelve la suma de los divisibles por n entre 1 y 50
  #
  # INPUT:
  #   n un numero entero
  #
  # Return:
  #   Un numero resultado de la suma de los divisibles por n entre 1 y 50
  #
  # Ejemplos:
  #   filtradoPalabras(3)
  #   filtradoPalabras(10)
  
  # Comprobamos Inputs
  checkCuestion4(n)
  
  v <- 1:50
  
  solucion <- v[v %% n == 0]
  
  return(solucion)
  
}

checkCuestion4 <- function(n){
  # Esta función comprueba que el input es correcto.
  if (!is.numeric(n)) {
    stop("N debe ser un numero entero.")
  }
  
  if (length(n)!=1) {
    stop("N debe ser un numero entero.")
  }
  
  if (n != floor(n)) {
    stop("N debe ser un numero entero.")
  }
  
  if (n>=50) {
    stop("N debe ser menor o igual que 50.")
  }
  
}

# COMMAND ----------

test_that("interseccion_lista funciona correctamente", {
  expect_equal(divisibles(3), c(3L, 6L, 9L, 12L, 15L, 18L, 21L, 24L, 27L, 30L, 33L, 36L, 39L, 42L, 45L, 48L))
  expect_equal(divisibles(5), c(5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L))
  
  # Prueba con elementos que no son lista
  expect_error(divisibles("Hola"), "N debe ser un numero entero.")
  expect_error(divisibles(list(5,2)), "N debe ser un numero entero.")
  expect_error(divisibles(c(5,2)), "N debe ser un numero entero.")
  expect_error(divisibles(TRUE), "N debe ser un numero entero.")
  expect_error(divisibles(5.2), "N debe ser un numero entero.")
  expect_error(divisibles(100), "N debe ser menor o igual que 50.")
  
})


# COMMAND ----------

# MAGIC %md
# MAGIC ## Cuestión 5
# MAGIC Se pide crear una funcion que devuelva el numero de vocales de una palabra

# COMMAND ----------

nvocales <- function(palabra){
  # Esta funcion toma una palabra y cuenta el numero de vocales que tiene
  #
  # INPUT:
  #   palabra una cadena de caracteres
  #
  # Return:
  #   Numero de vocales que contiene
  #
  # Ejemplos:
  #   nvocales("QUE")
  #   nvocales("Hola")
  
  # Comprobamos Inputs
  checkCuestion5(palabra)
  
  solo_vocales <- gsub("[^aeiouAEIOU]", "", palabra)    
  
  return(nchar(solo_vocales))
  
}

checkCuestion5 <- function(palabra){
  # Esta función comprueba que el input es correcto.
  if (!is.character(palabra)) {
    stop("La entrada debe ser tipo caracter.")
  }
  
  if (length(palabra)!=1) {
    stop("La entrada debe ser una palabra.")
  }
  
  if (!grepl("^[a-zA-Z]", palabra)) {
    stop("La entrada debe ser una palabra.")
  }
  
  if (nchar(palabra)==0) {
    stop("La entrada debe ser una palabra.")
  }
  
  if (grepl("\\s", palabra)) {
    stop("La entrada no debe ser una frase.")
  }
  
}

# COMMAND ----------

test_that("interseccion_lista funciona correctamente", {
  expect_equal(nvocales("Que"), 2)
  expect_equal(nvocales("hOla"), 2)
  
  # Prueba con elementos que no son lista
  expect_error(nvocales("Hola que tal"),"La entrada no debe ser una frase.")
  expect_error(nvocales(list(5,2)), "La entrada debe ser tipo caracter.")
  expect_error(nvocales(c(5,2)), "La entrada debe ser tipo caracter.")
  expect_error(nvocales(""), "La entrada debe ser una palabra.")
  expect_error(nvocales(c("Hola","Que")), "La entrada debe ser una palabra.")
  expect_error(nvocales("2Hola"), "La entrada debe ser una palabra.")
  expect_error(nvocales("_Hola"), "La entrada debe ser una palabra.")
})

