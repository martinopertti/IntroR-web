
## ***************************************************************************
##  Día 1: Introducción y fundamentos de la programación en R   
##  Código de la presentación                                      
##  Escuela de Invierno en Métodos                                 
##  Martín Opertti - 2022                                         
## ***************************************************************************

## En este script vamos a repasar algunas funciones y operadores básicos de R


##  1. Comandos básicos   ====================================================

# Creamos un objeto llamado "year" con este año en forma numérica
year <- 2022

# Lo podemos imprimir simplemente escribiendo su nombre.
year

# Otra forma:
(year <- 2022)

# También se puede imprimir de esta forma:
print(year)

# Podemos chequear si el objeto se creó correctamente mediante la lista del 
# environment
ls()

# Ahora, que podemos hacer con este objeto? Lo podemos usar en operaciones 
year - 1996



## 2. Objetos   =============================================================

## * 2.1. Crear objetos ----

# Usando la misma lógica, podemos almacenar el resultado
edad <- year - 1996
print(edad)

# Ahora podemos sumar estos objetos
year + edad

# Para obtener ayuda sobre una función usamos:
help(ls)

# Para eliminar el objeto del ambiente:
rm(year)

# Para eliminar todos los objetos del ambiente:
rm(list=ls())
print(year) # No encuentra el objeto porque lo borró


## * 2.2. Tipos de objetos ----

# Podemos volver a crear el objeto anterior, ahora de forma directa:
year <- 2022

# Creamos algunos objetos distintos
nombre <- "Dos mil veintidos"

# Uso la función typeof() para averiguar el tipo
typeof(year) 
typeof(nombre)

# Uso la función class() para averiguar la clase
class(year) # es un vector de tipo numérico
class(nombre) # es un vector de tipo character

year_2 <- "2022"
class(year_2)

vof <- TRUE
class(vof) 

## Por qué importa el tipo de objeto?
# Character
obj_1 <- "10"
class(obj_1)
obj_1 + 20 # Da error

# Numeric
obj_2 <- 10
class(obj_2)
obj_2 + 20 # Funciona

# Numeric to character
obj_1 <- as.numeric(obj_1)
class(obj_1)
is.numeric(obj_1) # Podemos verificarlo directamente también
obj_1 + 20 # Funciona



##  3. Vectores   ===========================================================

## * 3.1.  Crear un vector con c() ----
mi_primer_vector <- c(1, 2, 3, 4, 5) 
print(mi_primer_vector)
class(mi_primer_vector)
length(mi_primer_vector)
str(mi_primer_vector)

# Más vectores:
(v1 <- c(1:5)) # Todos los números de 1 a 5

(v2 <- seq(0, 50, 10)) # De 0 a 50 de a 10 números

(v3 <- c(v1, v2)) # Combino vectores creando un nuevo vector

(v4 <- c("rojo", "verde", "blanco")) # character

(v5 <- c(TRUE, TRUE, FALSE, TRUE)) # lógico


## * 3.2. Coerción automática: ----
(v6 <- c(v2, v4)) # Unir vector numérico con uno de caracteres
class(v6)


## * 3.3.  Indexación: ----
v2
v2[1] # El primer elemento dentro del vector 

# Nos sirve por ejemplo para extraer partes del vector:
(v3 <- v2[1:3]) # Creo nuevo vector con los elementos del 1 al 3



##  4. Dataframes  ==========================================================

rm(list=ls()) # Limpiamos el ambiente

## * 4.1. Crear un dataframe:

# Usamos la función data.frame
encuesta <- data.frame(
  edad = c(18, 24, 80, 40, 76),
  ideologia = c("Izquierda", "Izquierda", "Derecha", "Centro", "Derecha"),
  voto = c("Partido A", "Partido A", "Partido C", "Partido B", "Partido B")
  )

print(encuesta)
View(encuesta)
class(encuesta)

# Con la función head puedo ver las primeras filas del dataframe, en este caso 3
head(encuesta, 3) 

## Indexación dataframes
encuesta[1,1] # Valor de fila 1 y columna 1
encuesta[, 1] # Valor de toda la columna 1 (no especificamos filas, devuelve todas)
encuesta[1, ] # Valor de toda la fila 1 (no especificamos columnas, devuelve todas)
encuesta[1, c(1, 2)] # Valor de fila 1 y las columnas 1 y 2
encuesta[1, -3] # Valor de fila 1 y las columnas 1 y 2

# Copiar objeto
encuesta_2 <- encuesta
identical(y = encuesta_2, x = encuesta) # Son iguales



##  5. Funciones  ===========================================================

rm(list=ls()) # Limpiamos el ambiente


## * 5.1. Ejemplo del uso de una función: mean() ----

# Supongamos que queremos calcular la media de: 12,24,36,48,60 
(12 + 24 + 36 + 48 + 60)/5 # Calculo directamente la media
data_ej <- c(12, 24, 36, 48, 60) # Genero el vector con los 5 números 
sum(data_ej) / length(data_ej) # Calculo con dos funciones su media
mean(data_ej) # Calculo la media directamente con la función mean()

# También se puede ingresar data directamente en el argumento x
mean(c(12, 24, 36, 48, 60)) 

## Argumentos de funciones
media_fun <- mean(data_ej) # Sin explicitar argumento x
media_fun_x <- mean(x = data_ej) # Explicitando argumento x
identical(media_fun, media_fun_x) # El mismo resultado

# Dataframe con el resultado de Uruguuay en los últimos 5 mundiales
uru_mundial <- data.frame(year = c(2002, 2006, 2010, 2014, 2018),
                          posicion = c(26, NA, 4, 12, 5))

# Veamos la posición promedio:
mean(uru_mundial$posicion) # Como tenemos un dato perdido, la función nos devuelve NA

# Si especificamos el argumento na.rm (no tener en cuenta los datos perdidos):
mean(uru_mundial$posicion, na.rm = TRUE)


## * 5.2. Documentación de una función ----
help(mean)
?mean


## * 5.3. Funciones y códigos ----

rm(list=ls()) # Limpiamos el ambiente

data_ej <- c(12, 21, 33, 41, 27, 23) 
prueba <- mean(data_ej)

# Correr funciones al mismo objeto
resultado_A <- mean(data_ej) # Primero estimo la media
resultado_A <- round(resultado_A, digit=1) # Redondeo

# Correr funciones utilizando objetos intermedios
resultado_B_1 <- mean(data_ej) # Creo un primer objeto con la media
resultado_B_2 <- round(resultado_B_1, digits=1) # Creo otro objeto con la media redondeada

# Corro las dos funciones en la misma línea
resultado_C <- round(mean(data_ej), digits=1)

# Pruebo que los resultados sean iguales:
identical(resultado_A, resultado_B_2)
identical(resultado_A, resultado_C)


## * 5.4. Crear una función ---- 

# Supongamos que tenemos un dataframe con tres variables: pais, vacas y personas 
data <- data.frame(pais = c("Uruguay", "Argentina", "Brasil", "Mexico"),
                   humanos = c(3.4, 43.8, 209.5, 128.6),
                   vacas = c(11800, 53500, 22600, 16500))
data

# Ahora quiero calcular la cantidad de vacas per capita. Podría hacer:
data$vacas_pc <- (data$vacas / 1000) / data$humanos 
data

# Ahora me gustaría tener una tabla un poco más prolija: números redondeados y "per"
data$vacas_pc <- round(data$vacas_pc, digits = 1)
data$vacas_pc <- paste(data$vacas_pc, "per", sep = " ")
data

# Ok, lo logramos. Pero necesitar calcular más tablas:
data_2 <- data.frame(pais = c("Uruguay", "Nueva Zelanda", "Australia", "Japón"),
                     humanos = c(3.4, 4.5, 43.8, 126.3),
                     vacas = c(11800, 9900, 53500, 3800))
data_2

# Tendría que copiar y pegar varias veces el mismo código, cambiando el nombre 
# de los objetos. En este tipo de casos es muy util crear nuestra propia 
# función, para resumir este conjunto  de operaciones:

calc_vacas <- function(x, y){ 
  vacas_pc <- (x / 1000) / y   # Calculo la proporción de x / 1000 sobre y
  vacas_pc_1 <- round(vacas_pc, digits = 2) # Redondeo
  vacas_pc_2 <- paste(vacas_pc_1, "per", sep = " ")
  return(vacas_pc_2)
}

data_2$vacas_pc <- calc_vacas(x = data_2$vacas, y = data_2$humanos)
data_2


## * 5.5. Errores ---- 
vector_ej <- rnorm(n = 10, mean = 10, sd = 5) # Creo valores aleatorios
mean(Vector_ej) # Aplico función para obtener la media

# No funciona porque el nombre del objeto está mal escrito
mean(vector_ej) # Aplico función para obtener la media

## * 5.6. Advertencias ----
vector_1 <- c("10", "35%", "35", "50") # Vector de caracteres que contiene números 
vector_1

vector_2 <- as.numeric(vector_1) # Transformo a vector númerico
vector_2 # Los valores que además del número tenían (%) no pueden pasarse a númericos

vector_1 <- gsub("%", "", vector_1) # Quito los % del vector original y evito la advertencia
vector_1

vector_2 <- as.numeric(vector_1) # Transformo a vector númerico
vector_2 # Los valores que además del número tenían (%) no pueden pasarse a númericos



##  6. Paquetes  =============================================================

rm(list=ls()) # Limpiamos el ambiente

## * 6.1. Descargar y cargar ----

# Para descargar paquetes de CRAN, utilizamos la siguiente función:
install.packages("dplyr") # Ejemplo, correr solo si es necesario

# Existen otros paquetes no alojados en CRAN, que se instalan utilizando el paquete "devtools" 
library(dplyr) # Ejemplo

## * 6.2. Utilzar función sin cargar paquete ----
# Creo dataframes con columnas no iguales
df_1 <- data.frame(col_a = c(1, 2, 3),
                   col_b = c(2, 4, 6))

df_2 <- data.frame(col_a = c(1, 2, 3),
                   col_c = c(-2, 1, 6))

# Uso la función rbind.fill() que a diferencia de rbind() del Base, permite mergear
# dataframes con columnas que no matchean de forma exacta, agregando NAs
# No cargo todo el paquete plyr sino que llamo la función específica usando ::
df_3 <- rbind(df_1, df_2) # No me sirve rbind() del Base porque las columnas no son iguales
df_3 <- plyr::rbind.fill(df_1, df_2)



##  7. Operadores  ===========================================================

rm(list=ls()) # Limpiamos el ambiente

## * 7.1. Operadores aritméticos ----

# Ya vimos algunos al principio:
5 + 5 # Suma
5 - 5 # Resta
5 * 5 # Multiplicación
5 / 5 # División
5 ^ 5 # Potencia
log(100) # Logaritmo natural
log10(100) # Logaritmo en base 10

## * 7.2. Operadores relacionales ----

# Podemos testear expresiones y R nos devuelve si son verdaderas o falsas
6 < 10 # menor que
10 <= 10 # menor o igual que
6 > 10 # mayor que
6 >= 10 # mayor o igual que

# También con objetos
vnum_1 <- c(2, 4, 6, 8, 10)
vnum_2 <- c(2, 3, 6, 10, 10)

# Testear si son iguales para cada valor, por orden (tienen que tener el mismo largo)
vnum_1 == vnum_2 
vnum_1[4] == vnum_2[5] # Valores específicos con indexación
vnum_1[4] != vnum_2[5] # Testea si es distinto a...  

# Si queremos ver si un vector contiene un valor:
7 %in% vnum_1 # vnum_1 no contiene el valor 7
10 %in% vnum_2 # vnum_1 sí contiene el valor 10
vnum_1 %in% vnum_2 # Para cada valor

# Con caracteres
vcar_1 <- c("Natalia", "Nicolás", "Marcelo", "Florencia")
vcar_2 <- c("Natalia", "Nicolás", "Marcelo", "Florencia", "Romina", "Román")
vcar_1[1] == vcar_2[1] # El primer valor es igual
vcar_1 %in% vcar_2

## * 7.3. Operadores booleanos ----

vnum_3 <- c(2, 4, 6, 10, 12, 16)
6 & 7 %in% vnum_3 # Pruebo si 6 Y 7 están en vnum_3 (tienen que estar ambos para que sea TRUE)
6 | 7 %in% vnum_3 # Pruebo si al menos uno de 6 o 7 están en vnum_3 (con uno alcanza)
isTRUE(6 | 7 %in% vnum_3) # Otra forma de testear lo mismo

