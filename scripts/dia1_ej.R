
## ***************************************************************************
##  Día 1: Introducción y fundamentos de la programación en R   
##  Ejercicios                                                     
##  Escuela de Invierno en Métodos                                 
##  Martín Opertti - 2022                                        
## ***************************************************************************


## 1. Crea un objeto con el año de tu nacimiento y suma tu edad

## 2. Borra el objeto

## 3. Crea un vector con el apellido de los últimos 5 presidentes de tu país

## 4. Crea un vector los números enteros que van del 0 al 35 y otro con los
# números de 0 a 100 queson múltiplos de 5

## 5. Averigua el tipo de los vectores y comprueba que sean los correctos y 
# luego borra todos los objetos del ambiente

## 6. Crea un dataframe con tus cuatro canciones favoritas en orden (primer
# fila canción favorita y así sucesivamente).
# Tres columnas: nombre, banda, año de publicación

## 7. Usando la indexación guarda en un objeto tu segunda canción favorita y
# las bandas de tus  cuatro canciones favoritas. Imprime ambos objetos

## 8. Ahora vamos a trabajar con el siguiente dataframe con datos del Banco 
# Mundial para 2019:
paises_eco <- data.frame(
  pais = c("Argentina", "Brasil", "Chile", "Uruguay", "Gibraltar"),
  continente = c("América Latina", "América Latina", "América Latina", 
                 "América Latina", "Europa"),
  desempleo = c(10.4, 12, 7.1, 8.8, NA),
  pbi_perc = c(9912, 8717, 14896, 16190, NA))

paises_eco

## 9. Cuál fue la tasa de desempleo en Chile en 2019? y el pbi per capita en 
# Uruguay?  (responde en código)

## 10. Cuál fue la media de desempleo para estos países en 2019? 
# (si hay datos faltantes excluir al país)

## 11. Redondea ese resultado a un dígito después de la coma

## 12. Usando el dataframe "paises_eco" prueba que:
# A. Argentina y Chile están en el mismo continente y Uruguay y Gibraltar en distintos
# B. Que el desempleo en Brasil es mayor que en Argentina
# C. Que al menos uno de los países tiene una tasa de desempleo de 8.8

## 13. Por último, instala el paquete "dplyr", cargalo y usa la función "filter" para filtrar 
# "paises_eco" de modo que   solo queden los países de América Latina. Utiliza help(filter) para 
# entender como usar la función.Cuidado que el paquete "stats" también tiene una función "filter",
# por lo que cuando uses help(filter), selecciona la función del paquete "dplyr". 
# También puedes buscar directamente en un navegador



