
## ***************************************************************************
##   Día 4: Visualización de datos y estadística inferencial
##   Solución de ejercicios             
##   Escuela de Invierno en Métodos    
##   Martín Opertti - 2021             
## ***************************************************************************

library(tidyverse)

## Data de encuestas de las elecciones 2016 en EEUU del paquete dslabs, 
# de FiveThirtyEight. Pueden consultar la documentación aquí: 
# https://www.rdocumentation.org/packages/dslabs/versions/0.7.3/topics/polls_us_election_2016

library(dslabs)
data <- polls_us_election_2016

# Explorar data
glimpse(data)

## 1. Exploremos los datos: crea un histograma rápido para ver la distribución
# de la intención de voto de Donald Trump rawpoll_trump

## 2. En el histograma anterior vemos que hay mucha variación. Filtremos la data
# para analizar solamente las encuestas nacionales ("U.S." en variable state) y
# volvamos a tirar el histograma

## De ahora en más usaremos solamente las encuestas nacionales

## 3. La variable grade indica la calidad de cada encuestadora según 538
## Crea un gráfico de barras con la cantidad de encuestas por grade
# Ver: (https://projects.fivethirtyeight.com/pollster-ratings/)

## 4. Ahora por claridad eliminemos las encuestas que no tienen calificación y 
# revertamos el orden de la variable grade (fct_rev() puede servirte)

## 4b. Filtremos una vez más, está ves para quedarnos con las encuestas con 
# grado A- o más

## 5. Ahora veamos la evolución de la intención de voto a Trump a lo largo del 
# tiempo. Crea el gráfico que consideres apropiado para ello, usando ggplot2. 
# Utiliza el dataframe filtrado en  el paso anterior. Usa la fecha del último
# día de la encuesta (enddate)

## 6. Agregemos los datos de intención de voto para Clinton. 
# Cuidado! la data no está en formato tidy para realizar esto, pivot_longe() 
# puede servirte

## No, no fue un buen año para las encuestadoras...

## 7. Supongamos que queremos incluir este gráfico en una publicación. Eligamos
# colores representativos (asociados con los partidos de los candidatos), 
# definamos titulos y fuente. 

## 8. Agregemos a nuestro gráfico un último elemento: el tamaño muestral

## 9. Parecería que en el último mes la diferencia entre los candidatos era menor. 
# Filtremos las encuestas  desde 1 de octubre de 2016 y creamos un boxplot para 
# ver las diferencias en la intención de voto a ambos candidatos

## 10. Por último, estimemos un modelo de regresión para ver si el tamaño
# muestral es relevante en la precisión de las encuestadoras. Para ello
# calculemos el valor absoluto de la diferencia entre  la intención de voto a
# Trump en cada encuesta y su % final que obtuvo en 2016 (46.1%)
data <- data %>% 
  mutate(margen = abs(46.1 - rawpoll_trump))
range(data$margen) # Diferencias entre 0.1 y 17.3

# También usamos como control los días que faltan para la elección (electin day 
# es el proxy)
data <- data %>% 
  mutate(dias_ele = as.numeric(as.Date("2016-11-08") - enddate))

## Con el paquete broom y la función tidy() podemos extraer los resultaods del 
# modelo en un dataframe en formato tidy!

