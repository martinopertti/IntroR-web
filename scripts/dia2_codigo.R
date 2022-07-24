
## ***************************************************************************
##  Día 2: Estadística descriptiva y limpieza de datos          
##  Código de la presentación                                      
##  Escuela de Invierno en Métodos                                
##  Martín Opertti - 2022                                         
## ***************************************************************************


##  1. Directorios de trabajo  ===============================================

getwd() # Con está función puedo consultar el directorio

# Podríamos usar setwd() para cambiarlo
# Ahora estamos trabajando en un proyecto de R (.Rproj), por lo que el 
# directorio por defecto debe ser donde está ubicado el .Rproj


# Como todo lo que queremos importar a R está dentro de la carpeta, 
# solo hay que usar directorios relativos:

library(readxl)

desempleo_uru <- read_excel("data/desempleo.xlsx")
head(desempleo_uru, 4) 
rm(desempleo_uru)



##  2. Dialectos  ============================================================    

# Creo un data.frame "encuesta"
encuesta <- data.frame(edad = c(18,24,80), 
                       ideologia = c("Izquierda", "Izquierda", "Derecha"),
                       voto = c("Partido A", "Partido A", "Partido C")
)
encuesta # Retomemos el data.frame "encuesta"

# Supongamos que quiero quedarme solo con las variables de edad y voto
encuesta_base <-  encuesta[ , c("edad", "voto")] # R Base

library(tidyverse)
encuesta_tidy <- select(encuesta, edad, voto) # Tidyverse

library(data.table)
encuesta_dt <- as.data.table(encuesta)[ , .(edad, voto)]

# Tres formas de lograr lo mismo:
colnames(encuesta_base)
colnames(encuesta_tidy)
colnames(encuesta_dt)



##  3. Importar datos a R   ==================================================

rm(list=ls())


## 3.1. Datos desde .csv ----

# Uno de los formatos más utilizados para almacenar datos son los archivos .csv
# En la carpeta data verán un .csv con datos de gapminder. 
# Tidyverse (mediante readr) nos permite importarlo con la función read_csv()
gapminder_csv <- read_csv("data/gapminder.csv")
head(gapminder_csv) # Usamos head para imprimir las primeras filas


## 3.2. Datos desde excel ----

# En la carpeta hay también un archivo llamado "gapminder" pero en formato excel
# Para importar datos de excel podemos utilizar el paquete readxl 
# Usamos la función read_excel()
gapminder_excel <- read_excel("data/gapminder.xlsx")
head(gapminder_excel)


## 3.3. Desde un paquete de R ----

# Algunos paquetes vienen con datos, por ejemplo, gapminder.
# En la documentación del paquete se encuentra el nombre de los datos 
# Con una simple asignación los podemos cargar 
library(gapminder)
d_gap <- gapminder


## 3.4. Datos desde SPSS y Stata ----
# Para estos tipos de datos usamos el paquete haven
library(haven)
gapminder_spss <- read_spss("data/gapminder.sav") # SPSS
class(d_gap$continent)
head(gapminder_spss)

gapminder_stata <- read_stata("data/gapminder.dta") # STATA
head(gapminder_stata)

# Para esto no necesitamos cargar paquetes. 
# Guardar un objeto como .rds:
saveRDS(object = d_gap, file = "resultados/d_gap.rds") # Creamos un archivo .rds

miobjeto_rds <- readRDS(file = "resultados/d_gap.rds") # Leemos un archivo .rds

# Con .rda se pueden guardar varios objetos al mismo tiempo!
save(d_gap, miobjeto_rds, 
     file = "resultados/dos_dataframes.Rdata") # Creamos un archivo .Rdata

rm(d_gap, miobjeto_rds) # Eliminamos el objeto del ambiente

load("resultados/dos_dataframes.Rdata") # Leemos un archivo .Rdata



##  4. Exportar datos =======================================================

rm(list = ls())

# Guardar .csv
d_gap <- gapminder
write_excel_csv(d_gap, "resultados/gapminder.csv")

# Guardar excel
library(writexl)
write_xlsx(d_gap, "resultados/gapminder.xlsx")

# Guardar .dta (Stata)
write_dta(d_gap, "resultados/gapminder.dta")

# Guardar .sav (SPSS)
write_sav(d_gap, "resultados/gapminder.sav")

# Guardar .sas (SAS)
write_sas(d_gap, "resultados/gapminder.sas")



##  5. Factores  ============================================================

# Podemos chequear y coercionar factores
is.factor(d_gap$continent) # Chequeo si es factor
levels(d_gap$continent) # Chequeo los niveles

# Transformo a caracter
d_gap$continent <- as.character(d_gap$continent)
class(d_gap$continent)

# De vuelta a factor
d_gap$continent <- as.factor(d_gap$continent) 
class(d_gap$continent)

# Para crear un factor usamos la función factor()
paises_mercosur <- factor(c("Argentina", "Brasil", "Paraguay", "Uruguay"))
table(paises_mercosur)

# La función fct_relevel() nos permite reordenar los niveles del factor
paises_mercosur <- fct_relevel(paises_mercosur, "Uruguay") 
table(paises_mercosur)
rm(list=ls())



##  6. Tibbles  =============================================================

d_gap <- gapminder
class(d_gap) # Ya es un tibble 

d_gap <- as.data.frame(d_gap)
class(d_gap) # Ahora solamente dataframe
print(d_gap)

d_gap <- as_tibble(d_gap) # Pasamos nuevamente a tibble
class(d_gap)
print(d_gap)



##  7. Explorar datos  ======================================================

# R tiene un visor para datos. Pueden clickear en el dataframe en el ambiente o:
View(d_gap)
dim(d_gap) # Número de filas y columnas
names(d_gap) # Nombre de variables
head(d_gap, 3) # Imprime primeras filas (3 en este caso)
str(d_gap) 
summary(d_gap) 
glimpse(d_gap) # Recomiendo utilizar esta función



# Para obtener una tabla de frecuencias de una variable usamos la función 
# table() de R Base
table(d_gap$continent) # Frecuencia simple

tabla_1 <- table(d_gap$continent) # Frecuencia simple

prop.table(tabla_1) # Proporciones

addmargins(tabla_1) # Totales

addmargins(prop.table(tabla_1)) # Proporciones y totales

## Tablas cruzadas

# Creo una variable Mercosur
d_gap$mercosur <- ifelse(d_gap$country == "Uruguay", 1,
                         ifelse(d_gap$country == "Argentina", 1,
                                ifelse(d_gap$country == "Paraguay", 1,
                                       ifelse(d_gap$country == "Brazil", 1,
                                              0))))

tabla_2 <- table(d_gap$continent, d_gap$mercosur)
tabla_2

# Editar nombre de columna
colnames(tabla_2) <- c("No mercosur", "Mercosur")
tabla_2

# Totales por fila o columna
prop.table(tabla_2)

prop.table(tabla_2, 1) # Total por fila

addmargins(prop.table(tabla_2, 1), 2) # Total por filas

prop.table(tabla_2, 2) # Total por filas

addmargins(prop.table(tabla_2, 2), 1) # Total por filas



## 8. Estadisticos descriptivos ============================================

# R cuenta también con funciones para obtener estadísticos descriptivos
mean(d_gap$lifeExp) # Media
median(d_gap$lifeExp) # Mediana
sd(d_gap$lifeExp) # Desvío estandar
range(d_gap$lifeExp) # Rango
max(d_gap$lifeExp) # Minimo
min(d_gap$lifeExp) # Maximo

# También podemos crear un histograma muy fácilmente
hist(d_gap$lifeExp,
     main = "Distribución de expectativa de vida (Gapminder)")

# Gráfico de dispersión (scatterplot)
plot(d_gap$lifeExp, d_gap$gdpPercap,
     main = "Relación entre expectativa de vida y PBI per cápita")

quantile(d_gap$lifeExp, probs=c(0.2, 0.4, 0.8)) # Cuantiles
quantile(d_gap$lifeExp, probs=seq(0, 1, 0.2)) # Cuantiles

# Con la función ntile() de dplyr podemos asignar quintiles en una variable
d_gap$lifeExp_quant <- ntile(d_gap$lifeExp, 5)

# Tabla cruzada 
table(d_gap$continent, d_gap$lifeExp_quant)



##  9. Crear y recodificar variables   ======================================

rm(list=ls())


## 9.1.  Crear variables con mutate() de dplyr ----

d_gap <- gapminder
head(d_gap, 3)

# Primero veamos cómo crear una constante
d_gap <- mutate(d_gap, var1 = "Valor fijo") # Variable de caracteres
d_gap <- mutate(d_gap, var2 = 7) # Variable numérica
head(d_gap, 3)

# También podemos crear ambas variables dentro de la misma línea 
d_gap <- gapminder
d_gap <- mutate(d_gap, 
                var1 = "Valor fijo",
                var2 = 7)
head(d_gap, 3)

# Podríamos haber hecho lo mismo con R Base
d_gap <- gapminder
d_gap$var1 <- "Valor fijo"
d_gap$var2 <- 7
head(d_gap, 3)

# Con mutate() también podemos hacer operaciones sobre variables ya existentes
# Calculemos el pbi total (pbi per capita * población)
d_gap <- mutate(gapminder, gdp = gdpPercap * pop)
head(d_gap, 3)

# Podemos calcular el logaritmo
d_gap <- mutate(d_gap, gdp_log = log(gdp))
head(d_gap, 3)


## 9.2. Atrasar y retrasar variables con lag() y lead() ----

# Primero nos quedamos con los datos de Uruguay
data_uru <- filter(gapminder, country == "Uruguay") 
data_uru <- mutate(data_uru, gdpPercap_lag = lag(gdpPercap, n=1))
head(data_uru, 3)

## Rankings e identificadores
d_gap <- mutate(gapminder, id = row_number()) # Identificador (números consecutivos)
head(d_gap, 3)
d_gap <- mutate(d_gap, gdp_rank = row_number(gdpPercap)) # Ranking según variable
d_gap <- arrange(d_gap, desc(gdp_rank)) # Ordeno los datos según el ranking
head(d_gap, 3)


## 9.3 Transformar tipo de datos de variables 

# Exploro tipo de variables
glimpse(d_gap)

# Variable continente a caracteres y año a factor
d_gap <- d_gap %>% 
  mutate(continent = as.character(continent),
         year = as.factor(year))

# Exploro tipo de variables
glimpse(d_gap)

# Variable año a numérica nuevamente
d_gap <- d_gap %>% 
  mutate(year = as.numeric(year))

# Exploro tipo de variables
glimpse(d_gap)



## 10. Recodificaciones condicionales =========================================

rm(list=ls())


## 10.1. Recodificar con recode() ----

## recode() con factores
# Argumento .default
d_gap <- gapminder
table(d_gap$continent)
class(d_gap$continent)
d_gap <- mutate(d_gap, continent_sigla = recode(continent, 
                                                "Africa" = "AF",
                                                "Americas" = "AM",
                                                .default = levels(continent)))
table(d_gap$continent_sigla)

# Especificar todos los valores
table(d_gap$continent)
d_gap <- mutate(d_gap, continent_sigla2 = recode(continent,
                                                 "Africa" = "AF",
                                                 "Americas" = "AM",
                                                 "Asia" = "AS",
                                                 "Europe" = "EU",
                                                 "Oceania" = "OC"))
table(d_gap$continent_sigla2)

## recode() con variables de caracteres
d_gap <- gapminder
d_gap$continent <- as.character(d_gap$continent) 
class(d_gap$continent)
d_gap <- mutate(d_gap, continent_sigla3 = recode(continent,
                                                 "Africa" = "AF",
                                                 "Oceania" = "OC"))
table(d_gap$continent_sigla3)


## 10.2. Recodificación con case_when() ----
d_gap <- gapminder

# Creemos una variable que indique si el país es Uruguay o no
d_gap <- mutate(d_gap, uruono = case_when(
  country == "Uruguay" ~ "Si",
  TRUE ~ "No")
)
table(d_gap$uruono)

## Con case_when() podemos establecer varias condiciones fácilmente
d_gap <- mutate(d_gap, mercosur = case_when(country == "Uruguay" ~ 1,
                                            country == "Argentina" ~ 1,
                                            country == "Paraguay" ~ 1,
                                            country == "Brazil" ~ 1,
                                            TRUE ~ 0))
table(d_gap$mercosur)

# También podemos usar operadores para simplificar
d_gap <- mutate(d_gap, mercosur_2 = case_when(
  country %in% c("Argentina", "Paraguay", "Brazil", "Uruguay") ~ 1,
  TRUE ~ 0)
) 

d_gap <- mutate(d_gap, mercosur_3 = case_when(
  country == "Argentina" | country == "Paraguay" | 
    country == "Brazil" | country == "Uruguay" ~ 1,
  TRUE ~ 0)
)

identical(d_gap$mercosur, d_gap$mercosur_2)
identical(d_gap$mercosur_2, d_gap$mercosur_3)

# También puedo crear variables en función a dos variables 
d_gap <- mutate(d_gap, var1 = case_when(gdpPercap > 20000 ~ 1,
                                        lifeExp > 75 ~ 1,
                                        TRUE ~ 0))
table(d_gap$var1)


## 10.3. Recodificación condicional con ifelse() ----

# Recodificacion con ifelse (una sola condición)
d_gap$poburu <- ifelse(d_gap$pop > 3000000, 1, 0)
table(d_gap$poburu)

# ifelse() anidado para varias condiciones
d_gap <- gapminder
d_gap$mercosur <-  ifelse(d_gap$country == "Uruguay", 1,
                          ifelse(d_gap$country == "Argentina", 1,
                                 ifelse(d_gap$country == "Paraguay", 1,
                                        ifelse(d_gap$country == "Brazil", 1, 
                                               0))))
table(d_gap$mercosur)



##  11. Datos perdidos   ====================================================


## 11.1. Datos perdidos y vectores ----
# Creo un vector que incluye datos perdidos
vector_n <- c(1, 2, 3, 4, NA, 5)
mean(vector_n) # No funciona si no especifico el argumento na.rm = TRUE
mean(vector_n, na.rm = TRUE) # Funciona

# Para chequear si cada observación es un dato perdido o no
is.na(vector_n)

# Operaciones con vectores y datos perdidos
vector1 <- c(1, 2, 3, 4)
vector2 <- c(1, 0, 1, NA)
vector_final <- vector1 / vector2
vector_final # Operaciones con valores NA arrojar resultados NA


## 11.2. Usos de is.na() ----

# Por ejemplo, usando any() podemos ver si hay al menos un valor perdido
any(is.na(vector2))

# Con which() podemos ver cuáles valores son perdidos
which(is.na(vector2))

# Con mean() podemos calcular el procentaje de datos perdidos
mean(is.na(vector2))

# Con sum() podemos calcular cuántos valores son perdidos
sum(is.na(vector2)) 

## NA cuando anexamos datos
encuesta <- data.frame(edad = c(18,24,80), 
                       ideologia = c("Izquierda", "Izquierda", "Derecha"),
                       voto = c("Partido A", "Partido A", "Partido C"))

encuesta_2 <- data.frame(edad = c(40, 44, NA), 
                         ideologia = c("Derecha", "Izquierda", "Derecha"),
                         voto = c("Partido B", "Partido A", "Partido C"),
                         genero = c("Mujer", "Hombre", "Mujer"))

encuesta_anexada <- plyr::rbind.fill(encuesta, encuesta_2)
print(encuesta_anexada)

# Con complete.cases vemos que filas están completas
complete.cases(encuesta_anexada)

# Con na.omit nos eliminamos las observaciones con datos perdidos
na.omit(encuesta_anexada)
