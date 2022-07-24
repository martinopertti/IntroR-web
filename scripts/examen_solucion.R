
## ***************************************************************************
##   Examen de Introducción a la programación en R
##   Escuela de Invierno en Métodos - 2021    
##   Martín Opertti
## ***************************************************************************

## El examen consiste en explorar, anlizar y visualizar datos de una base de 
# datos con información sobre países. La data fue extraída y recortada del
# siguiente repositorio (https://www.kaggle.com/fernandol/countries-of-the-world)  
# y la pueden encontrar en la carpeta del examen: "countries_world.csv". 
# Utilizar el .csv situado en la Webasignatura y no desde el enlace.

# Los distintos puntos pueden tener diferentes soluciones. Pueden usar tanto
# las funciones utilizadas en el curso como otras no vistas, mientras se llegue
# al resultado deseado


## 1. Importar la base y nombrarla "cw_df" y aplicar la función clean_names()
# del paquete Janitor para limpiar el nombre de las variables

library(tidyverse)

cw_df <- read_csv("data/countries_world.csv") 

colnames(cw_df)

cw_df <-  janitor::clean_names(cw_df)

colnames(cw_df)


## 2. Explorar el nombre y tipo de las variables, chequeando si hay alguna 
# variable en formato incorrecto. En caso de que sí, transformar a formato 
# correcto (ej. character a numeric o vice-versa).

glimpse(cw_df)

cw_df <- cw_df %>% 
  mutate(literacy_percent = as.numeric(literacy_percent))

glimpse(cw_df)


## 3. Calcular estadísticos descriptivos de las variables que te parezcan más
# interesantes

mean(cw_df$population)

hist(cw_df$area_sq_mi)

table(cw_df$region)


## 4. Averiguar cuántos países no tienen datos en la variable literacy_percent,
# y luego eliminarlos del dataframe (sobreeescribir cw_df)

cw_df %>% 
  filter(is.na(literacy_percent)) %>% 
  pull(country)

cw_df <- cw_df %>% 
  drop_na(literacy_percent)

dim(cw_df)


## 5. Crear una nueva variable llamada "dens_pob" con la densidad poblacional
# de cada país (se calcula como población divido área)

cw_df <- mutate(cw_df, dens_pob = population / area_sq_mi)
cw_df


## 6. Crear una nueva variable "continente" con 5 categorías: Africa, Asia,
# Europa, America y Oceanía, a partir de los valores de la variable region
# Chequear con una tabla cruzada que la codificación sea correcta
# (incluir la región C.W. OF IND. STATES como parte de Asia)

table(cw_df$region)

cw_df <- cw_df %>% 
  mutate(continente = case_when(
    str_detect(region, "ASIA") ~ "Asia",
    str_detect(region, "EUROPE") ~ "Europa",
    str_detect(region, "AMER") ~ "America",
    str_detect(region, "AFRICA") ~ "Africa",
    region == "BALTICS" ~ "Europa",
    region == "C.W. OF IND. STATES" ~ "Asia",
    region == "NEAR EAST" ~ "Asia",
    region == "OCEANIA" ~ "Oceania"
  ))

table(cw_df$region, cw_df$continente)


## 7. Crear una nueva base llamada df_euro que contenga solamente países
# europeos y las variables country, region, continente, birthrate y deathrate

df_euro <- cw_df %>% 
  filter(continente == "Europa") %>% 
  select(country, region, continente, birthrate, deathrate)


## 8. Utilizando el dataframe df_euro crear una tabla que contenga la media de
# birthrate y deathrate según región. No tener en cuenta casos perdidos al 
# calcular las medias

df_euro %>% 
  group_by(region) %>% 
  summarize(mean_birth = mean(birthrate, na.rm = TRUE),
            mean_death = mean(deathrate, na.rm = TRUE))


## 9. Recodificar la variable "region" de forma tal que la categoría "BALTICS"
# se incluya dentro de "EASTERN EUROPE" y tirar la misma tabla que en el punto 
# anterior. Guardar como objeto llamado tabla_1

tabla_1 <- df_euro %>% 
  mutate(region = case_when(
    region == "BALTICS" ~ "EASTERN EUROPE",
    TRUE ~ region
  )) %>% 
  group_by(region) %>% 
  summarize(mean_birth = mean(birthrate, na.rm = TRUE),
            mean_death = mean(deathrate, na.rm = TRUE))
tabla_1


## 10. Transformar las variables region y continente de df_euro a factor

glimpse(df_euro)

df_euro <- df_euro %>% 
  mutate(continente = as.factor(continente),
         region = as.factor(region))

glimpse(df_euro)


## 11. Transformar la tabla_1 a formato largo

tabla_long <- tabla_1  %>% 
  pivot_longer(mean_birth:mean_death,
               names_to = "indicador",
               values_to = "valor")
tabla_long


## 12. Crear un gráfico de barras a partir de la tabla del punto 11 con los
# indicadores (media de birthrate y deathrate) en el eje de las x, el valor
# en el eje de las y, según región (denotada por el color de la barra, usar
# la paleta "Dark2" para seleccionar los colores del gráfico)

ggplot(tabla_long, 
       aes(x = indicador, y = valor, fill = region)) +
  geom_col(position = "dodge") +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2")


## 13. Por último, retomar el dataframe cw_df y crear una visualización
# que no sea de barras, de cualquier variable que te parezca interesante, 
# utilizando al menos un aesthetics además de la posición (ej, color, tamaño,
# línea según alguna variable relevante)

