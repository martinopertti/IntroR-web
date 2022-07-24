
## ***************************************************************************
##  Día 3: Manipulación de datos      
##  Código de la presentación         
##  Escuela de Invierno en Métodos    
##  Martín Opertti - 2022             
## ***************************************************************************


## 0. Cargar paquetes ========================================================

library(tidyverse)


## 1. Manipular datos  =======================================================

# Vamos a trabajar con una base de datos con estadísticas de partidos de la NBA.
# Cada fila es un partido y cada variable es un dato sobre ese partido.


## 1.1. Importar ----

# Exploremos la base:
nba_data <- read_csv("data/nba_data.csv") %>% 
  janitor::clean_names()

glimpse(nba_data)


## 1.2. Exploremos ----

# Analicemos la cantidad de puntos encestados por los equipos 

# pts_home indica cuántos puntos anotó el equipo local y pts_away los visitantes
mean(nba_data$pts_home) 

# Ya vemos que, como muchas de los datos que solemos trabajar, hay casos perdidos.
mean(nba_data$pts_home, na.rm = TRUE) 
mean(nba_data$pts_away, na.rm = TRUE) 

# Miremos la variación
sd(nba_data$pts_home, na.rm = TRUE) 
sd(nba_data$pts_away, na.rm = TRUE) 

# Un histograma nos puede dar una buena primera aproximación
hist(nba_data$pts_home,
     main = "Puntos anotados por partido por equipos de la NBA jugando de local")



##  2. Filtrar observaciones  ==============================================

## Una de las transformaciones más frecuentes cuando manipulamos datos 
# Tenemos datos de muchas temporadas:
table(nba_data$season)

# Filtremos para quedarnos con la temporada 2019 solamente
nba_data_19 <- filter(nba_data, season == 2019)

# Ahora con todas menos la 2020
nba_data_03_19 <- filter(nba_data, season != 2020)

# Ahora con las temporadas 2005, 2010, 2012 y 2017
temporadas <- c(2005, 2010, 2012, 2017)
nba_data_temp <- filter(nba_data, season %in% temporadas)
table(nba_data_temp$season)

# O podría hacer
nba_data_temp <- filter(nba_data, season %in% c(2005, 2010, 2012, 2017))
table(nba_data_temp$season)

# No tenemos datos de rebotes para algunos partidos...
filter(nba_data, is.na(reb_home)) 

# Extraer los casos con datos perdidos en la variable reb_home
data_incompleta <- filter(nba_data, is.na(reb_home)) 
glimpse(data_incompleta)

# Casos que tienen datos en reb_home
data_completa_reb <- filter(nba_data, !is.na(reb_home))
glimpse(data_completa_reb)

# Casos completos (elimino casos con NA en al menos una variable)
data_completa <- drop_na(nba_data)
dim(data_completa_reb)
  
rm(nba_data_19, nba_data_03_19, temporadas, nba_data_temp,
   data_completa_reb, data_completa, data_incompleta)



##  3. Seleccionar variables (columnas)  ====================================

colnames(nba_data)

# Seleccionar un conjunto de variables
nba_pts <- select(nba_data, pts_home, pts_away)
colnames(nba_pts)

# Seleccionar todas las variables menos las especificadas
nba_all <- select(nba_data, - pts_home)
colnames(nba_all)

# Seleccionar un rango de variables según orden
nba_seq <- select(nba_data, game_date_est:visitor_team_id)
colnames(nba_seq)

# También podemos usar "helpers"
# Por ejemplo, seleccionemos todas las variables que terminen en home
nba_home <- select(nba_data, ends_with("home"))
colnames(nba_home)

rm(nba_pts, nba_all, nba_seq, nba_home)



##  4. Pipelines  ===========================================================

# Supongamos que queremos un dataframe que solo incluya partidos de los Chicago 
# Bulls, sin datos perdidos, y que simplemente contenga la fecha, el nombre 
# y los puntos anotados de los dos equipos. 

data_bulls <- filter(nba_data,
                     home_team == "Chicago Bulls" | visitor_team == "Chicago Bulls")

data_bulls <- drop_na(data_bulls)

data_bulls <- select(data_bulls, 
                     game_date_est, home_team, visitor_team, pts_home, pts_away)

head(data_bulls)

# Podemos usar el pipeline
data_bulls_pip <- nba_data %>% 
  filter(home_team == "Chicago Bulls" | visitor_team == "Chicago Bulls") %>% 
  drop_na() %>% 
  select(game_date_est, home_team, visitor_team, pts_home, pts_away)

# Son iguales
identical(data_bulls, data_bulls_pip)

rm(data_bulls, data_bulls_pip)



##  5. Ordenar filas  =======================================================

# Usando el pipeline, seleccionemos algunas variables y luego ordenemos
nba_data %>% 
  filter(home_team == "Chicago Bulls" | visitor_team == "Chicago Bulls") %>% 
  select(game_date_est, home_team, visitor_team, pts_home, pts_away) %>% 
  arrange(pts_home)

nba_data %>% 
  filter(home_team == "Chicago Bulls" | visitor_team == "Chicago Bulls") %>% 
  select(game_date_est, home_team, visitor_team, pts_home, pts_away) %>% 
  arrange(desc(pts_home))

# También sirve para fechas o caracteres (alfabeticamente)
nba_data %>% 
  filter(home_team == "Chicago Bulls" | visitor_team == "Chicago Bulls") %>% 
  select(game_date_est, home_team, visitor_team, pts_home, pts_away) %>% 
  arrange(desc(game_date_est))

nba_data %>% 
  filter(home_team == "Chicago Bulls" | visitor_team == "Chicago Bulls") %>% 
  select(game_date_est, home_team, visitor_team, pts_home, pts_away) %>% 
  arrange(home_team)

# Podemos ordenar por más de una variable, en orden
nba_data %>% 
  filter(home_team == "Chicago Bulls" | visitor_team == "Chicago Bulls") %>% 
  select(game_date_est, home_team, visitor_team, pts_home, pts_away) %>% 
  arrange(home_team, desc(pts_home))



##  6. Resumir datos   ======================================================

## Resumen con la media de pts_home
nba_data %>% 
  drop_na() %>% 
  summarise(media = mean(pts_home))

## Con group_by() podemos crear grupos para nuestros resumenes
nba_data %>% 
  drop_na() %>% 
  group_by(season) %>% 
  summarise(media = mean(pts_home))

## Como con la mayoría de las funciones de dplyr, nos devuelve un dataframe, 
# que podemos guardar en un objeto
pts_per_season <- nba_data %>%
  drop_na() %>% 
  group_by(season) %>% 
  summarise(media = mean(pts_home))

## Con ungroup() dejamos de agrupar para las funciones que aún no han corrido,
# Con la lógica del pipeline podemos luego seguir transformado el resumen, 
# por ejemplo quedarnos con las temporadas posteriores a 2010
nba_data %>% 
  drop_na() %>% 
  group_by(season) %>% 
  summarise(media = mean(pts_home)) %>% 
  ungroup() %>% 
  filter(season > 2010)

## Podemos agrupar por dos o más variables si queremos también
nba_data %>% 
  drop_na() %>%
  filter(home_team == "Chicago Bulls" | 
           home_team == "New York Knicks") %>% 
  group_by(season, home_team) %>% 
  summarise(media = mean(pts_home)) 

# Podemos generar varios resumenes son summarise(), que son variables del 
# dataframe que devuelve
nba_data %>% 
  filter(season > 2015) %>%
  group_by(season) %>% 
  summarise(media_pts_home = mean(pts_home),
            suma_pts_home = sum(pts_home),
            max_pts_home = max(pts_home),
            partidos = n()) 

# Es muy flexible por ejemplo podemos hacer operaciones:
nba_data %>% 
  filter(season > 2015) %>%
  group_by(season) %>% 
  summarise(media_pts_home = mean(pts_home) + 100,
            suma_pts_home = sum(pts_home),
            max_pts_home = max(pts_home),
            partidos = n()) 

# Resumir usando across() todas las variables que terminen con cierto termino
nba_data %>%
  group_by(home_team) %>%
  summarise(across(ends_with("pct_home"), ~ mean(.x, na.rm = TRUE)))

# Usar across() con un vector de variables
nba_data %>%
  group_by(home_team) %>%
  summarise(across(c("pts_home", "ast_home"), ~ mean(.x, na.rm = TRUE)))

# Usar across() y where() para condiciones, por ejemplo variables numericas
nba_data %>%
  group_by(home_team) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

rm(pts_per_season)



##  7. Renombrar variables  =================================================

nba_data_2 <- nba_data %>% 
  rename(season = season,
         fecha = game_date_est)

colnames(nba_data_2)



##  8. Uniones (Merges)  ====================================================

## Unir dataframes por columnas

# Dividamos el dataframe en 2 para volver a unirlo
# mismas variables distintas observaciones
nba_data_18 <- nba_data %>% 
  filter(season == 2018)
table(nba_data_18$season)

nba_data_17 <- nba_data %>% 
  filter(season == 2017)
table(nba_data_17$season)

nba_data_17_18 <- bind_rows(nba_data_17, nba_data_18)
table(nba_data_17_18$season)

# Dividimos el dataframe en 2 pero con distinto número de columnas: 
# mismas variables distintas observaciones
nba_data_18 <- nba_data %>% 
  filter(season == 2018)

nba_data_17 <- nba_data %>% 
  filter(season == 2017) %>% 
  select(-pts_home)

nba_data_17_18 <- bind_rows(nba_data_17, nba_data_18)

rm(nba_data_17, nba_data_17_18, nba_data_18)


# Dividamos el dataframe en 2 para volver a unirlo  mismas observaciones 
# distintas columnas
nba_data_a <- nba_data %>% 
  select(game_date_est:fg3_pct_home)
glimpse(nba_data_a)

nba_data_b <- nba_data %>% 
  select(ast_home:home_team_wins)
glimpse(nba_data_b)

nba_data_C <- bind_cols(nba_data_a, nba_data_b)
glimpse(nba_data_C)



##  9. Uniones (joins)  ====================================================

# Los joins de dplyr nos sirven para unir datos con distintas estructuras
rm(list=ls())

# Problema: Supongamos que queremos averiguar los equipos de cuál conferencia 
# ganaron más partidos en los últimos 10 años. Con nba_data tenemos los datos 
# de los partidos y con nba_teams tenemos qué equipo pertenece a qué conferencia.

## A) Importamos el dataframe a nivel partido con el que veníamos trabajando
nba_data <- read_csv("data/nba_data.csv") %>% 
  janitor::clean_names()

# Importamos dataframe con data a nivel de equipo
nba_teams <- read_csv("data/nba_teams.csv") %>% 
  janitor::clean_names()

glimpse(nba_teams)


## B) Filtramos observaciones y variables relevantes de cada dataframe 

# Data a nivel partido de los últimos 10 años
nba_u10 <- nba_data %>% 
  filter(season > 2010) %>% 
  select(home_team, visitor_team, pts_home, pts_away)

glimpse(nba_u10)

# Data a nivel equipo (unificamos las categorías para que coincidan)
nba_teams_rec <- nba_teams %>% 
  mutate(team = paste(city, nickname)) %>% # Concateno ciudad y nombre
  select(team, conference)

glimpse(nba_teams_rec)


## C) Identifico variables "key" y chequeo que categorías coinidan 
table(nba_teams_rec$team)
table(nba_u10$home_team)


## D) Uno ambos dataframes usando left_join()
# Manera tradicional
nba_full_2 <- left_join(x = nba_u10,
                      y = nba_teams_rec,
                      by = c("home_team" = "team"))

# Con pipeline
nba_full <- nba_u10 %>% 
  left_join(nba_teams_rec, by = c("home_team" = "team"))

# Ambos caminos son iguales:
identical(nba_full, nba_full_2)

glimpse(nba_full)

rm(nba_full_2)


## E) Aplico left_join() para obtener la conferencia del equipo visitante
nba_full <- nba_full %>% 
  left_join(nba_teams_rec, by = c("visitor_team" = "team"))
glimpse(nba_full)


## F) Versión más prolija; con el arugmento suffix puedo definir el nombre de 
# las variables y pongo todo dentro de un pipeline
nba_full <- nba_u10 %>% 
  left_join(nba_teams_rec, by = c("home_team" = "team")) %>% 
  left_join(nba_teams_rec, by = c("visitor_team" = "team"),
            suffix = c("_home", "_away"))
glimpse(nba_full)


## Ahora veamos cómo funcionan todos los tipos de joins. Para ello crearemos dos
# dataframes  (incompletos) utilizando nba_teams: equipos del este; ID, nombre, 
# conferencia y año de fundación
nba_east <- nba_teams %>% 
  select(nickname, conference, yearfounded) %>% 
  filter(conference == "east")
print(nba_east)

# Algunos equipos (random, utilizando sample_n()), nombre conferencia y 
# campeonatos ganados
nba_random <- nba_teams %>% 
  select(nickname, championships) %>% 
  sample_n(size = 20)
print(nba_random)

# Utilizando distintos joins puedo unir estos datos de la manera que precise
# Mi dataframe original es nba_east y quiero usar nba_random para obtener más 
# información

# A) Incorporo datos de campeonatos ganados al dataframe nba_east, 
# en caso de estar diponibles
nba_east_2 <- nba_east %>% 
  left_join(nba_random, by = "nickname")
print(nba_east_2)

# B) Puedo llegar a lo mismo usando right_join si asigno de forma distinta a x e y
nba_east_3 <- nba_random %>% 
  right_join(nba_east, by = "nickname")
print(nba_east_3)

# C) Con inner_join() me quedo solo con los equipos presentes en ambos dataframes
nba_east_4 <- nba_east %>% 
  inner_join(nba_random, by = "nickname")
print(nba_east_4)

# D) Con full_join() me quedo con todos los equipos que esten al menos en x o y
nba_total_1 <- nba_east %>% 
  full_join(nba_random, by = "nickname")
print(nba_total_1)



##  10. Estructura de datos  ================================================

rm(list=ls())

## El formato tidy tiene tres reglas: cada fila es una observación, cada y
# columna una variable cada celda es un valor

## Este es un formato típico en el que podemos encontrar datos
wb_desempleo <- data.frame(pais = c("Argentina", "Chile", "Uruguay"),
                           d_2019 = c(9.8, 7.3, 9.3),
                           d_2020 = c(11.7, 11.5, 12.7))
print(wb_desempleo)

## Cambio de estructura (de ancho a largo en este caso) para transformar a 
# formato tidy
wb_unemp_tidy <- wb_desempleo %>% 
  pivot_longer(cols = c("d_2019", "d_2020"), # Columnas a unir 
               names_to = "year", # Nombre de variable "key" 
               values_to = "desempleo") # Nombre de variable con valores
print(wb_unemp_tidy)

## Otro caso es donde las observaciones están en más de una fila.
wb_unemp <- data.frame(
  pais = c("Argentina", "Argentina", "Argentina", "Argentina", "Argentina"),
  year = c(2018, 2019, 2020, 2018, 2019),
  variable = c("desempleo", "desempleo", "desempleo", "pbi_per_capita", "pbi_per_capita"),
  valor = c(9.2, 9.8, 11.7, 11633, 9912))
print(wb_unemp)

# En este caso, para llegar a formato tidy necesitamos pasar de formato largo a ancho
wb_unemp_tidy <- wb_unemp %>% 
  pivot_wider(names_from = variable,
              values_from = valor)
print(wb_unemp_tidy)


