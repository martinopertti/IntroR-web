
## ***************************************************************************
##   Día 3: Manipulación de datos      
##   Solución de ejercicios             
##   Escuela de Invierno en Métodos    
##   Martín Opertti - 2022             
## ***************************************************************************

library(tidyverse)
library(gapminder)

rm(list = ls())

## 1. Importar dataframe "nba_data", usaremos solo datos de la temporada 2019,
# filtrar y explorar el dataframe de la temporada 19. Nombrar este dataframe
# "nba_19"
nba_data <- read_csv("data/nba_data.csv") %>% 
  janitor::clean_names()

nba_19 <- nba_data %>% 
  filter(season == 2019)

glimpse(nba_19)


## 2. Usaremos únicamente las variables de fecha del partido, nombre y puntaje 
# de equipo local y visitante y  si ganó el equipos local. Seleccionar esas 
# variables y ordenar descendentemente según puntos del local. Nombrar este 
# dataframe "nba_19". 
nba_19 <- nba_19 %>% 
  select(game_date_est,  home_team, pts_home, visitor_team, pts_away,
         home_team_wins) %>% 
  arrange(desc(pts_home))

nba_19

##  Ahora con el dataframe generado en los puntos 1 y 2 calcularemos varias 
# tablas y valores


## 3. Calcular el promedio de puntos que anotó cada equipo jugando como LOCAL y
# filtrar por los 10 equipos con mayor promedio anotador (slice_max() puede ser
# útil)
nba_19 %>% 
  group_by(home_team) %>% 
  summarize(media = mean(pts_home, na.rm = TRUE)) %>% 
  slice_max(n = 10, media)


## 4. Extraer la suma de puntos del partido con mayor anotación (total,  
# suma de ambos equipos) en la temporada
nba_19 %>% 
  mutate(pts_total = pts_home + pts_away) %>% 
  slice_max(n = 1, pts_total) %>% 
  summarize(suma = sum(pts_total))


## 5. Utilizando la lista de equipos debajo, filtrar  los partidos en los que
# estos equipos jugaron de local (El operador %in% puede serte útil). Luego, en
# el mismo pipeline calcular el desvío estandar de los puntos anotados por 
# cada equipo 
equipos <- c("Boston Celtics", "Milwaukee Bucks",
             "New York Knicks", "Atlanta Hawks")

nba_19 %>% 
  filter(home_team %in% equipos) %>% 
  group_by(home_team) %>% 
  summarize(desvio = sd(pts_home))


## 6. Calcular en una tabla el máximo y mínimo de puntos según equipo (jugando 
# como local) y según resultado. Es decir, la tabla debe tener cuatro valores 
# por equipo: máximo de puntos en partidos ganados, máximo de puntos en partidos
# perdidos, mínimo de puntos en partidos ganados y máximo de puntos en partidos
# ganados (siempre usando pts_home y home_team). 
nba_19 %>% 
  group_by(home_team, home_team_wins) %>% 
  summarize(max = max(pts_home),
            min = min(pts_home))


## 7. Ahora recrearemos la tabla de posiciones de la temporada 2019. 

## A. Para ello primero eliminemos los partidos de pretemporada y postemporada 
# (utilizando la fecha) y creemos una dummy que indique si el equipo visistante 
# ganó

nba_data <- read_csv("data/nba_data.csv") %>% 
  janitor::clean_names()

nba_19 <- nba_data %>% 
  filter(game_date_est >= "2018-10-16" & game_date_est <= "2019-04-10") %>% 
  mutate(away_team_wins = case_when(pts_away  > pts_home ~ 1,
                                    TRUE ~ 0))
table(nba_19$home_team_wins, nba_19$away_team_wins)  

## B. Ahora divide el dataframe "nba_19" en dos: uno con las variables home_team
# y HOME_TEAM_WINS y otro con las variables away_team y AWAY_TEAM_WINS y
# renombrar las variables de ambos dataframes para que los dos dataframes tengan
# las mismas  variables: una llamada "equipo"y otra "ganador".

nba_19_home <- nba_19 %>% 
  select(home_team, home_team_wins) %>% 
  rename(equipo = home_team,
         ganador = home_team_wins)

nba_19_home

nba_19_away <- nba_19 %>% 
  select(visitor_team, away_team_wins) %>% 
  rename(equipo = visitor_team,
         ganador = away_team_wins)

nba_19_away

## C. Unir los dos dataframes 
nba_pos_19 <- bind_rows(nba_19_home, nba_19_away)

## D. Explorar la dimensión del dataframe unido. ¿Cuál es nuestra unidad de 
# análisis ahora?
dim(nba_pos_19)

# Nuestras observaciones pasaron de ser partidos a ser partidos-equipo. Cada 
# partido tiene dos filas, nuestra unidad pasa a ser el resultado de cada equipo
# en cada partido

## E. Crear una variable que tome el valor 1 si el equipo perdió y 0 si ganó
# (inversa a ganador) para luego resumir la suma de partidos ganados y perdidos
# por cada equipo (la tabla de posciones) y ordenar por partidos ganados

nba_pos_19 <- nba_pos_19 %>% 
  mutate(perdedor = case_when(ganador == 1 ~ 0,
                              TRUE ~ 1)) %>% 
  group_by(equipo) %>% 
  summarize(ganados = sum(ganador),
            perdidos = sum(perdedor)) %>% 
  arrange(desc(ganados))

nba_pos_19

# Chequear el resultado aquí: https://www.espn.com/nba/standings/_/season/2020


## 8. Utilizando la tabla de posiciones generada en el punto 7 y el dataframe
# nba_teams, averiguar el equipo con más partidos ganados en 2019 sin nunca
# haber ganado un campeonato (ver variable CHAMPIONSHIP en nba_teams).

nba_teams <- read_csv("data/nba_teams.csv") %>% 
  janitor::clean_names()

nba_teams_ch <- nba_teams %>% 
  mutate(team = paste(city, nickname)) %>% 
  select(team, championships, conference)

nba_teams_ch

nba_pos_19 %>% 
  left_join(nba_teams_ch, by = c("equipo" = "team")) %>% 
  filter(championships == 0) %>% 
  slice_max(n = 1, ganados)


## 9. Por último crea una tabla de posiciones solamente para la conferencia este
# (usando nba_teams) otro tipo de join puede ser útil...

nba_teams <- read_csv("data/nba_teams.csv") %>% 
  janitor::clean_names()

nba_teams_con <- nba_teams %>% 
  mutate(team = paste(city, nickname)) %>% 
  select(team, conference)

nba_teams_con

nba_pos_19 %>% 
  left_join(nba_teams_con, by = c("equipo" = "team")) %>% 
  filter(conference == "east") %>% 
  select(-conference)


