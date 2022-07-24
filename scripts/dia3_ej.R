
## ***************************************************************************
##  Día 3: Manipulación de datos
##  Ejercicios             
##  Escuela de Invierno en Métodos
##  Martín Opertti - 2022
## ***************************************************************************


## 1. Importar dataframe "nba_data", usaremos solo datos de la temporada 2019,
# filtrar y explorar el dataframe de la temporada 19. Nombrar este dataframe "nba_19"


## 2. Usaremos únicamente las variables de fecha del partido, nombre y puntaje 
# de equipo local y visitante y  si ganó el equipos local. Seleccionar esas 
# variables y ordenar descendentemente según puntos del local. Nombrar este 
# dataframe "nba_19". 

# Usar pipeline! 
# (nombre de variables: GAME_DATE_EST, home_team, PTS_home, visitor_team, 
# PTS_away, HOME_TEAM_WINS)

#  Ahora con el dataframe generado en los puntos 1 y 2 calcularemos varias 
# tablas y valores


## 3. Calcular el promedio de puntos que anotó cada equipo jugando como LOCAL y
# filtrar por los 10 equipos con mayor promedio anotador (slice_max() puede ser útil)


## 4. Extraer la suma de puntos de los 10 partidos con mayor antoación (total, 
# suma de ambos equipos) en la temporada


## 5. Utilizando la lista de equipos debajo, filtrar  los partidos en los que
# estos equipos jugaron de local (El operador %in% puede serte útil). Luego, en
# el mismo pipeline  calcular el desvío estandar de los puntos anotados por 
# cada equipo 

equipos <- c("Boston Celtics", "Milwaukee Bucks",
             "New York Knicks", "Atlanta Hawks")


## 6. Calcular en una tabla el máximo y mínimo de puntos según equipo (jugando 
# como local) y según resultado. Es decir, la tabla debe tener cuatro valores 
# por equipo: máximo de puntos en partidos ganados, máximo de puntos en partidos
# perdidos, mínimo de puntos en partidos ganados y máximo de puntos en partidos
# ganados (siempre usando PTS_home y home_team). 


## 7. Ahora recrearemos la tabla de posiciones de la temporada 2019. 

## A. Para ello primero eliminemos los partidos de pretemporada y postemporada 
# (utilizando la fecha) y creemos una dummy que indique si el equipo visistante ganó
nba_data <- read_csv("data/nba_data.csv")
nba_19 <- nba_data %>% 
  filter(GAME_DATE_EST >= "2018-10-16" & GAME_DATE_EST <= "2019-04-10") %>% 
  mutate(AWAY_TEAM_WINS = case_when(PTS_away  > PTS_home ~ 1,
                                    TRUE ~ 0))
table(nba_19$HOME_TEAM_WINS, nba_19$AWAY_TEAM_WINS)  

## B. Ahora divide el dataframe "nba_19" en dos: uno con las variables home_team
# y HOME_TEAM_WINS y otro con las variables away_team y AWAY_TEAM_WINS y
# renombrar las variables de ambos dataframes para que los dos dataframes tengan
# las mismas  variables: una llamada "equipo"y otra "ganador".


## C. Unir los dos dataframes 

## D. Explorar la dimensión del dataframe unido. ¿Cuál es nuestra unidad de 
# análisis ahora?

# Nuestras observaciones pasaron de ser partidos a ser partidos-equipo. Cada 
# partido tiene dos filas, nuestra unidad pasa a ser el resultado de cada
# equipo en cada partido

## E. Crear una variable que tome el valor 1 si el equipo perdió y 0 si ganó
# (inversa a ganador) para luego resumir la suma de partidos ganados y perdidos
# por cada equipo (la tabla de posciones) y ordenar por partidos ganados

# Chequear el resultado aquí: https://www.espn.com/nba/standings/_/season/2020


## 8. Utilizando la tabla de posiciones generada en el punto 7 y el dataframe
# nba_teams, averiguar el equipo con más partidos ganados en 2019 sin nunca
# haber ganado un campeonato (ver variable CHAMPIONSHIP en nba_teams).

# left_join() te puede servir! (recuerda unificar categorías como en el 
# ejemplo en clase)



## 9. Por último crea una tabla de posiciones solamente para la conferencia este
#  (usando nba_teams) otro tipo de join puede ser útil...


