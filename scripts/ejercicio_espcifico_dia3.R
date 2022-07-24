
## ===========================================================================
## G) Ejercicio: ¿Qué conferencia ganó más partidos entre ellas?
## ===========================================================================

nba_full %>%
  filter(conference_home != conference_away) %>% # Filtro solo partidos de conferencias contrarias
  mutate(win = case_when(pts_home > pts_away ~ 1, # Creo variable del ganador local del partido
                         TRUE ~ 0)) %>% 
  group_by(conference_home) %>% # Agrupo por conferencia
  summarise(n = sum(win)) # Sumo partidos ganados (de local)



## 0. Utilizando la base de datos de gapminder crear una variable que 
# identifique si el nombre del país contiene la palabra republica esta
# puede aparecer tanto como "Republic" o como "Rep." Ningún país se llama solo
# "Republic" o "Rep.", queremos diferenciar países que contengan el término
# solamente 

