## Código original para crear base "partidos"
## Descargar desde acá: https://www.kaggle.com/nathanlauga/nba-games?select=teams.csv

library(tidyverse)
nba_game <- read_csv("data_original_nba/games.csv")
nba_player <- read_csv("data_original_nba/games_details.csv")
nba_teams <- read_csv("data_original_nba/teams.csv")
nba_teams <- nba_teams %>% 
  mutate(name = paste(CITY, NICKNAME)) %>% 
  select(TEAM_ID, name)

# Nombre de equipo local
nba_base <- nba_game %>% 
  left_join(nba_teams, by = c("HOME_TEAM_ID" = "TEAM_ID")) %>% 
  mutate(home_team = name) %>% 
  select(-name)

# Nombre de equipo visitante
nba_base <- nba_base %>% 
  left_join(nba_teams, by = c("VISITOR_TEAM_ID" = "TEAM_ID")) %>% 
  mutate(visitor_team = name) %>% 
  select(-name)

write_csv(nba_base, "data/nba_data.csv")
