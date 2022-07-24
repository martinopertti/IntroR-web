
## ***************************************************************************
##   Pauta del examen     
##   Escuela de Invierno en Métodos - 2021    
##   Martín Opertti
## ***************************************************************************

## El examen consiste en explorar, anlizar y visualizar datos de una base de 
# datos con información sobre países. La data fue extraída del siguiente
# repositorio (https://www.kaggle.com/fernandol/countries-of-the-world) y 
# la pueden encontrar en la carpeta del examen: "countries_world.csv"

## 1. Importar la base y nombrarla "cw_df" y aplicar la función clean_names()
# del paquete Janitor para limpiar el nombre de las variables

library(tidyverse)

cw_df <- read_csv("data/countries_world.csv") %>% 
  janitor::clean_names()


## 2. Explorar el nombre y tipo de las variables, y calcular estadísticos
# descriptivos sobre el dataframe sobre las variables más interesantes

glimpse(cw_df)
