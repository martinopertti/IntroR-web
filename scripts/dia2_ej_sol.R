
## ***************************************************************************
##  Día 2: Estadística descriptiva y limpieza de datos          
##  Solución de ejercicio                                          
##  Escuela de Invierno en Métodos                                 
##  Martín Opertti - 2022                                         
## ***************************************************************************

## Trabajaremos con un dataframe con datos de económicos y políticos de Uruguay. 
# La base se llama "datauru" y esta en formato excel (.xlsx)

## 1. Importar dataframe "datauru" y asignarle el mismo nombre "datauru" 
library(tidyverse)
library(readxl)

datauru <- read_excel("data/datauru.xlsx")

## 2. Imprime "datauru" y fijate las variables del dataframe y su tipo
# (puedes ayudarte con el codebook que está en la carpeta "data")
print(datauru)

## 3. Cuántas variables y observaciones tiene datauru?
dim(datauru)

## 4. Usa glimpse() para obtener un resumen de las variables
glimpse(datauru)

## 5. Crea una tabla para saber a qué partido político pertenece cada presidente
table(datauru$presidente, datauru$partido)

## 6. ¿Cuál es el rango de años para los cuales datauru tiene datos?
range(datauru$year)

## 7. ¿Cuál es el promedio de inflación y de aprobación presidencial en Uruguay 
# en los años que tenemos datos?
mean(datauru$inflation)
mean(datauru$aprobacion, na.rm = TRUE)

## 8. ¿Cuál es el porcentaje máximo de aprobación de presidente en un año en 
# Uruguay?
max(datauru$aprobacion, na.rm = TRUE)

## 9. Crea un histograma con la distribución del crecimiento anual de pbi per
# capita (gdp_pc_growth)
hist(datauru$gdp_pc_growth, 
     main = "Crecimiento anual del PBI per cápita en Uruguay")

## 10. ¿Cuál es la relación entre la inflación y el PBI en Uruguay? Un gráfico
# de dispersión puede ayudar
plot(datauru$gdp_lcu, datauru$inflation,
     main = "Relación entre inflación y PBI en Uruguay")

## 11. Crea una variable "gdp_pc" que calcule el PBI per capita (utilizando las
# variables gdp_lcu y population)
datauru <- mutate(datauru, gdp_pc = gdp_lcu / population)

## 12. Crea un gráfico de dispersión con gdp_lcu y la nueva variable gdp_pc
plot(datauru$gdp_lcu, datauru$gdp_pc,
     main = "Relación entre PBI y PBI per cápita en Uruguay")

## 13. Crea una nueva variable "gdp_log"con el logaritmo del PBI bruto y
# grafica un histograma de la nueva variable
datauru <- mutate(datauru, gdp_log = log(gdp_lcu))
hist(datauru$gdp_log)

## 14. Crea una variable binaria "inf_digit"que indique si la inflación tiene 
# dos dígitos o no

# Tres maneras disintas:
# con case_when()
datauru <- mutate(datauru, inf_digit = case_when(inflation >= 10 ~ 1,
                                                   TRUE ~ 0))

# ifelse()
datauru$inf_digit_1 <- ifelse(datauru$inflation >= 10, 1, 0)

# mutate() e ifelse()
datauru <- mutate(datauru, inf_digit_2 = ifelse(datauru$inflation >= 10, 1, 0))

identical(datauru$inf_digit, datauru$inf_digit_1)
identical(datauru$inf_digit, datauru$inf_digit_2)

## 15. Crear una nueva variable que indique los años con inflación y desempleo
# de un dígito
datauru <- datauru %>% 
  mutate(inf_unemp = case_when(inflation < 10 & unemployment < 10 ~ 1,
                               TRUE ~ 0))
table(datauru$inf_unemp)

## 16. ¿Tenemos datos perdidos en la variable de aprobación del presidente?
any(is.na(datauru$aprobacion))

## 17. ¿Cuáles son los valores perdidos en aprobación del presidente?
which(is.na(datauru$aprobacion)) # Los primeros 5

## 18. Crear un nuevo dataframe solo con las observaciones sin ningún NA
datauru_complete <- na.omit(datauru)





