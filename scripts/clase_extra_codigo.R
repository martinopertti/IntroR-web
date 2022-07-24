
## ***************************************************************************
##  Clase extra: Datos de encuesta y loops      
##  Código de la presentación         
##  Escuela de Invierno en Métodos    
##  Martín Opertti - 2021             
## ***************************************************************************


## 0. Cargar paquetes ========================================================

library(tidyverse)


## 1. Encuestas =============================================================
library(haven)

## 1.1. Importar datos ----
lat_17 <- read_spss("data/Lat_17.sav") 

# Vemos etiqueta pero el valor real es el código
lat_17

# Por ejemplo, en vista de datos ya vemos solo el código
View(lat_17)


## 1.2. De etiquetas a valores  ----

# Revisemos que todas las variables tengan el formato que queremos...
glimpse(lat_17)

# Pasemos voto país, educación y sexo a factor 
lat_17 <- lat_17 %>% 
  mutate(idenpa = as_factor(idenpa),
         P16STGBS = as_factor(P16STGBS),
         sexo = as_factor(sexo),
         REEDUC_1  = as_factor(REEDUC_1 ))

glimpse(lat_17)

## 1.3. Recodifico valores ----

# Exploremos la variable voto
table(lat_17$P16STGBS) 

# Demasiados factores, pasemos a character
table(as.character(lat_17$P16STGBS))

# Renombremos la variable de intención de voto, transformemosla en caracteres 
# para quitarle los niveles vacíos y creemos una variable resumen con PRO/UCR, 
# FPV y otros
lat_17 <- lat_17 %>% 
  rename(int_voto = P16STGBS) %>% 
  mutate(int_voto = as.character(int_voto)) %>% 
  mutate(int_voto_res = case_when(
    str_detect(int_voto, "FPV") ~ "FPV",
    str_detect(int_voto, "PRO") ~ "Cambiemos",
    str_detect(int_voto, "UCR") ~ "Cambiemos",
    TRUE ~ "Otros"
  ))

# Chequeo que haya quedado bien
tabla_1 <- table(lat_17$int_voto_res)  # Frecuencia
tabla_1

prop.table(tabla_1) # Porcentaje

table(lat_17$int_voto, lat_17$int_voto_res)


## 1.4. Resumenes de datos ponderados  ----

# Intención de voto resumida (sin ponderar)
res_1 <- lat_17 %>% 
  group_by(int_voto_res) %>% 
  summarise(frec = n())
res_1

# Intención de voto resumida con ponderación
res_wtd <- lat_17 %>% 
  group_by(int_voto_res) %>% # Variables de agrupar
  summarise(frec_pond = sum(wt)) # Resumir frecuencias (ponderadas)
res_wtd

# Con proporción
lat_17 %>% 
  group_by(int_voto_res) %>% # Variables de agrupar
  summarise(valor_ponderado = sum(wt)) %>%  # Resumir frecuencias (ponderadas)
  mutate(per = valor_ponderado / sum(valor_ponderado)) # Agregar columna de porcentaje

# Con porcentajes
lat_17 %>% 
  group_by(int_voto_res) %>% 
  summarise(frec_pond = sum(wt)) %>%  # Resumir frecuencias (ponderadas)
  mutate(per = frec_pond / sum(frec_pond) * 100) 

# Con porcentajes (y simbolo)
lat_17 %>% 
  group_by(int_voto_res) %>% 
  summarise(frec_pond = sum(wt)) %>%  # Resumir frecuencias (ponderadas)
  mutate(per = paste0(frec_pond / sum(frec_pond) * 100, "%")) 

# Dos variables porcentaje por factor: sexo según voto
s_vot <- lat_17 %>% 
  group_by(int_voto_res, sexo) %>% 
  summarise(frec_pond = sum(wt)) %>%  
  mutate(per = frec_pond / sum(frec_pond)) 
s_vot

# Podemos graficar de forma sencilla
ggplot(s_vot,
       aes(x = int_voto_res, y = per, fill = sexo)) +
  geom_col(position = "dodge") +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2")
ggsave("resultados/plots/svy1.png", width = 30, height = 20, units = "cm")


# Lo mismo pero en formato ancho (seleccionando una variable)
resumen <- lat_17 %>% 
  group_by(int_voto_res, sexo) %>% 
  summarise(frec_pond = sum(wt)) %>%  
  mutate(per = frec_pond / sum(frec_pond)) %>%
  select(- frec_pond) %>% 
  pivot_wider(names_from = "sexo",
              values_from = "per") %>% 
  mutate(Male = round(Male * 100, digits = 0),
         Female = round(Female * 100, digits = 0),
         Total = Male + Female)
resumen

# Exporto a un excel
writexl::write_xlsx(resumen, "resultados/tabla_prolija.xlsx")

# Invertir: voto según sexo
vot_s <- lat_17 %>% 
  group_by(sexo, int_voto_res) %>% 
  summarise(frec_pond = sum(wt)) %>%  
  mutate(per = frec_pond / sum(frec_pond)) 
vot_s

# Podemos graficar de forma sencilla
ggplot(vot_s, 
       aes(x = sexo, y = per, fill = int_voto_res)) +
  geom_col() +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2")

# Cruce sexo x voto % sobre el total
vs_tot <- lat_17 %>% 
  group_by(int_voto_res, sexo) %>% 
  summarise(frec_pond = sum(wt)) %>%  
  ungroup() %>% 
  mutate(per = frec_pond / sum(frec_pond)) 
vs_tot

# Edito para graficar categorias cruzadas
vs_tot <- vs_tot %>% 
  mutate(categoria = paste(int_voto_res, sexo)) %>% 
  select(categoria, per)
vs_tot

ggplot(vs_tot, aes(x = categoria, y = per)) +
  geom_col() +
  theme_bw() 

ggplot(vs_tot, 
       aes(x = fct_reorder(categoria, -per), y = per)) +
  geom_col(color = "black", fill = adjustcolor("darkgreen", alpha.f = .6)) +
  theme_bw() 


## 1.5. Ponderación (raking) ----

# Con anesrake podemos ponderar con el método de raking

# install.packages("anesrake")
library(anesrake)

# Supongamos que quiero ponderar por sexo y el parámetro es 60% mujeres y 
# 40% hombres 

prop.table(table(lat_17$sexo)) # Hay diferencia importante

# Creo vector con parametro y nombro los valores para que coincidan con la var
par_sexo <- c(0.6, 0.4) # Es un ejemplo, no son reales
names(par_sexo) <- c("Female", "Male")
par_sexo

# También quiero recodificar por edad en las siguientes categorías:
lat_17 <- lat_17 %>% 
  mutate(edad_rec = case_when(
    edad <= 39 ~ "18 a 39",
    edad >= 40 & edad <=59 ~ "40 a 59",
    edad >= 60 ~ "60 o mas"
  ))

prop.table(table(lat_17$edad_rec))

# Creo vector con parámetro también:
par_edad <- c(0.3, 0.4, 0.3)
names(par_edad) <- c("18 a 39", "40 a 59", "60 o mas")

# Luego creo un id para cada encuestado
lat_17 <- mutate(lat_17, id = row_number())

# Chequeo que mis variables sean factores
is.factor(lat_17$sexo)
is.factor(lat_17$edad_rec) 
class(lat_17$edad_rec)

# Transformo edad a factor
lat_17 <- mutate(lat_17, edad_rec = as.factor(edad_rec))
is.factor(lat_17$edad_rec) 

# IMPORTANTE: La data tiene que estar en formato dataframe no tibble! 
class(lat_17)
lat_17 <- as.data.frame(lat_17)
class(lat_17)

# Ahora ya estamos prontos para ponderar
parametros <- list(par_sexo, par_edad) # Lista con todos los parametros
names(parametros) <- c("sexo", "edad_rec") # Nombrar con el nombre exacto de las variables

## Ponderador 
outsave <- anesrake(
  parametros, # Lista con parámetros 
  lat_17, # Data 
  caseid = lat_17$id, # Id único por caso
  cap = 5, # Fijo peso máximo de 5
  type = "nolim",
  pctlim = 5, # Si la diferencia no es > a 5% no tenga en cuenta la var
  force1 = TRUE # Asegura que cada parametro sume 1 si hay pequeñas diferencias
  )

summary(outsave) # Resumen

## Ponderador como variable
lat_17 <- mutate(lat_17, ponderador = unlist(outsave[1]))

lat_17

# Chequeo que haya funcionado
lat_17 %>% 
  group_by(sexo) %>% 
  summarise(sexo = sum(ponderador)) %>% 
  mutate(sexo = sexo / sum(sexo))
            
lat_17 %>% 
  group_by(edad_rec) %>% 
  summarise(edad_rec = sum(ponderador)) %>% 
  mutate(edad_rec = edad_rec / sum(edad_rec))

## Comparar intención de voto según ponderación:
lat_17 %>% 
  group_by(int_voto_res) %>% 
  summarise(cruda = n(),
            wt = sum(wt),
            pond = sum(ponderador)) %>% 
  mutate(cruda = cruda / sum(cruda),
         wt = wt / sum(wt),
         pond = pond / sum(pond))  


##  2 Loops  ==============================================================
rm(list=ls())

## 2.1. Loops tradicionales ----

# Un loop sencillo: 
numeros_primos <- c(2, 3, 5, 7, 11, 13, 17, 19)

for (i in seq_along(numeros_primos)){
  
  print(paste(numeros_primos[i], "es un numero primo"))
  
}

# Para guardar todos los elementos
numeros_primos <- c(2, 3, 5, 7, 11, 13, 17, 19)

# Creo objeto vacío
obj_vacio <- vector("character", # definimos el tipo de los datos  
                    length(numeros_primos))  # definimos la extensión

for (i in seq_along(numeros_primos)){
  
  obj_vacio[i] <- paste(numeros_primos[i], "es un numero primo")
  
}

obj_vacio

# Para guardar cada elemento
numeros_primos <- c(2, 3, 5, 7, 11, 13, 17, 19)

# Creo un vector con el nombre de cada objeto
nom_np <- paste(numeros_primos, "obj", sep = "_")

for (i in seq_along(numeros_primos)){
  
  assign(nom_np[i], 
         paste(numeros_primos[i], "es un numero primo")
  )
}

# Abrir archivos dentro de una carpeta
library(gapminder)
d_gap <- (gapminder)
table(gapminder$continent)

# Divido el dataframe de gapminder según continente
lista_df <- d_gap %>%
  group_split(continent, named = TRUE) %>% 
  setNames(sort(unique(d_gap$continent)))

# El objeto resultante es una lista con dataframes adentro
lista_df # Chequear que nombres estén bien

# De esta forma, puedo hacer un loop para guardar cada uno de ellos 
for (i in seq_along(lista_df)) {
  
  filename <-  paste0("resultados/loops/", names(lista_df)[i], ".csv")
  write.csv(lista_df[[i]], filename)

}

# Ahora con un loop leo todos los .csv en la carpeta "resultados/loop"
rm(list=ls())

# Lista con dataframes
filenames <- list.files("resultados/loops", full.names=TRUE)

# Creo la lista con los nombres que va a tener cada base (sacar .csv)
namesfiles <- substr(filenames, 18, nchar(filenames)-4) 

# Ahora hago un loop para leer cada base
for (i in seq_along(filenames)) {
  
  assign(namesfiles[i], 
         read_csv(filenames[i])
  )
}


## 2.2. Loops con dataframes ----

# Por último, operaciones sobre todos los dataframes
lista_df_new <- list(Africa, Americas, Asia, Europe, Oceania)
names(lista_df_new) <- c("Africa", "Americas", "Asia", "Europe", "Oceania")


# Una versión menos manual
rm(lista_df_new, filenames, i, namesfiles)
# Une todos los objetos en el ambiente, es decir, si tenemos algo en el 
# ambiente que no queremos incluir debemos o borrarlo o establecer el argumento
# pattern dentro de ls() para indicar que elementos incluir
lista_df_new <- mget(ls())

names(lista_df_new)

list_df_final <- lapply(lista_df_new, function(base) {
  
  base <- base %>% 
    mutate(var_nueva = "Valor nuevo") %>% 
    select(-lifeExp)
  
})

# Unir todos los dataframes
df_gapminder <- bind_rows(list_df_final)
as_tibble(df_gapminder)

# Elimino todos los objetos del ambiente menos list_df_final
rm(list=setdiff(ls(), "list_df_final"))

# Exportar todos los dataframes al ambiente
list2env(list_df_final, .GlobalEnv)

# Podríamos haber hecho todo con lapply 
filenames <- list.files("resultados/loops", full.names=TRUE)

lista_df_2 <- lapply(filenames, read.csv)

# Agrego nombres
names(lista_df_2) <- c("Africa", "Americas", "Asia", "Europe", "Oceania")

# Unimos nuevamente
df_gapminder_2 <- bind_rows(lista_df_2)
as_tibble(df_gapminder_2)

# Exporto nuevamente al ambiente
rm(list=setdiff(ls(), "lista_df_2"))

list2env(lista_df_2, .GlobalEnv)
