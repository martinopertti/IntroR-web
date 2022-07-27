
## ***************************************************************************
##  Clase extra: Datos de encuesta      
##  Código de la presentación         
##  Escuela de Invierno en Métodos    
##  Martín Opertti - 2022             
## ***************************************************************************


## 0. Cargar paquetes ========================================================

library(tidyverse)
library(haven)
library(srvyr)
  
rm(list = ls())



## 1. Importar datos y etiquetas  ==========================================

## 1.1. Importar datos ----
lat_17 <- read_spss("data/Lat_17.sav") 

# Vemos etiqueta pero el valor real es el código numérico 
lat_17

# Por ejemplo, en vista de datos ya vemos solo el código
View(lat_17)


## 1.2. De etiquetas a valores  ----

lat_17 <- read_spss("data/Lat_17.sav") %>% 
  as_factor() # Etiquetas como valores

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
  mutate(wt = as.numeric(wt)) %>%
  rename(int_voto = P16STGBS) %>% 
  mutate(int_voto = as.character(int_voto)) %>% 
  mutate(int_voto_res = case_when(
    str_detect(int_voto, "FPV") ~ "FPV",
    str_detect(int_voto, "PRO") ~ "Cambiemos",
    str_detect(int_voto, "UCR") ~ "Cambiemos",
    TRUE ~ "Otros"
  ))

# Chequeo
table(lat_17$int_voto, lat_17$int_voto_res)



## 2. Resumenes de datos ponderados  ======================================

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



## 3. Ponderación (raking) ===============================================

# Con anesrake podemos ponderar con el método de raking

# install.packages("anesrake")
library(anesrake)

# Supongamos que quiero ponderar por sexo y el parámetro es 60% mujeres y 
# 40% hombres 
prop.table(table(lat_17$sexo)) # Hay diferencia importante

# Creo vector con parámetro y nombro los valores 
# Tienen que coincidir con los valores de la variable
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

# Algunos paso previos: crear id para encuestados, chequear que las variables 
# sean factores y que la data sea formato data.frame no tibble

# Creo un id para cada encuestado
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
glimpse(lat_17)

# Chequeo que haya funcionado
lat_17 %>% 
  group_by(sexo) %>% 
  summarise(sexo = sum(ponderador)) %>% 
  mutate(sexo = sexo / sum(sexo))
            
lat_17 %>% 
  group_by(edad_rec) %>% 
  summarise(edad_rec = sum(ponderador)) %>% 
  mutate(edad_rec = edad_rec / sum(edad_rec))

## Comparar intención de voto según ponderación (sin ponderar, ponderador
# original y ponderador con parámetros falsos como ejemplo)
lat_17 %>% 
  group_by(int_voto_res) %>% 
  summarise(cruda = n(),
            wt = sum(wt),
            pond = sum(ponderador)) %>% 
  mutate(cruda = cruda / sum(cruda),
         wt = wt / sum(wt),
         pond = pond / sum(pond))  



## 4. survey y srvyr =====================================================

library(srvyr)

## 4.1. Crear objeto de encuesta ----

# Declarar como survey object con ponderador
svy <- lat_17 %>% 
  as_survey_design(weight = wt)
svy

# Manipulación igual a dplyr
svy <- svy %>% 
  rename(pais = idenpa)  
svy

# Podemos usar funciones de srvyr como survey_mean() o survey_total() 
# dentro de summarize() como en dplyr:


## 4.2. Resumenes de variables numéricas ----
#Media, desvío estandar e intervalo de confianza
svy %>%
  srvyr::summarize(media_edad = survey_mean(edad, vartype = c("se", "ci")))

# Cuantiles
svy %>%
  srvyr::summarize(edad_quantil = survey_quantile(edad, c(0.25, 0.5, 0.75)))

# Podemos estimar estas métricas por grupo:
edad_int <- svy %>%
  group_by(int_voto_res) %>%
  srvyr::summarize(edad_mean = survey_mean(edad, vartype = c("se", "ci")))
edad_int

# Graficar con ggplot
ggplot(edad_int, aes(x = int_voto_res, y = edad_mean, color = int_voto_res)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = edad_mean_low, ymax = edad_mean_upp), 
                width = .05, lwd = .5) +
  theme_minimal() +
  labs(title = "Media de edad según intención de voto",
       x = "",
       y = "") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")

ggsave("resultados/plots/svy2.png", width = 30, height = 20, units = "cm")


## 4.2. Resumenes de variables categóricas ----

# Frecuencias de una variable
svy %>%
  group_by(int_voto_res) %>%
  srvyr::summarize(porcentaje = survey_mean(),
                   n_pond = survey_total(),
                   n_no_pond = unweighted(n()))

# Sumar otra variable
res <- svy %>%
  group_by(sexo, int_voto_res) %>%
  srvyr::summarize(porcentaje = survey_mean(vartype = "ci"))
res

# Graficar
ggplot(res, aes(x = int_voto_res, y = porcentaje, color = int_voto_res)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = porcentaje_low, ymax = porcentaje_upp), 
                width = .05, lwd = .5) +
  theme_minimal() +
  labs(title = "% de intención de voto según sexo",
       x = "",
       y = "") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none") +
  facet_wrap(~sexo)

ggsave("resultados/plots/svy3.png", width = 30, height = 20, units = "cm")

