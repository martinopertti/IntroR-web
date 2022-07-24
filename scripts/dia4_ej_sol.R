
## ***************************************************************************
##   Día 4: Visualización de datos y estadística inferencial
##   Solución de ejercicios             
##   Escuela de Invierno en Métodos    
##   Martín Opertti - 2022             
## ***************************************************************************

library(tidyverse)

## Data de encuestas de las elecciones 2016 en EEUU del paquete dslabs, 
# de FiveThirtyEight. Pueden consultar la documentación aquí: 
# https://www.rdocumentation.org/packages/dslabs/versions/0.7.3/topics/polls_us_election_2016

library(dslabs)
data <- polls_us_election_2016

# Explorar data
glimpse(data)


## 1. Exploremos los datos: crea un histograma rápido para ver la distribución 
# de la intención de voto de Donald Trump rawpoll_trump
hist(data$rawpoll_trump)


## 2. En el histograma anterior vemos que hay mucha variación. Filtremos la 
# data para analizar solamente las encuestas nacionales ("U.S." en variable 
# state) y volvamos a tirar el histograma
data <- data %>% 
  filter(state == "U.S.")
hist(data$rawpoll_trump)


## De ahora en más usaremos solamente las encuestas nacionales

## 3. La variable grade indica la calidad de cada encuestadora según 538
## Crea un gráfico de barras con la cantidad de encuestas por grade
# Ver: (https://projects.fivethirtyeight.com/pollster-ratings/)
ggplot(data = data, aes(x = grade)) +
  geom_bar()


## 4. Ahora por claridad eliminemos las encuestas que no tienen calificación y 
# revertamos el orden de la variable grade (fct_rev() puede servirte)
data <- data %>% 
  drop_na(grade)

theme_set(theme_bw(base_size = 15)) # Fijamos el tema

ggplot(data = data, aes(x = fct_rev(grade))) +
  geom_bar()

## 4b. Filtremos una vez más, está ves para quedarnos con las encuestas con 
# grado A- o más
grades_aceptables <- c("A+", "A", "A-")
data <- data %>% 
  filter(grade %in% grades_aceptables)


## 5. Ahora veamos la evolución de la intención de voto a Trump a lo largo del 
# tiempo. Crea el gráfico que consideres apropiado para ello, usando ggplot2. 
# Utiliza el dataframe filtrado en  el paso anterior. Usa la fecha del último
# día de la encuesta (enddate)

ggplot(data, aes(x = enddate, y = rawpoll_trump)) +
  geom_point() +
  geom_line() 

# Hay mucha variación, probemos con geom_smooth() para ver una tendencia suavizada
ggplot(data, aes(x = enddate, y = rawpoll_trump)) +
  geom_point(size = 2, alpha = .6, color = "firebrick2") +
  geom_smooth(se = FALSE, color = "firebrick", size = 1.5)


## 6. Agregemos los datos de intención de voto para Clinton. 
# Cuidado! la data no está en formato tidy para realizar esto, pivot_longe() puede servirte
data_long <- data %>% 
  select(enddate, grade, samplesize, rawpoll_trump, rawpoll_clinton) %>% 
  pivot_longer(cols = c("rawpoll_trump", "rawpoll_clinton"),
               names_to = "candidate",
               values_to = "vote_int") 

ggplot(data_long, aes(x = enddate, y = vote_int, color = candidate)) +
  geom_point(size = 2, alpha = .6) +
  geom_smooth(se = FALSE, size = 1.5)


## No, no fue un buen año para las encuestadoras...

## 7. Supongamos que queremos incluir este gráfico en una publicación. Elijamos 
# colores  representativos (asociados con los partidos de los candidatos), 
# definamos titulos y fuente. 
ggplot(data_long, aes(x = enddate, y = vote_int, color = candidate)) +
  geom_point(size = 2, alpha = .4) +
  geom_smooth(se = FALSE, size = 1.5) +
  scale_color_manual(name = "Candidato/a",
                     values = c("rawpoll_clinton" = "royalblue3", 
                                "rawpoll_trump" = "firebrick2"),
                     labels = c("rawpoll_clinton" = "Hillary Clinton", 
                                "rawpoll_trump" = "Donald Trumo")) +
  theme(legend.position = "bottom") +
  labs(title = "Intención de voto para las elecciones de 2016 en Estados Unidos",
       subtitle = "% crudo. Solo encuestas nacionales de encuestadoras con grado A- o mayor",
       caption = "Data: FiveThirtyEight",
       x = "último día de campo de la encuesta",
       y = "% del electorado")

# Con el argument labels dentro de scale_color_manual también puedo cambiar las 
# etiquetas o podría directamente recodificar los valores de "candidate" en el
# dataframe

## 8. Agregemos a nuestro gráfico un último elemento: el tamaño muestral
ggplot(data_long, aes(x = enddate, y = vote_int, color = candidate)) +
  geom_point(aes(size = samplesize), alpha = .4) +
  geom_smooth(se = FALSE, size = 1.5) +
  scale_color_manual(name = "Candidato/a",
                     values = c("rawpoll_clinton" = "royalblue3", 
                                "rawpoll_trump" = "firebrick2"),
                     labels = c("rawpoll_clinton" = "Hillary Clinton", 
                                "rawpoll_trump" = "Donald Trumo")) +
  theme(legend.position = "bottom") +
  labs(title = "Intención de voto para las elecciones de 2016 en Estados Unidos",
       subtitle = "% crudo. Solo encuestas nacionales de encuestadoras con grado A- o mayor",
       caption = "Data: FiveThirtyEight",
       x = "último día de campo de la encuesta",
       y = "% del electorado") +
  scale_size_continuous(name = "Tamaño muestral")

## 9. Parecería que en el último mes la diferencia entre los candidatos era menor
# Filtremos las  encuestas  desde 1 de octubre de 2016 y creamos un boxplot para
# ver las diferencias  en la intención de voto a ambos candidatos
range(data_long$enddate)

data_oct <- data_long %>% 
  filter(enddate >= "2016-10-01")

range(data_oct$enddate)

ggplot(data_oct, aes(x = candidate, y = vote_int)) +
  geom_boxplot(aes(fill = candidate), outlier.shape = NA, lwd=1, alpha=0.4) +
  geom_jitter(aes(color = candidate), size = 4, alpha = 0.75) +
  scale_color_manual(name = "Candidato/a",
                     values = c("rawpoll_clinton" = "royalblue3", 
                                "rawpoll_trump" = "firebrick2"),
                     labels = c("rawpoll_clinton" = "Hillary Clinton", 
                                "rawpoll_trump" = "Donald Trumo")) +
  scale_fill_manual(name = "Candidato/a",
                     values = c("rawpoll_clinton" = "royalblue3", 
                                "rawpoll_trump" = "firebrick2"),
                     labels = c("rawpoll_clinton" = "Hillary Clinton", 
                                "rawpoll_trump" = "Donald Trumo")) +
  theme(legend.position = "bottom") +
  labs(title = "Diferencias en la intención de voto a Trump y Clinton en mes previo a la elección 2016",
       caption = "Data: FiveThirtyEight",
       x = "", 
       y = "") +
  scale_x_discrete(labels=c("Hillary Clinton", "Donald Trump")) # Cambio etiqueta de valores del eje x
  
## 10. Por último, estimemos un modelo de regresión para ver si el tamaño
# muestral es relevante en la precisión de las encuestadoras. Para ello
# calculemos el valor absoluto de la diferencia entre  la intención de voto a
# Trump en cada encuesta y su % final que obtuvo en 2016 (46.1%)
data <- data %>% 
  mutate(margen = abs(46.1 - rawpoll_trump))

range(data$margen) # Diferencias entre 0.1 y 17.3

# También podemos calcular los días que faltan para la elección (electin day es
# el proxy)
data <- data %>% 
  mutate(dias_ele = as.numeric(as.Date("2016-11-08") - enddate))

reg <- lm(margen ~ samplesize + dias_ele, data = data)
summary(reg) # COn summary podemos ver los resultados

## Con el paquete broom y la función tidy() podemos extraer los resultaods del 
# modelo en un dataframe en formato tidy!

library(broom)

coef <- tidy(reg, conf.int = TRUE)
print(coef)

# Ahora podemos directamente graficar los coeficinetes de la forma en queramos
ggplot(coef, aes(x = estimate, y = term)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  labs(title = "¿Cuánto influye el tamaño de la muestra en la precisión de la encuesta?",
       subtitle = "Coeficientes de modelo de regresión lineal",
       caption = "Data: FiveThirtyEight")

# Sin intercepto
coef %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = estimate, y = term)) +
  xlim(-0.015, 0.015) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  labs(title = "¿Cuánto influye el tamaño de la muestra en la precisión de la encuesta?",
       subtitle = "Coeficientes de modelo de regresión lineal",
       caption = "Data: FiveThirtyEight")
  

