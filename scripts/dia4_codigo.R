
## ***************************************************************************
##   Día 4: Visualización de datos y estadística inferencial
##   Código de la presentación         
##   Escuela de Invierno en Métodos    
##   Martín Opertti - 2022             
## ***************************************************************************


## 0. Paquetes  ==============================================================

library(tidyverse)
library(gapminder)
library(RColorBrewer)
library(viridis)
library(ggridges)
library(broom)
library(gganimate)
library(gifski) 



##  1. Anscombe cuartet ======================================================

# La data del cuarteto de Anscombe viene en R
anscombe <- datasets::anscombe

# Pasamos a formato tidy
anscombe <- anscombe %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)") %>% 
  mutate(set = recode(set, "1" = "Conjunto I",
                      "2" = "Conjunto II",
                      "3" = "Conjunto III",
                      "4" = "Conjunto IV"))
print(anscombe)

# Tabla resumen con algunos descriptivos (tienen más en común)
tabla_resumen <- anscombe %>% 
  group_by(set) %>% 
  summarize(n = 11,
            mean_x = mean(x),
            mean_y = mean(y),
            sd_x = sd(x),
            sd_y = sd(y)) 
tabla_resumen

# Gráfico por set
ggplot(data = anscombe, aes(x = x, y = y)) +
  geom_point(size = 4, color = "darkgreen", alpha = .8) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~ set) +
  theme_bw() +
  labs(title = "Cuartetos de Anscombe")



##  2. Gráficos con R Base  ==================================================

rm(anscombe, tabla_resumen)

# Data de gaminder para el año 2007
d_gap_7 <- gapminder %>% 
  filter(year == 2007)

# Histograma de población
hist(d_gap_7$pop,
     main = "Distribución de la población por país en 2007",
     xlab = "Población", 
     ylab = "Cantidad de países")

# Para guardarlo:
png("resultados/plots/rbase_hist.png") # Guardamos en png
hist(d_gap_7$pop,
     main = "Distribución de la población por país en 2007",
     xlab = "Población", 
     ylab = "Cantidad de países")
dev.off()

# Gráfico de barras
m_con_lif <- d_gap_7 %>% 
  group_by(continent) %>% 
  summarize(media_life_exp = mean(lifeExp))

png("resultados/plots/rbase_bar.png")
barplot(m_con_lif$media_life_exp, 
        names = m_con_lif$continent,
        main = "Expectativa de vida promedio por continente en 2007",
        col = "skyblue3",
        border ="black",
        density = 75)
dev.off()

# Gráfico de dispersión
png("resultados/plots/rbase_disp.png")
plot(x = d_gap_7$gdpPercap,
     y = d_gap_7$lifeExp,
     main = "Relación entre expectativa de vida y pbi per cápita en 2007",
     xlab = "PBI per cápita",
     ylab = "Expectativa de vida")
dev.off()



##  3. Gráficos con ggplot2   ===============================================

d_gap_7 <- gapminder %>% 
  filter(year == 2007)

# Especifico la data a usar
ggplot(data = d_gap_7)
ggsave("resultados/plots/plot1.png", width = 30, height = 20, units = "cm")


# Asigno las primeras aesthetics (posición: x e y)
ggplot(data = d_gap_7, aes(x = gdpPercap, y = lifeExp))
ggsave("resultados/plots/plot2.png", width = 30, height = 20, units = "cm")


# Agrego con + una segunda capa: geom_point para dispersión
ggplot(data = d_gap_7, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()
ggsave("resultados/plots/plot3.png", width = 30, height = 20, units = "cm")


# Asigno atributos de geom_point: color, size, shape
ggplot(data = d_gap_7, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(color = "black", fill = "skyblue3", size = 3, shape = 21)
ggsave("resultados/plots/plot4.png", width = 30, height = 20, units = "cm")


# Asigno color según una variable que agrupa (siempre dentro de aes()!)
ggplot(data = d_gap_7, aes(x = gdpPercap, y = lifeExp, fill = continent)) +
  geom_point(size = 3, shape = 21, alpha = .7) 
ggsave("resultados/plots/plot5.png", width = 30, height = 20, units = "cm")


# También puedo crear la línea de tendencia por grupo!
ggplot(data = d_gap_7, 
               aes(x = gdpPercap, y = lifeExp, fill = continent, color = continent)) +
  geom_point(size = 3, shape = 21,  alpha = .7) +
  geom_smooth(method = "lm", se = FALSE)
ggsave("resultados/plots/plot6.png", width = 30, height = 20, units = "cm")


# Cambiamos la escala de las x. Probar también xlim() e ylim()
ggplot(data = d_gap_7, 
       aes(x = gdpPercap, y = lifeExp, fill = continent, color = continent)) +
  geom_point(size = 3, shape = 21,  alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() 
ggsave("resultados/plots/plot7.png", width = 30, height = 20, units = "cm")


# Agregamos facetas
ggplot(data = d_gap_7 %>% 
         filter(continent != "Oceania"), 
       aes(x = gdpPercap, y = lifeExp, fill = continent, color = continent)) +
  geom_point(size = 3, shape = 21,  alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  facet_wrap(~ continent) 
ggsave("resultados/plots/plot8.png", width = 30, height = 20, units = "cm")


# Quitamos etiqueta duplicada
ggplot(data = d_gap_7 %>% 
         filter(continent != "Oceania"), 
       aes(x = gdpPercap, y = lifeExp, fill = continent, color = continent)) +
  geom_point(size = 3, shape = 21,  alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  facet_wrap(~ continent) +
  theme(legend.position = "none")
ggsave("resultados/plots/plot9.png", width = 30, height = 20, units = "cm")


# Quitamos colores innecesarios
ggplot(data = d_gap_7 %>% 
         filter(continent != "Oceania"), 
       aes(x = gdpPercap, y = lifeExp)) +
  geom_point(size = 3, shape = 21,  alpha = .7, fill = "skyblue") +
  geom_smooth(method = "lm", se = FALSE, color = "navyblue") +
  scale_x_log10() +
  facet_wrap(~ continent) 

ggsave("resultados/plots/plot10.png", width = 30, height = 20, units = "cm")


# Agregamos tamaño de población 
ggplot(data = d_gap_7 %>% 
         filter(continent != "Oceania"), 
       aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(size = pop), shape = 21,  alpha = .7, fill = "skyblue") +
  geom_smooth(method = "lm", se = FALSE, color = "navyblue") +
  scale_x_log10() +
  facet_wrap(~ continent) 

ggsave("resultados/plots/plot11.png", width = 30, height = 20, units = "cm")


# Población en millones
ggplot(data = d_gap_7 %>% 
         filter(continent != "Oceania") %>% 
         mutate(pop_mil = pop / 1000000), 
       aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(size = pop_mil), shape = 21,  alpha = .7, fill = "skyblue") +
  geom_smooth(method = "lm", se = FALSE, color = "navyblue") +
  scale_x_log10() +
  facet_wrap(~ continent) +
  theme(legend.position = "bottom") +
  scale_size_continuous(name = "esto es una prueba")

ggsave("resultados/plots/plot12.png", width = 30, height = 20, units = "cm")


# Agregamos tema y etiquetas
ggplot(data = d_gap_7 %>% 
         filter(continent != "Oceania") %>% 
         mutate(pop_mil = pop / 1000000),
       aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(size = pop_mil), shape = 21,  alpha = .7, fill = "skyblue") +
  geom_smooth(method = "lm", se = FALSE, color = "navyblue") +
  scale_x_log10() +
  facet_wrap(~ continent) +
  scale_size_continuous(name = "Población (en millones)") +
  labs(title = "PBI per cápita y expectativa de vida",
       subtitle = "Data de 2017",
       caption = "Fuente: Gapminder",
       x = "PBI per cápita",
       y = "Expectativa de vida") +
  theme_bw() +
  theme(legend.position = "bottom") # Movemos hacia abajo porque theme_bw() la soobreescribe

ggsave("resultados/plots/plot13.png", width = 30, height = 20, units = "cm")


## Con theme_set() podemos fijar el tema para todos los gráficos sin necesidad de volver a 
# especificar
theme_set(theme_bw(base_size = 15))



##  4. Aesthetics  ===========================================================

##  4.1. Formas  ----

# Sirve pirncipalmente para geom_point()

rm(list = ls())

d_gap_7 <- gapminder %>% 
  filter(year == 2007)

# Definir forma específica
ggplot(d_gap_7, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(shape = 9, size = 3)

ggsave("resultados/plots/plot14.png", width = 30, height = 20, units = "cm")


# Asignar forma según continente (ggplot elige por defecto formas)
ggplot(d_gap_7, 
       aes(x = gdpPercap, y = lifeExp, shape = continent)) +
  geom_point(size = 3) +
  theme(legend.position = "bottom")

ggsave("resultados/plots/plot15.png", width = 30, height = 20, units = "cm")


# Definir manualmente la forma para cada continente
ggplot(d_gap_7, 
       aes(x = gdpPercap, y = lifeExp, shape = continent)) +
  geom_point(size = 3) +
  theme(legend.position = "bottom") +
  scale_shape_manual(name = "Continente",
                     values = c(15, 16, 17, 18, 19))

ggsave("resultados/plots/plot16.png", width = 30, height = 20, units = "cm")


# Definir manualmente la forma para cada continente (otra forma)
ggplot(d_gap_7, 
       aes(x = gdpPercap, y = lifeExp, shape = continent)) +
  geom_point(size = 3) +
  theme(legend.position = "bottom") +
  scale_shape_manual(name = "Continente",
                     values = c("Europe" = 3,
                                "Oceania" = 8, 
                                "Africa" = 12, 
                                "Asia" = 18, 
                                "Americas" = 22))

ggsave("resultados/plots/plot17.png", width = 30, height = 20, units = "cm")


##  4.2. Líneas  ----

pais_conosur <- c("Argentina", "Chile", "Uruguay")

conosur <- gapminder %>% 
  filter(country %in% pais_conosur)

# Lineas por variable
ggplot(conosur, aes(x = year, y = lifeExp)) +
  geom_line(aes(linetype = country)) +
  theme(legend.position = "bottom")

ggsave("resultados/plots/plot18.png", width = 30, height = 20, units = "cm")


# Definir tipo de linea por país
ggplot(conosur, aes(x = year, y = lifeExp)) +
  geom_line(aes(linetype = country)) +
  theme(legend.position = "bottom") +
  scale_linetype_manual(name = "País",
                        values = c("Argentina" = "dotted",
                                   "Chile" = "dashed",
                                   "Uruguay" = "solid"))

ggsave("resultados/plots/plot19.png", width = 30, height = 20, units = "cm")


# Definir tipo de linea por país
ggplot(conosur, aes(x = year, y = lifeExp)) +
  geom_line(aes(linetype = country)) +
  geom_point(aes(shape = country)) +
  theme(legend.position = "bottom") +
  scale_linetype_manual(name = "País",
                        values = c("Argentina" = "dotted",
                                   "Chile" = "dashed",
                                   "Uruguay" = "solid")) +
  labs(linetype = "País", shape = "País")  # Une las etiquetas (sino se duplican)

ggsave("resultados/plots/plot20.png", width = 30, height = 20, units = "cm")


##  4.3. Colores  ----

# Lista de colores
colours()

# Paletas de colores de RColorBrewer
par(mar=c(3,4,2,2))
display.brewer.all()

america07 <- gapminder %>% 
  filter(continent == "Americas" & year == 2007)


# Color por país (ggplot elige automático)
ggplot(conosur, aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  geom_point() +
  theme(legend.position = "bottom")

ggsave("resultados/plots/plot21.png", width = 30, height = 20, units = "cm")


# Color por país (asigno colores manualmente)
ggplot(conosur, aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  geom_point() +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "País",
                     values = c("midnightblue", "red3", "lightskyblue"))

ggsave("resultados/plots/plot22.png", width = 30, height = 20, units = "cm")


# Color por país (otros detalles estéticos)
ggplot(conosur, aes(x = year, y = lifeExp, color = country)) +
  geom_line(size = 1.5, alpha = 0.4) +
  geom_point(size = 3) +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "País",
                     values = c("midnightblue", "red3", "lightskyblue"))

ggsave("resultados/plots/plot23.png", width = 30, height = 20, units = "cm")


# Color por país (usando paletas de RColorBrewer: elegir una discreta para este caso)
ggplot(conosur, aes(x = year, y = lifeExp, color = country)) +
  geom_line(size = 1.5, alpha = 0.4) +
  geom_point(size = 3) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2")

ggsave("resultados/plots/plot24.png", width = 30, height = 20, units = "cm")


# Color con paleta continua con paquete viridis 
ggplot(d_gap_7, aes(x = pop, y = gdpPercap, color = lifeExp)) +
  geom_point(size = 3) +
  scale_x_log10() +
  scale_color_viridis(name = "Expectativa de vida") +
  theme(legend.position = "bottom") 

ggsave("resultados/plots/plot25.png", width = 30, height = 20, units = "cm")


# Color con paleta continua especificando valores
ggplot(d_gap_7, aes(x = pop, y = gdpPercap, color = lifeExp)) +
  geom_point(size = 3) +
  scale_x_log10() +
  scale_color_gradient(name = "Expectativa de vida", low = "red", high = "Blue") +
  theme(legend.position = "bottom") 

ggsave("resultados/plots/plot26.png", width = 30, height = 20, units = "cm")



##  5. Otros geoms  =========================================================

##  5.1. geom_bar() ----
table(d_gap_7$continent)

# Graficar cuántos países hay por continente en nuestra base
ggplot(data = d_gap_7, aes(x = continent)) +
  geom_bar()

ggsave("resultados/plots/plot27.png", width = 30, height = 20, units = "cm")


# Con datos resumidos (no funciona)
data_resumen <- d_gap_7 %>% 
  group_by(continent) %>% 
  summarize(n = n())

ggplot(data_resumen, aes(x = continent)) +
  geom_bar()

ggsave("resultados/plots/plot28.png", width = 30, height = 20, units = "cm")


# Para eso podemos usar geom_col(), especificando cuál es la columna con el valor
data_resumen <- d_gap_7 %>% 
  group_by(continent) %>% 
  summarize(n = n())

ggplot(data_resumen, aes(x = continent, y = n)) +
  geom_col()

ggsave("resultados/plots/plot28_b.png", width = 30, height = 20, units = "cm")


# Con datos resumidos: especificar y y cambiar stat a identity
ggplot(data_resumen, aes(x = continent, y = n)) +
  geom_bar(stat = "identity")

ggsave("resultados/plots/plot29.png", width = 30, height = 20, units = "cm")


# Supongamos ahora que quiero (con los datos completos) calcular el porcentaje  
# de cada continente sobre el total de países
ggplot(d_gap_7, aes(x = continent)) +
  geom_bar(aes(y = ..prop.., group = 1))

ggsave("resultados/plots/plot30.png", width = 30, height = 20, units = "cm")


# Supongamos que queremos ver cuántos países con esperanza de vida mayor a 75
# hay por continente
data75 <- d_gap_7 %>% 
  mutate(esp = case_when(lifeExp > 75 ~ 1,
                         TRUE ~ 0)) %>% 
  group_by(continent, esp) %>% 
  summarize(n = n())

ggplot(data75, aes(x = continent, y = n, fill = as.factor(esp))) +
  geom_bar(stat = "identity", position = "stack")

ggsave("resultados/plots/plot31.png", width = 30, height = 20, units = "cm")


## Posición
# Barras agrupadas
ggplot(data75, aes(x = continent, y = n, fill = as.factor(esp))) +
  geom_bar(stat = "identity", position = "dodge")

ggsave("resultados/plots/plot32.png", width = 30, height = 20, units = "cm")


# Barras proporción
ggplot(data75, aes(x = continent, y = n, fill = as.factor(esp))) +
  geom_bar(stat = "identity", position = "fill")

ggsave("resultados/plots/plot33.png", width = 30, height = 20, units = "cm")


## Estética 
# Volvamos a nuestro gráfico de barras original 
ggplot(data = d_gap_7, aes(x = continent)) +
  geom_bar(color = "black", fill = "skyblue3", alpha = .8) +
  labs(title = "Cantidad de países por continente",
       subtitle = "Data de Gapminder para el año 2007",
       caption = "Fuente: Gapminder",
       x = "",
       y = "")

ggsave("resultados/plots/plot34.png", width = 30, height = 20, units = "cm")


# Podemos girar el gráfico 
ggplot(data = d_gap_7, aes(x = continent)) +
  geom_bar(color = "black", fill = "skyblue3", alpha = .8) +
  labs(title = "Cantidad de países por continente",
       subtitle = "Data de Gapminder para el año 2007",
       caption = "Fuente: Gapminder",
       x = "",
       y = "") +
  coord_flip()

ggsave("resultados/plots/plot35.png", width = 30, height = 20, units = "cm")


# Podemos ordenar categorías como queramos 
positions <- c("Americas", "Europe", "Africa", "Oceania", "Asia")

ggplot(data = d_gap_7, aes(x = continent)) +
  geom_bar(color = "black", fill = "skyblue3", alpha = .8) +
  labs(title = "Cantidad de países por continente",
       subtitle = "Data de Gapminder para el año 2007",
       caption = "Fuente: Gapminder",
       x = "",
       y = "") +
  scale_x_discrete(limits = positions)

ggsave("resultados/plots/plot36.png", width = 30, height = 20, units = "cm")


# Podemos ordenar por la frecuencia de la variable
ggplot(data = d_gap_7, aes(x = fct_infreq(continent))) +
  geom_bar(color = "black", fill = "skyblue3", alpha = .8) +
  labs(title = "Cantidad de países por continente",
       subtitle = "Data de Gapminder para el año 2007",
       caption = "Fuente: Gapminder",
       x = "",
       y = "") 

ggsave("resultados/plots/plot37.png", width = 30, height = 20, units = "cm")


##  5.2. geom_text() ----

lista_a_sur <- c("Argentina", "Brazil", "Bolivia", "Chile", "Colombia", 
                 "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")
a_sur <- gapminder %>% 
  filter(year == 2007 & country %in% lista_a_sur)

## Gráfico de dispersión expectativa de vida y pbi per cápita
ggplot(a_sur, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

ggsave("resultados/plots/plot38.png", width = 30, height = 20, units = "cm")


# No es muy informativo, tenemos pocos puntos, podemos agregar etiquetas
ggplot(a_sur, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_text(aes(label = country))

ggsave("resultados/plots/plot39.png", width = 30, height = 20, units = "cm")


# Podemos ajustar la posición de las etiquetas
ggplot(a_sur, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_text(aes(label = country), hjust = 0.5, vjust = -1)

ggsave("resultados/plots/plot40.png", width = 30, height = 20, units = "cm")


# Podemos también dejar solo el texto
ggplot(a_sur, aes(x = gdpPercap, y = lifeExp)) +
  geom_text(aes(label = country))

ggsave("resultados/plots/plot41.png", width = 30, height = 20, units = "cm")


# También podemos usar geom_text() combinados con otros geoms
ggplot(data = d_gap_7, aes(x = fct_infreq(continent))) +
  geom_bar(color = "black", fill = "skyblue3", alpha = .8) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -.5, fontface = "bold") +
  labs(title = "Cantidad de países por continente",
       subtitle = "Data de Gapminder para el año 2007",
       caption = "Fuente: Gapminder",
       x = "",
       y = "") 

ggsave("resultados/plots/plot42.png", width = 30, height = 20, units = "cm")


# Para anotaciones podemos usar annotate()
ggplot(data = d_gap_7, aes(x = fct_infreq(continent))) +
  geom_bar(color = "black", fill = "skyblue3", alpha = .8) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -.5, fontface = "bold") +
  labs(title = "Cantidad de países por continente",
       subtitle = "Data de Gapminder para el año 2007",
       caption = "Fuente: Gapminder",
       x = "",
       y = "") +
  annotate("text", x = "Oceania", y = 10, label = "Que pocos países \n hay en Oceanía")

ggsave("resultados/plots/plot43.png", width = 30, height = 20, units = "cm")


## 5.3. geom_boxplot() ----

## Boxplot tradicional
d_gap_7 %>%
  filter(continent != "Oceania") %>% 
  ggplot(aes(x = continent, y = gdpPercap)) +
  geom_boxplot() 

ggsave("resultados/plots/plot44.png", width = 30, height = 20, units = "cm")


# Boxplot con todos los puntos con geom_jitter()
d_gap_7 %>%
  filter(continent != "Oceania") %>% 
  ggplot(aes(x = continent, y = gdpPercap)) +
  geom_boxplot(aes(fill = continent), outlier.shape = NA, lwd=1, alpha=0.4) +
  geom_jitter(aes(color = continent), size = 4, alpha = 0.9) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none")

ggsave("resultados/plots/plot45.png", width = 30, height = 20, units = "cm")


## 5.4. geom_density() ----

## Distribución del PBI per cápita en 2007
ggplot(d_gap_7, aes(x = gdpPercap)) +
  geom_density() 

ggsave("resultados/plots/plot46.png", width = 30, height = 20, units = "cm")


# Agregamos colores
ggplot(d_gap_7, aes(x = gdpPercap)) +
  geom_density(fill = "seagreen3", alpha = .7) 

ggsave("resultados/plots/plot47.png", width = 30, height = 20, units = "cm")


# Desagregar por continente
d_gap_7 %>%
  filter(continent != "Oceania") %>%
  ggplot(aes(x = gdpPercap, fill = continent)) +
  geom_density( alpha = .7, adjust = 1.5) +
  scale_fill_brewer(palette = "Accent") +
  facet_wrap(~ continent)

ggsave("resultados/plots/plot48.png", width = 30, height = 20, units = "cm")


## 5.5. ggridges ----

# Cone l paquete ggridges podemos ver distribuciones en en un mismo eje según una variable

# Evolución de distribuciones de expectativa de vida por año
ggplot(gapminder, aes(x = lifeExp, y = as.factor(year))) + 
  geom_density_ridges(fill = "lightblue")  +
  labs(title = "Distribución de expectativa de vida por año",
       caption = "Data: Gapminder",
       x = "", y = "")

ggsave("resultados/plots/plot49.png", width = 25, height = 30, units = "cm")


# Podemos usar facetas 
gapminder %>% 
  filter(continent == "Africa" | continent == "Europe") %>%
  ggplot(aes(x = lifeExp, y = as.factor(year))) + 
  geom_density_ridges(fill = "lightblue") +
  facet_wrap(~ continent, nrow = 1) +
  labs(title = "Distribución de expectativa de vida por año en Africa y Europa",
       caption = "Data: Gapminder",
       x = "", y = "")

ggsave("resultados/plots/plot50.png", width = 30, height = 25, units = "cm")


# O en el mismo eje (mejor para comparar)
# Agrego la media con quantiles_ines y quantiles_fun!
gapminder %>% 
  filter(continent == "Africa" | continent == "Asia") %>% 
  ggplot(aes(x = lifeExp, y = as.factor(year), fill = continent)) + 
  geom_density_ridges(alpha = .6, quantile_lines = T, quantile_fun = mean) +
  scale_fill_manual(name = "Continente",
                    values = c("red3", "lightblue")) +
  labs(title = "Distribución de expectativa de vida por año en África y Asia",
       caption = "Data: Gapminder",
       x = "", y = "") 

ggsave("resultados/plots/plot51.png", width = 25, height = 30, units = "cm")


## 5.6. Gráficos animados -----

d_gap <- (gapminder)

## Supongamos que queremos anaizar el avanze de la expectativa de vida y del
# PBI per cápita en el mundo utilizando los datos de Gapminder

# Primero podríamos crear un gráfico tradicional 
p_static <- ggplot(d_gap, 
                   aes(x = gdpPercap, y = lifeExp, colour = continent)) +
  geom_point(aes(size = pop), alpha = 0.7) +
  scale_size(range = c(2, 12)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_log10() +
  guides(size = FALSE) +
  labs(x = "PBI per capita", y = "Expectativa de vida") +
  theme(legend.position = "bottom") 

p_static

ggsave("resultados/plots/static1.png", width = 25, height = 30, units = "cm")

# Agrego facetas por año 
p_static_facet <- p_static +
  facet_wrap(~ year)

p_static_facet

ggsave("resultados/plots/static2.png", width = 25, height = 30, units = "cm")

# Animo el primer gráfico con transition_time() de gganimate
# debo especificar la variable que se mueve, en 
p_ani <- p_static + 
  transition_time(year) +
  labs(title = "Año: {frame_time}")

anim_save("resultados/plots/anim_1.gif", p_ani)

# Agregamos sombra
p_ani_2 <- p_ani + 
  shadow_wake(wake_length = 0.1, alpha = FALSE)

anim_save("resultados/plots/anim_2.gif", p_ani_2)

# Identificamos países por color
d_gap <- gapminder %>% 
  mutate(etiqueta = case_when(
    country == "Argentina" ~ "Argentina",
    country == "Uruguay" ~ "Uruguay",
    country == "Chile" ~ "Chile",
    country == "Brazil" ~ "Brazil",
    country == "Costa Rica" ~ "Costa Rica",
    country == "Colombia" ~ "Colombia",
    country == "Italy" ~ "Italy",
    country == "Spain" ~ "Spain",
    TRUE ~ "Otros países"
  ))

# Color según variable etiqueta
p_static_2 <- ggplot(d_gap, 
                   aes(x = gdpPercap, y = lifeExp, colour = etiqueta)) +
  geom_point(aes(size = pop), alpha = 0.7) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  guides(size = FALSE) +
  labs(x = "PBI per capita", y = "Expectativa de vida") +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "País",
                     values = c("Argentina" = "dodgerblue3",
                                "Uruguay" = "skyblue",
                                "Chile" = "brown1",
                                "Brazil" = "chartreuse4",
                                "Costa Rica" = "brown4",
                                "Colombia" = "gold2",
                                "Italy" = "palegreen2",
                                "Spain" = "goldenrod4", 
                                "Otros países" = adjustcolor("gray72", alpha.f = .5)))
p_static_2

# Animamos
p_ani_3 <- p_static_2 + 
  transition_time(year) +
  labs(title = "Año: {frame_time}") +
  shadow_mark(alpha = 0.3, size = 0.5)

anim_save("resultados/plots/anim_3.gif", p_ani_3)



## 6. Intervalos de confianza  ==============================================

# Para instalar opuy
# install.packages("remotes")
# remotes::install_github("Nicolas-Schmidt/opuy")
library(opuy)

data <- opuy %>% 
  filter(medicion == "Evaluacion de gestion presidente",
         categoria == "Aprueba") %>% 
  select(fecha, empresa, presidente, valor) %>% 
  dplyr::rename(aprobacion = valor) %>%
  arrange(fecha)

# Leer directamente
# data <- readxl::read_excel("data/data_opuy.xlsx")

mean(data$aprobacion) # Media de todos los presidentes
sd(data$aprobacion) # Desvío de todos los presidentes

# Defino valor crítico con qnorm()
valor_critico <- qnorm(0.975)

# Calcular manualmente
data_resumen <- data %>% 
  group_by(presidente) %>% 
  dplyr::summarise(
    mean = mean(aprobacion),
    sd = sd(aprobacion),
    ci = valor_critico * (sd(aprobacion) / sqrt(n())),
    ci_low =  mean(aprobacion) - valor_critico * (sd(aprobacion) / sqrt(n())),
    ci_up =  mean(aprobacion) + valor_critico * (sd(aprobacion) / sqrt(n()))
    )

# Con summarySE() de Rmisc
table(opuy$medicion)
data_resumen_2 <- Rmisc::summarySE(data,
                                   measurevar = "aprobacion",
                                   conf.interval = 0.95,
                                   na.rm = TRUE,
                                   groupvars = "presidente") 
          
# Grafico resultados con ggplot2 intervalo de confianza
ggplot(data_resumen, aes(x = fct_reorder(presidente, -mean), y = mean)) + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_up), colour="black", width=.1) +
  geom_point(size=3) +
  labs(title = "Aprobación de presidente por administración",
       caption = "Fuente: OPUY",
       x = "",
       y = "",
       subtitle = "Las barras de error representan los intervalos de confianza") 

ggsave("resultados/plots/plot52.png", width = 30, height = 20, units = "cm")


# Grafico resultados con ggplot2 desvio estandar
ggplot(data_resumen, aes(x = fct_reorder(presidente, -mean), y = mean)) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                colour="black", width=.1) +
  geom_point(size=3) +
  labs(title = "Aprobación de presidente por administración",
       caption = "Fuente: OPUY",
       x = "",
       y = "", 
       subtitle = "Las barras de error representan el desvío estandar") 

ggsave("resultados/plots/plot52_b.png", width = 30, height = 20, units = "cm")


# Intervalo de confianza con barras
ggplot(data_resumen, aes(x = fct_reorder(presidente, -mean), y = mean)) + 
  geom_bar(aes(fill = presidente), 
           stat = "identity", alpha = .75, color = "black") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_up), colour = "black",
                width = .1, size = 1) +
  labs(title = "Aprobación de presidente por administración",
       caption = "Fuente: OPUY",
       x = "", y = "") +
  scale_fill_manual(values = c("Lacalle Pou" = "lightblue",
                               "Vazquez 1" = "midnightblue",
                               "Mujica" = "midnightblue", 
                               "Vazquez 2" = "midnightblue",
                               "Batlle" = "firebrick3",
                               "Sanguinetti 2" = "firebrick3", 
                               "Lacalle" = "lightblue")) +
  theme(legend.position = "none")

ggsave("resultados/plots/plot53.png", width = 30, height = 20, units = "cm")

  

## 7. Coeficientes de regresión  ==============================================

rm(list=ls())

## 7.1. Regresión lineal ----

# R trae un conjunto de funciones para estimar modelos de regresión.
# lm() sirve para estimar una regresión lineal. 

# Modelo de regresión lineal que predice la expectativa de vida en función del 
# pbi per cápita,  controlando por población, año y continente

gapminder <- (gapminder)

reg <- lm(lifeExp ~ gdpPercap + pop + year + continent, data = gapminder)
summary(reg) # COn summary podemos ver los resultados


##  7.2. Regresiones lineales generalizadas ----
## Con `glm()` podemos estimar otros modelos como probit, poisson o logit. 
# Para estimar una regresión logística creo una variable binaria a partir de lifeExp

gapminder <- mutate(gapminder, 
                    lifeExp_rec = case_when(lifeExp > 70 ~ 1,
                                            TRUE ~ 0)
                    )

# Por más que tenga solo dos valores, es numérica
class(gapminder$lifeExp_rec)

# Para esto debo transformarla a factor
gapminder <- mutate(gapminder,
                    lifeExp_rec = as.factor(lifeExp_rec))


class(gapminder$lifeExp_rec)

# Regresión logística
reg_logit <- glm(lifeExp_rec ~ gdpPercap + pop + year + continent,
                 family = binomial(link = "logit"),
                 data = gapminder)

summary(reg_logit)

# Cambiar para que la categoría de referencia de continente sea América

gapminder <- mutate(gapminder,
                    continent = relevel(continent, ref = "Americas"))

reg_logit_2 <- glm(lifeExp_rec ~ gdpPercap + pop + year + continent,
                 family = binomial(link = "logit"),
                 data = gapminder)

summary(reg_logit_2)


## 7.3. Limpiar resultados  y visualizar modelos ----

## Con el paquete broom y la función tidy() podemos extraer los resultados del
#  modelo en un dataframe en formato tidy!
coef <- tidy(reg, conf.int = TRUE)
print(coef)


# También para la regresión logística
coef_log2 <- tidy(reg_logit_2, conf.int = TRUE) %>%
  mutate_if(is.numeric, ~ round(., 6))

print(coef_log2)

# Odd ratios
coef_log3 <- tidy(reg_logit_2, 
                  exponentiate = TRUE,
                  conf.int = TRUE) %>%
  mutate_if(is.numeric, ~ round(., 5))

print(coef_log3)

# Estadisticas sobre la bondad del modelo
glance(reg_logit_2)

# Modelo más sencillo
reg_logit_4 <- glm(lifeExp_rec ~ gdpPercap + continent,
                   family = binomial(link = "logit"),
                   data = gapminder)

summary(reg_logit_4)

# Probabilidad esperadas (predicted probabilities)
pred_df <- expand.grid(continent = c("Africa", "Americas", "Europe"),
                          gdpPercap = seq(1000, 30000, by = 5000))

pred_prob <- augment(reg_logit_4, 
                     type.predict = "response", 
                     newdata = pred_df,
                     se_fit = TRUE) %>% 
  mutate(lower = .fitted - 1.96 * .se.fit,
         upper = .fitted + 1.96 * .se.fit) %>%
  mutate_if(is.numeric, ~ round(.,3)) 

pred_prob

# Grafico probabilidades
ggplot(pred_prob, aes(x = gdpPercap, y = .fitted, color = continent)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = lower, 
                    ymax = upper), 
                width = .1) +
  labs(x = "PBI per capita",
       y = "Probabilidad esperada",
       title = "Probabilidad esperada de tener una esperanza de vida mayor a 70 años según PBI y continente") +
  scale_color_brewer(palette = "Dark2")

ggsave("resultados/plots/plot54_pp.png", width = 30, height = 20, units = "cm")


## 7.4. Graficar coeficientes

# 2 modelos anidados
r_logit_1 <- glm(lifeExp_rec ~ continent,
                 family = binomial(link = "logit"),
                 data = gapminder)

coef_l_1 <- tidy(r_logit_1, conf.int = TRUE) 
  
r_logit_2 <- glm(lifeExp_rec ~ continent + gdpPercap,
                 family = binomial(link = "logit"),
                 data = gapminder)

coef_l_2 <- tidy(r_logit_2, conf.int = TRUE)
  

ggplot(coef_l_1, aes(x = estimate, y = term)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  labs(title = "Factores explicativos de la expectativa de vida",
       subtitle = "Coeficientes de regresión de lineal",
       caption = "Data: Gapminder")

ggsave("resultados/plots/plot54.png", width = 30, height = 20, units = "cm")


# Quitamos el intercepto y agregamos línea vertical en 0
coef_l_1 %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = estimate, y = term)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Factores explicativos de la expectativa de vida",
       subtitle = "Coeficientes de regresión de lineal (excluye intercepto)",
       caption = "Data: Gapminder")

ggsave("resultados/plots/plot55.png", width = 30, height = 20, units = "cm")


## Comparamos modelos anidados

# Primero variable que identifique cada dataframe
coef_l_1 <- mutate(coef_l_1, modelo = "Modelo 1")
coef_l_2 <- mutate(coef_l_2, modelo = "Modelo 2")

# Unimos los resultados de ambos modelos
coef_l_1_2 <- rbind(coef_l_1, coef_l_2)

# Graficamos ambos modelos
coef_l_1_2 %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = estimate, y = term, color = modelo)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Comparación modelos",
       subtitle = "Coeficientes de regresión logística (excluye intercepto)",
       caption = "Data: Gapminder")

ggsave("resultados/plots/plot56.png", width = 30, height = 20, units = "cm")






