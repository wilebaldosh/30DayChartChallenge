# Reto: 30DayChartChallenge ####
# Semana 1: Comparisons
# Día 3: historical
# Promedios de la EST 7 desde el 2005, y hasta el 2019.
# De acuerdo a los datos 

# Cargamos las librerías ####
library(tidyverse)
library(extrafont)

# Leemos y limpiamos los datos ####
# Leemos los datos del csv, bajo la codificación utf-8, indicamos que hay un
# fila con los nombres de las columnas,
est <- read_csv("data/raw/est.csv",,
                na = "NA",
                guess_max = 70e3) %>% 
  janitor::clean_names() %>% 
  filter(est == "7") %>% 
  drop_na() %>%
  select(1,3, 6:10, 18, 25) %>%
  mutate(aprov_fin = (as.numeric(aprov_b1) + as.numeric(aprov_b2) + 
           as.numeric(aprov_b3) + as.numeric(aprov_b4) + 
           as.numeric(aprov_b5))/5)

ggplot(est, aes(x = ciclo, y = aprov_fin)) +
  geom_line(colour = "blue") +
  geom_point() +
  ylim(5, 10) +
  scale_x_continuous(breaks = seq(2005, 2019, 1)) +
  ggtitle("Histórico de calificaciones de la Escuela Secundaria Técnica Pesquera No. 7 de San Blas, Nayarit") +
  theme(plot.title = element_text(vjust = 2))
