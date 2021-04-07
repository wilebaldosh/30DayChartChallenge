# Reto: 30DayChartChallenge ####
# Semana 1: Comparisons
# Día 3: Historical
# Tipo de cambio del Dolar y el Euro frente al Peso mexicano
# Con datos de Banxico

# Cargamos las librerías ####
library(tidyverse)
library(readxl)

# Leemos los datos ####
# Datos de Banxico
# https://www.banxico.org.mx/SieInternet/consultarDirectorioInternetAction.do?sector=6&accion=consultarCuadroAnalitico&idCuadro=CA113&locale=es
peso <- read_excel("data/raw/Consulta_20210402-234021418.xlsx",
                   sheet = "Hoja1", skip = 17) %>% 
  select(1, 51, 135)

# Transformación de datos ####
# Transformamos los datos para poder graficarlos
dolar <- peso %>% 
  select(., 1, 2) %>% 
  mutate(tipo = "Dólar")
names(dolar) <- c("fecha", "valor", "Moneda")

euro <- peso %>% 
  select(., 1, 3) %>% 
  mutate(tipo = "Euro")
names(euro) <- c("fecha", "valor", "Moneda")

peso <- rbind(dolar, euro)

# Graficamos ####
ggplot(peso, aes(x = fecha, y = valor)) +
  geom_area(aes(color = Moneda, fill = Moneda),
            alpha = 0.5, position = position_dodge(0.8))+
  scale_color_manual(name = NULL,
                     values = c("#000000", "#FF0000"),
                     labels =c("Dolar", "Euro")) +
  scale_fill_manual(name = NULL,
                    values = c("#00AFBB", "#DB3E2A"),
                    labels =c("Dolar", "Euro")) +
  theme_light() +
  labs(
    title = "Tipo de cambio del dolar y el euro frente al peso",
    subtitle = "Comparación mensual desde el 2000-01-01, hasta el 2021-03-01",
    y = "Valor",
    x = "Tiempo",
    caption = "Fuente: Banxico. Elaborado por el Profr. Wilebaldo Santana (@willshDev)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 20, face = "italic"),
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic"),
    legend.position = "bottom",
    axis.text.x = element_text(vjust = 0.5),
    text = element_text("Tahoma", size = 20)
  )


# Guardamos el gráfico ####
ggsave("reports/figures/day_03.png", height = 10, width = 12)
