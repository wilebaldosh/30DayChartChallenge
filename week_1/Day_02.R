# Reto: 30DayChartChallenge ####
# Semana 1: Comparisons
# Día 2: Pictogram
# Alumnos, hombres y mujeres, inscritos en mi escuela
# De acuerdo a los datos en el siie

# Cargamos las librerías ####
library(tidyverse)
library(extrafont)
library(ggimage)
library(waffle)
library(hrbrthemes)


# Leemos y limpiamos los datos ####
# Leemos los datos del csv, bajo la codificación utf-8, indicamos que hay un
# fila con los nombres de las columnas,
alumnos <- read.csv("data/raw/edades.csv",
                    encoding="UTF-8",
                    header=TRUE) %>% 
  # Creamos la variable genero para indicar si son Hombres o Mujeres
  mutate(genero = case_when(
    substring(raiz, first = 11, last = 11) == 'H' ~ "Hombres",
    TRUE ~ "Mujeres"
  )) %>% 
  # Agrupamos por género
  group_by(genero) %>% 
  # Contamos los datos
  summarise(cantidad = n())

# Personalizamos los datos ####
# Definimos un DataFrame para extraer el género y la cantidad
hombres <- alumnos %>%
  filter(genero == "Hombres")

mujeres <- alumnos %>%
  filter(genero == "Mujeres")

# Definimos las etiquetas personalizadas para cada género
customtext <- c(
  paste("Hombres", '=', hombres$cantidad),
  paste("Mujeres", '=', mujeres$cantidad)
)

# Trazamos el pictograma ####
ggplot(alumnos, aes(label = genero, 
                 values = cantidad, 
                 color = genero)) +
  geom_pictogram(n_rows = 10) +
  scale_color_manual(
    name = NULL,
    values = c(
      Hombres = "#01BFC4",
      Mujeres = "#AA2FF7"
    ),
    labels = customtext
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c(
      Hombres = "male",
      Mujeres = "female"
    ), 
    labels = customtext) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "Cantidad de alumnos de la Esc. Sec. Téc. No. 24 'Plan de San Luis'",
    subtitle = "Alumnos inscritos y activos",
    caption = "Fuente: Registro de estudianes de la escuela. Elaborado por el Profr. Wilebaldo Santana (@willshDev)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 20, face = "italic"),
    plot.caption = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.position = "bottom",
    # axis.text.x = element_text(vjust = 0.5),
    text = element_text("Tahoma", size = 20)
  )


# Guardamos el Pictograma ####
ggsave("reports/figures/day_2.png", height = 10, width = 12)
