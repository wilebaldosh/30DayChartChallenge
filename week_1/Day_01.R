# Reto: 30DayChartChallenge ####
# Semana 1: Comparisons
# Día 1: part-to-whole
# Adolescente de 12 a 14 años que no asisten a la escuela
# De acuerdo a los datos el censo 2020 del INEGI

# Cargamos las librerías ####
library(tidyverse)
library(extrafont)

# Leemos y limpiamos los datos ####
# Debido a mi mala conexión a internet, descargué el archivo de datos a mi
# computadora
# Leemos los datos del csv, bajo la codificación utf-8, indicamos que hay un
# fila con los nombres de las columnas,
escolar <- read.csv("data/raw/conjunto_de_datos_iter_00_cpv2020.csv",
  encoding = "UTF-8",
  header = TRUE) %>%
  # Limpiamos los nombres de las variables
  janitor::clean_names() %>%
  # Filtramos para encontrar el total de cada entidad
  filter(nom_loc == "Total de la Entidad") %>%
  # Seleccionamos las variables necesarias. El dataset original tiene 231 variblaes
  select(nom_ent, p12a14noa, p12a14noaf, p12a14noam) %>%
  # Creamos dos nuevas variables utilizando la población femenina y masculina
  # que no asiste a clases, dividido entre el total de esa población.
  mutate(
    Mujeres = as.numeric(p12a14noaf) / as.numeric(p12a14noa) * 100,
    Hombres = as.numeric(p12a14noam) / as.numeric(p12a14noa) * 100
  ) %>%
  # Seleccionamos las variables con las que vamos a trabajr
  select(nom_ent, Mujeres, Hombres) %>%
  # Transformamos los datos de {nom_ent, Mujeres, Hombres} a la forma
  # {nom_ent, genero, porcentaje}, ordenando por -nom_ent
  # pivot_longer(data, columns, names_to = "", values_to = "")
  pivot_longer(., !nom_ent, names_to = "genero", values_to = "porcentaje")


# Creamos el gráfico ####
escolar %>%
  group_by(nom_ent) %>%
  mutate(label_y = cumsum(porcentaje)) %>%
  ggplot(., aes(x = fct_rev(nom_ent), y = porcentaje, fill = genero)) +
  geom_col() +
  geom_text(aes(y = label_y, label = format(porcentaje, digits = 4)), hjust = 2, colour = "white") +
  coord_flip() +
  scale_fill_manual("Género:", values = c("#01BFC4", "#AA2FF7")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_light() +
  labs(
    title = "Adolescente de 12 a 14 años que no asisten a la escuela en México",
    subtitle = "(%) Mujeres y hombres, por entidad federativa",
    y = "%",
    x = "Entidad federativa",
    caption = "Fuente: INEGI: Censo de Población y Vivienda 2020. Elaborado por el Profr. Wilebaldo Santana (@willshDev)\n
Chiapas es la única entidad donde las mujeres asisten menos que los hombres."
  ) +
  theme(
    plot.title = element_text(hjust = 0, size = 25, face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 20, face = "italic"),
    plot.caption = element_text(hjust = 0, size = 10, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(vjust = 0.5),
    text = element_text("Tahoma", size = 20)
  ) +
  geom_hline(yintercept = 50, size = .5, linetype = "dotted")

# Guardamos el gráfico ####
ggsave("reports/figures/day_01.png", height = 10, width = 20)
