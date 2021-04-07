# Reto: 30DayChartChallenge ####
# Semana 1: Comparisons
# Día 4: Magical
# Los más grandes magos e ilusionistas de la época moderna

# Cargamos las librerías ####
library(lares)
library(ggplot2)
library(tidyverse)

magos <- read_csv("data/interim//magician.csv") %>% 
  mutate(end = case_when(
    end == "today" ~ as.character(Sys.Date()),
    TRUE ~ end
  ))

plot_timeline(
  event = magos$name,
  start = magos$start,
  end = magos$end,
  label = magos$nationality,
  group = magos$activity,
  save = TRUE,
  title = "Grandes magos e ilusionistas de la historia",
  subtitle = "Elaborado por Will Santana (@willshDev)",
  subdir = "reports/figures/"
)

file.rename("reports/figures/cv_timeline.png", "reports/figures/day_04.png")
