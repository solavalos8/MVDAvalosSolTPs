
# Importamos librerias

library(tidyverse)   # Manipulación de datos y visualización
library(tidytext)    # Análisis de texto y DTM
library(here)        # Manejo de rutas de archivos


# Definimos rutas

output_dir <- here("TP2", "output")

#Cargamos el archivo generado por processing.R

message("Cargando processed_text.rds...")
comunicados_lemas <- read_rds(file.path(output_dir, "processed_text.rds"))
message("Archivo cargado: ", nrow(comunicados_lemas), " tokens")

# Averiguamos cuales son los 20 terminos mas frecuentes en los comunicados y contamos la frecuencia de aparición.

palabras_frecuentes <- comunicados_lemas |>
  count(lemma, sort = TRUE)

head(palabras_frecuentes, 20)

# A partir de la exploración anterior, seleccionamos 5 términos relevantes en el contexto institucional de la OEA:
  # - "democrático": eje central de la misión de la OEA, refleja su compromiso con la promoción y defensa de la democracia en el continente
  # - "elección": relacionado con las misiones de observación electoral, una de las actividades más frecuentes de la organización
  # - "misión": alude a los despliegues concretos de la OEA en los países miembro para supervisar procesos políticos y electorales
  # - "americano": refleja el alcance hemisférico de la organización y su vínculo con los Estados miembro del continente
  # - "desarrollo": representa uno de los objetivos estratégicos centrales de la OEA, vinculado a la prosperidad integral de la región

terminos_seleccionados <- c("democrático", "elección", "misión", "americano", "desarrollo")