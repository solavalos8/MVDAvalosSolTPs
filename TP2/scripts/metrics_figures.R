
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

message("Top 20 palabras más frecuentes:")
print(head(palabras_frecuentes, 20))

# Construcción de la DTM

message("\n=== Construyendo Matriz Documento-Término (DTM) ===")

# Contamos frecuencia de cada término por documento con count(id, titulo, lemma)

dtm <- comunicados_lemas |>
  count(id, lemma) |>      # frecuencia de cada término por documento
  arrange(id, desc(n))     # ordenamos por documento y frecuencia

message("DTM construida: ", n_distinct(dtm$id), " documentos y ",
        n_distinct(dtm$lemma), " términos únicos")

# A partir de la exploración anterior, seleccionamos 5 términos relevantes en el contexto institucional de la OEA:
  # - "democrático": eje central de la misión de la OEA, refleja su compromiso con la promoción y defensa de la democracia en el continente
  # - "elección": relacionado con las misiones de observación electoral, una de las actividades más frecuentes de la organización
  # - "misión": alude a los despliegues concretos de la OEA en los países miembro para supervisar procesos políticos y electorales
  # - "americano": refleja el alcance hemisférico de la organización y su vínculo con los Estados miembro del continente
  # - "desarrollo": representa uno de los objetivos estratégicos centrales de la OEA, vinculado a la prosperidad integral de la región

message("\n=== Calculando frecuencia de términos seleccionados ===")

terminos_seleccionados <- c("democrático", "electoral", "misión", 
                            "americano", "desarrollo")

# Filtramos la DTM para quedarnos solo con los 5 términos y sumamos su frecuencia total en todos los documentos

frecuencia_terminos <- dtm |>
  filter(lemma %in% terminos_seleccionados) |>
  group_by(lemma) |>
  summarise(frecuencia_total = sum(n)) |>
  ungroup() |>
  arrange(desc(frecuencia_total))

message("Frecuencias calculadas:")
print(frecuencia_terminos)

# Armamos un grafico de barras

message("\n=== Generando gráfico de barras ===")

grafico <- ggplot(
  frecuencia_terminos, 
  aes(x = frecuencia_total, y = reorder(lemma, frecuencia_total))
) +
  geom_col(fill = "#2171B5", alpha = 0.8) +
  labs(
    title = "Frecuencia de Términos Clave en Comunicados de Prensa OEA",
    subtitle = "Enero - Abril 2026",
    x = "Frecuencia total",
    y = NULL,
    caption = "Fuente: OEA - Centro de Noticias"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x   = element_text(size = 11),
    axis.text.y   = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

# Guardamos la figura en /output

ggsave(
  filename = file.path(output_dir, "frecuencia_terminos.png"),
  plot     = grafico,
  width    = 8,
  height   = 5,
  dpi      = 300
)

message("Figura guardada en: ", file.path(output_dir, "frecuencia_terminos.png"))

