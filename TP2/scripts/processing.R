
# Agregamos las librerias necesarias para procesar el texto.

library(tidyverse)   
library(tidytext)   
library(udpipe)      
library(stopwords)   
library(here)        


# Cargamos la tabla generada por scraping_oea.R

message("Cargando comunicados_oea.rds...")

data_dir <- here("TP2", "data")
output_dir <- here("TP2", "output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

tabla_comunicados <- read_rds(file.path(data_dir, "comunicados_oea.rds"))

message("Comunicados cargados: ", nrow(tabla_comunicados))

# Paso 1: Limpieza del texto

message("\n=== Iniciando limpieza de texto ===")

tabla_limpia <- tabla_comunicados |>
  mutate(
    # Combinamos titulo y cuerpo en un solo texto 
    texto_completo = str_c(titulo, ". ", cuerpo),
    # Eliminamos saltos de línea, tabulaciones y retornos de carro
    texto_completo = str_replace_all(texto_completo, "[\\r\\n\\t]+", " "),
    # Eliminamos signos de puntuación y caracteres especiales
    texto_completo = str_replace_all(texto_completo, "[[:punct:]]", " "),
    # Eliminamos números
    texto_completo = str_replace_all(texto_completo, "[0-9]+", " "),
    # Eliminamos espacios múltiples
    texto_completo = str_squish(texto_completo),
    # Convertimos a minúsculas
    texto_completo = str_to_lower(texto_completo)
  )

message("Limpieza de texto completada.")

# Paso 2: Lematizamos usando udpipe y luego tokenizamos

message("\n=== Iniciando lematización con udpipe ===")

# Cargamos el modelo de español descargado previamente
m_es <- udpipe_load_model(here("TP2", "spanish-gsd-ud-2.5-191206.udpipe"))

# Lematizamos el texto completo de cada comunicado
comunicados_lemas <- udpipe_annotate(
  m_es,
  x      = tabla_limpia$texto_completo,
  doc_id = tabla_limpia$id
) |>
  as.data.frame() |>
  mutate(id = as.integer(doc_id)) |>
  select(id, lemma, upos)

message("Lematización completada.")

# Agregamos los títulos para referencia (igual que en clase)
comunicados_lemas <- comunicados_lemas |>
  left_join(
    tabla_limpia |> select(id, titulo),
    by = "id"
  )
# Filtramos por categoría gramatical y nos quedamos solo con sustantivos, verbos y adjetivos

message("\n=== Filtrando por categoría gramatical ===")

# Verificamos qué categorías hay
message("Categorías gramaticales encontradas:")
print(unique(comunicados_lemas$upos))

# Filtramos: NOUN = sustantivos, VERB = verbos, ADJ = adjetivos
comunicados_lemas <- comunicados_lemas |>
  filter(upos %in% c("NOUN", "VERB", "ADJ"))

message("Tokens después de filtrar por categoría: ", nrow(comunicados_lemas))

# Paso 3: Eliminamos stopwords

message("\n=== Removiendo stopwords ===")

# Contamos tokens antes de remover stopwords
tokens_antes <- nrow(comunicados_lemas)

# Cargamos stopwords en español
stop_es <- stopwords::stopwords("es")
stop_en <- stopwords::stopwords("en")
stop_words <- tibble(lemma = c(stop_es, stop_en))

# Removemos stopwords, números y palabras de una sola letra
# (igual que en clase con anti_join y filter)
comunicados_lemas <- comunicados_lemas |>
  anti_join(stop_words, by = "lemma") |>
  filter(
    !str_detect(lemma, "^\\d+$"),  # eliminamos tokens que sean solo números
    nchar(lemma) > 2               # eliminamos tokens de 1 o 2 caracteres
  )

# Comparamos antes y después
cat("Tokens antes de eliminar stop words:", tokens_antes, "\n")
cat("Tokens después de eliminar stop words:", nrow(comunicados_lemas), "\n")
cat(
  "Reducción relativa porcentual:",
  round(100 * ((tokens_antes - nrow(comunicados_lemas)) / tokens_antes), 1),
  "%\n"
)

# Guardamos el resultado en un .rds
message("\nGuardando processed_text.rds...")

attr(comunicados_lemas, "fecha_descarga") <- tabla_comunicados |>
  attr("fecha_descarga")

comunicados_lemas |>
  write_rds(file.path(output_dir, "processed_text.rds"))

message("Archivo guardado en: ", file.path(output_dir, "processed_text.rds"))

