
# Instalamos paquetes faltantes y agregamos las librerias necesarias

install.packages(c("rvest", "httr2", "tidytext", "robotstxt", "stopwords")) 

library(tidyverse)  
library(rvest)      
library(httr2)      
library(tidytext)   
library(robotstxt)  
library(here)       
library(xml2)       

# Antes de hacer scraping, verificamos que el sitio lo permita

allowed = paths_allowed(
  paths = "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=4&nAnio=2026",
  bot = "*"
)

cat(
  "Permiso para scrapear:", allowed, "\n"
)

# Crear carpeta /data si no existe ---
message("Verificando existencia de carpeta /data...")
data_dir <- here("TP2", "data")

if (!dir.exists(data_dir)) {
  dir.create(data_dir)
} else {
  cat("'data' ya existe. Se sobrescribirán los archivos HTML.\n")
}

# Parámetros generales para hacer el scraping

oea_url <- "https://www.oas.org"
anio     <- 2026
meses    <- 1:4

# Leemos el HTML de la página y analizamos la estructura
pagina_html <- read_html(oea_url)
pagina_html

# Guardamos el html y registramos cuándo la descargamos

data_dir = here("TP2", "data")

if (!dir.exists(data_dir)) {
  dir.create(data_dir)
} else {
  cat(
    "'data' ya existe. Se sobrescribirá el archivo de la página HTML.\n"
  )
}

# Guardamos en formato rds para uso interno y registro de fecha de descarga

attr(pagina_html, "fecha_descarga") <- Sys.time()

# Guardamos en html para abrir en el navegador si queremos 

write_html(pagina_html, file = file.path(data_dir, "pagina_noticias_gv.html"))


# Creamos funcion 1: extrae títulos y links de una página de listado mensual

scrapear_listado_oea <- function(mes, anio) {
    
    Sys.sleep(3)
    
    base_url <- "https://www.oas.org"
    
    url <- paste0(
      base_url,
      "/es/centro_noticias/comunicados_prensa.asp?nMes=",
      mes, "&nAnio=", anio
    )
  
  message("\nScrapeando listado mes: ", mes, "/", anio)
  
  # Leemos el HTML de la página de listado
  
  html <- read_html(url)
  
  # Guardamos el HTML crudo con registro de fecha de descarga
  
  attr(html, "fecha_descarga") <- Sys.time()
  write_html(
    html,
    file.path(data_dir, paste0("listado_mes", mes, "_", anio, ".html"))
  )
  message("HTML guardado: listado_mes", mes, "_", anio, ".html")
  
  # Extraemos títulos y links juntos desde el selector "td a"

  nodos           <- html |> html_elements("td a")
  titulos         <- nodos |> html_text2() |> str_trim()
  links_relativos <- nodos |> html_attr("href")
  
  # Filtramos entradas con título muy corto (navegación, etc.)
  
  validos         <- nchar(titulos) > 20
  titulos         <- titulos[validos]
  links_relativos <- links_relativos[validos]
  
  # Construimos URLs absolutas
  
  links_absolutos <- paste0(base_url, "/es/centro_noticias/", links_relativos)
  
  message("Comunicados encontrados en mes ", mes, ": ", length(links_absolutos))
  
  # Retornamos un tibble con títulos, links y mes
  
  tibble(
    titulo = titulos,
    url    = links_absolutos,
    mes    = mes
  )
}

# Creamos la funcion 2: Recibe la URL de un comunicado y devuelve su texto

extraer_cuerpo_comunicado <- function(url) {
  
  # Pausa para respetar el crawl-delay indicado en robots.txt
  
  Sys.sleep(3)
  
  # Descarga con manejo de errores para que el loop no se corte si un comunicado individual falla
  html_comunicado <- tryCatch(
    read_html(url),
    error = function(e) {
      message("  ERROR al descargar: ", url)
      return(NULL)
    }
  )
  
  # Si falló la descarga, devolvemos NA
  
  if (is.null(html_comunicado)) return(NA_character_)
  
  # Extraemos los párrafos con el selector "p"
  cuerpo <- html_comunicado |>
    html_elements("p") |>
    html_text2() |>
    str_trim()
  
  # Filtramos párrafos muy cortos (navegación, pie de página, etc.)
  
  cuerpo <- cuerpo[nchar(cuerpo) > 20]
  
  # Concatenamos todos los párrafos en un solo string
  
  cuerpo <- str_c(cuerpo, collapse = " ")
  
  # Limpiamos caracteres especiales
  
  cuerpo <- str_replace_all(cuerpo, "[\\r\\n\\t]+", " ")
  cuerpo <- str_replace_all(cuerpo, "[\\\"'\u201c\u201d\u2018\u2019\u00ab\u00bb`\u00b4%()]", "")
  cuerpo <- str_squish(cuerpo)  # elimina espacios múltiples
  
  return(cuerpo)
}

# Ahora si, comenzamos a scrapear iterando sobre los 4 meses

message("\n=== Iniciando scraping de listados mensuales ===")

listado_comunicados <- map(meses, scrapear_listado_oea, anio = anio)

# Agregamos un id único por comunicado

listado_comunicados <- bind_rows(listado_comunicados)
listado_comunicados <- listado_comunicados |> mutate(id = row_number())

message("\nTotal de comunicados encontrados: ", nrow(listado_comunicados))

#Extraemos el cuerpo de cada comunicado

message("\n=== Iniciando extracción de cuerpos de comunicados ===")

cuerpos_comunicados <- listado_comunicados |>
  select(id, url) |>
  mutate(cuerpo = map_chr(url, extraer_cuerpo_comunicado)) |>
  select(-url)

# Finalmente, creamos la tabla haciendo un join entre listado y cuerpos

message("\nConstruyendo tabla final...")

tabla_comunicados <- listado_comunicados |>
  left_join(cuerpos_comunicados, by = "id") |>
  select(id, titulo, cuerpo, mes)

message("Total de comunicados en tabla final: ", nrow(tabla_comunicados))

# Guardamos con fecha de descarga como atributo
attr(tabla_comunicados, "fecha_descarga") <- Sys.time()

tabla_comunicados |>
  write_rds(file.path(data_dir, "comunicados_oea.rds"))

message("Tabla guardada en: ", file.path(data_dir, "comunicados_oea.rds"))

  