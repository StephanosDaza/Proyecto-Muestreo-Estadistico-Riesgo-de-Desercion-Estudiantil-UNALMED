# ==========================================================
# 08_tamano_muestra_final.R
#
# Selección del tamaño de muestra definitivo del estudio.
#
# Entrada:
#   data/processed/tamano_muestra_prop.csv
#   data/processed/tamano_muestra_media.csv
#
# Salida:
#   data/processed/tamano_muestra_final.csv
#   data/processed/resumen_tamano_muestra.csv
#
# Descripción:
#   - Compara los tamaños de muestra obtenidos para la
#     proporción y la media.
#   - Selecciona el mayor tamaño de muestra.
#   - Guarda la asignación definitiva por estrato y un
#     resumen del diseño muestral.
# ==========================================================

# ----------------------------------------------------------
# Cargar librerías
# ----------------------------------------------------------

library(readr)
library(dplyr)

# ----------------------------------------------------------
# Leer resultados
# ----------------------------------------------------------

prop <- read_csv(
  "data/processed/tamano_muestra_prop.csv",
  show_col_types = FALSE
)

media <- read_csv(
  "data/processed/tamano_muestra_media.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------
# Tamaños globales
# ----------------------------------------------------------

n_prop <- sum(prop$nh)

n_media <- sum(media$nh)

# ----------------------------------------------------------
# Se selecciona el mayor tamaño de muestra
# para garantizar la precisión requerida
# para todas las variables de interés.
# ----------------------------------------------------------

if (n_media >= n_prop) {
  
  n_final <- n_media
  
  diseno_final <- media
  
  variable_control <- "Media"
  
} else {
  
  n_final <- n_prop
  
  diseno_final <- prop
  
  variable_control <- "Proporción"
  
}

stopifnot(sum(diseno_final$nh) == n_final)

# ----------------------------------------------------------
# Mostrar resultados
# ----------------------------------------------------------

cat("\n=====================================\n")
cat("Tamaño de muestra definitivo\n")
cat("=====================================\n")

cat("n (Proporción) =", n_prop, "\n")
cat("n (Media)      =", n_media, "\n")
cat("-------------------------------------\n")
cat("Variable que controla el diseño:", variable_control, "\n")
cat("Tamaño de muestra definitivo =", n_final, "\n\n")

print(diseno_final, width = Inf)

# ----------------------------------------------------------
# Guardar asignación definitiva
# ----------------------------------------------------------

write.csv(
  diseno_final,
  "data/processed/tamano_muestra_final.csv",
  row.names = FALSE
)

# ----------------------------------------------------------
# Guardar resumen del diseño
# ----------------------------------------------------------

resumen_final <- tibble(
  n_proporcion = n_prop,
  n_media = n_media,
  n_final = n_final,
  variable_control = variable_control
)

write.csv(
  resumen_final,
  "data/processed/resumen_tamano_muestra.csv",
  row.names = FALSE
)