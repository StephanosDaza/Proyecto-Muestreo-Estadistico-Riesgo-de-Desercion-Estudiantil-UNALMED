# ==========================================================
# 08_tamano_muestra_final.R
# Selección del tamaño de muestra definitivo
# Se toma el mayor tamaño de muestra requerido
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
# Seleccionar el mayor
# ----------------------------------------------------------

if (n_media >= n_prop) {
  
  n_final <- n_media
  
  diseno_final <- media
  
  parametro_control <- "Media"
  
} else {
  
  n_final <- n_prop
  
  diseno_final <- prop
  
  parametro_control <- "Proporción"
  
}

# ----------------------------------------------------------
# Mostrar resultados
# ----------------------------------------------------------

cat("\n=====================================\n")
cat("Tamaño de muestra definitivo\n")
cat("=====================================\n")

cat("n (Proporción) =", n_prop, "\n")
cat("n (Media)      =", n_media, "\n")
cat("-------------------------------------\n")
cat("Parámetro que controla el diseño:", parametro_control, "\n")
cat("Tamaño de muestra definitivo =", n_final, "\n\n")

print(diseno_final, width = Inf)

cat("\nSuma de tamaños por estrato =", sum(diseno_final$nh), "\n")

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
  parametro_control = parametro_control
)

write.csv(
  resumen_final,
  "data/processed/resumen_tamano_muestra.csv",
  row.names = FALSE
)