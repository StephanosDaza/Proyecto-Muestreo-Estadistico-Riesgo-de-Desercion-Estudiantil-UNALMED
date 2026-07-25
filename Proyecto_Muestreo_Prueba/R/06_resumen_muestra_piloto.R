# ==========================================================
# 06_resumen_muestra_piloto.R
#
# Construcción del resumen estadístico de la muestra piloto
# para la variable Gasto mensual.
#
# Entrada:
#   data/processed/encuesta_limpia.csv
#   data/processed/marco_muestral.csv
#
# Salida:
#   data/processed/resumen_muestra_piloto.csv
#
# Descripción:
#   - Selecciona una muestra piloto de tamaño fijo.
#   - Calcula los tamaños poblacionales por estrato.
#   - Obtiene la media y la varianza del gasto mensual
#     en cada estrato.
# ==========================================================

# ----------------------------------------------------------
# Cargar librerías
# ----------------------------------------------------------

library(readr)
library(dplyr)

# ----------------------------------------------------------
# Leer encuesta limpia
# ----------------------------------------------------------

encuesta <- read_csv(
  "data/processed/encuesta_limpia.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------
# Tamaño de la muestra piloto
# ----------------------------------------------------------

tam_piloto <- 30

# Si la encuesta contiene más de 30 respuestas,
# se selecciona aleatoriamente una muestra piloto
# de tamaño 30.

# ----------------------------------------------------------
# Seleccionar muestra piloto
# ----------------------------------------------------------

if(nrow(encuesta) > tam_piloto) {
  # Selección aleatoria reproducible
  set.seed(123)
  encuesta <- encuesta %>%
    slice_sample(n = tam_piloto)
  
}


# ----------------------------------------------------------
# Leer marco muestral
# ----------------------------------------------------------

marco <- read_csv(
  "data/processed/marco_muestral.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------
# Tamaños poblacionales por estrato
# ----------------------------------------------------------

estratos <- marco %>%
  count(
    estrato_mae,
    name = "Nh"
  )

N <- sum(estratos$Nh)

estratos <- estratos %>%
  mutate(
    Wh = Nh / N
  )

# ----------------------------------------------------------
# Resumen de la muestra piloto
# ----------------------------------------------------------

resumen_piloto <- encuesta %>%
  group_by(estrato_mae) %>%
  summarise(
    
    nh_piloto = n(),
    
    media = mean(gasto_mensual, na.rm = TRUE),
    
    Sh = sd(gasto_mensual, na.rm = TRUE),
    
    Sh2 = var(gasto_mensual, na.rm = TRUE),
    
    .groups = "drop"
    
  ) %>%
  left_join(
    estratos,
    by = "estrato_mae"
  ) %>%
  select(
    estrato_mae,
    Nh,
    Wh,
    nh_piloto,
    media,
    Sh,
    Sh2
  )

# ----------------------------------------------------------
# Verificaciones
# ----------------------------------------------------------

cat("\n=====================================\n")
cat("Resumen de la muestra piloto\n")
cat("=====================================\n\n")

cat("Número de observaciones piloto:",
    sum(resumen_piloto$nh_piloto),
    "\n\n")

print(resumen_piloto, width = Inf)

# ----------------------------------------------------------
# Guardar resultados
# ----------------------------------------------------------

write.csv(
  resumen_piloto,
  "data/processed/resumen_muestra_piloto.csv",
  row.names = FALSE
)

cat("\nResumen guardado en:\n")
cat("data/processed/resumen_muestra_piloto.csv\n")