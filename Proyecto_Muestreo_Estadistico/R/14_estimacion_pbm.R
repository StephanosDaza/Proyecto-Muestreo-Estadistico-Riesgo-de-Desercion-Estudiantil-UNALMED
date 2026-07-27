# ==========================================================
# 14_estimacion_pbm.R
# Estimación de la media del PBM poblacional bajo MAE
# para validación de la representatividad de la muestra.
# ==========================================================

# ----------------------------------------------------------
# Librerías
# ----------------------------------------------------------

library(readr)
library(dplyr)

# ----------------------------------------------------------
# Funciones
# ----------------------------------------------------------

source("R/funciones_mae.R")

# ----------------------------------------------------------
# Lectura de la base de datos
# ----------------------------------------------------------

# Cargar la muestra de inferencia (que ya trae el PBM)
base <- read_csv(
  "data/processed/muestra_inferencia.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------
# Resumen descriptivo preliminar del PBM
# ----------------------------------------------------------

resumen_descriptivo <- base |>
  group_by(ORIGEN) |>
  summarise(
    minimo = min(PBM, na.rm = TRUE),
    q1 = quantile(PBM, 0.25, na.rm = TRUE),
    mediana = median(PBM, na.rm = TRUE),
    media = mean(PBM, na.rm = TRUE),
    q3 = quantile(PBM, 0.75, na.rm = TRUE),
    maximo = max(PBM, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nResumen Descriptivo del PBM en la muestra:\n")
print(resumen_descriptivo)

# ----------------------------------------------------------
# Tamaños poblacionales
# ----------------------------------------------------------

tam_estratos <- tibble(
  ORIGEN = c(
    "Local",
    "Foráneo"
  ),
  Nh = c(
    5469,
    5881
  ),
  nh = c(
    262,
    260
  )
)

# ----------------------------------------------------------
# Verificación de consistencia
# ----------------------------------------------------------

stopifnot(
  all(
    tam_estratos$ORIGEN %in% unique(base$ORIGEN)
  )
)

# ----------------------------------------------------------
# Estimación del PBM bajo MAE
# ----------------------------------------------------------

resultado_pbm <- estimar_media_mae(
  datos = base,
  variable = PBM,
  estrato = ORIGEN,
  tam_estratos = tam_estratos,
  z = 2
)

# ----------------------------------------------------------
# Redondeo de resultados
# ----------------------------------------------------------

resumen_global <- resultado_pbm$resumen_global |>
  mutate(
    across(
      everything(),
      ~ round(.x, 4)
    )
  )

resumen_estratos <- resultado_pbm$resumen_estratos |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )

cat("\n=======================================================\n")
cat("Estimación Global del PBM\n")
cat("=======================================================\n")
print(resumen_global, width = Inf)

cat("\n=======================================================\n")
cat("Estimación por Estratos\n")
cat("=======================================================\n")
print(resumen_estratos, width = Inf)

# ----------------------------------------------------------
# Exportar resultados
# ----------------------------------------------------------

write_csv(
  resumen_global,
  "data/results/estimacion_pbm.csv"
)

write_csv(
  resumen_estratos,
  "data/results/resumen_pbm_estratos.csv"
)

cat("\nArchivos exportados exitosamente.\n")


# Calcular el verdadero parámetro poblacional
marco$PBM <- as.numeric(marco$PBM)
pbm_poblacional_real <- mean(marco$PBM, na.rm = TRUE)
cat("\nEl PBM promedio REAL del marco poblacional es:", round(pbm_poblacional_real, 2), "\n")