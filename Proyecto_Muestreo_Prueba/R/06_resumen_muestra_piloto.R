# ==========================================================
# 06_resumen_muestra_piloto.R
# Resumen de la muestra piloto para la variable
# Gasto mensual
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
# Parámetro:
# Tamaño de la muestra piloto
# ----------------------------------------------------------

tam_piloto <- 30

# ----------------------------------------------------------
# Seleccionar muestra piloto
# ----------------------------------------------------------

# Si la encuesta tiene más respuestas que el tamaño piloto,
# se toman únicamente las primeras 'tam_piloto'.

if(nrow(encuesta) > tam_piloto){
  
  set.seed(123)
  encuesta <- encuesta %>%
    slice_sample(n = tam_piloto)
  
}

# ----------------------------------------------------------
# Eliminar registros sin estrato
# ----------------------------------------------------------

encuesta <- encuesta %>%
  filter(!is.na(estrato_mae))

# ----------------------------------------------------------
# Resumen por estrato
# ----------------------------------------------------------

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

tam_estratos <- marco %>%
  count(
    estrato_mae,
    name = "Nh"
  )

N <- sum(tam_estratos$Nh)

tam_estratos <- tam_estratos %>%
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
    
    media = mean(gasto_mensual),
    
    Sh = sd(gasto_mensual),
    
    Sh2 = var(gasto_mensual),
    
    .groups = "drop"
    
  ) %>%
  left_join(
    tam_estratos,
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

print(resumen_piloto)

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