# ==========================================================
# 03_resumen_estudio_previo.R
#
# Resumen descriptivo del estudio previo utilizado para la
# estimación del tamaño de muestra de una proporción bajo
# Muestreo Aleatorio Estratificado (MAE).
#
# Entrada:
#   data/processed/estudio_previo.csv
#
# Salida:
#   data/processed/resumen_estudio_previo.csv
#
# Descripción:
#   - Calcula el tamaño de cada estrato.
#   - Estima la proporción de estudiantes que cancelaron
#     el semestre en cada estrato.
#   - Calcula la proporción complementaria, la varianza,
#     la desviación estándar y el peso de cada estrato.
# ==========================================================

#install.packages("readr")
#install.packages("dplyr")

# -----------------------------
# Cargar librerías
# -----------------------------

library(readr)
library(dplyr)

# ----------------------------------------------------------
# Leer estudio previo
# ----------------------------------------------------------

estudio_previo <- read_csv("data/processed/estudio_previo.csv")

# ----------------------------------------------------------
# Verificación del estudio previo
# ----------------------------------------------------------

dim(estudio_previo)

count(estudio_previo, estrato_mae)

# ----------------------------------------------------------
# Resumen por estrato
# ----------------------------------------------------------

resumen_estudio_previo <- estudio_previo %>%
  group_by(estrato_mae) %>%
  summarise(
    
    # Tamaño del estrato
    Nh = n(),
    
    # Proporción de cancelación
    Ph = mean(cancelo),
    
    # Proporción de no cancelación
    Qh = 1 - Ph,
    
    # Varianza de la variable indicadora
    Sh2 = var(cancelo),
    
    # Desviación estándar
    Sh = sd(cancelo)
    
  )

# ----------------------------------------------------------
# Peso de cada estrato
# ----------------------------------------------------------

N <- sum(resumen_estudio_previo$Nh)

resumen_estudio_previo <- resumen_estudio_previo %>%
  mutate(
    Wh = Nh / N
  )

# ----------------------------------------------------------
# Reordenar columnas
# ----------------------------------------------------------

resumen_estudio_previo <- resumen_estudio_previo %>%
  select(
    estrato_mae,
    Nh,
    Wh,
    Ph,
    Qh,
    Sh2,
    Sh
  )

# ----------------------------------------------------------
# Mostrar resultados
# ----------------------------------------------------------

resumen_estudio_previo

sum(resumen_estudio_previo$Wh)

# ----------------------------------------------------------
# Guardar resultados
# ----------------------------------------------------------

write.csv(
  resumen_estudio_previo,
  "data/processed/resumen_estudio_previo.csv",
  row.names = FALSE
)