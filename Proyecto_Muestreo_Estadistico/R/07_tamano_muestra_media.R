# ==========================================================
# 07_tamano_muestra_media.R
#
# Cálculo del tamaño de muestra para la estimación de una
# media bajo Muestreo Aleatorio Estratificado (MAE),
# utilizando la afijación óptima de Neyman con costos
# iguales y controlando el Error Máximo Relativo (EMR).
#
# Entrada:
#   data/processed/resumen_muestra_piloto.csv
#
# Salida:
#   data/processed/tamano_muestra_media.csv
#
# Descripción:
#   - Estima la media poblacional preliminar.
#   - Convierte el EMR en un error absoluto.
#   - Calcula el tamaño global de muestra.
#   - Obtiene la afijación óptima de Neyman.
# ==========================================================

# ----------------------------------------------------------
# Cargar librerías
# ----------------------------------------------------------

library(readr)
library(dplyr)

# ----------------------------------------------------------
# Leer resumen de la muestra piloto
# ----------------------------------------------------------

resumen <- read_csv(
  "data/processed/resumen_muestra_piloto.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------
# Parámetros del diseño
# ----------------------------------------------------------

# Nivel de confianza (aproximación utilizada en clase)

Z <- 2

# Error máximo relativo (8%)

epsilon <- 0.08

# ----------------------------------------------------------
# Tamaño poblacional
# ----------------------------------------------------------

N <- sum(resumen$Nh)

# ----------------------------------------------------------
# Pesos poblacionales
# ----------------------------------------------------------

resumen <- resumen %>%
  mutate(
    Wh = Nh / N
  )

# ----------------------------------------------------------
# Estimación preliminar de la media poblacional mediante
# la media estratificada de la muestra piloto
# ----------------------------------------------------------

mu_piloto <-
  sum(resumen$Wh * resumen$media)

# ----------------------------------------------------------
# Error máximo absoluto equivalente
# B = ε × media preliminar
# ----------------------------------------------------------

B <- epsilon * mu_piloto

# Constante utilizada en la expresión
# del tamaño de muestra

D <- (B^2) / (Z^2)

# ----------------------------------------------------------
# Afijación óptima de Neyman
# ----------------------------------------------------------

resumen <- resumen %>%
  mutate(
    peso_neyman = (Nh * Sh) / sum(Nh * Sh)
  )

# ----------------------------------------------------------
# Tamaño global de muestra
# ----------------------------------------------------------

numerador <-
  sum((resumen$Nh^2 * resumen$Sh2) / resumen$peso_neyman)

denominador <-
  (N^2 * D) +
  sum(resumen$Nh * resumen$Sh2)

n

n <- ceiling(numerador / denominador)

# ----------------------------------------------------------
# Tamaño por estrato
# ----------------------------------------------------------

resumen <- resumen %>%
  mutate(
    nh_teorico = n * peso_neyman,
    nh = round(nh_teorico)
  )

# ----------------------------------------------------------
# Ajuste para que sum(nh)=n
# ----------------------------------------------------------

diferencia <- n - sum(resumen$nh)

if(diferencia != 0){
  
  indice <- which.max(resumen$peso_neyman)
  
  resumen$nh[indice] <-
    resumen$nh[indice] + diferencia
  
}

# ----------------------------------------------------------
# Mostrar resultados
# ----------------------------------------------------------

cat("\n=====================================\n")
cat("Tamaño global de muestra (Media)\n")
cat("=====================================\n")

cat("N =", N, "\n")
cat("Z =", Z, "\n")
cat("Error relativo (ε) =", epsilon, "\n")
cat("Media piloto =", round(mu_piloto,2), "COP\n")
cat("Error absoluto equivalente (B) =", round(B,2), "COP\n")
cat("D =", D, "\n")
cat("n =", n, "\n\n")

print(resumen, width = Inf)

cat("\nSuma de tamaños por estrato =", sum(resumen$nh), "\n")

# ----------------------------------------------------------
# Guardar resultados
# ----------------------------------------------------------

write.csv(
  resumen,
  "data/processed/tamano_muestra_media.csv",
  row.names = FALSE
)