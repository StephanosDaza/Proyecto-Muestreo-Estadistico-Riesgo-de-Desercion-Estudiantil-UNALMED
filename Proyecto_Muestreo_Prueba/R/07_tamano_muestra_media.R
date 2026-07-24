# ==========================================================
# 07_tamano_muestra_media.R
# Tamaño de muestra para estimar una media en MAE
# Afijación óptima de Neyman (costos iguales)
# Controlando Error Máximo Relativo (EMR)
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

# Error máximo relativo (10%)

epsilon <- 0.10

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
# Estimación preliminar de la media poblacional
# usando la muestra piloto estratificada
# ----------------------------------------------------------

mu_piloto <-
  sum(resumen$Wh * resumen$media)

# ----------------------------------------------------------
# Error máximo absoluto equivalente
# ----------------------------------------------------------

B <- epsilon * mu_piloto

# Cantidad D

D <- (B^2) / (Z^2)

# ----------------------------------------------------------
# Afijación óptima de Neyman
# ----------------------------------------------------------

resumen <- resumen %>%
  mutate(
    w = (Nh * Sh) / sum(Nh * Sh)
  )

# ----------------------------------------------------------
# Tamaño global de muestra
# ----------------------------------------------------------

numerador <-
  sum((resumen$Nh^2 * resumen$Sh2) / resumen$w)

denominador <-
  (N^2 * D) +
  sum(resumen$Nh * resumen$Sh2)

n <- ceiling(numerador / denominador)

# ----------------------------------------------------------
# Tamaño por estrato
# ----------------------------------------------------------

resumen <- resumen %>%
  mutate(
    nh_teorico = n * w,
    nh = round(nh_teorico)
  )

# ----------------------------------------------------------
# Ajuste para que sum(nh)=n
# ----------------------------------------------------------

diferencia <- n - sum(resumen$nh)

if(diferencia != 0){
  
  indice <- which.max(resumen$w)
  
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