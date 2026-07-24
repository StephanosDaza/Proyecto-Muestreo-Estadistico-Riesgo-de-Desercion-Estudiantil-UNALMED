# ==========================================================
# 04_tamano_muestra_prop.R
# Tamaño de muestra para estimar una proporción en MAE
# Afijación óptima de Neyman (costos iguales)
# ==========================================================

# ----------------------------------------------------------
# Cargar librerías
# ----------------------------------------------------------

library(readr)
library(dplyr)

# ----------------------------------------------------------
# Leer resumen del estudio previo
# ----------------------------------------------------------

resumen <- read_csv(
  "data/processed/resumen_estudio_previo.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------
# Parámetros del diseño
# ----------------------------------------------------------

# Nivel de confianza (aproximación utilizada en clase)

Z <- 2

# Error máximo absoluto permitido para la proporción.

B <- 0.02

# Cantidad D de la fórmula

D <- (B^2) / (Z^2)

# ----------------------------------------------------------
# Tamaño poblacional
# ----------------------------------------------------------

N <- sum(resumen$Nh)

# ----------------------------------------------------------
# Afijación óptima de Neyman
# (costos iguales)
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

n <- numerador / denominador

# Redondear hacia arriba

n <- ceiling(n)

# ----------------------------------------------------------
# Tamaño de muestra por estrato
# ----------------------------------------------------------

resumen <- resumen %>%
  mutate(
    
    nh_teorico = n * w,
    
    nh = round(nh_teorico)
    
  )

# ----------------------------------------------------------
# Ajuste para que la suma sea exactamente n
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
cat("Tamaño global de muestra\n")
cat("=====================================\n")

cat("N =", N, "\n")
cat("Z =", Z, "\n")
cat("B =", B, "\n")
cat("D =", D, "\n")
cat("n =", n, "\n\n")

print(resumen)

cat("\nSuma de tamaños por estrato =", sum(resumen$nh), "\n")

# ----------------------------------------------------------
# Guardar resultados
# ----------------------------------------------------------

write.csv(
  resumen,
  "data/processed/tamano_muestra_prop.csv",
  row.names = FALSE
)