# ==========================================================
# 04_tamano_muestra_prop.R
#
# Cálculo del tamaño de muestra para la estimación de una
# proporción bajo Muestreo Aleatorio Estratificado (MAE),
# utilizando la afijación óptima de Neyman con costos
# iguales entre estratos.
#
# Entrada:
#   data/processed/resumen_estudio_previo.csv
#
# Salida:
#   data/processed/tamano_muestra_prop.csv
#
# Descripción:
#   - Calcula el tamaño global de muestra.
#   - Obtiene la afijación óptima de Neyman.
#   - Determina el tamaño de muestra por estrato.
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
# Verificación del archivo
# ----------------------------------------------------------

dim(resumen)

resumen

# ----------------------------------------------------------
# Parámetros del diseño
# ----------------------------------------------------------

# Nivel de confianza
# En el curso se utiliza la aproximación Z = 2

Z <- 2

# Error máximo absoluto permitido para la estimación
B <- 0.02

# Constante utilizada en la expresión del tamaño de muestra
D <- (B^2)/(Z^2)

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
    peso_neyman = (Nh * Sh)/sum(Nh * Sh)
  )

# ----------------------------------------------------------
# Tamaño global de muestra
# ----------------------------------------------------------

# Numerador de la expresión del tamaño de muestra

numerador <-
  sum((resumen$Nh^2 * resumen$Sh2) / resumen$peso_neyman)

denominador <-
  (N^2 * D) +
  sum(resumen$Nh * resumen$Sh2)

# Denominador de la expresión

n <- numerador / denominador

n

# Redondear hacia arriba

n <- ceiling(n)

# ----------------------------------------------------------
# Tamaño de muestra por estrato
# ----------------------------------------------------------

resumen <- resumen %>%
  mutate(
    
    nh_teorico = n * peso_neyman,
    
    nh = round(nh_teorico)
    
  )

# ----------------------------------------------------------
# Ajuste para que la suma sea exactamente n
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
cat("Tamaño global de muestra\n")
cat("=====================================\n")

cat("N =", N, "\n")
cat("Z =", Z, "\n")
cat("B =", B, "\n")
cat("D =", D, "\n")
cat("n =", n, "\n\n")

print(resumen, width = Inf)

cat("\nSuma de tamaños por estrato =", sum(resumen$nh), "\n")

# ----------------------------------------------------------
# Guardar resultados
# ----------------------------------------------------------

write.csv(
  resumen,
  "data/processed/tamano_muestra_prop.csv",
  row.names = FALSE
)