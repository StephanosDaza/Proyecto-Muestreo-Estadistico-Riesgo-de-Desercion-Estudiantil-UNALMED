# ==========================================================
# 09_preparar_muestra_inferencia.R
#
# Selección aleatoria de la muestra definitiva para la
# inferencia bajo Muestreo Aleatorio Estratificado (MAE).
#
# A partir de la base consolidada de respuestas se selecciona
# una muestra aleatoria simple independiente dentro de cada
# estrato, utilizando los tamaños de muestra definidos en el
# diseño muestral.
# ==========================================================

library(readxl)
library(dplyr)
library(readr)

#----------------------------------------------------------
# Semilla para garantizar reproducibilidad
#----------------------------------------------------------

set.seed(345)

#----------------------------------------------------------
# Leer base consolidada
#----------------------------------------------------------

base <-
  
  read_excel(
    "data/processed/Base_Consolidada_Final.xlsx"
  )

#----------------------------------------------------------
# Preparación de variables
#----------------------------------------------------------

base <-
  
  base |>
  
  filter(
    !is.na(ORIGEN)
  ) |>
  
  mutate(
    
    considero_cancelar_semestre =
      case_when(
        
        grepl("^Sí", considero_cancelar_semestre) ~ 1,
        
        grepl("^No", considero_cancelar_semestre) ~ 0,
        
        TRUE ~ NA_real_
        
      ),
    
    afecto_rendimiento_academico =
      case_when(
        
        grepl("^Sí", afecto_rendimiento_academico) ~ 1,
        
        grepl("^No", afecto_rendimiento_academico) ~ 0,
        
        TRUE ~ NA_real_
        
      )
    
  )

#----------------------------------------------------------
# Tamaños de muestra por estrato
#----------------------------------------------------------

n_local <- 262

n_foraneo <- 260

#----------------------------------------------------------
# Selección aleatoria independiente por estrato
#----------------------------------------------------------

base_local <-
  
  base |>
  
  filter(
    ORIGEN == "Local"
  )

base_foraneo <-
  
  base |>
  
  filter(
    ORIGEN == "Foráneo"
  )

stopifnot(
  nrow(base_local) >= n_local,
  nrow(base_foraneo) >= n_foraneo
)

muestra_local <-
  
  base_local |>
  
  slice_sample(
    n = n_local
  )

muestra_foraneo <-
  
  base_foraneo |>
  
  slice_sample(
    n = n_foraneo
  )

#----------------------------------------------------------
# Construcción de la muestra definitiva
#----------------------------------------------------------

muestra_final <-
  
  bind_rows(
    
    muestra_local,
    
    muestra_foraneo
    
  )

#----------------------------------------------------------
# Verificaciones
#----------------------------------------------------------

cat("\nDistribución por estrato:\n")

muestra_final |>
  
  count(ORIGEN) |>
  
  print()

cat("\nTamaño total de la muestra:", nrow(muestra_final), "\n")

stopifnot(
  nrow(muestra_final) ==
    n_local +
    n_foraneo
)

#----------------------------------------------------------
# Guardar muestra definitiva
#----------------------------------------------------------

write_csv(
  
  muestra_final,
  
  "data/processed/muestra_inferencia.csv"
  
)

cat("\nMuestra guardada correctamente.\n")