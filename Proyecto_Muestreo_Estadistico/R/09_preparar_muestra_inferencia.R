# ==========================================================
# 09_preparar_muestra_inferencia.R
#
# Selección aleatoria de la muestra definitiva para la
# inferencia bajo MAE.
#
# Se utilizan los tamaños de muestra teóricos obtenidos
# durante el diseño muestral.
# ==========================================================

library(readxl)
library(dplyr)
library(readr)

#----------------------------------------------------------
# Semilla
#----------------------------------------------------------

set.seed(345)

#----------------------------------------------------------
# Leer base consolidada
#----------------------------------------------------------

base <-
  
  read_excel(
    "data/processed/Base_Consolidada_Final.xlsx"
  )


# ----------------------------------------------------------
# Preparación de variables
# ----------------------------------------------------------

base <-
  
  base |>
  
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
# Tamaños teóricos
#----------------------------------------------------------

n_local <- 262

n_foraneo <- 260

#----------------------------------------------------------
# Selección aleatoria por estrato
#----------------------------------------------------------

muestra_local <-
  
  base |>
  
  filter(ORIGEN == "Local") |>
  
  slice_sample(n = n_local)

muestra_foraneo <-
  
  base |>
  
  filter(ORIGEN == "Foráneo") |>
  
  slice_sample(n = n_foraneo)

muestra_final <-
  
  bind_rows(
    muestra_local,
    muestra_foraneo
  )

#----------------------------------------------------------
# Verificación
#----------------------------------------------------------

muestra_final |>
  
  count(ORIGEN)

#----------------------------------------------------------
# Guardar base definitiva
#----------------------------------------------------------

write_csv(
  
  muestra_final,
  
  "data/processed/muestra_inferencia.csv"
  
)