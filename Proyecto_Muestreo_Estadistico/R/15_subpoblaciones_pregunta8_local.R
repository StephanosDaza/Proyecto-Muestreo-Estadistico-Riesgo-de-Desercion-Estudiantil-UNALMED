#===========================================================
# SUBPOBLACIONES
# Pregunta 8
# Estrato: Local
#===========================================================

library(readr)
library(dplyr)

# ----------------------------------------------------------
# Funciones
# ----------------------------------------------------------

source("R/funciones_subpoblaciones.R")

# ----------------------------------------------------------
# Base de datos
# ----------------------------------------------------------

muestra_inferencia <- read_csv(
  "data/processed/muestra_inferencia.csv",
  show_col_types = FALSE
)

#-----------------------------------------------------------
# Tamaño poblacional del estrato
#-----------------------------------------------------------

N_local <- 5469

#-----------------------------------------------------------
# Tamaño muestral del estrato
#-----------------------------------------------------------

datos_local <-
  muestra_inferencia %>%
  filter(ORIGEN == "Local")

n_local <- nrow(datos_local)

#===========================================================
# Subpoblación 1
# Sí
#===========================================================

subpoblacion_1 <-
  
  datos_local %>%
  
  filter(
    afecto_rendimiento_academico == "1"
  )

resultado_sub1 <-
  
  estimar_media_subpoblacion(
    
    datos = subpoblacion_1,
    
    variable = "gasto_mensual_estudiante",
    
    N = N_local,
    
    n_total = n_local
    
  )

#===========================================================
# Subpoblación 2
# No
#===========================================================

subpoblacion_2 <-
  
  datos_local %>%
  
  filter(
    afecto_rendimiento_academico == "0"
  )

resultado_sub2 <-
  
  estimar_media_subpoblacion(
    
    datos = subpoblacion_2,
    
    variable = "gasto_mensual_estudiante",
    
    N = N_local,
    
    n_total = n_local
    
  )

#===========================================================
# Resultados
#===========================================================

print(resultado_sub1, width = Inf)

print(resultado_sub2, width = Inf)

#===========================================================
# Tabla resumen
#===========================================================

bind_rows(
  
  "Sí" = resultado_sub1,
  
  "No" = resultado_sub2,
  
  .id = "Subpoblacion"
  
)