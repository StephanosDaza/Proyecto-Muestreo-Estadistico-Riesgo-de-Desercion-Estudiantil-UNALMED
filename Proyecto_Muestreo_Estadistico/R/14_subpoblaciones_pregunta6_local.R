#===========================================================
# SUBPOBLACIONES
# Pregunta 6
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
# Me sobra dinero
#===========================================================

subpoblacion_1 <-
  
  datos_local %>%
  
  filter(
    
    recursos_alcanzan_gastos ==
      "Me alcanzan para cubrir mis gastos como estudiante, además me sobra dinero."
    
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
# Apenas me alcanza
#===========================================================

subpoblacion_2 <-
  
  datos_local %>%
  
  filter(
    
    recursos_alcanzan_gastos ==
      "Apenas me alcanzan para cubrir mis gastos como estudiante, no me sobra dinero."
    
  )

resultado_sub2 <-
  
  estimar_media_subpoblacion(
    
    datos = subpoblacion_2,
    
    variable = "gasto_mensual_estudiante",
    
    N = N_local,
    
    n_total = n_local
    
  )

#===========================================================
# Subpoblación 3
# No me alcanza
#===========================================================

subpoblacion_3 <-
  
  datos_local %>%
  
  filter(
    
    recursos_alcanzan_gastos ==
      "No me alcanzan para cubrir mis gastos como estudiante."
    
  )

resultado_sub3 <-
  
  estimar_media_subpoblacion(
    
    datos = subpoblacion_3,
    
    variable = "gasto_mensual_estudiante",
    
    N = N_local,
    
    n_total = n_local
    
  )

#===========================================================
# Resultados
#===========================================================

print(resultado_sub1, width = Inf)

print(resultado_sub2, width = Inf)

print(resultado_sub3, width = Inf)

#===========================================================
# Tabla resumen
#===========================================================

bind_rows(
  
  "Me sobra dinero" = resultado_sub1,
  
  "Apenas me alcanza" = resultado_sub2,
  
  "No me alcanza" = resultado_sub3,
  
  .id = "Subpoblacion"
  
)