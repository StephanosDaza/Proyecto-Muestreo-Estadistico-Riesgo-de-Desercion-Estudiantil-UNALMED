#===========================================================
# SUBPOBLACIONES
# Pregunta 6
# Estrato: Foráneo
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

N_foraneo <- 5881  

#-----------------------------------------------------------
# Tamaño muestral del estrato
#-----------------------------------------------------------

datos_foraneo <-
  muestra_inferencia %>%
  filter(ORIGEN == "Foráneo")

n_foraneo <- nrow(datos_foraneo)

#===========================================================
# Subpoblación 1
# Me sobra dinero
#===========================================================

subpoblacion_1 <-
  
  datos_foraneo %>%
  
  filter(
    
    recursos_alcanzan_gastos ==
      "Me alcanzan para cubrir mis gastos como estudiante, además me sobra dinero."
    
  )

resultado_sub1 <-
  
  estimar_media_subpoblacion(
    
    datos = subpoblacion_1,
    
    variable = "gasto_mensual_estudiante",
    
    N = N_foraneo,
    
    n_total = n_foraneo
    
  )

#===========================================================
# Subpoblación 2
# Apenas me alcanza
#===========================================================

subpoblacion_2 <-
  
  datos_foraneo %>%
  
  filter(
    
    recursos_alcanzan_gastos ==
      "Apenas me alcanzan para cubrir mis gastos como estudiante, no me sobra dinero."
    
  )

resultado_sub2 <-
  
  estimar_media_subpoblacion(
    
    datos = subpoblacion_2,
    
    variable = "gasto_mensual_estudiante",
    
    N = N_foraneo,
    
    n_total = n_foraneo
    
  )

#===========================================================
# Subpoblación 3
# No me alcanza
#===========================================================

subpoblacion_3 <-
  
  datos_foraneo %>%
  
  filter(
    
    recursos_alcanzan_gastos ==
      "No me alcanzan para cubrir mis gastos como estudiante."
    
  )

resultado_sub3 <-
  
  estimar_media_subpoblacion(
    
    datos = subpoblacion_3,
    
    variable = "gasto_mensual_estudiante",
    
    N = N_foraneo,
    
    n_total = n_foraneo
    
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