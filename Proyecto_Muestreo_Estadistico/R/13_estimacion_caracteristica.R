# ==========================================================
# 13_estimacion_caracteristica.R
# Estimación del total de unidades con la característica
# bajo MAE
# ==========================================================

# ----------------------------------------------------------
# Librerías
# ----------------------------------------------------------

library(readr)
library(dplyr)

# ----------------------------------------------------------
# Funciones
# ----------------------------------------------------------

source("R/funciones_mae.R")

# ----------------------------------------------------------
# Base de datos
# ----------------------------------------------------------

base <- read_csv(
  "data/processed/muestra_inferencia.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------
# Tamaños poblacionales
# ----------------------------------------------------------

tam_estratos <- tibble(
  
  ORIGEN = c(
    "Local",
    "Foráneo"
  ),
  
  Nh = c(
    5469,
    5881
  ),
  
  nh = c(
    262,
    260
  )
  
)

#----------------------------------------------------------
# Verificación
#----------------------------------------------------------

stopifnot(
  
  all(
    
    tam_estratos$ORIGEN %in%
      
      unique(base$ORIGEN)
    
  )
  
)

#----------------------------------------------------------
# Estimación
#----------------------------------------------------------

resultado_caracteristica <-
  
  estimar_caracteristica_mae(
    
    datos = base,
    
    variable = considero_cancelar_semestre,
    
    estrato = ORIGEN,
    
    tam_estratos = tam_estratos,
    
    z = 2
    
  )

#----------------------------------------------------------
# Redondeo
#----------------------------------------------------------

resumen_global <-
  
  resultado_caracteristica$resumen_global |>
  
  mutate(
    
    across(
      everything(),
      ~ round(.x, 4)
    )
    
  )

resumen_estratos <-
  
  resultado_caracteristica$resumen_estratos |>
  
  mutate(
    
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
    
  )

#----------------------------------------------------------
# Resultados
#----------------------------------------------------------

print(resumen_global, width = Inf)

print(resumen_estratos, width = Inf)

#----------------------------------------------------------
# Exportar
#----------------------------------------------------------

write_csv(
  
  resumen_global,
  
  "data/results/estimacion_caracteristica.csv"
  
)

write_csv(
  
  resumen_estratos,
  
  "data/results/resumen_caracteristica_estratos.csv"
  
)