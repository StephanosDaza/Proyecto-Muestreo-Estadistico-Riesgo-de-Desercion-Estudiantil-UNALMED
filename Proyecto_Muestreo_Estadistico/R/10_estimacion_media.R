# ==========================================================
# 10_estimacion_media.R
# Estimación de la media poblacional bajo MAE
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

#----------------------------------------------------------
# Resumen descriptivo
#----------------------------------------------------------

resumen_descriptivo <-
  
  base |>
  
  group_by(ORIGEN) |>
  
  summarise(
    
    minimo = min(gasto_mensual_estudiante),
    
    q1 = quantile(gasto_mensual_estudiante, 0.25),
    
    mediana = median(gasto_mensual_estudiante),
    
    media = mean(gasto_mensual_estudiante),
    
    q3 = quantile(gasto_mensual_estudiante, 0.75),
    
    maximo = max(gasto_mensual_estudiante),
    
    .groups = "drop"
    
  )

print(resumen_descriptivo)

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

resultado_media <-
  
  estimar_media_mae(
    
    datos = base,
    
    variable = gasto_mensual_estudiante,
    
    estrato = ORIGEN,
    
    tam_estratos = tam_estratos,
    
    z = 2
    
  )

#----------------------------------------------------------
# Redondeo de resultados
#----------------------------------------------------------

resumen_global <-
  
  resultado_media$resumen_global |>
  
  mutate(
    
    across(
      everything(),
      ~ round(.x, 4)
    )
    
  )

resumen_estratos <-
  
  resultado_media$resumen_estratos |>
  
  mutate(
    
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
    
  )

print(resumen_global, width = Inf)

print(resumen_estratos, width = Inf)

#----------------------------------------------------------
# Exportar resultados
#----------------------------------------------------------

write_csv(
  
  resumen_global,
  
  "data/results/estimacion_media.csv"
  
)

write_csv(
  
  resumen_estratos,
  
  "data/results/resumen_media_estratos.csv"
  
)