# ==========================================================
# funciones_mae.R
# Funciones para inferencia en Muestreo Aleatorio
# Estratificado (MAE)
# ==========================================================

library(dplyr)
library(rlang)

# ==========================================================
# Funciones auxiliares
# ==========================================================

fpc <- function(Nh, nh){
  
  1 - nh/Nh
  
}

error_estandar <- function(varianza){
  
  sqrt(varianza)
  
}

ic_normal <- function(estimador,
                      ee,
                      z = 2){
  
  tibble(
    
    LI = estimador - z*ee,
    
    LS = estimador + z*ee
    
  )
  
}

# ==========================================================
# Resumen por estrato
# ==========================================================

resumen_estratos <- function(datos,
                             variable,
                             estrato,
                             tam_estratos){
  
  variable <- enquo(variable)
  estrato  <- enquo(estrato)
  
  nombre_estrato <- as_name(estrato)
  
  resumen <-
    
    datos |>
    
    group_by(!!estrato) |>
    
    summarise(
      
      n = n(),
      
      media = mean(!!variable, na.rm = TRUE),
      
      varianza = var(!!variable, na.rm = TRUE),
      
      desviacion = sd(!!variable, na.rm = TRUE),
      
      .groups = "drop"
      
    )
  
  resumen <-
    
    resumen |>
    
    left_join(
      tam_estratos,
      by = setNames("ORIGEN", nombre_estrato)
    )
  
  resumen |>
    
    mutate(
      
      Wh = Nh / sum(Nh),
      
      fh = nh / Nh,
      
      var_media =
        (1 - fh) *
        varianza /
        nh,
      
      ee_media =
        sqrt(var_media)
      
    )
  
}

# ==========================================================
# Estimación de la media
# ==========================================================

estimar_media_mae <- function(datos,
                              variable,
                              estrato,
                              tam_estratos,
                              z = 2){
  
  resumen <-
    
    resumen_estratos(
      
      datos,
      
      {{variable}},
      
      {{estrato}},
      
      tam_estratos
      
    )
  
  N <- sum(resumen$Nh)
  
  media <-
    
    sum(
      
      resumen$Nh *
        resumen$media
      
    ) / N
  
  varianza <-
    
    sum(
      
      resumen$Nh^2 *
        resumen$var_media
      
    ) / N^2
  
  ee <- error_estandar(varianza)
  
  ic <- ic_normal(media, ee, z)
  
  resumen_global <-
    
    tibble(
      
      estimacion = media,
      
      varianza = varianza,
      
      ee = ee,
      
      LI = ic$LI,
      
      LS = ic$LS
      
    )
  
  list(
    
    resumen_global = resumen_global,
    
    resumen_estratos = resumen
    
  )
  
}

# ==========================================================
# Estimación del total
# ==========================================================

estimar_total_mae <- function(datos,
                              variable,
                              estrato,
                              tam_estratos,
                              z = 2){
  
  media <-
    
    estimar_media_mae(
      
      datos,
      
      {{variable}},
      
      {{estrato}},
      
      tam_estratos,
      
      z
      
    )
  
  N <- sum(tam_estratos$Nh)
  
  total <-
    
    N *
    media$resumen_global$estimacion
  
  varianza <-
    
    N^2 *
    media$resumen_global$varianza
  
  ee <- error_estandar(varianza)
  
  ic <- ic_normal(total, ee, z)
  
  resumen_global <-
    
    tibble(
      
      estimacion = total,
      
      varianza = varianza,
      
      ee = ee,
      
      LI = ic$LI,
      
      LS = ic$LS
      
    )
  
  list(
    
    resumen_global = resumen_global,
    
    resumen_estratos = media$resumen_estratos
    
  )
  
}