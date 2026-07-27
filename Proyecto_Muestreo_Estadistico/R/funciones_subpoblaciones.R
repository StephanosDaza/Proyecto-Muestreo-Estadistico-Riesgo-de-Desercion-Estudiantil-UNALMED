#===========================================================
# FUNCIONES PARA ESTIMACIÓN EN SUBPOBLACIONES
# Caso: Nk desconocido
#===========================================================

library(dplyr)

#-----------------------------------------------------------
# Error estándar
#-----------------------------------------------------------

error_estandar <- function(varianza){
  sqrt(varianza)
}

#-----------------------------------------------------------
# Intervalo de confianza
#-----------------------------------------------------------

ic_t <- function(estimacion,
                 ee,
                 gl,
                 confianza = 0.95){
  
  alpha <- 1 - confianza
  
  t <- qt(
    1 - alpha/2,
    df = gl
  )
  
  tibble(
    LI = estimacion - t * ee,
    LS = estimacion + t * ee
  )
  
}

#===========================================================
# MEDIA EN UNA SUBPOBLACIÓN
#===========================================================

estimar_media_subpoblacion <- function(datos,
                                       variable,
                                       N,
                                       n_total,
                                       confianza = 0.95){
  
  # 1. Extraemos la variable
  y <- datos[[variable]]
  
  # 2. Filtramos los valores nulos (NA) para no inflar 'nk' ni dañar la media
  y_validos <- y[!is.na(y)]
  
  nk <- length(y_validos)
  
  # 3. Control de seguridad: Si la subpoblación queda vacía o con 1 dato, 
  # no se puede calcular varianza. Retornamos NAs para evitar que el script se caiga.
  if (nk <= 1) {
    warning(paste("La subpoblación no tiene suficientes datos válidos para:", variable))
    return(tibble(
      Nk_estimado = NA, nk = nk, media = NA, S2 = NA, var_media = NA, 
      ee = NA, cv = NA, cv_porcentaje = NA, LI = NA, LS = NA
    ))
  }
  
  # 4. Cálculos estadísticos con los datos limpios
  media <- mean(y_validos)
  S2 <- var(y_validos)
  
  var_media <- (1 - n_total/N) * S2 / nk
  
  ee <- sqrt(var_media)
  
  # Asumo que ya tienes definida la función ic_t() en otra parte de tu código
  ic <- ic_t(
    media,
    ee,
    nk-1,
    confianza
  )
  
  # 5. Salida en formato tibble
  tibble(
    Nk_estimado = N * nk / n_total,
    nk = nk,
    media = media,
    S2 = S2,
    var_media = var_media,
    ee = ee,
    cv = ee/media,
    cv_porcentaje = 100*ee/media,
    LI = ic$LI,
    LS = ic$LS
  )
}

#===========================================================
# PROPORCIÓN EN UNA SUBPOBLACIÓN
#===========================================================

estimar_proporcion_subpoblacion <- function(datos,
                                            variable,
                                            N,
                                            n_total,
                                            confianza = 0.95){
  
  y <- datos[[variable]]
  
  nk <- length(y)
  
  p <- mean(y)
  
  q <- 1-p
  
  var_p <-
    (1 - n_total/N) *
    (p*q)/(nk-1)
  
  ee <- sqrt(var_p)
  
  ic <- ic_t(
    p,
    ee,
    nk-1,
    confianza
  )
  
  tibble(
    
    Nk_estimado = N * nk / n_total,
    
    nk = nk,
    
    proporcion = p,
    
    var_proporcion = var_p,
    
    ee = ee,
    
    cv = ee/p,
    
    cv_porcentaje = 100*ee/p,
    
    LI = ic$LI,
    
    LS = ic$LS
    
  )
  
}
