
# PROYECTO MUESTREO ESTADÍSTICO / VULNERABILIDAD ECONÓMICA & RIESGO DE DESERCIÓN ESTUDIANTIL UNALMED
# FASE 1: Diseño Metodológico - Muestreo Aleatorio Estratificado (MAE)

library(readxl)
library(dplyr)

# SECCIÓN 4.1: Carga y Depuración del Marco Muestral (Población)

cat("Por favor, seleccione el archivo de la Base de Datos de Matriculados (Excel)...\n")
ruta_archivo <- file.choose()
datos_originales <- read_excel(ruta_archivo)

# Filtrar para mantener estrictamente a los estudiantes de PREGRADO
poblacion_pregrado <- datos_originales %>%
  filter(TIPO_NIVEL == "PREGRADO")

N_total_pregrado <- nrow(poblacion_pregrado)
cat("Tamaño de la Población Objetivo (N) de PREGRADO:", N_total_pregrado, "\n")


# SECCIÓN 4.2: Estratificación de la Población

# Definición de municipios del Área Metropolitana del Valle de Aburrá
valle_aburra <- c("MEDELLÍN", "MEDELLIN", "BELLO", "ENVIGADO", "ITAGÜÍ", "ITAGUI", 
                  "SABANETA", "LA ESTRELLA", "CALDAS", "COPACABANA", "GIRARDOTA", "BARBOSA")

# Creación de la variable 'Estrato' y homologación de tipo de dato para llaves
poblacion_estratificada <- poblacion_pregrado %>%
  mutate(
    Estrato = case_when(
      DEPARTAMENTO_PROCEDENCIA == "ANTIOQUIA" & MUNICIPIO_PROCEDENCIA %in% valle_aburra ~ "Local",
      is.na(DEPARTAMENTO_PROCEDENCIA) | is.na(MUNICIPIO_PROCEDENCIA) ~ "Desconocido",
      TRUE ~ "Foráneo"
    )
  ) %>%
  mutate(DOCUMENTO = as.character(DOCUMENTO))

cat("\n--- Tamaños Poblacionales por Estrato (N_h) ---\n")
print(table(poblacion_estratificada$Estrato))


# SECCIÓN 4.3: Estimación de Varianzas Históricas (Data Proxy 2025-2)

cat("\nPor favor, seleccione el archivo histórico de Cancelaciones (Excel)...\n")
ruta_cancelaciones <- file.choose()
datos_cancelaciones <- read_excel(ruta_cancelaciones)

cancelaciones_pregrado <- datos_cancelaciones %>%
  filter(TIPO_NIVEL == "PREGRADO") %>%
  mutate(DOCUMENTO = as.character(DOCUMENTO))

# Cruce de bases para identificar cancelaciones según el estrato del estudiante
estudiantes_cruzados <- cancelaciones_pregrado %>%
  inner_join(poblacion_estratificada %>% select(DOCUMENTO, Estrato), by = "DOCUMENTO")

cancelaciones_por_estrato <- table(estudiantes_cruzados$Estrato)

# Cálculo de proporciones empíricas poblacionales
P_L_empirico <- cancelaciones_por_estrato["Local"] / 5465
P_F_empirico <- cancelaciones_por_estrato["Foráneo"] / 5871

cat("\n--- Proporciones Históricas de Cancelación (P_h) ---\n")
cat("P_Local (P_L):", round(P_L_empirico, 4), "\n")
cat("P_Foraneo (P_F):", round(P_F_empirico, 4), "\n\n")


# SECCIÓN 4.4: Modelación Matemática del Tamaño de Muestra (MAE)
# Parámetros Poblacionales Generales
N_L <- 5465   
N_F <- 5871   
N <- N_L + N_F 
Z <- 1.96     

cat("--- RESULTADOS DEL CÁLCULO DE MUESTRA ---\n\n")

# A. Cálculo para la Variable Continua (Gasto Mensual)
B_gasto <- 50000 
var_gasto <- 301035111111 

S_L_gasto <- sqrt(var_gasto)
S_F_gasto <- sqrt(var_gasto)

# Asignación Óptima de Neyman (Continua)
sum_N_S_gasto <- (N_L * S_L_gasto) + (N_F * S_F_gasto)
w_L_gasto <- (N_L * S_L_gasto) / sum_N_S_gasto
w_F_gasto <- (N_F * S_F_gasto) / sum_N_S_gasto

D_gasto <- (B_gasto^2) / (Z^2)

num_gasto <- ((N_L^2 * var_gasto) / w_L_gasto) + ((N_F^2 * var_gasto) / w_F_gasto)
den_gasto <- (N^2 * D_gasto) + (N_L * var_gasto) + (N_F * var_gasto)

n_gasto <- ceiling(num_gasto / den_gasto)
cat("1. Encuestas exigidas por el Gasto (Media):", n_gasto, "\n")

# B. Cálculo para la Variable Dicotómica (Riesgo de Deserción)
B_prop <- 0.01 
P_L <- 0.0152
P_F <- 0.0133

# Varianza para proporciones (P * Q)
var_L_prop <- P_L * (1 - P_L)
var_F_prop <- P_F * (1 - P_F)

S_L_prop <- sqrt(var_L_prop)
S_F_prop <- sqrt(var_F_prop)

# Asignación Óptima de Neyman (Dicotómica)
sum_N_S_prop <- (N_L * S_L_prop) + (N_F * S_F_prop)
w_L_prop <- (N_L * S_L_prop) / sum_N_S_prop
w_F_prop <- (N_F * S_F_prop) / sum_N_S_prop

D_prop <- (B_prop^2) / (Z^2)

num_prop <- ((N_L^2 * var_L_prop) / w_L_prop) + ((N_F^2 * var_F_prop) / w_F_prop)
den_prop <- (N^2 * D_prop) + (N_L * var_L_prop) + (N_F * var_F_prop)

n_prop <- ceiling(num_prop / den_prop)
cat("2. Encuestas exigidas por Deserción (Proporción):", n_prop, "\n\n")


# SECCIÓN 4.5 y 4.6: Criterio de Decisión Operativa y Ajuste por No Respuesta

# Regla del máximo para garantizar cumplimiento de ambas cotas de error
n_efectivo <- max(n_gasto, n_prop)

# Ajuste por Tasa de No Respuesta (TNR) del 15%
TNR <- 0.15
n_ajustado <- ceiling(n_efectivo / (1 - TNR))

cat(">>> TAMAÑO DE MUESTRA EFECTIVO (n_efectivo):", n_efectivo, "<<<\n")
cat(">>> TAMAÑO DE MUESTRA INFLADO (Trabajo de campo):", n_ajustado, "<<<\n")