# ==========================================================
# 05_limpiar_encuesta.R
#
# Limpieza y preparación de la base de datos obtenida a
# partir de la encuesta piloto.
#
# Entrada:
#   data/raw/encuesta.xlsx
#   data/processed/marco_muestral.csv
#
# Salida:
#   data/processed/encuesta_limpia.csv
#
# Descripción:
#   - Renombra las variables de la encuesta.
#   - Estandariza los correos institucionales.
#   - Convierte la variable gasto mensual a formato numérico.
#   - Elimina registros incompletos y duplicados.
#   - Vincula la encuesta con el marco muestral para
#     incorporar el estrato de muestreo.
# ==========================================================

# ----------------------------------------------------------
# Cargar librerías
# ----------------------------------------------------------

library(readxl)
library(dplyr)
library(stringr)

# ----------------------------------------------------------
# Leer encuesta
# ----------------------------------------------------------

encuesta <- read_excel("data/raw/encuesta.xlsx")

# ----------------------------------------------------------
# Renombrar variables
# ----------------------------------------------------------

names(encuesta) <- c(
  "fecha",
  "CORREO",
  "fuente_financiacion",
  "horas_trabajo",
  "alojamiento",
  "apoyo_economico",
  "recursos_suficientes",
  "gasto_mensual",
  "afectacion_rendimiento",
  "considero_cancelar"
)

# ----------------------------------------------------------
# Eliminar variable descartada
# ----------------------------------------------------------

encuesta <- encuesta %>%
  select(-last_col())

# ----------------------------------------------------------
# Limpiar correo institucional
# ----------------------------------------------------------

encuesta <- encuesta %>%
  mutate(
    CORREO = str_trim(str_to_lower(CORREO))
  )

# ----------------------------------------------------------
# Estandarizar la variable gasto mensual
# ----------------------------------------------------------

encuesta <- encuesta %>%
  mutate(
    gasto_mensual = str_replace_all(gasto_mensual, "\\$", ""),
    gasto_mensual = str_replace_all(gasto_mensual, "\\.", ""),
    gasto_mensual = str_replace_all(gasto_mensual, ",", ""),
    gasto_mensual = str_trim(gasto_mensual),
    gasto_mensual = as.numeric(gasto_mensual)
  )

# ----------------------------------------------------------
# Construcción de variables binarias para el análisis
# ----------------------------------------------------------

encuesta <- encuesta %>%
  mutate(
    afectacion_rendimiento_bin = if_else(
      afectacion_rendimiento == "Sí",
      1,
      0
    ),
    
    considero_cancelar_bin = if_else(
      considero_cancelar == "Sí",
      1,
      0
    )
    
  )

# ----------------------------------------------------------
# Eliminar respuestas sin correo
# ----------------------------------------------------------

encuesta <- encuesta %>%
  filter(
    !is.na(CORREO),
    CORREO != ""
  )

# ----------------------------------------------------------
# Eliminar respuestas sin gasto
# ----------------------------------------------------------

encuesta <- encuesta %>%
  filter(!is.na(gasto_mensual))

# ----------------------------------------------------------
# Eliminar respuestas duplicadas
# (Conservar la primera respuesta)
# ----------------------------------------------------------

encuesta <- encuesta %>%
  distinct(CORREO, .keep_all = TRUE)

# ----------------------------------------------------------
# Cargar marco muestral limpio
# ----------------------------------------------------------

marco <- read.csv(
  "data/processed/marco_muestral.csv",
  stringsAsFactors = FALSE
)

# ----------------------------------------------------------
# Normalizar correo del marco
# ----------------------------------------------------------

marco <- marco %>%
  mutate(
    CORREO = str_trim(str_to_lower(CORREO))
  )

# ----------------------------------------------------------
# Verificar el marco muestral
# ----------------------------------------------------------

dim(marco)

count(marco, estrato_mae)

# ----------------------------------------------------------
# Incorporar el estrato de muestreo
# mediante el correo institucional
# ----------------------------------------------------------

encuesta <- encuesta %>%
  left_join(
    marco,
    by = "CORREO"
  )

encuesta %>%
  filter(is.na(estrato_mae))

# ----------------------------------------------------------
# Verificaciones
# ----------------------------------------------------------

cat("\n=====================================\n")
cat("Encuesta piloto\n")
cat("=====================================\n\n")

cat("Número de respuestas:", nrow(encuesta), "\n\n")

cat("Distribución por estrato:\n")
print(table(encuesta$estrato_mae, useNA = "ifany"))

cat("\nRegistros sin identificar en el marco:\n")
print(sum(is.na(encuesta$estrato_mae)))

# ----------------------------------------------------------
# Guardar encuesta limpia
# ----------------------------------------------------------

write.csv(
  encuesta,
  "data/processed/encuesta_limpia.csv",
  row.names = FALSE
)

cat("\nEncuesta limpia guardada en:\n")
cat("data/processed/encuesta_limpia.csv\n")