# ==========================================================
# 05_limpiar_encuesta.R
# Limpieza de la encuesta piloto
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
  "considero_cancelar",
  "razon_economica"
)

# ----------------------------------------------------------
# Limpiar correo institucional
# ----------------------------------------------------------

encuesta <- encuesta %>%
  mutate(
    CORREO = str_trim(str_to_lower(CORREO))
  )

# ----------------------------------------------------------
# Limpiar gasto mensual
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
# Eliminar respuestas sin correo
# ----------------------------------------------------------

encuesta <- encuesta %>%
  filter(!is.na(CORREO),
         CORREO != "")

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
# Cruzar encuesta con marco muestral
# ----------------------------------------------------------

encuesta <- encuesta %>%
  left_join(
    marco,
    by = "CORREO"
  )

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