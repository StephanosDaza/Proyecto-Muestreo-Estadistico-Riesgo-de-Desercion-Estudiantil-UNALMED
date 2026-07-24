# ==========================================================
# 01_marco_muestral.R
#
# Construcción del marco muestral para el Muestreo Aleatorio
# Estratificado (MAE).
#
# Entrada:
#   data/raw/marco.xlsx
#
# Salida:
#   data/processed/marco_muestral.csv
#
# Descripción:
#   - Conserva únicamente estudiantes de pregrado.
#   - Selecciona las variables necesarias para el estudio.
#   - Clasifica automáticamente a los estudiantes como
#     Locales o Foráneos utilizando primero la información
#     de procedencia y, cuando esta es insuficiente,
#     la información del colegio.
#   - Los registros que aún no pueden clasificarse quedan
#     identificados para una revisión manual.
# ==========================================================


#install.packages("readxl")
#install.packages("dplyr")
#install.packages("stringr")

# -----------------------------
# Cargar librerías
# -----------------------------

library(readxl)
library(dplyr)
library(stringr)

# ----------------------------------------------------------
# Lectura de la base institucional
# ----------------------------------------------------------

marco <- read_excel("data/raw/marco.xlsx")

# ----------------------------------------------------------
# Conservar únicamente estudiantes de PREGRADO
# ----------------------------------------------------------

marco <- marco %>%
  filter(TIPO_NIVEL == "PREGRADO")

dim(marco)

# ----------------------------------------------------------
# Selección de variables
# ----------------------------------------------------------

marco <- marco %>%
  select(
    CORREO,
    DOCUMENTO,
    DEPARTAMENTO_PROCEDENCIA,
    MUNICIPIO_PROCEDENCIA,
    DEPARTAMENTO_COLEGIO,
    MUNICIPIO_COLEGIO
  )

marco <- marco %>%
  mutate(
    CORREO = str_to_lower(str_trim(CORREO)),
    DOCUMENTO = as.character(DOCUMENTO)
  )


# ----------------------------------------------------------
# Municipios del Valle de Aburrá
# ----------------------------------------------------------

valle_aburra <- c(
  "MEDELLÍN",
  "MEDELLIN",
  "BELLO",
  "ENVIGADO",
  "ITAGÜÍ",
  "ITAGUI",
  "SABANETA",
  "LA ESTRELLA",
  "CALDAS",
  "COPACABANA",
  "GIRARDOTA",
  "BARBOSA"
)

# ----------------------------------------------------------
# Construcción del estrato de muestreo
# ----------------------------------------------------------

# ----------------------------------------------------------
# Construcción del estrato de muestreo usando la información
# de procedencia
# ----------------------------------------------------------

marco <- marco %>%
  mutate(
    estrato_mae = case_when(
      
      # Local: municipio del Valle de Aburrá
      DEPARTAMENTO_PROCEDENCIA == "ANTIOQUIA" &
        MUNICIPIO_PROCEDENCIA %in% valle_aburra ~ "Local",
      
      # Foráneo: departamento diferente de Antioquia
      !is.na(DEPARTAMENTO_PROCEDENCIA) &
        DEPARTAMENTO_PROCEDENCIA != "ANTIOQUIA" ~ "Foráneo",
      
      # Foráneo: municipio de Antioquia fuera del Valle de Aburrá
      DEPARTAMENTO_PROCEDENCIA == "ANTIOQUIA" &
        !is.na(MUNICIPIO_PROCEDENCIA) &
        !(MUNICIPIO_PROCEDENCIA %in% valle_aburra) ~ "Foráneo",
      
      # No fue posible clasificar
      TRUE ~ NA_character_
    )
  )

# ----------------------------------------------------------
# Intentar clasificar los registros restantes utilizando la
# información del colegio
# ----------------------------------------------------------

marco <- marco %>%
  mutate(
    estrato_mae = case_when(
      
      # Si ya fue clasificado, conservar la clasificación
      !is.na(estrato_mae) ~ estrato_mae,
      
      # Local según el colegio
      DEPARTAMENTO_COLEGIO == "ANTIOQUIA" &
        MUNICIPIO_COLEGIO %in% valle_aburra ~ "Local",
      
      # Foráneo según el departamento del colegio
      !is.na(DEPARTAMENTO_COLEGIO) &
        DEPARTAMENTO_COLEGIO != "ANTIOQUIA" ~ "Foráneo",
      
      # Foráneo según municipio del colegio
      DEPARTAMENTO_COLEGIO == "ANTIOQUIA" &
        !is.na(MUNICIPIO_COLEGIO) &
        !(MUNICIPIO_COLEGIO %in% valle_aburra) ~ "Foráneo",
      
      # Sigue siendo imposible clasificar
      TRUE ~ NA_character_
    )
  )

# ----------------------------------------------------------
# Revisión manual
#
# Se identificó un único registro que no pudo clasificarse
# automáticamente debido a información incompleta. Mediante
# la revisión manual de la dirección registrada ("La Sierra,
# Puerto Nare, Antioquia"), se determinó que el estudiante
# pertenece al estrato Foráneo.
# ----------------------------------------------------------

marco <- marco %>%
  mutate(
    estrato_mae = if_else(
      is.na(estrato_mae),
      "Foráneo",
      estrato_mae
    )
  )

# ----------------------------------------------------------
# Verificación del marco muestral final
# ----------------------------------------------------------

table(marco$estrato_mae)

nrow(marco)

# ----------------------------------------------------------
# Guardar marco muestral limpio
# ----------------------------------------------------------

write.csv(
  marco,
  "data/processed/marco_muestral.csv",
  row.names = FALSE
)

cat("\nMarco muestral guardado en:\n")
cat("data/processed/marco_muestral.csv\n")