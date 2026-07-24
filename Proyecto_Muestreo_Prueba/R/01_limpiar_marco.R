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
    MUNICIPIO_PROCEDENCIA
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

marco <- marco %>%
  mutate(
    estrato_mae = case_when(
      DEPARTAMENTO_PROCEDENCIA == "ANTIOQUIA" &
        MUNICIPIO_PROCEDENCIA %in% valle_aburra ~ "Local",
      
      is.na(DEPARTAMENTO_PROCEDENCIA) |
        is.na(MUNICIPIO_PROCEDENCIA) ~ NA_character_,
      
      TRUE ~ "Foráneo"
    )
  )

# ----------------------------------------------------------
# Verificación del estrato
# ----------------------------------------------------------

table(marco$estrato_mae, useNA = "ifany")

marco %>%
  count(estrato_mae)

# ----------------------------------------------------------
# Eliminar registros sin información suficiente para
# determinar el estrato de muestreo
# ----------------------------------------------------------

marco <- marco %>%
  filter(!is.na(estrato_mae))

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