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
# Lectura de las bases de datos
# ----------------------------------------------------------

matriculados <- read_excel("data/raw/matriculados_2025_1.xlsx")

matriculados <- matriculados %>%
  filter(TIPO_NIVEL == "PREGRADO")

cancelaciones <- read_excel("data/raw/cancelaciones_2025_1.xlsx")

cancelaciones <- cancelaciones %>%
  filter(TIPO_NIVEL == "PREGRADO")

# ----------------------------------------------------------
# Selección de variables de interés
# ----------------------------------------------------------

matriculados <- matriculados %>%
  select(
    DOCUMENTO,
    DEPARTAMENTO_PROCEDENCIA,
    MUNICIPIO_PROCEDENCIA
  ) %>%
  mutate(
    DOCUMENTO = as.character(DOCUMENTO)
  )

cancelaciones <- cancelaciones %>%
  mutate(
    DOCUMENTO = as.character(DOCUMENTO)
  )
# ----------------------------------------------------------
# Normalizar texto
# ----------------------------------------------------------

matriculados <- matriculados %>%
  mutate(
    DEPARTAMENTO_PROCEDENCIA =
      str_trim(str_to_upper(DEPARTAMENTO_PROCEDENCIA)),
    
    MUNICIPIO_PROCEDENCIA =
      str_trim(str_to_upper(MUNICIPIO_PROCEDENCIA))
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

matriculados <- matriculados %>%
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
# Eliminar registros sin información suficiente
# ----------------------------------------------------------

matriculados <- matriculados %>%
  filter(!is.na(estrato_mae))

# ----------------------------------------------------------
# Conservar únicamente cancelaciones aprobadas
# ----------------------------------------------------------

cancelaciones <- cancelaciones %>%
  filter(ESTADO == "APROBADA")

# ----------------------------------------------------------
# Conservar únicamente el documento
# ----------------------------------------------------------

cancelaciones <- cancelaciones %>%
  select(DOCUMENTO) %>%
  distinct() %>%
  mutate(cancelo = 1)

# ----------------------------------------------------------
# Construcción del estudio previo
# ----------------------------------------------------------

estudio_previo <- matriculados %>%
  left_join(cancelaciones, by = "DOCUMENTO")

# ----------------------------------------------------------
# Reemplazar NA por 0
# ----------------------------------------------------------

estudio_previo <- estudio_previo %>%
  mutate(
    cancelo = if_else(is.na(cancelo), 0, cancelo)
  )

# ----------------------------------------------------------
# Verificaciones
# ----------------------------------------------------------

dim(estudio_previo)

table(estudio_previo$estrato_mae)

table(estudio_previo$cancelo)

table(estudio_previo$estrato_mae,
      estudio_previo$cancelo)

estudio_previo %>%
  count(estrato_mae, cancelo)

# ----------------------------------------------------------
# Guardar estudio previo
# ----------------------------------------------------------

write.csv(
  estudio_previo,
  "data/processed/estudio_previo.csv",
  row.names = FALSE
)