# ==========================================================
# 02_estudio_previo.R
#
# Construcción del estudio previo para la estimación de la
# proporción de estudiantes que cancelan el semestre.
#
# Entrada:
#   data/raw/matriculados_2025_1.xlsx
#   data/raw/cancelaciones_2025_1.xlsx
#
# Salida:
#   data/processed/estudio_previo.csv
#
# Descripción:
#   - Conserva únicamente estudiantes de pregrado.
#   - Clasifica los estudiantes en los estratos Local y
#     Foráneo.
#   - Identifica los estudiantes con cancelaciones
#     aprobadas.
#   - Construye una variable binaria que indica si el
#     estudiante canceló o no el semestre.
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
    MUNICIPIO_PROCEDENCIA,
    DEPARTAMENTO_COLEGIO,
    MUNICIPIO_COLEGIO
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
      str_trim(str_to_upper(MUNICIPIO_PROCEDENCIA)),
    
    DEPARTAMENTO_COLEGIO =
      str_trim(str_to_upper(DEPARTAMENTO_COLEGIO)),
    
    MUNICIPIO_COLEGIO =
      str_trim(str_to_upper(MUNICIPIO_COLEGIO))
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

matriculados <- matriculados %>%
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

matriculados <- matriculados %>%
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
# Revisión manual de registros sin clasificar
# ----------------------------------------------------------
#
# Se identificaron dos registros que no pudieron
# clasificarse automáticamente.
#
# Caso 1:
# Mediante la revisión del nombre y la dirección del
# establecimiento educativo se determinó que el estudiante
# proviene de Puerto Nare (Antioquia), municipio que no
# pertenece al Valle de Aburrá. Por tanto, se clasifica
# como Foráneo.
#
# Caso 2:
# El registro no contenía información del establecimiento
# educativo ni del municipio o departamento de procedencia.
# Sin embargo, la información de residencia correspondía al
# municipio de Cocorná (Antioquia), el cual se encuentra
# fuera del Valle de Aburrá. En consecuencia, el registro
# también se clasifica como Foráneo.
# ----------------------------------------------------------

matriculados <- matriculados %>%
  mutate(
    estrato_mae = case_when(
      DOCUMENTO %in% c("70385513", "1011512781") ~ "Foráneo",
      TRUE ~ estrato_mae
    )
  )
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