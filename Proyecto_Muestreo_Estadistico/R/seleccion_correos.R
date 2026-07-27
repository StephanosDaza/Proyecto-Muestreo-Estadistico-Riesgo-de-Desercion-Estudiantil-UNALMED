#===========================================================
# SELECCIÓN DE LA MUESTRA
# Muestreo Aleatorio Estratificado
#===========================================================

library(readr)
library(dplyr)
library(writexl)

#-----------------------------------------------------------
# Semilla para garantizar reproducibilidad
#-----------------------------------------------------------

set.seed(20261)

#-----------------------------------------------------------
# Cargar marco muestral
#-----------------------------------------------------------

marco <- read_csv(
  "data/processed/marco_muestral.csv",
  show_col_types = FALSE
)

#-----------------------------------------------------------
# Tamaños muestrales por estrato
#-----------------------------------------------------------

n_local   <- 262
n_foraneo <- 260

#-----------------------------------------------------------
# Selección aleatoria dentro de cada estrato
#-----------------------------------------------------------

muestra_local <-
  marco %>%
  filter(estrato_mae == "Local") %>%
  slice_sample(n = n_local)

muestra_foraneo <-
  marco %>%
  filter(estrato_mae == "Foráneo") %>%
  slice_sample(n = n_foraneo)

#-----------------------------------------------------------
# Unir muestra completa
#-----------------------------------------------------------

muestra <-
  bind_rows(
    muestra_foraneo,
    muestra_local
  ) %>%
  mutate(
    ID_Encuesta = sprintf("ID_%03d", row_number())
  ) %>%
  relocate(ID_Encuesta)

#-----------------------------------------------------------
# Verificación
#-----------------------------------------------------------

muestra %>%
  count(estrato_mae)

#-----------------------------------------------------------
# Exportar muestra
#-----------------------------------------------------------

write_xlsx(
  muestra,
  "data/processed/muestra_contacto.xlsx"
)

cat("Muestra seleccionada correctamente.\n")