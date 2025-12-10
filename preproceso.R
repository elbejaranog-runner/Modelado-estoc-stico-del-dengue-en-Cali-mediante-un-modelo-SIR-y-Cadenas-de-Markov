###############################################################################
# preprocess.R
# Limpieza y construcción de la serie C_obs a partir de los microdatos SIVIGILA
# Proyecto: Modelado estocástico del dengue en Cali mediante SIR y MCMC
# Autor: Elián Steven Bejarano
###############################################################################

library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)

# ---------------------------------------------------------------------------
# 1. Cargar base de datos
# ---------------------------------------------------------------------------

df_raw <- read_excel("Datos_2024_210.xlsx")

# ---------------------------------------------------------------------------
# 2. Filtrar Cali por códigos o texto (robusto)
# ---------------------------------------------------------------------------

df_cali <- df_raw %>%
  filter(
    COD_MUN_O == "76001" |
      COD_MUN_R == "76001" |
      COD_MUN_N == "76001" |
      grepl("CALI", Municipio_ocurrencia,  ignore.case = TRUE) |
      grepl("CALI", Municipio_residencia,  ignore.case = TRUE) |
      grepl("CALI", Municipio_notificacion, ignore.case = TRUE)
  )

# ---------------------------------------------------------------------------
# 3. Filtrar solo casos confirmados
# ---------------------------------------------------------------------------

df_cali_conf <- df_cali %>%
  filter(confirmados == 1)

# ---------------------------------------------------------------------------
# 4. Construcción de fecha y semana epidemiológica
# ---------------------------------------------------------------------------

df_cali_conf <- df_cali_conf %>%
  mutate(
    fecha = case_when(
      !is.na(INI_SIN) ~ as.Date(INI_SIN),
      !is.na(FEC_NOT) ~ as.Date(FEC_NOT),
      TRUE ~ NA_Date_
    ),
    semana = epiweek(fecha)
  )

# ---------------------------------------------------------------------------
# 5. Crear serie semanal completa
# ---------------------------------------------------------------------------

ts_cali <- df_cali_conf %>%
  count(semana) %>%
  complete(semana = 1:52, fill = list(n = 0)) %>%
  arrange(semana)

C_obs <- ts_cali$n
T <- length(C_obs)

# ---------------------------------------------------------------------------
# 6. Guardar objetos procesados
# ---------------------------------------------------------------------------

save(df_cali_conf, ts_cali, C_obs, T,
     file = "datos/datos_procesados.RData")

message("✔ Datos procesados y guardados en datos/datos_procesados.RData")
