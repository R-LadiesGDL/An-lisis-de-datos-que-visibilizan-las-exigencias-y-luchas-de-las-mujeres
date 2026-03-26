## ENDIREH 2021 Este código calcula qué porcentaje de mujeres en México ha 
## vivido al menos una forma de violencia, usando datos de la ENDIREH y respetando su diseño muestral

# =========================
# Violencia total a lo largo de la vida
# =========================

rm(list = ls())
gc()

# Instalar si hace falta
# install.packages("survey")

library(survey)

# Ajusta tu directorio
setwd("U:/Informacion/Downloads")

# Carga la base oficial de INEGI
# Esta base debe contener el objeto TB_SEC_IVaVD
# Sección IV: Violencia a lo largo de la vida y en los últimos 12 meses
load("bd_endireh_2021.RData")

# -------------------------
# 1) Selección de variables
# -------------------------
P7_6  <- paste0("P7_6_", 1:18)    # Escolar, a lo largo de la vida
P7_8  <- paste0("P7_8_", 1:18)    # Escolar, últimos 12 meses
P8_9  <- paste0("P8_9_", 1:19)    # Laboral, a lo largo de la vida
P8_11 <- paste0("P8_11_", 1:19)   # Laboral, últimos 12 meses
P8_8  <- paste0("P8_8_", 1:9)     # Discriminación en últimos 12 meses
P9_1  <- paste0("P9_1_", 1:16)    # Comunitario, a lo largo de la vida
P9_3  <- paste0("P9_3_", 1:16)    # Comunitario, últimos 12 meses
P11_1 <- paste0("P11_1_", 1:20)   # Familiar
P14_1 <- paste0("P14_1_", 1:38)   # Pareja, a lo largo de la vida
P14_1[c(23,24,35:38)] <- paste0(P14_1[c(23,24,35:38)], "AB")
P14_3 <- paste0("P14_3_", 1:38)   # Pareja, últimos 12 meses
P14_3[c(23,24,35:38)] <- paste0(P14_3[c(23,24,35:38)], "AB")

variables <- c(
  "UPM_DIS", "EST_DIS", "FAC_MUJ", "CVE_ENT", "T_INSTRUM",
  "P7_1", "P7_2", P7_6, P7_8,
  "P8_1", "P8_2", "P8_3_1_1", "P8_3_1_2", "P8_3_2_1", "P8_3_2_2", "P8_3_2_3",
  "P8_4", "P8_5", P8_9, P8_11, P8_8,
  P9_1, P9_3, P11_1, "P13_C_1", P14_1, P14_3
)

muj <- TB_SEC_IVaVD[, variables]

# -----------------------------------------
# 2) Construcción del numerador del indicador
#    Mujeres que vivieron al menos un acto
#    de violencia a lo largo de la vida
# -----------------------------------------
muj$vtot_lv_con <- ifelse(
  (
    # Escolar a lo largo de la vida
    muj$P7_6_1 %in% "1"  | muj$P7_6_2 %in% "1"  | muj$P7_6_3 %in% "1"  |
      muj$P7_6_4 %in% "1"  | muj$P7_6_5 %in% "1"  | muj$P7_6_6 %in% "1"  |
      muj$P7_6_7 %in% "1"  | muj$P7_6_8 %in% "1"  | muj$P7_6_9 %in% "1"  |
      muj$P7_6_10 %in% "1" | muj$P7_6_11 %in% "1" | muj$P7_6_12 %in% "1" |
      muj$P7_6_13 %in% "1" | muj$P7_6_14 %in% "1" | muj$P7_6_15 %in% "1" |
      muj$P7_6_16 %in% "1" | muj$P7_6_17 %in% "1" | muj$P7_6_18 %in% "1" |
      
      # Laboral / discriminación
      muj$P8_3_1_1 %in% "1" | muj$P8_3_1_2 %in% "1" |
      muj$P8_3_2_1 %in% "1" | muj$P8_3_2_2 %in% "1" | muj$P8_3_2_3 %in% "1" |
      muj$P8_8_1 %in% "1" | muj$P8_8_2 %in% "1" | muj$P8_8_3 %in% "1" |
      muj$P8_8_4 %in% "1" | muj$P8_8_5 %in% "1" | muj$P8_8_6 %in% "1" |
      muj$P8_8_7 %in% "1" | muj$P8_8_8 %in% "1" | muj$P8_8_9 %in% "1" |
      muj$P8_9_1 %in% "1"  | muj$P8_9_2 %in% "1"  | muj$P8_9_3 %in% "1"  |
      muj$P8_9_4 %in% "1"  | muj$P8_9_5 %in% "1"  | muj$P8_9_6 %in% "1"  |
      muj$P8_9_7 %in% "1"  | muj$P8_9_8 %in% "1"  | muj$P8_9_9 %in% "1"  |
      muj$P8_9_10 %in% "1" | muj$P8_9_11 %in% "1" | muj$P8_9_12 %in% "1" |
      muj$P8_9_13 %in% "1" | muj$P8_9_14 %in% "1" | muj$P8_9_15 %in% "1" |
      muj$P8_9_16 %in% "1" | muj$P8_9_17 %in% "1" | muj$P8_9_18 %in% "1" |
      muj$P8_9_19 %in% "1" |
      
      # Comunitario
      muj$P9_1_1 %in% "1"  | muj$P9_1_2 %in% "1"  | muj$P9_1_3 %in% "1"  |
      muj$P9_1_4 %in% "1"  | muj$P9_1_5 %in% "1"  | muj$P9_1_6 %in% "1"  |
      muj$P9_1_7 %in% "1"  | muj$P9_1_8 %in% "1"  | muj$P9_1_9 %in% "1"  |
      muj$P9_1_10 %in% "1" | muj$P9_1_11 %in% "1" | muj$P9_1_12 %in% "1" |
      muj$P9_1_13 %in% "1" | muj$P9_1_14 %in% "1" | muj$P9_1_15 %in% "1" |
      muj$P9_1_16 %in% "1" |
      
      # Familiar
      muj$P11_1_1 %in% c("1","2","3")  | muj$P11_1_2 %in% c("1","2","3")  |
      muj$P11_1_3 %in% c("1","2","3")  | muj$P11_1_4 %in% c("1","2","3")  |
      muj$P11_1_5 %in% c("1","2","3")  | muj$P11_1_6 %in% c("1","2","3")  |
      muj$P11_1_7 %in% c("1","2","3")  | muj$P11_1_8 %in% c("1","2","3")  |
      muj$P11_1_9 %in% c("1","2","3")  | muj$P11_1_10 %in% c("1","2","3") |
      muj$P11_1_11 %in% c("1","2","3") | muj$P11_1_12 %in% c("1","2","3") |
      muj$P11_1_13 %in% c("1","2","3") | muj$P11_1_14 %in% c("1","2","3") |
      muj$P11_1_15 %in% c("1","2","3") | muj$P11_1_16 %in% c("1","2","3") |
      muj$P11_1_17 %in% c("1","2","3") | muj$P11_1_18 %in% c("1","2","3") |
      muj$P11_1_19 %in% c("1","2","3") | muj$P11_1_20 %in% c("1","2","3") |
      
      # Pareja a lo largo de la vida
      muj$P14_1_1 %in% c("1","2","3")  | muj$P14_1_2 %in% c("1","2","3")  |
      muj$P14_1_3 %in% c("1","2","3")  | muj$P14_1_4 %in% c("1","2","3")  |
      muj$P14_1_5 %in% c("1","2","3")  | muj$P14_1_6 %in% c("1","2","3")  |
      muj$P14_1_7 %in% c("1","2","3")  | muj$P14_1_8 %in% c("1","2","3")  |
      muj$P14_1_9 %in% c("1","2","3")  | muj$P14_1_10 %in% c("1","2","3") |
      muj$P14_1_11 %in% c("1","2","3") | muj$P14_1_12 %in% c("1","2","3") |
      muj$P14_1_13 %in% c("1","2","3") | muj$P14_1_14 %in% c("1","2","3") |
      muj$P14_1_15 %in% c("1","2","3") | muj$P14_1_16 %in% c("1","2","3") |
      muj$P14_1_17 %in% c("1","2","3") | muj$P14_1_18 %in% c("1","2","3") |
      muj$P14_1_19 %in% c("1","2","3") | muj$P14_1_20 %in% c("1","2","3") |
      muj$P14_1_21 %in% c("1","2","3") | muj$P14_1_22 %in% c("1","2","3") |
      muj$P14_1_23AB %in% c("1","2","3") | muj$P14_1_24AB %in% c("1","2","3") |
      muj$P14_1_25 %in% c("1","2","3") | muj$P14_1_26 %in% c("1","2","3") |
      muj$P14_1_27 %in% c("1","2","3") | muj$P14_1_28 %in% c("1","2","3") |
      muj$P14_1_29 %in% c("1","2","3") | muj$P14_1_30 %in% c("1","2","3") |
      muj$P14_1_31 %in% c("1","2","3") | muj$P14_1_32 %in% c("1","2","3") |
      muj$P14_1_33 %in% c("1","2","3") | muj$P14_1_34 %in% c("1","2","3") |
      muj$P14_1_35AB %in% c("1","2","3") | muj$P14_1_36AB %in% c("1","2","3") |
      muj$P14_1_37AB %in% c("1","2","3") | muj$P14_1_38AB %in% c("1","2","3")
  ),
  1, 0
)

# Denominador
muj$pob_muj <- 1

# -------------------------
# 3) Diseño muestral
# -------------------------
disenio <- svydesign(
  id = ~UPM_DIS,
  strata = ~EST_DIS,
  weights = ~FAC_MUJ,
  data = muj,
  nest = TRUE
)

# -------------------------
# 4) Estimación nacional
# -------------------------
n_vtot_lv_con <- svyratio(
  ~vtot_lv_con,
  denominator = ~pob_muj,
  design = disenio,
  na.rm = TRUE
)

# Resultado en porcentaje
est_n_vtot_lv_con <- coef(n_vtot_lv_con)[1] * 100
se_n_vtot_lv_con  <- SE(n_vtot_lv_con)[1] * 100
ic_n_vtot_lv_con  <- confint(n_vtot_lv_con, level = 0.90) * 100

cat("Prevalencia nacional de violencia total a lo largo de la vida:\n")
cat(round(est_n_vtot_lv_con, 1), "%\n")
cat("Error estándar:", round(se_n_vtot_lv_con, 2), "\n")
cat("IC 90%:", round(ic_n_vtot_lv_con[1], 1), "-", round(ic_n_vtot_lv_con[2], 1), "\n")


############################### SESNSP ###################################
library(tidyverse)

# 1. Cargar base de datos
df <- read.csv("U:/Informacion/Downloads/Estatal-Víctimas-2015-2025_feb2026.csv", fileEncoding = "latin1")

# 2. Pasar meses a formato largo
df_long <- df %>%
  pivot_longer(
    cols = Enero:Diciembre,
    names_to = "Mes",
    values_to = "Victimas"
  )

# 3. Filtrar solo mujeres y delitos clave
df_filtrado <- df_long %>%
  filter(
    Sexo == "Mujer",
    Subtipo.de.delito %in% c("Feminicidio", "Homicidio doloso")
  )

# 4. Calcular totales por año y tipo de delito
tabla <- df_filtrado %>%
  group_by(Año, Subtipo.de.delito) %>%
  summarise(total = sum(Victimas, na.rm = TRUE), .groups = "drop") %>%
  
  # Convertir a formato ancho (una columna por delito)
  pivot_wider(
    names_from = Subtipo.de.delito,
    values_from = total,
    values_fill = 0
  ) %>%
  
  # Calcular totales y promedio diario
  mutate(
    total_muertes_violentas = Feminicidio + `Homicidio doloso`,
    asesinadas_dia = round(total_muertes_violentas / 365, 1)
  ) %>%
  
  arrange(Año)

# 5. Ver tabla
print(tabla)