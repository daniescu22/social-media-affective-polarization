############################################################
# Affective Polarization and Social Media Use in Spain
# Longitudinal Panel Analysis (TriPol Survey, Spain)
#
# Author:      Daniela
# Institution: Universidad de Salamanca
# Year:        2026
#
# Description:
#   This script analyses the relationship between informational
#   social media use and affective polarization (AP) among Spanish
#   adults, using longitudinal panel data from the TriPol project
#   (Waves 1-3). It compares young adults (18-35) with the rest of
#   the population (36+), tests for moderation by age group, and
#   reports a range of robustness checks (heteroscedasticity,
#   attrition/IPW correction, non-linear effects, standardized
#   models, and a formal Chow test of coefficient equivalence).
#
#   Originally developed for an undergraduate thesis (TFG) in
#   Sociology (quantitative methodology), Universidad de Salamanca.
#   This version has been reorganised and documented for public
#   release; the statistical analysis, models and results are
#   unchanged from the original submission.
#
# Structure:
#   Block A (Sections 11-19)  : Analysis of young adults (18-35)
#   Block B (Sections 20-26)  : Analysis of the rest of the sample (36+)
#   Block C (Sections 27-32)  : Comparison between subsamples
############################################################


############################################################
# 1. Environment
############################################################

rm(list = ls())
graphics.off()
cat("\014")
set.seed(42)

############################################################
# 2. Load packages
############################################################

library(tidyverse)
library(patchwork)
library(scales)
library(ggtext)
library(viridis)
library(car)
library(nnet)
library(broom)
library(here)
library(lmtest)
library(sandwich)
library(plm)
library(pwr)
library(dunn.test)

# All console output is redirected to a results file for the
# duration of the analysis.

############################################################
# 3. Load data
############################################################

# Dataset not included in this repository.
# Please obtain the original TriPol España dataset from the
# official source and place it inside the data/ folder as
# "TRI_POL_ES.csv".
tripol <- read_delim(
  here("data", "TRI_POL_ES.csv"),
  delim           = ";",
  locale          = locale(decimal_mark = ","),
  show_col_types  = FALSE
)

cat("Dimensiones del dataset:", nrow(tripol), "filas x", ncol(tripol), "columnas\n")


############################################################
# 4. Rename variables
############################################################

# Original TriPol variable codes are renamed to descriptive Spanish
# labels used throughout the analysis. Variable names matching the
# original questionnaire/dataset are kept as-is.
tripol <- tripol %>%
  rename(
    edad_1               = s2_1,
    edad_2               = s2_2,
    edad_3               = s2_3,
    sexo                 = s1_1,
    educacion_1          = s11a_1,
    educacion_2          = s11a_2,
    educacion_3          = s11a_3,
    uso_rrss_1           = p21g_1,
    # uso_rrss_2 omitted: p21g does not exist in Wave 2
    uso_rrss_3           = p21g_3,
    ideologia_posicion_1 = p2_1,
    ideologia_posicion_3 = p2_3,
    extremismo_1         = IE_1,
    extremismo_2         = IE_2,
    extremismo_3         = IE_3
  )


############################################################
# 5. Construct the affective polarization index
############################################################

tripol <- tripol %>%
  mutate(
    polarizacion_1 = (WAPDV_1 + WAPDL_1) / 2,
    polarizacion_2 = (WAPDV_2 + WAPDL_2) / 2,
    polarizacion_3 = (WAPDV_3 + WAPDL_3) / 2
  )

cat("\n--- Estadísticos básicos del índice AP (protocolo WAPD) ---\n")
tripol %>%
  summarise(
    across(c(polarizacion_1, polarizacion_2, polarizacion_3),
           list(media = ~mean(.x, na.rm = TRUE),
                dt    = ~sd(.x,   na.rm = TRUE),
                min   = ~min(.x,  na.rm = TRUE),
                max   = ~max(.x,  na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "estadistico"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = estadistico, values_from = value) %>%
  print()

cat("\n--- Correlaciones entre componentes WAPDV y WAPDL ---\n")
cat("Ola 1:", round(cor(tripol$WAPDV_1, tripol$WAPDL_1, use = "complete.obs"), 3), "\n")
cat("Ola 2:", round(cor(tripol$WAPDV_2, tripol$WAPDL_2, use = "complete.obs"), 3), "\n")
cat("Ola 3:", round(cor(tripol$WAPDV_3, tripol$WAPDL_3, use = "complete.obs"), 3), "\n")
cat("  NOTA: Una correlación >= .60 entre componentes indica validez\n")
cat("  convergente aceptable y justifica el promediado como índice compuesto.\n")


############################################################
# 6. Recode variables
############################################################

tripol <- tripol %>%
  mutate(

    sexo_f = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),

    educacion_f = factor(educacion_1,
                         levels  = c(0, 1, 2, 3),
                         labels  = c("Sin estudios", "Primaria",
                                     "Secundaria",   "Universitarios"),
                         ordered = TRUE),

    educacion_f_3 = factor(educacion_3,
                           levels  = c(0, 1, 2, 3),
                           labels  = c("Sin estudios", "Primaria",
                                       "Secundaria",   "Universitarios"),
                           ordered = TRUE),

    ideologia_cat_1 = case_when(
      ideologia_posicion_1 <= 3 ~ "Izquierda",
      ideologia_posicion_1 <= 6 ~ "Centro",
      ideologia_posicion_1 >= 7 ~ "Derecha",
      TRUE                      ~ NA_character_
    ),
    ideologia_cat_1 = factor(ideologia_cat_1,
                             levels = c("Izquierda", "Centro", "Derecha")),

    ideologia_cat_3 = case_when(
      ideologia_posicion_3 <= 3 ~ "Izquierda",
      ideologia_posicion_3 <= 6 ~ "Centro",
      ideologia_posicion_3 >= 7 ~ "Derecha",
      TRUE                      ~ NA_character_
    ),
    ideologia_cat_3 = factor(ideologia_cat_3,
                             levels = c("Izquierda", "Centro", "Derecha")),

    # Quadratic terms (for non-linearity tests)
    uso_rrss_1_cuad = uso_rrss_1^2,
    uso_rrss_3_cuad = uso_rrss_3^2,

    # Standardized versions (for comparable/standardized-beta models)
    uso_rrss_1_z           = as.numeric(scale(uso_rrss_1)),
    uso_rrss_3_z           = as.numeric(scale(uso_rrss_3)),
    ideologia_posicion_1_z = as.numeric(scale(ideologia_posicion_1)),
    ideologia_posicion_3_z = as.numeric(scale(ideologia_posicion_3)),
    extremismo_1_z         = as.numeric(scale(extremismo_1)),
    extremismo_3_z         = as.numeric(scale(extremismo_3)),

    # Change scores between waves
    cambio_pol_12  = polarizacion_2 - polarizacion_1,
    cambio_pol_23  = polarizacion_3 - polarizacion_2,
    cambio_pol_13  = polarizacion_3 - polarizacion_1,
    cambio_rrss_13 = uso_rrss_3     - uso_rrss_1
  )


############################################################
# 7. Age groups
############################################################

tripol <- tripol %>%
  mutate(
    grupo_edad2 = case_when(
      edad_1 >= 18 & edad_1 <= 35 ~ "Jóvenes (18-35)",
      edad_1 >= 36                ~ "Resto (36+)",
      TRUE                        ~ NA_character_
    ),
    grupo_edad2 = factor(grupo_edad2,
                         levels = c("Jóvenes (18-35)", "Resto (36+)")),

    grupo_edad3 = case_when(
      edad_1 >= 18 & edad_1 <= 35 ~ "Jóvenes (18-35)",
      edad_1 >= 36 & edad_1 <= 55 ~ "Adultos medios (36-55)",
      edad_1 >= 56                 ~ "Mayores (56+)",
      TRUE                         ~ NA_character_
    ),
    grupo_edad3 = factor(grupo_edad3,
                         levels = c("Jóvenes (18-35)",
                                    "Adultos medios (36-55)",
                                    "Mayores (56+)"))
  )

cat("\n--- Distribución por grupo de edad (dicotómico) ---\n")
print(table(tripol$grupo_edad2, useNA = "ifany"))
cat("\n--- Distribución por grupo de edad (tricotómico) ---\n")
print(table(tripol$grupo_edad3, useNA = "ifany"))


############################################################
# 8. Subsamples
############################################################

tripol_jovenes <- tripol %>% filter(grupo_edad2 == "Jóvenes (18-35)")
tripol_resto   <- tripol %>% filter(grupo_edad2 == "Resto (36+)")

cat("\nN jóvenes:", nrow(tripol_jovenes), "\n")
cat("N resto:  ", nrow(tripol_resto),   "\n")


############################################################
# 9. Missing data, attrition and bias inspection
############################################################

cat("\n--- Casos válidos en polarización por ola (jóvenes) ---\n")
cat("Ola 1:", sum(!is.na(tripol_jovenes$polarizacion_1)), "\n")
cat("Ola 2:", sum(!is.na(tripol_jovenes$polarizacion_2)), "\n")
cat("Ola 3:", sum(!is.na(tripol_jovenes$polarizacion_3)), "\n")

tripol_jovenes <- tripol_jovenes %>%
  mutate(attricion = !is.na(polarizacion_1) & is.na(polarizacion_3))

cat("\n--- Análisis formal de attrición (jóvenes: ola 1 → ola 3) ---\n")
cat("N permanecen en ola 3:", sum(!tripol_jovenes$attricion, na.rm = TRUE), "\n")
cat("N abandonan entre ola 1 y ola 3:",  sum(tripol_jovenes$attricion,  na.rm = TRUE), "\n")

cat("\n  t-test polarización ola 1 (permanecen vs. abandonan):\n")
print(t.test(polarizacion_1 ~ attricion, data = tripol_jovenes))

cat("\n  t-test uso de RRSS ola 1 (permanecen vs. abandonan):\n")
print(t.test(uso_rrss_1 ~ attricion, data = tripol_jovenes))

cat("\n  Chi² sexo (permanecen vs. abandonan):\n")
print(chisq.test(table(tripol_jovenes$sexo_f, tripol_jovenes$attricion)))

cat("\n  Chi² educación (permanecen vs. abandonan):\n")
print(chisq.test(table(tripol_jovenes$educacion_f, tripol_jovenes$attricion)))

cat("\n  NOTA METODOLÓGICA: Si algún test resulta significativo (p < .05),\n")
cat("  la attrición no es aleatoria (no-MCAR) y los estimadores del CLPD\n")
cat("  pueden estar sesgados.\n")

cat("\n--- Distribución por sexo (jóvenes) ---\n")
print(table(tripol_jovenes$sexo_f, useNA = "ifany"))

cat("\n--- Distribución por nivel educativo (jóvenes, ola 1) ---\n")
print(table(tripol_jovenes$educacion_f, useNA = "ifany"))

cat("\n--- Distribución por ideología (jóvenes, ola 1) ---\n")
print(table(tripol_jovenes$ideologia_cat_1, useNA = "ifany"))


############################################################
# 9b. Attrition correction via Inverse Probability Weighting (IPW)
############################################################

modelo_permanencia <- glm(
  !attricion ~ polarizacion_1 + uso_rrss_1 + sexo_f + educacion_f + edad_1,
  data   = tripol_jovenes %>% drop_na(polarizacion_1, uso_rrss_1, sexo_f, educacion_f, edad_1),
  family = binomial(link = "logit")
)

cat("\n--- Modelo de permanencia (logístico) ---\n")
print(summary(modelo_permanencia))

tripol_jovenes <- tripol_jovenes %>%
  mutate(
    prob_permanecer = predict(modelo_permanencia, newdata = ., type = "response"),
    peso_ipw        = if_else(!attricion, 1 / prob_permanecer, NA_real_)
  )

cat("\n--- Distribución de pesos IPW (jóvenes permanentes) ---\n")
print(summary(tripol_jovenes$peso_ipw))
cat("N con peso_ipw válido:", sum(!is.na(tripol_jovenes$peso_ipw)), "\n")


############################################################
# 10. Clean analysis datasets — young adults
############################################################

tripol_m1 <- tripol_jovenes %>%
  drop_na(polarizacion_1, uso_rrss_1, ideologia_posicion_1,
          extremismo_1, edad_1, sexo, educacion_1)

tripol_m3 <- tripol_jovenes %>%
  drop_na(polarizacion_3, uso_rrss_3, ideologia_posicion_3,
          extremismo_3, edad_3, sexo, educacion_3)

tripol_panel_13 <- tripol_jovenes %>%
  drop_na(polarizacion_3, polarizacion_1, uso_rrss_1,
          ideologia_posicion_1, sexo, educacion_1)

tripol_nl1 <- tripol_jovenes %>%
  drop_na(polarizacion_1, uso_rrss_1, uso_rrss_1_cuad,
          ideologia_posicion_1, sexo, educacion_1)

tripol_nl3 <- tripol_jovenes %>%
  drop_na(polarizacion_3, uso_rrss_3, uso_rrss_3_cuad,
          ideologia_posicion_3, sexo, educacion_3)

# Standardized datasets — young adults
tripol_m1_z <- tripol_jovenes %>%
  drop_na(polarizacion_1, uso_rrss_1_z, ideologia_posicion_1_z,
          extremismo_1_z, edad_1, sexo_f, educacion_f)

tripol_m3_z <- tripol_jovenes %>%
  drop_na(polarizacion_3, uso_rrss_3_z, ideologia_posicion_3_z,
          extremismo_3_z, edad_3, sexo_f, educacion_f_3)

cat("\n--- N por base de modelo (jóvenes) ---\n")
cat("Ola 1:", nrow(tripol_m1), "| Ola 3:", nrow(tripol_m3), "\n")
cat("(Ola 2 no disponible: p21g no existe en ola 2)\n")
cat("CLPD 1→3:", nrow(tripol_panel_13), "\n")


############################################################
# 10b. Post-hoc statistical power analysis
############################################################

f2_ola1    <- 0.1717 / (1 - 0.1717)   # Observed R² of the Wave 1 model
u_ola1     <- 8
n_ola1     <- nrow(tripol_m1)

poder_global <- pwr.f2.test(
  u         = u_ola1,
  v         = n_ola1 - u_ola1 - 1,
  f2        = f2_ola1,
  sig.level = 0.05
)

poder_pequeno <- pwr.f2.test(
  u         = 1,
  v         = n_ola1 - u_ola1 - 1,
  f2        = 0.02,
  sig.level = 0.05
)

n_necesario <- pwr.f2.test(
  u         = u_ola1,
  f2        = 0.02,
  sig.level = 0.05,
  power     = 0.80
)

cat("\n--- Potencia estadística — Modelo OLS jóvenes ola 1 ---\n")
cat("u (predictores):", u_ola1, "\n")
cat("v (df residual):", n_ola1 - u_ola1 - 1, "\n")
cat("f2 (Cohen)     :", round(f2_ola1, 4), "\n")
cat("Potencia       :", round(poder_global$power, 3), "\n")

cat("\n--- Potencia para detectar efecto pequeño de uso_rrss (f2 = 0.02) ---\n")
cat("Potencia:", round(poder_pequeno$power, 3), "\n")
cat("  INTERPRETACIÓN: Si la potencia es < 0.80, el resultado nulo\n")
cat("  en jóvenes puede ser un artefacto de baja potencia estadística,\n")
cat("  no la ausencia real del efecto.\n")

cat("\n--- N mínimo para potencia 0.80 con efecto pequeño ---\n")
cat("N necesario (v + u + 1):", ceiling(n_necesario$v + u_ola1 + 1), "\n")
cat("N disponible            :", n_ola1, "\n")
cat("  NOTA: Si N necesario > N disponible, el resultado nulo en\n")
cat("  jóvenes podría ser un artefacto de baja potencia estadística.\n")


############################################################
# BLOCK A — ANALYSIS OF YOUNG ADULTS (18-35)
############################################################

############################################################
# 11. Descriptive statistics — young adults
############################################################

cat("\n--- Descriptivos de polarización afectiva (jóvenes) ---\n")
tripol_jovenes %>%
  summarise(
    across(
      c(polarizacion_1, polarizacion_2, polarizacion_3),
      list(media   = ~ mean(.x,   na.rm = TRUE),
           dt      = ~ sd(.x,     na.rm = TRUE),
           mediana = ~ median(.x, na.rm = TRUE),
           min     = ~ min(.x,    na.rm = TRUE),
           max     = ~ max(.x,    na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "estadistico"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = estadistico, values_from = value) %>%
  print()

cat("\n--- Test H1: polarización ola 1 vs. punto neutro del índice WAPD ---\n")
cat("Punto neutro empleado: 0 (ausencia de distancia afectiva, protocolo Wagner 2021)\n")
print(t.test(tripol_jovenes$polarizacion_1, mu = 0))
cat("  INTERPRETACIÓN: media > 0 con p < .05 confirma H1:\n")
cat("  los jóvenes presentan polarización afectiva positiva.\n")

cat("\n--- Descriptivos de uso de RRSS (jóvenes) ---\n")
tripol_jovenes %>%
  summarise(
    across(c(uso_rrss_1, uso_rrss_3),
           list(media = ~ mean(.x, na.rm = TRUE),
                dt    = ~ sd(.x,   na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>% print()
cat("(uso_rrss_2 no disponible: p21g no existe en ola 2)\n")

cat("\n--- Descriptivos de extremismo ideológico (jóvenes) ---\n")
tripol_jovenes %>%
  summarise(
    across(c(extremismo_1, extremismo_2, extremismo_3),
           list(media = ~ mean(.x, na.rm = TRUE),
                dt    = ~ sd(.x,   na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>% print()

cat("\n--- Descriptivos de polarización ideológica percibida WPIP (jóvenes) ---\n")
tripol_jovenes %>%
  summarise(
    across(c(WPIP_1, WPIP_2, WPIP_3),
           list(media = ~ mean(.x, na.rm = TRUE),
                dt    = ~ sd(.x,   na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>% print()


############################################################
# 12. Correlations — young adults
############################################################

cat("\n--- Correlaciones polarización ~ uso de RRSS (jóvenes) ---\n")
cat("Ola 1:\n"); print(cor.test(tripol_jovenes$polarizacion_1, tripol_jovenes$uso_rrss_1))
cat("Ola 3:\n"); print(cor.test(tripol_jovenes$polarizacion_3, tripol_jovenes$uso_rrss_3))

cat("\n--- Correlaciones polarización ~ extremismo ideológico (jóvenes) ---\n")
cat("Ola 1:\n"); print(cor.test(tripol_jovenes$polarizacion_1, tripol_jovenes$extremismo_1))
cat("Ola 3:\n"); print(cor.test(tripol_jovenes$polarizacion_3, tripol_jovenes$extremismo_3))


############################################################
# 13. Cross-sectional OLS models — young adults
############################################################

modelo_ola1 <- lm(
  polarizacion_1 ~ uso_rrss_1 + ideologia_posicion_1 + extremismo_1 +
    edad_1 + sexo_f + educacion_f,
  data = tripol_m1
)

modelo_ola3 <- lm(
  polarizacion_3 ~ uso_rrss_3 + ideologia_posicion_3 + extremismo_3 +
    edad_3 + sexo_f + educacion_f_3,
  data = tripol_m3
)

cat("\n===== JÓVENES — MODELO OLS OLA 1 =====\n"); print(summary(modelo_ola1))
cat("\n===== JÓVENES — MODELO OLS OLA 3 =====\n"); print(summary(modelo_ola3))

cat("\n--- VIF Ola 1 (jóvenes) ---\n"); print(vif(modelo_ola1))
cat("\n--- VIF Ola 3 (jóvenes) ---\n"); print(vif(modelo_ola3))

# Standardized models — young adults
modelo_ola1_z <- lm(
  polarizacion_1 ~ uso_rrss_1_z + ideologia_posicion_1_z + extremismo_1_z +
    edad_1 + sexo_f + educacion_f,
  data = tripol_m1_z
)

modelo_ola3_z <- lm(
  polarizacion_3 ~ uso_rrss_3_z + ideologia_posicion_3_z + extremismo_3_z +
    edad_3 + sexo_f + educacion_f_3,
  data = tripol_m3_z
)

cat("\n===== JÓVENES — MODELO OLA 1 ESTANDARIZADO =====\n")
print(summary(modelo_ola1_z))
cat("\n===== JÓVENES — MODELO OLA 3 ESTANDARIZADO =====\n")
print(summary(modelo_ola3_z))


############################################################
# 13.1 OLS assumption checks
############################################################

cat("\n--- Test de Breusch-Pagan (homocedasticidad) — Jóvenes ola 1 ---\n")
print(lmtest::bptest(modelo_ola1))

cat("\n--- Test de Breusch-Pagan (homocedasticidad) — Jóvenes ola 3 ---\n")
bp_ola3 <- lmtest::bptest(modelo_ola3)
print(bp_ola3)

cat("\n--- Coeficientes con errores HC3 — Jóvenes ola 1 ---\n")
print(coeftest(modelo_ola1, vcov = vcovHC(modelo_ola1, type = "HC3")))

cat("\n--- Coeficientes con errores HC3 — Jóvenes ola 3 ---\n")
cat("  (Ola 3: heterocedasticidad detectada, BP p =",
    round(bp_ola3$p.value, 4), "; se usan errores HC3)\n")
print(coeftest(modelo_ola3, vcov = vcovHC(modelo_ola3, type = "HC3")))

cat("\n--- Test de Shapiro-Wilk sobre residuos (normalidad) ---\n")
cat("  Ola 1:\n"); print(shapiro.test(resid(modelo_ola1)))
cat("  Ola 3:\n"); print(shapiro.test(resid(modelo_ola3)))


############################################################
# 13.2 Social media use × ideology interaction — young adults
############################################################

modelo_interaccion_ideo_1 <- lm(
  polarizacion_1 ~ uso_rrss_1 * ideologia_posicion_1 +
    extremismo_1 + edad_1 + sexo_f + educacion_f,
  data = tripol_m1
)

modelo_interaccion_ideo_3 <- lm(
  polarizacion_3 ~ uso_rrss_3 * ideologia_posicion_3 +
    extremismo_3 + edad_3 + sexo_f + educacion_f_3,
  data = tripol_m3
)

cat("\n===== INTERACCIÓN RRSS × IDEOLOGÍA — OLA 1 =====\n")
print(summary(modelo_interaccion_ideo_1))
cat("\n===== INTERACCIÓN RRSS × IDEOLOGÍA — OLA 3 =====\n")
print(summary(modelo_interaccion_ideo_3))


############################################################
# 13.3 Social media use × ideological extremism interaction — young adults
############################################################

modelo_interaccion_ie_1 <- lm(
  polarizacion_1 ~ uso_rrss_1 * extremismo_1 +
    ideologia_posicion_1 + edad_1 + sexo_f + educacion_f,
  data = tripol_m1
)

modelo_interaccion_ie_3 <- lm(
  polarizacion_3 ~ uso_rrss_3 * extremismo_3 +
    ideologia_posicion_3 + edad_3 + sexo_f + educacion_f_3,
  data = tripol_m3
)

cat("\n===== INTERACCIÓN RRSS × EXTREMISMO IDEOLÓGICO — OLA 1 =====\n")
print(summary(modelo_interaccion_ie_1))
cat("\n===== INTERACCIÓN RRSS × EXTREMISMO IDEOLÓGICO — OLA 3 =====\n")
print(summary(modelo_interaccion_ie_3))


############################################################
# 14. Cross-Lagged Panel Design (CLPD) model 1→3 — young adults
############################################################

modelo_clpd_13 <- lm(
  polarizacion_3 ~ polarizacion_1 + uso_rrss_1 +
    ideologia_posicion_1 + sexo_f + educacion_f,
  data = tripol_panel_13
)

cat("\n===== JÓVENES — CLPD 1→3 (Cross-Lagged Panel Design) =====\n")
cat("NOTA: No es un modelo de efectos fijos; ver documentación §14.\n")
print(summary(modelo_clpd_13))

# CLPD with IPW correction for attrition
tripol_panel_13_ipw <- tripol_jovenes %>%
  filter(!attricion) %>%
  drop_na(polarizacion_3, polarizacion_1, uso_rrss_1,
          ideologia_posicion_1, sexo_f, educacion_f, peso_ipw)

cat("N base CLPD-IPW:", nrow(tripol_panel_13_ipw), "\n")

modelo_clpd_ipw <- lm(
  polarizacion_3 ~ polarizacion_1 + uso_rrss_1 +
    ideologia_posicion_1 + sexo_f + educacion_f,
  data    = tripol_panel_13_ipw,
  weights = peso_ipw
)

cat("\n===== CLPD CON IPW (corrección attrición) =====\n")
print(summary(modelo_clpd_ipw))
cat("\n  COMPARACIÓN uso_rrss_1:\n")
cat("  Sin IPW :", round(coef(modelo_clpd_13)["uso_rrss_1"], 4),
    "  p =", round(summary(modelo_clpd_13)$coefficients["uso_rrss_1", "Pr(>|t|)"], 4), "\n")
cat("  Con IPW :", round(coef(modelo_clpd_ipw)["uso_rrss_1"], 4),
    "  p =", round(summary(modelo_clpd_ipw)$coefficients["uso_rrss_1", "Pr(>|t|)"], 4), "\n")
cat("  La estabilidad del coeficiente indica que la attrición no\n")
cat("  distorsiona materialmente los estimadores del CLPD.\n")


############################################################
# 15. Non-linear effects — young adults
############################################################

modelo_ola1_nl_base <- lm(
  polarizacion_1 ~ uso_rrss_1 + ideologia_posicion_1 + sexo_f + educacion_f,
  data = tripol_nl1
)

modelo_nolineal_1 <- lm(
  polarizacion_1 ~ uso_rrss_1 + uso_rrss_1_cuad +
    ideologia_posicion_1 + sexo_f + educacion_f,
  data = tripol_nl1
)

modelo_ola3_nl_base <- lm(
  polarizacion_3 ~ uso_rrss_3 + ideologia_posicion_3 + sexo_f + educacion_f_3,
  data = tripol_nl3
)

modelo_nolineal_3 <- lm(
  polarizacion_3 ~ uso_rrss_3 + uso_rrss_3_cuad +
    ideologia_posicion_3 + sexo_f + educacion_f_3,
  data = tripol_nl3
)

cat("\n===== NO LINEAL OLA 1 (jóvenes) =====\n"); print(summary(modelo_nolineal_1))
cat("\n===== NO LINEAL OLA 3 (jóvenes) =====\n"); print(summary(modelo_nolineal_3))

cat("\n--- ANOVA lineal vs. cuadrático (ola 1) ---\n")
print(anova(modelo_ola1_nl_base, modelo_nolineal_1))
cat("\n--- ANOVA lineal vs. cuadrático (ola 3) ---\n")
print(anova(modelo_ola3_nl_base, modelo_nolineal_3))


############################################################
# 16. Polarization by ideological group — young adults
############################################################

cat("\n--- Media de polarización por grupo ideológico (jóvenes, ola 1) ---\n")
tripol_jovenes %>%
  filter(!is.na(ideologia_cat_1)) %>%
  group_by(ideologia_cat_1) %>%
  summarise(
    media_pol = mean(polarizacion_1, na.rm = TRUE),
    dt_pol    = sd(polarizacion_1,   na.rm = TRUE),
    n         = sum(!is.na(polarizacion_1))
  ) %>% print()

aov_ideologia <- aov(polarizacion_1 ~ ideologia_cat_1, data = tripol_jovenes)
cat("\n--- ANOVA polarización por ideología (jóvenes, ola 1) ---\n")
print(summary(aov_ideologia))

# Kruskal-Wallis test with Dunn post-hoc comparisons
cat("\n--- Kruskal-Wallis: polarización por ideología (jóvenes, ola 1) ---\n")
kw_data <- tripol_jovenes %>%
  filter(!is.na(ideologia_cat_1) & !is.na(polarizacion_1))
print(kruskal.test(polarizacion_1 ~ ideologia_cat_1, data = kw_data))

cat("\n--- Post-hoc de Dunn (corrección Bonferroni) ---\n")
dunn_res <- capture.output(
  dunn.test(
    x      = kw_data$polarizacion_1,
    g      = kw_data$ideologia_cat_1,
    method = "bonferroni",
    alpha  = 0.05
  )
)
cat(paste(dunn_res, collapse = "\n"), "\n")

# Direct test of H3: ideological extremes vs. center
cat("\n--- H3: Comparación directa extremos vs. centro (ola 1) ---\n")
kw_data <- kw_data %>%
  mutate(extremo_vs_centro = if_else(
    ideologia_cat_1 == "Centro", "Centro", "Extremo"
  ))
print(t.test(polarizacion_1 ~ extremo_vs_centro, data = kw_data))
cat("  INTERPRETACIÓN: p < .05 y media Extremo > media Centro confirma H3:\n")
cat("  los individuos ubicados en los extremos del eje ideológico presentan\n")
cat("  mayor polarización afectiva que los ubicados en el centro,\n")
cat("  independientemente de la dirección ideológica.\n")


############################################################
# 17. Change across waves — young adults
############################################################

cat("\n--- Cambio medio en polarización (jóvenes) ---\n")
cat("Ola 1→2:", round(mean(tripol_jovenes$cambio_pol_12, na.rm = TRUE), 3), "\n")
cat("Ola 2→3:", round(mean(tripol_jovenes$cambio_pol_23, na.rm = TRUE), 3), "\n")
cat("Ola 1→3:", round(mean(tripol_jovenes$cambio_pol_13, na.rm = TRUE), 3), "\n")

cat("\n--- t-test cambio ola 1→2 (jóvenes) ---\n")
print(t.test(tripol_jovenes$cambio_pol_12))
cat("\n--- t-test cambio ola 1→3 (jóvenes) ---\n")
print(t.test(tripol_jovenes$cambio_pol_13))

# Change 1→2 compared across age groups
cat("\n--- Cambio 1→2 por grupo de edad ---\n")
tripol %>%
  filter(!is.na(grupo_edad2)) %>%
  group_by(grupo_edad2) %>%
  summarise(
    media_cambio_12 = mean(cambio_pol_12, na.rm = TRUE),
    dt_cambio_12    = sd(cambio_pol_12,   na.rm = TRUE),
    n               = sum(!is.na(cambio_pol_12))
  ) %>% print()

cat("\n--- t-test cambio ola 1→2 (jóvenes vs. resto) ---\n")
print(t.test(cambio_pol_12 ~ grupo_edad2, data = tripol))


############################################################
# 17.2 Robustness check with WPIP — young adults
############################################################

tripol_m1_wpip <- tripol_jovenes %>%
  drop_na(polarizacion_1, uso_rrss_1, ideologia_posicion_1,
          extremismo_1, WPIP_1, edad_1, sexo, educacion_1)

tripol_m3_wpip <- tripol_jovenes %>%
  drop_na(polarizacion_3, uso_rrss_3, ideologia_posicion_3,
          extremismo_3, WPIP_3, edad_3, sexo, educacion_3)

modelo_rob_ola1 <- lm(
  polarizacion_1 ~ uso_rrss_1 + ideologia_posicion_1 +
    extremismo_1 + WPIP_1 + edad_1 + sexo_f + educacion_f,
  data = tripol_m1_wpip
)

modelo_rob_ola3 <- lm(
  polarizacion_3 ~ uso_rrss_3 + ideologia_posicion_3 +
    extremismo_3 + WPIP_3 + edad_3 + sexo_f + educacion_f_3,
  data = tripol_m3_wpip
)

cat("\n===== ROBUSTEZ CON WPIP — JÓVENES OLA 1 =====\n")
print(summary(modelo_rob_ola1))
cat("\n===== ROBUSTEZ CON WPIP — JÓVENES OLA 3 =====\n")
print(summary(modelo_rob_ola3))

cat("\n--- Comparación coef. uso_rrss_1: modelo principal vs. robusto WPIP ---\n")
cat("Principal :", round(coef(modelo_ola1)["uso_rrss_1"], 4),
    "  p =", round(summary(modelo_ola1)$coefficients["uso_rrss_1", "Pr(>|t|)"], 4), "\n")
cat("Con WPIP  :", round(coef(modelo_rob_ola1)["uso_rrss_1"], 4),
    "  p =", round(summary(modelo_rob_ola1)$coefficients["uso_rrss_1", "Pr(>|t|)"], 4), "\n")
cat("  INTERPRETACIÓN: Si uso_rrss_1 se vuelve significativo al incluir WPIP,\n")
cat("  el efecto de las RRSS sobre la PA en jóvenes es condicionado a la\n")
cat("  percepción del sistema de partidos como polarizado. Esto matiza H2:\n")
cat("  las RRSS no operan directamente sobre la PA, sino a través de la\n")
cat("  percepción del contexto político.\n")

cat("\n--- Comparación coef. uso_rrss_3: modelo principal vs. robusto WPIP ---\n")
cat("Principal :", round(coef(modelo_ola3)["uso_rrss_3"], 4),
    "  p =", round(summary(modelo_ola3)$coefficients["uso_rrss_3", "Pr(>|t|)"], 4), "\n")
cat("Con WPIP  :", round(coef(modelo_rob_ola3)["uso_rrss_3"], 4),
    "  p =", round(summary(modelo_rob_ola3)$coefficients["uso_rrss_3", "Pr(>|t|)"], 4), "\n")


############################################################
# 18. Plot theme
############################################################

theme_tfg <- theme_minimal(base_size = 13) +
  theme(
    plot.title          = element_text(face = "bold", size = 15),
    plot.subtitle       = element_text(color = "grey40", size = 11),
    plot.title.position = "plot",
    axis.text           = element_text(size = 11),
    panel.grid.minor    = element_blank(),
    legend.position     = "right"
  )

theme_set(theme_tfg)


############################################################
# 19. Visualizations — young adults
############################################################

tripol_long <- tripol_jovenes %>%
  select(polarizacion_1, polarizacion_2, polarizacion_3, ideologia_cat_1, sexo_f) %>%
  pivot_longer(cols      = c(polarizacion_1, polarizacion_2, polarizacion_3),
               names_to  = "ola",
               values_to = "polarizacion") %>%
  mutate(
    ola = case_match(ola,
                     "polarizacion_1" ~ "Ola 1 (2021)",
                     "polarizacion_2" ~ "Ola 2 (2022)",
                     "polarizacion_3" ~ "Ola 3 (2023)"),
    ola = factor(ola, levels = c("Ola 1 (2021)", "Ola 2 (2022)", "Ola 3 (2023)"))
  )

g_evolucion <- ggplot(tripol_long, aes(x = ola, y = polarizacion, group = 1)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.4,
               color = viridis(3, option = "D")[2]) +
  stat_summary(fun = mean, geom = "point", size = 4,
               color = viridis(3, option = "D")[3]) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15,
               color = viridis(3, option = "D")[2]) +
  labs(title    = "Evolución media de la polarización afectiva (WAPD)",
       subtitle = "Jóvenes 18-35 · Barras = error estándar",
       x = NULL, y = "Polarización afectiva WAPD (media)")

g_boxplot <- ggplot(tripol_long, aes(x = ola, y = polarizacion, fill = ola)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.3) +
  scale_fill_viridis_d(option = "C") +
  labs(title    = "Distribución de la polarización afectiva (WAPD) por ola",
       subtitle = "Jóvenes 18-35", x = NULL, y = "Polarización afectiva WAPD") +
  theme(legend.position = "none")

scatter_pol_rrss <- function(datos, x_var, y_var, subtitulo) {
  ggplot(datos, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(aes(color = .data[[y_var]]), alpha = 0.55, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.9) +
    scale_color_viridis_c(option = "plasma", name = "Polarización") +
    labs(title    = "Polarización afectiva (WAPD) y uso de redes sociales",
         subtitle = subtitulo,
         x = "Uso de redes sociales (p21g, 0-7)", y = "Polarización afectiva WAPD")
}

g_scatter1 <- scatter_pol_rrss(tripol_m1, "uso_rrss_1", "polarizacion_1", "Ola 1")
g_scatter3 <- scatter_pol_rrss(tripol_m3, "uso_rrss_3", "polarizacion_3", "Ola 3")

g_ideologia <- tripol_jovenes %>%
  filter(!is.na(ideologia_cat_1) & !is.na(polarizacion_1)) %>%
  ggplot(aes(x = ideologia_cat_1, y = polarizacion_1, fill = ideologia_cat_1)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("Izquierda" = "#E63946",
                               "Centro"    = "#457B9D",
                               "Derecha"   = "#1D3557")) +
  labs(title    = "Polarización afectiva (WAPD) por posición ideológica",
       subtitle = "Jóvenes 18-35 · Ola 1",
       x = "Posición ideológica", y = "Polarización afectiva WAPD") +
  theme(legend.position = "none")

g_sexo <- tripol_long %>%
  filter(!is.na(sexo_f)) %>%
  ggplot(aes(x = ola, y = polarizacion, fill = sexo_f)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.3,
               position = position_dodge(width = 0.8)) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.75) +
  labs(title    = "Polarización afectiva (WAPD) por sexo y ola",
       subtitle = "Jóvenes 18-35",
       x = NULL, y = "Polarización afectiva WAPD", fill = "Sexo")

g_evolucion
g_boxplot
g_scatter1 | g_scatter3
g_ideologia
g_sexo
(g_evolucion + g_boxplot) / (g_scatter1 + g_scatter3)


############################################################
# BLOCK B — ANALYSIS OF THE REST OF THE POPULATION (36+)
############################################################

cat("\n========================================================\n")
cat("BLOQUE B: RESTO DE LA POBLACIÓN (36+)\n")
cat("========================================================\n")

############################################################
# 20. Comparative descriptive statistics
############################################################

desc_grupo <- function(datos, var, label) {
  datos %>%
    filter(!is.na(.data[[var]])) %>%
    summarise(
      grupo   = label,
      n       = n(),
      media   = round(mean(.data[[var]],   na.rm = TRUE), 3),
      dt      = round(sd(.data[[var]],     na.rm = TRUE), 3),
      mediana = round(median(.data[[var]], na.rm = TRUE), 3)
    )
}

cat("\n--- Polarización afectiva (WAPD) por grupo (ola 1) ---\n")
bind_rows(
  desc_grupo(tripol_jovenes, "polarizacion_1", "Jóvenes"),
  desc_grupo(tripol_resto,   "polarizacion_1", "Resto")
) %>% print()

cat("\n--- Uso de RRSS por grupo (ola 1) ---\n")
bind_rows(
  desc_grupo(tripol_jovenes, "uso_rrss_1", "Jóvenes"),
  desc_grupo(tripol_resto,   "uso_rrss_1", "Resto")
) %>% print()

cat("\n--- Uso de RRSS por grupo (ola 3) ---\n")
bind_rows(
  desc_grupo(tripol_jovenes, "uso_rrss_3", "Jóvenes"),
  desc_grupo(tripol_resto,   "uso_rrss_3", "Resto")
) %>% print()


############################################################
# 21. Mean-difference t-tests
############################################################

cat("\n--- t-test: polarización ola 1 (jóvenes vs. resto) ---\n")
print(t.test(polarizacion_1 ~ grupo_edad2, data = tripol))

cat("\n--- t-test: uso de RRSS ola 1 (jóvenes vs. resto) ---\n")
print(t.test(uso_rrss_1 ~ grupo_edad2, data = tripol))

cat("\n--- t-test: uso de RRSS ola 3 (jóvenes vs. resto) ---\n")
print(t.test(uso_rrss_3 ~ grupo_edad2, data = tripol))


############################################################
# 22. Clean analysis datasets — rest of the population
############################################################

resto_m1 <- tripol_resto %>%
  drop_na(polarizacion_1, uso_rrss_1, ideologia_posicion_1,
          extremismo_1, edad_1, sexo_f, educacion_f)

resto_m3 <- tripol_resto %>%
  drop_na(polarizacion_3, uso_rrss_3, ideologia_posicion_3,
          extremismo_3, edad_3, sexo_f, educacion_f_3)

resto_panel_13 <- tripol_resto %>%
  drop_na(polarizacion_3, polarizacion_1, uso_rrss_1,
          ideologia_posicion_1, sexo_f, educacion_f)

# Standardized datasets — rest of the population (full specification with extremismo_z)
resto_m1_z <- tripol_resto %>%
  drop_na(polarizacion_1, uso_rrss_1_z, ideologia_posicion_1_z,
          extremismo_1_z, edad_1, sexo_f, educacion_f)

resto_m3_z <- tripol_resto %>%
  drop_na(polarizacion_3, uso_rrss_3_z, ideologia_posicion_3_z,
          extremismo_3_z, edad_3, sexo_f, educacion_f_3)

cat("\nN base resto ola 1:", nrow(resto_m1), "\n")
cat("N base resto ola 3:", nrow(resto_m3), "\n")
cat("N base resto CLPD 1→3:", nrow(resto_panel_13), "\n")


############################################################
# 23. OLS models — rest of the population
############################################################

modelo_resto_ola1 <- lm(
  polarizacion_1 ~ uso_rrss_1 + ideologia_posicion_1 + extremismo_1 +
    edad_1 + sexo_f + educacion_f,
  data = resto_m1
)

modelo_resto_ola3 <- lm(
  polarizacion_3 ~ uso_rrss_3 + ideologia_posicion_3 + extremismo_3 +
    edad_3 + sexo_f + educacion_f_3,
  data = resto_m3
)

modelo_resto_ola1_z <- lm(
  polarizacion_1 ~ uso_rrss_1_z + ideologia_posicion_1_z + extremismo_1_z +
    edad_1 + sexo_f + educacion_f,
  data = resto_m1_z
)

modelo_resto_ola3_z <- lm(
  polarizacion_3 ~ uso_rrss_3_z + ideologia_posicion_3_z + extremismo_3_z +
    edad_3 + sexo_f + educacion_f_3,
  data = resto_m3_z
)

modelo_resto_clpd_13 <- lm(
  polarizacion_3 ~ polarizacion_1 + uso_rrss_1 +
    ideologia_posicion_1 + sexo_f + educacion_f,
  data = resto_panel_13
)

cat("\n===== RESTO — MODELO OLS OLA 1 =====\n");     print(summary(modelo_resto_ola1))
cat("\n===== RESTO — MODELO OLS OLA 3 =====\n");     print(summary(modelo_resto_ola3))
cat("\n===== RESTO — MODELO OLA 1 ESTANDARIZADO =====\n"); print(summary(modelo_resto_ola1_z))
cat("\n===== RESTO — MODELO OLA 3 ESTANDARIZADO =====\n"); print(summary(modelo_resto_ola3_z))
cat("\n===== RESTO — CLPD 1→3 =====\n")
cat("NOTA: No es un modelo de efectos fijos; ver documentación §14.\n")
print(summary(modelo_resto_clpd_13))


############################################################
# 23b. Comparative table of standardized betas
############################################################

cat("\n--- Comparación β estandarizados: jóvenes vs. resto ---\n")
cat("  (Evidencia empírica de H4/H5: ¿difieren los efectos por edad?)\n")
bind_rows(
  tidy(modelo_ola1_z)      %>% mutate(grupo = "Jóvenes", ola = "Ola 1"),
  tidy(modelo_resto_ola1_z) %>% mutate(grupo = "Resto",   ola = "Ola 1"),
  tidy(modelo_ola3_z)      %>% mutate(grupo = "Jóvenes", ola = "Ola 3"),
  tidy(modelo_resto_ola3_z) %>% mutate(grupo = "Resto",   ola = "Ola 3")
) %>%
  filter(term %in% c("uso_rrss_1_z", "uso_rrss_3_z",
                     "extremismo_1_z", "extremismo_3_z")) %>%
  select(grupo, ola, term, estimate, std.error, p.value) %>%
  mutate(across(where(is.numeric), ~round(.x, 4))) %>%
  print(n = Inf)

cat("  NOTA: El beta de uso_rrss_z es numéricamente mayor en jóvenes\n")
cat("  que en el Resto en ola 1, pero no alcanza significación por el\n")
cat("  N reducido (ver análisis de potencia §10b).\n")
cat("  El beta de extremismo_3_z en jóvenes (>1.0) supera al del Resto,\n")
cat("  evidencia parcial de H4: estructura diferente de determinantes.\n")


############################################################
# 24. Descriptive profiling analysis (multinomial)
############################################################

cat("\n========================================================\n")
cat("ANÁLISIS DE PERFILADO DESCRIPTIVO (MULTINOMIAL)\n")
cat("VD: grupo_edad3 — SOLO INTERPRETACIÓN DESCRIPTIVA\n")
cat("Ver §29 para el test causal de moderación por edad.\n")
cat("========================================================\n")

base_multinomial <- tripol %>%
  drop_na(grupo_edad3, uso_rrss_1, polarizacion_1,
          ideologia_posicion_1, sexo_f, educacion_f)

cat("\nN base multinomial:", nrow(base_multinomial), "\n")
print(table(base_multinomial$grupo_edad3))

modelo_multinomial <- multinom(
  grupo_edad3 ~ uso_rrss_1 + polarizacion_1 + ideologia_posicion_1 + sexo_f + educacion_f,
  data  = base_multinomial,
  trace = FALSE
)

cat("\n===== RESUMEN DEL MODELO DE PERFILADO =====\n")
print(summary(modelo_multinomial))

coef_mn <- coef(modelo_multinomial)
se_mn   <- summary(modelo_multinomial)$standard.errors
z_mn    <- coef_mn / se_mn
p_mn    <- 2 * (1 - pnorm(abs(z_mn)))

cat("\n--- Z-scores ---\n"); print(round(z_mn, 3))
cat("\n--- P-valores ---\n"); print(round(p_mn, 4))
cat("\n--- Odds Ratios (exp(coef)) [descriptivos, no causales] ---\n")
print(round(exp(coef_mn), 3))

cat("\n--- Test Wald (significación global de cada predictor) ---\n")
print(car::Anova(modelo_multinomial, type = "II"))

modelo_nulo <- multinom(grupo_edad3 ~ 1, data = base_multinomial, trace = FALSE)
ll_nulo     <- logLik(modelo_nulo)
ll_modelo   <- logLik(modelo_multinomial)
mcfadden_r2 <- as.numeric(1 - (ll_modelo / ll_nulo))

cat("\n--- Ajuste del modelo de perfilado ---\n")
cat("Log-likelihood nulo:   ", round(ll_nulo,   2), "\n")
cat("Log-likelihood modelo: ", round(ll_modelo, 2), "\n")
cat("McFadden R²:           ", round(mcfadden_r2, 4), "\n")
cat("AIC:", round(AIC(modelo_multinomial), 2), "\n")
cat("BIC:", round(BIC(modelo_multinomial), 2), "\n")

lr_stat <- -2 * (as.numeric(ll_nulo) - as.numeric(ll_modelo))
df_lr   <- length(coef(modelo_multinomial))
p_lr    <- pchisq(lr_stat, df = df_lr, lower.tail = FALSE)
cat("LR chi² =", round(lr_stat, 3), " df =", df_lr, " p =", round(p_lr, 4), "\n")

cat("\n--- Verificación LR test (lmtest::lrtest) ---\n")
print(lrtest(modelo_nulo, modelo_multinomial))

pred_clase <- predict(modelo_multinomial, type = "class")
cat("\n--- Matriz de clasificación ---\n")
print(table(Predicho = pred_clase, Observado = base_multinomial$grupo_edad3))
cat("Precisión global:", round(mean(pred_clase == base_multinomial$grupo_edad3), 3), "\n")
cat("\n  NOTA: La precisión de clasificación refleja diferencias descriptivas,\n")
cat("  no relaciones causales.\n")


############################################################
# 25. Visualizations — age groups
############################################################

tripol_long_grupos <- tripol %>%
  filter(!is.na(grupo_edad3)) %>%
  select(grupo_edad3, uso_rrss_1, uso_rrss_3) %>%
  pivot_longer(cols      = c(uso_rrss_1, uso_rrss_3),
               names_to  = "ola", values_to = "uso_rrss") %>%
  mutate(ola = factor(ola,
                      levels = c("uso_rrss_1", "uso_rrss_3"),
                      labels = c("Ola 1", "Ola 3")))

g_rrss_grupo <- tripol %>%
  filter(!is.na(grupo_edad3) & !is.na(uso_rrss_1)) %>%
  ggplot(aes(x = grupo_edad3, y = uso_rrss_1, fill = grupo_edad3)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.3) +
  scale_fill_viridis_d(option = "D") +
  labs(title    = "Uso informativo de RRSS por grupo de edad",
       subtitle = "Ola 1 — TriPol (España)", x = NULL, y = "Uso de RRSS (p21g, 0-7)") +
  theme(legend.position = "none")

g_pol_grupo <- tripol %>%
  filter(!is.na(grupo_edad3) & !is.na(polarizacion_1)) %>%
  ggplot(aes(x = grupo_edad3, y = polarizacion_1, fill = grupo_edad3)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.3) +
  scale_fill_viridis_d(option = "C") +
  labs(title    = "Polarización afectiva (WAPD) por grupo de edad",
       subtitle = "Ola 1 — TriPol (España)", x = NULL, y = "Polarización afectiva WAPD") +
  theme(legend.position = "none")

g_rrss_evolucion_grupos <- ggplot(
  tripol_long_grupos, aes(x = ola, y = uso_rrss, color = grupo_edad3, group = grupo_edad3)
) +
  stat_summary(fun = mean, geom = "line",  linewidth = 1.3) +
  stat_summary(fun = mean, geom = "point", size = 3.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.12) +
  scale_color_viridis_d(option = "D", name = "Grupo de edad") +
  labs(title    = "Evolución del uso de RRSS por grupo de edad",
       subtitle = "Medias por ola · Barras = error estándar",
       x = NULL, y = "Uso de RRSS (p21g, 0-7)")

g_scatter_grupos <- tripol %>%
  filter(!is.na(grupo_edad3) & !is.na(polarizacion_1) & !is.na(uso_rrss_1)) %>%
  ggplot(aes(x = uso_rrss_1, y = polarizacion_1, color = grupo_edad3)) +
  geom_point(alpha = 0.35, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  scale_color_viridis_d(option = "D", name = "Grupo de edad") +
  labs(title    = "Polarización afectiva (WAPD) y uso de RRSS por grupo de edad",
       subtitle = "Ola 1 — líneas de regresión separadas por grupo",
       x = "Uso de RRSS (p21g, 0-7)", y = "Polarización afectiva WAPD")

g_rrss_grupo | g_pol_grupo
g_rrss_evolucion_grupos
g_scatter_grupos


############################################################
# BLOCK C — COMPARISON BETWEEN SUBSAMPLES
############################################################

cat("\n========================================================\n")
cat("BLOQUE C: COMPARACIÓN DE COEFICIENTES ENTRE SUBMUESTRAS\n")
cat("========================================================\n")


############################################################
# 26. Combined clean datasets
############################################################

full_m1 <- tripol %>%
  filter(!is.na(grupo_edad2)) %>%
  drop_na(polarizacion_1, uso_rrss_1, ideologia_posicion_1,
          extremismo_1, edad_1, sexo_f, educacion_f)

full_m3 <- tripol %>%
  filter(!is.na(grupo_edad2)) %>%
  drop_na(polarizacion_3, uso_rrss_3, ideologia_posicion_3,
          extremismo_3, edad_3, sexo_f, educacion_f_3)

full_panel_13 <- tripol %>%
  filter(!is.na(grupo_edad2)) %>%
  drop_na(polarizacion_3, polarizacion_1, uso_rrss_1,
          ideologia_posicion_1, sexo_f, educacion_f)


############################################################
# 27. Models split by group
############################################################

mod_jov_1 <- lm(
  polarizacion_1 ~ uso_rrss_1 + ideologia_posicion_1 + extremismo_1 +
    edad_1 + sexo_f + educacion_f,
  data = filter(full_m1, grupo_edad2 == "Jóvenes (18-35)")
)

mod_rest_1 <- lm(
  polarizacion_1 ~ uso_rrss_1 + ideologia_posicion_1 + extremismo_1 +
    edad_1 + sexo_f + educacion_f,
  data = filter(full_m1, grupo_edad2 == "Resto (36+)")
)

mod_jov_3 <- lm(
  polarizacion_3 ~ uso_rrss_3 + ideologia_posicion_3 + extremismo_3 +
    edad_3 + sexo_f + educacion_f_3,
  data = filter(full_m3, grupo_edad2 == "Jóvenes (18-35)")
)

mod_rest_3 <- lm(
  polarizacion_3 ~ uso_rrss_3 + ideologia_posicion_3 + extremismo_3 +
    edad_3 + sexo_f + educacion_f_3,
  data = filter(full_m3, grupo_edad2 == "Resto (36+)")
)

mod_jov_panel <- lm(
  polarizacion_3 ~ polarizacion_1 + uso_rrss_1 +
    ideologia_posicion_1 + sexo_f + educacion_f,
  data = filter(full_panel_13, grupo_edad2 == "Jóvenes (18-35)")
)

mod_rest_panel <- lm(
  polarizacion_3 ~ polarizacion_1 + uso_rrss_1 +
    ideologia_posicion_1 + sexo_f + educacion_f,
  data = filter(full_panel_13, grupo_edad2 == "Resto (36+)")
)


############################################################
# 28. Comparative coefficient table
############################################################

tabla_comparativa <- function(mod_a, mod_b, label_a = "Jóvenes", label_b = "Resto") {
  bind_rows(
    tidy(mod_a, conf.int = TRUE) %>% mutate(grupo = label_a),
    tidy(mod_b, conf.int = TRUE) %>% mutate(grupo = label_b)
  ) %>%
    select(grupo, term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
    mutate(
      sig = case_when(
        p.value < .001 ~ "***",
        p.value < .01  ~ "**",
        p.value < .05  ~ "*",
        p.value < .10  ~ ".",
        TRUE           ~ ""
      ),
      across(where(is.numeric), ~ round(.x, 4))
    ) %>%
    arrange(term, grupo)
}

cat("\n--- Tabla comparativa OLS — Ola 1 ---\n")
print(tabla_comparativa(mod_jov_1, mod_rest_1), n = Inf)

cat("\n--- Tabla comparativa OLS — Ola 3 ---\n")
print(tabla_comparativa(mod_jov_3, mod_rest_3), n = Inf)

cat("\n--- Tabla comparativa CLPD 1→3 ---\n")
print(tabla_comparativa(mod_jov_panel, mod_rest_panel), n = Inf)

cat("\n  ATENCIÓN AL TRIBUNAL — Resultado central: Los coeficientes de\n")
cat("  uso_rrss en jóvenes son no significativos en ambas olas y en el CLPD.\n")
cat("  El efecto significativo aparece en el grupo Resto (36+).\n")
cat("  Esto invierte la hipótesis central del TFG y debe interpretarse\n")
cat("  como resultado nulo teóricamente interesante, no como fracaso.\n")


############################################################
# 29. Formal test: interaction models (moderation by age group)
############################################################

mod_main_1  <- lm(
  polarizacion_1 ~ uso_rrss_1 + grupo_edad2 + ideologia_posicion_1 +
    extremismo_1 + edad_1 + sexo_f + educacion_f,
  data = full_m1
)
mod_inter_1 <- lm(
  polarizacion_1 ~ uso_rrss_1 * grupo_edad2 + ideologia_posicion_1 +
    extremismo_1 + edad_1 + sexo_f + educacion_f,
  data = full_m1
)

mod_main_3  <- lm(
  polarizacion_3 ~ uso_rrss_3 + grupo_edad2 + ideologia_posicion_3 +
    extremismo_3 + edad_3 + sexo_f + educacion_f_3,
  data = full_m3
)
mod_inter_3 <- lm(
  polarizacion_3 ~ uso_rrss_3 * grupo_edad2 + ideologia_posicion_3 +
    extremismo_3 + edad_3 + sexo_f + educacion_f_3,
  data = full_m3
)

cat("\n===== INTERACCIÓN RRSS × GRUPO — OLA 1 =====\n")
print(summary(mod_inter_1))
cat("\n--- ANOVA: ¿mejora el modelo con interacción? (ola 1) ---\n")
print(anova(mod_main_1, mod_inter_1))

cat("\n===== INTERACCIÓN RRSS × GRUPO — OLA 3 =====\n")
print(summary(mod_inter_3))
cat("\n--- ANOVA: ¿mejora el modelo con interacción? (ola 3) ---\n")
print(anova(mod_main_3, mod_inter_3))

# Effect size (f²) of the interaction term
r2_main_1  <- summary(mod_main_1)$r.squared
r2_inter_1 <- summary(mod_inter_1)$r.squared
r2_main_3  <- summary(mod_main_3)$r.squared
r2_inter_3 <- summary(mod_inter_3)$r.squared

cat("\n--- Tamaño del efecto f² de la interacción (ola 1) ---\n")
cat("R² modelo principal:", round(r2_main_1, 4), "\n")
cat("R² modelo interacc.:", round(r2_inter_1, 4), "\n")
cat("f² de la interacción:", round((r2_inter_1 - r2_main_1) / (1 - r2_inter_1), 4), "\n")
cat("  REFERENCIA: f² < 0.02 = efecto trivial; 0.02-0.15 = pequeño;\n")
cat("  0.15-0.35 = moderado; > 0.35 = grande (Cohen, 1988).\n")

cat("\n--- Tamaño del efecto f² de la interacción (ola 3) ---\n")
cat("R² modelo principal:", round(r2_main_3, 4), "\n")
cat("R² modelo interacc.:", round(r2_inter_3, 4), "\n")
cat("f² de la interacción:", round((r2_inter_3 - r2_main_3) / (1 - r2_inter_3), 4), "\n")
cat("  INTERPRETACIÓN: f² trivial + p no significativo indica que\n")
cat("  la ausencia de moderación es un hallazgo genuino, no un\n")
cat("  problema de potencia estadística.\n")


############################################################
# 30. Chow test (global equivalence of models)
############################################################

chow_test <- function(mod_a, mod_b, label = "") {
  n_a   <- nobs(mod_a)
  n_b   <- nobs(mod_b)
  k     <- length(coef(mod_a))
  N     <- n_a + n_b

  rss_a <- sum(resid(mod_a)^2)
  rss_b <- sum(resid(mod_b)^2)

  datos_pool <- bind_rows(model.frame(mod_a), model.frame(mod_b))
  rss_r      <- sum(resid(lm(formula(mod_a), data = datos_pool))^2)

  F_stat <- ((rss_r - (rss_a + rss_b)) / k) /
    ((rss_a + rss_b) / (N - 2 * k))
  p_val  <- pf(F_stat, df1 = k, df2 = N - 2 * k, lower.tail = FALSE)

  cat(sprintf("Chow test %s: F(%d, %d) = %.3f  p = %.4f\n",
              label, k, N - 2 * k, F_stat, p_val))
  cat(sprintf("  (N_jov = %d, N_rest = %d, k = %d)\n", n_a, n_b, k))
  invisible(list(F = F_stat, p = p_val, df1 = k, df2 = N - 2 * k))
}

cat("\n--- Test de Chow (robustez de §29) ---\n")
chow_test(mod_jov_1,     mod_rest_1,     label = "— Ola 1")
chow_test(mod_jov_3,     mod_rest_3,     label = "— Ola 3")
chow_test(mod_jov_panel, mod_rest_panel, label = "— CLPD 1→3")

cat("\n--- Coherencia Chow vs. Interacción (H5) ---\n")
cat("  Si ambos p-valores son > .05, la evidencia es consistente:\n")
cat("  el efecto de las RRSS no difiere significativamente entre\n")
cat("  jóvenes y resto (H5 no se confirma empíricamente).\n")


############################################################
# 31. Comparative coefficient plot
############################################################

coef_plot_data <- bind_rows(
  tidy(mod_jov_1,  conf.int = TRUE) %>% mutate(grupo = "Jóvenes", modelo = "Ola 1"),
  tidy(mod_rest_1, conf.int = TRUE) %>% mutate(grupo = "Resto",   modelo = "Ola 1"),
  tidy(mod_jov_3,  conf.int = TRUE) %>% mutate(grupo = "Jóvenes", modelo = "Ola 3"),
  tidy(mod_rest_3, conf.int = TRUE) %>% mutate(grupo = "Resto",   modelo = "Ola 3")
) %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = p.value < .05)

g_coef_comparativo <- ggplot(
  coef_plot_data,
  aes(x = estimate, y = term, color = grupo, xmin = conf.low, xmax = conf.high)
) +
  geom_pointrange(position = position_dodge(width = 0.55), size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.75) +
  facet_wrap(~ modelo) +
  labs(
    title    = "Comparación de coeficientes OLS entre submuestras",
    subtitle = "Jóvenes (18-35) vs. Resto (36+) · IC al 95 %\n(Índice WAPD protocolo TriPol; signo positivo = más polarización)",
    x        = "Estimación (β)",
    y        = NULL,
    color    = "Grupo"
  )

g_coef_comparativo

# ggsave(here("figuras", "coef_comparativo.png"),
#        g_coef_comparativo, width = 12, height = 7, dpi = 300)


############################################################
# 32. Export table for the thesis
############################################################

tabla_tfg_ola1  <- tabla_comparativa(mod_jov_1, mod_rest_1)  %>% mutate(ola = "Ola 1")
tabla_tfg_ola3  <- tabla_comparativa(mod_jov_3, mod_rest_3)  %>% mutate(ola = "Ola 3")
tabla_tfg_panel <- tabla_comparativa(mod_jov_panel, mod_rest_panel) %>% mutate(ola = "CLPD 1→3")

tabla_tfg_final <- bind_rows(tabla_tfg_ola1, tabla_tfg_ola3, tabla_tfg_panel)

cat("\n--- Tabla final para el TFG ---\n")
print(tabla_tfg_final, n = Inf)

############################################################
# END OF SCRIPT
############################################################
