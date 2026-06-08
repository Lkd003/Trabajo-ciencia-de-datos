# =============================================================================
# 03_analisis.R
# Métodos estadísticos para H1 y H2
# Ciencia de Datos — Grupo 11
# =============================================================================

library(tidyverse)

# Países seleccionados (Polonia reemplazada por Corea del Sur por falta de
# datos de PBI per cápita en 1970)
paises_seleccionados <- c("ARG","BRA","MEX","CHL","COL",
                          "USA","DEU","GBR","FRA","TUR",
                          "CHN","IDN","KOR","ESP","CRI")

paises_repr <- c("CHN", "KOR", "ARG", "ESP")

# =============================================================================
# CLASIFICACIÓN DE GRUPOS
# Variación del índice industrial 1970-2023, corte por mediana
# =============================================================================

variacion_industrial <- datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados) %>%
  filter(anio %in% c(1970, 2023)) %>%
  select(codigo_pais, pais, anio, pbi_indust_pc_idx) %>%
  pivot_wider(names_from = anio, values_from = pbi_indust_pc_idx,
              names_prefix = "idx_") %>%
  mutate(
    variacion_idx = idx_2023 - idx_1970,
    grupo = if_else(variacion_idx >= median(variacion_idx, na.rm = TRUE),
                    "Más industrializado", "Menos industrializado")
  )

panel_indexado <- datos_panel %>%
  left_join(variacion_industrial %>% select(codigo_pais, variacion_idx, grupo),
            by = "codigo_pais")

# =============================================================================
# MÉTODO 1 — Indexación y trayectorias (H1)
# =============================================================================

base_1970 <- panel_indexado %>%
  filter(anio == 1970) %>%
  group_by(grupo) %>%
  summarise(base = mean(pbi_pc, na.rm = TRUE))

trayectorias <- panel_indexado %>%
  filter(!is.na(pbi_pc)) %>%
  group_by(grupo, anio) %>%
  summarise(pbi_pc_mean = mean(pbi_pc, na.rm = TRUE), .groups = "drop") %>%
  left_join(base_1970, by = "grupo") %>%
  mutate(pbi_pc_idx = pbi_pc_mean / base * 100)

trayectorias %>%
  filter(anio %in% c(1970, 1990, 2010, 2023)) %>%
  select(grupo, anio, pbi_pc_idx) %>%
  pivot_wider(names_from = anio, values_from = pbi_pc_idx) %>%
  write.csv("output/tablas/tabla_trayectorias.csv", row.names = FALSE)

# =============================================================================
# MÉTODO 2 — Descomposición de series temporales (H1)
# Países representativos: China + Corea del Sur vs. Argentina + España
# =============================================================================

descomposicion_resultados <- datos_panel %>%
  filter(codigo_pais %in% paises_repr, !is.na(pbi_pc)) %>%
  group_by(codigo_pais, pais) %>%
  arrange(anio) %>%
  group_modify(~ {
    tendencia <- as.numeric(fitted(loess(pbi_pc ~ anio, data = .x, span = 0.3)))
    tibble(
      anio      = .x$anio,
      pbi_pc    = .x$pbi_pc,
      tendencia = tendencia,
      residuo   = .x$pbi_pc - tendencia
    )
  }) %>%
  ungroup() %>%
  left_join(variacion_industrial %>% select(codigo_pais, grupo),
            by = "codigo_pais")

descomposicion_resultados %>%
  group_by(pais, grupo) %>%
  summarise(
    residuo_medio = mean(abs(residuo), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(grupo) %>%
  write.csv("output/tablas/tabla_residuos.csv", row.names = FALSE)

# =============================================================================
# MÉTODO 3A — Test de Welch (H1)
# Compara medias de crecimiento del PBI per cápita entre grupos
# =============================================================================

crecimiento_pbi <- datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados) %>%
  filter(anio %in% c(1970, 2023), !is.na(pbi_pc)) %>%
  select(codigo_pais, pais, anio, pbi_pc) %>%
  pivot_wider(names_from = anio, values_from = pbi_pc,
              names_prefix = "pbi_") %>%
  mutate(crecimiento_pct = (pbi_2023 - pbi_1970) / pbi_1970 * 100) %>%
  left_join(variacion_industrial %>% select(codigo_pais, grupo),
            by = "codigo_pais") %>%
  filter(!is.na(crecimiento_pct), !is.na(grupo))

# Estadísticas descriptivas por grupo
crecimiento_pbi %>%
  group_by(grupo) %>%
  summarise(
    n       = n(),
    media   = mean(crecimiento_pct),
    mediana = median(crecimiento_pct),
    desvio  = sd(crecimiento_pct),
    minimo  = min(crecimiento_pct),
    maximo  = max(crecimiento_pct)
  ) %>%
  write.csv("output/tablas/tabla_descriptiva_welch.csv", row.names = FALSE)

# Test de Welch
# Nota: Polonia excluida por falta de dato de PBI per cápita en 1970
welch_resultado <- t.test(crecimiento_pct ~ grupo,
                          data = crecimiento_pbi,
                          var.equal = FALSE)
print(welch_resultado)

tibble(
  estadistico     = welch_resultado$statistic,
  p_valor         = welch_resultado$p.value,
  ic_inf          = welch_resultado$conf.int[1],
  ic_sup          = welch_resultado$conf.int[2],
  media_mas_ind   = welch_resultado$estimate[1],
  media_menos_ind = welch_resultado$estimate[2],
  interpretacion  = if_else(welch_resultado$p.value < 0.05,
                            "Se rechaza H0: diferencia significativa",
                            "No se rechaza H0: diferencia no significativa")
) %>%
  write.csv("output/tablas/tabla_welch.csv", row.names = FALSE)

# =============================================================================
# MÉTODO 3B — Regresión OLS (H2)
# Variable independiente: variación índice industrial
# Variable dependiente: Gini en 4 puntos temporales (~60 observaciones)
# =============================================================================

años_ref <- c(1970, 1990, 2010, 2023)

gini_temporal <- datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados) %>%
  filter(!is.na(coef_gini)) %>%
  crossing(año_ref = años_ref) %>%
  mutate(distancia = abs(anio - año_ref)) %>%
  group_by(codigo_pais, pais, año_ref) %>%
  slice_min(distancia, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(codigo_pais, pais, año_ref, anio_real = anio, coef_gini)

datos_ols <- gini_temporal %>%
  left_join(variacion_industrial %>% select(codigo_pais, variacion_idx),
            by = "codigo_pais")

ols_resultado <- lm(coef_gini ~ variacion_idx, data = datos_ols)
print(summary(ols_resultado))

as.data.frame(confint(ols_resultado)) %>%
  rownames_to_column("termino") %>%
  mutate(estimacion = coef(ols_resultado)) %>%
  write.csv("output/tablas/tabla_ols.csv", row.names = FALSE)

# Confirmar archivos guardados
cat("\nArchivos guardados en output/tablas/:\n")
list.files("output/tablas/")