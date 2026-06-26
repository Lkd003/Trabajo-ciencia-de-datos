# Analisis

library(tidyverse)

datos_panel <- read.csv("input/datos_panel.csv")


paises_seleccionados <- c("ARG","BRA","MEX","CHL","COL",
                          "USA","DEU","GBR","FRA","TUR",
                          "CHN","IDN","KOR","ESP","CRI")


paises_repr <- c("CHN", "KOR", "ARG", "BRA")

# CLASIFICACIÓN DE GRUPOS
# Se corrige x corte en 2023 (pbi_indust_pc, no el indice).

nivel_industrial <- datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados) %>%
  filter(anio == 2023) %>%
  select(codigo_pais, pais, pbi_indust_pc) %>%
  filter(!is.na(pbi_indust_pc)) %>%
  mutate(
    grupo = if_else(pbi_indust_pc >= median(pbi_indust_pc, na.rm = TRUE),
                    "Más industrializado", "Menos industrializado")
  )

panel_indexado <- datos_panel %>%
  left_join(nivel_industrial %>% select(codigo_pais, grupo),
            by = "codigo_pais")

# Estadisticas de las variables principales, en gral y x grupo

datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados, anio == 2023) %>%
  select(pbi_pc, pbi_indust_pc, coef_gini) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  group_by(variable) %>%
  summarise(
    n       = n(),
    media   = mean(valor),
    mediana = median(valor),
    desvio  = sd(valor),
    minimo  = min(valor),
    maximo  = max(valor)
  ) %>%
  write.csv("output/tablas/tabla_descriptiva_general.csv", row.names = FALSE)

# Lo mismo pero separado por grupo, para los 3 indicadores principales

datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados, anio == 2023) %>%
  left_join(nivel_industrial %>% select(codigo_pais, grupo), by = "codigo_pais") %>%
  filter(!is.na(grupo)) %>%
  select(grupo, pbi_pc, pbi_indust_pc, coef_gini) %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  group_by(grupo, variable) %>%
  summarise(
    n       = n(),
    media   = mean(valor),
    mediana = median(valor),
    desvio  = sd(valor),
    minimo  = min(valor),
    maximo  = max(valor),
    .groups = "drop"
  ) %>%
  arrange(variable, grupo) %>%
  write.csv("output/tablas/tabla_descriptiva_por_grupo.csv", row.names = FALSE)

# Descriptivas del Gini

datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados, !is.na(coef_gini)) %>%
  summarise(
    n       = n(),
    media   = mean(coef_gini),
    mediana = median(coef_gini),
    desvio  = sd(coef_gini),
    minimo  = min(coef_gini),
    maximo  = max(coef_gini)
  ) %>%
  write.csv("output/tablas/tabla_descriptiva_gini.csv", row.names = FALSE)

# Robustez: comparacion con y sin China
# Cada fila se duplica via pivot_longer con una bandera de inclusion.
# filter(incluir) elimina China del escenario "sin China".

datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados, anio == 2023) %>%
  left_join(nivel_industrial %>% select(codigo_pais, grupo), by = "codigo_pais") %>%
  filter(!is.na(grupo)) %>%
  mutate(con_china = TRUE,
         sin_china = codigo_pais != "CHN") %>%
  pivot_longer(cols = c(con_china, sin_china),
               names_to = "muestra", values_to = "incluir") %>%
  filter(incluir) %>%
  mutate(muestra = if_else(muestra == "con_china", "Con China", "Sin China")) %>%
  group_by(muestra, grupo) %>%
  summarise(
    n          = n(),
    media_pbi  = mean(pbi_pc, na.rm = TRUE),
    media_ind  = mean(pbi_indust_pc, na.rm = TRUE),
    media_gini = mean(coef_gini, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(muestra, grupo) %>%
  write.csv("output/tablas/tabla_robustez_china.csv", row.names = FALSE)

# MÉTODO 1 — Indexación y trayectorias (H1)

base_1970 <- panel_indexado %>%
  filter(anio == 1970) %>%
  group_by(grupo) %>%
  summarise(base = mean(pbi_pc, na.rm = TRUE))

trayectorias <- panel_indexado %>%
  filter(!is.na(pbi_pc), !is.na(grupo)) %>%
  group_by(grupo, anio) %>%
  summarise(pbi_pc_mean = mean(pbi_pc, na.rm = TRUE), .groups = "drop") %>%
  left_join(base_1970, by = "grupo") %>%
  mutate(pbi_pc_idx = pbi_pc_mean / base * 100)

trayectorias %>%
  filter(anio %in% c(1970, 1990, 2010, 2023), !is.na(grupo)) %>%
  select(grupo, anio, pbi_pc_idx) %>%
  pivot_wider(names_from = anio, values_from = pbi_pc_idx) %>%
  write.csv("output/tablas/tabla_trayectorias.csv", row.names = FALSE)

# MÉTODO 2 — Descomposición de series temporales (H1)
# Países representativos: China + Corea del Sur (mas industrializado)
# vs. Argentina + Brasil (menos industrializado). Antes el par menos
# industrializado era Argentina + España, pero España paso al grupo
# "mas industrializado" con el nuevo criterio de nivel, asi que se
# reemplazo por Brasil.

descomposicion_resultados <- datos_panel %>%
  filter(codigo_pais %in% paises_repr, !is.na(pbi_pc)) %>%
  arrange(codigo_pais, anio) %>%
  group_by(codigo_pais, pais) %>%
  mutate(
    tendencia = as.numeric(fitted(loess(pbi_pc ~ anio, span = 0.3))),
    residuo   = pbi_pc - tendencia
  ) %>%
  ungroup() %>%
  left_join(nivel_industrial %>% select(codigo_pais, grupo),
            by = "codigo_pais")

descomposicion_resultados %>%
  group_by(pais, grupo) %>%
  summarise(
    residuo_medio = mean(abs(residuo), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(grupo) %>%
  write.csv("output/tablas/tabla_residuos.csv", row.names = FALSE)


# MÉTODO 3A — Test de Welch (H1)
# Compara medias de crecimiento del PBI per cápita entre grupos

crecimiento_pbi <- datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados) %>%
  filter(anio %in% c(1970, 2023), !is.na(pbi_pc)) %>%
  select(codigo_pais, pais, anio, pbi_pc) %>%
  pivot_wider(names_from = anio, values_from = pbi_pc,
              names_prefix = "pbi_") %>%
  mutate(crecimiento_pct = (pbi_2023 - pbi_1970) / pbi_1970 * 100) %>%
  left_join(nivel_industrial %>% select(codigo_pais, grupo),
            by = "codigo_pais") %>%
  filter(!is.na(crecimiento_pct), !is.na(grupo))

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
# Polonia excluida x falta de PBI pc 70


welch_resultado <- t.test(crecimiento_pct ~ grupo,
                          data = crecimiento_pbi,
                          var.equal = FALSE)
print(welch_resultado)

shapiro.test(crecimiento_pbi$crecimiento_pct[crecimiento_pbi$grupo == "Más industrializado"])
shapiro.test(crecimiento_pbi$crecimiento_pct[crecimiento_pbi$grupo == "Menos industrializado"])

data.frame(
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

# MÉTODO 3B — Regresión OLS (H2)

años_ref <- c(1970, 1990, 2010, 2023)

# Se arma un data.frame con los 4 años de referencia para cada país,
# buscando el gini mas cercano disponible.
gini_temporal <- data.frame(año_ref = rep(años_ref, each = length(paises_seleccionados)),
                            codigo_pais = rep(paises_seleccionados, times = length(años_ref))) %>%
  left_join(datos_panel %>%
              filter(codigo_pais %in% paises_seleccionados, !is.na(coef_gini)) %>%
              select(codigo_pais, pais, anio, coef_gini),
            by = "codigo_pais",
            relationship = "many-to-many") %>%
  mutate(distancia = abs(anio - año_ref)) %>%
  group_by(codigo_pais, año_ref) %>%
  slice_min(distancia, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(codigo_pais, pais, año_ref, anio_real = anio, coef_gini)

datos_ols <- gini_temporal %>%
  left_join(datos_panel %>% select(codigo_pais, anio, pbi_indust_pc_idx),
            by = c("codigo_pais", "anio_real" = "anio"))

ols_resultado <- lm(coef_gini ~ pbi_indust_pc_idx, data = datos_ols)
print(summary(ols_resultado))

resumen_ols <- summary(ols_resultado)

ic <- confint(ols_resultado)

data.frame(
  termino        = rownames(ic),
  ic_inf         = ic[, 1],
  ic_sup         = ic[, 2],
  estimacion     = coef(ols_resultado),
  r_cuadrado     = resumen_ols$r.squared,
  r_cuadrado_adj = resumen_ols$adj.r.squared,
  p_valor        = resumen_ols$coefficients[, "Pr(>|t|)"]
) %>%
  write.csv("output/tablas/tabla_ols.csv", row.names = FALSE)

plot(ols_resultado, which = 1)


# Robustez regresión OLS con y sin China

ols_con_china <- lm(coef_gini ~ pbi_indust_pc_idx, data = datos_ols)
ols_sin_china <- lm(coef_gini ~ pbi_indust_pc_idx,
                    data = datos_ols %>% filter(codigo_pais != "CHN"))

resumen_con <- summary(ols_con_china)
resumen_sin <- summary(ols_sin_china)

data.frame(
  muestra        = c("Con China", "Sin China"),
  n              = c(nrow(datos_ols), nrow(datos_ols %>% filter(codigo_pais != "CHN"))),
  coef_pendiente = c(coef(ols_con_china)[["pbi_indust_pc_idx"]],
                     coef(ols_sin_china)[["pbi_indust_pc_idx"]]),
  r_cuadrado     = c(resumen_con$r.squared, resumen_sin$r.squared),
  p_valor        = c(resumen_con$coefficients["pbi_indust_pc_idx", "Pr(>|t|)"],
                     resumen_sin$coefficients["pbi_indust_pc_idx", "Pr(>|t|)"])
) %>%
  write.csv("output/tablas/tabla_robustez_china_ols.csv", row.names = FALSE)

# Datos faltantes x variable
datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados) %>%
  summarise(across(c(pbi_pc, coef_gini, poblacion, exp_servicios, pbi_indust_pc_idx),
                   ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_faltantes") %>%
  mutate(pct_faltantes = round(n_faltantes / nrow(datos_panel) * 100, 1)) %>%
  write.csv("output/tablas/tabla_faltantes.csv", row.names = FALSE)

# Outliers por criterio IQR
datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados) %>%
  select(pbi_indust_pc_idx, coef_gini, pbi_pc) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  group_by(variable) %>%
  summarise(
    q1         = quantile(valor, 0.25),
    q3         = quantile(valor, 0.75),
    iqr        = IQR(valor),
    lim_inf    = q1 - 1.5 * iqr,
    lim_sup    = q3 + 1.5 * iqr,
    n_outliers = sum(valor < lim_inf | valor > lim_sup)
  ) %>%
  write.csv("output/tablas/tabla_outliers.csv", row.names = FALSE)

cat("\nArchivos guardados en output/tablas/:\n")
list.files("output/tablas/")

# Criterio viejo de clasificación (variación del índice industrial entre 1970 y 2023)
variacion_industrial <- datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados) %>%
  filter(anio %in% c(1970, 2023)) %>%
  select(codigo_pais, pais, anio, pbi_indust_pc_idx) %>%
  pivot_wider(names_from = anio, values_from = pbi_indust_pc_idx, names_prefix = "idx_") %>%
  mutate(
    variacion_idx = idx_2023 - idx_1970,
    grupo = if_else(variacion_idx >= median(variacion_idx, na.rm = TRUE),
                    "Más industrializado", "Menos industrializado")
  ) %>%
  select(codigo_pais, pais, grupo)

#escritura de archivos
write.csv(panel_indexado, "input/panel_indexado.csv", row.names = FALSE)
write.csv(nivel_industrial, "auxiliar/auxiliar_grupos.csv", row.names = FALSE)
write.csv(variacion_industrial, "auxiliar/auxiliar_grupos_viejo.csv", row.names = FALSE)
write.csv(gini_temporal, "input/gini_temporal.csv", row.names = FALSE)

