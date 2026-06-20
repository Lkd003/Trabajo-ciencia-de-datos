# =============================================================================
# 04_graficos.R
# Gráfico comunicacional y exploratorio
# Ciencia de Datos — Grupo 11
# =============================================================================

library(tidyverse)
library(ggtext)
library(ggrepel)

# ── Paleta y tema ─────────────────────────────────────────────────────────────
owid_azul <- "#4C6A9C"
owid_rojo <- "#B13507"
owid_gris <- "#C9C9C9"

theme_owid <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_markdown(face = "bold", size = rel(1.3),
                                       colour = "#1d1d1d", lineheight = 1.2,
                                       margin = margin(b = 4)),
      plot.subtitle = element_markdown(size = rel(0.98), colour = "#5b5b5b",
                                       margin = margin(b = 16)),
      plot.caption  = element_markdown(hjust = 0, size = rel(0.72),
                                       colour = "#8a8a8a", margin = margin(t = 14)),
      axis.title    = element_blank(),
      axis.text     = element_text(colour = "#5b5b5b"),
      axis.ticks    = element_blank(),
      panel.grid.major.y = element_line(colour = "#e6e6e6", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "none",
      plot.margin = margin(t = 14, r = 100, b = 10, l = 16)
    )
}

# ── Datos base (usa lo generado en 03_analisis.R) ─────────────────────────────
datos_panel      <- read.csv("input/datos_panel.csv")
nivel_industrial <- read.csv("auxiliar/auxiliar_grupos.csv")
gini_temporal    <- read.csv("input/gini_temporal.csv")

paises_seleccionados <- c("ARG","BRA","MEX","CHL","COL",
                          "USA","DEU","GBR","FRA","TUR",
                          "CHN","IDN","KOR","ESP","CRI")

panel_indexado <- datos_panel %>%
  left_join(nivel_industrial %>% select(codigo_pais, grupo),
            by = "codigo_pais")

# =============================================================================
# GRÁFICO 1 — COMUNICACIONAL
# Trayectorias de PBI per cápita indexado por grupo
# =============================================================================

benchmark_mundial <- datos_panel %>%
  filter(codigo_pais %in% paises_seleccionados) %>%
  group_by(anio) %>%
  summarise(pbi_pc_idx = mean(pbi_pc, na.rm = TRUE)) %>%
  mutate(pbi_pc_idx = pbi_pc_idx / first(pbi_pc_idx) * 100,
         grupo = "Promedio de los 15 países")

base_1970 <- panel_indexado %>%
  filter(anio == 1970) %>%
  group_by(grupo) %>%
  summarise(base = mean(pbi_pc, na.rm = TRUE))

trayectorias <- panel_indexado %>%
  filter(!is.na(pbi_pc), !is.na(grupo)) %>%
  group_by(grupo, anio) %>%
  summarise(pbi_pc_mean = mean(pbi_pc, na.rm = TRUE), .groups = "drop") %>%
  left_join(base_1970, by = "grupo") %>%
  mutate(pbi_pc_idx = pbi_pc_mean / base * 100) %>%
  select(anio, grupo, pbi_pc_idx)

datos_grafico1 <- bind_rows(trayectorias, benchmark_mundial)

punto_1990 <- datos_grafico1 %>%
  filter(anio == 1990, grupo == "Menos industrializado") %>%
  pull(pbi_pc_idx)

titulo_g1 <- sprintf(
  "Los países <span style='color:%s'>**más industrializados**</span> crecieron más del doble que el <span style='color:%s'>**resto**</span>",
  owid_azul, owid_rojo)

g1 <- ggplot(datos_grafico1, aes(x = anio, y = pbi_pc_idx, color = grupo)) +
  geom_line(aes(linewidth = grupo)) +
  scale_linewidth_manual(values = c(
    "Más industrializado"       = 1.4,
    "Menos industrializado"     = 1.2,
    "Promedio de los 15 países" = 0.7
  )) +
  scale_color_manual(values = c(
    "Más industrializado"       = owid_azul,
    "Menos industrializado"     = owid_rojo,
    "Promedio de los 15 países" = owid_gris
  )) +
  annotate("text", x = 2024, y = 375,
           label = "Más\nindustrializado", hjust = 0, size = 3,
           fontface = "bold", color = owid_azul) +
  annotate("text", x = 2024, y = 240,
           label = "Menos\nindustrializado", hjust = 0, size = 3,
           fontface = "bold", color = owid_rojo) +
  annotate("text", x = 2024, y = 280,
           label = "Promedio\n15 países", hjust = 0, size = 3,
           color = "gray60") +
  annotate("segment",
           x = 1990, xend = 1996,
           y = punto_1990, yend = punto_1990 - 40,
           linewidth = 0.45, colour = "#555555",
           arrow = arrow(length = unit(2.2, "mm"),
                         type = "closed", ends = "first")) +
  annotate("text",
           x = 1997, y = punto_1990 - 40,
           hjust = 0, size = 3, lineheight = 0.95, colour = "#555555",
           label = "Desde 1990 la brecha\nse abre sostenidamente") +
  scale_x_continuous(breaks = seq(1970, 2020, by = 10),
                     expand = expansion(mult = c(0.01, 0.22))) +
  scale_y_continuous(breaks = seq(100, 400, by = 50)) +
  labs(
    title    = titulo_g1,
    subtitle = "PBI per cápita indexado (base 100 = 1970). Promedio simple por grupo de países.",
    caption  = "Fuente: Argendata (Fundar) y Banco Mundial (WDI).<br>Grupos definidos por nivel del índice de PBI industrial per cápita en 2023."
  ) +
  theme_owid()

ggsave("output/graficos/grafico_comunicacional.png", g1,
       width = 10, height = 6, dpi = 300, bg = "white")

# =============================================================================
# GRÁFICO 2 — EXPLORATORIO
# Scatter Gini vs. índice industrial con pares destacados
# =============================================================================

paises_destacados <- c("CHN", "KOR", "ARG", "BRA")

scatter_data2 <- gini_temporal %>%
  left_join(nivel_industrial %>% select(codigo_pais, grupo),
            by = "codigo_pais") %>%
  left_join(
    datos_panel %>% select(codigo_pais, anio, pbi_indust_pc_idx),
    by = c("codigo_pais", "anio_real" = "anio")
  ) %>%
  mutate(
    destacado = codigo_pais %in% paises_destacados,
    par = case_when(
      codigo_pais == "CHN" ~ "China",
      codigo_pais == "KOR" ~ "Corea del Sur",
      codigo_pais == "ARG" ~ "Argentina",
      codigo_pais == "BRA" ~ "Brasil",
      TRUE ~ "Otros"
    ),
    color_grupo = case_when(
      codigo_pais %in% c("CHN", "KOR") ~ "Más industrializado",
      codigo_pais %in% c("ARG", "BRA") ~ "Menos industrializado",
      TRUE ~ "Otros"
    )
  )

promedios <- scatter_data2 %>%
  filter(destacado) %>%
  group_by(color_grupo, año_ref) %>%
  summarise(
    coef_gini         = mean(coef_gini, na.rm = TRUE),
    pbi_indust_pc_idx = mean(pbi_indust_pc_idx, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(color_grupo, año_ref) %>%
  mutate(par = case_when(
    color_grupo == "Más industrializado"   ~ "Chn + Kor",
    color_grupo == "Menos industrializado" ~ "Arg + Bra"
  ))

titulo_g2 <- sprintf(
  "Mayor industrialización se asocia con <span style='color:%s'>**menor desigualdad**</span>",
  owid_azul)

g2 <- ggplot() +
  geom_point(data = scatter_data2 %>% filter(!destacado),
             aes(x = pbi_indust_pc_idx, y = coef_gini),
             color = "gray80", size = 2, alpha = 0.6) +
  geom_text(data = scatter_data2 %>% filter(!destacado),
            aes(x = pbi_indust_pc_idx, y = coef_gini,
                label = case_when(
                  año_ref == 1970 ~ "A",
                  año_ref == 1990 ~ "B",
                  año_ref == 2010 ~ "C",
                  año_ref == 2023 ~ "D")),
            color = "gray70", size = 2.5, fontface = "bold") +
  geom_smooth(data = scatter_data2,
              aes(x = pbi_indust_pc_idx, y = coef_gini),
              method = "lm", se = TRUE,
              color = "gray50", fill = "gray90",
              linewidth = 0.7, linetype = "dashed") +
  # geom_path en vez de geom_line: respeta el orden A-B-C-D de las filas
  # sin reordenar por el valor del eje X (necesario porque el indice
  # industrial no crece de forma monotona en el tiempo para algunos paises)
  geom_path(data = promedios,
            aes(x = pbi_indust_pc_idx, y = coef_gini,
                color = color_grupo, group = color_grupo),
            linewidth = 1) +
  geom_text(data = promedios,
            aes(x = pbi_indust_pc_idx, y = coef_gini,
                color = color_grupo,
                label = case_when(
                  año_ref == 1970 ~ "A",
                  año_ref == 1990 ~ "B",
                  año_ref == 2010 ~ "C",
                  año_ref == 2023 ~ "D")),
            size = 5, fontface = "bold") +
  geom_text_repel(
    data = scatter_data2 %>% filter(destacado, año_ref == 2023),
    aes(x = pbi_indust_pc_idx, y = coef_gini,
        label = par, color = color_grupo),
    size = 3, fontface = "bold",
    box.padding = 0.5, nudge_y = 1.5,
    show.legend = FALSE
  ) +
  annotate("text",
           x = promedios %>%
             filter(color_grupo == "Menos industrializado", año_ref == 2023) %>%
             pull(pbi_indust_pc_idx),
           y = promedios %>%
             filter(color_grupo == "Menos industrializado", año_ref == 2023) %>%
             pull(coef_gini) + 2.5,
           label = "Arg + Bra", hjust = 0.5, size = 3,
           fontface = "bold", color = owid_rojo) +
  annotate("text",
           x = promedios %>%
             filter(color_grupo == "Más industrializado", año_ref == 2023) %>%
             pull(pbi_indust_pc_idx) * 1.15,
           y = promedios %>%
             filter(color_grupo == "Más industrializado", año_ref == 2023) %>%
             pull(coef_gini) + 2.5,
           label = "Chn + Kor", hjust = 0, size = 3,
           fontface = "bold", color = owid_azul) +
  annotate("point", x = 400, y = 63, color = owid_azul, size = 3) +