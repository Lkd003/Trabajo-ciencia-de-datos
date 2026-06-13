
# Gráfico comunicacional y exploratorio


library(tidyverse)
library(ggtext)
library(ggrepel)
library(patchwork)

# Paleta y tema 
owid_azul <- "#4C7A9C"
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

# GRÁFICO COMUNICACIONAL | PBI per cápita indexado por grupo

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
  filter(!is.na(pbi_pc)) %>%
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
    caption  = "Fuente: Argendata (Fundar) y Banco Mundial (WDI). Grupos definidos por variación del índice de PBI industrial per cápita 1970–2023."
  ) +
  theme_owid()

ggsave("output/graficos/grafico_comunicacional.png", g1,
       width = 10, height = 6, dpi = 300, bg = "white")


# GRÁFICO 2 — EXPLORATORIO
# Scatter Gini vs. índice industrial | Se agrega 2 pares más para evitar a China como outlier

paises_destacados <- c("CHN", "KOR", "DEU", "ARG", "BRA", "ESP")

scatter_data2 <- gini_temporal %>%
  left_join(variacion_industrial %>% select(codigo_pais, grupo),
            by = "codigo_pais") %>%
  left_join(
    datos_panel %>% select(codigo_pais, anio, pbi_indust_pc_idx),
    by = c("codigo_pais", "anio_real" = "anio")
  ) %>%
  mutate(
    destacado = codigo_pais %in% paises_destacados,
    letra = case_when(
      año_ref == 1970 ~ "A", año_ref == 1990 ~ "B",
      año_ref == 2010 ~ "C", año_ref == 2023 ~ "D"
    )
  )

pares <- list(
  list(paises = c("CHN", "KOR"), nombre = "Chn + Kor"),
  list(paises = c("KOR", "DEU"), nombre = "Kor + Deu"),
  list(paises = c("ARG", "BRA"), nombre = "Arg + Bra"),
  list(paises = c("ARG", "ESP"), nombre = "Arg + Esp")
)

promedios <- map_dfr(pares, function(p) {
  scatter_data2 %>%
    filter(codigo_pais %in% p$paises) %>%
    group_by(año_ref) %>%
    summarise(
      coef_gini = mean(coef_gini, na.rm = TRUE),
      pbi_indust_pc_idx = mean(pbi_indust_pc_idx, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      par = p$nombre,
      letra = case_when(
        año_ref == 1970 ~ "A", año_ref == 1990 ~ "B",
        año_ref == 2010 ~ "C", año_ref == 2023 ~ "D"
      )
    )
})

titulo_g2 <- sprintf(
  "Mayor industrialización se asocia con <span style='color:%s'>**menor desigualdad**</span>",
  owid_azul)

g2 <- ggplot() +
  geom_text(data = scatter_data2 %>% filter(!destacado),
            aes(x = pbi_indust_pc_idx, y = coef_gini, label = letra),
            color = "gray60", size = 2.5, alpha = 0.8,
            fontface = "bold",
            show.legend = FALSE) +
  geom_smooth(data = scatter_data2,
              aes(x = pbi_indust_pc_idx, y = coef_gini),
              method = "lm", se = TRUE,
              color = "gray50", fill = "gray90",
              linewidth = 0.7, linetype = "dashed") +
  geom_line(data = promedios,
            aes(x = pbi_indust_pc_idx, y = coef_gini,
                color = par, linetype = par),
            linewidth = 1) +
  geom_text(data = promedios,
            aes(x = pbi_indust_pc_idx, y = coef_gini,
                color = par, label = letra),
            size = 5, fontface = "bold",
            show.legend = FALSE) +
  geom_text_repel(
    data = promedios %>% filter(año_ref == 2023),
    aes(x = pbi_indust_pc_idx, y = coef_gini,
        label = par, color = par),
    size = 3.2, fontface = "bold",
    nudge_y = 1, box.padding = 0.4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "Chn + Kor" = owid_azul, "Kor + Deu" = owid_azul,
    "Arg + Bra" = owid_rojo, "Arg + Esp" = owid_rojo
  )) +
  scale_linetype_manual(values = c(
    "Chn + Kor" = "solid", "Kor + Deu" = "dashed",
    "Arg + Bra" = "solid", "Arg + Esp" = "dashed"
  )) +
  guides(color = guide_legend(title = "Par"),
         linetype = guide_legend(title = "Par")) +
  scale_x_log10(labels = scales::comma) +
  labs(
    title    = titulo_g2,
    subtitle = "Eje X: índice de PBI industrial per cápita (base 100 = 1970, escala log). Eje Y: coeficiente de Gini.<br>**A** = 1970 · **B** = 1990 · **C** = 2010 · **D** = último año disponible.",
    caption  = "Fuente: Argendata (Fundar) y Banco Mundial (WDI).",
    x = "Índice industrial per cápita (log)",
    y = "Coeficiente de Gini"
  ) +
  theme_owid() +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 9, face = "bold"),
    legend.text     = element_text(size = 8),
    axis.title      = element_text(size = 9, color = "gray40")
  )

ggsave("output/graficos/grafico_exploratorio.png", g2,
       width = 11, height = 7, dpi = 300, bg = "white")

cat("\nGráficos guardados en output/graficos/:\n")
list.files("output/graficos/")