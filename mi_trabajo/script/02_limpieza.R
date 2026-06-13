library(tidyverse)

# Datos crudos 

datos_wdi <- read.csv("raw/wdi_raw.csv")

datos_argendata <- read.csv("raw/pib_indust_per_capita_comparado.csv")

# Visualizacion de datos 

glimpse(datos_wdi)
dim(datos_wdi)
str(datos_wdi)
summary(datos_wdi)

glimpse(datos_argendata)
dim(datos_argendata)
str(datos_argendata)
summary(datos_argendata)


#ver tabla de paises 
unique(datos_wdi$country)

unique(datos_argendata$geonombreFundar)


#Renombre y limpieza de WDI
datos_wdi_limpio <- datos_wdi %>%
  rename(
    codigo_pais    = iso3c,
    pais           = country,
    anio           = year,
    pbi_pc         = pbi_pc,
    coef_gini      = gini,
    poblacion      = poblacion,
    exp_servicios  = exp_servicios,
    region         = region,
    nivel_ingreso  = income
  ) %>%
  filter(anio >= 1970 & anio <= 2023) %>%
  filter(!is.na(codigo_pais)) %>%
  select(codigo_pais, pais, anio, pbi_pc, coef_gini,
         poblacion, exp_servicios, region, nivel_ingreso)

# Renombre y limpieza de ARGENDATA
datos_argendata_limpio <- datos_argendata %>%
  rename(
    codigo_pais        = geocodigoFundar,
    pais               = geonombreFundar,
    anio               = anio,
    pbi_indust_pc      = gdp_indust_pc,
    pbi_indust_pc_idx  = gdp_indust_pc_indice
  ) %>%
  filter(anio >= 1970 & anio <= 2023) %>%
  select(codigo_pais, anio, pbi_indust_pc, pbi_indust_pc_idx)

#Union de tablas
datos_panel <- datos_wdi_limpio %>%
  left_join(datos_argendata_limpio, by = c("codigo_pais", "anio"))

write.csv(datos_panel, "input/datos_panel.csv", row.names = FALSE)
