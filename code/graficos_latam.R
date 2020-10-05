library(janitor)
library(futurevisions)
library(highcharter)
library(EpiEstim)
library(tidyverse)
library(magrittr)
library(furrr)
library(future)

plan(multiprocess) # activar procesamiento paralelo

source("code/limpieza_bases.R")
source("code/funciones.R")


# vector de países a remover
quitar <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Guyana", "Suriname", 
            "Trinidad and Tobago", "Dominica", "Grenada", "Saint Kitts and Nevis", "Saint Lucia",
            "Saint Vincent and the Grenadines", "Jamaica")


codigos <- read_csv("input/codigos_covid_paises.csv") %>% 
  remove_empty() %>% 
  filter(
    region_es == "América Latina y el Caribe",
    !country_region %in% quitar
  ) %>% 
  dplyr::select(
    pais = pais_region,
    pais_region = country_region,
    pais_nombre_corto,
    continente
  ) 

#----------------------------------
# Rt
#----------------------------------
df_mundo %>% 
  filter(pais_region %in% codigos$pais_region) %>% 
  filter(base == "confirmados") -> temp


# partir bases para aplicar funciones
temp %>% 
  group_split(base, pais_region) -> temp_latam

# aplicar funciones: epistim_intervalo_1 == ICL
furrr::future_map(temp_latam, epistim_intervalo_1, .progress = T) %>% bind_rows() -> estim_1_latam

estim_1_latam %>% 
  group_split(`País o Región`) %>% 
  map(., ~arrange(., `Día de cierre`)) %>% 
  map(., ~slice(., nrow(.))) %>% 
  bind_rows() %>% 
  arrange(Promedio) %>% 
  clean_names() -> aa

# grafico rt para confirmados
highchart() %>% 
  hc_add_series(data = aa, type = "errorbar",
                hcaes(x = pais_o_region, low = limite_inferior, high = limite_superior),
                color = "#CB1724",  id = "error",
                stemWidth = 3,  whiskerLength = 0) %>% 
  hc_add_series(data = aa, "scatter", hcaes(x = pais_o_region, y = promedio), 
                color = "#CB1724", name = "Rt", linkedTo = "error") %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_plotOptions(
    scatter = list(
      marker = list(radius = 7, enabled = T, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>% 
  hc_xAxis(title = list(text = ""), 
           categories = aa$pais_o_region) %>% 
  hc_yAxis(title = list(text = "Rt"), min = 0, 
           plotLines = list(
             list(label = list(text = "Rt = 1"),
                  color = "#09283C",
                  width = 2,
                  value = 1))) %>% 
  hc_tooltip(enabled = T, valueDecimals = 3, borderWidth = 0.01,
             pointFormat=paste("<b>{point.pais_o_region}</b>
                               Rt last week: <b>{point.promedio}</b><br>
                               Higher interval: <b>{point.limite_superior}</b><br>
                               Lower interval: <b>{point.limite_inferior}</b><br>
                               Measurement starting day: <b>{point.dia_de_inicio}</b><br>
                               Measurement closing day: <b>{point.dia_de_cierre}</b><br>"), 
             headerFormat = "<b>{point.pais_o_region}</b>", 
             style = list(fontFamily = "Open Sans")) -> rt_confirmados 


# rt para fallecidos
df_mundo %>% 
  filter(pais_region %in% codigos$pais_region) %>% 
  filter(base == "fallecidos") -> temp

# partir bases para aplicar funciones
temp %>% 
  group_split(base, pais_region) -> temp_latam

# aplicar funciones: epistim_intervalo_1 == ICL
furrr::future_map(temp_latam, epistim_intervalo_1, .progress = T) %>% bind_rows() -> estim_1_latam

estim_1_latam %>% 
  group_split(`País o Región`) %>% 
  map(., ~arrange(., `Día de cierre`)) %>% 
  map(., ~slice(., nrow(.))) %>% 
  bind_rows() %>% 
  arrange(Promedio) %>% 
  clean_names() -> aa

# grafico rt fallecidos
highchart() %>% 
  hc_add_series(data = aa, type = "errorbar",
                hcaes(x = pais_o_region, low = limite_inferior, high = limite_superior),
                color = "#09283C",  id = "error",
                stemWidth = 3,  whiskerLength = 0) %>% 
  hc_add_series(data = aa, "scatter", hcaes(x = pais_o_region, y = promedio), 
                color = "#09283C", name = "Rt", linkedTo = "error") %>% 
  hc_chart(style = list(fontFamily = "Open Sans")) %>% 
  hc_plotOptions(
    scatter = list(
      marker = list(radius = 7, enabled = T, symbol = "circle"),
      states = list(hover = list(halo = list(size = 1)))
    )
  ) %>% 
  hc_xAxis(title = list(text = ""), 
           categories = aa$pais_o_region) %>% 
  hc_yAxis(title = list(text = "Rt"), min = 0, 
           plotLines = list(
             list(label = list(text = "Rt = 1"),
                  color = "#CB1724",
                  width = 2,
                  value = 1))) %>% 
  hc_tooltip(enabled = T, valueDecimals = 3, borderWidth = 0.01,
             pointFormat=paste("<b>{point.pais_o_region}</b><br>
                               Rt last week: <b>{point.promedio}</b><br>
                               Higher interval: <b>{point.limite_superior}</b><br>
                               Lower interval: <b>{point.limite_inferior}</b><br>
                               Measurement starting day: <b>{point.dia_de_inicio}</b><br>
                               Measurement closing day: <b>{point.dia_de_cierre}</b><br>"), 
             headerFormat = "<b>{point.pais_o_region}</b>") -> rt_fallecidos
  

# serie de tiempo de Rt
df_mundo %>% 
  filter(pais_region %in% codigos$pais_region) -> temp 

# partir bases para aplicar funciones
temp %>% 
  group_split(base, pais_region) -> temp_latam

# aplicar funciones: epistim_intervalo_1 == ICL
furrr::future_map(temp_latam, epistim_intervalo_1, .progress = T) %>% bind_rows() -> estim_1_latam

estim_1_latam %>% 
  janitor::clean_names() %>% 
  mutate(
    base = str_replace(base, "confirmados", "Confirmed"),
    base = str_replace(base, "fallecidos", "Deaths")
  ) %>% 
  group_split(pais_o_region) -> estim_1_latam

# graficar
map(estim_1_latam, rt_tiempo) -> graficos




