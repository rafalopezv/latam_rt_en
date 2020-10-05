# sobre: limpieza de bases para calculo de Rt semanal
library(R0)
library(tidyverse)
library(magrittr)
library(rvest)

#-------------------------
# limpieza datos globales
#-------------------------

# serie de tiempo
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_m <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

# descarga y limpieza
df <- read_csv(url) %>% mutate(base = "confirmados")
df_m <- read_csv(url_m) %>% mutate(base = "fallecidos")

df <- bind_rows(df, df_m) 
rm(df_m, url, url_m)

df %<>% 
  rename(pais_region = `Country/Region`) %>% 
  dplyr::select(-matches("Lat|Long")) %>% 
  mutate(pais_region = case_when(
    str_detect(string = `Province/State`, "Hong Kong") ~ "Hong Kong SAR, China",
    str_detect(string = `Province/State`, "Macau") ~ "Macao SAR, China",
    T ~ pais_region
  )) %>% 
  dplyr::select(-`Province/State`) %>% 
  gather(fecha, casos_acumulados, -pais_region, -base) 
  
df$fecha %<>% as.Date(., format = "%m/%d/%y")

# estandarización desde pacientes 0 e individualización de países en formato lista
df %>% 
  filter(casos_acumulados != 0) %>% 
  group_by(pais_region, base, fecha) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_split(pais_region, base) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., dias = 1:nrow(.),
                 total_semanas = nrow(.)/7)) %>% 
  map(., ~mutate_if(., is.numeric, round, 0)) %>% 
  bind_rows() %>% 
  arrange(base, pais_region, fecha) -> df

# añadir variable de número de semanas: extensión de un año 
tibble(
  semana = rep((1:52), 7)
) %>% 
  arrange(semana) %>% 
  mutate(dias = 1:nrow(.)) -> temp

df %<>% merge(., temp, all.x = T)
rm(temp)

# descarga de población por país: fuente Banco Mundial
poblacion <- read_csv("input/API_SP.POP.TOTL_DS2_en_csv_v2_1120881/API_SP.POP.TOTL_DS2_en_csv_v2_1120881.csv", skip = 3)
poblacion %<>% 
  dplyr::select(pais_region = `Country Name`, poblacion = `2018`) 

# compatibilidad d datos población con base JHU
(df$pais_region %>% unique)[!(df$pais_region %>% unique) %in%  (poblacion$pais_region %>% unique)]

poblacion$pais_region %<>% gsub("Bahamas\\, The", "Bahamas", .)
poblacion$pais_region %<>% gsub("Brunei Darussalam", "Brunei", .)
poblacion$pais_region %<>% gsub("Egypt\\, Arab Rep\\.", "Egypt", .)
poblacion$pais_region %<>% gsub("Gambia\\, The", "Gambia", .)
poblacion$pais_region %<>% gsub("Iran\\, Islamic Rep\\.", "Iran", .)
poblacion$pais_region %<>% gsub("Korea\\, Rep\\.", "Korea, South", .)
poblacion$pais_region %<>% gsub("Kyrgyz Republic", "Kyrgyzstan", .)
poblacion$pais_region %<>% gsub("Russian Federation", "Russia", .)
poblacion$pais_region %<>% gsub("St\\. Lucia", "Saint Lucia", .)
poblacion$pais_region %<>% gsub("St\\. Vincent and the Grenadines", "Saint Vincent and the Grenadines", .)
poblacion$pais_region %<>% gsub("Slovak Republic", "Slovakia", .)
poblacion$pais_region %<>% gsub("United States", "US", .)
poblacion$pais_region %<>% gsub("Venezuela\\, RB", "Venezuela", .)
poblacion$pais_region %<>% gsub("Syrian Arab Republic", "Syria", .)
poblacion$pais_region %<>% gsub("Lao PDR", "Laos", .)
poblacion$pais_region %<>% gsub("St. Kitts and Nevis", "Saint Kitts and Nevis", .)
poblacion$pais_region %<>% gsub("Yemen\\, Rep\\.", "Yemen", .)
poblacion$pais_region %<>% gsub("Myanmar", "Burma", .)
poblacion$pais_region %<>% gsub("Czech Republic", "Czechia", .)
poblacion$pais_region %<>% gsub("Congo\\, Dem\\. Rep\\.", "Congo (Kinshasa)", .)
poblacion$pais_region %<>% gsub("Congo\\, Rep\\.", "Congo (Brazzaville)", .)


# Población de Taiwán no está reportada en China: https://databank.worldbank.org/reports.aspx?source=2&type=metadata&series=SP.POP.TOTL

# filtro la base de poblacion respecto a la data de JHU
poblacion %>%  
  filter(pais_region %in% (df %>% pull(pais_region) %>% unique)) %>% 
  dplyr::select(pais_region, poblacion) -> poblacion

df %<>% 
  merge(., poblacion, all.x = T) %>% 
  arrange(base, pais_region, fecha) 

# completar poblaciones manualmente
df %>% 
  filter(is.na(poblacion)) %>% 
  pull(pais_region) %>% 
  unique


df[df$pais_region == "Eritrea", "poblacion"] <- 6081196 # fuente cia.gov, año 2020
df[df$pais_region == "Taiwan*", "poblacion"] <- 23603049 # fuente cia.gov, año 2020
df[df$pais_region == "Holy See", "poblacion"] <- 1000 # fuente cia.gov, año 2019
df[df$pais_region == "Western Sahara", "poblacion"] <- 652271 # fuente cia.gov, año 2020


(df$pais_region %>% unique)[!(df$pais_region %>% unique) %in%  (poblacion$pais_region %>% unique)]
rm(poblacion)

# quitar a: Diamond Princess y MS Zaandam
df %<>%  
  filter(!is.na(poblacion))

# dividir entre países/semanas
df %>% 
  group_split(base, pais_region) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., incidencia = lag(casos_acumulados),
                 incidencia = casos_acumulados - incidencia,
                 incidencia = abs(incidencia))) %>%
  bind_rows() %>% 
  filter(!is.na(incidencia)) -> df_mundo 

rm(df)
