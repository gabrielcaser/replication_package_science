### Creating Outcome Variables -------------------------------------------------------------------

# Loading data
sivep_full <- readRDS(paste0(data_dir, "/intermediary/sivep_full.rds"))

sivep_2020 <- sivep_full %>%
  filter((CLASSI_FIN == "SRAG COVID-19" | CLASSI_FIN == "SRAG não especificado") & DT_SIN_PRI >= start_date_covid_2016 & DT_SIN_PRI <= end_date_covid_2016) %>%
  group_by(CO_MUN_RES) %>%
  summarise(deaths = sum(EVOLUCAO == "Óbito", na.rm = TRUE),
            hosp = sum(HOSPITAL == "Sim", na.rm = TRUE),
            coorte = 2016) %>%
  arrange(desc(deaths))

sivep_2021 <- sivep_full %>%
  filter((CLASSI_FIN == "SRAG COVID-19") & DT_SIN_PRI > end_date_covid_2016 & DT_SIN_PRI <= end_date_covid_2020) %>%
  group_by(CO_MUN_RES) %>%
  summarise(deaths = sum(EVOLUCAO == "Óbito", na.rm = TRUE),
            hosp = sum(HOSPITAL == "Sim", na.rm = TRUE),
            coorte = 2020) %>%
  arrange(desc(deaths))


sivep <- bind_rows(sivep_2020, sivep_2021) # joining coortes
rm(sivep_2020, sivep_2021)
sivep <- sivep %>%
  rename(id_municipio = CO_MUN_RES)
sivep <- sivep %>%
  rename(deaths_sivep = deaths, hosp_sivep = hosp)

sivep <- sivep %>% # changing data type
  mutate(id_municipio = as.character(id_municipio))


sivep <- sivep %>% # creating delta outcomes
  group_by(id_municipio) %>%
  arrange(coorte) %>%
  mutate(delta_deaths_sivep = deaths_sivep - lag(deaths_sivep, n = 1, default = NA),
         delta_hosp_sivep = hosp_sivep - lag(hosp_sivep, n = 1, default = NA)) %>%
  arrange(desc(coorte))

# Merging with population data

df_population <- read.csv2(paste0(data_dir, "/raw/populacao.csv"), sep = ",") # source: https://iepsdata.org.br/data-downloads

## merging year of population with coorte
df_population <- df_population %>%
  mutate(coorte = recode(ano, '2020' = '2016', '2021' = '2020')) %>%
  summarise(coorte = as.double(coorte), populacao, id_municipio = as.character(id_municipio), sigla_uf)

sivep <- sivep %>%
  ungroup()

sivep <- left_join(sivep, df_population, by = c("id_municipio", "coorte"))


# Creating outcome variables

sivep <- sivep %>%
  dplyr::group_by(id_municipio, coorte) %>%
  dplyr::mutate(hosp_per_100k_inhabitants = (hosp_sivep / populacao) * 100000,
                deaths_sivep_per_100k_inhabitants = (deaths_sivep / populacao) * 100000) %>%
  ungroup()



# Saving

rm(sivep_full)

saveRDS(sivep, paste0(data_dir, "/intermediary/covid_data.rds"))
