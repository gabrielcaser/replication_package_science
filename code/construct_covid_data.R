### Creating Outcome Variables ----------------------------------------------

# Open parquet
sivep_full <- arrow::read_parquet(paste0(data_dir, "/intermediary/covid_day.parquet"))

# Load population data (outside the loop to avoid reloading)
df_population <- read.csv2(paste0(data_dir, "/raw/populacao.csv"), sep = ",") # source: https://iepsdata.org.br/data-downloads

## merging year of population with coorte
df_population <- df_population %>%
  mutate(coorte = recode(ano, '2020' = '2016', '2021' = '2020')) %>% 
  summarise(coorte = as.double(coorte), populacao, id_municipio = as.character(id_municipio), sigla_uf)

# Define date adjustments to create different datasets
date_adjustments <- c(-30, -15, 0, 15, 30)

# Loop through each date adjustment
for (days_adjustment in date_adjustments) {
  
  # Calculate adjusted end date for cohort 2016
  end_date_adjusted <- as.Date(end_date_covid_2016) + days_adjustment
  
  # Print progress
  print(paste0("Processing dataset with ", days_adjustment, " days adjustment. End date: ", end_date_adjusted))
  
  # Filter data for cohort 2016 (2020)
  sivep_2020 <- sivep_full %>%
    filter((CLASSI_FIN == "SRAG COVID-19" | CLASSI_FIN == "SRAG não especificado") & 
             DT_SIN_PRI >= start_date_covid_2016 & DT_SIN_PRI <= end_date_adjusted) %>%
    group_by(id_municipio) %>%
    summarise(deaths = sum(EVOLUCAO == "Óbito", na.rm = TRUE),
              hosp = sum(HOSPITAL == "Sim", na.rm = TRUE),
              coorte = 2016) %>%
    arrange(desc(deaths)) 
  
  # Filter data for cohort 2020 (2021)
  sivep_2021 <- sivep_full %>%
    filter((CLASSI_FIN == "SRAG COVID-19") & DT_SIN_PRI > end_date_adjusted & DT_SIN_PRI <= end_date_covid_2020) %>% 
    group_by(id_municipio) %>%
    summarise(deaths = sum(EVOLUCAO == "Óbito", na.rm = TRUE),
              hosp = sum(HOSPITAL == "Sim", na.rm = TRUE),
              coorte = 2020) %>%
    arrange(desc(deaths)) 
  
  # Join cohorts
  sivep <- bind_rows(sivep_2020, sivep_2021)
  rm(sivep_2020, sivep_2021)
  
  # Rename columns
  sivep <- sivep %>% 
    rename(deaths_sivep = deaths, hosp_sivep = hosp)
  
  # Create delta outcomes
  sivep <- sivep %>%
    group_by(id_municipio) %>% 
    arrange(coorte) %>% 
    mutate(delta_deaths_sivep = deaths_sivep - lag(deaths_sivep, n = 1, default = NA),
           delta_hosp_sivep = hosp_sivep - lag(hosp_sivep, n = 1, default = NA)) %>% 
    arrange(desc(coorte))
  
  # Merge with population data
  sivep <- sivep %>% 
    ungroup()
  
  sivep <- left_join(sivep, df_population, by = c("id_municipio", "coorte"))
  
  # Create outcome variables per 100k inhabitants
  sivep <- sivep %>% 
    dplyr::group_by(id_municipio, coorte) %>%
    dplyr::mutate(hosp_per_100k_inhabitants = (hosp_sivep / populacao) * 100000,
                  deaths_sivep_per_100k_inhabitants = (deaths_sivep / populacao) * 100000) %>% 
    ungroup()
  
  # Save with appropriate filename
  if (days_adjustment < 0) {
    filename <- paste0(data_dir, "/intermediary/covid_data_minus_", abs(days_adjustment), "days.rds")
  } else if (days_adjustment > 0) {
    filename <- paste0(data_dir, "/intermediary/covid_data_plus_", days_adjustment, "days.rds")
  } else {
    filename <- paste0(data_dir, "/intermediary/covid_data.rds")
  }
  
  saveRDS(sivep, filename)
  print(paste0("Saved: ", filename))
}

# Clean up
rm(sivep_full, sivep)

# Process 2015 data for placebo
library(bigrquery)
project_id <- "econometria-314719"

query_2015 <- "
SELECT 
  id_municipio_6_notificacao AS id_municipio,
  data_primeiros_sintomas AS DT_SIN_PRI,
  evolucao_caso AS EVOLUCAO,
  internacao AS HOSPITAL,
  classificacao_final AS CLASSI_FIN
FROM `basedosdados.br_ms_sinan.microdados_influenza_srag`
WHERE ano = 2015
"

sivep_2015_full <- bq_table_download(bq_project_query(project_id, query_2015))

# Process similar to COVID
sivep_2015_full <- sivep_2015_full %>%
  mutate(CLASSI_FIN = recode(CLASSI_FIN,
                             "1" = "Influenza",
                             "2" = "Outros vírus",
                             "3" = "Outros agentes",
                             "4" = "Não especificado",
                             "5" = "COVID-19",
                             .default = as.character(CLASSI_FIN)
  )) %>%
  mutate(EVOLUCAO = recode(EVOLUCAO,
                           "1" = "Cura",
                           "2" = "Óbito",
                           "3" = "Óbito por outras causas",
                           "9" = "Ignorado",
                           .default = as.character(EVOLUCAO)
  )) %>%
  mutate(HOSPITAL = recode(HOSPITAL,
                           "1" = "Sim",
                           "2" = "Não",
                           "9" = "Ignorado",
                           .default = as.character(HOSPITAL)
  )) %>%
  mutate(EVOLUCAO = replace_na(EVOLUCAO, "Em andamento")) %>%
  mutate(id_municipio = as.character(id_municipio))

# Aggregate for 2015 data (for placebo, assign to coorte 2016)
sivep_2015 <- sivep_2015_full %>%
  filter(EVOLUCAO == "Óbito" | HOSPITAL == "Sim") %>%
  group_by(id_municipio) %>%
  summarise(deaths = sum(EVOLUCAO == "Óbito", na.rm = TRUE),
            hosp = sum(HOSPITAL == "Sim", na.rm = TRUE),
            coorte = 2016) %>%  # Assign to 2016 for placebo
  arrange(desc(deaths))

# Rename columns
sivep_2015 <- sivep_2015 %>% 
  rename(deaths_sivep = deaths, hosp_sivep = hosp)

# Create delta outcomes (though not used for placebo)
sivep_2015 <- sivep_2015 %>%
  mutate(delta_deaths_sivep = deaths_sivep,
         delta_hosp_sivep = hosp_sivep)

# Merge with population data (use 2020 population for 2015, as approximation)
df_population_2015 <- df_population %>% filter(coorte == 2016)  # Use 2016 population
sivep_2015 <- left_join(sivep_2015, df_population_2015, by = c("id_municipio", "coorte"))

# Create outcome variables per 100k inhabitants
sivep_2015 <- sivep_2015 %>% 
  mutate(hosp_per_100k_inhabitants_2015 = (hosp_sivep / populacao) * 100000,
         deaths_sivep_per_100k_inhabitants_2015 = (deaths_sivep / populacao) * 100000)

# Save
saveRDS(sivep_2015, paste0(data_dir, "/intermediary/2015_data.rds"))

# Clean up
rm(sivep_2015_full, sivep_2015, df_population_2015)