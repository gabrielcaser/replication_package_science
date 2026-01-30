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
rm(sivep_full, df_population, sivep)