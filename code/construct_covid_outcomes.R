### Creating Outcome Variables -------------------------------------------------------------------


library(arrow)

# Loading data as data.table with filters applied immediately
sivep_full <- setDT(readRDS(paste0(data_dir, "/intermediary/sivep_full.rds")))

# Read from parquet with filters (much faster for subsequent runs)
sivep_full <- setDT(read_parquet(
    paste0(data_dir, "/intermediary/sivep_full.parquet"),
    as_data_frame = TRUE,
    col_select = NULL,
    filters = list(
        c("CLASSI_FIN", "%in%", c("SRAG COVID-19", "SRAG não especificado")),
        c("DT_SIN_PRI", ">=", start_date_covid_2016),
        c("DT_SIN_PRI", "<=", end_date_covid_2020)
    )
))

# Filter relevant classifications and date range upfront to reduce memory usage
sivep_full <- sivep_full[CLASSI_FIN %in% c("SRAG COVID-19", "SRAG não especificado") & 
                         DT_SIN_PRI >= start_date_covid_2016 & 
                         DT_SIN_PRI <= end_date_covid_2020]

# Create coorte 2016
sivep_2020 <- sivep_full[DT_SIN_PRI >= start_date_covid_2016 & DT_SIN_PRI <= end_date_covid_2016,
                         .(deaths = sum(EVOLUCAO == "Óbito", na.rm = TRUE),
                           hosp = sum(HOSPITAL == "Sim", na.rm = TRUE),
                           coorte = 2016),
                         by = CO_MUN_RES][order(-deaths)]

# Create coorte 2020
sivep_2021 <- sivep_full[CLASSI_FIN == "SRAG COVID-19" & 
                         DT_SIN_PRI > end_date_covid_2016 & 
                         DT_SIN_PRI <= end_date_covid_2020,
                         .(deaths = sum(EVOLUCAO == "Óbito", na.rm = TRUE),
                           hosp = sum(HOSPITAL == "Sim", na.rm = TRUE),
                           coorte = 2020),
                         by = CO_MUN_RES][order(-deaths)]

# Free memory
rm(sivep_full)
gc()

# Combine cohorts
sivep <- rbindlist(list(sivep_2020, sivep_2021))
rm(sivep_2020, sivep_2021)

# Rename columns
setnames(sivep, 
         old = c("CO_MUN_RES", "deaths", "hosp"),
         new = c("id_municipio", "deaths_sivep", "hosp_sivep"))

# Convert id_municipio to character
sivep[, id_municipio := as.character(id_municipio)]

# Create delta outcomes
setorder(sivep, id_municipio, coorte)
sivep[, `:=`(delta_deaths_sivep = deaths_sivep - shift(deaths_sivep, n = 1, type = "lag"),
             delta_hosp_sivep = hosp_sivep - shift(hosp_sivep, n = 1, type = "lag")),
      by = id_municipio]
setorder(sivep, -coorte)

# Load and prepare population data
df_population <- fread(paste0(data_dir, "/raw/populacao.csv"), sep = ",") # source: https://iepsdata.org.br/data-downloads

# Recode year to cohort
df_population[, coorte := fcase(ano == 2020, 2016,
                                ano == 2021, 2020)]
df_population[, `:=`(coorte = as.numeric(coorte),
                     id_municipio = as.character(id_municipio))]
df_population <- df_population[, .(coorte, populacao, id_municipio, sigla_uf)]

# Merge with population data
sivep <- df_population[sivep, on = .(id_municipio, coorte)]

# Create outcome variables per 100k inhabitants
sivep[, `:=`(hosp_per_100k_inhabitants = (hosp_sivep / populacao) * 100000,
             deaths_sivep_per_100k_inhabitants = (deaths_sivep / populacao) * 100000)]

# Saving
saveRDS(sivep, paste0(data_dir, "/intermediary/covid_data.rds"))
