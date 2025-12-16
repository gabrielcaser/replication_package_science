# Program - This program creates a dataset with baseline variables (health, ideology, demography) and non-pharmaceutical interventions (NPIs) outcomes

# Libs --------------------------------------------------------------------

library(tidyverse)
library(skimr)
library(readxl)

# Health data -------------------------------------------------------------

df_health <- read.csv2(paste0(data_dir, "/raw/221130_16_indicadores_mun_data_ieps_2019_2015.csv"), sep = ",") # source: https://iepsdata.org.br/data-downloads

## cleaning

df_health <- df_health %>%
  filter(ano == 2015) %>% # keeping only one year before elections
  rename(id_municipio = codmun,
         coorte = ano)

df_health <- df_health %>% 
  summarise(id_municipio, tx_med, tx_med_ch, tx_enf, tx_enf_ch, pct_desp_recp_saude_mun, cob_esf, tx_leito_sus, idhm, renda_pc = pib_cte_pc, per_populacao_urbana = 1 - pct_rural, per_populacao_homens = pct_pop_masc) # keeping only a few 

df_health$id_municipio <- as.character(df_health$id_municipio)

## saving

saveRDS(df_health, paste0(data_dir, "/intermediary/health_data.Rds"))

# Political scores --------------------------------------------------------
## source: https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/8USPML/20URAD&version=2.0

df_political <- read.csv2(paste0(data_dir, "/raw/223007_partidos_ano_v2018.csv"), sep = ";")

## cleaning

df_political$ANO_ELEICAO <- ifelse(df_political$ANO_ELEICAO == 2018, 2020, df_political$ANO_ELEICAO)

df_political <- df_political %>% 
  filter(ANO_ELEICAO == 2016 | ANO_ELEICAO == 2020) %>% 
  mutate(sigla_partido = recode(SIGLA_PARTIDO,
                                "SD" = "SOLIDARIEDADE",
                                "PPS" = "CIDADANIA",
                                "PRB" = "REPUBLICANOS",
                                "PMDB" = "MDB",
                                "PEN" = "PATRIOTA",
                                "PR" = "PL",
                                "PTN" = "PODE",
                                "PSDC" = "DC",
                                "PT do B" = "AVANTE",
                                .default = SIGLA_PARTIDO)) %>% 
  transmute(sigla_partido, ideology_party = as.numeric(IDEO_IMPUTED), coorte = ANO_ELEICAO)

## saving

saveRDS(df_political, paste0(data_dir, "/intermediary/political_data.Rds"))


# Municipality ideology ---------------------------------------------------
## source: https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/8USPML/20URAD&version=2.0

df_mun_ideo <- read.csv(paste0(data_dir, "/raw/221011_municipal_ideology.csv"), sep = ",")

## cleaning

df_mun_ideo$GEOCODIG_M = substr(df_mun_ideo$GEOCODIG_M,1,6)

df_mun_ideo$year <- ifelse(df_mun_ideo$year == 2018, 2020, df_mun_ideo$year)

df_mun_ideo <- df_mun_ideo %>% 
  transmute(id_municipio = GEOCODIG_M, coorte = year, ideology_municipality = ideo_na)

## saving

saveRDS(df_mun_ideo, paste0(data_dir, "/intermediary/ideology_data.Rds"))


# Non-Pharmaceutical Interventions (NPI) ----------------------------------

df_npi <- read_excel(paste0(data_dir, "/raw/220927_cnm_medidas_sanitarias.xlsx"))

df_npi <- df_npi %>% 
  transmute(id_municipio = substr(Ibge,1,6),
            barreiras_sanitarias = as.numeric(as.factor(df_npi$`Q1. Barreiras sanitárias (posto de monitoramento de entrada e saída de pessoas no Município)`)) - 1,
            mascaras = as.numeric(as.factor(df_npi$`Q4. Uso obrigatório de máscaras faciais.`)) - 1,
            restricao_atv_nao_essenciais = as.numeric(as.factor(df_npi$`Q3. Medidas de isolamento social, permitindo APENAS serviços essenciais.`)) - 1,
            restricao_circulacao = as.numeric(as.factor(df_npi$`Q2. Medidas restritivas para diminuição da circulação/aglomeração de pessoas.`)) - 1,
            restricao_transporte_publico = as.numeric(as.factor(df_npi$`Q5. Foram adotadas medidas de redução na oferta de transporte público?`)) - 1)

df_npi <- df_npi %>%
  group_by(id_municipio) %>% 
  mutate(total_nfi = sum(barreiras_sanitarias, mascaras, restricao_atv_nao_essenciais, restricao_circulacao, restricao_transporte_publico, na.rm = FALSE)) %>% 
  ungroup()

## saving

saveRDS(df_npi, paste0(data_dir, "/intermediary/npi_data.Rds"))


# Density data ------------------------------------------------------------

df_densidade <- read_excel(paste0(data_dir, "/raw/densidade_2015.xls"))

df_densidade$Codigo = substr(df_densidade$Codigo,1,6)

df_densidade <- df_densidade %>% 
  transmute(id_municipio = Codigo, densidade)

## saving

saveRDS(df_densidade, paste0(data_dir, "/intermediary/density_data.Rds"))


# STEM classification -----------------------------------------------------

df_stem <- read_excel(paste0(data_dir, '/raw/stem_classification.xlsx'))

## cleaning - create cbo_agregado with leading zeros for values less than 4 digits
df_stem <- df_stem %>%
  mutate(cbo_agregado = sprintf("%04d", as.numeric(code_cbo2002)))

df_stem_eleito <- df_stem %>% 
  reframe(cbo_agregado_eleito = cbo_agregado, 
          cbo_agregado_nome_caser.laverde.rothwell_eleito = stem_classification_caser.laverde.rothwell)

df_stem_naoeleito <- df_stem %>% 
  reframe(cbo_agregado_naoeleito = cbo_agregado, 
          cbo_agregado_nome_caser.laverde.rothwell_eleito_naoeleito = stem_classification_caser.laverde.rothwell)

## saving

saveRDS(df_stem_eleito, paste0(data_dir, "/intermediary/stem_classification_eleito.Rds"))
saveRDS(df_stem_naoeleito, paste0(data_dir, "/intermediary/stem_classification_naoeleito.Rds"))

print("Baseline data created successfully!")
