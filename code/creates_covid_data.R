### 2020 ------------------------------------------------------------------
### source: https://opendatasus.saude.gov.br/dataset/srag-2020
### cleaning based on: Escola de Dados -> https://escoladedados.org/coda2021/

formato_data <- "%d/%m/%Y" # defining date type


# defining data types

sivep_2020 <- read_csv2(paste0(data_dir, '/raw/220613_srag_base_oficial_2020.csv'), col_types = cols(
  DT_NOTIFIC = col_date(format = formato_data),
  SEM_NOT = col_double(),
  DT_SIN_PRI = col_date(format = formato_data),
  SEM_PRI = col_double(),
  SG_UF_NOT = col_character(),
  ID_REGIONA = col_character(),
  CO_REGIONA = col_double(),
  ID_MUNICIP = col_character(),
  CO_MUN_NOT = col_double(),
  ID_UNIDADE = col_character(),
  CO_UNI_NOT = col_double(),
  CS_SEXO = col_character(),
  DT_NASC = col_date(format = formato_data),
  NU_IDADE_N = col_double(),
  TP_IDADE = col_double(),
  COD_IDADE = col_double(),
  CS_GESTANT = col_double(),
  CS_RACA = col_double(),
  CS_ETINIA = col_character(),
  CS_ESCOL_N = col_double(),
  ID_PAIS = col_character(),
  CO_PAIS = col_double(),
  SG_UF = col_character(),
  ID_RG_RESI = col_character(),
  CO_RG_RESI = col_double(),
  ID_MN_RESI = col_character(),
  CO_MUN_RES = col_double(),
  CS_ZONA = col_double(),
  SURTO_SG = col_double(),
  NOSOCOMIAL = col_double(),
  AVE_SUINO = col_double(),
  FEBRE = col_double(),
  TOSSE = col_double(),
  GARGANTA = col_double(),
  DISPNEIA = col_double(),
  DESC_RESP = col_double(),
  SATURACAO = col_double(),
  DIARREIA = col_double(),
  VOMITO = col_double(),
  OUTRO_SIN = col_double(),
  OUTRO_DES = col_character(),
  PUERPERA = col_double(),
  FATOR_RISC = col_double(),
  CARDIOPATI = col_double(),
  HEMATOLOGI = col_double(),
  SIND_DOWN = col_double(),
  HEPATICA = col_double(),
  ASMA = col_double(),
  DIABETES = col_double(),
  NEUROLOGIC = col_double(),
  PNEUMOPATI = col_double(),
  IMUNODEPRE = col_double(),
  RENAL = col_double(),
  OBESIDADE = col_double(),
  OBES_IMC = col_double(),
  OUT_MORBI = col_double(),
  MORB_DESC = col_character(),
  VACINA = col_double(),
  DT_UT_DOSE = col_date(format = formato_data),
  MAE_VAC = col_double(),
  DT_VAC_MAE = col_date(format = formato_data),
  M_AMAMENTA = col_double(),
  DT_DOSEUNI = col_date(format = formato_data),
  DT_1_DOSE = col_date(format = formato_data),
  DT_2_DOSE = col_date(format = formato_data),
  ANTIVIRAL = col_double(),
  TP_ANTIVIR = col_double(),
  OUT_ANTIV = col_character(),
  DT_ANTIVIR = col_date(format = formato_data),
  HOSPITAL = col_double(),
  DT_INTERNA = col_date(format = formato_data),
  SG_UF_INTE = col_character(),
  ID_RG_INTE = col_character(),
  CO_RG_INTE = col_double(),
  ID_MN_INTE = col_character(),
  CO_MU_INTE = col_double(),
  UTI = col_double(),
  DT_ENTUTI = col_date(format = formato_data),
  DT_SAIDUTI = col_date(format = formato_data),
  SUPORT_VEN = col_double(),
  RAIOX_RES = col_double(),
  RAIOX_OUT = col_double(),
  DT_RAIOX = col_date(format = formato_data),
  AMOSTRA = col_double(),
  DT_COLETA = col_date(format = formato_data),
  TP_AMOSTRA = col_double(),
  OUT_AMOST = col_character(),
  PCR_RESUL = col_double(),
  DT_PCR = col_date(format = formato_data),
  POS_PCRFLU = col_double(),
  TP_FLU_PCR = col_double(),
  PCR_FLUASU = col_double(),
  FLUASU_OUT = col_character(),
  PCR_FLUBLI = col_double(),
  FLUBLI_OUT = col_character(),
  POS_PCROUT = col_double(),
  PCR_VSR = col_double(),
  PCR_PARA1 = col_double(),
  PCR_PARA2 = col_double(),
  PCR_PARA3 = col_double(),
  PCR_PARA4 = col_double(),
  PCR_ADENO = col_double(),
  PCR_METAP = col_double(),
  PCR_BOCA = col_double(),
  PCR_RINO = col_double(),
  PCR_OUTRO = col_double(),
  DS_PCR_OUT = col_character(),
  CLASSI_FIN = col_double(),
  CLASSI_OUT = col_character(),
  CRITERIO = col_double(),
  EVOLUCAO = col_double(),
  DT_EVOLUCA = col_date(format = formato_data),
  DT_ENCERRA = col_date(format = formato_data),
  DT_DIGITA = col_date(format = formato_data),
  HISTO_VGM = col_double(),
  PAIS_VGM = col_character(),
  CO_PS_VGM = col_double(),
  LO_PS_VGM = col_character(),
  DT_VGM = col_date(format = formato_data),
  DT_RT_VGM = col_date(format = formato_data),
  PCR_SARS2 = col_double(),
  PAC_COCBO = col_character(),
  PAC_DSCBO = col_character(),
  OUT_ANIM = col_character(),
  DOR_ABD = col_double(),
  FADIGA = col_double(),
  PERD_OLFT = col_double(),
  PERD_PALA = col_double(),
  TOMO_RES = col_double(),
  TOMO_OUT = col_double(),
  DT_TOMO = col_date(format = formato_data),
  TP_TES_AN = col_double(),
  DT_RES_AN = col_date(format = formato_data),
  RES_AN = col_double(),
  POS_AN_FLU = col_double(),
  TP_FLU_AN = col_double(),
  POS_AN_OUT = col_double(),
  AN_SARS2 = col_double(),
  AN_VSR = col_double(),
  AN_PARA1 = col_double(),
  AN_PARA2 = col_double(),
  AN_PARA3 = col_double(),
  AN_ADENO = col_double(),
  AN_OUTRO = col_double(),
  DS_AN_OUT = col_character(),
  TP_AM_SOR = col_double(),
  SOR_OUT = col_character(),
  DT_CO_SOR = col_date(format = formato_data),
  TP_SOR = col_double(),
  OUT_SOR = col_character(),
  DT_RES = col_date(format = formato_data),
  RES_IGG = col_double(),
  RES_IGM = col_double(),
  RES_IGA = col_double(),
  ESTRANG = col_double(),
  VACINA_COV = col_double(),
  DOSE_1_COV = col_date(format = formato_data),
  DOSE_2_COV = col_date(format = formato_data),
  FABRICANTE = col_double(),
  LOTE_1_COV = col_character(),
  LOTE_2_COV = col_character()
))


# Filtering data

sivep_2020 <- sivep_2020 %>% 
  filter(SG_UF != "IGNORADO" & SG_UF != "EXTERIOR")

# Recategorizing

sivep_2020 <- sivep_2020 %>%
  mutate(
    SG_UF = recode(SG_UF, 
                   "DISTRITO FEDERAL" = "DF"),
    CLASSI_FIN = recode(CLASSI_FIN,
                        '1' = 'SRAG influenza',
                        '2' = 'SRAG outros vírus',
                        '3' = 'SRAG outros agentes',
                        '4' = 'SRAG não especificado',
                        '5' = 'SRAG COVID-19'),
    EVOLUCAO = recode(EVOLUCAO,
                      '1' = 'Cura',
                      '2' = 'Óbito',
                      '3' = 'Óbito por outras causas',
                      '9' = 'Ignorado'),
    HOSPITAL = recode(HOSPITAL,
                      '1' = 'Sim',
                      '2' = 'Não',
                      '9' = 'Ignorado'),
    CS_SEXO = recode(CS_SEXO,
                     'I' = 'Ignorado',
                     'M' = 'Masculino',
                     'F' = 'Feminino'),
    CS_RACA = recode(CS_RACA,
                     '1' = 'Branca',
                     '2' = 'Preta',
                     '3' = 'Amarela',
                     '4' = 'Parda',
                     '5' = 'Indígena',
                     '9' = 'Ignorado')
  )


# defining death date

sivep_2020 <- sivep_2020 %>% 
  mutate(death_date = ifelse(EVOLUCAO == "Óbito" | EVOLUCAO == "Óbito por outras causas", DT_EVOLUCA, NA))


# Saving

saveRDS(sivep_2020, paste0(data_dir, "/intermediary/sivep_2020.rds"))



### 2021 ------------------------------------------------------------------
### source: https://opendatasus.saude.gov.br/dataset/srag-2021-e-2022
### cleaning based on: Escola de Dados -> https://escoladedados.org/coda2021/

formato_data <- "%d/%m/%Y" # defining date type


# defining data types

sivep_2021 <- read_csv2(paste0(data_dir, '/raw/230831_srag_base_oficial_2021.csv'), col_types = cols(
  DT_NOTIFIC = col_date(format = formato_data),
  SEM_NOT = col_double(),
  DT_SIN_PRI = col_date(format = formato_data),
  SEM_PRI = col_double(),
  SG_UF_NOT = col_character(),
  ID_REGIONA = col_character(),
  CO_REGIONA = col_double(),
  ID_MUNICIP = col_character(),
  CO_MUN_NOT = col_double(),
  ID_UNIDADE = col_character(),
  CO_UNI_NOT = col_double(),
  CS_SEXO = col_character(),
  DT_NASC = col_date(format = formato_data),
  NU_IDADE_N = col_double(),
  TP_IDADE = col_double(),
  COD_IDADE = col_double(),
  CS_GESTANT = col_double(),
  CS_RACA = col_double(),
  CS_ETINIA = col_character(),
  CS_ESCOL_N = col_double(),
  ID_PAIS = col_character(),
  CO_PAIS = col_double(),
  SG_UF = col_character(),
  ID_RG_RESI = col_character(),
  CO_RG_RESI = col_double(),
  ID_MN_RESI = col_character(),
  CO_MUN_RES = col_double(),
  CS_ZONA = col_double(),
  SURTO_SG = col_double(),
  NOSOCOMIAL = col_double(),
  AVE_SUINO = col_double(),
  FEBRE = col_double(),
  TOSSE = col_double(),
  GARGANTA = col_double(),
  DISPNEIA = col_double(),
  DESC_RESP = col_double(),
  SATURACAO = col_double(),
  DIARREIA = col_double(),
  VOMITO = col_double(),
  OUTRO_SIN = col_double(),
  OUTRO_DES = col_character(),
  PUERPERA = col_double(),
  FATOR_RISC = col_double(),
  CARDIOPATI = col_double(),
  HEMATOLOGI = col_double(),
  SIND_DOWN = col_double(),
  HEPATICA = col_double(),
  ASMA = col_double(),
  DIABETES = col_double(),
  NEUROLOGIC = col_double(),
  PNEUMOPATI = col_double(),
  IMUNODEPRE = col_double(),
  RENAL = col_double(),
  OBESIDADE = col_double(),
  OBES_IMC = col_double(),
  OUT_MORBI = col_double(),
  MORB_DESC = col_character(),
  VACINA = col_double(),
  DT_UT_DOSE = col_date(format = formato_data),
  MAE_VAC = col_double(),
  DT_VAC_MAE = col_date(format = formato_data),
  M_AMAMENTA = col_double(),
  DT_DOSEUNI = col_date(format = formato_data),
  DT_1_DOSE = col_date(format = formato_data),
  DT_2_DOSE = col_date(format = formato_data),
  ANTIVIRAL = col_double(),
  TP_ANTIVIR = col_double(),
  OUT_ANTIV = col_character(),
  DT_ANTIVIR = col_date(format = formato_data),
  HOSPITAL = col_double(),
  DT_INTERNA = col_date(format = formato_data),
  SG_UF_INTE = col_character(),
  ID_RG_INTE = col_character(),
  CO_RG_INTE = col_double(),
  ID_MN_INTE = col_character(),
  CO_MU_INTE = col_double(),
  UTI = col_double(),
  DT_ENTUTI = col_date(format = formato_data),
  DT_SAIDUTI = col_date(format = formato_data),
  SUPORT_VEN = col_double(),
  RAIOX_RES = col_double(),
  RAIOX_OUT = col_double(),
  DT_RAIOX = col_date(format = formato_data),
  AMOSTRA = col_double(),
  DT_COLETA = col_date(format = formato_data),
  TP_AMOSTRA = col_double(),
  OUT_AMOST = col_character(),
  PCR_RESUL = col_double(),
  DT_PCR = col_date(format = formato_data),
  POS_PCRFLU = col_double(),
  TP_FLU_PCR = col_double(),
  PCR_FLUASU = col_double(),
  FLUASU_OUT = col_character(),
  PCR_FLUBLI = col_double(),
  FLUBLI_OUT = col_character(),
  POS_PCROUT = col_double(),
  PCR_VSR = col_double(),
  PCR_PARA1 = col_double(),
  PCR_PARA2 = col_double(),
  PCR_PARA3 = col_double(),
  PCR_PARA4 = col_double(),
  PCR_ADENO = col_double(),
  PCR_METAP = col_double(),
  PCR_BOCA = col_double(),
  PCR_RINO = col_double(),
  PCR_OUTRO = col_double(),
  DS_PCR_OUT = col_character(),
  CLASSI_FIN = col_double(),
  CLASSI_OUT = col_character(),
  CRITERIO = col_double(),
  EVOLUCAO = col_double(),
  DT_EVOLUCA = col_date(format = formato_data),
  DT_ENCERRA = col_date(format = formato_data),
  DT_DIGITA = col_date(format = formato_data),
  HISTO_VGM = col_double(),
  PAIS_VGM = col_character(),
  CO_PS_VGM = col_double(),
  LO_PS_VGM = col_character(),
  DT_VGM = col_date(format = formato_data),
  DT_RT_VGM = col_date(format = formato_data),
  PCR_SARS2 = col_double(),
  PAC_COCBO = col_character(),
  PAC_DSCBO = col_character(),
  OUT_ANIM = col_character(),
  DOR_ABD = col_double(),
  FADIGA = col_double(),
  PERD_OLFT = col_double(),
  PERD_PALA = col_double(),
  TOMO_RES = col_double(),
  TOMO_OUT = col_double(),
  DT_TOMO = col_date(format = formato_data),
  TP_TES_AN = col_double(),
  DT_RES_AN = col_date(format = formato_data),
  RES_AN = col_double(),
  POS_AN_FLU = col_double(),
  TP_FLU_AN = col_double(),
  POS_AN_OUT = col_double(),
  AN_SARS2 = col_double(),
  AN_VSR = col_double(),
  AN_PARA1 = col_double(),
  AN_PARA2 = col_double(),
  AN_PARA3 = col_double(),
  AN_ADENO = col_double(),
  AN_OUTRO = col_double(),
  DS_AN_OUT = col_character(),
  TP_AM_SOR = col_double(),
  SOR_OUT = col_character(),
  DT_CO_SOR = col_date(format = formato_data),
  TP_SOR = col_double(),
  OUT_SOR = col_character(),
  DT_RES = col_date(format = formato_data),
  RES_IGG = col_double(),
  RES_IGM = col_double(),
  RES_IGA = col_double(),
  ESTRANG = col_double(),
  VACINA_COV = col_double(),
  DOSE_1_COV = col_date(format = formato_data),
  DOSE_2_COV = col_date(format = formato_data),
  FABRICANTE = col_double(),
  LOTE_1_COV = col_character(),
  LOTE_2_COV = col_character(),
  DOSE_REF = col_date(format = formato_data),
  LOTE_REF = col_character(),
  FAB_COVREF = col_double()
))


# Filtering data

sivep_2021 <- sivep_2021 %>% 
  filter(SG_UF != "IGNORADO" & SG_UF != "EXTERIOR")

# Recategorizing

sivep_2021 <- sivep_2021 %>%
  mutate(
    SG_UF = recode(SG_UF, 
                   "DISTRITO FEDERAL" = "DF"),
    CLASSI_FIN = recode(CLASSI_FIN,
                        '1' = 'SRAG influenza',
                        '2' = 'SRAG outros vírus',
                        '3' = 'SRAG outros agentes',
                        '4' = 'SRAG não especificado',
                        '5' = 'SRAG COVID-19'),
    EVOLUCAO = recode(EVOLUCAO,
                      '1' = 'Cura',
                      '2' = 'Óbito',
                      '3' = 'Óbito por outras causas',
                      '9' = 'Ignorado'),
    HOSPITAL = recode(HOSPITAL,
                      '1' = 'Sim',
                      '2' = 'Não',
                      '9' = 'Ignorado'),
    CS_SEXO = recode(CS_SEXO,
                     'I' = 'Ignorado',
                     'M' = 'Masculino',
                     'F' = 'Feminino'),
    CS_RACA = recode(CS_RACA,
                     '1' = 'Branca',
                     '2' = 'Preta',
                     '3' = 'Amarela',
                     '4' = 'Parda',
                     '5' = 'Indígena',
                     '9' = 'Ignorado')
  )


# defining death date

sivep_2021 <- sivep_2021 %>% 
  mutate(death_date = ifelse(EVOLUCAO == "Óbito" | EVOLUCAO == "Óbito por outras causas", DT_EVOLUCA, NA))


# Saving

saveRDS(sivep_2021, paste0(data_dir, "/intermediary/sivep_2021.rds"))


### Joining ------------------------------------------------------------------


sivep_2020 <- readRDS(paste0(data_dir, "/intermediary/sivep_2020.rds"))
sivep_2021 <- readRDS(paste0(data_dir, "/intermediary/sivep_2021.rds"))

sivep_full <- bind_rows(sivep_2020, sivep_2021)
rm(sivep_2020, sivep_2021)


# checking municipalities

length(unique(sivep_full$ID_MN_RESI)) # 200 municipalities missing.
length(unique(sivep_full$CO_MUN_RES)) 



### Saving ------------------------------------------------------------------

saveRDS(sivep_full, paste0(data_dir, "/intermediary/sivep_full.rds"))

df_covid <- sivep_full %>%
  filter((CLASSI_FIN == "SRAG COVID-19" | CLASSI_FIN == "SRAG não especificado") & (EVOLUCAO == "Óbito" | HOSPITAL == "Sim")) %>% 
  reframe(CLASSI_FIN, id_municipio = as.character(CO_MUN_RES), DT_SIN_PRI, EVOLUCAO, HOSPITAL)
saveRDS(df_covid, paste0(data_dir, "/intermediary/covid_day_data.rds"))

rm(sivep_full, df_covid)