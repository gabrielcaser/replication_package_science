# This program run moderation analysis
set.seed(1234)

# Oppening ----------------------------------------------------------------

df <- readRDS(paste(data_dir, "/final/", data, sep = ""))

# Merging with Revenue data

data_revenue <- readxl::read_excel(paste(data_dir,"/raw/ipea_receitas_municipais.xlsx", sep = ""), sheet = "Receitas Totais")

data_revenue <- data_revenue %>% 
  summarise(id_municipio = Cod.IBGE,
            receita_2015 = `2015` / 10000000)

df <- merge(df, data_revenue, by = "id_municipio")

rm(data_revenue)

# Regression OLS with moderator

## Creating interaction term

df$tenure <- df$tenure / 12

#df_subset <- subset(df, X >= -1 * janela & X <= janela)
df_subset <- df

df_subset$inter_receita_stem <- df_subset$receita_2015 * (as.double(df_subset$stem_background) - 1)
df_subset$log_tenure         <- log(df_subset$tenure + 1)
df_subset$inter_tenure_stem  <- df_subset$tenure  * (as.double(df_subset$stem_background) - 1)
df_subset$inter_ideo_stem    <- df_subset$ideology_party * (as.double(df_subset$stem_background) - 1)
df_subset$inter_X_stem       <- df_subset$X * (as.double(df_subset$stem_background) - 1)
df_subset$zero               <- 0

# Applying Callonico package for heterogeneous analysis
library("rdhte")

state.f = factor(df_subset$sigla_uf)

state.d = model.matrix(~state.f+0)


year.f = factor(df_subset$coorte)

if (cohort_filter == "") {
  year.d = model.matrix(~year.f+0)
}
if (cohort_filter == "2016_") {
  year.d = 1
}

covsZ <- cbind(
  as.factor(df_subset$coorte),
  as.factor(df_subset$sigla_uf),
  df_subset$mulher,
  df_subset$ideology_party,
  df_subset$instrucao,
  df_subset$reeleito,
  df_subset$idade,
  df_subset$idade * df_subset$idade
)

# Trocando 0 em log_tenure por numero aleatorio entre 0 e 1
#df_subset$log_tenure[df_subset$log_tenure == 0] <- runif(sum(df_subset$log_tenure == 0), 0, 0.3)



he_1 <- rdhte(y        = df_subset$Y_deaths_sivep,
              x        = df_subset$X,
              covs.hte = df_subset$receita_2015,
              covs.eff = covsZ,
              kernel   = k,
              bwselect = "mserd",
              p        = poli
)
summary(he_1)

he_2 <- rdhte(y        = df_subset$Y_hosp,
              x        = df_subset$X,
              covs.hte = df_subset$receita_2015,
              covs.eff = covsZ,
              kernel   = k,
              p        = poli,
              bwselect = "mserd"
)
summary(he_2)

he_3 <- rdhte(y        = df_subset$total_nfi,
              x        = df_subset$X,
              covs.hte = df_subset$receita_2015,
              covs.eff = covsZ,
              kernel   = k,
              p        = poli,
              bwselect = "mserd"
             
)

summary(he_3)

# Testing tenure


he_4 <- rdhte(y        = df_subset$Y_deaths_sivep,
              x        = df_subset$X,
              covs.hte = df_subset$coorte,
              covs.eff = covsZ,
              kernel   = k,
              bwselect = "mserd"
)

summary(he_4)

he_5 <- rdhte(y        = df_subset$Y_hosp,
              x        = df_subset$X,
              covs.hte = df_subset$coorte,
              covs.eff = covsZ,
              kernel   = k,
              bwselect = "mserd",
              poli = 1
)

summary(he_5)

# FE


pdata <- pdata.frame(df_subset, c("coorte","sigla_uf"))

# Controles

covsZ = cbind(pdata$mulher,
              pdata$ideology_party,
              pdata$instrucao,
              pdata$reeleito,
              pdata$idade,
              pdata$idade * pdata$idade)

#covsZ <- cbind(pdata$zero)

# Final table
out1 <- plm(
  Y_hosp ~ X + T + T_X + inter_tenure_stem + tenure + covsZ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within" ,
  effect = "twoways"
)
out2 <- plm(
  Y_deaths_sivep ~ X + T + T_X + inter_tenure_stem + tenure + covsZ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within"  ,
  effect = "twoways"
)
out3 <- plm(
  total_nfi ~ X + T + T_X + inter_tenure_stem + tenure + covsZ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within" ,
  effect = "twoways"
)


moderation_tenure <- stargazer::stargazer(
  out2,
  out1,
  out3,
  type = "text",
  #type = "text",
  covariate.labels = c("STEM Background", "Tenure Moderation Effect"),
  dep.var.labels = c("Hospitalizations", "Deaths", "NPI"),
  out = paste(output_dir, "/tables/moderation_tenure.tex", sep = ""),
  title = "Moderating effects of scientific intensity on the impact of STEM background",
  omit = c("X", "T_X", "covsZ1", "covsZ2", "covsZ3", "covsZ4", "covsZ5", "covsZ6"),
  notes = NULL
)

writeLines(moderation_tenure, con = "outputs/tables/moderation_tenure.tex")

out4 <- plm(
  Y_hosp ~ X + T + T_X + receita_2015 + inter_receita_stem + covsZ ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within"
)
out5 <- plm(
  Y_deaths_sivep ~ X + T + T_X + receita_2015  + inter_receita_stem + covsZ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within"
)
out6 <- plm(
  total_nfi ~ X + T + T_X + receita_2015 + inter_receita_stem + covsZ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within"
)

moderation_revenue <- stargazer::stargazer(
  out5,
  out4,
  out6,
  type = "text",
  #type = "text",
  covariate.labels = c(
    "STEM Background",
    "Revenue Modereration Effect"
  ),
  dep.var.labels = c("Hospitalizations", "Deaths", "NPI"),
  title = "Moderating effects of cities’ development on the impact of STEM background",
  out = paste(output_dir, "/tables/moderation_revenue.tex", sep = ""),
  omit = c("X", "T_X", "covsZ1", "covsZ2", "covsZ3", "covsZ4", "covsZ5", "covsZ6", "receita_2015"),
  notes = NULL
)

writeLines(moderation_revenue, con = "outputs/tables/moderation_revenue.tex")


# Testing


out7 <- plm(
  Y_hosp ~ X + T + T_X + receita_2015 +  covsZ + inter_receita_stem + inter_tenure_stem ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within"
)
out8 <- plm(
  Y_deaths_sivep ~ X + T + T_X + receita_2015 + covsZ + inter_receita_stem + inter_tenure_stem,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within"
)
out9 <- plm(
  total_nfi ~ X + T + T_X + receita_2015 + covsZ  + inter_receita_stem + inter_tenure_stem,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within"
)

moderation_both <- stargazer::stargazer(
  out7,
  out8,
  out9,
  type = "text",
  covariate.labels = c(
    "STEM Education",
    "2015 City's Revenue Mod. Eff.",
    "STEM Work Tenure Mod. Eff."
  ),
  dep.var.labels = c("Hospitalizations", "Deaths", "NPI"),
  title = "Moderating effects of cities’ development and STEM work tenure on the impact of STEM Education",
  out = paste(output_dir, "/tables/moderation_both.tex", sep = ""),
  omit = c("X", "T_X", "covsZ1", "covsZ2", "covsZ3", "covsZ4", "covsZ5", "receita_2015"),
  notes = NULL
)

writeLines(moderation_both, con = "outputs/tables/moderation_both.tex")
