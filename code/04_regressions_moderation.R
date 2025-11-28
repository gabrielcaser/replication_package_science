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

#df_subset <- subset(df, df$X >= -1 * 0.082 & df$X <= 0.082)
df_subset <- df
df_subset$inter_receita_stem <- df_subset$receita_2015 * (as.double(df_subset$stem_background) - 1)
df_subset$log_tenure         <- log(df_subset$tenure + 1)
df_subset$inter_tenure_stem  <- df_subset$tenure  * (as.double(df_subset$stem_background) - 1)
df_subset$inter_ideo_stem    <- df_subset$ideology_party * (as.double(df_subset$stem_background) - 1)
df_subset$inter_X_stem       <- df_subset$X * (as.double(df_subset$stem_background) - 1)
df_subset$zero               <- 0

# FE


pdata <- pdata.frame(df_subset, c("sigla_uf"))
pdata_subset <- pdata[pdata$stem_background == 1, ]
# Controles

covsZ = cbind(pdata$mulher,
              pdata$ideology_party,
              pdata$instrucao,
              pdata$reeleito,
              pdata$idade,
              pdata$idade * pdata$idade
              )

#covsZ <- cbind(pdata$zero)

# Final table

top1 <- plm(
  Y_deaths_sivep ~ stem_background + inter_tenure_stem ,
  data = pdata,
  index = c("sigla_uf"),
  model = "within" ,
  effect = "twoways"
)

top1_controls <- plm(
  Y_deaths_sivep ~ stem_background + inter_tenure_stem + mulher + ideology_party + reeleito + instrucao,
  data = pdata,
  index = c("sigla_uf"),
  model = "within" ,
  effect = "twoways"
)

top3 <- plm(
  Y_deaths_sivep ~  tenure,
  data = pdata_subset,
  index = c("sigla_uf"),
  model = "within" ,
  effect = "twoways"
)
top3_controls <- plm(
  Y_deaths_sivep ~  tenure + mulher + ideology_party + reeleito + instrucao ,
  data = pdata_subset,
  index = c("sigla_uf"),
  model = "within"  ,
  effect = "twoways"
)


moderation_tenure <- stargazer::stargazer(
  list(top1, top1_controls, top3, top3_controls),
  type = "text",
 # dep.var.labels = c("Deaths per 100k Inhabitants"),
  out = paste(output_dir, "/tables/moderation_tenure.tex", sep = ""),
  title = paste("Moderating effects of scientific intensity on the impact",
                "of STEM background"),
  covariate.labels = c("STEM Background", "STEM x Sci. Intensity", "Woman Mayor", "Ideology of the Party",
                       "Reelected", "Education Level", "Scientific Intensity"),
  notes = NULL#,
  #omit = c("covsZ1", "covsZ2", "covsZ3", "covsZ4")
)

writeLines(moderation_tenure, con = "outputs/tables/moderation_tenure.tex")

