# Description: This program creates sum stats

# Producing Mean between groups along time

## Opening main dataset
df <- readRDS(paste(data_dir, "/final/", data, sep = ""))
df_all_cohorts <- readRDS(paste(data_dir, "/final/", data_all_cohorts, sep = ""))

## Mantaining main vars
df_aux <- df_all_cohorts %>%
  dplyr::select(
    id_municipio,
    stem_background,
    coorte,
    X,
    tenure
  )

## Opening covid day data

df_mean <- readRDS(paste(data_dir, "/intermediary/covid_day_data.rds", sep = ""))

df_mean <- df_mean %>%
  filter(EVOLUCAO == "Óbito")

df_mean <- df_mean %>% 
  mutate(coorte = case_when(as.numeric(format(df_mean$DT_SIN_PRI, "%Y")) == 2020 ~ as.factor(2016),
                            as.numeric(format(df_mean$DT_SIN_PRI, "%Y")) == 2021 ~ as.factor(2020)),
         month = as.numeric(format(df_mean$DT_SIN_PRI, "%m")))

df_mean <- df_mean %>%
reframe(id_municipio,
deaths = EVOLUCAO,
date_sintoms = DT_SIN_PRI,
month,
coorte,
srag_class = CLASSI_FIN)

df_mean <- df_mean %>%
  filter(!is.na(coorte))

df_mean <- merge(df_mean, df_aux, by = c("id_municipio", "coorte"), all.x = TRUE)

# Replaceing null stem_backgroun for 0
df_mean <- df_mean %>%
  mutate(teste = as.numeric(stem_background))

df_mean <- df_mean %>%
  mutate(teste = ifelse(teste == 2, 1, 0))

df_mean <- df_mean %>% 
mutate(teste = ifelse(is.na(teste), 0, teste))

df_mean <- df_mean %>%
  mutate(stem_background = teste)

# Counting the number of unique id_municipio
n_distinct(df_mean$id_municipio)

# Aggregating to month level
df_plot <- df_mean %>%
  group_by(id_municipio, coorte, month, stem_background) %>%
  summarise(deaths = n())

df_regs <- df_mean %>%
  group_by(id_municipio, coorte, month, stem_background, tenure) %>%
  summarise(deaths = n())

# Aggregating to mean and then cumulative deaths
df_plot <- df_plot %>%
  group_by(coorte, month, stem_background) %>%
  summarise(mean_deaths = mean(deaths)) %>%
  group_by(coorte, stem_background) %>%
  mutate(cum_deaths = cumsum(mean_deaths))
  # Plot mean deaths over time by stem_background for cohorts 2016 and 2020
  plots_list <- list()
  for (c in 2016) {
    df_plot_coorte <- df_plot %>% filter(coorte == c)
    # Set month labels
    month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    p <- ggplot(df_plot_coorte, aes(x = month, y = cum_deaths, shape = as.factor(stem_background), group = stem_background)) +
      geom_line(size = 1) +
      geom_point(size = 4) +
      scale_shape_manual(
        values = c("0" = 17, "1" = 15), 
        labels = c("Non-STEM", "STEM")
      ) +
      scale_x_continuous(
        breaks = 1:12,
        labels = month_labels
      ) +
      labs(
        x = NULL,
        y = "Deaths",
        shape = "Background"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    plots_list[[as.character(c)]] <- p
  
    # Saving plot
  ggsave(
    filename = paste0(output_dir, "/figures/mean_cumulative_deaths_stem_background_", c, ".png"),
    plot = plots_list[[as.character(c)]],
    height = 5.0,
    width = 10,
    dpi = 600
  )
  
  }


# OLS (plm with states fixed effect)

# getting sigla_uf

df_population <- read.csv2(paste0(data_dir, "/raw/populacao.csv"), sep = ",") # source: https://iepsdata.org.br/data-downloads

## merging year of population with coorte
df_population <- df_population %>%
  mutate(coorte = recode(ano, '2020' = '2016', '2021' = '2020')) %>% 
  summarise(coorte = as.factor(coorte), populacao, id_municipio = as.character(id_municipio), sigla_uf)


df_regs <- left_join(df_regs, df_population, by = c("id_municipio", "coorte"))


# Replace missing tenure with 0
df_regs$tenure[is.na(df_regs$tenure)] <- 0
df_regs$tenure <- df_regs$tenure / 12

df_regs <- df_regs %>%
  ungroup() 

# Saving as .dta

haven::write_dta(df_regs, path = paste0(data_dir, "/intermediary/data_ols.dta"))
skim(df_regs)
# adding sigla_uf to municipalities

library(sandwich)
library(lmtest)

model <- lm(deaths ~ stem_background, data = df_regs[df_regs$coorte == 2016 , ])
lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC1"))

# Criar uma nova tabela com isso
library(plm)

# Convert to panel data frame
pdata <- df_regs[df_regs$coorte == 2016 & !is.na(df_regs$sigla_uf), ]
pdata <- pdata.frame(pdata, 
           index = c("sigla_uf"))

# Estimate fixed effects model with robust standard errors
model <- plm(deaths ~ stem_background * month, 
       data = pdata,
       #effect = "twoways",
       model = "within")
lmtest::coeftest(model, vcov = vcovHC(model, type = "HC1"))


model2 <- lm(deaths ~ stem_background*month + sigla_uf, data = df_regs[df_regs$coorte == 2016, ])
lmtest::coeftest(model2, vcov = sandwich::vcovHC(model2, type = "HC1"))


summary(lm(deaths ~ stem_background + month , data = df_regs[df_regs$coorte == 2016 & df_regs$populacao > 50000, ]))
summary(lm(deaths ~ stem_background  + sigla_uf , data = df_regs[df_regs$coorte == 2016 & df_regs$populacao > 50000, ]))

summary(lm(deaths ~ stem_background*tenure , data = df_regs[df_regs$coorte == 2016 & df_regs$populacao > 50000, ]))
summary(lm(deaths ~ stem_background*tenure + month, data = df_regs[df_regs$coorte == 2016 & df_regs$populacao > 50000, ]))
summary(lm(deaths ~ stem_background*tenure  + sigla_uf, data = df_regs[df_regs$coorte == 2016 & df_regs$populacao > 50000, ]))

summary(lm(deaths ~ stem_background, data = df_regs))
summary(lm(deaths ~ stem_background, data = df_regs))
summary(lm(deaths ~ stem_background*coorte, data = df_regs))
summary(lm(deaths ~ stem_background*coorte + month, data = df_regs))
summary(lm(deaths ~ stem_background*coorte + month + sigla_uf, data = df_regs))


pdata <- pdata.frame(df_regs, c("sigla_uf"))

ols_deaths <- plm(
  Y_deaths_sivep ~ T + inter_tenure_stem + tenure,
  data = pdata,
  index = c("sigla_uf"),
  model = "within" ,
  effect = "twoways"
)

summary(ols_deaths_tenure)

# Merging
df_population <- read.csv2(paste0(data_dir, "/raw/populacao.csv"), sep = ",") # source: https://iepsdata.org.br/data-downloads
df_population <- df_population %>%
  mutate(coorte = recode(ano, '2020' = '2016', '2021' = '2020')) %>% 
  reframe(coorte = as.factor(coorte), populacao, id_municipio = as.character(id_municipio))


# Cleaning NPI data
df <- df %>% # Removing NPI data from municipalities in 2020 chort (since this data only regards mayors elected in 2016)
  mutate(
    total_nfi = ifelse(coorte == 2020, NA, total_nfi),
    barreiras_sanitarias = ifelse(coorte == 2020, NA, barreiras_sanitarias),
    mascaras = ifelse(coorte == 2020, NA, mascaras),
    restricao_atv_nao_essenciais = ifelse(coorte == 2020, NA, restricao_atv_nao_essenciais),
    restricao_circulacao = ifelse(coorte == 2020, NA, restricao_circulacao),
    restricao_transporte_publico = ifelse(coorte == 2020, NA, restricao_transporte_publico)
  )

# Sum stats ---------------------------------------------------------------

# keeping only relevant variables
dat <- df[c(
  "stem_background",
  "tenure",
  "X",
  "mulher",
  "idade",
  "instrucao",
  "reeleito",
  "ideology_party",
  "populacao",
  "densidade",
  "idhm",
  "renda_pc",
  "per_populacao_urbana",
  "per_populacao_homens",
  "tx_med",
  "pct_desp_recp_saude_mun",
  "cob_esf",
  "tx_leito_sus",
  "ideology_municipality",
  "Y_deaths_sivep",
  "Y_hosp",
  "barreiras_sanitarias",
  "mascaras",
  "restricao_atv_nao_essenciais",
  "restricao_circulacao",
  "restricao_transporte_publico",
  "total_nfi"
)]

dat <- dat %>%
  summarise(
    "Tenure in STEM job" = tenure,
    "Female" = as.numeric(mulher),
    "Age" = idade,
    "Education" = instrucao,
    "Incumbent when elected" = as.numeric(reeleito),
    "Party ideology" = ideology_party,
    "Deaths per 100k inhabitants" = Y_deaths_sivep,
    "Hospitalizations per 100k inhabitants" = Y_hosp,
    "Cordon sanitaire" = barreiras_sanitarias,
    "Face covering required" = mascaras,
    "Closure of non-essential activities" = restricao_atv_nao_essenciais,
    "Gathering prohibition" = restricao_circulacao,
    "Public transport restriction" = restricao_transporte_publico,
    "Number of Non-Pharma. Interventions" = total_nfi,
    "Log of population in 2010" = log(populacao),
    "Human Development Index" = idhm,
    "Per capita income" = renda_pc,
    "Population density" = densidade,
    "Urban population rate" = per_populacao_urbana,
    "Men population rate" = per_populacao_homens,
    "Physicians per 1k inhabitants" = tx_med,
    "Health municipal spending rate" = pct_desp_recp_saude_mun,
    "Community health agency coverage rate" = cob_esf,
    "Hospital beds per 100k population" = tx_leito_sus,
    stem_background
  )

# Creating McCrary test figure
mctest  <- rddensity::rddensity(X = df$X, c = 0)
mc_plot <- rddensity::rdplotdensity(mctest, df$X)
png(file.path(output_dir, "figures", "mccrary_density_rddensity.png"),
    width = 1600, height = 1200, res = 200)
rddensity::rdplotdensity(mctest, df$X)
dev.off()

## creating table without groups

tab <- datasummary(All(data.frame(dat)) ~ N  + Min + Mean + Max + SD,
                   data = dat,
                   fmt = 2)
tab

datasummary(
  All(data.frame(dat)) ~ N  + Min + Mean + Max + SD,
  data = dat,
  fmt = 2,
  align = "lrrrrr",
  title = "Summary Statistics",
  output = paste0(output_dir, "/tables/table_sum_stats.tex")
)


## creating table with groups

dat$stem_background <- ifelse(dat$stem_background == 1, "STEM", "Non-STEM")

datasummary_balance(
  ~ stem_background,
  dinm_statistic = "p.value",
  data = dat,
  fmt = 2,
  align = "lrrrrrr",
  title = "Summary Statistics by Group",
  output = paste0(output_dir, "/tables/table_sum_stats_groups.tex")
)

## Figures for STEM candidates ---------------------------------------------------------

### Main occupations

profi <- df %>% 
  filter(stem_background == 1) %>%
  group_by(cbo_agregado_nome_caser.laverde.rothwell) %>%
  summarise(number = n(), .groups = "drop") %>%
  mutate(percentage = 100 * number / sum(number)) %>%
  slice_max(order_by = percentage, n = 10) %>%
  arrange(desc(percentage))


library(forcats)

# Ordering
profi <- profi %>%
  mutate(cbo_agregado_nome_caser.laverde.rothwell = fct_reorder(
    cbo_agregado_nome_caser.laverde.rothwell, percentage, .desc = FALSE
  ))

# Creating the plot
p <- ggplot(profi, aes(y = cbo_agregado_nome_caser.laverde.rothwell, x = percentage)) +
  geom_bar(pattern = "occupation", stat = "identity") +
  geom_text(aes(label = sprintf("%.0f%%", percentage)), 
            hjust = -0.1, 
            size = 5) +   
  theme_minimal(base_size = 16) +
  xlab("") +  
  ylab("Occupations") +
  xlim(0, 70) + 
  scale_fill_discrete(name = "occupation") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.title = element_blank() 
  )

p

ggsave(
  filename = paste0(output_dir, "/figures/barplot_stem_cbos_stem_ocupacao.png"),
  plot = p,  
  height = 4.0,
  width = 10,
  dpi = 600 
)



### getting all Brazilian municipalities

df_cities <- read.csv("data/raw/cities_and_states.csv")
df_cities$id_municipio <- as.character(df_cities$id_municipio)

df_boxplots <- merge(df_cities, df, by = c("id_municipio", "sigla_uf"), all = TRUE) # creating a dataset with all municipalities


### ploting

states <- df_boxplots %>%
  group_by(sigla_uf) %>% 
  dplyr::summarise(perc_stem = sum(stem_background == 1, na.rm = TRUE) / length(id_municipio)) %>% 
  arrange(desc(perc_stem))

box2 <- ggplot(states, aes(y = perc_stem * 100)) + 
  geom_boxplot() + 
  theme_minimal(base_size = 16) + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + # Adiciona o símbolo de %
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank()
  )


box2 

ggsave(
  filename = paste0(output_dir, "/figures/sumstats_boxplot.png"),
  plot = box2,  # Replace with the actual ggplot object
  height = 3.0,
  width = 2.5,
  dpi = 600
)

# Creating map  ---------------------------------------------------------

## states
#utils::remove.packages('geobr')
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")

dados_mapa <- read_state(year=2015, showProgress = FALSE, simplified = FALSE)

sf2 <- states 

dados_mapa <- dados_mapa %>% 
  rename(sigla_uf = abbrev_state)

sf2 <- left_join(dados_mapa, sf2, by = c("sigla_uf"))
dim(df)

sf2$perc_stem = sf2$perc_stem * 100

sf2 %>%
  ggplot() +
  geom_sf(aes(fill = perc_stem), alpha = 0.9, color = NA) +
  labs(#title="Percentage of STEM mayors per state (2016)",
    caption='Source: Author', size = 8) +
  viridis::scale_fill_viridis(
    direction = 1,
    name="% of STEM mayors",
    na.value = "white"
  ) +
  theme_minimal(base_size = 16) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave(
  filename = paste0(output_dir, "/figures/mapa_stem_estados.png"),
 # plot = box2,  # Replace with the actual ggplot object
  height = 5.0,
  width = 10
)

## municipios

mun <- read_municipality(year=2015, showProgress = FALSE, simplified = TRUE, cache = FALSE)

mun <- mun %>% 
  rename(id_municipio = code_muni)

mun$id_municipio = substr(mun$id_municipio,1,6)

sf3 <- df %>% 
  filter(coorte == 2016)

sf3 <- left_join(mun, sf3, by = c("id_municipio"))

sf3 %>%
  ggplot() +
  geom_sf(data = subset(dados_mapa)) +
  geom_sf(aes(fill = stem_background), alpha = .7, color = NA) +
  labs(#title="Municipalities where a STEM canditate was among the top 2 voted (2016)",
    caption='Source: Author', size = 8) +
  scale_fill_manual(values = c("red", "blue"),
                    name = "STEM candidate",
                    na.value = "grey90",
                    labels = c("Lost","Won", "Not in top 2")) +
  theme_minimal(base_size = 16) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave(
  filename = paste0(output_dir, "/figures/mapa_stem_municipios_2016.png"),
  # plot = box2,  # Replace with the actual ggplot object
  height = 5.0,
  width = 10,
  dpi = 600
)

# Plots - Discontinuity  ----------------------------------------------

## outcomes

### running regressions to get bandwidths

amostra <- cbind()

state.f = factor(df$sigla_uf)
state.d = model.matrix( ~ state.f + 0) # creating fixed effects

year.f = factor(df$coorte) # creating dummies
if (cohort_filter == "") {
  year.d = model.matrix(~year.f+0)
}
if (cohort_filter == "2016_") {
  year.d = 1
}

covsZ  = cbind(
    state.d,
    year.d,
    df$mulher,
    df$ideology_party,
    df$instrucao,
    df$reeleito,
    df$idade,
    df$idade * df$idade) # Controls

r4 = rdrobust(
  df$Y_deaths_sivep,
  df$X,
  p      = poli,
  kernel = k,
  h      = 0.082,
  subset = amostra,
  covs   = covsZ
)
r5 = rdrobust(
  df$Y_hosp,
  df$X,
  kernel = k,
  #h      = janela,
  p      = poli,
  subset = amostra,
  covs   = covsZ
)

summary(r4)
summary(r5)

### hosp 

df_plots <- df[abs(df$X) < r4$bws[1], ]

state.f = factor(df_plots$sigla_uf)
state.d = model.matrix( ~ state.f + 0) # creating fixed effects

year.f = factor(df_plots$coorte) # creating dummies
if (cohort_filter == "") {
  year.d = model.matrix(~year.f+0)
}
if (cohort_filter == "2016_") {
  year.d = 1
}

covsZ  = cbind(state.d,
               year.d,
               df_plots$mulher,
               df_plots$ideology_party,
               df_plots$instrucao,
               df_plots$reeleito,
               df_plots$idade,
               df_plots$idade * df_plots$idade) # Controls

hosp <- rdplot(df_plots$Y_hosp, df_plots$X,
               covs = covsZ,
               p = poli,
               binselect = "esmv",
               kernel = k,
               x.label = "STEM candidate's margin of victory",
               y.label = "",
               title = "(A) Impact of Treatment on Hospitalizations"
               )


### deaths


death <- rdplot(df_plots$Y_deaths_sivep , df_plots$X,
                covs = covsZ,
                p = poli,
                scale = 2,
                binselect = "esmv",
                kernel = k,
                x.label = "STEM candidate's margin of victory",
                y.label = "Deaths per 100k inhabitants",
                title = ""
               )

death <- death$rdplot

death <- death +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 10, face = "plain"),
    title = element_text(size = 12),
    panel.grid = element_blank(),
    line = element_line(size = 1.5)
  ) +
  geom_line(size = 2)

death

hosp <- hosp$rdplot

hosp <- hosp +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    axis.title = element_text(size = 10, face = "plain"),
    title = element_text(size = 12),
    panel.grid = element_blank()
  )

hosp

plots <- death

plots

ggsave(paste0(output_dir, "/figures/bigsample_plots_outcomes.png"), plot = plots,
       width = 8.0,
       height = 5,
       units = "in")
