# Program - This program run main RDD regressions, including robustness, tables and pictures 


# Oppening ----------------------------------------------------------------

df             <- readRDS(paste(data_dir, "/final/", data, sep = ""))
df_all_cohorts <- readRDS(paste(data_dir, "/final/", data_all_cohorts, sep = ""))

# Creating functions -----------------------------------

## table of results

tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term      = row.names(model$coef),
    estimate  = model[["coef"]][1],
    std.error = model[["se"]][1],
    p.value   = model[["pv"]][1],
    conf.low  = model[["ci"]][1,1],
    conf.high = model[["ci"]][1,2])
  
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    "Eff N obs." = as.character(model$N_h[1] + model$N_h[2]),
    "Bandwidth" = paste0(as.character(round(model$bws[1,1] * 100, 2)),"%")
  )
  ret
}

## table of robustness


robust_check <- function(outcome, poli, covariadas, k, x_value) {
  
  df_robs <- data.frame()
  
  
  for (i in seq(0.03, 0.24, by = 0.01)) {
    
    prov = rdrobust(y = outcome, x_value, p = poli, level = 90, kernel = k, h = i, covs = covariadas) #rodando rdd

    df_robs = rbind(df_robs, c(i, prov[["coef"]][1], prov[["coef"]][3], prov[["ci"]][3,1], prov[["ci"]][3,2], prov[["ci"]][1,1], prov[["ci"]][1,2], prov[["z"]][3], prov[["N_h"]][1] + prov[["N_h"]][2])) #salvando colunas
    
  }
  
  colnames(df_robs) <- c("bw", "coef_conv", "coef_robs", "ci_lower_rob", "ci_higher_rob",  "ci_lower_conv", "ci_higher_conv", "z", "eff_n")
  
  
  return(df_robs)
  
  
}

# Creating state dummies for fixed effects --------------------------------

# Defining regressions' parameters

state.f = factor(df$sigla_uf)
state.d = model.matrix(~state.f+0)

year.f = factor(df$coorte)
if (cohort_filter == "") {
  year.d = model.matrix(~year.f+0)
}
if (cohort_filter == "2016_") {
  year.d = 1
}


# Panel A (2016 cohort)
df_2016 <- df_all_cohorts[df_all_cohorts$coorte == 2016, ]
state.f_2016 = factor(df_2016$sigla_uf)
state.d_2016 = model.matrix(~state.f_2016+0)
year.d_2016 = 1

# Panel B (2020 cohort)
df_2020 <- df_all_cohorts[df_all_cohorts$coorte == 2020, ]
state.f_2020 = factor(df_2020$sigla_uf)
state.d_2020 = model.matrix(~state.f_2020+0)
year.d_2020 = 1

# Panel C (all cohorts)

state.f_all_cohorts = factor(df_all_cohorts$sigla_uf)
state.d_all_cohorts = model.matrix(~state.f_all_cohorts+0)
year.f_all_cohorts = factor(df_all_cohorts$coorte)
year.d_all_cohorts = model.matrix(~year.f_all_cohorts+0)

# Covariates
covs_base <- cbind(state.d, year.d)
covs_full <- cbind(
  state.d,
  year.d,
  df$mulher,
  df$ideology_party,
  df$instrucao,
  df$reeleito,
  df$idade,
  df$idade * df$idade
)

# Covariates for each panel
covs_base_2016 <- cbind(state.d_2016, year.d_2016)
covs_full_2016 <- cbind(
  state.d_2016,
  year.d_2016,
  df_2016$mulher,
  df_2016$ideology_party,
  df_2016$instrucao,
  df_2016$reeleito,
  df_2016$idade,
  df_2016$idade * df_2016$idade
)

covs_base_2020 <- cbind(state.d_2020, year.d_2020)
covs_full_2020 <- cbind(
  state.d_2020,
  year.d_2020,
  df_2020$mulher,
  df_2020$ideology_party,
  df_2020$instrucao,
  df_2020$reeleito,
  df_2020$idade,
  df_2020$idade * df_2020$idade
)

covs_base_all_cohorts <- cbind(state.d_all_cohorts, year.d_all_cohorts)
covs_full_all_cohorts <- cbind(
  state.d_all_cohorts,
  year.d_all_cohorts,
  df_all_cohorts$mulher,
  df_all_cohorts$ideology_party,
  df_all_cohorts$instrucao,
  df_all_cohorts$reeleito,
  df_all_cohorts$idade,
  df_all_cohorts$idade * df_all_cohorts$idade
)

# Main Results -------------------------------------------------------------

# estimates

# OLS
df_ols <- readRDS(paste0(data_dir, "/intermediary/data_ols.rds"))
model <- lm(deaths_100k ~ stem_background + sigla_uf, data = df_ols[df_ols$coorte == 2016 & df_ols$population > 70000, ])
ols_results <- lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC1"))

# Create a df with ols results
df_ols <- data.frame(
  term = row.names(ols_results),
  estimate = ols_results[, "Estimate"],
  std.error = ols_results[, "Std. Error"],
  p.value = ols_results[, "Pr(>|t|)"],
  n_obs = nobs(model)
)

# OLS with month interaction using PLM
df_ols_month <- readRDS(paste0(data_dir, "/intermediary/data_ols_month.rds"))

# Manter apenas os id_municipio presentes em df_2016
df_ols_month <- df_ols_month[df_ols_month$id_municipio %in% df_2016$id_municipio, ]

pdata_month <- df_ols_month[(df_ols_month$coorte == 2016 | df_ols_month$month %in% c(1,2)) & !is.na(df_ols_month$sigla_uf) , ]
pdata_month <- pdata_month %>%
  ungroup()


pdata_month <- plm::pdata.frame(pdata_month, index = c("sigla_uf"))

pdata_month$month <- ifelse(pdata_month$coorte == 2020, pdata_month$month + 12, pdata_month$month)


model_ols_month <- plm::plm(deaths_100k ~ stem_background*month , 
  data = pdata_month[pdata_month$month > 2, ],
  model = "within")
  
  modelsummary(
    model_ols_month,
    vcov = function(x) vcovHC(x, type = "HC1", cluster = "group"),
    estimate = "{estimate}",
    statistic = c("[{std.error}]", "{p.value}{stars}"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    fmt = 2,
    #output = "outputs/tables/ols_month_interaction.tex",
    title = "Impact of STEM Leadership on Deaths with Month Interaction",
    coef_map = c(
      "stem_background" = "STEM Background",
      "month" = "Month",
      "stem_background:month" = "STEM Background × Month"
    ),
    add_rows = data.frame(
      term = c("State FE", "Mayor Controls"),
      `Model 1` = c("Yes", "No"),
      check.names = FALSE
    ),
    gof_omit = "BIC|AIC| Log.Lik.|Adj.R2|R2|RMSE|Std.Errors"
  )

# Rdrobust function
run_rdrobust_models <- function(df, state.d, year.d, poli, k, janela, outcome, prefix = "") {
  covs_base <- cbind(state.d, year.d)
  covs_full <- cbind(
    state.d,
    year.d,
    df$mulher,
    df$ideology_party,
    df$instrucao,
    df$reeleito,
    df$idade,
    df$idade * df$idade
  )
  models <- list(
    rdrobust(outcome, df$X, p = poli, kernel = k, bwselect = "mserd", covs = covs_full),
    rdrobust(outcome, df$X, p = poli, kernel = k, bwselect = "mserd", covs = covs_full, h = janela - 0.01),
    rdrobust(outcome, df$X, p = poli, kernel = k, bwselect = "mserd", covs = covs_full, h = janela),
    rdrobust(outcome, df$X, p = poli, kernel = k, bwselect = "mserd", covs = covs_full, h = janela + 0.01),
    rdrobust(outcome, df$X, p = poli, kernel = k, bwselect = "mserd", covs = covs_full, h = 1.00)
  )
  return(models)
}

# Running model for this specification
models_death   <- run_rdrobust_models(df, state.d, year.d, poli, k, janela, df$Y_deaths_sivep)
models_death_2 <- run_rdrobust_models(df, state.d, year.d, poli + 1, k, janela, df$Y_deaths_sivep)
models_hosp    <- run_rdrobust_models(df, state.d, year.d, poli, k, janela, df$Y_hosp)
models_hosp_2  <- run_rdrobust_models(df, state.d, year.d, poli + 1, k, janela, df$Y_hosp)

# Extract optimal bandwidths for each panel (for reference or reporting)
optimal_bw       <- models_death[[1]]$bws[[1]]
optimal_bw2      <- models_death_2[[1]]$bws[[1]]
optimal_bw_hosp  <- models_hosp[[1]]$bws[[1]]
optimal_bw_hosp2 <- models_hosp_2[[1]]$bws[[1]]
# Creating table death
modelsummary(
  models_death,
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  coef_rename = c("Robust" = "RD estimator"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2,
  #output = "outputs/tables/estimates.png",
  output = "outputs/tables/estimates.tex",
  title = "Impact of STEM Leadership on Deaths — RD estimates",
  coef_omit = "Corrected|Conventional",
  coef_map = NULL,
  add_rows = data.frame(
    term = "Type of Bandwidth",
    `Model 1` = "Optimal",
    `Model 2` = "Fixed",
    `Model 3` = "Fixed",
    `Model 4` = "Fixed",
    `Model 5` = "Fixed",
    check.names = FALSE
  )
)

modelsummary(
  models_death_2,
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  coef_rename = c("Robust" = "RD estimator"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2,
  output = "outputs/tables/estimates_quadratic.tex",
  title = "Impact of STEM Leadership on Deaths — RD estimates (quadratic equation)",
  coef_omit = "Corrected|Conventional",
  coef_map = NULL,
  add_rows = data.frame(
    term = "Type of Bandwidth",
    `Model 1` = "Optimal",
    `Model 2` = "Fixed",
    `Model 3` = "Fixed",
    `Model 4` = "Fixed",
    `Model 5` = "Fixed",
    check.names = FALSE
  )
)

# Creating table hosps
modelsummary(
  models_hosp,
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  coef_rename = c("Robust" = "RD estimator"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2,
  #output = "outputs/tables/estimates.png",
  output = "outputs/tables/estimates_hosp.tex",
  title = "Impact of STEM Leadership on Hospitalizations — RD estimates",
  coef_omit = "Corrected|Conventional",
  coef_map = NULL,
  add_rows = data.frame(
    term = "Type of Bandwidth",
    `Model 1` = "Optimal",
    `Model 2` = "Fixed",
    `Model 3` = "Fixed",
    `Model 4` = "Fixed",
    `Model 5` = "Fixed",
    check.names = FALSE
  )
)

modelsummary(
  models_hosp_2,
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  coef_rename = c("Robust" = "RD estimator"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2,
  #output = "outputs/tables/estimates.png",
  output = "outputs/tables/estimates_hosp_quadratic.tex",
  title = "Impact of STEM Leadership on Hospitalizations — RD estimates (quadratic equation)",
  coef_omit = "Corrected|Conventional",
  coef_map = NULL,
  add_rows = data.frame(
    term = "Type of Bandwidth",
    `Model 1` = "Optimal",
    `Model 2` = "Fixed",
    `Model 3` = "Fixed",
    `Model 4` = "Fixed",
    `Model 5` = "Fixed",
    check.names = FALSE
  )
)
# Baseline table

renda_pc                <- rdrobust(df$renda_pc,                 df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covs_base, h = optimal_bw)
populacao               <- rdrobust(log(df$populacao),           df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covs_base, h = optimal_bw)
idhm                    <- rdrobust(df$idhm,                     df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covs_base, h = optimal_bw)
densidade               <- rdrobust(df$densidade,                df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covs_base, h = optimal_bw)
per_populacao_homens    <- rdrobust(df$per_populacao_homens,     df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covs_base, h = optimal_bw)
pct_desp_recp_saude_mun <- rdrobust(df$pct_desp_recp_saude_mun,  df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covs_base, h = optimal_bw)
tx_med_ch               <- rdrobust(df$tx_med_ch,                df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covs_base, h = optimal_bw)
cob_esf                 <- rdrobust(df$cob_esf,                  df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covs_base, h = optimal_bw)
tx_leito_sus            <- rdrobust(df$tx_leito_sus,             df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covs_base, h = optimal_bw)
ideology_municipality   <- rdrobust(df$ideology_municipality,    df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covs_base, h = optimal_bw)

# Models for Panel 1 (Demography)
models_1 <- list(
  "PC income" = renda_pc,
  "Log Population" = populacao,
  "HDI" = idhm,
  "Density" = densidade,
  "% Masc. Pop" = per_populacao_homens
)

# Models for Panel 2 (Health and Ideology)
models_2 <- list(
  "% Health spending" = pct_desp_recp_saude_mun,
  "Doctors" = tx_med_ch,
  "CHA program" = cob_esf,
  "Hosp. beds" = tx_leito_sus,
  "Mun. ideology" = ideology_municipality
)


# Create the first table (Demography)
baseline_table_1 <- modelsummary(
  models_1,
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  coef_rename = c("Robust" = "RD estimator"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2,
  #output = "outputs/tables/baseline_table_panel1.png",
  output = "outputs/tables/baseline_table_panel1.tex",
  title = "Baseline Characteristics - RD Estimates (Demography)",
  coef_omit = "Corrected|Conventional"
)

# Create the second table (Health and Ideology)
baseline_table_2 <- modelsummary(
  models_2,
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  coef_rename = c("Robust" = "RD estimator"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2,
  #output = "outputs/tables/baseline_table_panel2.png",
  output = "outputs/tables/baseline_table_panel2.tex",
  title = "Baseline Characteristics - RD Estimates (Health and Ideology)",
  coef_omit = "Corrected|Conventional"#,
  #align = paste(rep("c", length(models_2) + 1), collapse = "")
)

baseline_table_1
baseline_table_2

# Personal charact

mulher   <- rdrobust(df$mulher,         df$X, p = poli, kernel = k, h = optimal_bw,  covs = covs_base)
reeleito <- rdrobust(df$reeleito,       df$X, p = poli, kernel = k, h = optimal_bw,  covs = covs_base)
idade    <- rdrobust(df$idade,          df$X, p = poli, kernel = k, h = optimal_bw,  covs = covs_base)
ens.sup  <- rdrobust(df$instrucao,      df$X, p = poli, kernel = k, h = optimal_bw,  covs = covs_base)
ideology <- rdrobust(df$ideology_party, df$X, p = poli, kernel = k, h = optimal_bw,  covs = covs_base)

models <- list(
  "Women" = mulher,
  "Incumbent" = reeleito,
  "Age" = idade,
  "Education" = ens.sup,
  "Mayors' party ideology" = ideology)


teste_chr <- modelsummary(models,
             estimate = "{estimate}",
             statistic = c("[{std.error}]","{p.value}{stars}"
             ),
             coef_rename = c("Robust" = "RD estimator"),
             stars = c('*'=.1, '**'=.05, '***'=.01),
             fmt = 2, # decimal places
             #output = "outputs/tables/personal_char.png",
             output = "outputs/tables/personal_char.tex",
             title = "STEM candidates' personal characteristics — RD estimates",
             coef_omit = "Corrected|Conventional")#,
            # align = paste(rep("c", length(models) + 1), collapse = ""))

teste_chr
#gt::gtsave(teste_chr, filename =  "outputs/tables/personal_char.tex")


# Mechanism  -----------------------
r12 = rdrobust(df$total_nfi,                    df$X, p = poli, kernel = k, h = optimal_bw,   covs = covs_full)
r22 = rdrobust(df$mascaras,                     df$X, p = poli, kernel = k, h = optimal_bw,   covs = covs_full)
r32 = rdrobust(df$restricao_atv_nao_essenciais, df$X, p = poli, kernel = k, h = optimal_bw,   covs = covs_full)
r42 = rdrobust(df$restricao_circulacao,         df$X, p = poli, kernel = k, h = optimal_bw,   covs = covs_full)
r52 = rdrobust(df$restricao_transporte_publico, df$X, p = poli, kernel = k, h = optimal_bw,   covs = covs_full)
r62 = rdrobust(df$barreiras_sanitarias,         df$X, p = poli, kernel = k, h = optimal_bw,   covs = covs_full)

models <- list(
               "Total NPI" = r12,
               "Masks" = r22,
               "Restrictions atv." = r32,
               "Restrictions circu." = r42,
               "Restrictions transp." = r52,
               "Sani barriers" = r62)


# Create a gt object
mr3 <- modelsummary(
  models,
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2, # decimal places
  #output = "outputs/tables/mechanism.png",
  output = "outputs/tables/mechanism.tex", 
  title = "Impact of STEM Candidate Elected in 2016 on Non-Pharmaceutical Interventions in 2020",
  coef_omit = "Bias-Corrected|Conventional",
  align = paste(rep("r", length(models) + 1), collapse = ""),
  add_rows = data.frame(
    term = "Mayor Controls",
    `Model 1` = "Yes",
    `Model 2` = "Yes",
    `Model 3` = "Yes",
    `Model 4` = "Yes",
    `Model 5` = "Yes",
    `Model 6` = "Yes",
    check.names = FALSE
  ) 
)

# Robustness --------------------------------------------------------------


## Different windows -----------------------------------------------------

df_robs_hosp      <- robust_check(df$Y_hosp,                       1, covs_full, k, df$X)
df_robs_hosp_2    <- robust_check(df$Y_hosp,                       2, covs_full, k, df$X)
df_robs_deaths    <- robust_check(df$Y_deaths_sivep,               1, covs_full, k, df$X)
df_robs_deaths_2  <- robust_check(df$Y_deaths_sivep,               2, covs_full, k, df$X)
df_robs_nfi       <- robust_check(df$total_nfi,                    1, covs_full, k, df$X)
df_robs_masks     <- robust_check(df$mascaras,                     1, covs_full, k, df$X)
df_robs_trans_pub <- robust_check(df$restricao_transporte_publico, 1, covs_full, k, df$X)
df_robs_circu     <- robust_check(df$restricao_circulacao,         1, covs_full, k, df$X)


theme_clean <- theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(color = "black")
)

# Function to add a red point at the value of bw closest to bw_optimal
add_optimal_point <- function(plot, df, bw_optimal) {
  df$distance <- abs(df$bw - bw_optimal)
  optimal_point <- df[which.min(df$distance), ]
  
  plot +
    geom_point(data = optimal_point, aes(x = bw, y = coef_conv), color = "red", size = 2) 
}

# Hospitalizations and deaths plots
plot_hosp_robs <- ggplot(df_robs_hosp, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12)) +
  ggtitle("(b) COVID-19 hospitalizations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean

plot_hosp_robs_2 <- ggplot(df_robs_hosp_2, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12)) +
  ggtitle("(d) COVID-19 hospitalizations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean


plot_hosp_robs <- add_optimal_point(plot_hosp_robs, df_robs_hosp, optimal_bw_hosp)

plot_deaths_robs <- ggplot(df_robs_deaths, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12)) +
  ggtitle("(a) COVID-19 deaths") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean

plot_deaths_robs <- add_optimal_point(plot_deaths_robs, df_robs_deaths, optimal_bw)

plot_deaths_robs_2 <- ggplot(df_robs_deaths_2, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12)) +
  ggtitle("(c) COVID-19 deaths") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean



# Combinar gráficos de hospitalizações e mortes
graficos_juntos <- (plot_deaths_robs + plot_deaths_robs_2) / (plot_hosp_robs + plot_hosp_robs_2)

ggsave("outputs/figures/robust_outcomes.png", graficos_juntos,
       width = 11.00, height = 5.00, units = "in")

# Gráficos de NPIs
plot_nfi_robs <- ggplot(df_robs_nfi, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(a) Total number of NPIs") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean

bw_optimal_nfi <- r12$bws[[1]]
plot_nfi_robs <- add_optimal_point(plot_nfi_robs, df_robs_nfi, bw_optimal_nfi)

plot_nfi_masks <- ggplot(df_robs_masks, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(b) Face covering restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean

bw_optimal_masks <- r22$bws[[1]]
plot_nfi_masks <- add_optimal_point(plot_nfi_masks, df_robs_masks, bw_optimal_masks)

plot_nfi_trans <- ggplot(df_robs_trans_pub, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(c) Transportation restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean

bw_optimal_trans <- r32$bws[[1]]
plot_nfi_trans <- add_optimal_point(plot_nfi_trans, df_robs_trans_pub, bw_optimal_trans)

plot_nfi_circu <- ggplot(df_robs_circu, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(d) Public gathering restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean

bw_optimal_circu <- r62$bws[[1]]
plot_nfi_circu <- add_optimal_point(plot_nfi_circu, df_robs_circu, bw_optimal_circu)

# Combining NPI graphs
graf <- (plot_nfi_robs + plot_nfi_masks) / (plot_nfi_trans + plot_nfi_circu)

ggsave("outputs/figures/npi_rob.png", graf,
       width = 10.00, height = 8.00, units = "in")