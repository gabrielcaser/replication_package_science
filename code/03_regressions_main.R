# Program - This program run main RDD regressions, including robustness, tables and pictures 


# Oppening ----------------------------------------------------------------

df <- readRDS(paste(data_dir, "/final/", data, sep = ""))

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
    "Bandwidth" = as.character(round(model$bws[1,1] * 100, 2))#,
    #"State FE" = "X",
    #"Election FE" = "X",
    #"Gender" = ifelse("mulher" %in% colnames(as.data.frame(covsZ)),"X"," ")
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



# Main Results -------------------------------------------------------------


# estimates

# Função para rodar modelos rdrobust com diferentes especificações
run_rdrobust_models <- function(df, state.d, year.d, poli, k, janela, prefix = "") {
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
    rdrobust(df$Y_deaths_sivep, df$X, p = poli, kernel = k, bwselect = "mserd", covs = covs_base),
    rdrobust(df$Y_deaths_sivep, df$X, p = poli, kernel = k, bwselect = "mserd", covs = covs_full),
    rdrobust(df$Y_deaths_sivep, df$X, p = poli, kernel = k, bwselect = "mserd", covs = covs_full, h = janela),
    rdrobust(df$Y_deaths_sivep, df$X, p = poli, kernel = k, bwselect = "mserd", covs = covs_full, h = janela + 0.02),
    rdrobust(df$Y_deaths_sivep, df$X, p = poli, kernel = k, bwselect = "mserd", covs = covs_full, h = janela + 0.04)
  )
  return(models)
}

# Panel C (both cohorts)
models_panelC <- run_rdrobust_models(df, state.d, year.d, poli, k, janela)

# Panel B (2020 cohort)
df_2020 <- df[df$coorte == 2020, ]
state.f_2020 = factor(df_2020$sigla_uf)
state.d_2020 = model.matrix(~state.f_2020+0)
year.d_2020 = 1
models_panelB <- run_rdrobust_models(df_2020, state.d_2020, year.d_2020, poli, k, janela)

# Panel A (2016 cohort)
df_2016 <- readRDS(paste(data_dir, "/final/", data_2016, sep = ""))
state.f_2016 = factor(df_2016$sigla_uf)
state.d_2016 = model.matrix(~state.f_2016+0)
year.d_2016 = 1
models_panelA <- run_rdrobust_models(df_2016, state.d_2016, year.d_2016, poli, k, janela)

# Extract optimal bandwidths for each panel (for reference or reporting)
optimal_bw_panelA <- models_panelA[[2]]$bws[[1]]

# Creating table
models <- list(
  "Panel A: Deaths (only 2016 cohort)" = models_panelA,
  "Panel B: Deaths (only 2020 cohort)" = models_panelB,
  "Panel C: Deaths (both cohorts)" = models_panelC
)

modelsummary(
  models,
  shape = "rbind",
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  coef_rename = c("Robust" = "RD estimator"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2,
  output = "outputs/tables/estimates.png",
  # output = "outputs/tables/estimates.tex",
  title = "Impact of STEM Leadership on Epidemiological Outcomes — RD estimates",
  coef_omit = "Corrected|Conventional",
  coef_map = NULL
)


# Baseline table


covsZ = cbind(state.d, year.d)
#rdrobust(df$taxa_analfabetismo_18_mais, df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)

#taxa_analfabetismo_18_mais <- rdrobust(df$taxa_analfabetismo_18_mais, df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
#indice_gini <- rdrobust(df$indice_gini,  df$X, p = poli, kernel = k, h = janela,  covs = covsZ)
# Subset only for cohort == 2016
df_2016_baseline <- df[df$coorte == 2016, ]
covsZ_2016 <- cbind(
  model.matrix(~factor(df_2016_baseline$sigla_uf)+0),
  1 # year.d = 1 for 2016
)

renda_pc                <- rdrobust(df_2016_baseline$renda_pc,                 df_2016_baseline$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ_2016, h = optimal_bw_panelA)
populacao               <- rdrobust(log(df_2016_baseline$populacao),           df_2016_baseline$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ_2016, h = optimal_bw_panelA)
idhm                    <- rdrobust(df_2016_baseline$idhm,                     df_2016_baseline$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ_2016, h = optimal_bw_panelA)
densidade               <- rdrobust(df_2016_baseline$densidade,                df_2016_baseline$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ_2016, h = optimal_bw_panelA)
per_populacao_homens    <- rdrobust(df_2016_baseline$per_populacao_homens,     df_2016_baseline$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ_2016, h = optimal_bw_panelA)
pct_desp_recp_saude_mun <- rdrobust(df_2016_baseline$pct_desp_recp_saude_mun,  df_2016_baseline$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ_2016, h = optimal_bw_panelA)
tx_med_ch               <- rdrobust(df_2016_baseline$tx_med_ch,                df_2016_baseline$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ_2016, h = optimal_bw_panelA)
cob_esf                 <- rdrobust(df_2016_baseline$cob_esf,                  df_2016_baseline$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ_2016, h = optimal_bw_panelA)
tx_leito_sus            <- rdrobust(df_2016_baseline$tx_leito_sus,             df_2016_baseline$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ_2016, h = optimal_bw_panelA)
ideology_municipality   <- rdrobust(df_2016_baseline$ideology_municipality,    df_2016_baseline$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ_2016, h = optimal_bw_panelA)


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
  output = "outputs/tables/baseline_table_panel1.png",
  #output = "outputs/tables/baseline_table_panel1.tex",
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
  output = "outputs/tables/baseline_table_panel2.png",
  #output = "outputs/tables/baseline_table_panel2.tex",
  title = "Baseline Characteristics - RD Estimates (Health and Ideology)",
  coef_omit = "Corrected|Conventional"#,
  #align = paste(rep("c", length(models_2) + 1), collapse = "")
)

baseline_table_1
baseline_table_2


# Personal charact

covsZ = cbind(state.d, year.d)
poli = 1

mulher   <- rdrobust(df$mulher,         df$X, p = poli, kernel = k,  covs = covsZ)
reeleito <- rdrobust(df$reeleito,       df$X, p = poli, kernel = k,  covs = covsZ)
idade    <- rdrobust(df$idade,          df$X, p = poli, kernel = k,  covs = covsZ)
ens.sup  <- rdrobust(df$instrucao,      df$X, p = poli, kernel = k,  covs = covsZ)
ideology <- rdrobust(df$ideology_party, df$X, p = poli, kernel = k,  covs = covsZ)



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
             output = "outputs/tables/personal_char.png",
             #output = "outputs/tables/personal_char.tex",
             title = "STEM candidates' personal characteristics — RD estimates",
             coef_omit = "Corrected|Conventional")#,
            # align = paste(rep("c", length(models) + 1), collapse = ""))

teste_chr
#gt::gtsave(teste_chr, filename =  "outputs/tables/personal_char.tex")


# Mechanism  -----------------------

df2 <- df[df$coorte == 2016, ]
state.f2 = factor(df2$sigla_uf)
state.d2 = model.matrix(~state.f2+0)

covsZ <- cbind(
  state.d2,
  df2$mulher,
  df2$ideology_party,
  df2$instrucao,
  df2$reeleito,
  df2$idade,
  df2$idade * df2$idade
)

r12 = rdrobust(df2$total_nfi, df2$X, p = poli, kernel = k,   covs = covsZ)
r22 = rdrobust(df2$mascaras, df2$X,   p = poli, kernel = k,   covs = covsZ)
r32 = rdrobust(df2$restricao_atv_nao_essenciais, df2$X,   p = poli, kernel = k,   covs = covsZ)
r42 = rdrobust(df2$restricao_circulacao, df2$X,   p = poli, kernel = k,   covs = covsZ)
r52 = rdrobust(df2$restricao_transporte_publico, df2$X,   p = poli, kernel = k,   covs = covsZ)
r62 = rdrobust(df2$barreiras_sanitarias, df2$X,   p = poli, kernel = k,   covs = covsZ)


models <- list(#"Total NFI" = r1,
               #"Masks" = r2,
               #"Restrictions atv." = r3,
               #"Restrictions circu." = r4,
               #"Restrictions transp." = r5,
               #"Sani barriers" = r6,
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
  output = "outputs/tables/mechanism.png",
  #output = "outputs/tables/mechanism.tex", 
  title = "Impact of STEM Candidate Elected in 2016 on Non-Pharmaceutical Interventions in 2020",
  coef_omit = "Bias-Corrected|Conventional",
  align = paste(rep("r", length(models) + 1), collapse = "") # Create alignment string
)

# Add tab spanners
#mr3 <- mr3 %>%
#  tab_spanner(label = "(1)", columns = 2:7) %>% 
#  tab_spanner(label = "(2)", columns = 8:13)
#
## Convert gt table to raw HTML and then to Markdown
#markdown_output <- gt::as_raw_html(mr3)
#
## Save Markdown content to a file
#writeLines(markdown_output, con = "outputs/tables/mechanism.tex")



# Robustness --------------------------------------------------------------


## Different windows -----------------------------------------------------

CovsZ <- cbind(
  state.d,
  year.d,
  df$mulher,
  df$ideology_party,
  df$instrucao,
  df$reeleito,
  df$idade,
  df$idade * df$idade
) # mechanisms
CovsZ_2016 <- cbind(
  state.d2,
  df2$mulher,
  df2$ideology_party,
  df2$instrucao,
  df2$reeleito,
  df2$idade,
  df2$idade * df2$idade
)

  
df_robs_hosp      <- robust_check(df2$Y_hosp,                       1, CovsZ_2016, k, df2$X)
df_robs_deaths    <- robust_check(df2$Y_deaths_sivep,               1, CovsZ_2016, k, df2$X)
df_robs_nfi       <- robust_check(df2$total_nfi,                    1, CovsZ_2016, k, df2$X)
df_robs_masks     <- robust_check(df2$mascaras,                     1, CovsZ_2016, k, df2$X)
df_robs_trans_pub <- robust_check(df2$restricao_transporte_publico, 1, CovsZ_2016, k, df2$X)
df_robs_circu     <- robust_check(df2$restricao_circulacao,         1, CovsZ_2016, k, df2$X)
df_robs_atv       <- robust_check(df2$restricao_atv_nao_essenciais, 1, CovsZ_2016, k, df2$X)
df_robs_sani      <- robust_check(df2$barreiras_sanitarias,         1, CovsZ_2016, k, df2$X)

theme_clean <- theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(color = "black")
)

# Função para adicionar ponto vermelho no valor de bw mais próximo de bw_optimal
add_optimal_point <- function(plot, df, bw_optimal) {
  df$distance <- abs(df$bw - bw_optimal)
  optimal_point <- df[which.min(df$distance), ]
  
  plot +
    geom_point(data = optimal_point, aes(x = bw, y = coef_conv), color = "red", size = 2) 
}

# Gráficos de hospitalizações e mortes
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

bw_optimal_hosp <- r4$bws[[1]]
plot_hosp_robs <- add_optimal_point(plot_hosp_robs, df_robs_hosp, bw_optimal_hosp)

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

bw_optimal_deaths <- optimal_bw_panelA
plot_deaths_robs <- add_optimal_point(plot_deaths_robs, df_robs_deaths, bw_optimal_deaths)

# Combinar gráficos de hospitalizações e mortes
graficos_juntos <- plot_deaths_robs / plot_hosp_robs

ggsave("outputs/figures/robust_outcomes.png", graficos_juntos,
       width = 5.50, height = 5.00, units = "in")

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

plot_nfi_sani <- ggplot(df_robs_sani, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(d) Cordon sanitaire restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean

bw_optimal_sani <- r42$bws[[1]]
plot_nfi_sani <- add_optimal_point(plot_nfi_sani, df_robs_sani, bw_optimal_sani)

plot_nfi_atv <- ggplot(df_robs_atv, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(e) Non-essential activ. restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean

bw_optimal_atv <- r52$bws[[1]]
plot_nfi_atv <- add_optimal_point(plot_nfi_atv, df_robs_atv, bw_optimal_atv)

plot_nfi_circu <- ggplot(df_robs_circu, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(f) Public gathering restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean

bw_optimal_circu <- r62$bws[[1]]
plot_nfi_circu <- add_optimal_point(plot_nfi_circu, df_robs_circu, bw_optimal_circu)

# Combinar gráficos de NPIs
graf <- (plot_nfi_robs + plot_nfi_masks) / (plot_nfi_sani + plot_nfi_trans) / (plot_nfi_atv + plot_nfi_circu)

ggsave("outputs/figures/npi_rob.png", graf,
       width = 10.00, height = 8.00, units = "in")


# Placebo test
placebo           <- robust_check(df$renda_pc,  1, state.d,  k, df$X)
placebo_2016      <- robust_check(df2$renda_pc, 1, state.d2, k, df2$X)

plot_placebo <- ggplot(placebo, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("Placebo - Impact on Per capta Revenue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean

plot_placebo

plot_placebo_2016 <- ggplot(placebo_2016, aes(x = bw, y = coef_conv)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("Placebo - Impact on Per capta Revenue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_conv, ymax = ci_higher_conv), alpha = 0.2) + 
  theme_clean

plot_placebo_2016
