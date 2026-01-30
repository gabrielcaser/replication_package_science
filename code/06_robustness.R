# Program - This program runs robustness checks using different COVID data windows


# Define the COVID data suffixes to loop through (excluding "original" which is the main analysis)
covid_suffixes <- c("minus30", "minus15", "plus15", "plus30")

# Creating tidy function for rdrobust
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

# Rdrobust function
run_rdrobust_models <- function(df, state.d, year.d, poli, k, janela, outcome) {
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

# Loop through each COVID data variation
for (covid_suffix in covid_suffixes) {
  
  # Update the data file name with the current suffix
  if (stem_definition == "strict") {
    data_file <- paste0("rdd_data_college_mayors_only_2016_strict_definition_", covid_suffix, ".rds")
  } else {
    data_file <- paste0("rdd_data_college_mayors_only_2016_broad_definition_", covid_suffix, ".rds")
  }
  
  print(paste0("Processing robustness check for: ", covid_suffix))
  
  # Load data
  df <- readRDS(paste0(data_dir, "/final/", data_file))
  
  # Creating state dummies for fixed effects
  state.f <- factor(df$sigla_uf)
  state.d <- model.matrix(~state.f+0)
  year.d <- 1
  
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
  
  # Running models for this specification
  models_death <- run_rdrobust_models(df, state.d, year.d, poli, k, janela, df$Y_deaths_sivep)
  models_hosp  <- run_rdrobust_models(df, state.d, year.d, poli, k, janela, df$Y_hosp)
  
  # Extract optimal bandwidths
  optimal_bw       <- models_death[[1]]$bws[[1]]
  optimal_bw_hosp  <- models_hosp[[1]]$bws[[1]]
  
  # Creating table for deaths
  modelsummary(
    models_death,
    estimate = "{estimate}",
    statistic = c("[{std.error}]", "{p.value}{stars}"),
    coef_rename = c("Robust" = "RD estimator"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    fmt = 2,
    output = paste0("outputs/tables/estimates_", covid_suffix, ".tex"),
    title = paste0("Impact of STEM Leadership on Deaths — RD estimates (", covid_suffix, ")"),
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
  
  print(paste0("Saved: outputs/tables/estimates_", covid_suffix, ".tex"))
  
  # Creating table for hospitalizations
  modelsummary(
    models_hosp,
    estimate = "{estimate}",
    statistic = c("[{std.error}]", "{p.value}{stars}"),
    coef_rename = c("Robust" = "RD estimator"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    fmt = 2,
    output = paste0("outputs/tables/estimates_hosp_", covid_suffix, ".tex"),
    title = paste0("Impact of STEM Leadership on Hospitalizations — RD estimates (", covid_suffix, ")"),
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
  
  print(paste0("Saved: outputs/tables/estimates_hosp_", covid_suffix, ".tex"))
  
}

print("Robustness checks completed for all COVID data variations.")
