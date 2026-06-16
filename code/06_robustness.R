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

# ── 1. HELPER FUNCTIONS ───────────────────────────────────────────────────────
tidy.rdrobust <- function(model, ...) {
  data.frame(term      = row.names(model$coef),
             estimate  = model$coef[1, ],
             std.error = model$se[1, ],
             p.value   = model$pv[1, ],
             conf.low  = model$ci[1, 1],
             conf.high = model$ci[1, 2],
             row.names = NULL)
}

glance.rdrobust <- function(model, ...) {
  data.frame("Eff N obs." = as.character(model$N_h[1] + model$N_h[2]),
             "Bandwidth"  = paste0(round(model$bws[1, 1] * 100, 2), "%"))
}

fix_types <- function(d) {
  d$mulher      <- as.logical(as.numeric(d$mulher))
  d$reeleito    <- as.logical(as.numeric(d$reeleito))
  d$instrucao   <- as.numeric(d$instrucao)
  d$idade       <- as.numeric(d$idade)
  d$tenure      <- as.numeric(d$tenure)
  d$tenure_rais <- as.numeric(d$tenure_rais)
  d$T           <- as.numeric(d$T)
  d
}

make_covs <- function(df, state.d, year.d = 1,
                      include_edu = TRUE, include_inc = TRUE,
                      include_fem = TRUE) {
  cols <- list(state.d, year.d,
               df$ideology_party,
               df$idade, df$idade * df$idade)
  if (include_edu) cols <- c(cols, list(df$instrucao))
  if (include_inc) cols <- c(cols, list(as.numeric(df$reeleito)))
  if (include_fem) cols <- c(cols, list(as.numeric(df$mulher)))
  do.call(cbind, cols)
}

ms_args <- list(
  estimate    = "{estimate}",
  statistic   = c("[{std.error}]", "{p.value}{stars}"),
  coef_rename = c("Robust" = "STEM Background"),
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  fmt    = 2,
  coef_omit = "Corrected|Conventional",
  gof_omit  = "BIC|AIC|Log.Lik.|Adj.R2|R2|RMSE|Std.Errors"
)

# ── 2. LOAD MAIN DATA ─────────────────────────────────────────────────────────
df      <- readRDS(paste(data_dir, "/final/", data_regs, sep = ""))
df_2020 <- readRDS(paste(data_dir, "/final/", data_all_cohorts_regs, sep = ""))
df_2020 <- df_2020[df_2020$coorte == "2020", ]

state.f   <- factor(df$sigla_uf)
state.d   <- model.matrix(~ state.f + 0)
covs_full <- make_covs(df, state.d)

cat(sprintf("2016 N=%d | STEM=%d\n2020 N=%d | STEM=%d\n",
            nrow(df), sum(df$T), nrow(df_2020), sum(df_2020$T)))

################################################################################
# SUPPLEMENTARY METHODS 7: COVARIATE SENSITIVITY
# Table: 4 covariate specs × h = 7%, 8.2%, 9%
################################################################################
cat("\n=== SUPP METHODS 7: COVARIATE SENSITIVITY ===\n")

covs_A <- make_covs(df, state.d, include_edu=TRUE,  include_inc=TRUE,  include_fem=TRUE)
covs_B <- make_covs(df, state.d, include_edu=FALSE, include_inc=TRUE,  include_fem=TRUE)
covs_C <- make_covs(df, state.d, include_edu=FALSE, include_inc=FALSE, include_fem=TRUE)
covs_D <- make_covs(df, state.d, include_edu=FALSE, include_inc=FALSE, include_fem=FALSE)

cov_sens_results <- list()
for (bw in c(0.07, 0.082, 0.09)) {
  for (spec in c("A","B","C","D")) {
    cv  <- get(paste0("covs_", spec))
    lbl <- paste0("h=", bw*100, "% (", spec, ")")
    cov_sens_results[[lbl]] <- rdrobust(df$Y_deaths_sivep, df$X,
                                         kernel=k, p=poli, h=bw, covs=cv)
  }
}

# Print summary
cat(sprintf("  %-8s %-5s %9s %8s %8s\n","BW","Spec","Coef","SE","p"))
for (nm in names(cov_sens_results)) {
  r <- cov_sens_results[[nm]]
  cat(sprintf("  %-14s %9.2f %8.2f %8.3f  N_eff=%d\n",
              nm,
              r$coef["Conventional",],
              r$se["Conventional",],
              r$pv["Conventional",],
              r$N_h[1]+r$N_h[2]))
}

# LaTeX table
bw_specs <- expand.grid(bw=c(0.07,0.082,0.09), spec=c("A","B","C","D"))
models_sens <- lapply(seq_len(nrow(bw_specs)), function(i) {
  cv <- get(paste0("covs_", bw_specs$spec[i]))
  rdrobust(df$Y_deaths_sivep, df$X, kernel=k, p=poli, h=bw_specs$bw[i], covs=cv)
})
names(models_sens) <- paste0("h=", bw_specs$bw*100, "% (", bw_specs$spec, ")")

n_sens <- nrow(bw_specs)
add_rows_7 <- data.frame(
  term = c("Bandwidth", "Specification", "State FE"),
  matrix(c(paste0("h=", bw_specs$bw * 100, "%"),
            as.character(bw_specs$spec),
            rep("Yes", n_sens)),
         nrow = 3, byrow = TRUE,
         dimnames = list(NULL, paste0("Model ", seq_len(n_sens)))),
  check.names = FALSE
)
do.call(modelsummary, c(list(models_sens,
  output   = "outputs/tables/supp_covariate_sensitivity.tex",
  title    = "Covariate Sensitivity — RD estimates across specifications and bandwidths",
  add_rows = add_rows_7),
  ms_args))
cat("\nTable saved to: outputs/tables/supp_covariate_sensitivity.tex\n")

################################################################################
# SUPPLEMENTARY METHODS 8: EDUCATION-SPECIFIC TESTS
################################################################################
cat("\n=== SUPP METHODS 8: EDUCATION TESTS ===\n")

# ── 8a. COLLEGE GRADUATES ONLY (edu=7) — 2016 AND 2020 COHORTS ───────────────
cat("\n--- 8a: College graduates only (edu=7) ---\n")

run_edu7 <- function(df, bws = c(0.082, 0.09, 0.10)) {
  df7 <- df[df$instrucao == 7, ]
  sf  <- factor(df7$sigla_uf)
  sd7 <- model.matrix(~ sf + 0)
  # Education omitted (constant); age dropped as uninformative within college grads
  cv7 <- cbind(sd7, 1,
                as.numeric(df7$mulher),
                df7$ideology_party,
                as.numeric(df7$reeleito))
  lapply(bws, function(bw)
    rdrobust(df7$Y_deaths_sivep, df7$X, kernel=k, p=poli, h=bw, covs=cv7))
}

r16_edu7 <- run_edu7(df)
r20_edu7 <- run_edu7(df_2020)

bws_edu7 <- c(0.082, 0.09, 0.10)
cat(sprintf("  %-8s | %-30s | %-30s\n",
            "BW", "2016 cohort", "2020 cohort"))
for (i in seq_along(bws_edu7)) {
  r16 <- r16_edu7[[i]]; r20 <- r20_edu7[[i]]
  cat(sprintf("  h=%-5.1f%% | coef=%7.2f SE=%6.2f p=%5.3f N=%d | coef=%7.2f SE=%6.2f p=%5.3f N=%d\n",
              bws_edu7[i]*100,
              r16$coef["Conventional",], r16$se["Conventional",],
              r16$pv["Conventional",],   r16$N_h[1]+r16$N_h[2],
              r20$coef["Conventional",], r20$se["Conventional",],
              r20$pv["Conventional",],   r20$N_h[1]+r20$N_h[2]))
}

cat(sprintf("\n  2016 edu=7: N=%d, STEM=%d | 2020 edu=7: N=%d, STEM=%d\n",
            sum(df$instrucao==7), sum(df$T[df$instrucao==7]),
            sum(df_2020$instrucao==7), sum(df_2020$T[df_2020$instrucao==7])))

names(r16_edu7) <- paste0("h=", bws_edu7 * 100, "%")
names(r20_edu7) <- paste0("h=", bws_edu7 * 100, "%")
add_rows_8a <- data.frame(
  term      = c("State FE", "Mayor controls", "Sample"),
  `Model 1` = c("Yes", "Yes", "College only"),
  `Model 2` = c("Yes", "Yes", "College only"),
  `Model 3` = c("Yes", "Yes", "College only"),
  check.names = FALSE
)
do.call(modelsummary, c(list(r16_edu7,
  output   = "outputs/tables/supp_edu7_2016.tex",
  title    = "College Graduates Only (edu=7) — 2016 Cohort — RD estimates",
  add_rows = add_rows_8a),
  ms_args))
cat("    Saved: outputs/tables/supp_edu7_2016.tex\n")
do.call(modelsummary, c(list(r20_edu7,
  output   = "outputs/tables/supp_edu7_2020.tex",
  title    = "College Graduates Only (edu=7) — 2020 Cohort — RD estimates",
  add_rows = add_rows_8a),
  ms_args))
cat("    Saved: outputs/tables/supp_edu7_2020.tex\n")

# ── 8b. RESIDUALIZED STEM TREATMENT ──────────────────────────────────────────
cat("\n--- 8b: Residualized STEM treatment ---\n")

covs_no_edu <- make_covs(df, state.d, include_edu = FALSE)
Z_reg  <- cbind(state.d, df$instrucao, 1)
T_resid <- df$T - lm.fit(Z_reg, df$T)$fitted.values
cat(sprintf("  Corr(T, edu) = %.3f | Corr(T_resid, edu) = %.8f\n",
            cor(df$T, df$instrucao), cor(T_resid, df$instrucao)))

resid_results <- list()
for (bw in c(0.07, 0.082, 0.09)) {
  lbl <- paste0("h=", bw*100, "%")
  tryCatch({
    r <- rdrobust(df$Y_deaths_sivep, T_resid, kernel=k, p=poli, h=bw, covs=covs_no_edu)
    resid_results[[lbl]] <- r
    cat(sprintf("  %s: coef=%8.2f SE=%7.2f p=%6.3f N_eff=%d\n",
                lbl, r$coef["Conventional",], r$se["Conventional",],
                r$pv["Conventional",], r$N_h[1]+r$N_h[2]))
  }, warning = function(w) {
    r <- rdrobust(df$Y_deaths_sivep, T_resid, kernel=k, p=poli, h=bw, covs=covs_no_edu)
    resid_results[[lbl]] <<- r
    cat(sprintf("  %s: coef=%8.2f SE=%7.2f p=%6.3f N_eff=%d [mass points]\n",
                lbl, r$coef["Conventional",], r$se["Conventional",],
                r$pv["Conventional",], r$N_h[1]+r$N_h[2]))
  })
}

if (length(resid_results) > 0) {
  n_resid <- length(resid_results)
  add_rows_8b <- data.frame(
    term = c("State FE", "Mayor controls", "Treatment"),
    matrix(c(rep("Yes", n_resid), rep("Yes", n_resid), rep("Residualized", n_resid)),
           nrow = 3, byrow = TRUE,
           dimnames = list(NULL, paste0("Model ", seq_len(n_resid)))),
    check.names = FALSE
  )
  do.call(modelsummary, c(list(resid_results,
    output   = "outputs/tables/supp_resid_stem.tex",
    title    = "Residualized STEM Treatment — RD estimates",
    add_rows = add_rows_8b),
    ms_args))
  cat("    Saved: outputs/tables/supp_resid_stem.tex\n")
}

# ── 8c. STEM × EDUCATION INTERACTION ─────────────────────────────────────────
cat("\n--- 8c: STEM x Education interaction (h=8.2%, N=109) ---\n")

df_bw      <- df[abs(df$X) <= 0.082, ]
state.f_bw <- factor(df_bw$sigla_uf)
state.d_bw <- model.matrix(~ state.f_bw + 0)
edu_c      <- df_bw$instrucao - mean(df_bw$instrucao)

Z_int <- cbind(df_bw$T, edu_c, df_bw$T * edu_c,
               df_bw$X, df_bw$T * df_bw$X,
               as.numeric(df_bw$mulher), df_bw$ideology_party,
               as.numeric(df_bw$reeleito), df_bw$idade, df_bw$idade^2,
               state.d_bw, 1)

coefs_int <- lm.fit(Z_int, df_bw$Y_deaths_sivep)$coefficients[1:3]

set.seed(42)
B <- 2000
boot_coefs <- matrix(NA, nrow = B, ncol = 3)
n_bw <- nrow(df_bw)
for (b in seq_len(B)) {
  idx <- sample(n_bw, replace = TRUE)
  boot_coefs[b, ] <- tryCatch(
    lm.fit(Z_int[idx, ], df_bw$Y_deaths_sivep[idx])$coefficients[1:3],
    error = function(e) rep(NA, 3)
  )
}
boot_se <- apply(boot_coefs, 2, sd, na.rm = TRUE)
boot_p  <- 2 * pmin(colMeans(boot_coefs > 0, na.rm=TRUE),
                     colMeans(boot_coefs < 0, na.rm=TRUE))

terms <- c("STEM (T)", "Education (centered)", "STEM x Education")
cat(sprintf("  N in bandwidth: %d\n", n_bw))
for (i in 1:3) {
  sig <- ifelse(boot_p[i]<0.01,"***", ifelse(boot_p[i]<0.05,"**",
                ifelse(boot_p[i]<0.10,"*","")))
  cat(sprintf("  %-25s coef=%8.2f  boot_SE=%6.2f  boot_p=%5.3f %s\n",
              terms[i], coefs_int[i], boot_se[i], boot_p[i], sig))
}

tab_8c <- data.frame(
  Term     = terms,
  Estimate = round(coefs_int, 2),
  Boot_SE  = round(boot_se, 2),
  Boot_p   = round(boot_p, 3),
  Sig      = ifelse(boot_p < 0.01, "***", ifelse(boot_p < 0.05, "**",
             ifelse(boot_p < 0.10, "*", "")))
)
datasummary_df(tab_8c,
  output = "outputs/tables/supp_stem_edu_interaction.tex",
  title  = "STEM $\\times$ Education Interaction (OLS, h=8.2\\%, bootstrap SE)")
cat("    Saved: outputs/tables/supp_stem_edu_interaction.tex\n")

################################################################################
# SUPPLEMENTARY METHODS 9: ROBUSTNESS CHECKS
################################################################################
cat("\n=== SUPP METHODS 9: ROBUSTNESS ===\n")

# ── 9a. PLACEBO: 2015 PRE-COVID MORTALITY ─────────────────────────────────────
cat("\n--- 9a: Placebo 2015 ---\n")

# Load 2015 data and merge onto main dataset
if (file.exists("data/intermediary/2015_data.rds")) {
  df_2015 <- readRDS("data/intermediary/2015_data.rds")
  df$Y_deaths_2015 <- df_2015$deaths_sivep_per_100k_inhabitants_2015[
    match(df$id_municipio, df_2015$id_municipio)]
  df$Y_hosp_2015 <- df_2015$hosp_per_100k_inhabitants_2015[
    match(df$id_municipio, df_2015$id_municipio)]
  df$Y_deaths_2015[is.na(df$Y_deaths_2015)] <- 0
  df$Y_hosp_2015[is.na(df$Y_hosp_2015)]     <- 0

  # Rebuild covs after adding 2015 cols
  covs_full <- make_covs(df, state.d)

  run_placebo <- function(outcome, label) {
    mods <- list(
      rdrobust(outcome, df$X, kernel=k, p=poli, bwselect="mserd", covs=covs_full),
      rdrobust(outcome, df$X, kernel=k, p=poli, h=janela-0.01, covs=covs_full),
      rdrobust(outcome, df$X, kernel=k, p=poli, h=janela,      covs=covs_full),
      rdrobust(outcome, df$X, kernel=k, p=poli, h=janela+0.01, covs=covs_full),
      rdrobust(outcome, df$X, kernel=k, p=poli, h=1.00,        covs=covs_full)
    )
    cat(sprintf("  %s:\n", label))
    for (i in seq_along(mods)) {
      r  <- mods[[i]]
      bw_label <- c("Optimal","h=7%","h=8%","h=9%","Full")[i]
      sig <- ifelse(r$pv["Conventional",]<0.01,"***",
                    ifelse(r$pv["Conventional",]<0.05,"**",
                           ifelse(r$pv["Conventional",]<0.10,"*","")))
      cat(sprintf("    %-8s coef=%8.2f  SE=%7.2f  p=%6.3f  N_eff=%d %s\n",
                  bw_label,
                  r$coef["Conventional",], r$se["Conventional",],
                  r$pv["Conventional",],   r$N_h[1]+r$N_h[2], sig))
    }
    do.call(modelsummary, c(list(mods,
      output = paste0("outputs/tables/supp_placebo_", label, ".tex"),
      title  = paste0("Placebo: ", label, " (2015) — RD estimates"),
      add_rows = data.frame(
        term=c("Type of Bandwidth","State FE","Mayor controls"),
        `Model 1`=c("Optimal","Yes","Yes"), `Model 2`=c("Fixed","Yes","Yes"),
        `Model 3`=c("Fixed","Yes","Yes"),   `Model 4`=c("Fixed","Yes","Yes"),
        `Model 5`=c("Fixed","Yes","Yes"),   check.names=FALSE)),
      ms_args))
    cat(sprintf("    Saved: outputs/tables/supp_placebo_%s.tex\n", label))
  }

  run_placebo(df$Y_deaths_2015, "deaths_2015")
  run_placebo(df$Y_hosp_2015,   "hosp_2015")

} else {
  cat("  data/intermediary/2015_data.rds not found — skipping placebo\n")
}

# ── 9b. ALTERNATIVE COVID OUTCOME WINDOWS (±15 and ±30 days) ─────────────────
cat("\n--- 9b: Alternative outcome windows ---\n")

window_suffixes <- c("minus30", "minus15", "plus15", "plus30")
window_labels   <- c("-30 days", "-15 days", "+15 days", "+30 days")

for (i in seq_along(window_suffixes)) {
  fname <- paste0("rdd_data_all_2016_broad_definition_", window_suffixes[i], ".rds")

  if (!file.exists(fname)) {
    cat(sprintf("  %s: file not found — skipping\n", window_labels[i]))
    next
  }

  df_w      <- fix_types(as.data.frame(readRDS(fname)))
  state.f_w <- factor(df_w$sigla_uf)
  state.d_w <- model.matrix(~ state.f_w + 0)
  covs_w    <- cbind(state.d_w, 1,
                     as.numeric(df_w$mulher), df_w$ideology_party,
                     df_w$instrucao, as.numeric(df_w$reeleito),
                     df_w$idade, df_w$idade^2)

  mods_w <- list(
    rdrobust(df_w$Y_deaths_sivep, df_w$X, kernel=k, p=poli, bwselect="mserd", covs=covs_w),
    rdrobust(df_w$Y_deaths_sivep, df_w$X, kernel=k, p=poli, h=janela-0.01, covs=covs_w),
    rdrobust(df_w$Y_deaths_sivep, df_w$X, kernel=k, p=poli, h=janela,      covs=covs_w),
    rdrobust(df_w$Y_deaths_sivep, df_w$X, kernel=k, p=poli, h=janela+0.01, covs=covs_w),
    rdrobust(df_w$Y_deaths_sivep, df_w$X, kernel=k, p=poli, h=1.00,        covs=covs_w)
  )

  cat(sprintf("  Window %s (N=%d):\n", window_labels[i], nrow(df_w)))
  for (j in seq_along(mods_w)) {
    r  <- mods_w[[j]]
    bw_label <- c("Optimal","h=7%","h=8%","h=9%","Full")[j]
    sig <- ifelse(r$pv["Conventional",]<0.01,"***",
                  ifelse(r$pv["Conventional",]<0.05,"**",
                         ifelse(r$pv["Conventional",]<0.10,"*","")))
    cat(sprintf("    %-8s coef=%8.2f  p=%6.3f  N_eff=%d %s\n",
                bw_label, r$coef["Conventional",],
                r$pv["Conventional",], r$N_h[1]+r$N_h[2], sig))
  }

  do.call(modelsummary, c(list(mods_w,
    output = paste0("outputs/tables/supp_window_", window_suffixes[i], ".tex"),
    title  = paste0("RD estimates — outcome window ", window_labels[i]),
    add_rows = data.frame(
      term=c("Type of Bandwidth","State FE","Mayor controls"),
      `Model 1`=c("Optimal","Yes","Yes"), `Model 2`=c("Fixed","Yes","Yes"),
      `Model 3`=c("Fixed","Yes","Yes"),   `Model 4`=c("Fixed","Yes","Yes"),
      `Model 5`=c("Fixed","Yes","Yes"),   check.names=FALSE)),
    ms_args))
  cat(sprintf("    Saved: outputs/tables/supp_window_%s.tex\n", window_suffixes[i]))
}

cat("\n=== ALL SUPPLEMENTARY METHODS 7–9 COMPLETE ===\n")
cat("Tables saved in: outputs/tables/\n")
