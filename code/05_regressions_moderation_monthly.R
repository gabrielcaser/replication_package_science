# This program runs moderation analysis with monthly data
# Testing heterogeneous effects by month using rdrobust

set.seed(1234)

# Opening monthly data ----------------------------------------------------
df_monthly <- readRDS(paste(data_dir, "/final/rdd_data_monthly_", definition, "_definition.rds", sep = ""))

# Data preparation --------------------------------------------------------

# Remove rows with missing outcomes
df_monthly <- df_monthly %>%
  filter(!is.na(Y_deaths_sivep) | !is.na(Y_hosp))

# Create numeric month variable for interaction
df_monthly <- df_monthly %>%
  mutate(month_num = as.numeric(as.factor(month)))

# Center the month variable for better interpretation
df_monthly <- df_monthly %>%
  mutate(month_centered = month_num - mean(month_num, na.rm = TRUE))

# Create interaction term
df_monthly$stem_x_month <- df_monthly$stem_background * df_monthly$month_centered

# Summary statistics by month
cat("\n=== Summary Statistics by Month ===\n")
summary_by_month <- df_monthly %>%
  group_by(month) %>%
  summarise(
    n_obs = n(),
    n_stem = sum(stem_background == 1, na.rm = TRUE),
    mean_deaths = mean(Y_deaths_sivep, na.rm = TRUE),
    mean_hosp = mean(Y_hosp, na.rm = TRUE)
  )
print(summary_by_month)

# RD Robust Analysis with heterogeneous effects --------------------------

cat("\n=== RD Robust Analysis: Deaths (SIVEP) ===\n")

# Main effect (pooled across all months)
rd_deaths_main <- rdrobust(
  y = df_monthly$Y_deaths_sivep,
  x = df_monthly$X,
  c = 0,
  kernel = "triangular",
  bwselect = "mserd",
  vce = "hc1"
)

cat("\nMain Effect (Pooled):\n")
print(summary(rd_deaths_main))

# Heterogeneous effects by month using regression approach
# We'll use a panel regression approach with month fixed effects and interactions

# Create panel data
df_monthly <- df_monthly %>%
  arrange(id_municipio, coorte, month)

cat("\n=== Panel Regression with Month Interactions ===\n")

# Model with month fixed effects and interaction
model_het_deaths <- felm(
  Y_deaths_sivep ~ stem_background + stem_background:month_centered | 
    month + sigla_uf | 0 | id_municipio,
  data = df_monthly
)

cat("\nHeterogeneous Effects - Deaths (SIVEP):\n")
print(summary(model_het_deaths))

# Model for hospitalizations
model_het_hosp <- felm(
  Y_hosp ~ stem_background + stem_background:month_centered | 
    month + sigla_uf | 0 | id_municipio,
  data = df_monthly
)

cat("\nHeterogeneous Effects - Hospitalizations:\n")
print(summary(model_het_hosp))

# With controls
model_het_deaths_controls <- felm(
  Y_deaths_sivep ~ stem_background + stem_background:month_centered + 
    mulher + ideology_party + reeleito + instrucao | 
    month + sigla_uf | 0 | id_municipio,
  data = df_monthly
)

cat("\nHeterogeneous Effects - Deaths (with controls):\n")
print(summary(model_het_deaths_controls))

model_het_hosp_controls <- felm(
  Y_hosp ~ stem_background + stem_background:month_centered + 
    mulher + ideology_party + reeleito + instrucao | 
    month + sigla_uf | 0 | id_municipio,
  data = df_monthly
)

cat("\nHeterogeneous Effects - Hospitalizations (with controls):\n")
print(summary(model_het_hosp_controls))

# Alternative approach: Run RD separately by month groups ----------------

cat("\n=== RD Analysis by Month Groups ===\n")

# Get unique months and divide into groups
unique_months <- sort(unique(df_monthly$month))
n_months <- length(unique_months)

# Early, middle, and late periods
early_months <- unique_months[1:floor(n_months/3)]
middle_months <- unique_months[(floor(n_months/3)+1):floor(2*n_months/3)]
late_months <- unique_months[(floor(2*n_months/3)+1):n_months]

# Early period
df_early <- df_monthly %>% filter(month %in% early_months)
rd_early <- rdrobust(
  y = df_early$Y_deaths_sivep,
  x = df_early$X,
  c = 0,
  kernel = "triangular",
  bwselect = "mserd"
)

cat("\nEarly Period (", paste(early_months[1], "-", early_months[length(early_months)]), "):\n")
print(summary(rd_early))

# Middle period
df_middle <- df_monthly %>% filter(month %in% middle_months)
rd_middle <- rdrobust(
  y = df_middle$Y_deaths_sivep,
  x = df_middle$X,
  c = 0,
  kernel = "triangular",
  bwselect = "mserd"
)

cat("\nMiddle Period (", paste(middle_months[1], "-", middle_months[length(middle_months)]), "):\n")
print(summary(rd_middle))

# Late period
df_late <- df_monthly %>% filter(month %in% late_months)
rd_late <- rdrobust(
  y = df_late$Y_deaths_sivep,
  x = df_late$X,
  c = 0,
  kernel = "triangular",
  bwselect = "mserd"
)

cat("\nLate Period (", paste(late_months[1], "-", late_months[length(late_months)]), "):\n")
print(summary(rd_late))

# Export results ----------------------------------------------------------

# Create summary table comparing periods
results_summary <- data.frame(
  Period = c("Early", "Middle", "Late"),
  Coefficient = c(rd_early$coef[1], rd_middle$coef[1], rd_late$coef[1]),
  SE = c(rd_early$se[1], rd_middle$se[1], rd_late$se[1]),
  P_value = c(rd_early$pv[1], rd_middle$pv[1], rd_late$pv[1]),
  N = c(rd_early$N[1] + rd_early$N[2], 
        rd_middle$N[1] + rd_middle$N[2], 
        rd_late$N[1] + rd_late$N[2])
)

cat("\n=== Summary of Results by Period ===\n")
print(results_summary)

# Create regression table with modelsummary

models_list <- list(
  "Deaths (No Controls)" = model_het_deaths,
  "Deaths (Controls)" = model_het_deaths_controls,
  "Hosp (No Controls)" = model_het_hosp,
  "Hosp (Controls)" = model_het_hosp_controls
)

modelsummary(
  models_list,
  output = paste(output_dir, "/tables/moderation_month_interaction.tex", sep = ""),
  title = "Heterogeneous Effects of STEM Background by Month",
  stars = TRUE,
  coef_map = c(
    "stem_background1" = "STEM Background",
    "stem_background1:month_centered" = "STEM x Month (centered)",
    "mulherTRUE" = "Woman Mayor",
    "ideology_party" = "Ideology of the Party",
    "reeleitoTRUE" = "Reelected",
    "instrucao" = "Education Level"
  ),
  gof_omit = "IC|Log|Adj|Within|R2|RMSE|Std.Errors|FE",
  notes = "Standard errors clustered at municipality level. Month and state fixed effects included."
)

cat("\n=== Analysis Complete ===\n")
cat("Monthly dataset contains", nrow(df_monthly), "observations\n")
cat("Results saved to:", paste(output_dir, "/tables/moderation_month_interaction.tex\n"))
