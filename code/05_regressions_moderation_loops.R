# Libraries --------------------------------------------------------------
library(dplyr)
library(plm)
library(readxl)

# Parameters --------------------------------------------------------------
stem_definitions <- c("strict", "broad") # STEM definitions
non_stem_colleges <- c("all", "college_mayors_only") # College education conditions
cohort_filters <- c("2016_", "") # Cohort filters
windows <- c(0.05, 0.10, 1.00) # Margins of victory
deaths_and_hosp_options <- c("yes", "no") # Log-transform options for outcomes
outcomes <- c("Y_deaths_sivep", "Y_hosp", "total_nfi") # Outcomes to be analyzed
personal_characteristics_controls <- c("yes", "no")

# Loop through parameter combinations -------------------------------------
results_list <- list() # To store results from each iteration

# Create variables for each combination of parameters
for (stem_definition in stem_definitions) {
  for (non_stem_college in non_stem_colleges) {
    for (cohort_filter in cohort_filters) {
      for (window in windows) {
        for (deaths_and_hosp_in_log in deaths_and_hosp_options) {
          for (outcome in outcomes) {
            for (controls in personal_characteristics_controls) {
              
              # Define dataset name
              data <- paste0(
                "rdd_data_", non_stem_college, "_", cohort_filter,
                stem_definition, "_definition.Rds"
              )
              
              # Open dataset
              df <- readRDS(paste(data_dir, "/final/", data, sep = ""))
              
              # Merge with revenue data -----------------------------------
              data_revenue <- readxl::read_excel(
                paste(data_dir, "/raw/ipea_receitas_municipais.xlsx", sep = ""),
                sheet = "Receitas Totais"
              )
              
              data_revenue <- data_revenue %>%
                summarise(
                  id_municipio = Cod.IBGE,
                  receita_2015 = `2015` / 10000000
                )
              
              df <- merge(df, data_revenue, by = "id_municipio")
              
              # Data preparation -----------------------------------------
              df$tenure <- df$tenure / 12
              df_subset <- subset(df, X >= -1 * window & X <= window)
              
              df_subset$inter_receita_stem <- df_subset$receita_2015 * (as.double(df_subset$stem_background) - 1)
              df_subset$inter_tenure_stem <- df_subset$tenure * (as.double(df_subset$stem_background) - 1)
              df_subset$zero <- 0
              
              # Panel data setup
              pdata <- pdata.frame(df_subset, c("sigla_uf"))
              
              # Define controls
              if (controls == "yes") {
                covsZ <- cbind(
                  pdata$mulher,
                  pdata$ideology_party,
                  pdata$instrucao,
                  pdata$reeleito,
                  pdata$idade
                )
              } else {
                covsZ <- cbind(pdata$zero)
              }
              
              # Run regression ------------------------------------------
              results <- plm(
                get(outcome) ~ X + T + T_X + inter_tenure_stem + tenure +
                  inter_receita_stem + receita_2015 + covsZ,
                data = pdata,
                index = c("sigla_uf", "coorte"),
                model = "within",
                effect = "twoways"
              )
              
              # Dynamically create a variable for each combination of parameters
              var_name <- paste0(
                "results_", stem_definition, "_", non_stem_college, "_", cohort_filter, 
                "_", window, "_", deaths_and_hosp_in_log, "_", outcome, "_", controls
              )
              
              # Store results as separate variables
              assign(var_name, summary(results))
              
              # Store results summary in the list for future access
              results_list[[var_name]] <- summary(results)
            }
          }
        }
      }
    }
  }
}

# Extract relevant variables and create a table ---------------------------
results_table <- data.frame()

# Loop over the dynamically created results variables
for (name in names(results_list)) {
  model_summary <- results_list[[name]]
  coefficients <- model_summary$coefficients
  
  # Check if the relevant variables exist in the coefficients
  relevant_vars <- coefficients[c("T", "inter_tenure_stem", "inter_receita_stem"), , drop = FALSE]
  
  # Ensure that relevant variables exist and do not contain NaN values
  if (!any(is.na(relevant_vars))) {
    # Convert to data frame and add metadata
    relevant_vars <- data.frame(
      variable = rownames(relevant_vars),
      att = relevant_vars[, "Estimate"],
      se = relevant_vars[, "Std. Error"],
      ci_low = relevant_vars[, "Estimate"] - 1.96 * relevant_vars[, "Std. Error"],
      ci_high = relevant_vars[, "Estimate"] + 1.96 * relevant_vars[, "Std. Error"],
      p_value = relevant_vars[, "Pr(>|t|)"],
      parameters = name
    )
    
    # Append to the results table
    results_table <- rbind(results_table, relevant_vars)
  } else {
    # Print a message to debug if coefficients are missing or NaN
    cat("Missing or NaN coefficients for model:", name, "\n")
  }
}

# Display results table
print(results_table)
