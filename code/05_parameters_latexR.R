# Define the parameter values
dados_sumstats      <- readRDS(paste(data_dir, "/final/", data, sep = ""))
nobs_sumstats       <- nrow(dados_sumstats[dados_sumstats$coorte == 2016, ])
nobs_sumstats_stem  <- nrow(dados_sumstats[dados_sumstats$stem_background == 1 & dados_sumstats$coorte == 2016, ])
cohorts_used        <- 2016 #unique(dados_sumstats$coorte)
college_description <- ifelse(non_stem_college == "yes", " with college degree", "")
cohort_fe           <- "" #ifelse(cohort_filter == "", "and cohort ", "")
log_outcomes        <- ifelse(deaths_and_hosp_in_log == "yes", " in the form of inverse hyperbolic sine","")

# Create a LaTeX file with the parameter values
tex_file_path <- paste0(output_dir,"/parameters_overleaf.tex")

# Open file for writing
fileConn <- file(tex_file_path, open = "w")

# Write the LaTeX code to the file
writeLines(c(
  "\\newcommand{\\parNobsSumstats}{", nobs_sumstats, "}",
  "\\newcommand{\\parNobsSumstatsStem}{", nobs_sumstats_stem, "}",
  "\\newcommand{\\parCohortsUsed}{", paste(cohorts_used, collapse = " and "), "}",
  "\\newcommand{\\parCollegeDescription}{", college_description, "}",
  "\\newcommand{\\parCohortFe}{", cohort_fe, "}",
  "\\newcommand{\\parLogOutcomes}{", ifelse(deaths_and_hosp_in_log == "yes", " in the form of inverse hyperbolic sine", "in number of persons"), "}",
  "\\newcommand{\\parKernel}{", k, "}",
  "\\newcommand{\\parWindow}{", janela*100, "}",
  "\\newcommand{\\parPoli}{", poli, "}",
  "\\newcommand{\\parMainEffectDeaths}{", round(models_panelA[[2]]$coef[[1]],2), "}"
  
), fileConn)

# Close the file
close(fileConn)

# Notify the user
cat("The parameter values have been saved in 'parameters.tex'. You can upload this file to Overleaf.\n")
