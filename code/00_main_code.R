# Description - This code reproduces all tables and figures from the paper
renv::restore() # RUN IT FIRST to download the required libraries
##renv::install("jsonlite", type = "binary") # run that in case you receive any error message during the instalation of the jsonlite package

# Initial Commands --------------------------------------------------------
rm(list = ls(all.names = TRUE)) # clear objects
gc() # free up memory
set.seed(1234)

# Libraries ---------------------------------------------------------------
library("tidyverse")    # to handle data
library("estimatr")     # to run regressions with robust se
library("modelsummary") # to create tables
library("gt")           # better tables
library('geobr')        # to create maps
library('skimr')        # to create sumstats
library('rdrobust')     # to run rdd
library('patchwork')    # to create figures with plots together
library('knitr')        # render presentations
library('forcats')      # better figures
library('plm')          # regressions with fixed effects
library('rdhte')        # heterogeneous treatment effects in rdd
library('sandwich')     # robust standard errors
library('lmtest')       # coeftest function
library('lfe')          # high-dimensional fixed effects
library('data.table')   # to handle large data
library('arrow')        # to read parquet files

# Setting -----------------------------------------------------------------
output_dir                     = paste0(getwd(),"/outputs")
data_dir                       = paste0(getwd(),"/data")

# Parameters --------------------------------------------------------------
stem_definition        = "broad"                # "broad" (Machado) or "strict" (Machine Learning selection based on online profiles)
non_stem_college       = "all"                  # "college_mayors_only" OR "all"
cohort_filter          = "2016_"                # "2016_" "2020_" or ""
poli                   = 1                      # Functional form (1 or 2)
janela                 = 0.08                   # Defining margin of victory for robustness tests (0.05, 0.10 or 1.00)
k                      = "uniform"              # Kernel triangular or uniform  
deaths_and_hosp_in_log = "no"                   # yes or no

start_date_covid_2016       = "2020-02-01"           # Start date for covid outcomes
end_date_covid_2016         = "2021-02-28"           # End date for covid outcomes cohort 2016
end_date_covid_2020         = "2021-12-31"           # End date for covid outcomes

# Defining dataset
data             = paste0("rdd_data_", non_stem_college,"_", cohort_filter, stem_definition, "_definition.Rds") 
data_all_cohorts = paste0("rdd_data_", non_stem_college, "_", "", stem_definition, "_definition.Rds")

# Running scripts ---------------------------------------------------------
# Create baseline data (health, ideology, political, npi, density, STEM classification)
source("code/creates_baseline_data.R")

# Create COVID data 
#source("code/creates_covid_data.R") #(requires large raw files -> contact authors for access)
source("code/construct_covid_data.R")

# Create final RDD dataset
source("code/01_create_dataset.R")

# Generate summary statistics
source("code/02_sum_stats.R")

# Run main regressions
source("code/03_regressions_main.R")

# Run moderation analysis
source("code/04_regressions_moderation.R")

# Generate LaTeX parameters
source("code/05_parameters_latexR.R")

# Create robustness tables
source("code/06_robustness.R")

print("All tables and figures have been reproduced and saved in the outputs folder.")
# End of code -------------------------------------------------------------