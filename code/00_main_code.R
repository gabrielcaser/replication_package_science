
# Description - This code reproduces all tables and figures from the paper


renv::restore() # RUN IT FIRST to download the required libraries

##renv::install("jsonlite", type = "binary") # run that in case you receive any error message during the instalation of the jsonlite package




# Initial Commands --------------------------------------------------------

rm(list = ls(all.names = TRUE)) # clear objects
gc() # free up memory
set.seed(1234)


# Libraries ---------------------------------------------------------------
library("tidyverse")    # to handle data
library("estimatr")
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
# Setting -----------------------------------------------------------------

output_dir                     = paste0(getwd(),"/outputs")
data_dir                       = paste0(getwd(),"/data")

mayors_data_dir                = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/3_create_education_data/output/data"
baseline_data_dir              = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/5_create_baseline_data/output/data"
covid_data_dir                 = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/4_create_covid_data/output/data"
tenure_data_dir                = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/6_create_rdd_dataset/input"


# Parameters --------------------------------------------------------------
stem_definition        = "broad"                # "broad" (Machado) or "strict" (Machine Learning selection based on online profiles)
non_stem_college       = "all"                  # "college_mayors_only" OR "all"
cohort_filter          = ""                     # "2016_" "2020_" or ""
poli                   = 1                      # Functional form (1 or 2)
janela                 = 0.15                   # Defining margin of victory for robustness tests (0.05, 0.10 or 1.00)
k                      = "uniform"              # Kernel triangular or uniform  
deaths_and_hosp_in_log = "no"                  # yes or no

# Defining dataset
data   = paste0("rdd_data_", non_stem_college,"_", cohort_filter, stem_definition, "_definition.Rds") 
data_2016 = paste0("rdd_data_", non_stem_college, "_", "2016_", stem_definition, "_definition.Rds")

# Running scripts ---------------------------------------------------------
source("code/creates_covid_data.R")
source("code/01_create_dataset.R")
#source("code/02_sum_stats.R")
#source("code/03_regressions_main.R")
#source("code/04_regressions_moderation.R")
#source("code/05_regressions_moderation_loops.R")
#source("code/06_parameters_latexR.R")
