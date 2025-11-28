# Data Pipeline Documentation

This document describes the complete data pipeline for the replication package.

## Data Creation Pipeline

### 1. Baseline Data (`create_baseline_data.R`)

Creates intermediary datasets from raw data files:

**Input files** (in `data/raw/`):
- `221130_16_indicadores_mun_data_ieps_2019_2015.csv` - Health indicators (source: https://iepsdata.org.br/data-downloads)
- `223007_partidos_ano_v2018.csv` - Political party ideology scores (source: https://dataverse.harvard.edu/)
- `221011_municipal_ideology.csv` - Municipality ideology scores (source: https://dataverse.harvard.edu/)
- `220927_cnm_medidas_sanitarias.xlsx` - Non-pharmaceutical interventions (NPIs)
- `densidade_2015.xls` - Population density data
- `stem_classification.xlsx` - STEM occupation classification

**Output files** (in `data/intermediary/`):
- `health_data.Rds` - Municipal health indicators (doctors, beds, health spending, etc.)
- `political_data.Rds` - Political party ideology scores
- `ideology_data.Rds` - Municipality ideology scores
- `npi_data.Rds` - Non-pharmaceutical interventions data
- `density_data.Rds` - Population density data
- `stem_classification_eleito.Rds` - STEM classification for elected mayors
- `stem_classification_naoeleito.Rds` - STEM classification for non-elected candidates

### 2. COVID Data (`clean_covid_data.R` + `construct_covid_outcomes.R`)

**Note:** COVID raw data files are not included in the GitHub repository due to size limitations. Contact the authors to access them.

**Part 1 - Clean COVID data** (`clean_covid_data.R`):
- Reads SIVEP-Gripe data for 2020 and 2021
- Cleans and standardizes variables
- Filters and recodes key variables

**Output files** (in `data/intermediary/`):
- `sivep_2020.rds` - Cleaned 2020 COVID data
- `sivep_2021.rds` - Cleaned 2021 COVID data
- `sivep_full.rds` - Combined COVID data
- `covid_day_data.rds` - Daily COVID data

**Part 2 - Construct outcomes** (`construct_covid_outcomes.R`):
- Creates aggregated COVID outcomes by municipality and cohort
- Calculates deaths and hospitalizations per 100k inhabitants
- Merges with population data

**Output files** (in `data/intermediary/`):
- `covid_data.rds` - Final COVID outcomes dataset

### 3. Candidates and Tenure Data

**Pre-existing files** (copied to `data/intermediary/`):
- `candidates_dataset.Rds` - Electoral and education data for mayoral candidates
- `tenure_data.Rds` - Mayors' tenure information

These files were created by external scripts in the original replication folder.

### 4. Final RDD Dataset (`01_create_dataset.R`)

Merges all intermediary datasets to create the final RDD analysis dataset.

**Output files** (in `data/final/`):
- `rdd_data_all_broad_definition.rds` - All municipalities, broad STEM definition
- `rdd_data_all_strict_definition.rds` - All municipalities, strict STEM definition
- `rdd_data_all_2016_broad_definition.rds` - 2016 cohort only, broad definition
- `rdd_data_all_2016_strict_definition.rds` - 2016 cohort only, strict definition
- `rdd_data_college_mayors_only_broad_definition.rds` - College-educated mayors only, broad definition
- `rdd_data_college_mayors_only_strict_definition.rds` - College-educated mayors only, strict definition

## Running the Complete Pipeline

To replicate all data creation steps, run in `00_main_code.R`:

```r
# 1. Create baseline data
source("code/create_baseline_data.R")

# 2. Create COVID data (requires raw files)
source("code/clean_covid_data.R")
source("code/construct_covid_outcomes.R")

# 3. Create final RDD dataset
source("code/01_create_dataset.R")
```

## Data Sources

1. **Health Indicators**: IEPS Data (https://iepsdata.org.br/data-downloads)
2. **Political Ideology**: Harvard Dataverse (doi:10.7910/DVN/8USPML/20URAD)
3. **COVID Data**: SIVEP-Gripe/OpenDataSUS (https://opendatasus.saude.gov.br/)
4. **Electoral Data**: TSE (Tribunal Superior Eleitoral)
5. **NPIs**: CNM Survey (Confederação Nacional de Municípios)

## Notes

- All intermediary files use `.Rds` format for efficient storage
- Raw data files in `.csv`, `.xlsx`, and `.xls` formats are kept in `data/raw/`
- The pipeline is designed to be fully reproducible given access to raw data files
- STEM definitions: "broad" includes all STEM occupations; "strict" requires both STEM occupation and STEM degree
