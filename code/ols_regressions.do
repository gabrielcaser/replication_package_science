* ==============================================================================
* Stata Do-File: OLS Regressions with Robust Standard Errors
* Description: Replicates R models from 02_sum_stats.R
* ==============================================================================

clear all
set more off

* Set working directory (adjust as needed)
cd "C:\Users\gabri\Documents\Github\Personal\replication_package_science"

* Load data
use "data\intermediary\data_ols.dta", clear

* ==============================================================================
* Model 1: Main effect of STEM background with state fixed effects
* ==============================================================================

* Filter to 2016 cohort only
preserve
keep if coorte == 1

* Run regression with state fixed effects and robust standard errors
encode sigla_uf, gen(uf_encoded)
reg deaths i.stem_background, vce(robust)
xtreg deaths i.stem_background, fe i(uf_encoded) vce(robust)

* Display results
estimates store model1

* ==============================================================================
* Model 2: Interaction between STEM background and month with state FE
* ==============================================================================

* Run regression with interaction term and robust standard errors
reg deaths i.stem_background##c.month, vce(robust)
xtreg deaths i.stem_background##.month ,  fe i(uf_encoded) vce(robust)

* Display results
estimates store model2

restore

* ==============================================================================
* Display results table
* ==============================================================================

* Compare models
estimates table model1 model2, b(%9.4f) se stats(N r2)

* Export results to LaTeX (optional)
* Uncomment the following lines if you want to export to LaTeX
/*
esttab model1 model2 using "outputs/tables/ols_results.tex", ///
    replace booktabs ///
    b(%9.4f) se(%9.4f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("OLS Regression Results") ///
    mtitles("Model 1" "Model 2") ///
    stats(N r2, labels("Observations" "R-squared"))
*/

* ==============================================================================
* Alternative: Export to RTF or Excel
* ==============================================================================

* Export to Excel
* esttab model1 model2 using "outputs/tables/ols_results.xlsx", replace

* Export to RTF (Word)
* esttab model1 model2 using "outputs/tables/ols_results.rtf", replace

* ==============================================================================
* End of do-file
* ==============================================================================
