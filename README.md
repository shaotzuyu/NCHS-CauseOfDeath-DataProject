# NCHS-Causes-of-Death-Data-Cleaning

This repository contains working code & ideas using county-level causes of death records from the (restricted) US National Vital Statistics System (NVSS), ACS, and/or census.
Detailed descriptions are provided in the files.

File Descriptions
1. MMR-calculation.R
  - Purpose: Calculates the Multiple Mortality Rate (MMR) for each county and year.
  - Function: Analyzes death records to identify cases with multiple distinct ICD-10 cause-of-death classifications and computes MMR at the county level.

2. NVSS-CoD-loop.R
  - Purpose: Prepares county-level mortality data (2000–2022) for analysis by standardizing cause-of-death coding.
  - Function: Cleans, formats, and ensures each county has complete cause-of-death and age-group mortality records to allow accurate comparisons over time.
    
3. NVSS-CoD-network.R
  - Purpose: A working project using a network approach to analyze multiple underlying causes of death across counties.
  - Function: Constructs county-level networks based on shared cause-of-death patterns.

4. PAFM_Building.R
  - Purpose: A working project looking at mismatches between the place of death and residence in U.S. mortality data (2003–2022).
  - Function: Constructs individual-level records identifying county- and state-level mismatches, and classifies causes of death.
  - Includes a Stata file "PAFM-analysis.do" running preliminary logit models in 2010 and 2020.
