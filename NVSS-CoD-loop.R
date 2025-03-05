#######################################################
# Project: Building COD data
# Start date: 10 Feb, 2025
# Update date: 5 March, 2025
#######################################################

library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)

# ------------------------------------------------------------------------------------------------------------ #
# This script processes U.S. county-level mortality data (2000–2022) into a consistent format for analysis. 
# It standardizes age groups, classifies causes of death, and makes sure no missing county-age combinations. 
# Data from 2000–2002 uses a different age coding system, 
#   - so the script maps it to match 2003–2022 to create a unified dataset.
# ------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------ #
# Saved Output Files:
# ------------------------------------------------------------------------------------------------------------ #
# 1 nvss_cod_YYYY.csv
#   - County-level mortality data for each year (YYYY = 2000-2022).
#   - Contains deaths classified by FIPS county codes, age groups, and ICD-10 causes of death.
#   - Source: Raw mortality data processed to standardize age groups and causes of death.

# 2 nvss_cod_collapsed_YYYY.csv
#   - County-level mortality data, aggregated to match ACS age categories.
#   - Checks every county has all age groups represented, even if no deaths were recorded.

# 3 nvss_cod_gp_YYYY.csv
#   - Mortality data grouped into broad cause-of-death categories.
#   - Consolidates ICD-10 causes into major public health categories.

# 4 death_comparison.csv
#   - Summary of total deaths in raw vs. processed datasets (2000-2022).
#   - Helps validate that no deaths were lost during processing.

# 5 Full yearly mortality files
#   - 'nvss_cod_collapsed_YYYY.csv' -> summarize mortality counts by county and ACS-aligned age group.
#   - 'nvss_cod_gp_YYYY.csv' -> broad cause-of-death classifications by county and age group.
#   - 'nvss_cod_YYYY.csv' -> raw processed mortality data with ICD-10 groupings before summarization.

# 6 Yearly collapsed files (summed across age groups)
#   - 'nvss_cod_collapsed_YYYY_sum.csv' -> mortality counts aggregated to the county-year level.
#   - 'nvss_cod_gp_YYYY_sum.csv' -> broad cause-of-death counts summed across all age groups by county-year.
#   - 'nvss_cod_YYYY_sum.csv' -> raw processed mortality data collapsed to county-year without age breakdown.

# 7 Final merged panel datasets
#   - 'nvss_cod_collapsed_all.csv' -> full dataset with mortality counts at the county-year level.
#   - 'nvss_cod_gp_all.csv' -> county-year panel with broad cause-of-death classifications.
#   - 'nvss_cod_all.csv' -> merged dataset of all raw processed mortality records at the county-year level.

# 8 FIPS-adjusted mortality datasets
#   - 'nvss_cod_collapsed_YYYY_sum_FipsAdj.csv' -> County-year mortality data with corrected FIPS codes.
#   - 'nvss_cod_gp_YYYY_sum_FipsAdj.csv' -> Broad cause-of-death counts with valid FIPS only.
#   - 'nvss_cod_all_sum_FipsAdj.csv' -> Final cleaned county-year mortality dataset with valid FIPS.
# ------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------ #
# 2000-2002
# These years have different coding than the rest
# ------------------------------------------------------------------------ #

# Set base directory
base_dir <- ""

# Loop over years 2000-2002
for (year in 2000:2002) {
  
  # Construct file path for the current year
  year_folder <- paste0("MULT", year, ".USPSAllCnty")
  file_path <- paste0(base_dir, year_folder, "/MULT", year, ".USAllCnty.txt")
  
  # Read in Data
  df <- read_fwf(file_path,
                 fwf_positions(
                      start = c(31, 33, 67, 69, 142, 338,  
                                341, 346, 351, 356, 361, 366, 371, 376, 381, 386, 
                                391, 396, 401, 406, 411, 416, 421, 426, 431, 436),  
                      end   = c(32, 35, 68, 70, 145, 339,  
                                345, 350, 355, 360, 365, 370, 375, 380, 385, 390, 
                                395, 400, 405, 410, 415, 420, 425, 430, 435, 440),  
                      col_names = c('state', 'fips', 'Age_Recode_52', 'Age_Recode_27', 
                                    'UNDERLYING_CAUSE_OF_DEATH', 'Number_Record_Axis_Conditions',
                                    'Condition_1RA', 'Condition_2RA', 'Condition_3RA', 'Condition_4RA', 'Condition_5RA', 
                                    'Condition_6RA', 'Condition_7RA', 'Condition_8RA', 'Condition_9RA', 'Condition_10RA', 
                                    'Condition_11RA', 'Condition_12RA', 'Condition_13RA', 'Condition_14RA', 'Condition_15RA', 
                                    'Condition_16RA', 'Condition_17RA', 'Condition_18RA', 'Condition_19RA', 'Condition_20RA')
                    ),
                    col_types = cols(.default = col_character()))  # Read all as character to avoid parsing issues

# Assign as ICD-10 chapters
  df <- df %>%
  mutate(
    icd10_chapter = case_when(
      grepl("^A|^B", UNDERLYING_CAUSE_OF_DEATH) ~ "Infectious Diseases",
      grepl("^C|^D[0-4]", UNDERLYING_CAUSE_OF_DEATH) ~ "Neoplasms",
      grepl("^D[5-8]", UNDERLYING_CAUSE_OF_DEATH) ~ "Blood & Immune Diseases",
      grepl("^E", UNDERLYING_CAUSE_OF_DEATH) ~ "Endocrine & Metabolic",
      grepl("^F", UNDERLYING_CAUSE_OF_DEATH) ~ "Mental & Behavioral Disorders",
      grepl("^G", UNDERLYING_CAUSE_OF_DEATH) ~ "Nervous System Diseases",
      grepl("^H[0-5]", UNDERLYING_CAUSE_OF_DEATH) ~ "Eye Diseases",
      grepl("^H[6-9]", UNDERLYING_CAUSE_OF_DEATH) ~ "Ear Diseases",
      grepl("^I", UNDERLYING_CAUSE_OF_DEATH) ~ "Circulatory System Diseases",
      grepl("^J", UNDERLYING_CAUSE_OF_DEATH) ~ "Respiratory System Diseases",
      grepl("^K", UNDERLYING_CAUSE_OF_DEATH) ~ "Digestive System Diseases",
      grepl("^L", UNDERLYING_CAUSE_OF_DEATH) ~ "Skin Diseases",
      grepl("^M", UNDERLYING_CAUSE_OF_DEATH) ~ "Musculoskeletal Diseases",
      grepl("^N", UNDERLYING_CAUSE_OF_DEATH) ~ "Genitourinary Diseases",
      grepl("^O", UNDERLYING_CAUSE_OF_DEATH) ~ "Pregnancy & Childbirth",
      grepl("^P", UNDERLYING_CAUSE_OF_DEATH) ~ "Perinatal Conditions",
      grepl("^Q", UNDERLYING_CAUSE_OF_DEATH) ~ "Congenital Disorders",
      grepl("^R", UNDERLYING_CAUSE_OF_DEATH) ~ "Symptoms & Unspecified Conditions",
      grepl("^S|^T", UNDERLYING_CAUSE_OF_DEATH) ~ "Injury & Poisoning",
      grepl("^V|^W|^X|^Y", UNDERLYING_CAUSE_OF_DEATH) ~ "External Causes",
      grepl("^Z", UNDERLYING_CAUSE_OF_DEATH) ~ "Factors Influencing Health",
      grepl("^U", UNDERLYING_CAUSE_OF_DEATH) ~ "Special Use Codes",
      TRUE ~ "Unknown"
    )
  )


# Combine 'state' and 'fips' into a 5-digit FIPS code
  df <- df %>%
  mutate(fips_n = str_pad(paste0(state, fips), width = 5, side = "left", pad = "0")) %>%  # Create new FIPS column first
  select(fips = fips_n, everything(), -state, -fips)  # Rename and move fips to the front, then remove old columns


# Summarize by county, age group, and ICD-10 chapter
county_age_summary <- df %>%
  group_by(fips, Age_Recode_52, icd10_chapter) %>%
  summarise(death_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = icd10_chapter, values_from = death_count, values_fill = 0) %>%
  mutate(total_death = rowSums(select(., -c(fips, Age_Recode_52))))  # Sum all ICD-10 categories

# Save file
write.csv(county_age_summary, paste0(base_dir, "nvss_cod_", year, ".csv"), row.names = FALSE)

}

# ------------------------------------------------------------------------ #
# 2000 - 2002
# Loop
# Regroup age categories to correspond with ACS age groups
# ------------------------------------------------------------------------ #

base_direct <- "/Users/sy9715/Library/CloudStorage/OneDrive-PrincetonUniversity/Data/NCHS mortality data/"

# Loop through years 2000-2002
for (year in 2000:2002) {
  
  # Load preprocessed mortality data from CSV
  file_path <- paste0(base_direct, "nvss_cod_", year, ".csv")
  df <- read_csv(file_path, col_types = cols())
  
  # Convert Age_Recode_52 to numeric
  df <- df %>%
    mutate(Age_Recode_52 = as.numeric(Age_Recode_52))
  
  # Define ACS age groups
  all_age_groups <- tibble(Age_Group = c("<5", "5-9", "10-14", "15-19", "20-24", 
                                         "25-34", "35-44", "45-54", "55-59", "60-64", 
                                         "65-74", "75-84", "85+", "999"))  # "999" for "Age not stated"
  
  # Adjust Age_Recode_52 categories to ACS groups
  df <- df %>%
    mutate(Age_Group = case_when(
      Age_Recode_52 %in% 1:26 ~ "<5",   # Under 5 years
      Age_Recode_52 == 27 ~ "5-9",      # 5-9 years
      Age_Recode_52 == 28 ~ "10-14",    # 10-14 years
      Age_Recode_52 == 29 ~ "15-19",    # 15-19 years
      Age_Recode_52 == 30 ~ "20-24",    # 20-24 years
      Age_Recode_52 %in% 31:32 ~ "25-34", # 25-34 years
      Age_Recode_52 %in% 33:34 ~ "35-44", # 35-44 years
      Age_Recode_52 %in% 35:36 ~ "45-54", # 45-54 years
      Age_Recode_52 == 37 ~ "55-59",    # 55-59 years
      Age_Recode_52 == 38 ~ "60-64",    # 60-64 years
      Age_Recode_52 %in% 39:40 ~ "65-74", # 65-74 years
      Age_Recode_52 %in% 41:42 ~ "75-84", # 75-84 years
      Age_Recode_52 %in% 43:51 ~ "85+", # 85+ years
      Age_Recode_52 == 52 ~ "999"       # "Age not stated"
    ))
  
  # Create a full set of fips-age group combinations "BEFORE sum"
  expected_combinations <- df %>%
    distinct(fips) %>%
    cross_join(all_age_groups)
  
  # Identify numeric columns for sum
  numeric_cols <- df %>% select(where(is.numeric)) %>% colnames()
  
  # summarize deaths by fips & Age_Group
  df_collapsed <- df %>%
    group_by(fips, Age_Group) %>%
    summarise(across(all_of(numeric_cols), sum, na.rm = TRUE), .groups = "drop")
  
  # merge summarized data with full fips-age set, filling missing values with 0
  df_final <- expected_combinations %>%
    left_join(df_collapsed, by = c("fips", "Age_Group")) %>%
    mutate(across(all_of(numeric_cols), ~ replace_na(.x, 0)))
  
  # Check for missing fips-age combinations (should be 0)
  missing_combinations <- expected_combinations %>%
    anti_join(df_final, by = c("fips", "Age_Group"))
  
  cat("Year:", year, "- Missing fips-age combinations after fixing:", nrow(missing_combinations), "\n")
  
  # Save 
  collapsed_file_path <- paste0(base_direct, "nvss_cod_collapsed_", year, ".csv")
  write_csv(df_final, collapsed_file_path)
}


# ------------------------------------------------------------------------ #
# 2003 - 2022
# Loop
# ------------------------------------------------------------------------ #

base_dir <- ""

# Loop over years 2003 to 2022
for (year in 2003:2022) {
  
  # Construct file path for the current year
  year_folder <- paste0("MULT", year, ".USPSAllCnty")
  file_path <- paste0(base_dir, year_folder, "/MULT", year, ".USAllCnty.txt")
  
  # Read in Data
  df <- read_fwf(file_path,
                 fwf_positions(
                   c(29, 35, 28, 69, 79, 63, 106, 489, 484, 146, 806, 810, 812, 816, 20, 64, 65, 70, 74, 75, 77, 81, 83, 84, 85, 102, 107, 108, 
                     109, 144, 145, 150, 154, 157, 160, 341, 344, 349, 354, 359, 364, 369, 374, 379, 384, 389, 394, 399, 404, 409,
                     414, 419, 424, 429, 434, 439, 448), 
                   c(30, 37, 28, 69, 80, 64, 106, 490, 486, 149, 809, 811, 815, 817, 20, 64, 66, 73, 74, 76, 78, 82, 83, 84, 85, 105, 107, 108, 
                     109, 144, 145, 152, 156, 159, 161, 342, 348, 353, 358, 363, 368, 373, 378, 383, 388, 393, 398, 403, 408, 413,
                     418, 423, 428, 433, 438, 443, 448),
                   c('state','fips','Pop_size_cat', 'Sex','Age_Recode_12','Education_2003','Injury_at_Work', 'RaceRecode_40', 'Hispanic_Origin',
                     'UNDERLYING_CAUSE_OF_DEATH', 'CensusOcc', 'Occ_26', 'CensusInd', 'Ind_23', 'Resident_Status_US', 'Education_flag',
                     'Month_of_Death', 'DetailAge', 'Age_Substitution_Flag', 'Age_Recode_52', 'Age_Recode_27', 'Infant_Age_Recode', 
                     'Place_of_Death_andStatus', 'Marital_Status', 'Day_of_Week_of_Death', 'Current_Data_Year', 'Manner_of_Death', 'Method_of_Disposition',
                     'Autopsy', 'Activity_Code', 'Place_of_Injury', 'Cause_Recode_358', 'Cause_Recode_113', 'Infant_Cause_Recode', 'Cause_Recode_39',
                     'Number_Record_Axis_Conditions', 'Condition_1RA', 'Condition_2RA', 'Condition_3RA', 'Condition_4RA', 'Condition_5RA',
                     'Condition_6RA', 'Condition_7RA', 'Condition_8RA', 'Condition_9RA', 'Condition_10RA', 'Condition_11RA', 'Condition_12RA',
                     'Condition_13RA', 'Condition_14RA', 'Condition_15RA', 'Condition_16RA', 'Condition_17RA', 'Condition_18RA', 'Condition_19RA',
                     'Condition_20RA', 'Race_Imputation_Flag')),
                 col_types = cols(.default = col_character())) 
  
  # Assign ICD-10 chapters
  df <- df %>%
    mutate(
      icd10_chapter = case_when(
        grepl("^A|^B", UNDERLYING_CAUSE_OF_DEATH) ~ "Infectious Diseases",
        grepl("^C|^D[0-4]", UNDERLYING_CAUSE_OF_DEATH) ~ "Neoplasms",
        grepl("^D[5-8]", UNDERLYING_CAUSE_OF_DEATH) ~ "Blood & Immune Diseases",
        grepl("^E", UNDERLYING_CAUSE_OF_DEATH) ~ "Endocrine & Metabolic",
        grepl("^F", UNDERLYING_CAUSE_OF_DEATH) ~ "Mental & Behavioral Disorders",
        grepl("^G", UNDERLYING_CAUSE_OF_DEATH) ~ "Nervous System Diseases",
        grepl("^H[0-5]", UNDERLYING_CAUSE_OF_DEATH) ~ "Eye Diseases",
        grepl("^H[6-9]", UNDERLYING_CAUSE_OF_DEATH) ~ "Ear Diseases",
        grepl("^I", UNDERLYING_CAUSE_OF_DEATH) ~ "Circulatory System Diseases",
        grepl("^J", UNDERLYING_CAUSE_OF_DEATH) ~ "Respiratory System Diseases",
        grepl("^K", UNDERLYING_CAUSE_OF_DEATH) ~ "Digestive System Diseases",
        grepl("^L", UNDERLYING_CAUSE_OF_DEATH) ~ "Skin Diseases",
        grepl("^M", UNDERLYING_CAUSE_OF_DEATH) ~ "Musculoskeletal Diseases",
        grepl("^N", UNDERLYING_CAUSE_OF_DEATH) ~ "Genitourinary Diseases",
        grepl("^O", UNDERLYING_CAUSE_OF_DEATH) ~ "Pregnancy & Childbirth",
        grepl("^P", UNDERLYING_CAUSE_OF_DEATH) ~ "Perinatal Conditions",
        grepl("^Q", UNDERLYING_CAUSE_OF_DEATH) ~ "Congenital Disorders",
        grepl("^R", UNDERLYING_CAUSE_OF_DEATH) ~ "Symptoms & Unspecified Conditions",
        grepl("^S|^T", UNDERLYING_CAUSE_OF_DEATH) ~ "Injury & Poisoning",
        grepl("^V|^W|^X|^Y", UNDERLYING_CAUSE_OF_DEATH) ~ "External Causes",
        grepl("^Z", UNDERLYING_CAUSE_OF_DEATH) ~ "Factors Influencing Health",
        grepl("^U", UNDERLYING_CAUSE_OF_DEATH) ~ "Special Use Codes",
        TRUE ~ "Unknown"
      )
    )
  
  # Create a lookup table for state abbreviations to FIPS codes
  state_fips <- tibble(
    state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", 
              "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", 
              "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
              "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"), # Added DC
    state_fips = c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13", "15", "16", "17", 
                   "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", 
                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
                   "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "11") # DC is "11"
  )
  
  # Add state FIPS to the dataframe by joining with the lookup table
  df_clean <- df %>%
    left_join(state_fips, by = "state")
  
  # Ensure county FIPS codes are formatted correctly (3-digit padded)
  df_clean <- df_clean %>%
    mutate(
      county_fips = sprintf("%03d", as.numeric(fips)), # Ensure 3-digit county FIPS
      fips = paste0(state_fips, county_fips)          # Combine state and county FIPS to create full 5-digit code
    ) %>%
    select(-county_fips) # Remove temporary county column if no longer needed
  
  # Summarize by county, age group, and ICD-10 chapter
  county_age_summary <- df_clean %>%
    group_by(fips, Age_Recode_52, icd10_chapter) %>%
    summarise(death_count = n(), .groups = "drop") %>%
    pivot_wider(names_from = icd10_chapter, values_from = death_count, values_fill = 0) %>%
    mutate(total_death = rowSums(select(., -c(fips, Age_Recode_52))))  # Sum all ICD-10 categories
  
  # Save file
  write.csv(county_age_summary, paste0(base_dir, "nvss_cod_", year, ".csv"), row.names = FALSE)
  cat("Saved:", paste0(base_dir, "nvss_cod_", year, ".csv"), "\n")
  
}

# ------------------------------------------------------------------------ #
# 2003 - 2022
# Loop
# Regroup age categories to correspond with ACS age groups
# ------------------------------------------------------------------------ #

base_direct <- ""

# Loop through years 2003-2022
for (year in 2003:2022) {
  
  file_path <- paste0(base_direct, "nvss_cod_", year, ".csv")
  df <- read_csv(file_path, col_types = cols())
  df <- df %>%
    mutate(Age_Recode_52 = as.numeric(Age_Recode_52))
    all_age_groups <- tibble(Age_Group = c("<5", "5-9", "10-14", "15-19", "20-24", 
                                         "25-34", "35-44", "45-54", "55-59", "60-64", 
                                         "65-74", "75-84", "85+", "999"))  # "999" for "Age not stated"
    df <- df %>%
    mutate(Age_Group = case_when(
      Age_Recode_52 %in% 1:26 ~ "<5",   # Under 5 years
      Age_Recode_52 == 27 ~ "5-9",      # 5-9 years
      Age_Recode_52 == 28 ~ "10-14",    # 10-14 years
      Age_Recode_52 == 29 ~ "15-19",    # 15-19 years
      Age_Recode_52 == 30 ~ "20-24",    # 20-24 years
      Age_Recode_52 %in% 31:32 ~ "25-34", # 25-34 years
      Age_Recode_52 %in% 33:34 ~ "35-44", # 35-44 years
      Age_Recode_52 %in% 35:36 ~ "45-54", # 45-54 years
      Age_Recode_52 == 37 ~ "55-59",    # 55-59 years
      Age_Recode_52 == 38 ~ "60-64",    # 60-64 years
      Age_Recode_52 %in% 39:40 ~ "65-74", # 65-74 years
      Age_Recode_52 %in% 41:42 ~ "75-84", # 75-84 years
      Age_Recode_52 %in% 43:51 ~ "85+", # 85+ years
      Age_Recode_52 == 52 ~ "999"       # "Age not stated"
    ))
  
  # Create a full set of fips-age group combinations "BEFORE sum"
  expected_combinations <- df %>%
    distinct(fips) %>%
    cross_join(all_age_groups)
  
  # Identify numeric columns for sum
  numeric_cols <- df %>% select(where(is.numeric)) %>% colnames()
  
  # sum deaths by fips & Age_Group
  df_collapsed <- df %>%
    group_by(fips, Age_Group) %>%
    summarise(across(all_of(numeric_cols), sum, na.rm = TRUE), .groups = "drop")
  
  # merge summarized data with full fips-age set, filling missing values with 0
  df_final <- expected_combinations %>%
    left_join(df_collapsed, by = c("fips", "Age_Group")) %>%
    mutate(across(all_of(numeric_cols), ~ replace_na(.x, 0)))
  
  # Check for missing fips-age combinations (should be 0)
  missing_combinations <- expected_combinations %>%
    anti_join(df_final, by = c("fips", "Age_Group"))
  
  cat("Year:", year, "- Missing fips-age combinations after fixing:", nrow(missing_combinations), "\n")
  
  #  Save the final collapsed file
  collapsed_file_path <- paste0(base_direct, "nvss_cod_collapsed_", year, ".csv")
  write_csv(df_final, collapsed_file_path)
  
}


# ------------------------------------------------------------------------ #
# 2000 - 2022
# Load Yearly Data & Assign Broader Cause Groups
# ------------------------------------------------------------------------ #

for (year in 2000:2022) {
  
  # Load the yearly ICD-10 grouped file
  file_path <- paste0(base_direct, "nvss_cod_collapsed_", year, ".csv")
  
  if (file.exists(file_path)) {
    
    df_year <- read_csv(file_path, col_types = cols())
    
    # Ensure 'total_death' is retained
    total_death_df <- df_year %>%
      select(fips, Age_Group, total_death)
    
    # Convert to long format for regrouping
    df_year_long <- df_year %>%
      select(-total_death) %>%
      pivot_longer(-c(fips, Age_Group), names_to = "icd10_chapter", values_to = "death_count")
    
    # Assign Broad Cause Groups Correctly
    df_year_long <- df_year_long %>%
      mutate(
        broad_cause_group = case_when(
          icd10_chapter %in% c("Infectious Diseases") ~ "Communicable & Vaccine-Preventable Diseases",
          icd10_chapter %in% c("Neoplasms", "Circulatory System Diseases", "Respiratory System Diseases",
                               "Digestive System Diseases", "Skin Diseases", "Musculoskeletal Diseases",
                               "Genitourinary Diseases", "Endocrine & Metabolic") ~ "Chronic Non-Communicable Diseases",
          icd10_chapter %in% c("Mental & Behavioral Disorders") ~ "Mental Health & Substance Use Disorders",
          icd10_chapter %in% c("Nervous System Diseases") ~ "Neurological & Cognitive Disorders",
          icd10_chapter %in% c("Eye Diseases", "Ear Diseases") ~ "Vision & Hearing Disorders",
          icd10_chapter %in% c("Pregnancy & Childbirth", "Perinatal Conditions", "Congenital Disorders") ~ "Reproductive, Maternal, & Neonatal Health",
          icd10_chapter %in% c("Injury & Poisoning", "External Causes") ~ "Violence, Injuries & External Causes",
          icd10_chapter %in% c("Symptoms & Unspecified Conditions", "Factors Influencing Health") ~ "Unspecified & Healthcare-Related Factors",
          icd10_chapter %in% c("Special Use Codes") ~ "Special Use Codes (COVID-19, Emerging Diseases)",  
          TRUE ~ "MISSING"  
        )
      ) %>%
      filter(!is.na(broad_cause_group)) %>%  
      group_by(fips, Age_Group, broad_cause_group) %>%
      summarise(total_deaths_broad = sum(death_count, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = broad_cause_group, values_from = total_deaths_broad, values_fill = 0)
    
    # Merge broad cause group data with total_death
    df_final <- total_death_df %>%
      left_join(df_year_long, by = c("fips", "Age_Group"))
    
    # Save new grouped file
    write.csv(df_final, paste0(base_direct, "nvss_cod_gp_", year, ".csv"), row.names = FALSE)
    
    cat("Saved Broad Group File:", paste0(base_dir, "nvss_cod_gp_", year, ".csv"), "\n")
  }
}

# ------------------------------------------------------------------------ #
# 2000 - 2022
# Compare Total Deaths from Raw and Processed Datasets
# ------------------------------------------------------------------------ #

base_direct <- ""

# Create an empty dataframe to store results
death_comparison <- tibble(year = integer(), 
                           raw_deaths = integer(), 
                           processed_deaths = integer(), 
                           broad_processed_deaths = integer(), 
                           diff_processed = integer(), 
                           diff_broad = integer())

# Loop over years 2000 to 2022
for (year in 2000:2022) {
  
  # Construct file paths
  raw_file_path <- paste0(base_dir, "MULT", year, ".USPSAllCnty/MULT", year, ".USAllCnty.txt")
  processed_file_path <- paste0(base_dir, "nvss_cod_collapsed_", year, ".csv")       # ICD-10 processed file
  broad_processed_file_path <- paste0(base_dir, "nvss_cod_gp_", year, ".csv") # Broad cause grouped file
  
  # 1️ Count Total Deaths in Raw File (Each Row = 1 Death)
  if (file.exists(raw_file_path)) {
    raw_deaths <- length(readLines(raw_file_path))  # Number of rows in raw file
  } else {
    raw_deaths <- NA  # File missing
  }
  
  # 2 Count Total Deaths in Processed ICD-10 File
  if (file.exists(processed_file_path)) {
    df_processed <- read_csv(processed_file_path, col_types = cols())
    processed_deaths <- sum(df_processed$total_death, na.rm = TRUE)  # Sum total deaths
  } else {
    processed_deaths <- NA  
  }
  
  # 3 Count Total Deaths in Broad Grouped File
  if (file.exists(broad_processed_file_path)) {
    df_broad_processed <- read_csv(broad_processed_file_path, col_types = cols())
    broad_processed_deaths <- sum(df_broad_processed$total_death, na.rm = TRUE)  # Sum total deaths
  } else {
    broad_processed_deaths <- NA  
  }
  
  # 4 Calculate Differences
  diff_processed <- raw_deaths - processed_deaths  # Dif between raw & ICD-10 processed
  diff_broad <- raw_deaths - broad_processed_deaths  # Dif between raw & broad group processed
  
  # Store results
  death_comparison <- death_comparison %>%
    add_row(year = year, 
            raw_deaths = raw_deaths, 
            processed_deaths = processed_deaths, 
            broad_processed_deaths = broad_processed_deaths, 
            diff_processed = diff_processed, 
            diff_broad = diff_broad)
}
print(death_comparison)


# ------------------------------------------------------------------------ #
# Combine all files in to panel structure
# ------------------------------------------------------------------------ #

# ------------------------------------------------------------------------ #
# Combine all files into panel structure with year column
# ------------------------------------------------------------------------ #

base_direct <- ""
file_types <- c("nvss_cod_collapsed", "nvss_cod_gp", "nvss_cod")

# Function to merge files by type with year column
merge_files_by_type <- function(file_prefix) {
  
  # Identify all files that match the pattern
  file_list <- list.files(path = base_direct, pattern = paste0("^", file_prefix, "_\\d{4}\\.csv$"), full.names = TRUE)
  
  # Read and combine all matching files
  merged_data <- file_list %>%
    map_dfr(~ {
      # Extract year from filename
      year <- as.numeric(str_extract(basename(.x), "\\d{4}"))
      
      # Read file and add year column
      read_csv(.x, col_types = cols(.default = col_character())) %>%
        mutate(year = year)  # Add extracted year column
    })
  
  # Save the merged file
  merged_file_path <- paste0(base_direct, file_prefix, "_all.csv")
  write_csv(merged_data, merged_file_path)
  
  cat("Merged and saved:", merged_file_path, "\n")
}

# Run merging for each file type
walk(file_types, merge_files_by_type)



# ------------------------------------------------------------------------ #
# Collapse Age Groups to County-Year Level (_sum)
# ------------------------------------------------------------------------ #

base_direct <- ""

# define file types
file_types <- c("nvss_cod_collapsed", "nvss_cod_gp", "nvss_cod")

# loop through years and process each file type
for (year in 2000:2022) {
  for (file_prefix in file_types) {
    
    file_path <- paste0(base_direct, file_prefix, "_", year, ".csv")
    
    if (file.exists(file_path)) {
      df <- read_csv(file_path, col_types = cols()) %>%
        mutate(year = year)
      
      # Identify numeric columns (excluding fips and year)
      numeric_cols <- df %>%
        select(where(is.numeric), -year) %>%  # Exclude year from being summed
        colnames()
      
      # Collapse data to fips-year level
      df_yearly <- df %>%
        group_by(fips, year) %>%
        summarise(across(all_of(numeric_cols), sum, na.rm = TRUE), .groups = "drop") %>%
        relocate(fips, year, total_death, .before = everything())
      
      # Save the yearly collapsed file
      save_path <- paste0(base_direct, file_prefix, "_", year, "_sum.csv")
      write_csv(df_yearly, save_path)
      
      cat("saved:", save_path, "\n")
    } 
  }
}

# ------------------------------------------------------------------------ #
# Clean the unbalanced fips code 
# Merge with a list of existing fips code file and drop the unmatched.
# From 2000-2022 -- note that fips in 2000-02 do not align with other files
# ------------------------------------------------------------------------ #

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

base_direct <- ""

# Define file types to process
file_types <- c("nvss_cod_collapsed", "nvss_cod_gp", "nvss_cod")

# Load FIPS reference file
fips_ref <- read.delim(paste0(base_direct, "fips2county.tsv"), sep = "\t", colClasses = "character")

# Ensure CountyFIPS codes are properly formatted as 5-digit numbers
fips_ref <- fips_ref %>%
  mutate(CountyFIPS = str_pad(CountyFIPS, width = 5, side = "left", pad = "0"))

# Loop through each year from 2003 to 2022
for (year in 2000:2022) {
  for (file_prefix in file_types) {
    
    # Define input and output file names
    input_file <- paste0(base_direct, file_prefix, "_", year, "_sum.csv")
    output_file <- paste0(base_direct, file_prefix, "_", year, "_sum_FipsAdj.csv")
    
    # Check if the file exists before proceeding
    if (file.exists(input_file)) {
      cat("Processing:", input_file, "\n")
      
      # Read the data
      df <- read.csv(input_file, colClasses = "character") %>%
        mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0"))  # Ensure 5-digit FIPS format
      
      # Merge with FIPS reference and drop unmatched rows
      df_cleaned <- df %>%
        inner_join(fips_ref, by = c("fips" = "CountyFIPS")) %>%  # Keep only matched rows
        select(-StateFIPS, -CountyFIPS_3, -StateName, -StateAbbr, -STATE_COUNTY, -CountyCBSA)  # Drop extra columns
      
      # Save the cleaned file
      write.csv(df_cleaned, output_file, row.names = FALSE)
      cat("Saved:", output_file, "\n")
      
    } 
  }
}

# ------------------------------------------------------------------------ #
# Append all cleaned files into single datasets
# ------------------------------------------------------------------------ #

# Function to append all yearly files into one dataset
append_files_by_type <- function(file_prefix) {
  
  # Identify all cleaned files that match the pattern
  file_list <- list.files(path = base_direct, pattern = paste0("^", file_prefix, "_\\d{4}_sum_FipsAdj\\.csv$"), full.names = TRUE)
  
  # If no files exist, skip
  if (length(file_list) == 0) {
    cat("No files found for:", file_prefix, "\n")
    return(NULL)
  }
  
  # Read and combine all matching files
  merged_data <- file_list %>%
    map_dfr(~ read_csv(.x, col_types = cols(.default = col_character())))  # Ensure character columns
  
  # Save the final merged file
  merged_file_path <- paste0(base_direct, file_prefix, "_all_sum_FipsAdj.csv")
  write_csv(merged_data, merged_file_path)
  
  cat("Merged and saved:", merged_file_path, "\n")
}

# Run merging for each file type
walk(file_types, append_files_by_type)

# Double check whether fips n looks correct

base_direct <- ""

# Double check the total n of county
file_path <- paste0(base_direct, "nvss_cod_collapsed_all_sum_FipsAdj.csv")

# Read the data
df <- read_csv(file_path, col_types = cols(.default = col_character()))

# Count unique FIPS codes
num_unique_fips <- df %>%
  distinct(fips) %>%
  nrow()
cat("Total unique FIPS codes in", file_path, ":", num_unique_fips, "\n")


#####################################################################################################################################################################
# Project: Building COD data
# Align with cause level 2 GBD list
# Start date: 18 Feb, 2025
# Update date: 18 Feb, 2025
# Below refine the grouping of multiple cod to GBD level 2 cause list, with additional reduced groupings.
#####################################################################################################################################################################

library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# ------------------------------------------------------------------------ #
# 2003 - 2022
# Loop
# ------------------------------------------------------------------------ #

base_direct <- "/Users/sy9715/Library/CloudStorage/OneDrive-PrincetonUniversity/Data/NCHS mortality data/"

# Loop over years 2003 to 2022
for (year in 2003:2022) {
  
  # Construct file path for the current year
  year_folder <- paste0("MULT", year, ".USPSAllCnty")
  file_path <- paste0(base_direct, year_folder, "/MULT", year, ".USAllCnty.txt")
  
  # Read in Data
  df <- read_fwf(file_path,
                 fwf_positions(
                   c(29, 35, 28, 69, 79, 63, 106, 489, 484, 146, 806, 810, 812, 816, 20, 64, 65, 70, 74, 75, 77, 81, 83, 84, 85, 102, 107, 108, 
                     109, 144, 145, 150, 154, 157, 160, 341, 344, 349, 354, 359, 364, 369, 374, 379, 384, 389, 394, 399, 404, 409,
                     414, 419, 424, 429, 434, 439, 448), 
                   c(30, 37, 28, 69, 80, 64, 106, 490, 486, 149, 809, 811, 815, 817, 20, 64, 66, 73, 74, 76, 78, 82, 83, 84, 85, 105, 107, 108, 
                     109, 144, 145, 152, 156, 159, 161, 342, 348, 353, 358, 363, 368, 373, 378, 383, 388, 393, 398, 403, 408, 413,
                     418, 423, 428, 433, 438, 443, 448),
                   c('state','fips','Pop_size_cat', 'Sex','Age_Recode_12','Education_2003','Injury_at_Work', 'RaceRecode_40', 'Hispanic_Origin',
                     'UNDERLYING_CAUSE_OF_DEATH', 'CensusOcc', 'Occ_26', 'CensusInd', 'Ind_23', 'Resident_Status_US', 'Education_flag',
                     'Month_of_Death', 'DetailAge', 'Age_Substitution_Flag', 'Age_Recode_52', 'Age_Recode_27', 'Infant_Age_Recode', 
                     'Place_of_Death_andStatus', 'Marital_Status', 'Day_of_Week_of_Death', 'Current_Data_Year', 'Manner_of_Death', 'Method_of_Disposition',
                     'Autopsy', 'Activity_Code', 'Place_of_Injury', 'Cause_Recode_358', 'Cause_Recode_113', 'Infant_Cause_Recode', 'Cause_Recode_39',
                     'Number_Record_Axis_Conditions', 'Condition_1RA', 'Condition_2RA', 'Condition_3RA', 'Condition_4RA', 'Condition_5RA',
                     'Condition_6RA', 'Condition_7RA', 'Condition_8RA', 'Condition_9RA', 'Condition_10RA', 'Condition_11RA', 'Condition_12RA',
                     'Condition_13RA', 'Condition_14RA', 'Condition_15RA', 'Condition_16RA', 'Condition_17RA', 'Condition_18RA', 'Condition_19RA',
                     'Condition_20RA', 'Race_Imputation_Flag')),
                 col_types = cols(.default = col_character())) 
  
  # Assign GBD level 2 cause list
  df <- df %>%
    mutate(
      
      gbd_level2 = case_when(
        
        # 1. Cardiovascular diseases (I00-I99)
        grepl("^I", UNDERLYING_CAUSE_OF_DEATH) ~ "cardio",
        
        # 2. Chronic respiratory diseases (J40-J47)
        grepl("^J4[0-7]", UNDERLYING_CAUSE_OF_DEATH) ~ "chronic_resp",
        
        # 3. Cirrhosis and other chronic liver diseases (K70-K77)
        grepl("^K7[0-7]", UNDERLYING_CAUSE_OF_DEATH) ~ "liver",
        
        # 4. Diabetes and kidney diseases (E10-E14, N00-N39)
        grepl("^E1[0-4]|^N[0-9]", UNDERLYING_CAUSE_OF_DEATH) ~ "diab_kidney",
        
        # 5. Digestive diseases (K20-K93 excluding K70-K77)
        grepl("^K[2-6]|^K[8-9]|^K9[0-3]|^K[0-1][0-9]", UNDERLYING_CAUSE_OF_DEATH) ~ "digest",
        
        # 6. HIV/AIDS and sexually transmitted infections (B20-B24, A50-A64)
        grepl("^B2[0-4]|^A5[0-9]|^A64", UNDERLYING_CAUSE_OF_DEATH) ~ "hiv_sti",
        
        # 7. Infectious diseases (A00-B19, B25-B99 excluding HIV/AIDS and TB)
        grepl("^A|^B[0-1]|^B2[5-9]|^B[3-9]", UNDERLYING_CAUSE_OF_DEATH) ~ "infectious",
        
        # 8. Injuries and external causes (S00-T98, V00-Y99)
        grepl("^S|^T|^V|^W|^X|^Y", UNDERLYING_CAUSE_OF_DEATH) ~ "injuries",
        
        # 9. Maternal disorders (O00-O99)
        grepl("^O", UNDERLYING_CAUSE_OF_DEATH) ~ "maternal",
        
        # 10. Mental disorders (F00-F99 excluding F10-F19)
        grepl("^F[0-9]", UNDERLYING_CAUSE_OF_DEATH) & !grepl("^F1[0-9]", UNDERLYING_CAUSE_OF_DEATH) ~ "mental",
        
        # 11. Musculoskeletal disorders (M00-M99)
        grepl("^M", UNDERLYING_CAUSE_OF_DEATH) ~ "musculo",
        
        # 12. Neoplasms (C00-D48)
        grepl("^C|^D[0-4][0-8]", UNDERLYING_CAUSE_OF_DEATH) ~ "neoplasm",
        
        # 13. Neurological disorders (G00-G99)
        grepl("^G", UNDERLYING_CAUSE_OF_DEATH) ~ "neuro",
        
        # 14. Neonatal disorders (P00-P96)
        grepl("^P", UNDERLYING_CAUSE_OF_DEATH) ~ "neonatal",
        
        # 15. Other non-communicable diseases (R00-R99, D50-D89, D3[0-9])
        grepl("^R|^D[5-9][0-9]|^D3[0-9]", UNDERLYING_CAUSE_OF_DEATH) ~ "other_ncd",
        
        # 16. Respiratory infections and tuberculosis (J00-J22, J60-J99, A15-A19)
        grepl("^J0|^J1|^J2|^J[3-9][0-9]|^J[6-9][0-9]|^A1[5-9]", UNDERLYING_CAUSE_OF_DEATH) ~ "resp_infect_tb",
        
        # 17. Sense organ diseases (H00-H95)
        grepl("^H", UNDERLYING_CAUSE_OF_DEATH) ~ "sense_organ",
        
        # 18. Skin and subcutaneous diseases (L00-L99)
        grepl("^L", UNDERLYING_CAUSE_OF_DEATH) ~ "skin",
        
        # 19. Substance use disorders (F10-F19)
        grepl("^F1[0-9]", UNDERLYING_CAUSE_OF_DEATH) ~ "substance",
        
        # 20. Nutritional deficiencies and endocrine diseases (E15-E89, E-specific cases)
        grepl("^E[1-9][0-9]|^E03[0-9]|^E04[0-9]|^E05[0-9]|^E07[0-9]|^E00[9]", UNDERLYING_CAUSE_OF_DEATH) ~ "nutrition",
        UNDERLYING_CAUSE_OF_DEATH %in% c("E063", "E060", "E061", "E065", "E069") ~ "nutrition",
        
        # 21. Congenital malformations (Q00-Q99)
        grepl("^Q", UNDERLYING_CAUSE_OF_DEATH) ~ "congenital",
        
        # Explicit mappings for D codes
        UNDERLYING_CAUSE_OF_DEATH %in% c("D090", "D099", "D091", "D291", "D190", "D197") ~ "other_ncd",
        
        # Explicit mappings for U codes
        UNDERLYING_CAUSE_OF_DEATH %in% c("^U") ~ "unknown",
        
        # Default for unmatched codes
        TRUE ~ "unknown"
      )
    )
  
  # Create broader groupings
  df_clean <- df %>%
    mutate(
      broad_group = case_when(
        
        # Infectious, communicable &/ vaccine-preventable diseases
        gbd_level2 %in% c("hiv_sti", "infectious", "resp_infect_tb", "other_communicable") ~ "communicable",
        
        # maternal & child health
        gbd_level2 %in% c("maternal", "neonatal", "nutrition") ~ "mater_neo_nutri",
        
        # chronic non-communicable diseases
        gbd_level2 %in% c("cardio", "chronic_resp", "diab_kidney", "digest", "neuro", "musculo", 
                          "skin", "other_ncd", "neoplasm", "sense_organ") ~ "ncds",
        
        # mental health & substance use 
        gbd_level2 %in% c("mental", "substance") ~ "mental_drug",
        
        # road traffic accidents & injuries & external
        gbd_level2 %in% c("injuries") ~ "external_vio",
        
        # Rare, Genetic, or Other Conditions
        gbd_level2 %in% c("congenital", "other_ncd") ~ "rare",
        TRUE ~ "unknown"
      )
    )
  
  # Create another dataframe
  df_clean <- df_clean %>%
    select(state, fips, Age_Recode_52, gbd_level2, broad_group)
    table(df_clean$gbd_level2)
  
  # Create a lookup table for state abbreviations to FIPS codes
  state_fips <- tibble(
    state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", 
              "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", 
              "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
              "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"), # Added DC
    state_fips = c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13", "15", "16", "17", 
                   "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", 
                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
                   "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "11") # Added 11 for DC
  )
 
  # Add state FIPS to the dataframe by joining with the lookup table
  df_clean <- df_clean %>%
    left_join(state_fips, by = "state")
  
 # Rename fips 
  df_clean <- df_clean %>%
    mutate(
      fips = sprintf("%03d", as.numeric(fips)),
      fips_code = paste0(state_fips, fips)      
    )
  df_clean <- df_clean %>%
    mutate(
      fips_county = sprintf("%03d", as.numeric(fips)), 
      fips = paste0(state_fips, fips_county)          
    ) 
  df_clean <- df_clean %>%
    select(-fips_county, -fips_code) 
  
  
  # Group by county (fips) and age group, then summarize deaths for each broad group
  county_age_summary <- df_clean %>%
    group_by(fips, Age_Recode_52, broad_group) %>%
    summarise(
      death_count = n(), 
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = broad_group,  
      values_from = death_count, 
      values_fill = 0            
    ) %>%
    mutate(
      total_death = communicable + mater_neo_nutri + ncds + mental_drug + external_vio + rare + unknown
    )
  
  # Save
  write.csv(county_age_summary, paste0(base_direct, "nvss_cod_broad_", year, ".csv"), row.names = FALSE)
  cat("Saved:", paste0(base_direct, "nvss_cod_broad_", year, ".csv"), "\n")
}


# ------------------------------------------------------------------------ #
# 2003 - 2022
# Loop
# Regroup age categories to correspond with ACS age groups
# ------------------------------------------------------------------------ #

base_direct <- "/Users/sy9715/Library/CloudStorage/OneDrive-PrincetonUniversity/Data/NCHS mortality data/"

# Loop through years 2003-2022
for (year in 2003:2022) {
  
  file_path <- paste0(base_direct, "nvss_cod_broad_", year, ".csv")
  df <- read_csv(file_path, col_types = cols())
  df <- df %>%
    mutate(Age_Recode_52 = as.numeric(Age_Recode_52))
  all_age_groups <- tibble(Age_Group = c("<5", "5-9", "10-14", "15-19", "20-24", 
                                         "25-34", "35-44", "45-54", "55-59", "60-64", 
                                         "65-74", "75-84", "85+", "999"))  # "999" for "Age not stated"
  df <- df %>%
    mutate(Age_Group = case_when(
      Age_Recode_52 %in% 1:26 ~ "<5",   
      Age_Recode_52 == 27 ~ "5-9",     
      Age_Recode_52 == 28 ~ "10-14",    
      Age_Recode_52 == 29 ~ "15-19",   
      Age_Recode_52 == 30 ~ "20-24",   
      Age_Recode_52 %in% 31:32 ~ "25-34", 
      Age_Recode_52 %in% 33:34 ~ "35-44", 
      Age_Recode_52 %in% 35:36 ~ "45-54", 
      Age_Recode_52 == 37 ~ "55-59",    
      Age_Recode_52 == 38 ~ "60-64",    
      Age_Recode_52 %in% 39:40 ~ "65-74", 
      Age_Recode_52 %in% 41:42 ~ "75-84", 
      Age_Recode_52 %in% 43:51 ~ "85+", 
      Age_Recode_52 == 52 ~ "999"       
    ))
  
  # Create a full set of fips-age group combinations "BEFORE sum"
  expected_combinations <- df %>%
    distinct(fips) %>%
    cross_join(all_age_groups)
  
  # Identify numeric columns for sum
  numeric_cols <- df %>% select(where(is.numeric)) %>% colnames()
  
  # sum deaths by fips & Age_Group
  df_collapsed <- df %>%
    group_by(fips, Age_Group) %>%
    summarise(across(all_of(numeric_cols), sum, na.rm = TRUE), .groups = "drop")
  
  # merge summarized data with full fips-age set, filling missing values with 0
  df_final <- expected_combinations %>%
    left_join(df_collapsed, by = c("fips", "Age_Group")) %>%
    mutate(across(all_of(numeric_cols), ~ replace_na(.x, 0)))
  
  # Check for missing fips-age combinations (should be 0)
  missing_combinations <- expected_combinations %>%
    anti_join(df_final, by = c("fips", "Age_Group"))
  
  cat("Year:", year, "- Missing fips-age combinations after fixing:", nrow(missing_combinations), "\n")
  
  #  Save the final collapsed file
  collapsed_file_path <- paste0(base_direct, "nvss_cod_broad_collapsed_", year, ".csv")
  write_csv(df_final, collapsed_file_path)
  
}


# ------------------------------------------------------------------------ #
# Combine all files into panel structure with year column
# ------------------------------------------------------------------------ #

base_direct <- "/Users/sy9715/Library/CloudStorage/OneDrive-PrincetonUniversity/Data/NCHS mortality data/"
file_types <- c("nvss_cod_broad_collapsed")

# Function to merge files by type with year column
merge_files_by_type <- function(file_prefix) {
  
  # Identify all files that match the pattern
  file_list <- list.files(path = base_direct, pattern = paste0("^", file_prefix, "_\\d{4}\\.csv$"), full.names = TRUE)
  
  # Read and combine all matching files
  merged_data <- file_list %>%
    map_dfr(~ {
      # Extract year from filename
      year <- as.numeric(str_extract(basename(.x), "\\d{4}"))
      
      # Read file and add year column
      read_csv(.x, col_types = cols(.default = col_character())) %>%
        mutate(year = year)  # Add extracted year column
    })
  
  # Save the merged file
  merged_file_path <- paste0(base_direct, file_prefix, "_all.csv")
  write_csv(merged_data, merged_file_path)
  
  cat("Merged & saved:", merged_file_path, "\n")
}

# Run merging for each file type
walk(file_types, merge_files_by_type)



# ------------------------------------------------------------------------ #
# Collapse Age Groups to County-Year Level (_sum)
# ------------------------------------------------------------------------ #

base_direct <- "/Users/sy9715/Library/CloudStorage/OneDrive-PrincetonUniversity/Data/NCHS mortality data/"

# define file types
file_types <- c("nvss_cod_broad_collapsed")

# loop through years and process each file type
for (year in 2003:2022) {
  for (file_prefix in file_types) {
    
    file_path <- paste0(base_direct, file_prefix, "_", year, ".csv")
    
    if (file.exists(file_path)) {
      df <- read_csv(file_path, col_types = cols()) %>%
        mutate(year = year)
      
      # Identify numeric columns (excluding fips and year)
      numeric_cols <- df %>%
        select(where(is.numeric), -year) %>%  # Exclude year from being summed
        colnames()
      
      # Collapse data to fips-year level
      df_yearly <- df %>%
        group_by(fips, year) %>%
        summarise(across(all_of(numeric_cols), sum, na.rm = TRUE), .groups = "drop") %>%
        relocate(fips, year, total_death, .before = everything())
      
      # Save the yearly collapsed file
      save_path <- paste0(base_direct, file_prefix, "_", year, "_sum.csv")
      write_csv(df_yearly, save_path)
      
      cat("saved:", save_path, "\n")
    } 
  }
}


# ------------------------------------------------------------------------ #
# Clean the unbalanced fips code 
# Merge with a list of existing fips code file and drop the unmatched.
# From 2000-2022 -- note that fips in 2000-02 do not align with other files
# ------------------------------------------------------------------------ #

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

base_direct <- "/Users/sy9715/Library/CloudStorage/OneDrive-PrincetonUniversity/Data/NCHS mortality data/"

# Define file types to process
file_types <- c("nvss_cod_broad_collapsed")

# Load FIPS reference file
fips_ref <- read.delim(paste0(base_direct, "fips2county.tsv"), sep = "\t", colClasses = "character")

# Ensure CountyFIPS codes are properly formatted as 5-digit numbers
fips_ref <- fips_ref %>%
  mutate(CountyFIPS = str_pad(CountyFIPS, width = 5, side = "left", pad = "0"))

# Loop through each year from 2003 to 2022
for (year in 2003:2022) {
  for (file_prefix in file_types) {
    
    # Define input and output file names
    input_file <- paste0(base_direct, file_prefix, "_", year, "_sum.csv")
    output_file <- paste0(base_direct, file_prefix, "_", year, "_sum_FipsAdj.csv")
    
    # Check if the file exists before proceeding
    if (file.exists(input_file)) {
      cat("Processing:", input_file, "\n")
      
      # Read the data
      df <- read.csv(input_file, colClasses = "character") %>%
        mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0"))  # Ensure 5-digit FIPS format
      
      # Merge with FIPS reference and drop unmatched rows
      df_cleaned <- df %>%
        inner_join(fips_ref, by = c("fips" = "CountyFIPS")) %>%  # Keep only matched rows
        select(-StateFIPS, -CountyFIPS_3, -StateName, -StateAbbr, -STATE_COUNTY, -CountyCBSA)  # Drop extra columns
      
      # Save the cleaned file
      write.csv(df_cleaned, output_file, row.names = FALSE)
      cat("Saved:", output_file, "\n")
      
    } 
  }
}

# ------------------------------------------------------------------------ #
# Append all cleaned files into single datasets
# ------------------------------------------------------------------------ #

file_types <- c("nvss_cod_broad_collapsed")

# Function to append all yearly files into one dataset
append_files_by_type <- function(file_prefix) {
  
  # Identify all cleaned files that match the pattern
  file_list <- list.files(path = base_direct, pattern = paste0("^", file_prefix, "_\\d{4}_sum_FipsAdj\\.csv$"), full.names = TRUE)
  
  # If no files exist, skip
  if (length(file_list) == 0) {
    cat("No files found for:", file_prefix, "\n")
    return(NULL)
  }
  
  # Read and combine all matching files
  merged_data <- file_list %>%
    map_dfr(~ read_csv(.x, col_types = cols(.default = col_character())))  # Ensure character columns
  
  # Save the final merged file
  merged_file_path <- paste0(base_direct, file_prefix, "_all_sum_FipsAdj.csv")
  write_csv(merged_data, merged_file_path)
  
  cat("Merged & saved:", merged_file_path, "\n")
}

# Run merging for each file type
walk(file_types, append_files_by_type)

# Double check whether fips n looks correct

base_direct <- "/Users/sy9715/Library/CloudStorage/OneDrive-PrincetonUniversity/Data/NCHS mortality data/"

# Double check the total n of county
file_path <- paste0(base_direct, "nvss_cod_broad_collapsed_all_sum_FipsAdj.csv")

# Read the data
df <- read_csv(file_path, col_types = cols(.default = col_character()))

# Count unique FIPS codes
num_unique_fips <- df %>%
  distinct(fips) %>%
  nrow()
cat("Total unique FIPS codes in", file_path, ":", num_unique_fips, "\n")






