#######################################################
# Project: Building COD data
# Align with cause level 2 GBD list
# Start date: 18 Feb, 2025
# Update date: 21 Feb, 2025
#######################################################

library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# ------------------------------------------------------------------------ #
# 2003 - 2022
# Loop
# ------------------------------------------------------------------------ #

base_direct <- ""

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

base_direct <- ""

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

base_direct <- ""
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
# ------------------------------------------------------------------------ #

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

base_direct <- ""

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

base_direct <- ""

# Double check the total n of county
file_path <- paste0(base_direct, "nvss_cod_broad_collapsed_all_sum_FipsAdj.csv")

# Read the data
df <- read_csv(file_path, col_types = cols(.default = col_character()))

# Count unique FIPS codes
num_unique_fips <- df %>%
  distinct(fips) %>%
  nrow()
cat("Total unique FIPS codes in", file_path, ":", num_unique_fips, "\n")





