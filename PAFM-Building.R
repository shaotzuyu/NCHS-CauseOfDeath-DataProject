#######################################################
# Project: Pass Away from home project
# Start date: 13 Feb, 2025
# Update date: 13 Feb, 2025
#######################################################

library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
  
# ------------------------------------------------------------------------ #
# 2003 - 2022
# Loop
# ------------------------------------------------------------------------ #

base_dir <- ""
save_dir <- ""

# define path for all year
for (year in 2003:2022) {
  
year_folder <- paste0("MULT", year, ".USPSAllCnty")
file_path <- paste0(base_dir, year_folder, "/MULT", year, ".USAllCnty.txt")

# Read in the fixed-width file
df <- read_fwf(file_path,
               fwf_positions(
                 start = c(21, 23, 29, 35, 75, 69, 84, 
                           146, 341,
                           344, 349, 354, 359, 364, 
                           369, 374, 379, 384, 389, 
                           394, 399, 404, 409, 414, 
                           419, 424, 429, 434, 439),  
                 end   = c(22, 25, 30, 37, 76, 69, 84, 
                           149, 342,
                           348, 353, 358, 363, 368, 
                           373, 378, 383, 388, 393, 
                           398, 403, 408, 413, 418, 
                           423, 428, 433, 438, 443),  
                 col_names = c('state_oc', 'fips_oc', 'state_res', 'fips_res', 'Age_Recode_52', 'sex', 'marital',
                               'underlying_cause', 'total_conditions',
                               'Condition_1RA', 'Condition_2RA', 'Condition_3RA', 'Condition_4RA', 'Condition_5RA', 
                               'Condition_6RA', 'Condition_7RA', 'Condition_8RA', 'Condition_9RA', 'Condition_10RA', 
                               'Condition_11RA', 'Condition_12RA', 'Condition_13RA', 'Condition_14RA', 'Condition_15RA', 
                               'Condition_16RA', 'Condition_17RA', 'Condition_18RA', 'Condition_19RA', 'Condition_20RA')
               ),
               col_types = cols(.default = col_character()))

# assign ICD-10 chapters for underlying cause
df <- df %>%
  mutate(
    icd10_chapter = case_when(
      grepl("^A|^B", underlying_cause) ~ "Infectious Diseases",
      grepl("^C|^D[0-4]", underlying_cause) ~ "Neoplasms",
      grepl("^D[5-8]", underlying_cause) ~ "Blood & Immune Diseases",
      grepl("^E", underlying_cause) ~ "Endocrine & Metabolic",
      grepl("^F", underlying_cause) ~ "Mental & Behavioral Disorders",
      grepl("^G", underlying_cause) ~ "Nervous System Diseases",
      grepl("^H[0-9]", underlying_cause) ~ "Eye & Ear Diseases",
      grepl("^I", underlying_cause) ~ "Circulatory System Diseases",
      grepl("^J", underlying_cause) ~ "Respiratory System Diseases",
      grepl("^K", underlying_cause) ~ "Digestive System Diseases",
      grepl("^L", underlying_cause) ~ "Skin Diseases",
      grepl("^M", underlying_cause) ~ "Musculoskeletal Diseases",
      grepl("^N", underlying_cause) ~ "Genitourinary Diseases",
      grepl("^O", underlying_cause) ~ "Pregnancy & Childbirth",
      grepl("^P", underlying_cause) ~ "Perinatal Conditions",
      grepl("^Q", underlying_cause) ~ "Congenital Disorders",
      grepl("^R", underlying_cause) ~ "Symptoms & Unspecified Conditions",
      grepl("^S|^T", underlying_cause) ~ "Injury & Poisoning",
      grepl("^V|^W|^X|^Y", underlying_cause) ~ "External Causes",
      grepl("^Z", underlying_cause) ~ "Factors Influencing Health",
      grepl("^U", underlying_cause) ~ "Special Use Codes",
      TRUE ~ "Unknown"
    )
  )

# create a lookup table for state abbreviations to FIPS codes
state_fips <- tibble(
  state_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", 
                 "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", 
                 "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
                 "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"), 
  state_fips = c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13", "15", "16", "17", 
                 "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", 
                 "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
                 "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "11") # DC is "11"
)

# convert state abbreviations to FIPS codes for occurrence and residence states
df <- df %>%
  left_join(state_fips, by = c("state_oc" = "state_abbr")) %>%
  rename(state_oc_fips = state_fips) %>%
  left_join(state_fips, by = c("state_res" = "state_abbr")) %>%
  rename(state_res_fips = state_fips) %>%
  select(-state_oc, -state_res)  # Remove old state abbreviations

# make sure county fips codes are correctly formatted (3-digit added)
df_clean <- df %>%
  mutate(
    fips_oc = sprintf("%03d", as.numeric(fips_oc)),  
    fips_res = sprintf("%03d", as.numeric(fips_res)),  
    fips_oc_full = paste0(state_oc_fips, fips_oc),  
    fips_res_full = paste0(state_res_fips, fips_res)  
  ) %>%
  select(-fips_oc, -fips_res) 

df_clean <- df_clean %>%
  relocate(state_oc_fips, state_res_fips, fips_oc_full, fips_res_full, 
           sex, marital, total_conditions, underlying_cause, .before = everything())

# count the number of mistached locations
df_mismatch_county <- df_clean %>%
  filter(fips_oc_full != fips_res_full)
df_mismatch_state <- df_clean %>%
  filter(state_oc_fips != state_res_fips)

# add mismatch indicators
df_clean <- df_clean %>%
  mutate(
    mismatch_fips = ifelse(fips_oc_full != fips_res_full, 1, 0),
    mismatch_state = ifelse(state_oc_fips != state_res_fips, 1, 0)
  ) %>%
  mutate(year = year) %>% 
  relocate(state_oc_fips, state_res_fips, fips_oc_full, fips_res_full, 
           year, sex, marital, total_conditions, underlying_cause, mismatch_fips, mismatch_state, .before = everything())

# Save yearly file
save_path <- paste0(save_dir, "nvss_cod_mismatch_", year, ".csv")
write.csv(df_clean, save_path, row.names = FALSE)
cat("Saved:", save_path, "\n")

}




