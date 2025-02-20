#######################################################
# Project: Building COD data - Multiple Mortality rates
# Start date: 12 Feb, 2025
# Update date: 12 Feb, 2025
#######################################################

# ------------------------------------------------------------------------------------------------------------ #
# This script processes U.S. county-level mortality data (2000–2022) to calculate Multiple Mortality Rate (MMR).
# It extracts cause-of-death conditions, groups them by ICD-10 chapters, and computes MMR at the county-year level.
# Data from 2000–2002 and 2003–2022 use different coding systems, so adjustments are made to ensure consistency.
# ------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------ #
# Saved Output Files:
# ------------------------------------------------------------------------------------------------------------ #
# 1 MMR_YYYY.csv
#   - County-level Multiple Mortality Rate (MMR) for each year (YYYY = 2000-2022).
#   - Contains total deaths, deaths with multiple distinct ICD-10 chapters, and computed MMR.
#   - Source: Individual-level mortality records aggregated to the county-year level.

# 2 MMR_Mapped_YYYY.csv
#   - Version of MMR_YYYY.csv with FIPS codes corrected for 2003–2022 data using state-county mappings.
#   - Ensures consistent 5-digit county FIPS codes across all years.
# ------------------------------------------------------------------------------------------------------------ #
# Methodology:
# ------------------------------------------------------------------------------------------------------------ #
# - Extract underlying cause of death and up to 20 additional conditions.
# - Convert ICD-10 codes into chapter-level classifications.
# - Identify deaths with multiple causes spanning different ICD-10 chapters.
# - Aggregate MMR at the county-year level for further analysis and mapping.
# ------------------------------------------------------------------------------------------------------------ #

library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)

# ------------------------------------------------------------------------ #
# 2000-2002
# These years have different coding than 2002+
# ------------------------------------------------------------------------ #

# Define base directory
base_direct <- ""

# Define file path for 2000
for (year in 2000:2002) {
  
year_folder <- paste0("MULT", year, ".USPSAllCnty")
file_path <- paste0(base_direct, year_folder, "/MULT", year, ".USAllCnty.txt")

# Read in
df <- read_fwf(file_path,
               fwf_positions(
                 start = c(21, 23, 67, 69, 142, 
                           341, 346, 351, 356, 361, 366, 371, 376, 381, 386, 
                           391, 396, 401, 406, 411, 416, 421, 426, 431, 436),  
                 end   = c(22, 25, 68, 70, 145,  
                           345, 350, 355, 360, 365, 370, 375, 380, 385, 390, 
                           395, 400, 405, 410, 415, 420, 425, 430, 435, 440),  
                 col_names = c('state', 'fips', 'Age_Recode_52', 'Age_Recode_27', 
                               'UNDERLYING_CAUSE_OF_DEATH',
                               paste0("Condition_", 1:20, "RA"))
               ),
               col_types = cols(.default = col_character()))

# Create full 5-digit FIPS code and assign a unique identifier
df <- df %>%
  mutate(
    fips_n = str_pad(paste0(state, fips), width = 5, side = "left", pad = "0"),
    id = row_number()  # Unique ID for each death record
  ) %>%
  select(fips_n, id, everything(), -state, -fips)

# Define condition columns
condition_cols <- paste0("Condition_", 1:20, "RA")

# Extract the first letter (ICD-10 Chapter) from each Condition variable in a vectorized way
df[condition_cols] <- lapply(df[condition_cols], function(x) substr(x, 1, 1))

# Convert to long format with unique ID per death record
df_long <- df %>%
  pivot_longer(cols = all_of(condition_cols), names_to = "Condition", values_to = "Chapter") %>%
  filter(!is.na(Chapter))  # Remove missing causes

# Count unique chapters per record using `id`
df_multiple <- df_long %>%
  group_by(fips_n, id) %>%  
  summarise(unique_chapters = n_distinct(Chapter), .groups = "drop") %>%
  mutate(
    multiple_cause = unique_chapters > 1,   # 2 or more unique causes
    multiple_cause_3plus = unique_chapters >= 3  # 3 or more unique causes
  )

# Summarize by county-year
df_summary <- df_multiple %>%
  group_by(fips_n) %>%
  summarise(
    total_death = n(),
    multiple_cause_death = sum(multiple_cause, na.rm = TRUE),
    MMR = multiple_cause_death / total_death,
    multiple_cause_death_3plus = sum(multiple_cause_3plus, na.rm = TRUE),
    MMR_3plus = multiple_cause_death_3plus / total_death,  # MMR for 3+ causes
    .groups = "drop"
  ) %>%
  mutate(year = year) %>%
  relocate(fips_n, year, total_death, multiple_cause_death, MMR, multiple_cause_death_3plus, MMR_3plus)

df_summary <- df_summary %>%
  rename(fips = fips_n)

# Save file
save_path <- paste0(base_dir, "MMR_", year, ".csv")
write_csv(df_summary, save_path)

cat("Saved:", save_path, "\n")
}

# ------------------------------------------------------------------------ #
# 2003-2022
# ------------------------------------------------------------------------ #

base_direct <- ""

# Define state FIPS mapping
state_fips <- tibble(
  state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", 
            "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", 
            "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
            "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"), 
  state_fips = c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13", "15", "16", "17", 
                 "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", 
                 "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
                 "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "11") 
)

# Process MMR for 2003-2022
for (year in 2003:2022) {
  
  cat("Processing Year:", year, "\n")
  
  # Define file path
  year_folder <- paste0("MULT", year, ".USPSAllCnty")
  file_path <- paste0(base_direct, year_folder, "/MULT", year, ".USAllCnty.txt")
  
  if (!file.exists(file_path)) {
    cat("File missing for year:", year, "\n")
    next
  }
  
  # Read in Data
  df <- read_fwf(file_path,
                 fwf_positions(
                   c(21, 35, 28, 69, 79, 63, 106, 489, 484, 146, 806, 810, 812, 816, 20, 64, 65, 70, 74, 75, 77, 81, 83, 84, 85, 102, 107, 108, 
                     109, 144, 145, 150, 154, 157, 160, 341, 344, 349, 354, 359, 364, 369, 374, 379, 384, 389, 394, 399, 404, 409,
                     414, 419, 424, 429, 434, 439, 448), 
                   c(22, 37, 28, 69, 80, 64, 106, 490, 486, 149, 809, 811, 815, 817, 20, 64, 66, 73, 74, 76, 78, 82, 83, 84, 85, 105, 107, 108, 
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
  
  # Ensure correct FIPS code
  df <- df %>%
    left_join(state_fips, by = "state") %>%
    mutate(
      county_fips = sprintf("%03d", as.numeric(fips)),
      fips_n = paste0(state_fips, county_fips)
    ) %>%
    select(-county_fips, -state, -fips) 
  
  # Assign a unique identifier
  df <- df %>%
    mutate(id = row_number()) %>%
    relocate(fips_n, id, everything())
  
  # Extract ICD-10 Chapters from multiple causes
  condition_cols <- paste0("Condition_", 1:20, "RA")
  
  df[condition_cols] <- lapply(df[condition_cols], function(x) substr(x, 1, 1))
  
  df_long <- df %>%
    pivot_longer(cols = all_of(condition_cols), names_to = "Condition", values_to = "Chapter") %>%
    filter(!is.na(Chapter))
  
  # Count unique chapters per record using `id`
  df_multiple <- df_long %>%
    group_by(fips_n, id) %>%
    summarise(unique_chapters = n_distinct(Chapter), .groups = "drop") %>%
    mutate(
      multiple_cause = unique_chapters > 1,  # 2 or more unique causes
      multiple_cause_3plus = unique_chapters >= 3  # 3 or more unique causes
    )
  
  # Summarize MMR at county-year level
  df_summary <- df_multiple %>%
    group_by(fips_n) %>%
    summarise(
      total_death = n(),
      multiple_cause_death = sum(multiple_cause, na.rm = TRUE),
      MMR = multiple_cause_death / total_death,
      multiple_cause_death_3plus = sum(multiple_cause_3plus, na.rm = TRUE),
      MMR_3plus = multiple_cause_death_3plus / total_death,
      .groups = "drop"
    ) %>%
    mutate(year = year) %>%
    relocate(fips_n, year, total_death, multiple_cause_death, MMR, multiple_cause_death_3plus, MMR_3plus)
  
  df_summary <- df_summary %>%
    rename(fips = fips_n)
  
  # Save file
  save_path <- paste0(base_direct, "MMR_", year, ".csv")
  write_csv(df_summary, save_path)
  
  cat("Saved:", save_path, "\n")
}


# ------------------------------------------------------------------------ #
# Plot 2010 by counties
# ------------------------------------------------------------------------ #

base_direct <- ""
file_path <- paste0(base_direct, "MMR_2010.csv")

# Load 2010 MMR data
df <- read_csv(file_path, col_types = cols(fips = col_character()))

# Get county shapefile and filter mainland (exclude AK, HI, and territories)
counties <- counties(cb = TRUE, year = 2019, class = "sf", resolution = "20m") %>%
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) %>%
  rename(fips = GEOID)  # Rename GEOID to match MMR data

# Check if FIPS codes match correctly
df <- df %>%
  mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0"))  # Ensure 5-digit FIPS

# Merge MMR data with county shapefile
counties <- counties %>%
  left_join(df, by = "fips")

# Plot - 2010
map2010 <- ggplot(data = counties) +
  geom_sf(aes(fill = MMR), color = "gray30", size = 0.002) +
  scale_fill_gradientn(colors = c("aliceblue", "lightblue", "magenta4"),
                       limits = c(0.4, 1), 
                       breaks = c(0.4,0.6, 0.8, 1),
                       oob = scales::squish,
                       na.value = "gray90") +
  labs(title = "Multiple Causes of Death Rate (MMR) by County - 2010",
       fill = "MMR 2/more Causes per ID") +
  theme_void() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "bottom")

map2010_2 <- ggplot(data = counties) +
  geom_sf(aes(fill = MMR_3plus), color = "gray30", size = 0.002) +
  scale_fill_gradientn(colors = c("aliceblue", "red", "magenta4"),
                       limits = c(0.4, 1), 
                       breaks = c(0.4,0.6, 0.8, 1),
                       oob = scales::squish,
                       na.value = "gray90") +
  labs(title = "Multiple Causes of Death  Rate (MMR) by County - 2010",
       fill = "MMR 3/more Causes per ID ") +
  theme_void() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "bottom")

map2010 + map2010_2

# ------------------------------------------------------------------------ #
# Plot 2020 by counties
# ------------------------------------------------------------------------ #

base_direct <- ""
file_path <- paste0(base_direct, "MMR_2020.csv")

# Load 2020 MMR data
df <- read_csv(file_path, col_types = cols(fips = col_character()))

# Get county shapefile and filter mainland (exclude AK, HI, and territories)
counties <- counties(cb = TRUE, year = 2019, class = "sf", resolution = "20m") %>%
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) %>%
  rename(fips = GEOID)  # Rename GEOID to match MMR data

# Check if FIPS codes match correctly
df <- df %>%
  mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0"))  # Ensure 5-digit FIPS

# Merge MMR data with county shapefile
counties <- counties %>%
  left_join(df, by = "fips")

# Plot - 2020
map2020 <- ggplot(data = counties) +
  geom_sf(aes(fill = MMR), color = "gray30", size = 0.002) +
  scale_fill_gradientn(colors = c("aliceblue", "lightblue", "magenta4"),
                       limits = c(0.4, 1), 
                       breaks = c(0.4,0.6, 0.8, 1),
                       oob = scales::squish,
                       na.value = "gray90") +
  labs(title = "Multiple Causes of Death Rate (MMR) by County - 2010",
       fill = "MMR 2/more Causes per ID") +
  theme_void() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "bottom")

map2020_2 <- ggplot(data = counties) +
  geom_sf(aes(fill = MMR_3plus), color = "gray30", size = 0.002) +
  scale_fill_gradientn(colors = c("aliceblue", "red", "magenta4"),
                       limits = c(0.4, 1), 
                       breaks = c(0.4,0.6, 0.8, 1),
                       oob = scales::squish,
                       na.value = "gray90") +
  labs(title = "Multiple Causes of Death  Rate (MMR) by County - 2010",
       fill = "MMR 3/more Causes per ID ") +
  theme_void() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "bottom")

map2020 + map2020_2




