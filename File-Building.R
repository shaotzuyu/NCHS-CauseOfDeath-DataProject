######################################################
# Project: Multiple-CoD in Dynamic Networks
# Start date: 5 Feb, 2025
# Update date: 11 Feb, 2025
#######################################################

library(igraph)
library(dplyr)
library(tidyr)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(maps)

# ---------------------------------------------------------------------------------------- #
# Steps & logics
# ---------------------------------------------------------------------------------------- #

# Testing whether a network approach could help map "syndemic mortality"
# i.e., what are the common network topologies of multiple causes of death?

# 1 cleaning the raw NVSS 2019 file and collapse down difference causes for intepretation

# 2 construct a bipartite network (individuals <-> causes)
# 3 project and collapse bipartite graph into a cause-by-cause network 
#      edge weights reflect how often two causes appear together in death record

# 4 compute descriptive network stats: k, betweenness, closeness, and clustering
# 5 detect clusters of related causes using louvain community detection

# 7 plot the cause network with weighted edges
# 8 examine spatial patterns in cause clusters

# ---------------------------------------------------------------------------------------- #
# Read in/clean CoD data
# ---------------------------------------------------------------------------------------- #

# Load county-level causes of death
setwd("")

# Read in NVSS Data
start <- Sys.time() 
df <- read_fwf("", #Add file path in parenthesis
               fwf_positions(c(21, 35, 28, 69, 79, 63, 106, 489, 484, 146, 806, 810, 812, 816, 20, 64, 65, 70, 74, 75, 77, 81, 83, 84, 85, 102, 107, 108, 
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

# Refine ICD-10 mappings
df <- df %>%
  mutate(
    cause_level2_icd10 = case_when(
      
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
      UNDERLYING_CAUSE_OF_DEATH %in% c("U014", "U070") ~ "unknown",
      
      # Default for unmatched codes
      TRUE ~ "unknown"
    )
  )


# a function to categorise/collapse-down ICD-10 code
categorize_icd10 <- function(icd_code) {
  case_when(
    grepl("^I", icd_code) ~ "cardio",
    grepl("^J4[0-7]", icd_code) ~ "chronic_resp",
    grepl("^K7[0-7]", icd_code) ~ "liver",
    grepl("^E1[0-4]|^N[0-9]", icd_code) ~ "diab_kidney",
    grepl("^K[2-6]|^K[8-9]|^K9[0-3]|^K[0-1][0-9]", icd_code) ~ "digest",
    grepl("^B2[0-4]|^A5[0-9]|^A64", icd_code) ~ "hiv_sti",
    grepl("^A|^B[0-1]|^B2[5-9]|^B[3-9]", icd_code) ~ "infectious",
    grepl("^S|^T|^V|^W|^X|^Y", icd_code) ~ "injuries",
    grepl("^O", icd_code) ~ "maternal",
    grepl("^F[0-9]", icd_code) & !grepl("^F1[0-9]", icd_code) ~ "mental",
    grepl("^M", icd_code) ~ "musculo",
    grepl("^C|^D[0-4][0-8]", icd_code) ~ "neoplasm",
    grepl("^G", icd_code) ~ "neuro",
    grepl("^P", icd_code) ~ "neonatal",
    grepl("^R|^D[5-9][0-9]|^D3[0-9]", icd_code) ~ "other_ncd",
    grepl("^J0|^J1|^J2|^J[3-9][0-9]|^J[6-9][0-9]|^A1[5-9]", icd_code) ~ "resp_infect_tb",
    grepl("^H", icd_code) ~ "sense_organ",
    grepl("^L", icd_code) ~ "skin",
    grepl("^F1[0-9]", icd_code) ~ "substance",
    grepl("^E[1-9][0-9]|^E03[0-9]|^E04[0-9]|^E05[0-9]|^E07[0-9]|^E00[9]", icd_code) ~ "nutrition",
    icd_code %in% c("E063", "E060", "E061", "E065", "E069") ~ "nutrition",
    grepl("^Q", icd_code) ~ "congenital",
    icd_code %in% c("D090", "D099", "D091", "D291", "D190", "D197") ~ "other_ncd",
    icd_code %in% c("U014", "U070") ~ "unknown"  )
}

# apply the function to all 20 causes of death (conditions) columns
df <- df %>%
  mutate(across(starts_with("Condition_"), ~ categorize_icd10(.), .names = "cause_{.col}"))

# create a new df with selected variables
df_selected <- df %>%
  select(
    state, fips, Pop_size_cat, Sex, Age_Recode_12, Education_2003, 
    UNDERLYING_CAUSE_OF_DEATH, Number_Record_Axis_Conditions,
    cause_Condition_1RA, cause_Condition_2RA, cause_Condition_3RA, cause_Condition_4RA, 
    cause_Condition_5RA, cause_Condition_6RA, cause_Condition_7RA, cause_Condition_8RA, 
    cause_Condition_9RA, cause_Condition_10RA, cause_Condition_11RA, cause_Condition_12RA, 
    cause_Condition_13RA, cause_Condition_14RA, cause_Condition_15RA, cause_Condition_16RA, 
    cause_Condition_17RA, cause_Condition_18RA, cause_Condition_19RA, cause_Condition_20RA
  )
head(df_selected)


# Create a lookup table for state abbreviations to FIPS codes (for graph)
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

# add state FIPS to the dataframe by joining with the lookup table
df_selected <- df_selected %>%
  left_join(state_fips, by = "state")

# Rename fips 
df_selected <- df_selected %>%
  mutate(
    fips = sprintf("%03d", as.numeric(fips)), # Pad county FIPS to 3 digits
    fips_code = paste0(state_fips, fips)      # Combine state and county FIPS
  )
df_selected <- df_selected %>%
  mutate(
    fips_county = sprintf("%03d", as.numeric(fips)), # Pad original county FIPS to 3 digits
    fips = paste0(state_fips, fips_county)          # Combine state and county FIPS into new fips
  ) 
df_selected <- df_selected %>%
  select(-fips_county, -fips_code) # Optional: Remove fips_county if no longer needed

# Create unique ID and clear
df_selected$death_id <- seq_len(nrow(df_selected))
df_selected <- df_selected %>%
  select(state, fips, death_id, everything())

# Save
setwd("")
write.csv(df_selected, "nvss_Mcod_2019.csv", row.names = FALSE)


# ---------------------------------------------------------------------------------------- #
# Constructing a network object
# ---------------------------------------------------------------------------------------- #

setwd("/Users/sy9715/Library/CloudStorage/OneDrive-PrincetonUniversity/Projects/Multiple-CoD-Networks/Source")
mcod_2019 <- read.csv("nvss_Mcod_2019.csv")

# reshape Data to Long Format (Individual-Cause Pairs) and Remove "unknown"
bipartite_edgelist <- mcod_2019 %>%
  select(death_id, cause_Condition_1RA:cause_Condition_20RA) %>%
  pivot_longer(cols = starts_with("cause_"), names_to = "condition", values_to = "cause") %>%
  filter(!is.na(cause) & cause != "unknown") %>%  
  select(death_id, cause) 

# create a Bipartite Graph (Individuals <-> Causes)
g_bipartite <- graph_from_data_frame(bipartite_edgelist, directed = FALSE)
V(g_bipartite)$type <- bipartite_mapping(g_bipartite)$type  # Assign node types

# Project to a Cause-by-Cause network
bipartite_matrix <- as_biadjacency_matrix(g_bipartite)
cause_overlap_matrix <- tcrossprod(t(bipartite_matrix))  
diag(cause_overlap_matrix) <- 0  # Remove self-loops

# Create cause Network Graph
cause_network <- graph_from_adjacency_matrix(cause_overlap_matrix, mode = "undirected", weighted = TRUE)

# extract cause Network edgelist
cause_edge_list <- as_data_frame(cause_network, what = "edges") %>%
  rename(from = from, to = to, weight = weight)  

# Visualize the Network
plot(cause_network)


# ---------------------------------------------------------------------------------------- #
# Correlation in network and geographic spaces - working
# ---------------------------------------------------------------------------------------- #

# Convert to tidygraph object
cause_tidygraph <- as_tbl_graph(cause_network)

# Plot with ggraph
ggraph(cause_tidygraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = weight, edge_width = weight), color = "gray70") +
  geom_node_point(aes(size = degree(cause_network)), color = "skyblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void()

# Construct measures
degree_centrality <- degree(cause_network)
betweenness_centrality <- betweenness(cause_network)
closeness_centrality <- closeness(cause_network)
clustering_coefficient <- transitivity(cause_network, type = "local")
clusters <- cluster_louvain(cause_network)

# Plot
plot(clusters, cause_network)

# Assign cluster labels to each cause
cluster_mapping <- data.frame(
  cause = V(cause_network)$name,  
  cluster = clusters$membership   # Cluster assignment
)

# Merge cluster labels back to the dataset
df_clustered <- df_selected %>%
  pivot_longer(cols = starts_with("cause_"), names_to = "condition", values_to = "cause") %>%
  left_join(cluster_mapping, by = "cause") %>%
  filter(!is.na(cluster)) %>%  
  select(death_id, state, fips, cluster) 

# Count deaths per cluster in each state
state_cluster_summary <- df_clustered %>%
  group_by(state, cluster) %>%
  summarise(death_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = cluster, values_from = death_count, values_fill = 0)



