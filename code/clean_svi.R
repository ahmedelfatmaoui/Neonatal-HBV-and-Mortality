# Set the directory to file location
setwd(normalizePath(dirname(rstudioapi::getSourceEditorContext()$path), winslash = "\\"))

# Load required libraries
library(tidyverse)
library(dplyr)
library(magrittr)

# Set working directory to the data folder
setwd("../data/svi-raw")

# Get list of CSV files in the directory
csv_files <- list.files(pattern = "*.csv")

# Function to read CSV and add year
read_csv_with_year <- function(file) {
  # Extract year from filename
  year <- parse_number(file) %>%  as.numeric()
  
  # Read CSV
  df <- read_csv(file)
  
  # Add year column
  df$year <- year
  
  return(df)
}

# Read all CSV files and store in a list
data_list <- lapply(csv_files, read_csv_with_year)

# Identify common variables across all dataframes
common_vars <- Reduce(intersect, lapply(data_list, names))

# Keep only common variables in each dataframe
data_list_common <- lapply(data_list, function(df) df[, common_vars])

# Combine all dataframes
combined_data <- bind_rows(data_list_common)

# View the first few rows of the combined data
head(combined_data)
combined_data %>% names

# Assuming your combined dataset is named 'combined_data'
combined_data <- combined_data %>%
  rename(
    # Population estimates
    population_estimate = E_TOTPOP,
    population_estimate_moe = M_TOTPOP,
    
    # Housing units estimates
    housing_units_estimate = E_HU,
    housing_units_estimate_moe = M_HU,
    
    # Unemployment estimates (Civilian, age 16+)
    unemployed_estimate = E_UNEMP,
    unemployed_estimate_moe = M_UNEMP,
    
    # Limited English proficiency estimates (age 5+)
    limited_english_estimate = E_LIMENG,
    limited_english_estimate_moe = M_LIMENG,
    
    # Multi-unit housing estimates (10+ units)
    multi_unit_housing_estimate = E_MUNIT,
    multi_unit_housing_estimate_moe = M_MUNIT,
    
    # Mobile homes estimates
    mobile_homes_estimate = E_MOBILE,
    mobile_homes_estimate_moe = M_MOBILE,
    
    # Crowded housing estimates (more people than rooms)
    crowded_housing_estimate = E_CROWD,
    crowded_housing_estimate_moe = M_CROWD,
    
    # No vehicle households estimates
    no_vehicle_households_estimate = E_NOVEH,
    no_vehicle_households_estimate_moe = M_NOVEH
  ) %>%  select(-dplyr::ends_with("moe")) %>% 
  select(-c("ST","STATE" ,"LOCATION"  )) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  dplyr::relocate(year,.after = "FIPS")



# Load necessary libraries
library(dplyr)
library(tidyr)
library(zoo)

# Generate a complete sequence of years for each FIPS code
complete_years <- combined_data %>%
  select(FIPS) %>%
  distinct() %>%
  crossing(year = 2010:2020)

# Merge with the original data
expanded_data <- complete_years %>%
  left_join(combined_data, by = c("FIPS", "year"))

# Interpolate missing values
interpolated_data <- expanded_data %>%
  group_by(FIPS) %>%
  arrange(year) %>%
  mutate(across(where(is.numeric), ~ na.approx(.x, na.rm = FALSE))) %>%
  ungroup()

# View the first few rows of the interpolated data
head(interpolated_data)
names(interpolated_data) %<>% tolower()
names(interpolated_data)[3:10] <- paste0("svi_",names(interpolated_data)[3:10])
#names(interpolated_data)[3] <- "population_estimate_svi"

# convert to shares of population
# interpolated_data <- interpolated_data %>% 
#   mutate(across(starts_with("svi_"), ~ .x / population_estimate_svi))

# take log 
interpolated_data <- interpolated_data %>% mutate(across(starts_with("svi_"), ~ log(.x + 1e-3)))

write_csv(interpolated_data, "../cdc_svi_data.csv")

df <- read_csv( "../hepb_mortality3.csv") %>% left_join(interpolated_data)

modelsummary::datasummary_skim(df %>% select(starts_with("svi_")), fmt = "%.4f")

df$policy_dummy[df$year %in% 2017] <- 1
write_csv(df, "../hepb_mortality_svi.csv")

# correct the policy dummy
df %>% select(year, policy_dummy) %>% distinct() %>% arrange(year) %>% view
df <- df %>% filter(!year%in%2017)
write_csv(df, "../hepb_mortality_svi2.csv")

