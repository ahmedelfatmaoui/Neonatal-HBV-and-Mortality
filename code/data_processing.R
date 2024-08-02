# List of packages to install and load
packages <- c("tidyverse", "stringi", "ggplot2", "readxl", "tigris", "fixest", "modelsummary", 
              "AER", "ivreg", "bacondecomp", "lmtest", "multiwayvcov", "plm", "stargazer", 
              "foreign", "zoo", "sandwich", "latex2exp", "haven", "gt", "magrittr", 
              "fastDummies", "rvest", "lubridate", "nycflights13", "xml2", "kableExtra", 
              "Synth", "synthdid", "groupdata2", "bea.R", "httr", "devtools")

# Function to install and load packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

# Apply the function to each package
invisible(sapply(packages, install_and_load))

# Load required libraries
library(tidyverse)
library(stringi)
library(ggplot2)
library(readxl)
library(tigris)
library(fixest)
library(modelsummary)
library(AER)
library(ivreg)
library(bacondecomp)
library(lmtest)
library(multiwayvcov)
library(plm)
library(stargazer)
library(foreign)
library(zoo)
library(sandwich)
library(latex2exp)
library(haven)
library(gt)
library(magrittr)
library(fastDummies)
library(rvest)
library(lubridate)
library(nycflights13)
library(xml2)
library(kableExtra)
library(Synth)
library(synthdid)
library(groupdata2)
library(bea.R)
library(httr)
library(devtools)

# Set working directory to script location
set_working_directory <- function() {
  script_path <- normalizePath(dirname(rstudioapi::getSourceEditorContext()$path), winslash = "\\")
  setwd(script_path)
  print(getwd())
}
set_working_directory()

# Data processing function
process_mortality_data <- function() {
  setwd("data/files")
  years <- 1990:2019
  
  data_decoding <- function(yr) {
    if (yr %in% 1989:2001) {
      zip_file_path <- paste0("MortAC", yr, "/MULT", yr, ".AllCnty.zip")
    } else {
      zip_file_path <- paste0("MortAC", yr, "/MULT", yr, ".USPSAllCnty.zip")
    }
    
    if (yr %in% 1989:1999) {
      txt_file_path <- paste0("MULT", yr, ".AllCnty.txt")
    } else if (yr %in% 2000:2017) {
      txt_file_path <- paste0("MULT", yr, ".USAllCnty.txt")
    } else if (yr == 2018) {
      txt_file_path <- paste0("Mort", yr, "US.AllCnty.txt")
    } else {
      txt_file_path <- paste0("MULT", yr, "US.AllCnty.txt")
    }
    
    unzip(zip_file_path, files = txt_file_path, exdir = ".")
    df <- read_delim(txt_file_path, delim = ",", col_names = FALSE)
    
    if (yr %in% 1989:1995) {
      my_df <- data_frame(
        year = paste0(19, stringr::str_sub(df$X1, 1, 2)),
        occurrence_state = stringr::str_sub(df$X1, 119, 120),
        occurrence_county = stringr::str_sub(df$X1, 121, 123),
        residence_state = stringr::str_sub(df$X1, 124, 125),
        residence_county = stringr::str_sub(df$X1, 126, 128),
        age_infant = stringr::str_sub(df$X1, 73, 74),
        month = substr(df$X1, 55, 56),
        race = substr(df$X1, 62, 62),
        sex = substr(df$X1, 59, 59)
      ) %>%
        filter(
          residence_state == occurrence_state,
          occurrence_county == residence_county
        ) %>%
        select(year, age_infant, state = occurrence_state, county = occurrence_county)
      
      my_df$age_infant <- as.numeric(my_df$age_infant)
      my_df$deaths_count <- 1
      my_df <- my_df %>%
        filter(!is.na(age_infant)) %>%
        group_by(year, state, county, age_infant) %>%
        summarize(deaths_count = sum(deaths_count))
    } else if (yr %in% 1996:2002) {
      my_df <- data_frame(
        year = stringr::str_sub(df$X1, 115, 118),
        occurrence_state = stringr::str_sub(df$X1, 119, 120),
        occurrence_county = stringr::str_sub(df$X1, 121, 123),
        residence_state = stringr::str_sub(df$X1, 124, 125),
        residence_county = stringr::str_sub(df$X1, 126, 128),
        age_infant = stringr::str_sub(df$X1, 73, 74),
        month = substr(df$X1, 55, 56),
        race = substr(df$X1, 62, 62),
        sex = substr(df$X1, 59, 59)
      ) %>%
        filter(
          residence_state == occurrence_state,
          occurrence_county == residence_county
        ) %>%
        select(year, age_infant, state = occurrence_state, county = occurrence_county)
      
      my_df$age_infant <- as.numeric(my_df$age_infant)
      my_df$deaths_count <- 1
      my_df <- my_df %>%
        filter(!is.na(age_infant)) %>%
        group_by(year, state, county, age_infant) %>%
        summarize(deaths_count = sum(deaths_count))
    } else {
      my_df <- data_frame(
        year = stringr::str_sub(df$X1, 102, 105),
        occurrence_state = stringr::str_sub(df$X1, 21, 22),
        occurrence_county = stringr::str_sub(df$X1, 23, 25),
        residence_state = stringr::str_sub(df$X1, 29, 30),
        residence_county = stringr::str_sub(df$X1, 35, 37),
        age_infant = stringr::str_sub(df$X1, 81, 82),
        month = substr(df$X1, 65, 66),
        race = substr(df$X1, 449, 449),
        sex = substr(df$X1, 69, 69)
      ) %>%
        filter(
          residence_state == occurrence_state,
          occurrence_county == residence_county
        ) %>%
        select(year, age_infant, state = occurrence_state, county = occurrence_county)
      
      my_df$age_infant <- as.numeric(my_df$age_infant)
      my_df$deaths_count <- 1
      my_df <- my_df %>%
        filter(!is.na(age_infant)) %>%
        group_by(year, state, county, age_infant) %>%
        summarize(deaths_count = sum(deaths_count))
    }
    
    return(my_df)
  }
  
  df <- data_decoding(1989)
  
  for (year in years) {
    print(year)
    df <- rbind(df, data_decoding(year))
  }
  
  state_data <- fips_codes %>% select(state, state_code) %>% unique()
  
  df$is_numeric <- grepl("^-?\\d+(\\.\\d+)?$", df$state)
  
  df1 <- df %>%
    filter(is_numeric) %>%
    rename(state_code = state) %>%
    left_join(state_data)
  
  df2 <- df %>%
    filter(!is_numeric) %>%
    left_join(state_data)
  
  data <- rbind(df1, df2) %>%
    select(-is_numeric)
  
  data$year <- as.numeric(data$year)
  data$fips <- paste0(data$state_code, data$county)
  
  write_csv(data, "~/Desktop/vax_infant_mortality/death_raw.csv")
}

# Uncomment to process data for the first time
# process_mortality_data()

# Load processed data
data <- read_csv("~/Desktop/vax_infant_mortality/death_raw.csv")

# Data aggregation and transformations
data_all <- data %>%
  select(year, month, state_code, state, county, fips, deaths_count) %>%
  group_by(year, month, state_code, state, county, fips) %>%
  summarize(deaths_count = sum(deaths_count))

# Create spread datasets for race, sex, and age
data_spread_race <- data %>%
  select(year, month, state_code, state, county, fips, race, deaths_count) %>%
  group_by(year, month, state_code, state, county, fips, race) %>%
  summarize(deaths_count = sum(deaths_count)) %>%
  pivot_wider(names_from = race, values_from = deaths_count)

data_spread_sex <- data %>%
  select(year, month, state_code, state, county, fips, sex, deaths_count) %>%
  group_by(year, month, state_code, state, county, fips, sex) %>%
  summarize(deaths_count = sum(deaths_count)) %>%
  pivot_wider(names_from = sex, values_from = deaths_count)

data_spread_age <- data %>%
  select(year, month, state_code, state, county, fips, age_infant, deaths_count) %>%
  group_by(year, month, state_code, state, county, fips, age_infant) %>%
  summarize(deaths_count = sum(deaths_count)) %>%
  pivot_wider(names_from = age_infant, values_from = deaths_count)

# Combine all spread datasets
data_combined <- data_all %>%
  left_join(data_spread_race) %>%
  left_join(data_spread_sex) %>%
  left_join(data_spread_age)

# BEA API function to get population data
get_bea_population <- function() {
  readRenviron("~/.Renviron")
  BEA_KEY <- Sys.getenv("BEA_KEY")
  
  population <- list(
    "UserID" = BEA_KEY,
    "Method" = "GetData",
    "datasetname" = "Regional",
    "TableName" = "CAINC1",
    "LineCode" = 2,
    "GeoFips" = "COUNTY",
    "Year" = paste(1989:2020, collapse = ",")
  )
  
  population_df0 <- beaGet(population, asWide = FALSE)
  
  population_df <- population_df0 %>%
    select(fips = GeoFips, year = TimePeriod, DataValue, GeoName) %>%
    mutate(GeoName = gsub(",.*$", "", GeoName)) %>%
    rename(population = DataValue)
  
  population_df$population <- as.numeric(population_df$population)
  population_df$year <- as.numeric(population_df$year)
  
  return(population_df)
}

# Uncomment to get population data
# population_df <- get_bea_population()
# write_csv(population_df, "~/Desktop/vax_infant_mortality/bea_pop")

population_df <- read_csv("~/Desktop/vax_infant_mortality/bea_pop")

# Combine vaccination and mortality data
combine_vax_mort_data <- function() {
  df1 <- read_csv("~/Desktop/vax_infant_mortality/state_vax.csv") %>%
    select(state_name, state_code, age, 
           year = `Birth Year/Birth Cohort`,
           vax_est = vax_coverage_percent,
           Dose, vax_type = Vaccine)
  
  df1$age <- case_when(
    df1$age == "0-1 Days" ~ 0,
    df1$age == "0-2 Days" ~ 1,
    df1$age == "0-3 Days" ~ 2,
    TRUE ~ as.numeric(parse_number(df1$age))
  )
  
  mort1 <- read_csv("~/Desktop/vax_infant_mortality/death_raw.csv") %>%
    select(state_code, state, fips, age = age_infant, year, deaths_count) %>%
    left_join(population_df) %>%
    select(-GeoName)
  
  mort1$mortality_rate <- (mort1$deaths_count / mort1$population) * 100000
  
  # Age recoding
  mort1$age <- case_when(
    mort1$age %in% c("deaths_age_3", "deaths_age_2", "deaths_age_1") ~ 0,
    mort1$age %in% c("deaths_age_4", "deaths_age_3", "deaths_age_2", "deaths_age_1") ~ 1,
    mort1$age %in% c("deaths_age_5", "deaths_age_4", "deaths_age_3", "deaths_age_2", "deaths_age_1") ~ 2,
    mort1$age == "deaths_age_14" ~ 3,
    mort1$age == "deaths_age_15" ~ 4,
    mort1$age == "deaths_age_16" ~ 5,
    mort1$age == "deaths_age_17" ~ 6,
    mort1$age == "deaths_age_18" ~ 7,
    mort1$age == "deaths_age_19" ~ 8,
    mort1$age == "deaths_age_20" ~ 9,
    mort1$age == "deaths_age_21" ~ 10,
    mort1$age == "deaths_age_22" ~ 11,
    TRUE ~ as.numeric(parse_number(mort1$age))
  )
  
  mort1$state_code <- as.numeric(mort1$state_code)
  
  mort11 <- mort1 %>%
    select(-c(mortality_rate, state, population)) %>%
    filter(age %in% c(0:11)) %>%
    group_by(year, state_code, fips, age) %>%
    summarize(deaths_count = sum(deaths_count, na.rm = TRUE)) %>%
    left_join(population_df) %>%
    select(-GeoName)
  
  mort11$mortality_rate <- (mort11$deaths_count / mort11$population) * 100000
  
  hr_data <- read_csv("~/Desktop/vax_infant_mortality/hr_controls.csv")
  hr_data$state_code <- as.numeric(hr_data$state_code)
  hr_data$fips <- as.character(hr_data$fips)
  
  mort_vax_df <- df1 %>%
    inner_join(as_tibble(mort11), multiple = "all") %>%
    left_join(hr_data)
  
  write_csv(mort_vax_df, "~/Desktop/vax_infant_mortality/hepb_mortality2.csv")
}

combine_vax_mort_data()

# Create data for older kids (placebo test)
create_data_for_older_kids <- function() {
  df_placebo <- read_csv("~/Desktop/vax_infant_mortality/Vaccination_Coverage_among_Young_Children_13_35_Months.csv") %>%
    filter(
      !(grepl("City", Geography) | grepl("County", Geography) |
          grepl("Region", Geography) | grepl("Rest", Geography)),
      `Dimension Type` == "Age",
      `Birth Year/Birth Cohort` %in% as.character(2000:2023)
    ) %>%
    rename(
      state_name = Geography,
      age = Dimension,
      vax_coverage_percent = `Estimate (%)`
    )
  
  data(fips_codes)
  fips_codes$state_name <- tolower(fips_codes$state_name)
  fips_codes1 <- fips_codes %>% select(state_code, state_name) %>% unique()
  
  df_placebo1 <- df_placebo %>%
    mutate(state_name = tolower(state_name)) %>%
    left_join(fips_codes1) %>%
    filter(!is.na(state_name), state_code < 57) %>%
    mutate(
      Vaccine = as.character(Vaccine),
      Dose = as.character(Dose),
      age = as.character(age),
      vax_coverage_percent = as.numeric(vax_coverage_percent),
      state_code = as.numeric(state_code)
    )
  
  df_placebo2 <- df_placebo1 %>%
    select(state_code, age, year = `Birth Year/Birth Cohort`, vax_coverage_percent, Vaccine) %>%
    group_by(year, state_code, Vaccine, age) %>%
    summarize(vax_coverage_percent = mean(vax_coverage_percent, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      age = as.numeric(parse_number(age)),
      year = as.numeric(year)
    )
  
  hr_data <- read_csv("~/Desktop/vax_infant_mortality/hr_controls.csv")
  hr_data$state_code <- as.numeric(hr_data$state_code)
  hr_data$fips <- as.character(hr_data$fips)
  
  df_placebo3 <- df_placebo2 %>% left_join(hr_data, multiple = "all")
  
  write_csv(df_placebo1, "~/Desktop/vax_infant_mortality/vax_placebo.csv")
}

create_data_for_older_kids()

# Create adjacent county IV
create_adjacent_iv <- function() {
  df <- read_csv("~/Desktop/vax_infant_mortality/hepb_mortality2.csv") %>%
    select(year, fips_base = fips, age, Dose, vax_type)
  df$fips_base <- as.numeric(df$fips_base)
  
  df_adj <- read_csv("~/Desktop/vax_infant_mortality/county_adjacency_NBER.csv") %>%
    filter(fipsneighbor != fipscounty) %>%
    select(-c(FIPS, Year, adjacent_cnty_obesity)) %>%
    unique() %>%
    rename(fips_base = fipscounty) %>%
    filter(fips_base %in% unique(df$fips_base))
  
  df2 <- df %>%
    left_join(df_adj, multiple = "all") %>%
    unique() %>%
    rename(dose = Dose)
  
  df_vax <- read_csv("~/Desktop/vax_infant_mortality/hepb_mortality2.csv") %>%
    select(fipsneighbor = fips, year, age, dose = Dose, vax_type, vax_est_adj = vax_est)
  df_vax$fipsneighbor <- as.numeric(df_vax$fipsneighbor)
  
  df3 <- df2 %>%
    left_join(df_vax) %>%
    unique() %>%
    select(-c(fipsneighbor, countyname, neighborname)) %>%
    group_by(year, fips_base, age, dose, vax_type) %>%
    summarize(vax_est_adj = mean(vax_est_adj, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(fips = fips_base)
  
  df <- read_csv("~/Desktop/vax_infant_mortality/hepb_mortality2.csv")
  names(df) <- names(df) %>%
    gsub("\\-", " ", .) %>%
    gsub("\\%", "percent", .) %>%
    gsub(" ", "_", .) %>%
    tolower()
  
  # Normalize certain variables by population
  df <- df %>%
    mutate(
      uninsured_adults_raw_value = uninsured_adults_raw_value / population * 1000,
      sexually_transmitted_infections_raw_value = sexually_transmitted_infections_raw_value / population * 1000,
      teen_births_raw_value = teen_births_raw_value / population * 1000,
      preventable_hospital_stays_raw_value = preventable_hospital_stays_raw_value / population * 1000,
      violent_crime_raw_value = violent_crime_raw_value / population * 1000,
      hiv_prevalence_raw_value = hiv_prevalence_raw_value / population * 1000,
      median_household_income_raw_value = log(median_household_income_raw_value),
      age_fe = case_when(
        age < 3 ~ 1,
        age > 2 & age < 6 ~ 2,
        age > 5 ~ 3
      ),
      policy_dummy = ifelse(year > 2017, 1, 0),
      fips = as.numeric(fips)
    )
  
  df4 <- df3 %>% left_join(df)
  
  ggplot(df4, aes(vax_est, vax_est_adj)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_light()
  
  write_csv(df4, "~/Desktop/vax_infant_mortality/hepb_mortality3.csv")
}

create_adjacent_iv()

# Additional analyses and visualizations

# Plot mortality rate by year
data1 %>% 
  group_by(year) %>% 
  summarize(mortality_rate = mean(mortality_rate, na.rm = TRUE)) %>%
  ggplot(aes(year, mortality_rate)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Plot male deaths by year
data1 %>% 
  group_by(year) %>% 
  summarize(Male_deaths = mean(Male_deaths, na.rm = TRUE)) %>%
  ggplot(aes(year, Male_deaths)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Plot mortality rate by sex
df_plot1 %>%
  ggplot(aes(year, mortality_rate, color = sex)) +
  geom_smooth() +
  theme_bw()

# Plot mortality rate by race
df_plot1 %>%
  ggplot(aes(year, mortality_rate, color = race)) +
  geom_smooth() +
  theme_bw()

# Plot mortality rate by age
df_plot1 %>%
  ggplot(aes(year, mortality_rate, color = age_infant)) +
  geom_smooth() +
  theme_bw()

# Initial plots function
initial_plots <- function(data) {
  p1 <- ggplot(data, aes(x = year, y = mortality_rate)) +
    geom_point() +
    theme_bw()
  
  p2 <- ggplot(data) +
    geom_histogram(aes(mortality_rate)) +
    theme_bw()
  
  p3 <- ggplot(data, aes(x = year, y = mortality_rate)) +
    geom_point() +
    facet_wrap(~state) +
    theme_bw()
  
  p4 <- ggplot(data, aes(x = year, y = mortality_rate)) +
    geom_point() +
    facet_wrap(~state) +
    theme_bw()
  
  return(list(p1, p2, p3, p4))
}

plots1 <- initial_plots(data1)
plots1[[1]]
plots1[[2]]
plots1[[3]]
plots1[[4]]

# Data summary
data1 %>% modelsummary::datasummary_skim()

# Plot deaths by age
df11 <- data1 %>% 
  select(year, mortality_rate, population, fips, starts_with("deaths")) %>%
  pivot_longer(cols = starts_with("deaths_age"), names_to = "age", values_to = "deaths")

df111 <- df11 %>% 
  group_by(age) %>%
  summarize(deaths_count = sum(deaths, na.rm = TRUE)) 

df111$age <- df111$age %>% parse_number()

ggplot(df111, aes(x = age, y = deaths_count)) +
  geom_line() +
  theme_bw()

# Regression models
df <- read_csv("~/Desktop/vax_infant_mortality/hepb_mortality.csv")

model1 <- feols(mortality_rate ~ vax_est | state_code + age, 
                data = df %>% filter(age %in% c(0, 2, 1, 3, 4, 5, 6)))
model01 <- summary(model1, cluster = c('state_code'))

model2 <- feols(mortality_rate ~ vax_est | state_code + age + year, 
                data = df %>% filter(age %in% c(0, 2, 1, 3, 4, 5, 6)))
model02 <- summary(model2, cluster = c('state_code'))

model3 <- feols(mortality_rate ~ vax_est | age * year + state_code, 
                data = df %>% filter(age %in% c(0, 2, 1, 3, 4, 5, 6)))
model03 <- summary(model3, cluster = c('state_code'))

# Model summary
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N Obs.",             0,
  "adj.r.squared", paste0("Adjusted ", "R2"), 3,
  "r.squared", paste0("R2"), 3,
)

models <- list(
  "(1)" = model01,
  "(2)" = model02,
  "(3)" = model03
)

rows <- tribble(
  ~"Coefficients", ~"Model 1",  ~"Model 2", ~"Model 3",
  "State FE", "Y", "Y", "Y",
  "Vaccination age FE",   "Y", "Y", "Y",
  "Year FE",    "N", "Y", "Y",
  "Age*Year FE",   "N", "N", "Y",
)

var_names <- c("vax_est" = "Vaccination rate")

modelsummary(
  models,
  title = 'Dependent variable: mortality rate',
  stars = c('*' = .05, '**' = .01, '***' = .001),
  coef_map = var_names,
  gof_map = gm,
  add_rows = rows
)

# Plot vaccination rate by age and vaccine type
ggplot(
  mort_vax_df %>% 
    group_by(age, vax_type) %>% 
    summarize(vax_est = mean(vax_est, na.rm = TRUE)),
  aes(x = age, y = vax_est)
) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, max(mort_vax_df$age), by = 1)) +
  facet_wrap(. ~ vax_type, ncol = 2)

# Plot mortality rate and vaccination rate by age
mort_vax1 <- mort_vax_df %>% 
  select(age, mortality_rate, vax_est) %>%
  group_by(age) %>%
  summarize(
    mortality_rate = mean(mortality_rate, na.rm = TRUE),
    vax_est = mean(vax_est, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c("mortality_rate", "vax_est"), names_to = "type", values_to = "values")

ggplot(mort_vax1, aes(x = age, y = values, color = type)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, max(mort_vax_df$age), by = 1))

# Plot mortality rate by age
ggplot(
  mort22 %>%
    group_by(age) %>%
    summarize(mortality_rate = mean(mortality_rate, na.rm = TRUE)),
  aes(x = age, y = mortality_rate)
) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, max(mort22$age), by = 1))
