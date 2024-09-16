# sets the directory to file location
setwd(normalizePath(dirname(rstudioapi::getSourceEditorContext()$path),winslash = "\\"))

# Define the list of required packages
pkg <- c("tidyverse", "stringr", "ggplot2", "readxl", "tigris", "fixest", "modelsummary", 
         "lmtest", "multiwayvcov", "plm", "foreign", "gt", "magrittr", "rvest", 
         "writexl", "dplyr", "kableExtra","officer","flextable")

# Install required packages if not already installed
for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}


library(tidyverse)
library(stringr)
library(ggplot2)
library(readxl)
library(tigris)
library(fixest)
library(modelsummary)
library(lmtest) 
library(multiwayvcov) 
library(plm)
library(foreign)
library(gt)
library(magrittr) 
library(rvest)
library(writexl)
library(dplyr)
library(kableExtra)
library(officer)
library(flextable)


## change the following directories to where you saved the files
data_direct <- "../data/hepb_mortality_svi2.csv" 
census_data <- "../data/census_regions.xls"

setwd("../tables-svi-policy-pval") ## the outputs are saved here


#=================================
### summary stat by age groups====
#=================================

## census regions
## source: "https://www2.census.gov/programs-surveys/popest/geographies/2016/state-geocodes-v2016.xls"

census_reg <- read_xls(census_data,skip=5) %>% 
  select(
    Region,
    state_code=`State (FIPS)`
  ) %>% 
  filter(!state_code%in%c("00")) %>% 
  mutate(state_code=as.numeric(state_code))

# df <- read_csv(data_direct)  %>% left_join(census_reg)
# write_csv(census_reg,"~/Desktop/vax_infant_mortality/vax_heb_iv_code/census_reg.csv")
# write_csv(df,"~/Desktop/vax_infant_mortality/vax_heb_iv_code/hepb_mortality33.csv")

# 
# df <- read_csv(data_direct) %>% 
#   filter(vax_type=="Hep B"&age<3) %>% left_join(census_reg)


#--------------
# vaccination
# table 2
#--------------

# Helper function to create models
create_models <- function(data, dept_var) {
  model1 <- feols(.[dept_var] ~ .[endo], data = data)
  model2 <- feols(.[dept_var] ~ .[c(endo,exog1)] | fips + age_fe, data = data)
  model3 <- feols(.[dept_var] ~ .[c(endo, exog2)] | fips + age_fe, data = data)
  
  list(
    summary(model1, cluster = c('fips')),
    summary(model2, cluster = c('fips')),
    summary(model3, cluster = c('fips'))
  )
}

# Main function
print_reg2 <- function(latex_file_name1, title1, data_direct) {
  
  # Define variable names for output
  var_names <<- c(
    "vax_est:policy_dummy" = "HBV Vaccination Rate*Policy Dummy",
    "fit_vax_est:policy_dummy" = "Fitted HBV Vaccination Rate*Policy Dummy",
    "vax_est" = "HBV Vaccination Rate",
    "fit_vax_est" = "Fitted HBV Vaccination Rate",
    "policy_dummy" = "Policy Dummy",
    
     "svi_housing_units_estimate" = "Log Number of Housing Units",
     "svi_multi_unit_housing_estimate" = "Log Number of Multi-Unit Housing",
     "svi_mobile_homes_estimate" = "Log Number of Mobile Homes",
     "svi_crowded_housing_estimate" = "Log Number of Crowded Housing",
     "svi_no_vehicle_households_estimate" = "Log Number of No Vehicle Households",
     "svi_limited_english_estimate" = "Log Number of Limited English Speakers",
    
    "percent_non_hispanic_african_american_raw_value" = "African American (%)",
    "percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native (%)",
    "percent_hispanic_raw_value" = "Hispanic American (%)",
    "percent_asian_raw_value" = "Asian American (%)",
    "median_household_income_raw_value" = "Median Household Income (Logged)",
    "children_in_poverty_raw_value" = "Children in Poverty (%)",
    "percent_rural_raw_value" = "Rural Population (%)",
    "high_school_graduation_raw_value" = "High School Graduation",
    "some_college_raw_value" = "Some College Edu (%)",
    "adult_smoking_raw_value" = "Adult Smoking (%)",
    "unemployment_raw_value" = "Unemployment Rate",
    "low_birthweight_raw_value" = "Low Birthweight (%)",
    "percent_females_raw_value" = "Female (%)",
    "hiv_prevalence_raw_value" = "HIV Prevalence",
    "sexually_transmitted_infections_raw_value" = "Per Capita Sexually Transmitted Infections"
  )
  
  
  # Define exogenous and endogenous variables
  exog1 <<- c(
    "svi_housing_units_estimate",
    "svi_multi_unit_housing_estimate",
    "svi_mobile_homes_estimate",
    "svi_crowded_housing_estimate",
    "svi_no_vehicle_households_estimate",
    "svi_limited_english_estimate")
    
  exog2 <<- c(
    exog1,

    "percent_non_hispanic_african_american_raw_value",
    "percent_american_indian_and_alaskan_native_raw_value",
    "percent_hispanic_raw_value",
    "percent_asian_raw_value","time"
    # "median_household_income_raw_value",
    # "children_in_poverty_raw_value",
    # "percent_rural_raw_value",
    # "high_school_graduation_raw_value",
    # "some_college_raw_value",
    # "adult_smoking_raw_value",
    # "unemployment_raw_value",
    # "low_birthweight_raw_value",
    # "percent_females_raw_value",
    # "hiv_prevalence_raw_value",
    # "sexually_transmitted_infections_raw_value"
  )
  
  endo <<- c("policy_dummy")
  
  # Load and filter data
 df <-   read_csv(data_direct) %>%  filter(vax_type == "Hep B" & age < 3)
 df$time <- df$year - 2010
 
  # Create datasets for different age groups
  df1 <- df %>% dplyr::rename(vax_le4d = vax_est)
  df2 <- df %>% filter(age < 2) %>% dplyr::rename(vax_le3d = vax_est)
  df3 <- df %>% filter(age < 1) %>% dplyr::rename(vax_le2d = vax_est)
  
  # Create models
  models_age3 <- create_models(df1, "vax_le4d")
  models_age2 <- create_models(df2, "vax_le3d")
  models_age1 <- create_models(df3, "vax_le2d")
  
  models <- c(models_age3, models_age2, models_age1)
  
  # Create and print table
  gm <- tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "N Obs.", 0,
    "adj.r.squared", "Adjusted R Squared", 3
  )
  
  rows <- tribble(
    ~"Coefficients", ~"Model 1", ~"Model 2", ~"Model 3", ~"Model 4",
    ~"Model 5", ~"Model 6", ~"Model 7", ~"Model 8", ~"Model 9",
    "State FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y",
    "Age FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y",
    "Time Trend", "N", "N", "Y", "N", "N", "Y", "N", "N", "Y"
  )
  
  
  # Your existing code to create the table
  table <- modelsummary(
    models,
    title = title1,
    estimate  = "{estimate} [{p.value}]",
    coef_map = var_names,
    gof_map = gm,
    add_rows = rows,
    output = "flextable"
  ) 
  
  # Adjust the table structure and formatting
  table1 <- table %>%
    add_header_row(
      values = c("", "Age: 0-3 Days Old", "Age: 0-2 Days Old", "Age: 0-1 Days Old"),
      colwidths = c(1, 3, 3, 3)
    ) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 6, part = "all") %>%
    align(align = "center", part = "header") %>%
    align(align = "left", part = "body") %>%
    padding(padding = .1, part = "all") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black")) %>% 
    hline(i = 2, part = "header", border = fp_border(color = "black")) %>%
    hline_bottom(part = "body", border = fp_border(color = "black")) %>% 
    hline(i = 1, j = 2:4, part = "header", border = fp_border(color = "black")) %>%
    hline(i = 1, j = 5:7, part = "header", border = fp_border(color = "black")) %>%
    hline(i = 1, j = 8:10, part = "header", border = fp_border(color = "black")) 
 
   # Set specific widths for columns
  num_columns <- ncol(table1$body$dataset)
  table1 <- table1 %>%
            width(j = 1, width = 2.3) %>% # First column width
            width(j = 2:num_columns, width = .77) # Width for numeric columns
  
  # Add footnote
  footnote_text <- "Note: The table presents the Ordinary Least Squares (OLS) results from equation (1). In this equation, the dependent variable is the vaccination rate, while the independent variable of interest is the 2018 HBV Policy Dummy (referred to as Policy Dummy in the table). The policy is the CDC's changed recommendation of the first-dose HBV vaccination to neonates within 24 hours of birth on January 12, 2018. The policy dummy takes a binary value one, if the year is after 2017 and zero otherwise. The racial and ethnic variables indicate the percentages of the population belonging to specific racial or ethnic groups in a county. The median household income and unemployment rate variables are included to account for county-level economies. The proportion of children in poverty is calculated based on the federal poverty threshold of households in a county. Since parents' education level may influence the neonatal outcome variable, we incorporate two additional factors into our model. These factors are the percentage of parents who have a high school degree and the percentage of parents who have some college education. To control health behavior and gender, the model includes percentages of adult smokers and females in a county. To control for health-related factors that may impact the outcome, the model also includes the percentage of low birthweight, HIV prevalence, and per capita sexually transmitted infections. Further details about these variables are available from the data source County Health Rankings. Age 0-2 days include 0-1 and 0-2 days. Age 0-3 days include 0-1, 0-2, and 0-3 days. Standard errors are reported in parentheses and P-values in brackets."
  
  table1 <- table1 %>% add_footer_lines(footnote_text) %>% fontsize(size = 5, part = "footer")

  # If you still want to print the table in the console
  print(table1)
  
  # table1 <- table1 %>% set_table_properties(layout = "autofit") # Set total width
  
  # to save in landscape mode
  save_as_docx(table1, path = paste0(title1,".docx")
     ,pr_section = prop_section(
     page_size = page_size(orient = "landscape"))
    )
  

}

# Usage
latex_file_name1 <- "vaccination.csv"
title1 <- "Table 2: Policy effect on vaccination (time trend robustness check)"
print_reg2(latex_file_name1, title1, data_direct)


#--------------
# mortality
# table 4
#--------------

print_reg4 <- function(latex_file_name1, title1, data_direct) {
  exog1 <<- c(
    'svi_housing_units_estimate',
    'svi_multi_unit_housing_estimate',
    'svi_mobile_homes_estimate',
    'svi_crowded_housing_estimate',
    'svi_no_vehicle_households_estimate',
    'svi_limited_english_estimate')
  
  exog2 <<- c(
    exog1,
    "percent_non_hispanic_african_american_raw_value",
    "percent_american_indian_and_alaskan_native_raw_value",
    "percent_hispanic_raw_value",
    "percent_asian_raw_value","time"
    # "low_birthweight_raw_value",
    # "children_in_poverty_raw_value",
    # "percent_females_raw_value",
    # "adult_smoking_raw_value",
    # "median_household_income_raw_value",
    # "some_college_raw_value",
    # "unemployment_raw_value",
    # "sexually_transmitted_infections_raw_value",
    # "hiv_prevalence_raw_value",
    # "high_school_graduation_raw_value",
    # "percent_rural_raw_value"
  )
  
  endo <<- c("vax_est*policy_dummy")
  
  df = read_csv(data_direct) %>% 
    filter(vax_type == "Hep B") %>% filter(age < 3)
  df$time <- df$year - 2010
  
  # calculate the 75th percentile
  q3 = quantile(df$vax_est, 0.75)[[1]]
  df$vax_est <- ifelse(df$vax_est < q3, 1, 0)
  
  # Create datasets for different age groups
  df1 <- df %>% dplyr::rename(mort_le4d = mortality_rate)
  df2 <- df %>% filter(age < 2) %>% dplyr::rename(mort_le3d = mortality_rate)
  df3 <- df %>% filter(age < 1) %>% dplyr::rename(mort_le2d = mortality_rate)
  
  # Create models
  create_models <- function(data, dept_var) {
    model1 <- feols(.[dept_var] ~ .[endo], data = data)
    model2 <- feols(.[dept_var] ~ .[c(endo, exog1)] | fips + age_fe, data = data)
    model3 <- feols(.[dept_var] ~ .[c(endo, exog2)] | fips + age_fe, data = data)
    
    list(
      summary(model1, cluster = c('fips')),
      summary(model2, cluster = c('fips')),
      summary(model3, cluster = c('fips'))
    )
  }
  
  models_age3 <- create_models(df1, "mort_le4d")
  models_age2 <- create_models(df2, "mort_le3d")
  models_age1 <- create_models(df3, "mort_le2d")
  
  models <- c(models_age3, models_age2, models_age1)
  
  gm <- tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "N Obs.", 0,
    "adj.r.squared", "Adjusted R Squared", 3
  )
  
  rows <- tribble(
    ~"Coefficients", ~"Model 1", ~"Model 2", ~"Model 3", ~"Model 4",
    ~"Model 5", ~"Model 6", ~"Model 7", ~"Model 8", ~"Model 9",
    "State FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y",
    "Age FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y",
    "Time Trend", "N", "N", "Y", "N", "N", "Y", "N", "N", "Y"
  )
  
  var_names <- c(
    "vax_est:policy_dummy" = "HBV Vaccination Rate*Policy Dummy",
    "vax_est" = "HBV Vaccination Rate",
    "policy_dummy" = "Policy Dummy",
    
    "svi_housing_units_estimate" = "Log Number of Housing Units",
    "svi_multi_unit_housing_estimate" = "Log Number of Multi-Unit Housing",
    "svi_mobile_homes_estimate" = "Log Number of Mobile Homes",
    "svi_crowded_housing_estimate" = "Log Number of Crowded Housing",
    "svi_no_vehicle_households_estimate" = "Log Number of No Vehicle Households",
    "svi_limited_english_estimate" = "Log Number of Limited English Speakers",
    
    "percent_non_hispanic_african_american_raw_value" = "African American (%)",
    "percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native (%)",
    "percent_hispanic_raw_value" = "Hispanic American (%)",
    "percent_asian_raw_value" = "Asian American (%)"
  )
  
  # Create the table
  table <- modelsummary(
    models,
    title = title1,
    estimate = "{estimate} [{p.value}]",
    coef_map = var_names,
    gof_map = gm,
    add_rows = rows,
    output = "flextable"
  )
  
  # Adjust the table structure and formatting
  table1 <- table %>%
    add_header_row(
      values = c("", "Age: 0-3 Days Old", "Age: 0-2 Days Old", "Age: 0-1 Days Old"),
      colwidths = c(1, 3, 3, 3)
    ) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 6, part = "all") %>%
    align(align = "center", part = "header") %>%
    align(align = "left", part = "body") %>%
    padding(padding = .05, part = "all") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black")) %>%
    hline(i = 2, part = "header", border = fp_border(color = "black")) %>%
    hline_bottom(part = "body", border = fp_border(color = "black")) %>%
    hline(i = 1, j = 2:4, part = "header", border = fp_border(color = "black")) %>%
    hline(i = 1, j = 5:7, part = "header", border = fp_border(color = "black")) %>%
    hline(i = 1, j = 8:10, part = "header", border = fp_border(color = "black"))
  
  # Set specific widths for columns
  num_columns <- ncol(table1$body$dataset)
  table1 <- table1 %>%
    width(j = 1, width = 2.3) %>%
    width(j = 2:num_columns, width = .77)
  
  # Add footnote
  footnote_text <- "Note: The table presents the Ordinary Least Squares (OLS) results from equation (3). In this equation, the dependent variable is the neonatal mortality rate, while the independent variable of interest is the interaction term between a county as a low vaccination area (referred to as Vaccination Dummy) in the table and the 2018 HBV policy dummy (referred to as Policy Dummy in the table). The low vaccination area dummy takes value one if a county has a pre-policy vaccination rate below the 25th percentile. The policy is the CDC's changed recommendation of the first-dose HBV vaccination to neonates within 24 hours of birth on January 12, 2018. The policy dummy takes a binary value one, if the year is after 2017 and zero otherwise. The racial and ethnic variables indicate the percentages of the population belonging to specific racial or ethnic groups in a county. The median household income and unemployment rate variables are included to account for county-level economies. The proportion of children in poverty is calculated based on the federal poverty threshold of households in a county. Since parents' education level may influence the neonatal outcome variable, we incorporate two additional factors into our model. These factors are the percentage of parents who have a high school degree and the percentage of parents who have some college education. To control health behavior and gender, the model includes percentages of adult smokers and females in a county. To control for health-related factors that may impact the outcome, the model also includes the percentage of low birthweight, HIV prevalence, and per capita sexually transmitted infections. Further details about these variables are available from the data source County Health Rankings. Age 0-2 days include 0-1 and 0-2 days. Age 0-3 days include 0-1, 0-2, and 0-3 days. Standard errors are reported in parentheses and P-values in brackets."
  table1 <- table1 %>%
    add_footer_lines(footnote_text) %>%
    fontsize(size = 5, part = "footer")
  
  # Save the table to a DOCX file in landscape mode
  save_as_docx(table1, path = paste0(title1, ".docx"),
               pr_section = prop_section(
                 page_size = page_size(orient = "landscape")
               ))
  
  # Print the table in the console
  print(table1)
}
latex_file_name1= "mortality.csv"
title1="Table 4: Policy impacts on neonatal mortality through boosting first-dose HBV vaccination (time trend robustness check)"
print_reg4(latex_file_name1,title1,data_direct)

