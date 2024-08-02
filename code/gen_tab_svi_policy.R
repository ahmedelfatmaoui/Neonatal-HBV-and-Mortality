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

setwd("../tables-svi-policy") ## the outputs are saved here


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


df <- read_csv(data_direct) %>% 
  filter(vax_type=="Hep B"&age<3) %>% left_join(census_reg)

age_sum <- function(age_num) {
  return( datasummary_balance(~ policy_dummy,
                              fmt = fmt_decimal(digits = 3, pdigits = 3),
                              dinm_statistic='p.value',
                              stars=TRUE,
                              output = "dataframe",
                              data = df %>% filter(age<age_num) %>% 
                                select(
                                  policy_dummy,
                                  # Region,
                                  `HBV Vaccination Rate` = vax_est,
                                  `Infant Mortality Rate` = mortality_rate,
                                  
                                  # add the SVI variables here
                                  # `Log Unemployed Population` = svi_unemployed_estimate,
                                  
                                  `Log Number of Housing Units` = svi_housing_units_estimate,
                                  `Log Number of Multi-Unit Housing` = svi_multi_unit_housing_estimate,
                                  `Log Number of Mobile Homes` = svi_mobile_homes_estimate,
                                  `Log Number of Crowded Housing` = svi_crowded_housing_estimate,
                                  `Log Number of No Vehicle Households` = svi_no_vehicle_households_estimate,
                                  `Log Number of Limited English Speakers` = svi_limited_english_estimate,
                                  
                                  
                                  `African American (%)` = percent_non_hispanic_african_american_raw_value,
                                  `American Indian and Alaskan Native` = percent_american_indian_and_alaskan_native_raw_value,
                                  `Hispanic American (%)` = percent_hispanic_raw_value,
                                  `Asian American (%)` = percent_asian_raw_value,
                                  `Median Household Income (Logged)` = median_household_income_raw_value,
                                  `Children in Poverty (%)` = children_in_poverty_raw_value,
                                  `Rural Population (%)` = percent_rural_raw_value,
                                  `High School Graduation` = high_school_graduation_raw_value,
                                  `Some College Edu (%)` = some_college_raw_value,
                                  `Adult Smoking (%)` = adult_smoking_raw_value,
                                  `Unemployment Rate` = unemployment_raw_value,
                                  `Low Birthweight (%)` = low_birthweight_raw_value,
                                  `Female (%)` = percent_females_raw_value,
                                  `HIV Prevalence` = hiv_prevalence_raw_value,
                                  `Per Capita Sexually Transmitted Infections` = sexually_transmitted_infections_raw_value
                                  
                                )))
  
}
sum_age <- list()
# type = "Age: 1 to 3 Days"
# type = "Age: 2 Days Old and Less"
# type = "Age: 1 Days Old"
k=1
age_tresh <- c(3,2,1)
for (i in age_tresh) {
  sum_age[[k]] <-  age_sum(i) 
  
  names( sum_age[[k]]) <- c("","Mean","SD",
                            "Mean","SD",
                            "Diff. in Means","p")
  
  # remove the p-values
  sum_age[[k]] <- sum_age[[k]][,-ncol(sum_age[[k]])]
  
  k=k+1
  
}

put_sd_under <- function(data0) {
  # data0=sum_age[[1]]
  names(data0)[1] <- "Var"
  df1 <- data0 %>% select("SD",  "SD.1")
  df1$SD <- paste0("(",df1$SD,")")
  df1$SD.1 <- paste0("(",df1$SD.1,")")
  
  data0 <- data0 %>% select(-"SD",-"SD.1")
  
  # duplicate each row
  data0 <- data0[rep(row.names(data0), each=2),]
  
  # assign NA to all duplicated rows
  data0[seq(2,nrow(data0),2),] <- NA
  data0[seq(2,nrow(data0),2),"Mean"] <-   df1$SD
  data0[seq(2,nrow(data0),2),"Mean.1"] <-   df1$SD.1

  return(data0)
}

sum_age <- lapply(sum_age, put_sd_under)
sum1 <- cbind(sum_age[[1]],sum_age[[2]][,-1],sum_age[[3]][,-1])

# Rename the columns to make them unique
names(sum1) <- c("Var", "Mean1", "Mean1.1", "Diff1", "Mean2", "Mean2.1", "Diff2", "Mean3", "Mean3.1", "Diff3")

create_flextable <- function(sum1,title1) {

  # Create a flextable
  ft <- flextable(sum1)
  
  # Add first header row
  ft <- ft %>% add_header_row(
    values = c("", "Pre-Policy", "Post-Policy", "Difference", 
               "Pre-Policy", "Post-Policy", "Difference", 
               "Pre-Policy", "Post-Policy", "Difference")
  ) 
  
  # Add second header row
  ft <- ft %>% add_header_row(
    values = c("", "Age: 0-3 Days Old", "Age: 0-2 Days Old", "Age: 0-1 Days Old"),
    colwidths = c(1, 3, 3, 3)
  ) 
  
  # Merge cells in the header
  ft <- merge_h(ft, part = "header")
  
  # Set header labels
  ft <- ft %>% set_header_labels(ft,
                                 Var = "",
                                 Mean1 = "2011-2016", Mean1.1 = "2018-2019", Diff1 = "",
                                 Mean2 = "2011-2016", Mean2.1 = "2018-2019", Diff2 = "",
                                 Mean3 = "2011-2016", Mean3.1 = "2018-2019", Diff3 = "")
  
  # Adjust formatting
  ft <- autofit(ft)
  
  # Set font size to 7
  ft <- fontsize(ft, size = 7, part = "all")
  
  # Add footnote
  footnote_text <- "Note: The summary statistics table reports the means and standard deviations (in parentheses) of all variables in our final merged data, which consist of 8,397 total county-year observations from 2011 to 2019. Age 0-2 days include 0-1 and 0-2 days. Age 0-3 days include 0-1, 0-2, and 0-3 days. The policy is the CDC's changed recommendation of the first-dose HBV vaccination to neonates within 24 hours of birth on January 12, 2018. The asterisk in the third column of each panel denotes whether each pre and post-period difference is statistically significant at three levels: * p < 0.1, ** p < 0.05, *** p < 0.01."
  
  ft <- add_footer_lines(ft, values = footnote_text)
  ft <- fontsize(ft, size = 7, part = "footer")
  
  # Set column widths
  num_columns <- ncol(ft$body$dataset)
  ft <- ft %>%
    width(j = 1, width = 2.3) %>% # First column width
    width(j = 2:num_columns, width = 0.7) # Width for numeric columns
  
  # Final formatting
  ft <- ft %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 7, part = "all") %>%
    align(align = "center", part = "header") %>%
    align(align = "left", part = "body") %>%
    padding(padding = 2, part = "all") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black")) %>% 
    hline(i = 3, part = "header", border = fp_border(color = "black")) %>%
    hline_bottom(part = "body", border = fp_border(color = "black")) 
  
  # Set the caption/title and left-align it
  ft <- set_caption(ft, caption = title1)
  ft %>% print
  return(ft)
}

# Save to docx
# save in landscape mode
title1 <- "Table 1 (Panel A): County-level first-dose HBV vaccination rate, mortality rate, and socioeconomic and demographic characteristics of neonates"
save_as_docx(create_flextable(sum1,title1), path = paste0(title1,".docx"),pr_section = prop_section(
  page_size = page_size(orient = "landscape")
))



#=====================================
### summary stat by census regions====
#=====================================

sum_regions <- list()
region_sum <- function(name11) {
  return( datasummary_balance(~ policy_dummy,
                              fmt = fmt_decimal(digits = 3, pdigits = 3),
                              dinm_statistic='p.value',
                              stars=TRUE,
                              output = "dataframe",
                              #output =  paste0(file_char_name,".xlsx"),
                              data = df %>% filter(Region==name11) %>% 
                                select(
                                  policy_dummy,
                                  # Region,
                                  `HBV Vaccination Rate` = vax_est,
                                  
                                  `Log Number of Housing Units` = svi_housing_units_estimate,
                                  `Log Number of Multi-Unit Housing` = svi_multi_unit_housing_estimate,
                                  `Log Number of Mobile Homes` = svi_mobile_homes_estimate,
                                  `Log Number of Crowded Housing` = svi_crowded_housing_estimate,
                                  `Log Number of No Vehicle Households` = svi_no_vehicle_households_estimate,
                                  `Log Number of Limited English Speakers` = svi_limited_english_estimate,
                                  
                                  `Infant Mortality Rate` = mortality_rate,
                                  `African American (%)` = percent_non_hispanic_african_american_raw_value,
                                  `American Indian and Alaskan Native` = percent_american_indian_and_alaskan_native_raw_value,
                                  `Hispanic American (%)` = percent_hispanic_raw_value,
                                  `Asian American (%)` = percent_asian_raw_value,
                                  `Median Household Income (Logged)` = median_household_income_raw_value,
                                  `Children in Poverty (%)` = children_in_poverty_raw_value,
                                  `Rural Population (%)` = percent_rural_raw_value,
                                  `High School Graduation` = high_school_graduation_raw_value,
                                  `Some College Edu (%)` = some_college_raw_value,
                                  `Adult Smoking (%)` = adult_smoking_raw_value,
                                  `Unemployment Rate` = unemployment_raw_value,
                                  `Low Birthweight (%)` = low_birthweight_raw_value,
                                  `Female (%)` = percent_females_raw_value,
                                  `HIV Prevalence` = hiv_prevalence_raw_value,
                                  `Per Capita Sexually Transmitted Infections` = sexually_transmitted_infections_raw_value
                                  
                                  
                                  
                                )))
  
}

census_names <- df$Region %>% unique
k=1
for (i in census_names) {
  sum_regions[[k]] <-  region_sum(i) 
  
  names( sum_regions[[k]]) <- c("","Mean","SD",
                                "Mean","SD",
                                "Diff. in Means","p")
  # remove the p-values
  sum_regions[[k]] <- sum_regions[[k]][,-ncol(sum_regions[[k]])]
  k=k+1
  
}

sum_regions <- lapply(sum_regions, put_sd_under)

sum2 <- cbind(sum_regions[[1]],sum_regions[[2]][,-1],sum_regions[[3]][,-1],sum_regions[[4]][,-1])

# Rename the columns to make them unique
names(sum2) <- c("Var", "Mean1", "Mean1.1", "Diff1", "Mean2", "Mean2.1", "Diff2", "Mean3", "Mean3.1", "Diff3", "Mean4", "Mean4.1", "Diff4")


create_flextable2 <- function(sum11,title1) {
  
  # Create a flextable
  ft <- flextable(sum11)
 
  
  # Add first header row
  ft <- ft %>% add_header_row(
    values = c("", "Pre-Policy", "Post-Policy", "Difference", 
               "Pre-Policy", "Post-Policy", "Difference", 
               "Pre-Policy", "Post-Policy", "Difference",
               "Pre-Policy", "Post-Policy", "Difference")
  ) 
  
  # Add second header row
  ft <- ft %>% add_header_row(
    values = c("", "South Region", "West Region", "Northeast Region", "Midwest Region"),
    colwidths = c(1, 3, 3, 3, 3)
  ) 
  
  # Merge cells in the header
  ft <- merge_h(ft, part = "header")
  
  # Set header labels
  ft <- ft %>% set_header_labels(ft,
                                 Var = "",
                                 Mean1 = "2011-2016", Mean1.1 = "2018-2019", Diff1 = "",
                                 Mean2 = "2011-2016", Mean2.1 = "2018-2019", Diff2 = "",
                                 Mean3 = "2011-2016", Mean3.1 = "2018-2019", Diff3 = "",
                                 Mean4 = "2011-2016", Mean4.1 = "2018-2019", Diff4 = "")
  
  # Adjust formatting
  ft <- autofit(ft)
  
  # Set font size to 7
  ft <- fontsize(ft, size = 7, part = "all")
  
  # Add footnote
  footnote_text <- "Note: The summary statistics table reports the means and standard deviations (in parentheses) of all variables in our final merged data, which consist of 8,397 total county-year observations from 2011 to 2019. The policy is the CDC’s changed recommendation of the first-dose HBV vaccination to neonates within 24 hours of birth on January 12, 2018. The asterisk in the third column of each panel denotes whether each pre and post-period difference is statistically significant at three levels: * p < 0.1, ** p < 0.05, *** p < 0.01."
  
  ft <- add_footer_lines(ft, values = footnote_text)
  ft <- fontsize(ft, size = 7, part = "footer")
  
  # Set column widths
  num_columns <- ncol(ft$body$dataset)
  ft <- ft %>%
    width(j = 1, width = 2.3) %>% # First column width
    width(j = 2:num_columns, width = 0.7) # Width for numeric columns
  
  # Final formatting
  ft <- ft %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 7, part = "all") %>%
    align(align = "center", part = "header") %>%
    align(align = "left", part = "body") %>%
    padding(padding = 2, part = "all") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black")) %>% 
    hline(i = 3, part = "header", border = fp_border(color = "black")) %>%
    hline_bottom(part = "body", border = fp_border(color = "black")) 
  

  # Set the caption/title
  ft <- set_caption(ft, caption = title1)
  ft %>% print
  

   return(ft)
}

# Save to docx
# save in landscape mode
title1 <- "Table 1 (Panel B): County-level first-dose HBV vaccination rate, mortality rate, and socioeconomic and demographic characteristics of neonates aged 0-3 days by census region"
save_as_docx(create_flextable2(sum2,title1), path = paste0(title1,".docx"),pr_section = prop_section(
  page_size = page_size(orient = "landscape")
))


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
    "percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native",
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
    "percent_asian_raw_value",
    "median_household_income_raw_value",
    "children_in_poverty_raw_value",
    "percent_rural_raw_value",
    "high_school_graduation_raw_value",
    "some_college_raw_value",
    "adult_smoking_raw_value",
    "unemployment_raw_value",
    "low_birthweight_raw_value",
    "percent_females_raw_value",
    "hiv_prevalence_raw_value",
    "sexually_transmitted_infections_raw_value"
  )
  
  endo <<- c("policy_dummy")
  
  # Load and filter data
 df <-   read_csv(data_direct) %>%  filter(vax_type == "Hep B" & age < 3)
  
  
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
    "Age FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y"
  )
  
  
  # Your existing code to create the table
  table <- modelsummary(
    models,
    title = title1,
    stars = c('*' = .05, '**' = .01, '***' = .001),
    coef_map = var_names,
    gof_map = gm,
    add_rows = rows,
    output = "flextable"
  ) 
  
  # Adjust the table structure and formatting
  # Adjust the table structure and formatting
  table1 <- table %>%
    add_header_row(
      values = c("", "Age: 0-3 Days Old", "Age: 0-2 Days Old", "Age: 0-1 Days Old"),
      colwidths = c(1, 3, 3, 3)
    ) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 7, part = "all") %>%
    align(align = "center", part = "header") %>%
    align(align = "left", part = "body") %>%
    padding(padding = 2, part = "all") %>%
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
            width(j = 2:num_columns, width = 0.7) # Width for numeric columns
  
  # Add footnote
  footnote_text <- "Note: The table presents the Ordinary Least Squares (OLS) results from equation (1). In this equation, the dependent variable is the vaccination rate, while the independent variable of interest is the 2018 HBV Policy Dummy (referred to as Policy Dummy in the table). The policy is the CDC's changed recommendation of the first-dose HBV vaccination to neonates within 24 hours of birth on January 12, 2018. The policy dummy takes a binary value one, if the year is after 2017 and zero otherwise. The racial and ethnic variables indicate the percentages of the population belonging to specific racial or ethnic groups in a county. The median household income and unemployment rate variables are included to account for county-level economies. The proportion of children in poverty is calculated based on the federal poverty threshold of households in a county. Since parents' education level may influence the neonatal outcome variable, we incorporate two additional factors into our model. These factors are the percentage of parents who have a high school degree and the percentage of parents who have some college education. To control health behavior and gender, the model includes percentages of adult smokers and females in a county. To control for health-related factors that may impact the outcome, the model also includes the percentage of low birthweight, HIV prevalence, and per capita sexually transmitted infections. Further details about these variables are available from the data source County Health Rankings. Age 0-2 days include 0-1 and 0-2 days. Age 0-3 days include 0-1, 0-2, and 0-3 days. * p < 0.05, ** p < 0.01, *** p < 0.001"
  
  table1 <- table1 %>% add_footer_lines(footnote_text) %>% fontsize(size = 7, part = "footer")
 
  # Save the table to a DOCX file
  # save_as_docx(table1, path =  paste0(title1,".docx"))
  
  # If you still want to print the table in the console
  print(table1)
  
  
  # to save in landscape mode
  save_as_docx(table1, path = paste0(title1,".docx"),pr_section = prop_section(
    page_size = page_size(orient = "landscape")
  ))
  

  # Save as CSV
  # tab2 <- modelsummary(
  #   models,
  #   stars = c('*' = .05, '**' = .01, '***' = .001),
  #   coef_map = var_names,
  #   output = "dataframe",
  #   gof_map = gm
  # ) %>% .[, -c(1, 3)]
  # 
  # 
  # tab2$term[seq(2, (nrow(tab2) - 1), 2)] <- ""
  # names(tab2)[1] <- ""
  # 
  # tab2 <- rbind(
  #   tab2,
  #   c("State FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y"),
  #   c("Age FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y")
  # )
  # 
  # write_csv(tab2, latex_file_name1)
}

# Usage
latex_file_name1 <- "vaccination.csv"
title1 <- "Table 2: Policy effect on vaccination"
print_reg2(latex_file_name1, title1, data_direct)

#---------------------------------------------
# Race heterogeneity for vaccination
# table 3
#--------------------------------------------

print_reg3 <- function(latex_file_name1, title1, data_direct) {
  exog1 <<- c(
    "svi_housing_units_estimate",
    "svi_multi_unit_housing_estimate",
    "svi_mobile_homes_estimate",
    "svi_crowded_housing_estimate",
    "svi_no_vehicle_households_estimate",
    "svi_limited_english_estimate")
  
  exog2 <<- c(   
    
    exog1,
    
    
    "low_birthweight_raw_value",
    "children_in_poverty_raw_value"  ,
    "percent_females_raw_value"    ,
    "adult_smoking_raw_value",
    "median_household_income_raw_value",
    "some_college_raw_value"   ,
    "unemployment_raw_value"  ,
    "percent_asian_raw_value"    ,
    "sexually_transmitted_infections_raw_value", 
    "hiv_prevalence_raw_value"  ,
    "high_school_graduation_raw_value",
    "percent_american_indian_and_alaskan_native_raw_value",
    "percent_non_hispanic_african_american_raw_value"  ,
    "percent_hispanic_raw_value"   ,
    "percent_rural_raw_value"
  )
  
  endo <<- c("policy_dummy*percent_non_hispanic_african_american_raw_value",
            "policy_dummy*percent_hispanic_raw_value",
            "policy_dummy*percent_asian_raw_value",
            "policy_dummy*percent_american_indian_and_alaskan_native_raw_value"
  )
  
  df = read_csv(data_direct) %>% 
    filter(vax_type == "Hep B") %>% filter(age < 3)
  
  # Create datasets for different age groups
  df1 <- df %>% dplyr::rename(vax_le4d = vax_est)
  df2 <- df %>% filter(age < 2) %>% dplyr::rename(vax_le3d = vax_est)
  df3 <- df %>% filter(age < 1) %>% dplyr::rename(vax_le2d = vax_est)
  
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
  
  models_age3 <- create_models(df1, "vax_le4d")
  models_age2 <- create_models(df2, "vax_le3d")
  models_age1 <- create_models(df3, "vax_le2d")
  
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
    "Age FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y"
  )
  
  var_names <- c(
    "policy_dummy:percent_non_hispanic_african_american_raw_value" = "African American (%)*Policy Dummy",
    "policy_dummy:percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native*Policy Dummy",
    "policy_dummy:percent_hispanic_raw_value" = "Hispanic American (%)*Policy Dummy",
    "policy_dummy:percent_asian_raw_value" = "Asian American (%)*Policy Dummy",
    
    "svi_housing_units_estimate" = "Log Number of Housing Units",
    "svi_multi_unit_housing_estimate" = "Log Number of Multi-Unit Housing",
    "svi_mobile_homes_estimate" = "Log Number of Mobile Homes",
    "svi_crowded_housing_estimate" = "Log Number of Crowded Housing",
    "svi_no_vehicle_households_estimate" = "Log Number of No Vehicle Households",
    "svi_limited_english_estimate" = "Log Number of Limited English Speakers",
    
    
    "percent_non_hispanic_african_american_raw_value" = "African American (%)",
    "percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native",
    "percent_hispanic_raw_value" = "Hispanic American (%)",
    "percent_asian_raw_value" = "Asian American (%)",
    "vax_est:policy_dummy" = "HBV Vaccination Rate*Policy Dummy",
    "fit_vax_est:policy_dummy" = "Fitted HBV Vaccination Rate*Policy Dummy",
    "vax_est" = "HBV Vaccination Rate",
    "fit_vax_est" = "Fitted HBV Vaccination Rate",
    "policy_dummy" = "Policy Dummy",
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
  
  # Create the table
  table <- modelsummary(
    models,
    title = title1,
    stars = c('*' = .05, '**' = .01, '***' = .001),
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
    fontsize(size = 7, part = "all") %>%
    align(align = "center", part = "header") %>%
    align(align = "left", part = "body") %>%
    padding(padding = 2, part = "all") %>%
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
    width(j = 2:num_columns, width = 0.7)
  
  # Add footnote
  footnote_text1 <- "Note: The table presents the Ordinary Least Squares (OLS) results from equation (4). In this equation, the dependent variable is the neonatal mortality rate, while the independent variable of interest is the triple interaction term between the population share of a racial minority group, the lowest quartile vaccination dummy (referred to as Vaccination Dummy in the table), and the policy dummy (referred to as Policy Dummy in the table). The racial and ethnic variables indicate the percentages of the population belonging to specific racial or ethnic groups in a county. The low vaccination area dummy takes value one if a county has a pre-policy vaccination rate below the 25th percentile. The policy is the CDC’s changed recommendation of the first-dose HBV vaccination to neonates within 24 hours of birth on January 12, 2018. The policy dummy takes a binary value one, if the year is after 2017 and zero otherwise. The median household income and unemployment rate variables are included to account for county-level economies. The proportion of children in poverty is calculated based on the federal poverty threshold of households in a county. Since parents’ education level may influence the neonatal outcome variable, we incorporate two additional factors into our model. These factors are the percentage of parents who have a high school degree and the percentage of parents who have some college education. To control health behavior and gender, the model includes percentages of adult smokers and females in a county. To control for health-related factors that may impact the outcome, the model also includes the percentage of low birthweight, HIV prevalence, and per capita sexually transmitted infections. Further details about these variables are available from the data source County Health Rankings. Age 0-2 days include 0-1 and 0-2 days. Age 0-3 days include 0-1, 0-2, and 0-3 days. * p < 0.05, ** p < 0.01, *** p < 0.001"
  table1 <- table1 %>%
    add_footer_lines(footnote_text1) %>%
    fontsize(size = 7, part = "footer")
  
  # Save the table to a DOCX file in landscape mode
  save_as_docx(table1, path = paste0(title1, ".docx"),
               pr_section = prop_section(
                 page_size = page_size(orient = "landscape")
               ))
  
  # Print the table in the console
  print(table1)
  
  # # Save as CSV
  # tab2 <- modelsummary(
  #   models,
  #   stars = c('*' = .05, '**' = .01, '***' = .001),
  #   coef_map = var_names,
  #   output = "dataframe",
  #   gof_map = gm
  # ) %>% .[, -c(1, 3)]
  # 
  # tab2$term[seq(2, (nrow(tab2) - 1), 2)] <- ""
  # names(tab2)[1] <- ""
  # 
  # tab2 <- rbind(
  #   tab2,
  #   c("State FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y"),
  #   c("Age FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y")
  # )
  # 
  # write_csv(tab2, latex_file_name1)
}

title1='Table 3: Heterogeneous impacts of policy on first-dose HBV vaccination across racial and ethnic minority groups'
latex_file_name1= "hetrogeneity.csv"
print_reg3(latex_file_name1,title1,data_direct)


#--------------
# mortality
# table 4
#--------------

print_reg4 <- function(latex_file_name1, title1, data_direct) {
  
  exog1 <<- c(    'svi_housing_units_estimate',
                  'svi_multi_unit_housing_estimate',
                  'svi_mobile_homes_estimate',
                  'svi_crowded_housing_estimate',
                  'svi_no_vehicle_households_estimate',
                  'svi_limited_english_estimate')
  
  exog2 <<- c(
    
    exog1,
    
    "low_birthweight_raw_value", "children_in_poverty_raw_value", "percent_females_raw_value",
    "adult_smoking_raw_value", "median_household_income_raw_value", "some_college_raw_value",
    "unemployment_raw_value", "percent_asian_raw_value", "sexually_transmitted_infections_raw_value",
    "hiv_prevalence_raw_value", "high_school_graduation_raw_value",
    "percent_american_indian_and_alaskan_native_raw_value",
    "percent_non_hispanic_african_american_raw_value", "percent_hispanic_raw_value",
    "percent_rural_raw_value"
  )
  
  endo <<- c("vax_est*policy_dummy")
  
  df = read_csv(data_direct) %>% 
    filter(vax_type == "Hep B") %>% filter(age < 3)
  
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
    "Age FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y"
  )
  
  var_names <- c(
    "vax_est:policy_dummy" = "HBV Vaccination Rate*Policy Dummy",
    "fit_vax_est:policy_dummy" = "Fitted HBV Vaccination Rate*Policy Dummy",
    "vax_est" = "HBV Vaccination Rate",
    "fit_vax_est" = "Fitted HBV Vaccination Rate",
    "policy_dummy" = "Policy Dummy",
    "policy_dummy:percent_non_hispanic_african_american_raw_value" = "African American (%)*Policy Dummy",
    "policy_dummy:percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native*Policy Dummy",
    "policy_dummy:percent_hispanic_raw_value" = "Hispanic American (%)*Policy Dummy",
    "policy_dummy:percent_asian_raw_value" = "Asian American (%)*Policy Dummy",
    
    "svi_housing_units_estimate" = "Log Number of Housing Units",
    "svi_multi_unit_housing_estimate" = "Log Number of Multi-Unit Housing",
    "svi_mobile_homes_estimate" = "Log Number of Mobile Homes",
    "svi_crowded_housing_estimate" = "Log Number of Crowded Housing",
    "svi_no_vehicle_households_estimate" = "Log Number of No Vehicle Households",
    "svi_limited_english_estimate" = "Log Number of Limited English Speakers",
    
    "percent_non_hispanic_african_american_raw_value" = "African American (%)",
    "percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native",
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
  
  
  # Create the table
  table <- modelsummary(
    models,
    title = title1,
    stars = c('*' = .05, '**' = .01, '***' = .001),
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
    fontsize(size = 7, part = "all") %>%
    align(align = "center", part = "header") %>%
    align(align = "left", part = "body") %>%
    padding(padding = 2, part = "all") %>%
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
    width(j = 2:num_columns, width = 0.7)
  
  # Add footnote
  footnote_text <- "Note: The table presents the Ordinary Least Squares (OLS) results from equation (3). In this equation, the dependent variable is the neonatal mortality rate, while the independent variable of interest is the interaction term between a county as a low vaccination area (referred to as Vaccination Dummy) in the table and the 2018 HBV policy dummy (referred to as Policy Dummy in the table). The low vaccination area dummy takes value one if a county has a pre-policy vaccination rate below the 25th percentile. The policy is the CDC’s changed recommendation of the first-dose HBV vaccination to neonates within 24 hours of birth on January 12, 2018. The policy dummy takes a binary value one, if the year is after 2017 and zero otherwise. The racial and ethnic variables indicate the percentages of the population belonging to specific racial or ethnic groups in a county. The median household income and unemployment rate variables are included to account for county-level economies. The proportion of children in poverty is calculated based on the federal poverty threshold of households in a county. Since parents’ education level may influence the neonatal outcome variable, we incorporate two additional factors into our model. These factors are the percentage of parents who have a high school degree and the percentage of parents who have some college education. To control health behavior and gender, the model includes percentages of adult smokers and females in a county. To control for health-related factors that may impact the outcome, the model also includes the percentage of low birthweight, HIV prevalence, and per capita sexually transmitted infections. Further details about these variables are available from the data source County Health Rankings. Age 0-2 days include 0-1 and 0-2 days. Age 0-3 days include 0-1, 0-2, and 0-3 days. * p < 0.05, ** p < 0.01, *** p < 0.001 "
  table1 <- table1 %>%
    add_footer_lines(footnote_text) %>%
    fontsize(size = 7, part = "footer")
  
  # Save the table to a DOCX file in landscape mode
  save_as_docx(table1, path = paste0(title1, ".docx"),
               pr_section = prop_section(
                 page_size = page_size(orient = "landscape")
               ))

  table1 %>% print
}
latex_file_name1= "mortality.csv"
title1="Table 4: Policy impacts on neonatal mortality through boosting first-dose HBV vaccination"
print_reg4(latex_file_name1,title1,data_direct)

#--------------------------------------------
# Race heterogeneity for mortality
# table 5
#--------------------------------------------

print_reg5 <- function(latex_file_name1, title1, data_direct) {
  endo <<- c("policy_dummy*vax_est*percent_non_hispanic_african_american_raw_value",
             "policy_dummy*vax_est*percent_hispanic_raw_value",
             "policy_dummy*vax_est*percent_asian_raw_value",
             "policy_dummy*vax_est*percent_american_indian_and_alaskan_native_raw_value")
  
  
  exog1 <<- c(
    
    'svi_housing_units_estimate',
    'svi_multi_unit_housing_estimate',
    'svi_mobile_homes_estimate',
    'svi_crowded_housing_estimate',
    'svi_no_vehicle_households_estimate',
    'svi_limited_english_estimate')
    
    
  exog2 <<- c(
    
             exog1,
    
            "low_birthweight_raw_value",
             "children_in_poverty_raw_value",
             "percent_females_raw_value",
             "adult_smoking_raw_value",
             "median_household_income_raw_value",
             "some_college_raw_value",
             "unemployment_raw_value",
             "percent_asian_raw_value",
             "sexually_transmitted_infections_raw_value",
             "hiv_prevalence_raw_value",
             "high_school_graduation_raw_value",
             "percent_american_indian_and_alaskan_native_raw_value",
             "percent_non_hispanic_african_american_raw_value",
             "percent_hispanic_raw_value",
             "percent_rural_raw_value")
  
  df = read_csv(data_direct) %>%
    filter(vax_type=="Hep B") %>% filter(age<3)
  
  q3 = quantile(df$vax_est, 0.75)[[1]]
  df$vax_est <- ifelse(df$vax_est<q3, 1, 0)
  
  # Age: Day 1-3
  df1 <- df %>% dplyr::rename(mort_le4d=mortality_rate)
  dept_var="mort_le4d"
  model1 <- feols(.[dept_var] ~.[endo], data = df1)
  model01 = summary(model1, cluster = c('fips'))
  
  model2 <- feols(.[dept_var] ~.[c(endo,exog1)] | fips + age_fe, data = df1)
  model02 = summary(model2, cluster = c('fips'))
  
  model3 <- feols(.[dept_var] ~.[c(endo,exog2)] | fips + age_fe, data = df1)
  model03 = summary(model3, cluster = c('fips'))
  
  # Age: 2 Days Old and Less
  df2 <- df %>% filter(age<2) %>% dplyr::rename(mort_le3d=mortality_rate)
  dept_var="mort_le3d"
  model4 <- feols(.[dept_var] ~.[endo], data = df2)
  model04 = summary(model4, cluster = c('fips'))
  
  model5 <- feols(.[dept_var] ~.[c(endo,exog1)] | fips + age_fe, data = df2)
  model05 = summary(model5, cluster = c('fips'))
  
  model6 <- feols(.[dept_var] ~.[c(endo,exog2)] | fips + age_fe, data = df2)
  model06 = summary(model6, cluster = c('fips'))
  
  # Age: 1 Day Old
  df3 <- df %>% filter(age<1) %>% dplyr::rename(mort_le2d=mortality_rate)
  dept_var="mort_le2d"
  model7 <- feols(.[dept_var] ~.[endo], data = df3)
  model07 = summary(model7, cluster = c('fips'))
  
  model8 <- feols(.[dept_var] ~.[c(endo,exog1)] | fips + age_fe, data = df3)
  model08 = summary(model8, cluster = c('fips'))
  
  model9 <- feols(.[dept_var] ~.[c(endo,exog2)] | fips + age_fe, data = df3)
  model09 = summary(model9, cluster = c('fips'))
  
  gm <- tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "N Obs.", 0,
    "adj.r.squared", paste0("Adjusted R Squared"), 3
  )
  
  models = list(model01, model02, model03,
                model04, model05, model06,
                model07, model08, model09)
  
  rows <- tribble(
    ~"Coefficients", ~"Model 1", ~"Model 2", ~"Model 3", ~"Model 4",
    ~"Model 5", ~"Model 6", ~"Model 7", ~"Model 8", ~"Model 9",
    "State FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y",
    "Age FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y"
  )
  
  var_names <- c(
    "policy_dummy:vax_est:percent_non_hispanic_african_american_raw_value" = "African American (%)*HBV Vaccination Rate*Policy Dummy",
    "policy_dummy:vax_est:percent_hispanic_raw_value" = "Hispanic American (%)*HBV Vaccination Rate*Policy Dummy",
    "policy_dummy:vax_est:percent_asian_raw_value" = "Asian American (%)*HBV Vaccination Rate*Policy Dummy",
    "policy_dummy:vax_est:percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native*HBV Vaccination Rate*Policy Dummy",
    "policy_dummy:vax_est:children_in_poverty_raw_value" = "Children in Poverty (%)*HBV Vaccination Rate*Policy Dummy",
    "policy_dummy:percent_non_hispanic_african_american_raw_value" = "African American (%)*Policy Dummy",
    "policy_dummy:percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native*Policy Dummy",
    "policy_dummy:percent_hispanic_raw_value" = "Hispanic American (%)*Policy Dummy",
    "policy_dummy:percent_asian_raw_value" = "Asian American (%)*Policy Dummy",
    "vax_est:percent_non_hispanic_african_american_raw_value" = "African American (%)*HBV Vaccination Rate",
    "vax_est:percent_hispanic_raw_value" = "Hispanic American (%)*HBV Vaccination Rate",
    "vax_est:percent_asian_raw_value" = "Asian American (%)*HBV Vaccination Rate",
    "vax_est:percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native*HBV Vaccination Rate",
    "vax_est:children_in_poverty_raw_value" = "Children in Poverty (%)*HBV Vaccination Rate",
    "policy_dummy:vax_est" = "HBV Vaccination Rate*Policy Dummy",
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
    "percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native",
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
  
  
  # Create the table
  table <- modelsummary(
    models,
    title = title1,
    stars = c('*' = .05, '**' = .01, '***' = .001),
    coef_map = var_names,
    gof_map = gm,
    add_rows = rows,
    output = "flextable"
  )
  
  # Adjust the table structure and formatting
  table1 <- table %>%
    add_header_row(
      values = c("", "Age: Day 1-3", "Age: 2 Days Old and Less", "Age: 1 Day Old"),
      colwidths = c(1, 3, 3, 3)
    ) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 7, part = "all") %>%
    align(align = "center", part = "header") %>%
    align(align = "left", part = "body") %>%
    padding(padding = 2, part = "all") %>%
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
    width(j = 2:num_columns, width = 0.7)
  
  # Add footnote
  footnote_text <- "Note: The table presents the Ordinary Least Squares (OLS) results from equation (4). In this equation, the dependent variable is the neonatal mortality rate, while the independent variable of interest is the triple interaction term between the population share of a racial minority group, the lowest quartile vaccination dummy (referred to as Vaccination Dummy in the table), and the policy dummy (referred to as Policy Dummy in the table). The racial and ethnic variables indicate the percentages of the population belonging to specific racial or ethnic groups in a county. The low vaccination area dummy takes value one if a county has a pre-policy vaccination rate below the 25th percentile. The policy is the CDC’s changed recommendation of the first-dose HBV vaccination to neonates within 24 hours of birth on January 12, 2018. The policy dummy takes a binary value one, if the year is after 2017 and zero otherwise. The median household income and unemployment rate variables are included to account for county-level economies. The proportion of children in poverty is calculated based on the federal poverty threshold of households in a county. Since parents’ education level may influence the neonatal outcome variable, we incorporate two additional factors into our model. These factors are the percentage of parents who have a high school degree and the percentage of parents who have some college education. To control health behavior and gender, the model includes percentages of adult smokers and females in a county. To control for health-related factors that may impact the outcome, the model also includes the percentage of low birthweight, HIV prevalence, and per capita sexually transmitted infections. Further details about these variables are available from the data source County Health Rankings. Age 0-2 days include 0-1 and 0-2 days. Age 0-3 days include 0-1, 0-2, and 0-3 days. * p < 0.05, ** p < 0.01, *** p < 0.001"
    
  table1 <- table1 %>%
    add_footer_lines(footnote_text) %>%
    fontsize(size = 7, part = "footer")
  
  # Save the table to a DOCX file in landscape mode
  save_as_docx(table1, path = paste0(title1, ".docx"),
               pr_section = prop_section(
                 page_size = page_size(orient = "landscape")
               ))
  
  # Print the table
  table1 %>% print
  
  # Save to CSV file
  # tab2 <- modelsummary(
  #   dvnames(models),
  #   stars = c('*' = .05, '**' = .01, '***' = .001),
  #   coef_map = var_names,
  #   output = "dataframe",
  #   gof_map = gm
  # ) %>% .[, -c(1, 3)]
  # 
  # tab2$term[seq(2, (nrow(tab2) - 1), 2)] <- ""
  # names(tab2)[1] <- ""
  # 
  # tab2 <- rbind(
  #   tab2,
  #   c("State FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y"),
  #   c("Age FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y")
  # )
  # 
  # write_csv(tab2, latex_file_name1)
}

title1='Table 5: Heterogeneous impacts of policy on neonatal mortality through boosting first-dose HBV vaccination across racial and ethnic minority groups'
latex_file_name1= "hetrogeneity_mortality.csv"
print_reg5(latex_file_name1,title1,data_direct)


#######################
# Falsification tests #
#######################

df <- read_csv( "../data/hepb_mortality_svi.csv") %>% filter(year<2016) 
# choose randomly 2 years to be affected
df$policy_dummy <- ifelse(df$year %in% 2014:2015 ,1,0)

#-----------------
# Vaccination
#-----------------

print_reg2_placebo <- function(latex_file_name1, title1) {
  # Define variable names for output
  var_names <- c(
    "vax_est:policy_dummy" = "HBV Vaccination Rate*Policy Dummy",
    "fit_vax_est:policy_dummy" = "Fitted HBV Vaccination Rate*Policy Dummy",
    "vax_est" = "HBV Vaccination Rate",
    "fit_vax_est" = "Fitted HBV Vaccination Rate",
    "policy_dummy" = "2014 Hypothetical Policy Dummy",
    
    "svi_housing_units_estimate" = "Log Number of Housing Units",
    "svi_multi_unit_housing_estimate" = "Log Number of Multi-Unit Housing",
    "svi_mobile_homes_estimate" = "Log Number of Mobile Homes",
    "svi_crowded_housing_estimate" = "Log Number of Crowded Housing",
    "svi_no_vehicle_households_estimate" = "Log Number of No Vehicle Households",
    "svi_limited_english_estimate" = "Log Number of Limited English Speakers",
    
    "percent_non_hispanic_african_american_raw_value" = "African American (%)",
    "percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native",
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
  
  exog1 <<- c(
    
    'svi_housing_units_estimate',
    'svi_multi_unit_housing_estimate',
    'svi_mobile_homes_estimate',
    'svi_crowded_housing_estimate',
    'svi_no_vehicle_households_estimate',
    'svi_limited_english_estimate')
    
    
  # Define exogenous and endogenous variables
  exog <<- c(
    
    exog1,
    
    
    "percent_non_hispanic_african_american_raw_value",
    "percent_american_indian_and_alaskan_native_raw_value",
    "percent_hispanic_raw_value",
    "percent_asian_raw_value",
    "median_household_income_raw_value",
    "children_in_poverty_raw_value",
    "percent_rural_raw_value",
    "high_school_graduation_raw_value",
    "some_college_raw_value",
    "adult_smoking_raw_value",
    "unemployment_raw_value",
    "low_birthweight_raw_value",
    "percent_females_raw_value",
    "hiv_prevalence_raw_value",
    "sexually_transmitted_infections_raw_value"
  )
  endo <- c("policy_dummy")
  
  # Data preparation
  df <- df %>%
    filter(vax_type == "Hep B") %>%
    filter(age < 3)
  
  # Define a function to create and summarize models
  create_model <- function(data, dep_var, variables, fixed_effects = NULL) {
    formula <- as.formula(paste(dep_var, "~", paste(variables, collapse = " + "), 
                                if (!is.null(fixed_effects)) paste0(" | ", paste(fixed_effects, collapse = " + "))))
    model <- feols(formula, data = data)
    summary(model, cluster = c('fips'))
  }
  
  # Create models for different age groups
  models <- list(
    # Age: 0-3 Days
    create_model(df %>% rename(vax_le4d = vax_est), "vax_le4d", endo),
    create_model(df %>% rename(vax_le4d = vax_est), "vax_le4d", c(endo, exog1), c("fips", "age")),
    create_model(df %>% rename(vax_le4d = vax_est), "vax_le4d", c(endo, exog), c("fips", "age")),
    
    # Age: 0-2 Days
    create_model(df %>% filter(age < 2) %>% rename(vax_le3d = vax_est), "vax_le3d", endo),
    create_model(df %>% filter(age < 2) %>% rename(vax_le3d = vax_est), "vax_le3d", c(endo, exog1), c("fips", "age")),
    create_model(df %>% filter(age < 2) %>% rename(vax_le3d = vax_est), "vax_le3d", c(endo, exog), c("fips", "age")),
    
    # Age: 0-1 Days
    create_model(df %>% filter(age < 1) %>% rename(vax_le2d = vax_est), "vax_le2d", endo),
    create_model(df %>% filter(age < 1) %>% rename(vax_le2d = vax_est), "vax_le2d", c(endo, exog1), c("fips", "age")),
    create_model(df %>% filter(age < 1) %>% rename(vax_le2d = vax_est), "vax_le2d", c(endo, exog), c("fips", "age"))
  )
  
  # Define goodness-of-fit measures
  gm <- tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "N Obs.", 0,
    "adj.r.squared", "Adjusted R Squared", 3
  )
  
  # Define additional rows for the table
  rows <- tribble(
    ~"Coefficients", ~"Model 1", ~"Model 2", ~"Model 3", ~"Model 4", ~"Model 5", ~"Model 6", ~"Model 7", ~"Model 8", ~"Model 9",
    "State FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y",
    "Age FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y"
  )
  
  # Create the table
  table <- modelsummary(
    models,
    title = title1,
    stars = c('*' = .05, '**' = .01, '***' = .001),
    coef_map = var_names,
    gof_map = gm,
    add_rows = rows,
    output = "flextable"
  )
  
  # Format the table
  table1 <- table %>%
    add_header_row(
      values = c("", "Age: 0-3 Days Old", "Age: 0-2 Days Old", "Age: 0-1 Days Old"),
      colwidths = c(1, 3, 3, 3)
    ) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 7, part = "all") %>%
    align(align = "center", part = "header") %>%
    align(align = "left", part = "body") %>%
    padding(padding = 2, part = "all") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black")) %>%
    hline(i = 2, part = "header", border = fp_border(color = "black")) %>%
    hline_bottom(part = "body", border = fp_border(color = "black")) %>%
    hline(i = 1, j = 2:4, part = "header", border = fp_border(color = "black")) %>%
    hline(i = 1, j = 5:7, part = "header", border = fp_border(color = "black")) %>%
    hline(i = 1, j = 8:10, part = "header", border = fp_border(color = "black"))
  
  # Set column widths
  num_columns <- ncol(table1$body$dataset)
  table1 <- table1 %>%
    width(j = 1, width = 2.3) %>%
    width(j = 2:num_columns, width = 0.7)
  
  # Add footnote
  footnote_text <- "Note: This table conducts a falsification/sensitivity test to examine the effect of a hypothetical policy imposed in 2014, rather than 2018 (the true policy), on the county-level first-dose HBV vaccination rate, where we expect an insignificant effect, rather than a significant result in Table 2. The table estimates equation (1), where the dependent variable is the vaccination rate, while the independent variable of interest is the 2014 hypothetical HBV policy dummy (referred to as the 2014 Hypothetical Policy Dummy in the table). It takes a binary value of one if the year is after 2013 and zero otherwise. The results show that the hypothetical HBV policy in 2014 had no effect on the vaccination rate. This means that our results in Table 3 are most likely due to the true HBV policy in 2018 and not other unobserved policies or factors. This provides evidence that the true HBV policy in 2018 positively affected vaccination. Details about these variables are available from the data source County Health Rankings. Age 0-2 days include 0-1 and 0-2 days. Age 0-3 days include 0-1, 0-2, and 0-3 days. * p < 0.05, ** p < 0.01, *** p < 0.001 "
  table1 <- table1 %>% 
    add_footer_lines(footnote_text) %>% 
    fontsize(size = 7, part = "footer")
  
  # Print the table
  print(table1)
  
  # Save the table as a DOCX file in landscape mode
  save_as_docx(table1, path = paste0(title1, ".docx"),
               pr_section = prop_section(
                 page_size = page_size(orient = "landscape")
               ))
  
  # Save data as CSV (commented out in the original code)
  # write_csv(tab2, latex_file_name1)
}

latex_file_name1= "vaccination_placebo.csv"
title1="Table S1: Falsification test for first-dose HBV vaccination using hypothetical policy imposed in year 2014"
print_reg2_placebo(latex_file_name1,title1)

#-----------------
# Mortality
#-----------------
print_reg3 <- function(latex_file_name1, title1) {
  # Define endogenous variable
  endo <- c("policy_dummy")
  
  # Data preparation
  df <- df %>% 
    filter(vax_type == "Hep B", age < 3)
  
  # Calculate 75th percentile of vax_est and create binary variable
  q3 <- quantile(df$vax_est, 0.75)[[1]]
  df$vax_est <- ifelse(df$vax_est < q3, 1, 0)
  
  # Define a function to create and summarize models
  create_model <- function(data, dep_var, variables, fixed_effects = NULL) {
    formula <- as.formula(paste(dep_var, "~", paste(variables, collapse = " + "), 
                                if (!is.null(fixed_effects)) paste0(" | ", paste(fixed_effects, collapse = " + "))))
    model <- feols(formula, data = data)
    summary(model, cluster = c('fips'))
  }
  
  # Create models for different age groups
  models <- list(
    # Age: 0-3 Days
    create_model(df %>% rename(mort_le4d = mortality_rate), "mort_le4d", endo),
    create_model(df %>% rename(mort_le4d = mortality_rate), "mort_le4d", c(endo, exog1), c("fips", "age_fe")),
    create_model(df %>% rename(mort_le4d = mortality_rate), "mort_le4d", c(endo, exog), c("fips", "age_fe")),
    
    # Age: 0-2 Days
    create_model(df %>% filter(age < 2) %>% rename(mort_le3d = mortality_rate), "mort_le3d", endo),
    create_model(df %>% filter(age < 2) %>% rename(mort_le3d = mortality_rate), "mort_le3d", c(endo, exog1), c("fips", "age_fe")),
    create_model(df %>% filter(age < 2) %>% rename(mort_le3d = mortality_rate), "mort_le3d", c(endo, exog), c("fips", "age_fe")),
    
    # Age: 0-1 Days
    create_model(df %>% filter(age < 1) %>% rename(mort_le2d = mortality_rate), "mort_le2d", endo),
    create_model(df %>% filter(age < 1) %>% rename(mort_le2d = mortality_rate), "mort_le2d", c(endo, exog1), c("fips", "age_fe")),
    create_model(df %>% filter(age < 1) %>% rename(mort_le2d = mortality_rate), "mort_le2d", c(endo, exog), c("fips", "age_fe"))
  )
  
  # Define goodness-of-fit measures
  gm <- tibble::tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "N Obs.", 0,
    "adj.r.squared", "Adjusted R Squared", 3
  )
  
  # Define additional rows for the table
  rows <- tribble(
    ~"Coefficients", ~"Model 1", ~"Model 2", ~"Model 3", ~"Model 4", ~"Model 5", ~"Model 6", ~"Model 7", ~"Model 8", ~"Model 9",
    "State FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y",
    "Age FE", "N", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y"
  )
  
  # Define variable names for output
  var_names <- c(
    "vax_est:policy_dummy" = "HBV Vaccination Rate*Policy Dummy",
    "fit_vax_est:policy_dummy" = "Fitted HBV Vaccination Rate*Policy Dummy",
    "vax_est" = "HBV Vaccination Rate",
    "fit_vax_est" = "Fitted HBV Vaccination Rate",
    "policy_dummy" = "2014 Hypothetical Policy Dummy",
    "policy_dummy:percent_non_hispanic_african_american_raw_value" = "African American (%)*Policy Dummy",
    "policy_dummy:percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native*Policy Dummy",
    "policy_dummy:percent_hispanic_raw_value" = "Hispanic American (%)*Policy Dummy",
    "policy_dummy:percent_asian_raw_value" = "Asian American (%)*Policy Dummy",
   
    "svi_housing_units_estimate" = "Log Number of Housing Units",
    "svi_multi_unit_housing_estimate" = "Log Number of Multi-Unit Housing",
    "svi_mobile_homes_estimate" = "Log Number of Mobile Homes",
    "svi_crowded_housing_estimate" = "Log Number of Crowded Housing",
    "svi_no_vehicle_households_estimate" = "Log Number of No Vehicle Households",
    "svi_limited_english_estimate" = "Log Number of Limited English Speakers",
    
    "percent_non_hispanic_african_american_raw_value" = "African American (%)",
    "percent_american_indian_and_alaskan_native_raw_value" = "American Indian and Alaskan Native",
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
  
  # Create the table
  table <- modelsummary(
    models,
    title = title1,
    stars = c('*' = .05, '**' = .01, '***' = .001),
    coef_map = var_names,
    gof_map = gm,
    add_rows = rows,
    output = "flextable"
  )
  
  # Format the table
  table1 <- table %>%
    add_header_row(
      values = c("", "Age: 0-3 Days Old", "Age: 0-2 Days Old", "Age: 0-1 Days Old"),
      colwidths = c(1, 3, 3, 3)
    ) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 7, part = "all") %>%
    align(align = "center", part = "header") %>%
    align(align = "left", part = "body") %>%
    padding(padding = 2, part = "all") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black")) %>%
    hline(i = 2, part = "header", border = fp_border(color = "black")) %>%
    hline_bottom(part = "body", border = fp_border(color = "black")) %>%
    hline(i = 1, j = 2:4, part = "header", border = fp_border(color = "black")) %>%
    hline(i = 1, j = 5:7, part = "header", border = fp_border(color = "black")) %>%
    hline(i = 1, j = 8:10, part = "header", border = fp_border(color = "black"))
  
  # Set column widths
  num_columns <- ncol(table1$body$dataset)
  table1 <- table1 %>%
    width(j = 1, width = 2.3) %>%
    width(j = 2:num_columns, width = 0.7)
  
  # Add footnote
  footnote_text <- "Note: This table conducts a falsification/sensitivity test to examine the effect of a hypothetical policy imposed in 2014, rather than 2018 (the true policy), on the county-level neonatal mortality rate, where we expect an insignificant effect, rather than a significant result. We estimated Eq (1), where the dependent variable is the neonatal mortality rate, while the independent variable of interest is the 2014 hypothetical HBV policy dummy (referred to as the 2014 Hypothetical Policy Dummy in the table). It takes a binary value of one if the year is after 2013 and zero otherwise. The results show that the hypothetical HBV policy in 2014 had no effect on the mortality rate. This means our results in Table 5 are most likely due to the true HBV policy in 2018 and not other unobserved policies or factors. Details about these variables are available from the data source County Health Rankings. Age 0-2 days include 0-1 and 0-2 days. Age 0-3 days include 0-1, 0-2, and 0-3 days. * p < 0.05, ** p < 0.01, *** p < 0.001"
  table1 <- table1 %>% 
    add_footer_lines(footnote_text) %>% 
    fontsize(size = 7, part = "footer")
  
  # Save the table as a DOCX file in landscape mode
  save_as_docx(table1, path = paste0(title1, ".docx"),
               pr_section = prop_section(
                 page_size = page_size(orient = "landscape")
               ))
  
  # Print the table
  print(table1)
}
latex_file_name1= "mortality_placebo.csv"
title1="Table S2: Falsification test for neonatal mortality using hypothetical policy imposed in year 2014"
print_reg3(latex_file_name1,title1)
