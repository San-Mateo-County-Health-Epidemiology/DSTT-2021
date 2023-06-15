# ================================================
#
# pulling historical ESSENCE data 
# 
# ================================================

# set up -----------------------------------------

library(tidyverse)
library(httr)
library(jsonlite)
library(keyring)
library(lubridate)
library(plotly)

## functions ----
source("functions/function_pull_essence_api_with_start_and_end_dates.R")
source("functions//functions_to_clean_and_count_essence_data.R")

# pull all historical data -----------------------
## fill in the start and end years ----
start_year <- 2015
end_year <- year(Sys.Date()) - 1

## download data ----
historical_data <- data.frame()

for(i in start_year:end_year) {
  
  print(i)
  
  start_date <- paste0(i, "-01-01")
  end_date <- paste0(i, "-12-31")
  
  data <- pull_essence_api(start_date, end_date)
  
  historical_data <- historical_data %>%
    bind_rows(data)
 
}

# clean up the data ------------------------------
## list out disorders of interest ----
disorders_of_interest <- c("All Drug|Anxiety|Attention-Deficit Hyperactivity Disorders|Depressive Disorders|Mental Health|Opioid Overdose|Schizophrenia Spectrum Disorders|Suicide Related|Trauma and Stressor-related Disorders")

## run the function to clean up and filter the data ----
historical_data1 <- clean_essence_data(historical_data, disorders_of_interest)

# get historic counts for comparison -------------
historical_visit_counts_all_vars <- visits_by_dx_and_epi_week_hist(historical_data1)

historical_visit_counts_all_vars %>%
  group_by(timeframe) %>%
  summarize(all = sum(total))

# save the file ----------------------------------
## create the directory if needed ----
if(dir.exists("historical_data") == T) {
  
  print("folder exists, carry on")
  
} else {
  
  dir.create("historical_data")
  print("directory created")
  
}

## save the file ----
save(historical_visit_counts_all_vars, file = "historical_data/historic_visit_counts.Rda")
