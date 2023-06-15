# ================================================
#
# connecting to essence API code 
# 
# ================================================

rm(list=ls())

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

# pull ytd data ----------------------------------
end_date <- Sys.Date()
start_date <- floor_date(Sys.Date(), unit = "year")

ytd_data <- pull_essence_api(start_date, end_date)

## check on the date range ----
ytd_data %>%
  mutate(create_date = as.Date(Date, format = "%m/%d/%Y")) %>%
  summarize(min(create_date),
            max(create_date))

# clean up the data ------------------------------
## list out disorders of interest ----
disorders_of_interest <- c("All Drug|Anxiety|Attention-Deficit Hyperactivity Disorders|Depressive Disorders|Mental Health|Opioid Overdose|Schizophrenia Spectrum Disorders|Suicide Related|Trauma and Stressor-related Disorders")

## run the function to clean up and filter the data ----
ytd_data1 <- clean_essence_data(ytd_data, disorders_of_interest)

# get the counts for this year -------------------
ytd_visit_counts_all_vars <- visits_by_dx_and_epi_week_ytd(ytd_data1)

# save the data ----------------------------------
## create the directory if needed ----
if(dir.exists("ytd_data") == T) {
  
  print("folder exists, carry on")
  
} else {
  
  dir.create("ytd_data")
  
}

## save the file ----
date <- str_replace_all(Sys.Date(), "-", "")
path <- paste0("ytd_data/ytd_visit_counts_", date, ".Rda")
save(ytd_visit_counts_all_vars, file = path)