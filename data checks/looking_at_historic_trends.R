# ================================================
#
# Reviewing historical data 
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
library(zoo)
library(roll)

# load historical data ---------------------------
load("historical_data/historical_data.Rda")

# look at the data over time ---------------------
## visits by hospital ----
hist_time_p <- historical_data1 %>%
  count(visit_month, hospital_name) %>%
  tidyr::complete(visit_month, hospital_name) %>%
  plot_ly() %>%
  add_trace(x = ~visit_month, y = ~n, color = ~hospital_name, type = "scatter", mode = "lines")
hist_time_p

## get averages per week ----
historic_averages <- historical_data1 %>%
  group_by(epi_week, week_start) %>%
  summarize(particip_hosp = n_distinct(hospital_name),
            tot_visits = n(),
            .groups = "keep") %>%
  ungroup() %>%
  arrange(week_start) %>%
  mutate(avg_visits = tot_visits/particip_hosp,
         roll_avg_visits = rollmean(avg_visits, k = 5, na.pad = T, align = "center"))

hist_avg_p <- historic_averages %>%
  select(week_start, avg_visits, roll_avg_visits) %>%
  arrange(week_start) %>%
  plot_ly(x = ~week_start) %>%
  add_trace(y = ~avg_visits, mode = "lines", type = "scatter") %>%
  add_trace(y = ~roll_avg_visits, mode = "lines", type = "scatter")

hist_avg_p

## look at the plots side by side ----
subplot(hist_time_p,
        hist_avg_p,
        nrows = 2)