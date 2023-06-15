# function to clean and filter essence data -----------------------
## this function:
### selects and cleans up variables
### filters to only keep specific conditions 
### outputs a dataset that is one row per visit with a list of conditions in the ccdd_all column

clean_essence_data <- function(data, disorders_of_interest) {
  
  max_ccdd <- data %>%
    rename(ccdd = CCDDCategory_flat) %>%
    mutate(ccdd_ct = str_count(ccdd, ";")) %>%
    summarize(max = max(ccdd_ct)+1) %>%
    pull(max)
  
  data1 <- data %>%
    distinct(PID, Date, CCDDCategory_flat, .keep_all = T) %>%
    mutate(date = as.Date(Date, format = "%m/%d/%Y"),
           epi_week = epiweek(date), 
           week_start = floor_date(date, unit = "weeks", week_start = 7),
           visit_month = floor_date(date, unit = "month"), 
           visit_year = year(week_start), # since we're looking at things by epi week, we want the year of the epi week, not the year of the visit (ex: a visit on 1/1/2022 is attributed to epi week 52 of 2021)
           hospital_name = case_when(str_detect(HospitalName, "Stanford") ~ "Stanford",
                                     str_detect(HospitalName, "Mills-Peninsula") ~ "Mills-Peninsula",
                                     str_detect(HospitalName, "San Mateo Medical Center") ~ "San Mateo Medical Center", 
                                     str_detect(HospitalName, "Kfh Redwood") ~ "Kaiser - Redwood City",
                                     str_detect(HospitalName, "Kfh South San Francisco") ~ "Kaiser - South San Francisco",
                                     str_detect(HospitalName, "Seton Coastside") ~ "Seton Coastside",
                                     str_detect(HospitalName, "Seton Medical Center") ~ "Seton Medical Center"),
           sex = case_when(Sex == "F" ~ "Female", 
                           Sex == "M" ~ "Male",
                           TRUE ~ "Other"),
           age_group = case_when(Age %in% 0:9 ~ "0-9",
                                 Age %in% 10:19 ~ "10-19", 
                                 Age %in% 20:29 ~ "20-29", 
                                 Age %in% 30:39 ~ "30-39",
                                 Age %in% 40:49 ~ "40-49",
                                 Age %in% 50:59 ~ "50-59",
                                 Age %in% 60:69 ~ "60-69",
                                 Age %in% 70:79 ~ "70-79",
                                 Age %in% 80:89 ~ "80-89",
                                 Age >= 90 ~ "90+"),
           race_cat = case_when(c_ethnicity == "Hispanic or Latino" ~ "Hispanic/Latino",
                                c_race %in% c("Other Race", "Not Reported or Null", "Unknown", "Not Categorized") ~ "Unknown or Other",
                                TRUE ~ c_race)) %>%
    select(PID, week_start, epi_week, date, visit_month, visit_year, hospital_name, sex, Age, age_group, race_cat, DispositionCategory, CCDDCategory_flat) %>%
    rename(ccdd = CCDDCategory_flat,
           age = Age) %>%
    separate(ccdd, paste0("ccdd_", 1:max_ccdd), sep = ";", fill = "right") %>%
    pivot_longer(names_to = "var",
                 values_to = "val",
                 values_drop_na = T,
                 cols = matches("ccdd_")) %>%
    mutate(ccdd_new = str_squish(str_replace_all(val, "CDC|SDC|v\\d+|\\(without.*\\)", ""))) %>%
    filter(str_detect(ccdd_new, disorders_of_interest)) %>%
    distinct(PID, date, ccdd_new, .keep_all = T) %>%
    select(-val) %>%
    group_by(PID, date) %>%
    arrange(ccdd_new) %>%
    mutate(var = paste0("ccdd_", row_number())) %>% 
    pivot_wider(names_from = var,
                values_from = ccdd_new) %>%
    mutate(across(matches("ccdd_"), ~ case_when(is.na(.x) ~ "", TRUE ~ .x))) %>%
    unite("ccdd_all", matches("ccdd_"), sep = ", ") %>%
    mutate(ccdd_all = str_replace(str_squish(str_replace_all(ccdd_all, "\\,\\s\\,\\s|\\s$", "")), "\\,$", "")) %>%
    ungroup()
  
  data1
}

# function to get counts of visits by combination of conditions ---------------
visits_by_dx_and_epi_week_hist <- function(data) {
  
  last_year <- data %>%
    summarize(max_date = max(week_start)) %>%
    mutate(year = as.numeric(year(max_date))) %>%
    pull(year)
  
  data1 <- data %>%
    distinct(PID, date, epi_week, week_start, ccdd_all, hospital_name, age_group, race_cat, sex) %>%
    mutate(year_date = as.numeric(year(week_start)),
           last_3 = (last_year - 2),
           last_5 = (last_year - 4),
           last_7 = (last_year - 6),
           last_1_year = case_when(year_date == last_year ~ 1, TRUE ~ 0),
           last_3_years = case_when(last_3 <= year_date ~ 1, TRUE ~ 0),
           last_5_years = case_when(last_5 <= year_date ~ 1, TRUE ~ 0),
           last_7_years = case_when(last_7 <= year_date ~ 1, TRUE ~ 0)) %>%
    select(-c(last_3, last_5, last_7)) %>%
    pivot_longer(names_to = "timeframe",
                 values_to = "indicator",
                 cols = matches("last_")) %>%
    filter(indicator == 1) %>%
    group_by(week_start, epi_week, ccdd_all, timeframe, hospital_name, age_group, race_cat, sex) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    distinct(week_start, epi_week, ccdd_all, timeframe, hospital_name, age_group, race_cat, sex, total)
  
}

visits_by_dx_and_epi_week_ytd <- function(data) {

  data1 <- data %>%
    distinct(PID, date, epi_week, week_start, ccdd_all, hospital_name, age_group, race_cat, sex) %>%

    group_by(date, week_start, epi_week, ccdd_all, hospital_name, age_group, race_cat, sex) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    distinct(date, week_start, epi_week, ccdd_all, hospital_name, age_group, race_cat, sex, total)
  
}
