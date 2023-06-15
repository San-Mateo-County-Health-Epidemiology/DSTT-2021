# ================================================
#
# functions for pulling Mental Health ESSENCE data 
# 
# ================================================

# pull essence api with start + end dates --------
pull_essence_api <- function(start_date, end_date) {
  
  end_date <- format(as.Date(end_date), "%d%b%Y")
  start_date <- format(as.Date(start_date), "%d%b%Y")
  
  
  csv_url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?endDate=", end_date, "&geography=ca_san%20mateo&percentParam=noPercent&patientClass=e&datasource=va_er&startDate=", start_date,"&medicalGroupingSystem=essencesyndromes&userId=3953&site=869&aqtTarget=DataDetails&ccddCategory=cdc%20anxiety%20disorders%20v1&ccddCategory=cdc%20attention-deficit%20hyperactivity%20disorders%20v1&ccddCategory=cdc%20all%20drug%20v2&ccddCategory=cdc%20opioid%20overdose%20v3&ccddCategory=cdc%20depressive%20disorders%20v1&ccddCategory=cdc%20mental%20health%20v1&ccddCategory=sdc%20suicide%20related%20v1&ccddCategory=cdc%20schizophrenia%20spectrum%20disorders%20v1&ccddCategory=cdc%20trauma%20and%20stressor-related%20disorders%20v1&geographySystem=region&detector=probrepswitch&timeResolution=daily")
  
  
  api_response <- GET(csv_url, 
                      authenticate(key_list("essence")[1,2], 
                                   key_get("essence", 
                                           key_list("essence")[1,2])))
  
  data <- content(api_response, by = "csv/text") %>%
    read_csv() %>%
    mutate_all(as.character)
  
  data
  
}

