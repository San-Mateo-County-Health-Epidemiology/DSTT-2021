# shiny app functions ------------------------------
## for plots on tab 1 ----

t1_plot_fun <- function(data, disorder, color) {
  
  data %>%
    filter(str_detect(ccdd_all, disorder)) %>%
    group_by(date) %>%
    mutate(ytd_total = sum(ytd_total)) %>%
    ungroup() %>%
    complete(date = seq.Date(from = min(date), 
                             to = max(date), 
                             by = "day")) %>%
    arrange(date) %>%
    mutate(ytd_total = case_when(is.na(ytd_total) ~ as.integer(0), TRUE ~ ytd_total),
           roll_avg_7 = round(rollmean(ytd_total, k = 7, na.pad = T, align = "left"), 2)) %>%
    plot_ly(x = ~date) %>%
    add_trace(y = ~ytd_total, mode = "lines", type = "scatter", name = "Visit Count", line = list(color = "#D5D8DC")) %>%
    add_trace(x = ~date, y= ~roll_avg_7, mode = "lines", type = "scatter", name = "7-Day Rolling Average", line = list(color = color, width = 4)) %>%
    layout(xaxis = list(title = ""))
  
}