# ================================================
#
# R Shiny App for Mental Health ESSENCE data 
# 
# ================================================

rm(list=ls())

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(zoo)
library(lubridate)
library(shinythemes)
library(DT)

# set up ------------------------------------------
source("functions\\functions_for_shiny_app.R")

end_date <- Sys.Date()
start_date <- floor_date(Sys.Date(), unit = "year")


# load data ---------------------------------------
## historical data ----
load("historical_data/historic_visit_counts.Rda")

## ytd data ----
ytd_file <- file.info(list.files(path = "ytd_data",
                    pattern = "*.Rda",
                    full.names = T)) %>%
  data.frame(.) %>%
  rownames_to_column("file") %>%
  select(file, ctime) %>%
  arrange(desc(ctime)) %>%
  slice(1) %>%
  pull(file)
load(ytd_file)

## conditions ----
cond_ct <- historical_visit_counts_all_vars %>%
  mutate(cond_ct = str_count(ccdd_all, "\\,")+1) %>%
  summarize(cond_ct = max(cond_ct)) %>%
  pull(cond_ct)

conditions <- historical_visit_counts_all_vars %>%
  select(ccdd_all) %>%
  separate(ccdd_all, paste("ccdd_", 1:cond_ct, sep = ""), sep = ", ", fill = "right") %>%
  pivot_longer(names_to = "var",
               values_to = "cond",
               values_drop_na = T,
               cols = matches("ccdd_")) %>%
  distinct(cond) %>%
  arrange(cond) %>%
  pull(.)

## timeframes ----
timeframes <- historical_visit_counts_all_vars %>%
  distinct(timeframe) %>%
  arrange(timeframe) %>%
  pull(timeframe)

# prep data --------------------------------------
## get visit counts per day ----
ytd_visit_counts_day <- ytd_visit_counts_all_vars %>%
  group_by(date, ccdd_all) %>%
  summarize(ytd_total = sum(total), 
            .groups = "keep") %>%
  ungroup() %>%
  mutate(weekday = weekdays(date),
         weekday_num = wday(date))

## get visit counts per week ----
ytd_visit_counts_week <- ytd_visit_counts_all_vars %>%
  group_by(epi_week, ccdd_all) %>%
  summarize(ytd_total = sum(total),
            .groups = "keep") %>%
  ungroup()

all_visit_counts <- historical_visit_counts_all_vars %>%
  group_by(epi_week, timeframe, ccdd_all) %>%
  summarize(hist_total = sum(total),
            .groups = "keep") %>%
  ungroup() %>%
  complete(epi_week, timeframe, ccdd_all) %>%
  mutate(hist_total = case_when(is.na(hist_total) ~ 0, TRUE ~ hist_total)) %>%
  full_join(ytd_visit_counts_week, by = c("epi_week", "ccdd_all")) %>%
  select(epi_week, timeframe, ccdd_all, matches("total")) %>%
  mutate(across(where(is.integer), ~case_when(!is.na(.x) ~ .x, 
                                              is.na(.x) ~ as.integer(0)))) 

# server ----------------------------------
server <-  function(input, output) {
  
  ## tab 1: all disease summary ----
  ### make datasets ----
  data_t1_charts <- reactive({
    ytd_visit_counts_day %>%
    #  filter(date <= Sys.Date() & date %in% input$time_slider[1]:input$time_slider[2])
      filter(input$time_slider[1] <= date & date <= input$time_slider[2])
    
  })
  
  ### make charts ----
  output$graph_t1_mh <- renderPlotly({
    data_t1_charts() %>%
      t1_plot_fun(., "Mental Health", "#FCA50AFF") %>%
      layout(title = "Mental Health",
             xaxis = list(title = "Visit Date"),
             yaxis = list(title = "Visit Count"))
  })
  
  output$graph_t1_anx_depress <- renderPlotly({
    
    anx <- t1_plot_fun(data_t1_charts(), "Anxiety", "#38939B")
    depress <- t1_plot_fun(data_t1_charts(), "Depressive", "#38939B")
    suicide <- t1_plot_fun(data_t1_charts(), "Suicide Related", "#38939B")
    
    subplot(anx, depress, suicide, nrows = 1, shareX = TRUE, shareY = TRUE, titleX = FALSE) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Visit Count"),
             showlegend = F,
             annotations = list(
               list(x = 0.16, y = 1, text = "Anxiety", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE),
               list(x = 0.5, y = 1, text = "Depressive Disorders", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE),
               list(x = 0.85, y = 1, text = "Suicide Related", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE)
             ))
    
  })
  
  output$graph_t1_drug <- renderPlotly({
    
    all_drug <- t1_plot_fun(data_t1_charts(), "All Drug", "#A12A63FF")
    opioid <- t1_plot_fun(data_t1_charts(), "Opioid Overdose", "#A12A63FF")
    
    subplot(all_drug, opioid, nrows = 1, shareX = TRUE, shareY = TRUE) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Visit Count"),
             showlegend = F,
             annotations = list(
               list(x = 0.25, y = 1, text = "All Drug", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE),
               list(x = 0.75, y = 1, text = "Opioid Overdose", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE)
             ))
    
  })
  
  output$graph_t1_schiz_traum <- renderPlotly({
    
    schizo <- t1_plot_fun(data_t1_charts(), "Schizophrenia Spectrum Disorders", "#56106EFF")
    trauma <- t1_plot_fun(data_t1_charts(), "Trauma and Stressor-related Disorders", "#56106EFF")
    
    subplot(schizo, trauma, nrows = 1, shareX = TRUE, shareY = TRUE) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Visit Count"),
             showlegend = F,
             annotations = list(
               list(x = 0.25, y = 1, text = "Schizophrenia Spectrum Disorders", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE),
               list(x = 0.75, y = 1, text = "Trauma/Stressor-related Disorders", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE)
             ))
    
  })
  
  
  ## tab 2: selected diseases over time ----
  ### make datasets ----
  data_t2_timeseries <- reactive({
    
    all_visit_counts %>%
      filter(str_detect(ccdd_all, paste(c(input$dis_check), collapse = "|")) & 
               timeframe %in% c(input$timeframes)) %>%
      group_by(epi_week, timeframe) %>%
      summarize(hist_total = sum(hist_total),
                ytd_total = sum(ytd_total), 
                .groups = "keep") %>%
      ungroup() %>%
      mutate(years = as.numeric(str_extract(timeframe, "\\d+")),
             hist_avg = hist_total/years, 
             ytd_total = case_when(epiweek(Sys.Date()-6) >= epi_week ~ ytd_total)) %>%
      rowwise() %>%
      group_by(timeframe) %>%
      arrange(epi_week) %>%
      mutate(ytd_roll_5w_mean = rollmean(ytd_total, k = 5, na.pad = T, align = "center"),
             hist_roll_5w_mean = rollmean(hist_avg, k = 5, na.pad = T, align = "center")) %>%
      ungroup()
  
  })

  
  data_t2_weekdays <- reactive({
    ytd_visit_counts_day %>%
      filter(str_detect(ccdd_all, paste(c(input$dis_check), collapse = "|"))) %>%
      group_by(weekday, weekday_num)  %>%
      summarize(ytd_per_day = sum(ytd_total), 
                .groups = "keep") %>%
      ungroup() %>%
      mutate(ytd_total = sum(ytd_per_day)) %>%
      rowwise() %>%
      mutate(pct = round(ytd_per_day/ytd_total*100, 1)) %>%
      ungroup() 
  })
  
  data_t2_hospitals <- reactive({
    ytd_visit_counts_all_vars %>%
      filter(str_detect(ccdd_all, paste(c(input$dis_check), collapse = "|"))) %>%
      group_by(hospital_name) %>% 
      summarize(total = sum(total), 
                .groups = "keep") %>%
      ungroup() 
  })
  
  ### make charts ----
    output$graph_t2_timeseries <- renderPlotly({
    
      data_t2_timeseries() %>%
        plot_ly() %>%
      add_trace(x = ~epi_week, y = ~hist_roll_5w_mean, 
                color = ~timeframe, 
                colors = "Greys", 
                mode = "lines", type = "scatter") %>%
      
      add_trace(x = ~epi_week, y = ~ytd_roll_5w_mean, name = "5-Week Rolling Average", mode = "lines", type = "scatter", 
                line = list(color = "#17202A", width = 4,
                            dash = "dot")) %>%
      
      add_trace(x = ~epi_week, y = ~ytd_total, name = "Weekly Visit Count", mode = "lines", type = "scatter",
                line = list(color = "#FFCE00", width = 4)) %>%
      
      layout(xaxis = list(title = "Epi Week"),
             yaxis = list(title = "Number of Visits"),
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           y = -0.2))
    
  })

 
  output$graph_t2_weekdays <- renderPlotly({
    data_t2_weekdays() %>%
      arrange(weekday_num) %>%
      plot_ly() %>%
      add_trace(x = ~weekday, y = ~pct, color = ~weekday, type = "bar") %>% 
         #       marker = list(color = c("#006cb6", "#ffce00", "#fa8d29", 
          #                              "#a65a95", "#009d4e", "#38939b", "#d12a29"))) %>%
      layout(title = "Percent of Visits by Day",
             xaxis = list(title = "",
                          categoryorder = "array", 
                          categoryarray = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
             yaxis = list(title = ""))
    
  })
  
  output$graph_t2_hospitals <- renderPlotly({
    data_t2_hospitals() %>%
      plot_ly() %>%
      add_trace(x = ~total, y = ~reorder(hospital_name, total), type = "bar", orientation = "h", marker = list(color = "#38939B")) %>%
      layout(title = "Visits by Hospital",
             xaxis = list(title = "Number of Visits"),
             yaxis = list(title = ""))
  })
  
  
  
  ## tab 3: demographic datasets ----
  ### make datasets ----
  data_t3_demographics <- reactive ({
    ytd_visit_counts_all_vars %>%
      filter(str_detect(ccdd_all, paste(c(input$dis_select), collapse = "|"))) %>%
      mutate(n = case_when(epiweek(Sys.Date()) >= epi_week ~ total))
  })


  ### make charts ----
  output$graph_t3_sex <- renderPlotly({
    
    color_map <- c(Female="#1C494D", Male="#38939B", Other="#B3B3B3")
    data_t3_demographics() %>%
      group_by(epi_week, sex) %>%
      summarize(n = sum(total),
                .groups = "keep") %>%
      ungroup() %>% 
      plot_ly()%>%
      add_trace(x=~epi_week, y=~n, color=~sex, colors=color_map, type="bar")%>%
      layout(barmode="stack",
             xaxis = list(title = "Epi Week"),
             yaxis = list(title = "Visit Count"),
             title = "Sex")
  })
  
  output$graph_t3_age <- renderPlotly({
    
    data_t3_demographics() %>%
      group_by(epi_week, age_group) %>%
      summarize(n = sum(total),
                .groups = "keep") %>%
      ungroup() %>% 
      plot_ly()%>%
      add_trace(x=~epi_week, y=~n, color=~age_group, colors=c("#668C4A", "#A6BF4B", "#F2F0D5", "#F2C53D", "#F99C32"), type="bar")%>%
      layout(barmode="stack",
             xaxis = list(title = "Epi Week"),
             yaxis = list(title = "Visit Count"),
             title = "Age")
  })
  
  
  output$graph_t3_race <- renderPlotly({
    
    data_t3_demographics() %>%
      group_by(epi_week, race_cat) %>%
      summarize(n = sum(total),
                .groups = "keep") %>%
      ungroup() %>% 
      plot_ly()%>%
      add_trace(x=~epi_week, y=~n, color=~race_cat, colors=c("#6B007B", "#810F7C", "#8856A7", "#8C96C6", "#9EBCDA", "#BFD3E6", "#B3B3B3", "#056CB6"), type="bar")%>%
      layout(barmode="stack",
             xaxis = list(title = "Epi Week"),
             yaxis = list(title = "Visit Count"),
             title = "Race/Ethnicity")
  })
  
  
}

# ui ----------------------------------------
disease_checkbox <- checkboxGroupInput(inputId = "dis_check",
                                       label = "Disease group: ",
                                       choices = conditions,
                                       #inline = T,
                                       selected = "Mental Health")
denom_select <- selectInput(inputId = "timeframes",
                            label = "Comparison Timeframe: ",
                            choices = timeframes,
                            selected = "last_1_year",
                            multiple = T)

time_slider <- sliderInput(inputId = "time_slider",
                           label = "Date Range: ",
                           min = as.Date(start_date),
                           max = as.Date(end_date),
                           value = c(as.Date(start_date), as.Date(end_date)))

disease_select <-selectInput(inputId = "dis_select",
                             label = "Disease Group:",
                             choices = conditions,
                             selected = "Anxiety")

# assemble the UI ---------------------------------------------
header <- dashboardHeader(title = HTML("ESSENCE Mental Health Emergency Department Visits"),
                          disable = F,
                          titleWidth = 600)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'sidebar',
    style = "position: relative; overflow: visible",
    menuItem("Overview Report", tabName = "overview", icon = icon("table")),
    div(id = "overview",
        conditionalPanel('input.sidebar === "overview"',
                         time_slider)),
    menuItem("Disease Specific", tabName = "dis_spec", icon = icon("table")),
    div(id = 'dis_spec',
        conditionalPanel('input.sidebar === "dis_spec"',
                         disease_checkbox,
                         denom_select)),
    menuItem("Demographics", tabName = "demos", icon = icon("table")),
    div(id = 'demos',
        conditionalPanel('input.sidebar === "demos"',
                         disease_select))
  )
)

body <- dashboardBody(
  tags$style(HTML(".main-sidebar .sidebar .irs-grid-text {color: white}",
                  ".irs-grid-pol {background:white}")),
  tags$style(".small-box.bg-green { background-color: #FFCE00 !important; color: #000000 !important; }"),
  
  tabItems(
    tabItem(tabName = "overview",
            h2("Overview of Year to Date Emergency Department Visits by Disease Condition"),
            br(),
            h3("Any Mental Health Related Visit"),
            fluidRow(plotlyOutput(outputId = "graph_t1_mh", width = "100%")),
            br(),
            h3("Anxiety, Depression or Suicide Related"),
            fluidRow(plotlyOutput(outputId = "graph_t1_anx_depress", width = "100%")),
            br(),
            h3("Substance Related"),
            fluidRow(plotlyOutput(outputId = "graph_t1_drug", width = "100%")),
            br(), 
            h3("Other Conditions"), 
            fluidRow(plotlyOutput(outputId = "graph_t1_schiz_traum")),
            div("Each visit is counted once per condition. If a person went to the emergency department for both All Drug and Mental Health, that visit would be counted in both the All Drug and Mental Health charts."),
            div("The rolling averages displayed here are an average of the first day and the next 20 days. Ex: the data point on January 1 is an average of the data points from January 1-21. Rolling averages are used to smooth out day to day variation and better identify trends in data.")
    ),
    
    tabItem(tabName = "dis_spec",
            h2("Disorder Specific"),
            fluidRow(plotlyOutput(outputId = "graph_t2_timeseries",
                                  width = "100%")),
            br(),
            fluidRow(splitLayout(plotlyOutput(outputId =  "graph_t2_weekdays"),
                                 plotlyOutput(outputId =  "graph_t2_hospitals"))),
            br(),
            div("An epidemiological week, commonly referred to as an epi week or a CDC week, is simply a standardized method of counting weeks to allow for the comparison of data year after year. Each epi week begins on a Sunday and ends on a Saturday.")
    ),
    tabItem(tabName = "demos",
            h2("Year-to-Date Emergency Room Visits by Disorder & Demographics"),
            fluidRow(splitLayout(plotlyOutput(outputId = "graph_t3_age", width="100%"))),
            br(),
            fluidRow(splitLayout(plotlyOutput(outputId = "graph_t3_sex", width="100%"),
                                 plotlyOutput(outputId = "graph_t3_race", width="100%"))),
            br(),
            div("An epidemiological week, commonly referred to as an epi week or a CDC week, is simply a standardized method of counting weeks to allow for the comparison of data year after year. Each epi week begins on a Sunday and ends on a Saturday.")
    )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

shinyApp(ui = ui, server = server)
