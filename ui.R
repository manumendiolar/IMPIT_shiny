# This is a Shiny web application for IMPIT indices. You can run the application by clicking
# the 'Run App' button above.
# Manuela M.


# packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)


# pre defined var
not_sel <- "Not selected"


# Define UI for application
ui <- navbarPage(
  
  theme = shinytheme('united'), #cerulean
  
  title = "IMPIT indices",
  
  # Introduction
  tabPanel(
    title = "Introduction",
    # titlePanel("Uploading files"),
    # sidebarLayout(
    #   sidebarPanel(title = "Inputs",
    #                
    #                fileInput("csv_input","Select CSV File to Import",accept=".csv"),
    #                selectInput("time_var","Time Variable",choices=c(not_sel)),
    #                selectInput("env_var","Environmental Variable",choices=c(not_sel)),
    #                dateRangeInput("daterange", "Date range for plot (yyyy-mm-dd)", start="2000-01-01", end="2020-01-01"),
    #                actionButton("run_button_data", "Plot", icon=icon("play"))
    #   ),
    #   mainPanel(
    #     tabsetPanel(
    #       tabPanel("Table", div(DT::dataTableOutput("contents_data"), style = "font-size: 100%; width: 50%")),
    #       tabPanel("Plot", plotOutput("envPlot")), 
    #       tabPanel("Summary", verbatimTextOutput("envSummary"))
    #     )
    #   )
    # )
  ), 
  
  # main page
  tabPanel(
    title = "Data",
    titlePanel("Uploading files"),
    sidebarLayout(
      sidebarPanel(title = "Inputs",
                   
                   fileInput("csv_input","Select CSV File to Import",accept=".csv"),
                   selectInput("time_var","Time Variable",choices=c(not_sel)),
                   selectInput("env_var","Environmental Variable",choices=c(not_sel)),
                   dateRangeInput("daterange", "Date range for plot (yyyy-mm-dd)", start="2000-01-01", end="2020-01-01"),
                   actionButton("run_button_data", "Plot", icon=icon("play"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Table", div(DT::dataTableOutput("contents_data"), style = "font-size: 100%; width: 50%")),
          tabPanel("Plot", plotOutput("envPlot")), 
          tabPanel("Summary", verbatimTextOutput("envSummary"))
        )
      )
    )
  ), 
  
  # second page
  tabPanel(
    title = "Episodes",
    titlePanel("Compute episodes"),
    sidebarLayout(
      sidebarPanel(title = "EpiInputs",
                   
                   checkboxGroupInput("choice_epifile", "List of episodes", choices=list("generate"=1, "upload"=2), selected=NULL),
                   conditionalPanel(
                     condition = "input.choice_epifile == '2'",
                     fileInput("epifile_input","Select CSV or RData File to Import",accept=c(".csv",".RData"))
                   ),
                   conditionalPanel(
                     condition = "input.choice_epifile == '1'",
                     numericInput('thres', 'Threshold', 8, min = -Inf, max = Inf),
                     checkboxInput("thres_above", "Above", value = FALSE),
                     numericInput('duration_min', 'Minimum duration', 1, min = 1, max = Inf)
                   ),
                   checkboxGroupInput("unit_var", "Time units", choices=list("days"=1, "months"=2, "years"=3), selected=NULL),
                   checkboxInput("choice_timfoc", "Timing focus", value = FALSE),
                   conditionalPanel(
                     condition = "input.choice_timfoc",
                     strong("Starts"),
                     br(),
                     div(style="display:inline-block",textInput("start_timfoc_day", label="day", value=1)),
                     div(style="display:inline-block",textInput("start_timfoc_month", label="month", value = 5)),
                     div(style="display:inline-block",textInput("start_timfoc_year", label="year", value = 5)),
                     br(),
                     strong("Ends"),
                     br(),
                     div(style="display:inline-block",textInput("end_timfoc_day", label="day", value=1)),
                     div(style="display:inline-block",textInput("end_timfoc_month", label="month", value = 5)),
                     div(style="display:inline-block",textInput("end_timfoc_year", label="year", value = 5))
                   ),
                   actionButton("run_button_epi", "Run analysis", icon=icon("play"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Table", DT::dataTableOutput("contents_epi")),
          tabPanel("Plot", plotOutput("plot_epi"))
        )
        
      )
    )
  ),
  
  
  # third page
  tabPanel(
    title = "Index",
    titlePanel("Compute index"),
    sidebarLayout(
      
      sidebarPanel(title = "SetUp",
                   
                   selectInput("choice_intensity", "Intensity", choices = c("mean","min","max","log"), selected = "mean"),
                   numericInput('a_w1', label = 'Persistence (a)', min = 1, max = 5, value = 2),
                   numericInput("b_w2", label = "Recency (b)", min = 0, max = 5, value = 0),
                   sliderInput("c_w2", label = "Recency (c)", min = 0, max = 1, value = 0),
                   conditionalPanel(
                     condition = "input.choice_timfoc",
                     sliderInput("d_w3", label = "Timing focus (d)", min = 0, max = 15, value = 2)
                   ),
                   numericInput("m", label = "Memory (years)", min = 1, max = 40, value = 1),
                   strong("Index period"),
                   br(),
                   div(style="display:inline-block",numericInput("period_index_start", label="from", value=1980)),
                   div(style="display:inline-block",numericInput("period_index_end", label="to", value = 2019)),
                   actionButton("run_button_index", "Plot", icon=icon("play"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Table", div(DT::dataTableOutput("contents_index"), style = "font-size: 100%; width: 50%")),
          tabPanel("Plot", plotOutput("plot_index", width = "100%"))
        )
        
      )
    )
  ),
  
  
  # fourth page
  tabPanel(
    title = "Application",
    titlePanel("Example"),
    sidebarLayout(
      
      sidebarPanel("Response variable",
                   br(),
                   br(),
                   fileInput("resp_file","Select CSV to Import", accept=c(".csv",".RData")),
                   selectInput("resp_time_var","Time Variable", choices=c(not_sel)),
                   selectInput("resp_var","Response Variable", choices=c(not_sel)),
                   sliderInput("periodInput", "Period", 1950, 2020, c(1988, 2019), pre = "year "),
                   actionButton("run_button_application", "Run analysis", icon=icon("play")),
                   br(),
                   br(),
                   downloadButton('downloadPlot_application', 'Download Plot')
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Table", div(DT::dataTableOutput("contents_application"), style = "font-size: 100%; width: 50%")),
          tabPanel("Plot", plotOutput("plot_resp_application")),
          tabPanel("Analysis", plotOutput("plot_corr_application", width = "50%"))
        )
      )
    )
  ),
  
  # about_page
  tabPanel(
    title = "About",
    titlePanel("About"),
    "Created with R Shiny",
    br(),
    "2021 December"
    
  )
)

