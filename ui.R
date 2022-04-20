# This is a Shiny web application for IMPIT indices. You can run the application by clicking
# the 'Run App' button above.
# Manuela M.

# packages
library(shinydashboard) # which version of this 
library(dashboardthemes)
library(shinydashboardPlus)
library(shinyFiles)
library(shinyhelper)
library(tidyverse)
library(DT)

# source
source("./source/mydetect_event.R")
source("./source/mydetect_timfoc.R")
source("./source/fun_IMPIT.R")
source("./source/fun_getdates.R")
source("./source/fun_overlap.R")
source("./source/fun_nu2.R")
source("./source/fun_nu3.R")
source("./source/fun_w1.R")
source("./source/fun_w2.R")
source("./source/fun_w3.R")


# Define UI for app
ui <- dashboardPage(
  
  ###
  ### Header
  ###
  dashboardHeader(
    # changing logo
    title = shinyDashboardLogo(
      theme = "blue_gradient",
      boldText = "IMPIT",
      mainText = "app",
      badgeText = "v1.0"
    )
  ),
  
  ###
  ### Sidebar content
  ###
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data", tabName = "data", icon = icon("database")
      ),
      menuItem("Episodes", tabName = "episodes", icon = icon("crosshairs")),
      menuItem("Index", tabName = "index", icon = icon("chart-line")),
      menuItem("Application", tabName = "application", icon = icon("brain")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  
  ###
  ### Body content
  ###
  dashboardBody(
    
    # changing theme
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    
    # contents
    tabItems(
      
      #
      # Home tab ==========
      #
      tabItem(
        tabName = "home",
        h2("IMPIT web-app"),
        
        fluidRow(
          box(
            width = 12,
            title = "Description",
            solidHeader = F,
            h5("An intuitive R-Shiny app that simplifies the construction of IMPIT indices. This app combines the analytic framework developed in (IMPIT paper, 2022) with the visual power offered by R."),
            h5(" IMPIT tool has a friendly workflow for importing raw data, exploring, defining and computing basic episodes reports and constructing IMPIT indices."),
            h5(" The user can choose between a menu of intensity and relative weight functions."),
            h5(" The results are indices and their corresponding visualization with the aim of constituiting a useful data analysis exploratory tool.")
          )
        ),
        fillRow(
          
          box(
            width = 12,
            title = "Workflow",
            align = "center", 
            solidHeader = F,
            img(src = "images/roadmap.png",style="width: 80%"))
        )
      ),
      
      #
      # Data tab ==========
      #
      tabItem(
        tabName = "data",
        # data-upload row 
        fluidRow(
          box(
            title = "Required Data", 
            status="purple",
            id = "box_source_data",
            collapsible = TRUE,
            icon = icon("upload"),
            width = 3,
            height = "500px",
            helper(fileInput("csv_input","Select CSV File to Import",accept=".csv", placeholder = "ex: soi_monthly.csv"),
                   icon = "question", 
                   size = "m",
                   title = "Data format", 
                   type = "markdown", 
                   content = "source_data_help")
          ),
          box(
            title = "Check data",
            status="purple",
            id = "box_table_data",
            collapsible = TRUE,
            icon = icon("file-import"),
            width = 8,
            height = "500px",
            div(DT::dataTableOutput("contents_data"), style = "font-size: 100%; width: 100%")
          )),
        # descriptives row
        fluidRow(
          box(
            title = "Controls",
            status="purple",
            id = "box_controls_data",
            collapsible = TRUE,
            icon = icon("cog"),
            width = 3,
            height = "500px",
            selectInput("time_var","Time Variable",choices=c("Not selected")),
            selectInput("env_var","Environmental Variable",choices=c("Not selected")),
            dateRangeInput("daterange", "Date range for plot (yyyy-mm-dd)", start="2000-01-01", end="2020-01-01"),
            actionButton("run_button_data", "Plot", icon=icon("play"))
          ),
          box(
            title = "Plot", 
            status="purple",
            id = "box_plot_data",
            collapsible = TRUE,
            icon = icon("chart-line"),
            width = 8,
            height = "500px",
            plotOutput("envPlot", height = 275)
          )
        )
      ),
      
      #
      # Episodes tab ==========
      #
      tabItem(
        tabName = "episodes",
        fluidRow(
          box(
            title = "List of episodes", 
            status="navy",
            id = "box_list_epi",
            collapsible = TRUE,
            icon = icon("cogs"),
            width = 4,
            height = "500px",
            checkboxGroupInput("choice_epifile", "You can generate or upload the list of episodes:", choices=list("generate?"=1, "upload?"=2), selected=NULL),
            conditionalPanel(
              condition = "input.choice_epifile == '2'",
              fileInput("epifile_input","Select CSV or RData File to Import",accept=c(".csv",".RData"))
            ),
            conditionalPanel(
              condition = "input.choice_epifile == '1'",
              checkboxGroupInput("choice_thres", "Observations which exceed, or fall below specified threshold:", choices=list("up-episodes?"=1, "down-episodes?"=2), selected = NULL),
              fluidRow(column(4, numericInput('thres', 'Threshold:', 8, min = -Inf, max = Inf))),
              fluidRow(column(4, numericInput('duration_min', 'Minimum duration:', 1, min = 1, max = Inf)))
            )
          ),
          box(
            title = "Set up", 
            status="navy",
            id = "box_setup_epi",
            collapsible = TRUE,
            icon = icon("cogs"),
            width = 4,
            height = "500px",
            checkboxGroupInput("unit_var", "Time units", choices=list("days"=1, "months"=2, "years"=3), selected=NULL),
            checkboxInput("choice_timfoc", "Timing", value = FALSE),
            conditionalPanel(
              condition = "input.choice_timfoc",
              strong("When does the special season start?"),
              br(),
              fluidRow(column(4, selectInput("start_timfoc_day", "", 
                                             choices=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16",
                                                       "17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"),
                                             selected = "1")
                              ),
                       column(4, selectInput("start_timfoc_month", "", 
                                             choices=c("January","February","March","April","May","June","July",
                                                       "August","September","October","November","December"),
                                             selected = "January")
                              )
              ),
              br(),
              strong("When does the special season end?"),
              br(),
              fluidRow(column(4, selectInput("end_timfoc_day", "", 
                                             choices=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16",
                                                       "17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"),
                                             selected = "1")
                              ),
                       column(4, selectInput("end_timfoc_month", "", 
                                             choices=c("January","February","March","April","May","June","July",
                                                       "August","September","October","November","December"),
                                             selected = "January")
                              )
              )
            ),
            actionButton("run_button_epi", "Run", icon=icon("play"))
          )
        ),
        fluidRow(
          box(
            title = "Episodes",
            status="navy",
            id = "box_table_epi",
            collapsible = TRUE,
            icon = icon("tasks"),
            width = 11,
            height = "250px",
            tabsetPanel(
              tabPanel("Table", 
                       fluidRow(column(10, DT::dataTableOutput("contents_epi")),
                                column( 3, downloadButton("downloadTable_epi", "Download Table"), style = "margin-top: 25px;")
                       )
              ),
              tabPanel("Plot", 
                       fluidRow(column(10, plotOutput("plot_epi")),
                                column( 3,  downloadButton("downloadPlot_epi", "Download Plot"), style = "margin-top: 25px;")
                       )
              )
            ) 
          )
        )
      ),
      
      #
      # Index tab ==========
      #
      tabItem(
        tabName = "index",
        fluidRow(
          # title = "Set up", 
          # id = "box_setup_index",
          # collapsible = TRUE,
          # icon = icon("user-cog"),
          # width = 3,
          # height = "600px",
          column(width = 3,
                 box(
                   title="Set up", width=NULL, status="primary", icon = icon("user-cog"), solidHeader=TRUE,
                 box(
                   title="Index period", width=NULL, status="primary", collapsible = TRUE, collapsed=TRUE,
                   dateRangeInput("daterange_index", "Date range (yyyy-mm-dd):", start="1900-01-01", end="2020-01-01")
                 ),
                 box(
                   title="Memory", width=NULL, status="primary", collapsible = TRUE, collapsed=TRUE,
                   numericInput("m", label = "Years: ", min = 1, max = 40, value = 1)
                 ),
                 box(
                   title="Intensity", width=NULL, status="primary", collapsible = TRUE, collapsed=TRUE,
                   selectInput("choice_intensity", "", choices = c("mean","median","min","max","log"), selected = "mean")
                 ),
                 box(
                   title="Weights", width=NULL, status="primary", collapsible = TRUE, collapsed=TRUE,
                   sliderInput('a_w1', label = 'Persistence (a)', min = 1, max = 5, value = 2),
                   sliderInput("b_w2", label = "Recency (b)", min = 0, max = 5, value = 0),
                   sliderInput("c_w2", label = "Recency (c)", min = 0, max = 1, value = 0),
                   conditionalPanel(
                     condition = "input.choice_timfoc",
                     h5("Timing weight"),
                     numericInput("d_w3", label = "Parameter d", min = 0, max = 15, value = 1)
                   )
                 ),
                 actionButton("run_button_index", "Compute index", icon=icon("play")) 
                
                 )
          ),
          column(width=8,
                 box(
                   title = "IMPIT index",
                   width = NULL,
                   status="primary",
                   id = "box_table_epi",
                   collapsible = TRUE,
                   icon = icon("tasks"),
                   height = "250px",
                   solidHeader=TRUE,
                   tabsetPanel(
                     tabPanel("Table", 
                              fluidRow(column(10, div(DT::dataTableOutput("contents_index"), style = "font-size: 100%; width: 50%")),
                                       column( 3, downloadButton("downloadTable_index", "Download Table"), style = "margin-top: 25px;")
                              )
                     ),
                     tabPanel("Plot", 
                              fluidRow(column(10, plotOutput("plot_index", width = "100%")),
                                       column( 3,  downloadButton("downloadPlot_index", "Download Plot"), style = "margin-top: 25px;")
                              )
                     )
                   )
                 )
            )
          )
         

      ),
      
      #
      # Application tab ==========
      #
      tabItem(
        tabName = "application",
        
        fluidRow(
          
          box(
            title = "Source IMPIT index",
            status="success",
            solidHeader = TRUE,
            id = "box_app_data_index",
            collapsible = TRUE,
            icon = icon("upload"),
            width = 4,
            height = "500px",
            fileInput("index_file","Select CSV to Import", accept=c(".csv",".RData")),
            selectInput("index_time_var","Time Variable", choices=c("Not selected")),
            selectInput("index_var","Variable", choices=c("Not selected")),
            #sliderInput("index_periodInput", "Period", 1950, 2020, c(1988, 2019), pre = "year "),
            #dateRangeInput("daterange_index_app", "Date range (yyyy-mm-dd):", start="1950-01-01", end="2022-01-01"),
            actionButton("run_button_app_index", "Plot", icon=icon("play"))
          ),
          box(
            title = "Explore trend in IMPIT index",
            status="success",
            id = "box_app_data_index_exp",
            collapsible = TRUE,
            icon = icon("chart"),
            width = 8,
            height = "500px",
            tabsetPanel(
              tabPanel("Table",
                       div(DT::dataTableOutput("contents_app_index"), style = "font-size: 100%; width: 100%")
                       ),
              tabPanel("Plot",
                       plotOutput("plot_app_index"),
                       br(),
                       downloadButton("downloadPlot_app_index", "Download Plot"), style = "margin-top: 25px;"
                       )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Source response",
            status="success",
            solidHeader = TRUE,
            id = "box_app_data_resp",
            collapsible = TRUE,
            icon = icon("upload"),
            width = 4,
            height = "500px",
            fileInput("resp_file","Select CSV to Import", accept=c(".csv",".RData")),
            selectInput("resp_time_var","Time Variable", choices=c("Not selected")),
            selectInput("resp_var","Response Variable", choices=c("Not selected")),
            #sliderInput("resp_periodInput", "Period", 1950, 2020, c(1988, 2019), pre = "year "),
            actionButton("run_button_application", "Run analysis", icon=icon("play"))
          ),

          box(
            title = "Explore association between IMPIT index and response variable",
            status="success",
            id = "box_app_data",
            collapsible = TRUE,
            icon = icon("chart"),
            width = 8,
            height = "700px",
            tabsetPanel(
              tabPanel("Table",
                       div(DT::dataTableOutput("contents_app_resp"), style = "font-size: 100%; width: 50%")
                       ),
              tabPanel("Plot",
                       plotOutput("plot_app_resp"),
                       br(),
                       downloadButton("downloadPlot_app_resp", "Download Plot")
                       ),
              tabPanel("Correlation analysis",
                     fluidRow(column(10, plotOutput("plot_corr_application", width = "100%")),
                              column( 3,  downloadButton("downloadPlot_app", "Download Plot"), style = "margin-top: 25px;")
                     )
              )
            )
          )
        )
      ),
      
      
      # sixth tab content
      tabItem(
        tabName = "about",
        h4("Created with R Shiny"),
        br(),
        "2022 March",
        
        fluidRow(
          box(
            width = 12,
            title = "About us",
            solidHeader = F
            # userDescription(
            #   title = "Manuela Mendiolar",
            #   subtitle = "lead Developer",
            #   type = 2,
            #   image = "images/UQ_Manuela_.jpg",
            # )
            
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Rsession",
            solidHeader = F
            
            )
        )
        
      )
    )
  )
)


