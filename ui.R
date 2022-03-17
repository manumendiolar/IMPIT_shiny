# This is a Shiny web application for IMPIT indices. You can run the application by clicking
# the 'Run App' button above.
# Manuela M.

# packages
library(shinydashboard)
library(dashboardthemes)
library(shinydashboardPlus)
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
  
  ### Header
  dashboardHeader(
    # changing logo
    title = shinyDashboardLogo(
      theme = "blue_gradient",
      boldText = "IMPIT",
      mainText = "app",
      badgeText = "v1.1"
    )
  ),
  
  ### Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data", tabName = "data", icon = icon("database"),
               startExpanded = FALSE,
               menuSubItem("Upload", tabName = "upload_data", icon = icon("file-upload")),
               menuSubItem("Descriptives", tabName = "descriptives_data", icon = icon("chart-bar"))
      ),
      menuItem("Episodes", tabName = "episodes", icon = icon("crosshairs")),
      menuItem("Index", tabName = "index", icon = icon("chart-line")),
      menuItem("Application", tabName = "application", icon = icon("brain")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  
  ### Body content
  dashboardBody(
    
    # changing theme
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    
    # contents
    tabItems(
      
      # Home tab =====
      tabItem(
        tabName = "home",
        h2("IMPIT web-app"),
        
        fluidRow(
          box(
            width = 12,
            title = "Description",
            solidHeader = F,
            h5("An intuitive R-Shiny web-app that simplifies the construction of IMPIT indices. This app combines the analytic framework developed in (IMPIT paper, 2022) with the visual power offered by R."),
            h5(" IMPIT tool has a friendly workflow for importing raw data, exploring, defining and computing basic episodes reports and constructing IMPIT indices."),
            h5(" The user can choose between a menu of intensity and relative weight functions, as well as the freedom to define user functions."),
            h5(" The results are indices and their corresponding visualization with the aim of constituiting a useful data analysis exploratory tool.")
          )
        ),
        fillRow(
          
          box(
            width = 12,
            title = "Workflow",
            align = "center", 
            solidHeader = F,
            img(src = "roadmap.png",style="width: 75%"))
        )
      ),
      
      # Data tab =====
      # data-upload 
      tabItem(
        tabName = "upload_data",
        fluidRow(
          box(
            title = "Source",
            id = "box_source_data",
            collapsible = TRUE,
            icon = icon("upload"),
            width = 4,
            height = "500px",
            fileInput("csv_input","Select CSV File to Import",accept=".csv")
          ),
          box(
            title = "Data",
            id = "box_table_data",
            collapsible = TRUE,
            icon = icon("file-import"),
            width = 10,
            height = "500px",
            div(DT::dataTableOutput("contents_data"), style = "font-size: 100%; width: 100%")
          )
        )
      ),
      # data-descriptives
      tabItem(
        tabName = "descriptives_data",
        fluidRow(
          box(
            title = "Plot", 
            id = "box_plot_data",
            collapsible = TRUE,
            icon = icon("chart-line"),
            width = 7,
            height = "500px",
            plotOutput("envPlot", height = 250)
          ),
          box(
            title = "Controls",
            id = "box_controls_data",
            collapsible = TRUE,
            icon = icon("cog"),
            width = 4,
            height = "500px",
            selectInput("time_var","Time Variable",choices=c("Not selected")),
            selectInput("env_var","Environmental Variable",choices=c("Not selected")),
            dateRangeInput("daterange", "Date range for plot (yyyy-mm-dd)", start="2000-01-01", end="2020-01-01"),
            actionButton("run_button_data", "Plot", icon=icon("play"))
          )
        )
      ),
      
      # Episodes tab =====
      tabItem(
        tabName = "episodes",
        fluidRow(
          box(
            title = "Set up", 
            id = "box_setup_epi",
            collapsible = TRUE,
            icon = icon("cogs"),
            width = 4,
            height = "500px",
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
            actionButton("run_button_epi", "Compute episodes", icon=icon("play"))
          ),
          box(
            title = "Episodes",
            id = "box_table_epi",
            collapsible = TRUE,
            icon = icon("tasks"),
            width = 10,
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
      
      # Index tab =====
      tabItem(
        tabName = "index",
        fluidRow(
          box(
            title = "Set up", 
            id = "box_setup_index",
            collapsible = TRUE,
            icon = icon("user-cog"),
            width = 10,
            height = "600px",
            box(
              title = "Intensity", width = 3, 
              selectInput("choice_intensity", "Intensity", choices = c("mean","min","max","log"), selected = "mean")
            ),
            box(
              title = "Weight", width = 3, 
              sliderInput('a_w1', label = 'Persistence (a)', min = 1, max = 5, value = 2),
              sliderInput("b_w2", label = "Recency (b)", min = 0, max = 5, value = 0),
              sliderInput("c_w2", label = "Recency (c)", min = 0, max = 1, value = 0),
              conditionalPanel(
                condition = "input.choice_timfoc",
                sliderInput("d_w3", label = "Timing focus (d)", min = 0, max = 15, value = 2)
              )
            ),
            box(
              title = "Memory", width = 3, 
              numericInput("m", label = "in years", min = 1, max = 40, value = 1)
            ),
            box(
              title = "Index period", width = 3, height = "500px",
              br(),
              div(style="display:inline-block",numericInput("period_index_start", label="From", value=1980)),
              div(style="display:inline-block",numericInput("period_index_end", label="To", value = 2019))
            )
          ),
          box(
            width = 2, 
            actionButton("run_button_index", "Compute index", icon=icon("play"))
          ),
          box(
            title = "IMPIT index",
            id = "box_table_epi",
            collapsible = TRUE,
            icon = icon("tasks"),
            width = 10,
            height = "250px",
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
      ),
      
      # Application tab =====
      tabItem(
        tabName = "application",
        
        fluidRow(
          
          box(
            title = "Source response",
            id = "box_app_data_resp",
            collapsible = TRUE,
            icon = icon("upload"),
            width = 4,
            height = "500px",
            fileInput("resp_file","Select CSV to Import", accept=c(".csv",".RData")),
            selectInput("resp_time_var","Time Variable", choices=c("Not selected")),
            selectInput("resp_var","Response Variable", choices=c("Not selected")),
            sliderInput("resp_periodInput", "Period", 1950, 2020, c(1988, 2019), pre = "year "),
            actionButton("run_button_app_resp", "Plot response", icon=icon("play"))
          ),
          box(
            title = "Source index",
            id = "box_app_data_index",
            collapsible = TRUE,
            icon = icon("upload"),
            width = 4,
            height = "500px",
            fileInput("index_file","Select CSV to Import", accept=c(".csv",".RData")),
            selectInput("index_time_var","Time Variable", choices=c("Not selected")),
            selectInput("index_var","Variable", choices=c("Not selected")),
            sliderInput("index_periodInput", "Period", 1950, 2020, c(1988, 2019), pre = "year "),
            actionButton("run_button_app_index", "Plot index", icon=icon("play"))
          ),
          box(
            title = "Check inputs",
            id = "box_app_data_index_exp",
            collapsible = TRUE,
            icon = icon("chart"),
            width = 8,
            #height = "500px",
            tabsetPanel(
              tabPanel("Table",
                       fluidRow(column(10, div(DT::dataTableOutput("contents_app_resp"), style = "font-size: 100%; width: 50%"),
                                       br(),
                                       br(),
                                       div(DT::dataTableOutput("contents_app_index"), style = "font-size: 100%; width: 50%")
                       ),
                       column( 3, downloadButton("downloadTable_app_resp", "Download Table"), style = "margin-top: 25px;")
                       )
              ),
              tabPanel("Plot",
                       fluidRow(column(10, plotOutput("plot_app_resp"),
                                       plotOutput("plot_app_index")
                       ),
                       column( 3,  downloadButton("downloadPlot_app_resp", "Download Plot"), style = "margin-top: 25px;")
                       )
              )
            )
          ),
          box(
            title = "Application",
            id = "box_app_data",
            collapsible = TRUE,
            icon = icon("upload"),
            width = 4,
            #height = "500px",
            actionButton("run_button_application", "Run analysis", icon=icon("play")),
            tabPanel("Analysis",
                     fluidRow(column(10, plotOutput("plot_corr_application", width = "40%")),
                              column( 3,  downloadButton("downloadPlot_app", "Download Plot"), style = "margin-top: 25px;")
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
        "2022 January"
      )
    )
  )
)

