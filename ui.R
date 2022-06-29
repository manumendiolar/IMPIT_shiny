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
library(plotly)


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


steps <- read_csv("./helpfiles/help.csv")
choices_days <- as.character(1:31)
choices_months <- c("January","February","March","April","May","June","July","August","September","October","November","December") 



# Define UI for app
ui <- dashboardPage(
  
  title = "IMPITa",
   
  
  # HEADER ------------------------------------------------------------------

  dashboardHeader(
    
    title = span(img(src = "images/icon_explore_2.svg", height = 30), "IMPIT-a"),
    
    dropdownMenu(
      type = "notifications", 
      headerText = strong("HELP"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      notificationItem(
        text = steps$text[1],
        icon = icon("database")
      ),
      notificationItem(
        text = steps$text[2],
        icon = icon("crosshairs")
      ),
      notificationItem(
        text = steps$text[3],
        icon = icon("chart-line")
      ),
      notificationItem(
        text = steps$text[4],
        icon = icon("brain")
      ),
      notificationItem(
        text = strong("Important info"),
        icon = icon("exclamation")
      )
    )
  ),
  
  
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Episodes", tabName = "episodes", icon = icon("crosshairs")),
      menuItem("Index", tabName = "index", icon = icon("chart-line")),
      menuItem("Application", tabName = "application", icon = icon("brain")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  
  
  # BODY --------------------------------------------------------------------
  
  dashboardBody(
    
    # changing theme
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "IMPITa_style.css")
    ),

    
    # MAIN BODY ---------------------------------------------------------------
    
    tabItems(
      
      
      # HOME TAB ----------------------------------------------------------------
      
      tabItem(
        tabName = "home",
        h2("IMPIT app"),
        br(),
        fluidRow(
          # Description
          box(
            width = 12, 
            height = "30em",
            title = h4(tags$b("Description")),
            solidHeader = F,
            collapsible = T,
            tags$ul(
              tags$li(h4("Provide a user-friendly interface for constructing IMPIT indices.")),
              tags$li(h4("Provide a smooth workflow from importing raw data, exploring and defining episodes, to constructing IMPIT indices.")),
              tags$li(h4("Allow users to choose between a menu of intensity and relative weight functions.")), 
              tags$li(h4("Visualize imported data, defined episodes, and constructed IMPIT indices"))
            )
            #br(), br(), br(), br(), br(), br(),
          ),
          # Roadmap
          box(
            width = 12,
            title = h4(tags$b("Roadmap")),
            align = "center", 
            solidHeader = F,
            collapsible = T,
            img(src = "images/IMPIT_roadmap.svg", style="width: 65%")
          )
        )
      ),
      
      
      
      # DATA TAB ----------------------------------------------------------------
     
      tabItem(
        tabName = "data",
        fluidRow(
          
          # Import data
          box(
            title = "Required Data", 
            id = "box_source_data",
            collapsible = T,
            icon = icon("upload"),
            width = 3,
            height = "500px",
            helper(fileInput("csv_input","Select CSV File to Import",
                             accept=".csv", 
                             placeholder = "ex: soi_monthly.csv"),
                   icon = "question", 
                   size = "m",
                   #colour = "black",
                   title = "Data format", 
                   type = "markdown", 
                   content = "source_data_help"),
            br(),
            br(),
            div(DT::dataTableOutput("contents_data"), style = "font-size: 100%; width: 100%")
            ),
          
          # Check data 
          box(
            title = "Check data",
            id = "box_table_data",
            collapsible = TRUE,
            icon = icon("file-import"),
            width = 8,
            height = "500px",
            plotlyOutput("envPlot", height = 700)
            )
        )
      ),
        

      
      # EPISODES TAB ------------------------------------------------------------
      
      tabItem(
        tabName = "episodes",
        
        # Set up episodes
        fluidRow(
          
          box(
            title = "Set up", 
            id = "box_list_epi",
            collapsible = TRUE,
            icon = icon("cogs"),
            width = 11,
            height = "500px",
            
            # Generate / Upload episodes 
            column(width = 4,
              checkboxGroupInput("choice_epifile", "You can generate or upload the list of episodes:", 
                                 choices=list("generate?"=1, "upload?"=2), 
                                 selected=NULL),
              conditionalPanel(
                condition = "input.choice_epifile == '2'",
                helper(
                  fileInput("epifile_input","Select CSV or RData File to Import",
                            accept = ".csv", 
                            placeholder = "ex: soi_episodes.csv"
                            ),
                  icon = "question", 
                  size = "l",
                  title = "Episodes data format", 
                  type = "markdown", 
                  content = "source_epi_upload_help"
                  )
                ),
              conditionalPanel(
                condition = "input.choice_epifile == '1'",
                helper(
                  checkboxGroupInput("choice_thres", "Observations which exceed, or fall below specified threshold:", 
                                     choices=list("up-episodes?"=1, "down-episodes?"=2), 
                                     selected = NULL),
                  icon = "exclamation",
                  size = "l",
                  title = "Episodes file generated format",
                  type = "markdown",
                  content = "source_epi_generate_help"
                  ),
                fluidRow(column(4, numericInput('thres', 'Threshold:', 8, min = -Inf, max = Inf))),
                fluidRow(column(4, numericInput('duration_min', 'Minimum duration:', 1, min = 1, max = Inf))),
                br(),
                br()
                ),
              actionButton("run_button_epi", "Submit", icon=icon("play"))
              ),
            # Time units
            column(width = 3,
              checkboxGroupInput("unit_var", "Time units", choices=list("days"=1,"months"=2,"years"=3), selected=NULL)
              ),
            # Timing
            column(width = 4,
              checkboxGroupInput("choice_timfoc", "Timing", choices=list("yes"=1,"no"=2), selected=NULL),
              conditionalPanel(
                condition = "input.choice_timfoc == '1'",
                strong("When does the special season start?"),
                br(),
                fluidRow(
                  column(4, selectInput("start_timfoc_day", "", choices=choices_days, selected=choices_days[1])),
                  column(4, selectInput("start_timfoc_month", "", choices=choices_months, selected=choices_months[1]))
                  ),
                br(),
                strong("When does the special season end?"),
                br(),
                fluidRow(
                  column(4, selectInput("end_timfoc_day", "", choices=choices_days, selected=choices_days[1])),
                  column(4, selectInput("end_timfoc_month", "", choices=choices_months, selected=choices_months[1]))
                  )
                )
              )
          )
        ),
        
        # Explore episodes
        fluidRow(
          box(
            title = "Episodes",
            id = "box_table_epi",
            collapsible = TRUE,
            icon = icon("tasks"),
            width = 11,
            height = "500px",
            tabsetPanel(
              # Episodes: table
              tabPanel("Table",
                 fluidRow(
                   column(11, div(DT::dataTableOutput("contents_epi"), style="font-size: 100%; width: 100%")),
                   column( 3, downloadButton("downloadTable_epi", "Download Table"), style="margin-top: 25px;")
                   )
                 ),
              # Episodes: plot
              tabPanel("Plot", 
                 fluidRow(
                   column(11, plotlyOutput("plot_epi", height=500)),
                   column( 3, downloadButton("downloadPlot_epi", "Download Plot"), style="margin-top: 25px;")
                   )
              )
            ) 
          )
        )
      ),
      
      
      # INDEX TAB ---------------------------------------------------------------
      
      tabItem(
        tabName = "index",
        
        fluidRow(
          
          # Set up index
          column(
            width = 3,
            box(
              title="Set up", 
              width=NULL, 
              #status="primary", 
              icon = icon("user-cog"), 
              solidHeader=TRUE,
              collapsible = T,
              helper(
                numericInput("m", label = "Memory ", min=1, max=40, value=1),
                icon = "question",
                size = "m",
                title = "Set up for constructing IMPIT index",
                type = "markdown",
                content = "set_up_index_help"
              ),
              selectInput("choice_intensity", "Intensity", choices=c("mean","median","min","max","log"), selected=NULL),
              h5(strong("Weights")),
              sliderInput('a_w1', label = 'Persistence (a)', min=0, max=5, value=2),
              sliderInput("b_w2", label = "Recency (b)", min=0, max=5, value=0, step = 0.01),
              sliderInput("c_w2", label = "Recency (c)", min=0, max=1, value=0),
              conditionalPanel(
                condition = "input.choice_timfoc == '1'",
                numericInput("d_w3", label = "Timing (d)", min=0.01, max=15, value=1)
              ),
              br(),
              dateRangeInput("daterange_index", "Index period (yyyy-mm-dd):", start="1900-01-01", end="2020-01-01"),
              br(),
              actionButton("run_button_index", "Compute", icon=icon("play"))
              )
            ),
          
          # IMPIT index: exploratory
          column(
            width = 8,
            box(
              title = "IMPIT index",
              width = NULL,
              #status="primary",
              id = "box_table_epi",
              collapsible = T,
              icon = icon("tasks"),
              height = "500px",
              solidHeader=T,
              plotlyOutput("plot_index", width = "100%"),
              br(),
              fluidRow(column(12, div(DT::dataTableOutput("contents_index"), style = "font-size: 100%; width: 100%")),
                       column( 3, downloadButton("downloadTable_index", "Download Table"), style = "margin-top: 25px;")
                       )
              # tabsetPanel(
              #   tabPanel("Table",
              #      fluidRow(column(12, div(DT::dataTableOutput("contents_index"), style = "font-size: 100%; width: 100%")),
              #               column( 3, downloadButton("downloadTable_index", "Download Table"), style = "margin-top: 25px;")
              #               )
              #      ),
              #   tabPanel("Plot", 
              #      fluidRow(column(12, plotlyOutput("plot_index", width = "100%")),
              #               column( 3,  downloadButton("downloadPlot_index", "Download Plot"), style = "margin-top: 25px;")
              #               )
              #      )
              #   )
              )
            )
          )
      ),
      
      
      
      # APPLICATION TAB ---------------------------------------------------------
      
      tabItem(
        tabName = "application",
        
        # Explore IMPIT index trend row
        fluidRow(
          
          # Import IMPIT index values
          box(
            title = "Source IMPIT index",
            #status="success",
            solidHeader = T,
            id = "box_app_data_index",
            collapsible = T,
            icon = icon("upload"),
            width = 3,
            height = "500px",
            helper(fileInput("index_file","Select CSV File to Import",
                             accept=".csv",
                             placeholder = "ex: impit_index.csv"),
                   icon = "question",
                   size = "m",
                   title = "Data format",
                   type = "markdown",
                   content = "source_data_impit_help"),
            br(),
            br(),
            div(DT::dataTableOutput("contents_app_index"), style="font-size: 100%; width: 100%")
            #dateRangeInput("daterange_app_index", "Date range (yyyy-mm-dd):", start="1950-01-01", end="2022-01-01"),
            #actionButton("run_button_app_index", "Submit", icon=icon("play"))
          ),
          
          # IMPIT index table and plot 
          box(
            title = "Explore trend in IMPIT index",
            #status="success",
            #id = "box_app_data_index_exp",
            collapsible = T,
            icon = icon("chart"),
            width = 8,
            height = "500px",
            tabsetPanel(
             tabPanel("Plot",
               fluidRow(column(10, plotlyOutput("plot_app_index", width = "100%"))),
               fluidRow(column( 3, downloadButton("downloadPlot_app_index", "Download"), style="margin-top: 25px;"))
               )
             )
            )
          ),
        
        # Explore association between IMPIT index and other variable
        fluidRow(
           
          # upload other variable
          box(
            title = "Source response",
            #status="success",
            solidHeader = T,
            id = "box_app_data_resp",
            collapsible = T,
            icon = icon("upload"),
            width = 3,
            height = "500px",
            helper(fileInput("resp_file","Select CSV File to Import",
                             accept=".csv",
                             placeholder = "ex: resp_variable.csv"),
                   icon = "question",
                   size = "m",
                   title = "Data format",
                   type = "markdown",
                   content = "source_data_resp_help"),
            div(DT::dataTableOutput("contents_app_resp"), style="font-size: 100%; width: 100%")
            #dateRangeInput("daterange_app_resp", "Choose date range to explore association:", start="1950-01-01", end="2022-01-01"),
            #actionButton("run_button_app_resp", "Submit", icon=icon("play"))
            ),

          # table and plot of other variable (to check) and correlation analysis with IMPIT index
          box(
            title = "Explore association between IMPIT index and response variable",
            #status="success",
            collapsible = T,
            icon = icon("chart"),
            width = 8,
            height = "500px",
            tabsetPanel(
              #tabPanel("Table", div(DT::dataTableOutput("contents_app_resp"), style="font-size: 100%; width: 100%")),
              tabPanel("Plot",
                       fluidRow(column(10, plotlyOutput("plot_app_resp", width = "100%"))),
                       fluidRow(column( 3, downloadButton("downloadPlot_app_resp", "Download"), style="margin-top: 25px;"))
              ),
              tabPanel("Correlation analysis",
                       fluidRow(column(10, plotlyOutput("plot_corr_application", width="100%"))),
                       fluidRow(column( 3, downloadButton("downloadPlot_app", "Download"), style="margin-top: 25px;"))
              ),
              tabPanel("Summary",
                       verbatimTextOutput("summary"))
            )
          )
        )
      ),
      
      
      # ABOUT US TAB ------------------------------------------------------------
     
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
  ) #close dashboardBody
) #close dashboardPage


