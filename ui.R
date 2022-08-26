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
library(shinyalert)
library(shinyvalidate)
library(validate)

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
      )#,
      # notificationItem(
      #   text = strong("Important info"),
      #   icon = icon("exclamation")
      # )
    )
  ),
  
  
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Episodes", tabName = "episodes", icon = icon("crosshairs")),
      menuItem("Index", tabName = "index", icon = icon("chart-line")),
      menuItem("Application", tabName = "application", icon = icon("brain")),
      menuItem("About", tabName = "about", icon = icon("info-circle",verify_fa = FALSE))
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

    useShinyalert(force = TRUE),
    
    # MAIN BODY ---------------------------------------------------------------
    
    tabItems(
      
      
      # HOME TAB ----------------------------------------------------------------
      
      tabItem(
        tabName = "home",
        h4(em("Web Application for IMPIT index construction"), style = "font-family: monospace;"),
        br(),
        fluidRow(
          # Description
          box(
            width = 12, 
            height = "30em",
            title = div(icon("house"), strong("Description")),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            tags$ul(
              tags$li(h4("Provide a user-friendly interface for constructing IMPIT indices.")),
              tags$li(h4("Provide a smooth workflow from importing raw data, exploring and defining episodes, to constructing IMPIT indices.")),
              tags$li(h4("Allow users to choose between a menu of intensity and relative weight functions.")), 
              tags$li(h4("Visualize imported data, defined episodes, and constructed IMPIT indices"))
              )
            ),
          # Roadmap
          box(
            width = 12,
            title = div(icon("map-location-dot"), strong("Roadmap")),
            align = "center", 
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            img(src = "images/IMPIT_roadmap.svg", style="width: 65%")
            )
          )
        ),
      
      
      
      # DATA TAB ----------------------------------------------------------------
     
      tabItem(
        tabName = "data",
        h4("Use this tab if you want to generate episodes using this app"),
        br(),
        fluidRow(
          # Import data
          box(
            title = div(icon("upload"), strong("Required Data")), 
            id = "box_source_data",
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            width = 3,
            height = "500px",
            helper(
              fileInput("csv_input","Select CSV File to Import", accept = ".csv", placeholder = "ex: soi_monthly.csv"),
              icon = "question", 
              size = "m",
              title = "Data format", 
              type = "markdown", 
              content = "source_data_help"
              )
            ),
          
          # Check data 
          box(
            title = div(icon("file-import"), strong("Check data")),
            id = "box_table_data",
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            width = 9,
            height = "500px",
            tabsetPanel(
              tabPanel("Plot", plotlyOutput("envPlot", height = 700)),
              tabPanel("Table", div(DT::dataTableOutput("contents_data"), style = "font-size: 100%; width: 100%")),
              tabPanel("Summary", verbatimTextOutput("summary_contents_data")),
              tabPanel("str()", verbatimTextOutput("str_contents_data"))
              )
            )
          )
        ),
      
      
      
      # EPISODES TAB ------------------------------------------------------------
      
      tabItem(
        tabName = "episodes",
        
        # Set up episodes
        fluidRow(
          
          box(
            title = div(icon("cogs"), strong("Set up")), 
            id = "box_list_epi",
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            width = 12,
            height = "500px",
            
            # Generate / Upload episodes 
            column(width = 4,
                   radioButtons("choice_epifile",
                                "You can generate or upload the list of episodes:", 
                                choices = list("generate"=1,"upload"=2)
                                ),
                   conditionalPanel(
                     condition = "input.choice_epifile == '2'",
                     helper(
                       fileInput("epifile_input","Select CSV to Import", accept=".csv", placeholder="ex: soi_episodes.csv"),
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
                       radioButtons("choice_thres",
                                    "Observations which exceed, or fall below a threshold:",
                                    choices = list("up-episodes" = 1, "down-episodes" = 2)
                                    ),
                       icon = "question",
                       size = "l",
                       title = "Format of generated episodes file",
                       type = "markdown",
                       content = "source_epi_generate_help"
                       ),
                     fluidRow(column(5, 
                                     helper(
                                       numericInput('thres', 'Threshold:', 8, min = -Inf, max = Inf),
                                       icon = "question",
                                       size = "m",
                                       title = "Threshold",
                                       type = "markdown",
                                       content = "help_threshold"
                                     ))),
                     fluidRow(column(5, 
                                     helper(
                                       numericInput('duration_min','Minimum duration:', 1, min = 1, max = Inf),
                                       #verbatimTextOutput("value_duration_min"),
                                       icon = "question",
                                       size = "m",
                                       title = "Minimum duration",
                                       type = "markdown",
                                       content = "help_min_duration"
                                     )))
                     ),
                   actionButton("run_button_epi", "Compute", icon=icon("play"))
                   ),
            # Time units
            column(width = 3,
                   helper(
                     radioButtons("unit_var", "Episodes time units", choices=list("days"=1,"months"=2,"years"=3)),
                     icon = "question",
                     size = "m",
                     title = "Episodes time units",
                     type = "markdown",
                     content = "help_epi_units"
                     )
                   ),
            # Timing
            column(width = 4,
                   helper(
                     radioButtons("choice_timfoc", "Episodes overlapping special season", choices=list("yes"=1,"no"=2), selected=NULL),
                     icon = "question",
                     size = "m",
                     title = "Episodes time units",
                     type = "markdown",
                     content = "help_epi_units"
                   ),
                   conditionalPanel(
                     condition = "input.choice_timfoc == '1'",
                     strong("Special season start"),
                     fluidRow(
                       column(5, selectInput("start_timfoc_month", "", choices=choices_months, selected=choices_months[1])),
                       column(3, selectInput("start_timfoc_day", "", choices=choices_days, selected=choices_days[1]))
                       ),
                     strong("Special season end"),
                     fluidRow(
                       column(5, selectInput("end_timfoc_month", "", choices=choices_months, selected=choices_months[2])),
                       column(3, selectInput("end_timfoc_day", "", choices=choices_days, selected=choices_days[1])),
                       )
                     )
                   )
            )
          ),
        
        # Explore episodes
        fluidRow(
          box(
            title = div(icon("list-check"), strong("Episodes")),
            id = "box_table_epi",
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            width = 12,
            height = "500px",
            tabsetPanel(
              # Episodes: table
              tabPanel("Table",
                       fluidRow(
                         column(12, div(DT::dataTableOutput("contents_epi"), style="font-size: 100%; width: 100%")),
                         column( 3, downloadButton("downloadTable_epi", "Download Table"), style="margin-top: 25px;")
                         )
                       ),
              # Episodes: plot
              tabPanel("Plot: Intensity",
                       fluidRow(
                         column(11, plotlyOutput("plot_epi_intensity", height=500)),
                         column( 3, downloadButton("downloadPlot_epi_intensity", "Download Plot"), style="margin-top: 25px;")
                         )
                       ),
              # Episodes: plot
              tabPanel("Plot: Duration",
                       fluidRow(
                         column(11, plotlyOutput("plot_epi_duration", height=500)),
                         column( 3, downloadButton("downloadPlot_epi_duration", "Download Plot"), style="margin-top: 25px;")
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
              title = div(icon("user-cog"), strong("Set up")), 
              width = NULL, 
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = FALSE,
              status = "info",
              # Memory
              helper(
                numericInput("m", label = "Memory", min=1, max=Inf, value=1),
                icon = "question",
                size = "m",
                title = "Memory",
                type = "markdown",
                content = "set_up_index_m_help"
                ),
              # Intensity
              helper(
                selectInput("choice_intensity", "Intensity", choices=c("mean","median","min","max","log"), selected=NULL),
                icon = "question",
                size = "m",
                title = "Intensity",
                type = "markdown",
                content = "set_up_index_intensity_help"
                ),
              p(h5(strong("Weights")), style = "margin-top: 20px;"),
              # Weight: Persistence
              h5(em(strong("Persitence"))),
              helper(
                fluidRow(
                tags$head(
                  tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; }
                              #inline .form-group { display: table-row;}")
                ),
                tags$div(id = "inline", column(5, numericInput("a_w1", label = 'a:', min=0, max=Inf, value=2, step = 0.01)))
                ),
                icon = "question",
                size = "m",
                title = "Persistence",
                type = "markdown",
                content = "set_up_index_a_help"
                ),
              # Weight: Recency
              h5(em(strong("Recency"))),
              helper(
                fluidRow(
                  tags$head(
                    tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle}
                              #inline .form-group { display: table-row;}")
                    ),
                  tags$div(id = "inline", column(5, numericInput("b_w2", label = 'b:', min=0, max=Inf, value=1.75, step = 0.01))),
                  tags$div(id = "inline", column(5, numericInput("c_w2", label = 'c:', min=0, max=1, value=0.25, step = 0.01)))
                  ),
                icon = "question",
                size = "m",
                title = "Recency: b and c",
                type = "markdown",
                content = "set_up_index_b_help"
                ),
              # Weight: Timing
              conditionalPanel(
                condition = "input.choice_timfoc == '1'",
                h5(em(strong("Timing"))),
                helper(
                  fluidRow(
                    tags$head(
                      tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; }
                              #inline .form-group { display: table-row;}")
                    ),
                    tags$div(id = "inline", column(5, numericInput("d_w3", label = 'd:', min=0, max=Inf, value=1, step = 0.01)))
                  ),
                  icon = "question",
                  size = "m",
                  title = "Timing",
                  type = "markdown",
                  content = "set_up_index_d_help"
                  )
                ),
              # Period time Index
              helper(
                dateRangeInput("daterange_index", p(h5(strong("Index period (YYYY-MM-DD)")), style = "margin-top: 30px;"), start="1900-01-01", end="2020-01-01"),
                icon = "question",
                size = "m",
                title = "Set up for constructing IMPIT index",
                type = "markdown",
                content = "set_up_index_range_help"
                ),
              radioButtons("choice_index_unit", h5(strong("Display:")), choices = list("annually?"=1, "monthly?"=2, "daily?"=3)),
              actionButton("run_button_index", "Compute", icon=icon("paper-plane"))
              ),
            ),
          
          
          # IMPIT index: exploratory
          column(
            width = 9,
            box(
              title = div(icon("chart-line"), strong("IMPIT index")),
              width = NULL,
              id = "box_table_epi",
              solidHeader = FALSE,
               collapsible = TRUE,
              collapsed = FALSE,
              status = "info",
              height = "500px",
              plotlyOutput("plot_index", width = "100%"),
              br(),
              fluidRow(
                column(12, div(DT::dataTableOutput("contents_index"), style = "font-size: 100%; width: 100%")),
                column( 3, downloadButton("downloadTable_index", "Download Table"), style = "margin-top: 25px;")
                )
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
            title = div(icon("upload"), strong("Source IMPIT index")),
            id = "box_app_data_index",
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            width = 3,
            height = "500px",
            helper(
              fileInput("index_file","Select CSV File to Import", accept = ".csv", placeholder = "ex: impit_index.csv"),
              icon = "question",
              size = "m",
              title = "Data format",
              type = "markdown",
              content = "source_data_impit_help"
              ),
            div(DT::dataTableOutput("contents_app_index"), style="font-size: 100%; width: 100%")
            ),
          
          # IMPIT index plot 
          box(
            title = div(icon("magnifying-glass"), strong("Explore trend in IMPIT index")),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            width = 9,
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
            title = div(icon("upload"), strong("Source response")),
            id = "box_app_data_resp",
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            width = 3,
            height = "500px",
            helper(
              fileInput("resp_file","Select CSV File to Import", accept = ".csv", placeholder = "ex: resp_variable.csv"),
              icon = "question",
              size = "m",
              title = "Data format",
              type = "markdown",
              content = "source_data_resp_help"
              ),
            div(DT::dataTableOutput("contents_app_resp"), style="font-size: 100%; width: 100%")
            ),
          
          # table and plot of other variable (to check) and correlation analysis with IMPIT index
          box(
            title = div(icon("magnifying-glass", lib = "font-awesome"), strong("Explore association between IMPIT index and response variable")),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            width = 9,
            height = "500px",
            tabsetPanel(
              tabPanel("Plot",
                       fluidRow(column(10, plotlyOutput("plot_app_resp", width = "100%"))),
                       fluidRow(column( 3, downloadButton("downloadPlot_app_resp", "Download"), style="margin-top: 25px;"))
                       ),
              tabPanel("Correlation analysis",
                       fluidRow(column(10, plotlyOutput("plot_corr_application", width="90%"))),
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
        "2022 August",
        br(),
        br(),
        fluidRow(
          box(
            width = 12,
            title = strong("About us"),#div(icon("user"), strong("About us")),
            collapsible = TRUE,
            collapsed = FALSE,
            solidHeader = FALSE,
            status = "info",
            h5(strong("Contact")),
            h5("IMPIT-a is created and mantained by Manuela Mendiolar. Bugs reports and features requests can be communicated in two ways:"),
            uiOutput("githubissues"),
            uiOutput("mm_email"),
            br(),
            h5(strong("Source")),
            uiOutput("githublink")
            )
          ),
        fluidRow(
          box(
            width = 12,
            title = div(icon("jsfiddle"), strong("Rsession")),
            collapsible = TRUE,
            collapsed = FALSE,
            solidHeader = FALSE,
            status = "info",
            verbatimTextOutput("Rsession")
          )
          )
        )
      )
    ) #close dashboardBody
  ) #close dashboardPage



