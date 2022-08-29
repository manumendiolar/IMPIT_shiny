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
library(spsComps)
library(shinyjs)

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

source("./source/subtract_mem.R")
source("./source/is_convertible_to_date.R")
source("./source/fun_IMPITv2.R")
source("./source/gather_timfoc_dates.R")

steps <- read_csv("./helpfiles/help.csv", show_col_types = FALSE)
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
    
    # tags$head(
    #   tags$link(
    #     rel = "stylesheet",
    #     type = "text/css",
    #     href = "IMPITa_style.css")
    #   ),

    useShinyalert(force = TRUE),
    useShinyjs(),
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
              tabPanel("Plot", plotlyOutput("envPlot", height = 500)),
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
            title = div(icon("cogs", verify_fa = FALSE), strong("Set up")), 
            id = "box_list_epi",
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            width = 12,
            height = "500px",
            
            # Generate / Upload episodes 
            column(width = 4,
                   radioButtons("choice_epifile", "List of episodes:", choices = list("generate"=1,"upload"=2)),
                   actionButton("run_button_epi", "Compute", icon=icon("paper-plane"))
                   ),
            column(width = 4,
                   conditionalPanel(
                     condition = "input.choice_epifile == '2'",
                     helper(
                       fileInput("epifile_input","Select CSV to Import", accept=".csv", placeholder="ex: soi_episodes.csv"),
                       content = "source_epi_upload_help",
                       title = "Episodes data format",
                       icon = "question", 
                       size = "l",
                       type = "markdown"
                       )
                     ),
                   conditionalPanel(
                     condition = "input.choice_epifile == '1'",
                     helper(
                       radioButtons("choice_thres", "Type of episodes:", choices = list("up-episodes"=1, "down-episodes"=2)),
                       content = "help_type_episodes",
                       title = "Type of episodes",
                       icon = "question",
                       size = "l",
                       type = "markdown"
                       ),
                     fluidRow(
                       column(5,
                              helper(
                                numericInput('thres', 'Threshold', 8, min = -Inf, max = Inf),
                                content = "help_threshold",
                                title = "Threshold",
                                icon = "question",
                                size = "m",
                                type = "markdown"
                                )
                              )
                       ),
                     fluidRow(
                       column(5,
                              helper(
                                numericInput('duration_min','Minimum duration', 1, min = 1, max = Inf),
                                content = "help_min_duration",
                                title = "Minimum duration",
                                icon = "question",
                                size = "m",
                                type = "markdown"
                                )
                              )
                       )
                     )
                   ),
            # Time units
            # column(width = 3,
            #        helper(
            #          radioButtons("unit_var", "Episodes time units", choices=list("days"=1,"months"=2,"years"=3)),
            #          icon = "question",
            #          size = "m",
            #          title = "Episodes time units",
            #          type = "markdown",
            #          content = "help_epi_units"
            #          )
            #        ),
            # Timing
            column(width = 4,
                   helper(
                     radioButtons("choice_timfoc", "Episodes overlapping special season", choices=list("yes"=1,"no"=2)),
                     content = "help_special_season",
                     title = "Special season",
                     icon = "question",
                     size = "m",
                     type = "markdown"
                     ),
                   conditionalPanel(
                     condition = "input.choice_timfoc == '1'",
                     fluidRow(
                       column(5, selectInput("start_timfoc_month", "Start: Month", choices=choices_months, selected=choices_months[1])),
                       column(3, selectInput("start_timfoc_day", "Start: Day", choices=choices_days, selected=choices_days[1]))
                       ),
                     fluidRow(
                       column(5, selectInput("end_timfoc_month", "End: Month", choices=choices_months, selected=choices_months[2])),
                       column(3, selectInput("end_timfoc_day", "End: Day", choices=choices_days, selected=choices_days[1]))
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
              title = div(icon("user-cog",verify_fa = FALSE), strong("Set up")), 
              width = NULL, 
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = FALSE,
              status = "info",
              # Memory
              helper(
                numericInput("m", label = "Memory", min=1, max=Inf, value=1),
                content = "help_set_up_index_m",
                title = "Memory",
                icon = "question",
                size = "m",
                type = "markdown"
                ),
              # Intensity
              helper(
                selectInput("choice_intensity", "Intensity", choices=c("mean","median","min","max","log")),
                content = "help_set_up_index_intensity",
                title = "Intensity",
                icon = "question",
                size = "m",
                type = "markdown"
                ),
              # 
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
                content = "help_set_up_index_a",
                title = "Persistence",
                icon = "question",
                size = "m",
                type = "markdown"
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
                content = "help_set_up_index_b_and_c",
                title = "Recency: b and c",
                icon = "question",
                size = "m",
                type = "markdown"
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
                  content = "help_set_up_index_d",
                  title = "Timing",
                  icon = "question",
                  size = "m",
                  type = "markdown"
                  )
                ),
              br(),
              # Period time Index
              helper(
                #dateRangeInput("daterange_index", p(h5(strong("Index period:")), style = "margin-top: 30px;"), start="1900-01-01", end="2020-01-01"),
                dateRangeInput("daterange_index", h5(strong("Index time domain")), start="1900-01-01", end="2020-01-01"),
                content = "help_set_up_index_time_range",
                title = "Index time range",
                icon = "question",
                size = "m",
                type = "markdown"
                ),
              radioButtons("choice_index_unit", label = h5(strong("Index time units:")), choices = list("years"=1, "months"=2, "days"=3)),
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
              tabsetPanel(
                tabPanel("Plot",
                         fluidRow(
                           column(11, plotlyOutput("plot_index", width = "100%")),
                           column( 3, downloadButton("downloadPlot_index", "Download Plot"), style="margin-top: 25px;")
                         )
                ),
                tabPanel("Table",
                         fluidRow(
                           column(12, div(DT::dataTableOutput("contents_index"), style = "font-size: 100%; width: 100%")),
                           column( 3, downloadButton("downloadTable_index", "Download Table"), style = "margin-top: 25px;")
                           )
                         )
                )
              )
            )
          )
        ),
      
      
      
      # APPLICATION TAB ---------------------------------------------------------
      
      tabItem(
        tabName = "application",
        h4("Select CSV File to import and explore"),
        # Explore IMPIT index trend row
        fluidRow(
          # Upload data
          box(
            title = div(icon("upload"), strong("Upload files")),
            id = "box_app_data_index",
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            width = 3,
            height = "500px",
            
            helper(
              fileInput("index_file","IMPIT index", accept = ".csv", placeholder = "ex: impit_index.csv"),
              content = "help_source_index",
              title = "Data format",
              icon = "question",
              size = "m",
              type = "markdown"
              ),
            helper(
              fileInput("resp_file","Response variable", accept = ".csv", placeholder = "ex: resp_variable.csv"),
              content = "help_source_resp",
              title = "Data format",
              icon = "question",
              size = "m",
              type = "markdown"
            )
            ),
          # Explore 
          box(
            title = div(icon("magnifying-glass"), strong("Explore")),
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = FALSE,
            status = "info",
            width = 9,
            height = "500px",
            tabsetPanel(
              tabPanel("Plot: IMPIT index",
                       fluidRow(column(10, plotlyOutput("plot_app_index", width = "100%"))),
                       fluidRow(column( 3, downloadButton("downloadPlot_app_index", "Download"), style="margin-top: 25px;"))
                       ),
              tabPanel("Table: IMPIT index",
                       div(DT::dataTableOutput("contents_app_index"), style="font-size: 100%; width: 100%")
                       ),
              tabPanel("Plot: Response",
                       fluidRow(column(10, plotlyOutput("plot_app_resp", width = "100%"))),
                       fluidRow(column( 3, downloadButton("downloadPlot_app_resp", "Download"), style="margin-top: 25px;"))
              ),
              tabPanel("Table: Response", div(DT::dataTableOutput("contents_app_resp"), style="font-size: 100%; width: 100%")),
              tabPanel("Correlation analysis",
                       fluidRow(column(10, plotlyOutput("plot_corr_application", width="90%"))),
                       fluidRow(column( 3, downloadButton("downloadPlot_app", "Download"), style="margin-top: 25px;"))
              ),
              tabPanel("Summary", verbatimTextOutput("summary"))
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



