# This is a Shiny web application for IMPIT indices. You can run the application by clicking
# the 'Run App' button above.
# Manuela M.


# packages
library(shinydashboard)
library(dashboardthemes)
library(shinydashboardPlus)
library(shinyFiles)
library(shinyhelper)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(glue)
library(ggtext)
library(shinyjs)
library(plotly)
library(mgcv)
library(lubridate)
library(shinyalert)
library(shinyvalidate)
library(validate)
library(spsComps)

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
source("./source/fun_IMPITv2.R")

setBackgroundImage(src = NULL, shinydashboard = TRUE)

# Attach the folder where the 'QU-GENE' engine and QuLinePlus are stored.
IMPIT_base <- paste0(getwd(),"/")


choices_days <- as.character(1:31)
choices_months <- c("January","February","March","April","May","June","July","August","September","October","November","December") 


# Define server for app
server <- function(input, output, session) {
  
  
  # HELPFILES ---------------------------------------------------------------
  
  # uses 'helpfiles' directory by default
  # in this example, we use the withMathJax parameter to render formulae
  observe_helpers(session = getDefaultReactiveDomain(),
                  help_dir = paste0(IMPIT_base,"helpfiles"), 
                  withMathJax = TRUE)
  
  
  
  
  # HOME TAB ----------------------------------------------------------------
 
  output$myimage <- renderImage({
    # filename is ./www/myimage.jpeg
    filename <- normalizePath(file.path('./www',paste('myimage', input$n, '.jpeg', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  
  
  # DATA TAB ----------------------------------------------------------------
  
  # Import environmental data 
  data_input <- eventReactive(input$csv_input, 
    {
      #if (is.null(input$csv_input)) return(NULL)
      
      # check extension file
      if (tools::file_ext(input$csv_input$datapath) != "csv") {
        shinyalert("Invalid extension file", type = "error")
      } else {
        # read file
        aux <- read.csv(input$csv_input$datapath)
        # check format file
        if ( (dim(aux)[2] != 4) | 
             any(aux[ ,1:3]-floor(aux[ ,1:3]) != 0) |
             any(is.na(aux)) | 
             any(is.character(aux))|
             !is.numeric(aux[ ,4]) ) {
          shinyalert("Invalid format file", type = "error")
        } else {
          aux
        }
      }
    })
  
  # data_input(): table
  output$contents_data <- DT::renderDataTable({
    DT::datatable(data_input(),
                  options = list(
                    pageLength = 15,
                    lengthMenu = c(5,10,15,20,25,30,50,100), 
                    autoWidth = FALSE,  
                    searching = TRUE,
                    search = list(regex = TRUE, caseInsensitive = TRUE)
                    ),
                    rownames = FALSE) %>%
        formatRound(c(4),2)
  })
  
  # data_input(): plot
  output$envPlot <- renderPlotly({
    # xx with date format 
    xx <- as.Date(paste0(data_input()[ ,1],"-",data_input()[ ,2],"-",data_input()[ ,3]), format = "%Y-%m-%d")
    # yy environmental signal
    yy <- data_input()[ ,4]
    plot_ly( x = ~xx, y = ~yy) %>%
      add_lines() %>%
      layout(xaxis = list(title="Date", titlefont=list(size=12)),
             yaxis = list(title=colnames(data_input())[4], titlefont=list(size=12)))
  })
  
  # data_input(): summary
  output$summary_contents_data <- renderPrint({
    summary(data_input())
  })
  
  # data_input(): str()
  output$str_contents_data <- renderPrint({
    str(data_input())
  })
  
  
  
  
  # EPISODES TAB ------------------------------------------------------------
  
  state <- reactiveValues()
  
  # minimum duration: check valid entry
  observeEvent(input$duration_min, {
    if ( !is.integer(input$duration_min) | input$duration_min < 0 ){
      shinyalert(title = "Enter a positive integer as the minimum duration", type = "error")
    }
  })
  
  # special season: update fields
  months1 <- c("January","March","May","July","August","October","December") 
  months2 <- c("April","June","September","November") 
  months3 <- c("February") 
  # start
  observeEvent(input$start_timfoc_month,{
    # days options according to month
    if (input$start_timfoc_month %in% months1) updateSelectInput(session, "start_timfoc_day", choices = 1:31, selected = 1)
    if (input$start_timfoc_month %in% months2) updateSelectInput(session, "start_timfoc_day", choices = 1:30, selected = 1)
    if (input$start_timfoc_month %in% months3) updateSelectInput(session, "start_timfoc_day", choices = 1:28, selected = 1)
  })
  # end
  observeEvent(input$end_timfoc_month,{
    # days options according to month
    if (input$end_timfoc_month %in% months1) updateSelectInput(session, "end_timfoc_day", choices = 1:31, selected = 1)
    if (input$end_timfoc_month %in% months2) updateSelectInput(session, "end_timfoc_day", choices = 1:30, selected = 1)
    if (input$end_timfoc_month %in% months3) updateSelectInput(session, "end_timfoc_day", choices = 1:28, selected = 1)
  })
  
  # special season: check valid entries
  observeEvent(c(input$start_timfoc_month, input$start_timfoc_day, input$end_timfoc_month, input$end_timfoc_day), {
    
    months <- c("January","February","March","April","May","June","July","August","September","October","November","December")    
    
    # build start MM-DD
    ss_start <- paste0("2000-",which(months == input$start_timfoc_month),"-",input$start_timfoc_day)
    ss_start <- as.Date(ss_start)
    ss_end <- paste0("2000-",which(months == input$end_timfoc_month),"-",input$end_timfoc_day)
    ss_end <- as.Date(ss_end)

    if (!(ss_start < ss_end)) {
      shinyalert(title = "The end of the special season should be after the start", type = "error")
    }
  })
  
  # episode list: if upload... check file extension and format
  observeEvent(input$choice_epifile, {
    
    if (input$choice_epifile == '2'){
     
       observeEvent(input$epifile_input,{
        if (tools::file_ext(input$epifile_input$datapath) != "csv") {
          shinyjs::disable("run_button_epi")
          shinyCatch(stop("Invalid extension file"), blocking_level = "error")
        } else {
          # read file
          aux <- read.csv(input$epifile_input$datapath)
          # check format file
          conditions <- (dim(aux)[2]<6) | (!all(colnames(aux)[1:5] != c("event_no","duration","date_start","date_peak","date_end"))) #|
          (length(grep("intensity_", colnames(aux))) < 1) | (aux[ ,1:2]-floor(aux[ ,aux[ ,1:2]]) != 0) | (any(is.na(aux))) |
          (!is.Date(as.Date(aux[,3]))) | (!is.Date(as.Date(aux[,4]))) | (!is.Date(as.Date(aux[,5]))) | (!is.numeric(aux[ ,6:dim(aux)[2]]))
          if (conditions) {
            shinyjs::disable("run_button_epi")
            shinyCatch(stop("Invalid format file"), blocking_level = "error")
          } else {
            shinyjs::enable("run_button_epi")
          } 
        }
      })
    }
  })
  
  
  # depending on generate / upload episode file
  observeEvent(input$run_button_epi, {
    
    if (input$choice_epifile == '1'){
      
      # check for up or dwn episodes
      if (input$choice_thres == '1') thres_above <- TRUE
      if (input$choice_thres == '2') thres_above <- FALSE
      
      # compute episodes 
      state$episodes <- mydetect_event(data_input(), thres_above, input$thres, input$duration_min)
      
      # intensity choices
      state$choices_int <- c("intensity_mean","intensity_median","intensity_min","intensity_max","intensity_log")
    }
    
    if (input$choice_epifile == '2'){
     
      # load episode list 
      data_epi <- eventReactive(input$epifile_input,
         {
           if (is.null(input$epifile_input))  return(NULL)
           inFile <- input$epifile_input
           read.csv(inFile$datapath)
         })
      
      state$episodes <- as.data.frame(data_epi())
      if (state$episodes[1,1] != 0) {
        state$episodes$date_start <- as.Date(state$episodes$date_start)
        state$episodes$date_peak <- as.Date(state$episodes$date_peak)
        state$episodes$date_end <- as.Date(state$episodes$date_end)

        # update intensity choices
        state$choices_int <- grep("intensity_", colnames(state$episodes), value = TRUE)
        new_choices_int <- str_remove(state$choices_int, "intensity_")
        updateSelectInput(session, "choice_intensity",
                          label = "Intensity",
                          choices =  new_choices_int,
                          selected = head(new_choices_int, 1)
        )
      }
    }
    
    # special season
    if (input$choice_timfoc == '1'){

      # start date format
      ss_start_dd <- as.character(input$start_timfoc_day)
      ss_start_mm <- factor(input$start_timfoc_month, levels = choices_months)
      ss_start_mm <- as.integer(ss_start_mm)
      state$start_day <- ifelse(ss_start_dd %in% seq(1,9,1), paste0("0",ss_start_dd), paste0(ss_start_dd))
      state$start_month <- ifelse(ss_start_mm %in% seq(1,9,1), paste0("0",ss_start_mm), paste0(ss_start_mm))
      
      # end date format
      ss_end_dd <- as.character(input$end_timfoc_day)
      ss_end_mm <- factor(input$end_timfoc_month, levels = choices_months)
      ss_end_mm <- as.integer(ss_end_mm)
      state$end_day <- ifelse(ss_end_dd %in% seq(1,9,1), paste0("0",ss_end_dd), paste0(ss_end_dd))
      state$end_month <- ifelse(ss_end_mm %in% seq(1,9,1), paste0("0",ss_end_mm), paste0(ss_end_mm))
      
      # gather as a vector
      state$period_timfoc <- c(paste0(state$start_month,"-",state$start_day), paste0(state$end_month,"-",state$end_day))
      
      # update episodes
      state$episodes <- mydetect_timfoc(episodes = state$episodes, timfoc_dates = state$period_timfoc)
      
    }
    
  })
  
  
  # Episodes: table print
  output$contents_epi <- DT::renderDataTable({
    DT::datatable(state$episodes,
                  rownames = FALSE,
                  options = list(
                    pageLength = 10,
                    autoWidth = FALSE,
                    scrollX = TRUE,
                    searching = TRUE,
                    search = list(regex=TRUE, caseInsensitive=TRUE),
                    language = list(emptyTable = 'My Custom No Data Message')
                    )
                  ) %>%
      formatRound(c(6,7,8,9,10), 2) %>%
      formatStyle(columns=c(1:4), 'text-align'='centre')
  })

  # Episodes: table download .csv
  output$downloadTable_epi <- downloadHandler(
    filename = function() {
      paste0("Episodes_table.csv")
    },
    content = function(file) {
      write.csv(state$episodes, file, row.names = FALSE, col.names = TRUE)
    }
  )
  
  # Episodes: plot print
  output$plot_epi_intensity <- renderPlotly({
    
    if (state$episodes[1,1] != 0){
    x <- state$episodes$date_start
    y <- as.numeric(state$episodes[ , as.character(state$choices_int[1])])
    df <- data.frame(x,y)
    df$x <- as.Date(df$x)
    d1 <- as.Date(head(df$x,1))
    d2 <- as.Date(tail(df$x,1))
    
    if (input$choice_timfoc == '1'){
      
      df$z <- state$episodes$overlap
      df$z <- as.factor(df$z)
      lolliEp1 <- ggplot(df, aes(x, y, col = z)) +
        geom_segment( aes(x=x, xend=x, y=0, yend=y), alpha = 0.5) +
        geom_point(size=2) +
        labs(x=" ", y=" ", title=state$choices_int[1], col="Overlap")
      
      if (length(levels(df$z)) > 1) {
        levels(df$z) <- c("No","Yes")
        lolliEp1 <- lolliEp1 + scale_color_manual(values = c("black","orange"))
      } else {
        if (levels(df$z)[1] == "TRUE"){
          levels(df$z)[1] <- "Yes"
          levels(df$z)[2] <- "No"
          lolliEp1 <- lolliEp1 + scale_color_manual(values = c("orange","black"))
        } else {
          levels(df$z)[1] <- "No"
          levels(df$z)[2] <- "Yes"
          lolliEp1 <- lolliEp1 + scale_color_manual(values = c("black","orange"))
        }
      }
    } else {
      
      lolliEp1 <- ggplot(df, aes(x, y)) +
        geom_segment( aes(x=x, xend=x, y=0, yend=y), alpha = 0.5) +
        geom_point(color="black", size=2) +
        labs(x=" ", y=" ", title = state$choices_int[1]) 
    }
    
    state$plot_epi_intensity <- lolliEp1 +
      scale_x_date(breaks=seq(d1, d2, by="5 years"), limits=c(d1,d2), date_labels="%Y") +
      theme_light() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        title = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size= 9, angle=45, hjust=1),
        axis.text.y = element_text(size=9), 
        legend.position="right"
        )
    ggplotly(state$plot_epi_intensity)
    } else {plot_ly(type='scatter3d')}
  })
  
  output$plot_epi_duration <- renderPlotly({
    if (state$episodes[1,1] != 0){
    x <- state$episodes$date_start
    y <- as.numeric(state$episodes[ , "duration"])
    state$unit_var <- ifelse(input$unit_var=="1","days", ifelse(input$unit_var=="2", "months", "years"))
    df <- data.frame(x,y)
    df$x <- as.Date(df$x)
    d1 <- as.Date(head(df$x,1))
    d2 <- as.Date(tail(df$x,1))
    
    if (input$choice_timfoc == '1'){
      
      df$z <- state$episodes$overlap
      df$z <- as.factor(df$z)
      lolliEp2 <- ggplot(df, aes(x, y, col = z)) +
        geom_segment( aes(x=x, xend=x, y=0, yend=y), alpha = 0.5) +
        geom_point(size=2) +
        labs(x="date_start", y=paste0("[",state$unit_var,"]"), title="Duration", col="Overlap")
      
      if (length(levels(df$z)) > 1) {
        levels(df$z) <- c("No","Yes")
        lolliEp2 <- lolliEp2 + scale_color_manual(values = c("black","orange"))
      } else {
        if (levels(df$z)[1] == "TRUE"){
          levels(df$z)[1] <- "Yes"
          levels(df$z)[2] <- "No"
          lolliEp2 <- lolliEp2 + scale_color_manual(values = c("orange","black"))
        } else {
          levels(df$z)[1] <- "No"
          levels(df$z)[2] <- "Yes"
          lolliEp2 <- lolliEp2 + scale_color_manual(values = c("black","orange"))
        }
      }
    } else {
      
      lolliEp2 <- ggplot(df, aes(x, y)) +
        geom_segment( aes(x=x, xend=x, y=0, yend=y), alpha = 0.5) +
        geom_point(color="black", size=2) +
        labs(x="date_start", y=paste0("[",state$unit_var,"]"), title="Duration")
    }
    
    
    state$plot_epi_duration <- lolliEp2 +
      scale_x_date(breaks=seq(d1, d2, by="5 years"), limits=c(d1,d2), date_labels="%Y") +
      theme_light() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        title = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size= 9, angle=45, hjust=1),
        axis.text.y = element_text(size=9), 
        legend.position="right"
      )
  
    ggplotly(state$plot_epi_duration)
    } else {plot_ly(type='scatter3d')}
  })

    
  # Episodes: plot intensity download .png
  output$downloadPlot_epi_intensity<- downloadHandler(
    filename = function(){
      paste0("Episodes_LolliChart_intensity",'.png')
    },
    content = function(file){
      ggsave(file, plot = state$plot_epi_intensity, width = 26, height = 12, units = "cm", dpi = 300)
    }
  )
  
  # Episodes: plot intensity download .png
  output$downloadPlot_epi_duration <- downloadHandler(
    filename = function(){
      paste0("Episodes_LolliChart_duration",'.png')
    },
    content = function(file){
      ggsave(file, plot = state$plot_epi_duration, width = 26, height = 12, units = "cm", dpi = 300)
    }
  )

  # observeEvent(input$unit_var, {
  #   if (input$unit_var == '2'){
  #     updateRadioButtons(session,"choice_index_unit", choices = list("annually?"=1, "monthly?"=2))
  #   } else {
  #     if (input$unit_var == '3'){
  #       updateRadioButtons(session,"choice_index_unit", choices = list("annually?"=1))
  #      }
  #   }
  # })
    
    
    # # # Let's update index display unit choices
    # new_choices_index_unit2 <- list("annually?"=1, "monthly?"=2)
    # new_choices_index_unit1 <- list("annually?"=1)
    # 
    # if(input$unit_var == '2'){
    #   updateRadioButtons(
    #     session, "choice_index_unit",
    #     label = h5(strong("Display:")),
    #     choices =  new_choices_int2,
    #     selected = 1)
    # } else {
    #   if(input$unit_var == '3'){
    #     updateRadioButtons(
    #       session, "choice_index_unit",
    #       label = h5(strong("Display:")),
    #       choices =  new_choices_int1,
    #       selected = 1)
    #   }}
    
  #})
  
  
  # INDEX TAB ---------------------------------------------------------------

  
  observeEvent(input$run_button_index,{
    
    state$episodes$date_start <- as.Date(state$episodes$date_start)
    state$episodes$date_peak <- as.Date(state$episodes$date_peak)
    state$episodes$date_end <- as.Date(state$episodes$date_end)
    
    # observe({
    #   updateDateRangeInput(session, "daterange_index", 
    #                        start = as.Date(paste0(state$yr_first_epi + input$m,"-01-01")), 
    #                        end = "2020-01-01")
    # })
    
    # keep unit choice
    state$unit_var <- ifelse(input$unit_var == '1','days', ifelse(input$unit_var == '2', 'months', 'years'))
    

    # keep intensity choice
    state$intensity <- input$choice_intensity
    
    # build vector of dates (index period)
    # yr_index_start <- lubridate::year(as.Date(input$daterange_index[1]))
    # yr_index_end <- lubridate::year(as.Date(input$daterange_index[2]))
    # state$yrs_index <- seq(yr_index_start, yr_index_end, 1)
    
    # compute d and tau (total units of special timing)
    state$d <- NULL
    state$tau <- NULL
    state$choice_timfoc <- FALSE
    
    if (input$choice_timfoc == '1'){
      state$choice_timfoc <- TRUE
      state$d <- input$d_w3
      # alternative to compute tau
      if (state$unit_var == 'months') {
        aux_d1 <- as.Date(paste0("2000-",state$period_timfoc[1]))
        aux_d2 <- as.Date(paste0("2000-",state$period_timfoc[2]))
        state$tau <- interval(aux_d1, aux_d2) %/% months(1)
      } else {
        if (state$unit_var == 'days'){
          aux_d1 <- as.Date(paste0("2000-",state$period_timfoc[1]))
          aux_d2 <- as.Date(paste0("2000-",state$period_timfoc[2]))
          state$tau <- interval(aux_d1, aux_d2) %/% days(1)
        } 
      }
    }
    
    # compute IMPIT index
    # state$index <- fun_IMPIT(episodes = state$episodes,
    #                          unit = state$unit_var,
    #                          yrs = state$yrs_index,
    #                          m = input$m,
    #                          a = input$a_w1,
    #                          b = input$b_w2,
    #                          c = input$c_w2,
    #                          d = state$d,
    #                          intensity = state$intensity,
    #                          time_focus = state$choice_timfoc,
    #                          tau = state$tau)
    # 
    # state$contents_index <- data.frame(time = state$yrs_index, index = state$index)
    
    # compute with new function
    state$index_unit <- ifelse(input$choice_index_unit == '1','years', ifelse(input$choice_index_unit == '2', 'months', 'days'))
    # if (input$unit_var == '1') {
    #   state$index_unit <- ifelse(input$choice_index_unit == '1','years', ifelse(input$choice_index_unit == '2', 'months', 'days'))
    # } else {
    #   if (input$unit_var == '2'){
    #     state$index_unit <- ifelse(input$choice_index_unit == '1','years', 'months')
    #   } else{
    #     state$index_unit <-  'years'
    #   }
    # }
    print(input$choice_index_unit)
    print(state$index_unit)
    
    # period to compute index (YYYY-MM-DD)
    index_d1 <- as.Date(input$daterange_index[1])
    index_d2 <- as.Date(input$daterange_index[2])
    state$index_range <- seq(index_d1, index_d2, by = state$index_unit)

    # compute IMPIT index
    state$index <- fun_IMPITv2(episodes = state$episodes,
                               unit = state$unit_var,
                               index_range = state$index_range,
                               m = input$m,
                               a = input$a_w1,
                               b = input$b_w2,
                               c = input$c_w2,
                               d = state$d,
                               intensity = state$intensity,
                               time_focus = state$choice_timfoc,
                               tau = state$tau)

    # Arrange data frame index
    state$contents_index <- data.frame(
      Year = lubridate::year(state$index_range),
      Month = lubridate::month(state$index_range),
      Day = lubridate::day(state$index_range),
      time = state$index_range,
      index = state$index)
  })
  
  observeEvent(input$run_button_index,{
    
    # Table index: print
    output$contents_index <- DT::renderDataTable({ 
      DT::datatable(state$contents_index[, c("Year","Month","Day","index")], 
                    rownames = FALSE, 
                    options = list(
                      pageLength=10,
                      autoWidth = TRUE,
                      searching = TRUE,
                      search = list(regex=TRUE, caseInsensitive=TRUE)
                      )
                    ) %>%
        formatRound(c(4), 2)
    }) 
    
    # Table index: download .csv
    output$downloadTable_index <- downloadHandler(
      filename = function() {
        paste0("IMPIT_index_table.csv")
      },
      content = function(file) {
        write.csv(state$contents_index[, c("Year","Month","Day","index")], file, row.names = FALSE)
      }
    )
    
    # Plot index: print
    output$plot_index <- renderPlotly({
      
      state$plot_index <- plot_ly(data = state$contents_index) %>%
        add_lines(x = ~time, y = ~index) %>% 
        layout(xaxis = list(title="Time"),
               yaxis = list(title="IMPIT index"))
      
      state$plot_index
    })
    
    
    # Plot index: download .png
    output$downloadPlot_index <- downloadHandler(
      filename = function(){
        paste0("IMPIT_index_plot",'.png')
      },
      content = function(file){
        ggsave(file, plot = state$plot_index, width = 24, height = 12, units = "cm", dpi = 300)
      }
    )
    
  })
  
  
  
  # APPLICATION TAB ---------------------------------------------------------
  
  # Import IMPIT index 
  data_index <- eventReactive(input$index_file, {
    
    if (is.null(input$index_file)) return(NULL)
    
    read.csv(file=input$index_file$datapath,
             header=T, 
             stringsAsFactors=F)
  })
  
  # IMPIT index: table
  output$contents_app_index <- DT::renderDataTable({
    DT::datatable(data_index(),
                  options = list(
                    pageLength=5,
                    autoWidth=TRUE,
                    searching=TRUE,
                    search=list(regex=TRUE, caseInsensitive=TRUE)
                  ),
                  rownames = FALSE) %>%
      formatRound(c(4), 2) %>%
      formatStyle(columns=c(1:2), 'text-align'='centre')
  })
    
  
  # IMPIT index + trend
  output$plot_app_index <- renderPlotly({
    
    # LOESS
    # LOESS (Locally Estimated Scatterplot Smoother) combines local regression 
    # with kernels by using locally weighted polynomial regression (by default, 
    # quadratic regression with tri-cubic weights). It also allows estimation of
    # approximate confidence intervals. However, it is important to note that 
    # unlike smooth.spline or gam, loess does not use cross-validation. 
    # By default, the span is set to 0.75; that is, the estimated smooth at each
    # target value consists of a local regression constructed using 75% of the 
    # data points closest to the target value. This span is fairly large and 
    # results in estimated values that are smoother than those from other methods.
    
    xx <- as.Date(paste0(data_index()[ ,1],"-",data_index()[ ,2],"-",data_index()[ ,3]))
    yy <- data_index()[ ,4]
    xx <- as.integer(lubridate::year(xx))

    ll.smooth = loess(yy ~ xx, span=0.75)
    ll.pred = predict(ll.smooth, se = TRUE)
    ll.df = data.frame(x=ll.smooth$x, fit=ll.pred$fit,
                      lb = ll.pred$fit - (1.96 * ll.pred$se),
                      ub = ll.pred$fit + (1.96 * ll.pred$se))
    ll.df = ll.df[order(ll.df$xx),]
     
    p.llci = plot_ly(x=xx, y=yy, type="scatter", mode="lines", line=list(width=2), name="Data")
    p.llci = add_lines(p.llci, x=xx, y=ll.pred$fit, name="Mean", line=list(color="black", width=1))
    p.llci = add_ribbons(p.llci, x=ll.df$tt, ymin=ll.df$lb, ymax=ll.df$ub, name="95% CI", 
                         fillcolor=list(color="rgb(195, 195, 195)", opacity=0.4), 
                         line=list(color="rgb(195, 195, 195)", opacity=0.4, width=0))
    p.llci = layout(p.llci, title = "LOESS with confidence intervals")
    p.llci

  })
  
 
  # Import Response variable 
  data_resp <- eventReactive(input$resp_file, {
    
    if (is.null(input$resp_file)) return(NULL)
    
    read.csv(file=input$resp_file$datapath,
             header=TRUE, 
             stringsAsFactors=FALSE)
  })
  
  # Response variable: table
  output$contents_app_resp <- DT::renderDataTable({
    DT::datatable(data_resp(),
                  options = list(
                    pageLength=5,
                    autoWidth=TRUE,
                    searching=TRUE,
                    search=list(regex=TRUE, caseInsensitive=TRUE)
                  ),
                  rownames = FALSE) %>%
      formatRound(c(4), 2) %>%
      formatStyle(columns=c(1:4), 'text-align'='centre')
  }) 
  

  # Response variable: plot
  output$plot_app_resp <- renderPlotly({
    
    xx <-  as.Date(paste0(data_resp()[ ,1],"-",data_resp()[ ,2],"-",data_resp()[ ,3])) #data_resp()[ ,1]
    yy <- data_resp()[ ,4]
    df_resp <- data.frame(xx, yy)
    t1 <- head(xx,1)
    t2 <- tail(xx,1)
    data_resp_name <- colnames(data_resp())
    
    
    #p <- plot_ly(x=xx, y=yy, type="scatter", mode="lines", col="purple")
    p <- plot_ly(df_resp, x = ~xx, y = ~yy, type = 'scatter', mode = 'lines', 
                 line = list(color = 'rgb(205, 12, 24)', width = 2)) 
    p <- p %>% layout(
      xaxis = list(title = "Time", titlefont = list(size=12)),
      yaxis = list(title = data_resp_name[4], titlefont=list(size=12)))
    p
    
  })
  
  
  # Correlation analysis (Response vs IMPIT index): plot
  output$plot_corr_application <- renderPlotly({
    
    xx <- data_index()[ ,"index"]
    yy <- data_resp()[ ,4]
    yy <- log(yy)
    
    xx_name <- "IMPIT index"#colnames(data_index())[5]
    yy_name <- colnames(data_resp())[4]
    
    # linear model
    mod <- lm(yy ~ xx)
    CI95 <- confint(mod, "xx", level = 0.95)
    cor.out95 <- cor.test(yy, xx, alternative="two.sided", conf.level=0.95, method="pearson")

    # data frame with results of LR
    res.LR <-  data.frame(slope = as.numeric(coef(mod)[2]),
                            lb95 = CI95[ ,1],
                            ub95 = CI95[ ,2],
                            R2 = as.numeric(summary(mod)$r.squared),
                            pVal = as.numeric(anova(mod)$'Pr(>F)'[1]),
                            corr = as.numeric(cor.out95$estimate),
                            corr.lb95 = as.numeric(cor.out95$conf.int[1]),
                            corr.ub95 = as.numeric(cor.out95$conf.int[2]))
   
    res.LR <- res.LR %>%
      mutate(
        xx= max(xx), yy = max(yy),
        labelR2 =  glue("Corr = {round(corr, 2)}<br>*R*<sup>2</sup> = {round(R2, 2)} ")
      )
    
    # extra for plotting
    #col.reg <- ifelse(res.LR$pVal <= 0.05, "#4292C6", "grey40")
    col.reg <- ifelse(res.LR$pVal <= 0.05, "blue", "grey40")
    col.ann <- "#08519C"
    col.box <- ifelse(res.LR$pVal <= 0.05, "#9ECAE1", "grey60")
    size.num <- 3
    size.ann <- 3
    year.text <- substr(as.character(data_resp()[ ,1]), nchar(as.character(data_resp()[ ,1]))-2+1, nchar(as.character(data_resp()[ ,1])))
    
    # state$plot_corr_application <- ggplot(data = data.frame(xx, yy), aes(x = xx, y = yy)) +
    #   geom_smooth(method ='lm', se = TRUE, col = col.reg, alpha = 0.25) +
    #   geom_text(aes(label = year.text), size = size.num) +
    #   labs(x = "IMPIT index", y = "log( SCPUE )") +
    #   geom_richtext(data =  res.LR, aes(label = labelR2),
    #                 fill = after_scale(alpha(col.box,.2)),
    #                 color =  col.box,
    #                 text.colour = "black",
    #                 size = 3.1, hjust = 1, vjust = 1) +
    #   theme_bw(base_size = 11)
    # 
    # state$plot_corr_application
    
    
    # save corr and p-value info
    COR = c(res.LR$corr, res.LR$pVal)
    
    
    # build plot
    mod.pred = predict(mod, type="response", se.fit=TRUE)
    mod.df = data.frame(x=xx, 
                        y=mod.pred$fit,
                        lb=as.numeric(mod.pred$fit - (1.96 * mod.pred$se.fit)),
                        ub=as.numeric(mod.pred$fit + (1.96 * mod.pred$se.fit)))
    mod.df = mod.df[order(mod.df$x),]
    
    pp <- plot_ly(x=xx, y=yy)
    pp <- add_ribbons(pp, x=mod.df$x, ymin=mod.df$lb, ymax=mod.df$ub, name="95% CI", 
                      fillcolor = list(color="rgb(195, 195, 195)", opacity=0.4), 
                      line = list(color="rgb(195, 195, 195)", opacity=0.4, width=0))
    pp <- add_lines(pp, x=xx, y=mod.pred$fit, name="Linear Regression", line=list(color=col.reg, width=2))
    pp <- add_text(pp, text = ~year.text, textposition="top center", showlegend = F)
    pp <- layout(pp, 
                 xaxis = list(title = xx_name), 
                 yaxis = list(title = yy_name),
                 title = list(
                   text = paste(c("R=","p="), signif(as.numeric(COR,3),3), collapse=" "),
                   font = list(color="blue", size=11),
                   y=0.75, x=0.90, 
                   xanchor='center', yanchor='top'
                   )
                 )
    pp
    
  })
  
  
  # Save corr plot
  output$downloadPlot_app <- downloadHandler(
    filename = function(){
      paste0("Plot_application",'.png')
    },
    content = function(file){
      ggsave(file, plot = state$plot_corr_application, width = 14, height = 14, units = "cm", dpi = 300)
    }
  )
  
  # Summary of regression analysis
  output$summary <- renderPrint({
    
    xx <- data_index()[ ,4]
    yy <- data_resp()[ ,4]
    yy <- log(yy)
    
    # linear model
    mod <- lm(yy ~ xx)
    summary(mod)
    
  })

  
  # APPLICATION TAB ---------------------------------------------------------
  
  output$Rsession <- renderPrint(
    print(sessionInfo())
  )
  
  # ABOUT TAB ---------------------------------------------------------------
  url <- a("github/manumendiolar", href="https://github.com/manumendiolar/IMPIT_shiny/")
  output$githublink <- renderUI({
    tagList("Source code is available at:", url)
  })
  
  url_gitissues <- a("https://github.com/manumendiolar/IMPIT_shiny/issues", href="https://github.com/manumendiolar/IMPIT_shiny/issues")
  output$githubissues <- renderUI({
    tagList("* Github:", url_gitissues)
  })
  
  url_mm_email <- a("m.mendiolar@uq.edu.au",href="m.mendiolar@uq.edu.au")
  output$mm_email <- renderUI({
    tagList("* Email:", url_mm_email)
  })
  
  
}

#shinyApp(ui, server)