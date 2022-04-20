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

setBackgroundImage(src = NULL, shinydashboard = TRUE)

# Attach the folder where the 'QU-GENE' engine and QuLinePlus are stored.
IMPIT_base <- paste0(getwd(),"/")



# Define server for app
server <- function(input, output, session) {
  
  #
  # Helpfiles
  #
  # uses 'helpfiles' directory by default
  # in this example, we use the withMathJax parameter to render formulae
  observe_helpers(session = getDefaultReactiveDomain(),
                  help_dir = paste0(IMPIT_base,"helpfiles"), withMathJax = TRUE)
  
  #
  # HOME tab
  #
  output$myimage <- renderImage({
    # filename is ./www/myimage.jpeg
    filename <- normalizePath(file.path('./www',
                                        paste('myimage', input$n, '.jpeg', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  #
  # DATA tab
  #
  
  # read data file with env. signal info
  data_input <- reactive({
    req(input$csv_input)
    inFile <- input$csv_input
    read.csv(inFile$datapath)
  })
  
  # update time and env variables
  observeEvent(data_input(),{
    choices <- c("Not selected", names(data_input()))
    updateSelectInput(inputId = "time_var", choices = choices)
    updateSelectInput(inputId = "env_var", choices = choices)
    
  })
  time_var <- eventReactive(input$run_button_data, input$time_var)
  env_var <- eventReactive(input$run_button_data, input$env_var)
  
  # construct data table
  output$contents_data <- DT::renderDataTable({ 
    DT::datatable(data_input(), options = list(lengthMenu=c(5,10,30,50), pageLength=5), rownames=FALSE) %>%
      formatRound(c(2), 2) #%>% formatStyle(columns = c(1:2), 'text-align' = 'left')
  })
  
  # construct summary 
  # output$envSummary <- renderPrint({
  #   cat(paste0("Statistics summary of ",names(data_input())[2]),"\n")
  #   cat("\n")
  #   summary(data_input()[ ,input$env_var])
  # })
  
  # basic data plot (when user click button)
  observeEvent(input$run_button_data,{
    output$envPlot <- renderPlot({
      x <- data_input()[ ,input$time_var]
      x <- lubridate::dmy(x)
      y <- data_input()[ ,input$env_var]
      
      df <- data.frame(x,y)
      date1 <- as.Date(input$daterange[1])
      date2 <- as.Date(input$daterange[2])
      ggplot(data=df, aes(x,y)) + 
        geom_line() + 
        labs(x=as.character(input$time_var), y=as.character(input$env_var)) +
        scale_x_date(breaks=seq(date1, date2, by="5 years"), limits=c(date1, date2), date_labels="%Y") +
        theme_light() 
    })
  })
  
  
  
  #
  # EPISODES tab
  #
  
  state <- reactiveValues()
  
  # depending on generate / upload episode file
  observeEvent(input$run_button_epi,{
    
   if (input$choice_epifile == '1'){
      
      # check for up or dwn episodes
      if (input$choice_thres == '1'){
        thres_above <- TRUE
      } else{
        thres_above <- FALSE
      }
      
      # compute episodes/update if necessary 
      state$episodes <- mydetect_event(data_input(), thres_above, input$thres, input$duration_min)
      
    } else {
      
      # load episode list if already available
      epifile_input <- reactive({
        req(input$epifile_input)
        inFile <- input$epifile_input
        ext <- tools::file_ext(inFile$datapath)
        
        validate(need(ext %in% c("csv"), "Please upload a csv file"))
        
        if (ext == "csv") {
          read.csv(inFile$datapath)
        } else {
          print("This doesn't work for this file extention...")
        }
      })
      state$episodes <- epifile_input()
    }
    
    # check for timing focus
    if (input$choice_timfoc){

      # check format timing start date
      start_timfoc_day <- as.character(input$start_timfoc_day)
      start_timfoc_month <- factor(input$start_timfoc_month,
                                   levels = c("January","February","March","April","May","June","July",
                                              "August","September","October","November","December"))
      start_timfoc_month <- as.integer(start_timfoc_month)
      
      # check format timing end date
      end_timfoc_day <- as.character(input$end_timfoc_day)
      end_timfoc_month <- factor(input$end_timfoc_month,
                                   levels = c("January","February","March","April","May","June","July",
                                              "August","September","October","November","December"))
      end_timfoc_month <- as.integer(end_timfoc_month)
      
      
      # update format of start date
      state$start_day <- ifelse(start_timfoc_day %in% seq(1,9,1), paste0("0",start_timfoc_day), paste0(start_timfoc_day))
      state$start_month <- ifelse(start_timfoc_month %in% seq(1,9,1), paste0("0",start_timfoc_month), paste0(start_timfoc_month))
      
      # update format of end date
      state$end_day <- ifelse(end_timfoc_day %in% seq(1,9,1), paste0("0",end_timfoc_day), paste0(end_timfoc_day))
      state$end_month <- ifelse(end_timfoc_month %in% seq(1,9,1), paste0("0",end_timfoc_month), paste0(end_timfoc_month))
      
      # gather as a vector
      state$period_timfoc <- c(paste0(state$start_month,"/",state$start_day), paste0(state$end_month,"/",state$end_day))
      
      # update episodes
      state$episodes <- mydetect_timfoc(episodes = state$episodes, timfoc_dates = state$period_timfoc)
      
    } else {
      # keep the same
      state$episodes <- state$episodes
    }
  })
  
  
  # Table episodes: print
  output$contents_epi <- DT::renderDataTable({ 
    DT::datatable(state$episodes, rownames=FALSE, options = list(pageLength=5)) %>%
      formatRound(c(6:10), 2)
  })
  
  # Table episodes: download .csv
  output$downloadTable_epi <- downloadHandler(
    filename = function() {
      paste0("Table_episodes.csv")
    },
    content = function(file) {
      write.csv(state$episodes, file)
    }
  )
  
  # Plot episodes: print
  observeEvent(input$run_button_epi,{
    
    output$plot_epi <- renderPlot({
      x <- state$episodes$date_start
      y <- state$episodes$intensity_mean
      df <- data.frame(x,y)
      df$x <- as.Date(df$x)
      d1 <- as.Date(head(df$x,1))
      d2 <- as.Date(tail(df$x,1))
      
      if (input$choice_timfoc){
        df$z <- state$episodes$overlap
        df$z <- as.factor(df$z)
        
        state$plot_epi <- ggplot(df, aes(x, y, col = z)) +
          geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
          geom_point( size=3) +
          labs(x="date_start", 
               y="intensity_mean", 
               title="Lolli plot events",
               col="Overlap") +
          scale_x_date(breaks=seq(d1, d2, by="5 years"), limits=c(d1, d2), date_labels="%Y") +
          scale_color_manual(values = c("orange","blue")) +
          theme_light() +
          theme(panel.grid.major.x = element_blank(),
                panel.border = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 9), 
                legend.position = "right") 
      } else {
        state$plot_epi <- ggplot(df, aes(x, y)) +
          geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
          geom_point( color="orange", size=3) +
          labs(x="date_start", y="intensity_mean", title="Lolli plot events") +
          scale_x_date(breaks=seq(d1, d2, by="5 years"), limits=c(d1, d2), date_labels="%Y") +
          theme_light() +
          theme(panel.grid.major.x = element_blank(),
                panel.border = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 9)) 
      }
      
      
      state$plot_epi
      
    })
  })
  
  # Plot episodes: download .png
  output$downloadPlot_epi <- downloadHandler(
    filename = function(){
      paste0("Plot_episodes",'.png')
    },
    content = function(file){
      ggsave(file, plot = state$plot_epi, width = 26, height = 12, units = "cm", dpi = 300)
    }
  )
  
  
  
  #
  # INDEX tab
  #
  
  # computing index
  observeEvent(input$run_button_index,{
    
    state$unit_var <- ifelse(input$unit_var == '1','days', ifelse(input$unit_var == '2', 'months', 'years'))
    state$intensity <- input$choice_intensity
    state$period_index <- seq(input$period_index_start, input$period_index_end, 1)
    
    
    # daterange_index_start <- as.Date(input$daterange_index[1])
    # daterange_index_end <- as.Date(input$daterange_index[2])
    # period_index_start <- lubridate::year(daterange_index_start)
    # period_index_end <- lubridate::year(daterange_index_end)
    #state$period_index <- seq(period_index_start, period_index_end, 1)
    
    if (input$choice_timfoc){
      
      if (state$unit_var == 'days') {
        state$tau <- as.Date(paste0("2000","/",state$period_timfoc[2])) - as.Date(paste0("2000","/",state$period_timfoc[1]))
      } else {
        if (state$unit_var == 'months'){
          state$tau <- as.integer(input$end_timfoc_month) - as.integer(input$start_timfoc_month) + 1
        } else {
          state$tau <- as.integer(input$end_timfoc_year) - as.integer(input$start_timfoc_year) + 1
        }
      }
      state$tau <- as.integer(state$tau)
      state$d <- input$d_w3
    } else {
      
      state$d <- NULL
      state$tau <- NULL
    }
    
    state$index <- fun_IMPIT(episodes = state$episodes,
                             unit = state$unit_var,
                             yrs = state$period_index,
                             m = input$m,
                             a = input$a_w1,
                             b = input$b_w2,
                             c = input$c_w2,
                             d = state$d,
                             intensity = state$intensity,
                             time_focus = input$choice_timfoc,
                             tau = state$tau)
    
    state$contents_index <- data.frame(time = state$period_index, index = state$index)
    
  })
  
  observeEvent(input$run_button_index,{
    
    # Table index: print
    output$contents_index <- DT::renderDataTable({ 
      DT::datatable(state$contents_index, rownames=FALSE, options = list(pageLength=5)) %>%
        formatRound(c(2), 2)
    }) 
    
    # Table index: download .csv
    output$downloadTable_index <- downloadHandler(
      filename = function() {
        paste0("Table_index.csv")
      },
      content = function(file) {
        write.csv(state$contents_index, file)
      }
    )
    
    # Plot index: print
    output$plot_index <- renderPlot({
      y1 <- head(state$contents_index$time,1)
      y2 <- tail(state$contents_index$time,1)
      
      state$plot_index <- ggplot(state$contents_index, aes(time, index)) +
        geom_line( ) +
        # geom_smooth( method = "lm", alpha = 0.05, level = 0.95, aes(colour="Lin. Reg.", fill="Lin. Reg.", lty="Lin. Reg.")) + 
        # geom_smooth( method = "lm", alpha = 0.05, level = 0.95, formula = y ~ poly(x, 2), aes(colour="Cuad. Reg.", fill="Cuad. Reg.", lty="Cuad. Reg.")) + 
        # scale_colour_manual(name="", values = c("seagreen","darkorchid")) +
        # scale_fill_manual(name="", values = c("seagreen","darkorchid")) +
        # scale_linetype_manual(name="", values = c(2,3)) +
        # labs(x = "Time", y = "IMPIT index") +
        scale_x_continuous(breaks=seq(y1, y2, 2), limits=c(y1, y2)) +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 10))
      
      state$plot_index
    })
    
    # Plot index: download .png
    output$downloadPlot_index <- downloadHandler(
      filename = function(){
        paste0("Plot_index",'.png')
      },
      content = function(file){
        ggsave(file, plot = state$plot_index, width = 24, height = 12, units = "cm", dpi = 300)
      }
    )
    
  })
  
  
  
  #
  # APPLICATION tab
  #
  
  # load index data
  data_index <- reactive({
    req(input$index_file)
    inFile <- input$index_file
    read.csv(inFile$datapath)
  })
  # choose index variable
  observeEvent(data_index(),{
    choices <- c("Not selected", names(data_index()))
    updateSelectInput(inputId = "index_time_var", choices = choices)
    updateSelectInput(inputId = "index_var", choices = choices)
  })
  index_var <- eventReactive(input$run_button_app_index, input$index_var)
  index_time_var <- eventReactive(input$run_button_app_index, input$index_time_var)
  
  observeEvent(input$run_button_app_index,{
    
    # Table Index: print
    output$contents_app_index <- DT::renderDataTable({ 
      DT::datatable(data_index(), rownames=FALSE, options = list(pageLength=5)) %>%
        formatRound(c(2), 2)
    })
    
    # Plot Index: print 
    output$plot_app_index <- renderPlot({
      
      state$ind_time <-  as.numeric(data_index()[ ,input$index_time_var])
      state$ind_var <- as.numeric(data_index()[ ,input$index_var])
      t1 <- head(state$ind_time,1)
      t2 <- tail(state$ind_time,1)

      df_index <- data.frame(time = state$ind_time, index = state$ind_var)
      
      state$plot_app_index <- ggplot(df_index, aes(time, index)) +
        geom_line() +
        geom_smooth( method = "lm", alpha = 0.10, level = 0.95, formula = y ~ poly(x, 1), aes(colour="Linear", fill="Linear", lty="Linear")) + 
        geom_smooth( method = "lm", alpha = 0.10, level = 0.95, formula = y ~ poly(x, 2), aes(colour="Cuadratic", fill="Cuadratic", lty="Cuadratic")) + 
        scale_colour_manual(name="Regression", values = c("seagreen","darkorchid")) +
        scale_fill_manual(name="Regression", values = c("seagreen","darkorchid")) +
        scale_linetype_manual(name="Regression", values = c(3,2)) +
        scale_x_continuous(breaks=seq(t1, t2, 2), limits=c(t1,t2)) +
        labs(x = "Time", 
             y = "IMPIT index") +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 10))

      state$plot_app_index
    })
  })
  
  
  # load response data
  data_resp <- reactive({
    req(input$resp_file)
    inFile <- input$resp_file
    read.csv(inFile$datapath)
  })
  # choose response variable
  observeEvent(data_resp(),{
    choices <- c("Not selected", names(data_resp()))
    updateSelectInput(inputId = "resp_time_var", choices = choices)
    updateSelectInput(inputId = "resp_var", choices = choices)
  })
  resp_var <- eventReactive(input$run_button_app_resp, input$resp_var)
  resp_time_var <- eventReactive(input$run_button_app_resp, input$resp_time_var)
  
  observeEvent(input$run_button_application,{
    
    # Table Response: print
    output$contents_app_resp <- DT::renderDataTable({ 
      DT::datatable(data_resp(), rownames=FALSE, options = list(pageLength=5)) %>%
        formatRound(c(2), 2)
    })
    
    # Plot Response: print 
    output$plot_app_resp <- renderPlot({
      
      state$dep_time <-  as.numeric(data_resp()[ ,input$resp_time_var])
      state$dep_var <- as.numeric(data_resp()[ ,input$resp_var])
      t1 <- head(state$dep_time,1)
      t2 <- tail(state$dep_time,1)
      
      df_resp <- data.frame(time = state$dep_time, resp = state$dep_var)
      
      state$plot_app_resp <- ggplot(df_resp, aes(time, resp)) +
        geom_line() +
        scale_x_continuous(breaks=seq(t1, t2, 2), limits=c(t1, t2)) +
        labs(x = "Time", 
             y = "Response variable") +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 10))
      
      state$plot_app_resp
    })
    
    # Plot Regression: print 
    output$plot_corr_application <- renderPlot({
      
      # keep vars
      xx <- as.numeric(state$ind_var)
      yy <- log(as.numeric(state$dep_var))
      
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
          # location of text label in data coordinates
          xx= max(xx), yy = max(yy),
          # text label containing corr and r^2 value 
          labelR2 =  glue("Corr = {round(corr, 2)}<br>*R*<sup>2</sup> = {round(R2, 2)} ")
        )
      
      
      # extra for plotting
      col.reg <- ifelse(res.LR$pVal <= 0.05, "#4292C6", "grey40")
      col.ann <- "#08519C"
      col.box <- ifelse(res.LR$pVal <= 0.05, "#9ECAE1", "grey60")
      size.num <- 3
      size.ann <- 3
      year.text <- substr(as.character(state$dep_time), nchar(as.character(state$dep_time))-2+1, nchar(as.character(state$dep_time)))
      #c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18")
      
      state$plot_corr_application <- ggplot(data = data.frame(xx, yy), aes(x = xx, y = yy)) +
        geom_smooth(method ='lm', se = TRUE, col = col.reg, alpha = 0.25) +
        geom_text(aes(label = year.text), size = size.num) +
        labs(x = "IMPIT index", y = "log( SCPUE )") +
        geom_richtext(data =  res.LR, aes(label = labelR2),
                      fill = after_scale(alpha(col.box,.2)),
                      color =  col.box,
                      text.colour = "black",
                      size = 3.1, hjust = 1, vjust = 1) +
        theme_bw(base_size = 11)
      
      state$plot_corr_application
      
    }, height = 450, width = 450 )
    
    # Plot Regression: download .png
    output$downloadPlot_app <- downloadHandler(
      filename = function(){
        paste0("Plot_application",'.png')
      },
      content = function(file){
        ggsave(file, plot = state$plot_corr_application, width = 14, height = 14, units = "cm", dpi = 300)
      }
    )
    
  })
  
  
  
}

#shinyApp(ui, server)