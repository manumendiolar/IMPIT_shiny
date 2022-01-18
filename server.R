# This is a Shiny web application for IMPIT indices. You can run the application by clicking
# the 'Run App' button above.
# Manuela M.


# packages
library(shiny)
library(shinythemes)
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



# Server
server <- function(input, output, session) {
  
  #
  # main page: loading data
  #
  data_input <- reactive({
    req(input$csv_input)
    inFile <- input$csv_input
    read.csv(inFile$datapath)
  })
  
  observeEvent(data_input(),{
    choices <- c("Not selected", names(data_input()))
    updateSelectInput(inputId = "time_var", choices = choices)
    updateSelectInput(inputId = "env_var", choices = choices)
    
  })
  
  time_var <- eventReactive(input$run_button_data, input$time_var)
  env_var <- eventReactive(input$run_button_data, input$env_var)
  
  output$contents_data <- DT::renderDataTable({ 
    DT::datatable(data_input(), options = list(lengthMenu=c(5,10,30,50), pageLength=10), rownames=FALSE) %>%
      formatRound(c(2), 2) #%>% formatStyle(columns = c(1:2), 'text-align' = 'left')
  })
  
  output$envSummary <- renderPrint({
    cat(paste0("Statistics summary of ",names(data_input())[2]),"\n")
    cat("\n")
    summary(data_input()[ ,input$env_var])
  })
  
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
  # second page: episodes computation
  #
  state <- reactiveValues()
  
  observeEvent(input$run_button_epi,{
    
    if (input$choice_epifile == '1'){
      
      # compute episodes/update if necessary 
      state$episodes <- mydetect_event(data_input(), input$thres_above, input$thres, input$duration_min)
    } else {
      
      # load episode list if already available
      epifile_input <- reactive({
        req(input$epifile_input)
        inFile <- input$epifile_input
        ext <- tools::file_ext(inFile$datapath)
        
        validate(need(ext %in% c("csv","RData"), "Please upload a csv or RData file"))
        
        if (ext == "csv") {
          read.csv(inFile$datapath)
        } else {
          print("This doesn't work for RData files...")
          load_Rdata(inFile$datapath)
        }
      })
      state$episodes <- epifile_input()
    }
    
    # Check for timing focus
    if (input$choice_timfoc){
      
      state$start_day <- ifelse(input$start_timfoc_day %in% c(1,2,3,4,5,6,7,8,9), 
                                paste0("0",input$start_timfoc_day), paste0(input$start_timfoc_day))
      state$start_month <- ifelse(input$start_timfoc_month %in% c(1,2,3,4,5,6,7,8,9), 
                                  paste0("0",input$start_timfoc_month), paste0(input$start_timfoc_month))
      state$end_day <- ifelse(input$end_timfoc_day %in% c(1,2,3,4,5,6,7,8,9), 
                              paste0("0",input$end_timfoc_day), paste0(input$end_timfoc_day))
      state$end_month <- ifelse(input$end_timfoc_month %in% c(1,2,3,4,5,6,7,8,9), 
                                paste0("0",input$end_timfoc_month), paste0(input$end_timfoc_month))
      
      state$period_timfoc <- c(paste0(state$start_month,"/",state$start_day), paste0(state$end_month,"/",state$end_day))
      
      state$episodes <- mydetect_timfoc(episodes = state$episodes, timfoc_dates = state$period_timfoc)
      
    } else {
      state$episodes <- state$episodes
    }
  })
  
  # print table with episodes
  output$contents_epi <- DT::renderDataTable({ 
    DT::datatable(state$episodes, rownames=FALSE) %>%
      formatRound(c(9:12), 2)
  }) 
  
  # print plot of episodes
  observeEvent(input$run_button_epi,{
    
    output$plot_epi <- renderPlot({
      x <- state$episodes$date_start
      y <- state$episodes$intensity_mean
      df <- data.frame(x,y)
      df$x <- as.Date(df$x)
      d1 <- as.Date(head(df$x,1))
      d2 <- as.Date(tail(df$x,1))
      
      
      ggplot(df, aes(x, y)) +
        geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
        geom_point( color="orange", size=3) +
        labs(x="date_start", y="intensity_mean", title="Lolli plot events") +
        scale_x_date(breaks=seq(d1, d2, by="5 years"), limits=c(d1, d2), date_labels="%Y") +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_blank()) 
    })
  })
  
  
  #
  # third page: index computation
  #
  # Index computation
  observeEvent(input$run_button_index,{
    
    state$unit_var <- ifelse(input$unit_var == '1','days', ifelse(input$unit_var == '2', 'months', 'years'))
    state$intensity <- input$choice_intensity
    state$period_index <- seq(input$period_index_start, input$period_index_end, 1)
    
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
    
    # print table with episodes
    output$contents_index <- DT::renderDataTable({ 
      DT::datatable(state$contents_index, rownames=FALSE) %>%
        formatRound(c(2), 2)
    }) 
    
    # print plot index
    output$plot_index <- renderPlot({
      y1 <- head(state$contents_index$time,1)
      y2 <- tail(state$contents_index$time,1)
      
      ggplot(state$contents_index, aes(time, index)) +
        geom_line() +
        labs(x = "Time", y = "Index", title = paste0("IMPIT index for ",as.character(input$env_var)," signal")) +
        scale_x_continuous(breaks=seq(y1, y2, 2), limits=c(y1, y2)) +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_blank())
    })
  })
  
  
  #
  # fourth page: application
  #
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
  
  resp_var <- eventReactive(input$run_button_application, input$resp_var)
  resp_time_var <- eventReactive(input$run_button_application, input$resp_time_var)
  
  observeEvent(input$run_button_application,{
    
    # print response data table for checking
    output$contents_application <- DT::renderDataTable({ 
      DT::datatable(data_resp(), rownames=FALSE) %>%
        formatRound(c(2:5), 2)
    })
    
    # print response plot 
    output$plot_resp_application <- renderPlot({
      
      state$dep_time <-  as.numeric(data_resp()[ ,input$resp_time_var])
      state$dep_var <- as.numeric(data_resp()[ ,input$resp_var])
      t1 <- head(state$dep_time,1)
      t2 <- tail(state$dep_time,1)
      
      df_resp <- data.frame(time = state$dep_time, resp = state$dep_var)
      
      ggplot(df_resp, aes(time, resp)) +
        geom_line() +
        labs(x = as.character(input$resp_time_var), 
             y = as.character(input$resp_var), 
             title = "Response Variable") +
        scale_x_continuous(breaks=seq(t1, t2, 2), limits=c(t1, t2)) +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_blank())
    })
    
    
    # print regression 
    output$plot_corr_application <- renderPlot({
      
      # keep independent variable
      state$indep_var <- as.numeric(state$index)
      
      # linear model
      mod <- lm(state$dep_var ~ state$indep_var)
      CI90 <- confint(mod, "state$indep_var", level = 0.90)
      CI95 <- confint(mod, "state$indep_var", level = 0.95)
      cor.out95 <- cor.test(state$dep_var, state$indep_var, alternative="two.sided", conf.level=0.95, method="pearson")
      cor.out90 <- cor.test(state$dep_var, state$indep_var, alternative="two.sided", conf.level=0.90, method="pearson")
      
      # data frame with results of LR
      res.LR <-  data.frame(slope = as.numeric(coef(mod)[2]), 
                            lb90 = CI90[ ,1], 
                            ub90 = CI90[ ,2], 
                            lb95 = CI95[ ,1], 
                            ub95 = CI95[ ,2],
                            R2 = as.numeric(summary(mod)$r.squared), 
                            pVal = as.numeric(anova(mod)$'Pr(>F)'[1]), 
                            corr = as.numeric(cor.out95$estimate), 
                            corr.lb90 = as.numeric(cor.out90$conf.int[1]),
                            corr.ub90 = as.numeric(cor.out90$conf.int[2]),
                            corr.lb95 = as.numeric(cor.out95$conf.int[1]),
                            corr.ub95 = as.numeric(cor.out95$conf.int[2]))
      
      # extra for plotting
      col1 <- ifelse( res.LR$pVal > 0.10, "black", ifelse( res.LR$pVal > 0.05, "magenta", "green"))
      col2 <- ifelse( res.LR$pVal > 0.10, "black", ifelse( res.LR$pVal > 0.05, "magenta", "green4"))
      subtitle <- ifelse(round(res.LR$pVal,3) < 0.001,
                         paste0("Corr = ", round(res.LR$corr,2),", Rsq = ",round(res.LR$R2,2),", p value < 0.001"),
                         paste0("Corr = ", round(res.LR$corr,2),", Rsq = ",round(res.LR$R2,2),", p value = ",round(res.LR$pVal,3)))
      
      ggplot(data = data.frame(yrs.lab = state$dep_time, xx = state$indep_var, yy = state$dep_var), aes(x = xx, y = yy)) +
        geom_smooth(method ='lm', se = TRUE, col = col1, alpha = 0.25) +
        geom_text(aes(label = yrs.lab), size = 3.5) +
        labs(y = as.character(input$resp_var),
             x = "IMPIT index", 
             title = "Correlation analysis",
             subtitle = subtitle) +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),
              panel.border = element_blank(),
              axis.ticks.x = element_blank(),
              plot.title = element_text(size = 12, face = 1),
              plot.subtitle = element_text(size = 11, face = 2, colour = col2))
      
    }, height = 500, width = 500 )
    
  })
  
  # Download plot
  output$downloadPlot_application <- downloadHandler(
    filename = function(){
      paste('test', '.png', sep = '')
    },
    content = function(file){
      req(plot_corr_application())
      ggsave(file, plot = plot_corr_application(), device = 'png')
    }
  )
  
  
}





