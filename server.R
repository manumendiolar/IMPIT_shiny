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


choices_days <- as.character(1:31)
choices_months <- c("January","February","March","April","May","June","July","August","September","October","November","December") 



# Define server for app
server <- function(input, output, session) {
  
  
  # HELPFILES ---------------------------------------------------------------
  
  # uses 'helpfiles' directory by default
  # in this example, we use the withMathJax parameter to render formulae
  observe_helpers(session = getDefaultReactiveDomain(),
                  help_dir = paste0(IMPIT_base,"helpfiles"), withMathJax = TRUE)
  
  
  
  
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
      if (is.null(input$csv_input)) retunr(NULL)
      
      inFile <- input$csv_input
      read.csv(inFile$datapath)
    }
  )
  
  # 
  # # update time and env variables
  # observeEvent(data_input(),{
  #   choices <- c("Not selected", names(data_input()))
  #   updateSelectInput(inputId = "time_var", choices = choices)
  #   updateSelectInput(inputId = "env_var", choices = choices)
  #   
  # })
  # time_var <- eventReactive(input$run_button_data, input$time_var)
  # env_var <- eventReactive(input$run_button_data, input$env_var)
  # 
  
  # Environmental data: table
  output$contents_data <- DT::renderDataTable({ 
    DT::datatable(data_input(),
                  options = list(
                    pageLength=10,
                    lengthMenu=c(5,10,30,50), 
                    autoWidth=T,  
                    searching=T,
                    search=list(regex=T, caseInsensitive=T)
                    ),
                  rownames = F) %>%
      formatRound(c(2), 2) %>%
      formatStyle(columns=c(1:2), 'text-align'='centre')
  })
  
  
  # Environmental data: line plot
  output$envPlot <- renderPlotly({
    
    withProgress(message = 'Creating plot', style = 'notification', value = 0.1, {
      Sys.sleep(0.5)
      
      data_input_name <- colnames(data_input())
      
      # Set x and y axis and display data in line plot using plotly
      plot_ly( x = ~as.Date(data_input()[ ,1], format = "%Y-%m-%d"), y = ~data_input()[ ,2]) %>%
        add_lines() %>% 
        layout(xaxis = list(title=data_input_name[1],titlefont=list(size=12)),
               yaxis = list(title=data_input_name[2],titlefont=list(size=12)))
    }
    )
  }) 
    

  
  
  # EPISODES TAB ------------------------------------------------------------
  
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
      epifile_input <- eventreactive(input$epifile_input,
        {
          inFile <- input$epifile_input
          ext <- tools::file_ext(inFile$datapath)
          validate(need(ext %in% c("csv"), "Please upload a csv file"))
          
          if (ext == "csv") {
            read.csv(inFile$datapath, header=T, stringsAsFactors=T)
          } else {
            print("This doesn't work for this file extention...")
          }
        }
      )
      
      state$episodes <- epifile_input()
    }
    
    # check for timing focus
    if (input$choice_timfoc == '1'){

      # check format timing start date
      start_timfoc_day <- as.character(input$start_timfoc_day)
      start_timfoc_month <- factor(input$start_timfoc_month, levels = choices_months)
      start_timfoc_month <- as.integer(start_timfoc_month)
      
      # check format timing end date
      end_timfoc_day <- as.character(input$end_timfoc_day)
      end_timfoc_month <- factor(input$end_timfoc_month, levels = choices_months)
      end_timfoc_month <- as.integer(end_timfoc_month)
      
      
      # update format of start date
      state$start_day <- ifelse(start_timfoc_day %in% seq(1,9,1), paste0("0",start_timfoc_day), paste0(start_timfoc_day))
      state$start_month <- ifelse(start_timfoc_month %in% seq(1,9,1), paste0("0",start_timfoc_month), paste0(start_timfoc_month))
      
      # update format of end date
      state$end_day <- ifelse(end_timfoc_day %in% seq(1,9,1), paste0("0",end_timfoc_day), paste0(end_timfoc_day))
      state$end_month <- ifelse(end_timfoc_month %in% seq(1,9,1), paste0("0",end_timfoc_month), paste0(end_timfoc_month))
      
      # gather as a vector
      state$period_timfoc <- c(paste0(state$start_month,"-",state$start_day), paste0(state$end_month,"-",state$end_day))
      
      # update episodes
      state$episodes <- mydetect_timfoc(episodes = state$episodes, timfoc_dates = state$period_timfoc)
      
    } else {
      # keep the same
      state$episodes <- state$episodes
    }
  })
  
  
  # Episodes: table print
  output$contents_epi <- DT::renderDataTable({ 
    DT::datatable(state$episodes, 
                  rownames=F, 
                  options = list(
                    pageLength=10,
                    autoWidth=F,
                    scrollX = T,
                    searching=T,
                    search = list(regex=T, caseInsensitive=T)
                    )
                  ) %>%
    formatRound(c(6:10), 2)
    })
  
  
  # Episodes: table download .csv
  output$downloadTable_epi <- downloadHandler(
    filename = function() {
      paste0("Episodes_table.csv")
    },
    content = function(file) {
      write.csv(state$episodes, file)
    }
  )
  
  
  # Episodes: plot print
  output$plot_epi <- renderPlotly({
    
    x <- state$episodes$date_start
    y <- state$episodes$intensity_mean
    df <- data.frame(x,y)
    df$x <- as.Date(df$x)
    d1 <- as.Date(head(df$x,1))
    d2 <- as.Date(tail(df$x,1))
      
    
    if (input$choice_timfoc == '1'){
      df$z <- state$episodes$overlap
      df$z <- as.factor(df$z)
      
      # if there is timing we add colour  
      lolliEp <- ggplot(df, aes(x, y, col = z)) +
        geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
        geom_point(size=2) +
        labs(x="date_start", y="intensity_mean", title="Lollipop chart", col="Overlap") +
        scale_color_manual(values = c("orange","blue"), labels=c("no","yes"))
      } else {
        
        lolliEp <- ggplot(df, aes(x, y)) +
          geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
          geom_point(color="orange", size=2) +
          labs(x="date_start", y="intensity_mean", title="Lollipop chart") 
      }
    
    state$plot_epi <- lolliEp +
      scale_x_date(breaks=seq(d1, d2, by="5 years"), limits=c(d1,d2), date_labels="%Y") +
      theme_light() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size= 9, angle=45, hjust=1),
        axis.text.y = element_text(size=9), 
        legend.position="right"
        )
      
    ggplotly(state$plot_epi)
      
    })
  
  
  # Episdes: plot download .png
  output$downloadPlot_epi <- downloadHandler(
    filename = function(){
      paste0("Episodes_LolliChart",'.png')
    },
    content = function(file){
      ggsave(file, plot = state$plot_epi, width = 26, height = 12, units = "cm", dpi = 300)
    }
  )
  
  
  
  
  # INDEX TAB ---------------------------------------------------------------
  
  observeEvent(input$run_button_index,{
    
    # keep unit choice
    state$unit_var <- ifelse(input$unit_var == '1','days', ifelse(input$unit_var == '2', 'months', 'years'))
    # keep intensity choice
    state$intensity <- input$choice_intensity
    # build vector of dates (index period)
    yr_index_start <- lubridate::year(as.Date(input$daterange_index[1]))
    yr_index_end <- lubridate::year(as.Date(input$daterange_index[2]))
    state$yrs_index <- seq(yr_index_start, yr_index_end, 1)
    
    
    # compute d and tau (total units of special timing)
    state$d <- NULL
    state$tau <- NULL
    state$choice_timfoc <- FALSE
    
    if (input$choice_timfoc == '1'){
      state$choice_timfoc <- TRUE
      state$d <- input$d_w3
      if (state$unit_var == 'days') {
        state$tau <- as.Date(paste0("2000","/",state$period_timfoc[2])) - as.Date(paste0("2000","/",state$period_timfoc[1]))
      } else {
        if (state$unit_var == 'months'){
          state$tau <- as.integer(input$end_timfoc_month) - as.integer(input$start_timfoc_month) + 1
        } else {
          state$tau <- as.integer(input$end_timfoc_year) - as.integer(input$start_timfoc_year) + 1
        }
      }
    }
    
    # compute IMPIT index
    state$index <- fun_IMPIT(episodes = state$episodes,
                             unit = state$unit_var,
                             yrs = state$yrs_index,
                             m = input$m,
                             a = input$a_w1,
                             b = input$b_w2,
                             c = input$c_w2,
                             d = state$d,
                             intensity = state$intensity,
                             time_focus = state$choice_timfoc,
                             tau = state$tau)
    
    state$contents_index <- data.frame(time = state$yrs_index, index = state$index)
    
  })
  
  observeEvent(input$run_button_index,{
    
    # Table index: print
    output$contents_index <- DT::renderDataTable({ 
      DT::datatable(state$contents_index, 
                    rownames=F, 
                    options = list(
                      pageLength=10,
                      autoWidth = T,
                      searching = T,
                      search = list(regex=T, caseInsensitive=T)
                      )
                    ) %>%
        formatRound(c(2), 2)
    }) 
    
    # Table index: download .csv
    output$downloadTable_index <- downloadHandler(
      filename = function() {
        paste0("IMPIT_index_table.csv")
      },
      content = function(file) {
        write.csv(state$contents_index, file)
      }
    )
    
    # Plot index: print
    output$plot_index <- renderPlotly({
      
      state$plot_index <- plot_ly(data = state$contents_index) %>%
        add_lines(x = ~time, y = ~index) %>% 
        layout(xaxis = list(title="Year"),
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
                    pageLength=10,
                    autoWidth=T,
                    searching=T,
                    search=list(regex=T, caseInsensitive=T)
                  ),
                  rownames = F) %>%
      formatRound(c(2), 2) %>%
      formatStyle(columns=c(1:2), 'text-align'='centre')
  })
    
  
  # IMPIT index + trend
  output$plot_app_index <- renderPlotly({
    
    xx <- data_index()[ ,1]
    yy <- data_index()[ ,2]
    df_index <- data.frame(xx, yy)
    t1 <- head(xx,1)
    t2 <- tail(xx,1)
    data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
    line.fmt = list(dash="solid", width = 1.5, color=NULL)
    
    m1 <- lm(yy ~ xx)
    m2 <- lm(yy ~ xx + I(xx^2))
    m3 <- lm(log(yy) ~ log(xx))
    
    
    p <- plot_ly(x=xx, y=yy, type="scatter", mode="lines", line=data.fmt, name="Data")
    p <- add_lines(p, x=xx, y=predict(m1), line=line.fmt, name="Linear")
    p <- add_lines(p, x=xx, y=predict(m2), line=line.fmt, name="Quadratic")
    p <- add_lines(p, x=xx, y=exp(coef(m3)[1])*(xx^coef(m3)[2]), line=line.fmt, name="Exponential")
    p
    
  })
  
  
  
  
  # Import Response variable 
  data_resp <- eventReactive(input$resp_file, {
    
    if (is.null(input$resp_file)) return(NULL)
    
    read.csv(file=input$resp_file$datapath,
             header=T, 
             stringsAsFactors=F)
  })
  
  # Response variable: table
  output$contents_app_resp <- DT::renderDataTable({
    DT::datatable(data_resp(),
                  options = list(
                    pageLength=10,
                    autoWidth=T,
                    searching=T,
                    search=list(regex=T, caseInsensitive=T)
                  ),
                  rownames = F) %>%
      formatRound(c(2), 2) %>%
      formatStyle(columns=c(1:2), 'text-align'='centre')
  }) 
  

  # Response variable: plot
  output$plot_app_resp <- renderPlotly({
    
    xx <- data_resp()[ ,1]
    yy <- data_resp()[ ,2]
    df_resp <- data.frame(xx, yy)
    t1 <- head(xx,1)
    t2 <- tail(xx,1)
    
    p <- plot_ly(x=xx, y=yy, type="scatter", mode="lines")
    p
    
  })
  
  
  # Correlation analysis (Response vs IMPIT index): plot
  output$plot_corr_application <- renderPlotly({
    
    xx <- data_index()[ ,2]
    yy <- data_resp()[ ,2]
    yy <- log(yy)
    
    
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
    col.reg <- ifelse(res.LR$pVal <= 0.05, "#4292C6", "grey40")
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
    
    
    
    data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
    line.fmt = list(dash="solid", width = 1.5, color=NULL)
    
    mod.pred = predict(mod, type="response", se.fit=TRUE)
    mod.df = data.frame(x=xx, 
                        y=mod.pred$fit,
                        lb=as.numeric(mod.pred$fit - (1.96 * mod.pred$se.fit)),
                        ub=as.numeric(mod.pred$fit + (1.96 * mod.pred$se.fit)))
    mod.df = mod.df[order(mod.df$x),]

    pp <- plot_ly(x=xx, y=yy)
    pp <- add_text(pp, text = ~year.text, textposition="top center", showlegend = F)
    pp <- add_ribbons(pp, x=mod.df$x, ymin=mod.df$lb, ymax=mod.df$ub, name="95% CI", line=list(color="grey60", opacity=0.4, width=0))
    pp <- add_lines(pp, x=xx, y=mod.pred$fit, name="Linear Regression", line=list(color=col.reg, width=2))
    #pp = layout(pp, title="Outcome")
    
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

  
  
  
  
}

#shinyApp(ui, server)