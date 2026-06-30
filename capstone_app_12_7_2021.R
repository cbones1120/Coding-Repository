# This is a Shiny web application. 
# You can run the application by clicking the 'Run App' button above.
# You can upload csv file, filter data, view plots and find the optimal ad spend in this app.

options(shiny.maxRequestSize = 30*1024^2)
# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(tidyverse)
library(readr)
library(ggplot2)
library(forcats)
library(glmnet)
library(broom)
library(shinydashboard)
library(plyr)
library(modelr)
#library(radiant)
#library(Metrics)
####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('Power Digital * UCSD'),
                
                
                tabsetPanel(
                    tabPanel("Summary", fluid = TRUE,
                             sidebarLayout(sidebarPanel(
                                 width = 4,
                                 HTML("<h3>Input files</h3>"),
                                 
                                 fileInput("file1", "Choose CSV File", accept = ".csv"),
                                 checkboxInput("header", "Header", TRUE),
                                 actionButton("submitbutton", "Submit", class = "btn btn-primary"),
                                 br(),
                                 uiOutput("Box0")
                             ),
                             mainPanel(
                                 tags$label(h3('Fields')), # Status/Output Text Box
                                 verbatimTextOutput('summary'),
                                 tags$label(h3('Descriptive Statistics')),
                                 verbatimTextOutput('des_stat')
                                 
                             ))),
                    
                    tabPanel('View', fluid = TRUE,
                             sidebarLayout(
                               sidebarPanel(
                                 width = 4,
                                 HTML("<h3>Filter the dataset</h3>"),
                                 helpText('Note: type filter conditions in format of Field Name == or > or < value, e.g. ad_spend < 1000. If you have several conditions, make sure place & sign between conditions'),
                                 checkboxInput("filter_check_box", "Filter data", value = TRUE),
                                 uiOutput('filter_page_box'),
                                 HTML("<h3>Download the dataset</h3>"),
                                 helpText('Select the download format'),
                                 radioButtons('download_type', 'Format type:',
                                              choices = c('CSV')),
                                 br(),
                                 helpText('Click on the download button to download the dataset observations'),
                                 downloadButton('downloadData', 'Download',class = "btn btn-primary")
                               ),
                               mainPanel(
                                 DT::dataTableOutput('filtered_dataset'),
                               )
                             )),
                    
                    tabPanel('Distribution', fluid = TRUE,
                             sidebarLayout(
                               sidebarPanel(
                               width = 4,
                               HTML("<h3>Distribution</h3>"),
                               HTML("<h4>Numeric variables:</h4>"),
                               uiOutput('var_to_hist'),
                               sliderInput('bin', 'Number of bins:', min = 3, max = 30, value = 10),
                               br(),
                               HTML("<h4>Categorical variables:</h4>"),
                               uiOutput('var_to_fre')
                               ),
                            
                             mainPanel(
                             box(plotOutput('histPlot')),                                 
                             box(plotOutput('freqPlot'))
                             )
                             #Boxes to display the plots                                
                  
                    )),
                    tabPanel('Dashboard', fluid = TRUE,
                             sidebarLayout(
                               sidebarPanel(
                                 width = 4,
                                 HTML("<h3>Filter</h3>"),
                                 uiOutput('filter1'),
                                 uiOutput('filter2'),
                                 uiOutput('filter3')
                               ),
                               mainPanel(
                                 box(plotOutput('scatterplot1')),
                                 box(plotOutput('scatterplot2'))
                               )
                             )
                      
                    ),
                    
                    tabPanel("Optimal", fluid = TRUE,
                             sidebarLayout(sidebarPanel(
                                 HTML("<h3>Input parameters</h3>"),
                                 
                                 sliderInput("target",label = "ROAS:",
                                             min = 0, max = 10,
                                             value = 1),
                                 HTML("<h4>Select parameters</h3>"),
                                 checkboxInput("cb", "Client id as input", value = FALSE),
                                 uiOutput("Box1"),
                                 uiOutput("Box2"),
                                 uiOutput("Box3"),
                                 uiOutput("Box4"),
                                 uiOutput("Box5")),
                                 mainPanel(
                                     tags$label(h3('Status/Output')), # Status/Output Text Box
                                     verbatimTextOutput('choice'),
                                     verbatimTextOutput('contents'),
                                   #  plotOutput('plot1'),
                                     plotOutput('plot2'),
                                     tags$label(h4('Optimal Ad Spend:')),
                                     verbatimTextOutput('optimal'),
                                    tags$label(h4('Model Performance:')),
                                    verbatimTextOutput('performance'))))))


####################################
# Server                           #
####################################

server <- function(input, output) {

    df <- eventReactive(input$submitbutton,  { 
        pdm <- read.csv(input$file1$datapath,
                        header = input$header)
        ## transform variable
        pdm <- mutate(pdm, ad_spend_ln=log(ad_spend)) %>%
            mutate(pdm, conversions_value_ln=log(conversions_value))
        ## create new variable(s)
        pdm <- mutate(pdm, net=conversions_value-ad_spend) %>%
            mutate(pdm, roas=(conversions_value-ad_spend)/ad_spend) %>%
            mutate(pdm, ctr=clicks/impressions) %>%
            mutate(pdm, conversion_rate=conversions_cnt/clicks)%>%
            mutate(pdm, month = month(date))

        pdm$client_id <- as.character(pdm$client_id)
        pdm$industry_id <- as.character(pdm$industry_id)
        pdm$date <- as.Date(pdm$date)
        pdm$month <- as.character(pdm$month)
        ## price level
        a <- pdm %>% 
          group_by(client_id) %>%
          summarize(value_per_conv_median = median(value_per_conv))
        # merge groupby dataframe with full data
        pdm <- merge(a, pdm)
        # cut into bins
        pdm$price_level <- ntile(pdm$value_per_conv_median, n=3)
        pdm$price_level <- as.character(pdm$price_level)
        pdm$price_level <- fct_recode(pdm$price_level, 
                                      "Low"='1','Medium'='2','High'='3')
        df<- pdm[ , -which(names(pdm) %in% c("value_per_conv_median"))]
    })
    
    output$summary <- renderPrint(colnames(df()))
    #output$summary <- renderPrint(c('Not-specified', levels(df()$price_level)))
    
    output$Box0 = renderUI({
      req(df())
      selectInput('field', 'Select a field', c(colnames(df())))
      })
    
    output$des_stat <- renderPrint({
      req(df())
      # summary({df()$field})
      summary(df()[[input$field]])
      })
    
    # filter page
    output$filter_page_box <- renderUI({
      req(df())
      if (input$filter_check_box == TRUE) {
        textAreaInput("filter_text", "Filter the data", "date > '2020-10-31' & channel_grouping___template != 'Steelhouse' & ad_spend > 1 & clicks != 0 & client_id != '1783' & client_id != '1737' & conversions_value >0 & net != 0", height = '400px', width = '400px')
      }
    })
    
    # for data table
    df_2 <- reactive({
      req(df())
      df_2 <- df() %>% filter(!! rlang::parse_expr((input$filter_text)))
    })
    
    output$filtered_dataset <- DT::renderDataTable({
      if (input$filter_check_box == TRUE){
        DT::datatable(df_2())
      } else {
        DT::datatable(df())
      }
    })
    
     fileext <- reactive({
       switch(input$download_type, 'CSV' = 'csv')
     })
     
     output$downloadData <- downloadHandler(
         filename = function(){
           if (input$filter_check_box == TRUE) {
             paste('filtered_df', fileext(), sep = '.')
           }
           else {
             paste('full_df', fileext(), sep = '.')
           }
           },
         content = function(file){
           sep <- switch(input$download_type, 'CSV' = ',')
           if (input$filter_check_box == TRUE) {
             write.table(df_2(), file, sep = sep, row.names = FALSE)
           }
           else {
             write.table(df(), file, sep = sep, row.names = FALSE)
           }
         }
     )
     
    ## plot page
    output$var_to_hist <- renderUI({
      req(df())
      selectInput('var_to_histogram', 'Select a numeric variable:', choices = colnames(df() %>% select_if(is.numeric)))
      })
    
    output$var_to_fre <- renderUI({
      req(df())
      selectInput('var_to_freq', 'Select a vategorical variable:', choices = colnames(df() %>% select_if(is.character)))
    })
    
    output$histPlot <- renderPlot({
      req(df())
      if (input$filter_check_box == TRUE) {
      ggplot(df_2(), aes(x = df_2()[[input$var_to_histogram]]))+       
        geom_histogram(bins = input$bin, stat = "bin", fill = 'steelblue3',                      
                       color = 'lightgrey')+      
        theme(axis.text = element_text(size = 12),            
              axis.title = element_text(size = 14),            
              plot.title = element_text(size = 16, face = 'bold'))+      
        labs(title = sprintf('Histogram plot of the variable %s', input$var_to_histogram),           
             x = sprintf('%s', input$var_to_histogram),y = 'Frequency')
      } else {
        ggplot(df(), aes(x = df()[[input$var_to_histogram]]))+       
          geom_histogram(bins = input$bin, stat = "bin", fill = 'steelblue3',                      
                         color = 'lightgrey')+      
          theme(axis.text = element_text(size = 12),            
                axis.title = element_text(size = 14),            
                plot.title = element_text(size = 16, face = 'bold'))+      
          labs(title = sprintf('Histogram plot of the variable %s', input$var_to_histogram),           
               x = sprintf('%s', input$var_to_histogram),y = 'Frequency')
      }
    })
    output$freqPlot <- renderPlot({
      req(df())
      if (input$filter_check_box == TRUE){
        if (input$var_to_freq == 'client_id'){
          top_10<-as.data.frame(dplyr::count(df_2(), client_id, sort = TRUE))
          top_10 <- top_10 %>% top_n(10)
          ggplot(data = top_10, aes(x = client_id, y = n))+      
            geom_bar(stat="identity", fill = 'mediumseagreen',                
                     width = 0.5)+
            geom_text(aes(label=n), vjust=-0.3, size=3.5) + 
            theme(axis.text.x = element_text(angle=0, hjust=1, vjust=.5),
                  axis.text.y = element_blank(),            
                  axis.ticks.y = element_blank(),            
                  axis.text = element_text(size = 12),            
                  axis.title = element_text(size = 14),            
                  plot.title = element_text(size = 16, face="bold"))+      
            labs(title = sprintf('Top 10 frecuency plot of the variable %s', input$var_to_freq),           
                 x = sprintf('%s', input$var_to_freq), y = 'Count')
        }
        else if (input$var_to_freq == 'month') {
          month_freq<-as.data.frame(dplyr::count(df_2(), month, sort = TRUE))
          month_freq$month <- as.integer(month_freq$month)
          month_freq <- month_freq[with(month_freq, order(month)),]
          month_freq$month <- as.character(month_freq$month)
          ggplot(data = month_freq, aes(x = month, y = n))+      
            geom_bar(stat="identity", fill = 'mediumseagreen',                
                     width = 0.5)+
            geom_text(aes(label=n), vjust=-0.3, size=3.5) + 
            theme(axis.text.x = element_text(angle=0, hjust=1, vjust=.5),
                  axis.text.y = element_blank(),            
                  axis.ticks.y = element_blank(),            
                  axis.text = element_text(size = 12),            
                  axis.title = element_text(size = 14),            
                  plot.title = element_text(size = 16, face="bold"))+      
            labs(title = sprintf('Top 10 frecuency plot of the variable %s', input$var_to_freq),           
                 x = sprintf('%s', input$var_to_freq), y = 'Count')
        }
        else {
          ggplot(data = df_2(), aes(x = df_2()[[input$var_to_freq]]))+      
            geom_bar(stat = 'count', fill = 'mediumseagreen',                
                     width = 0.5)+      
            stat_count(geom = 'text', size = 4,                 
                       aes(label = ..count..),                 
                       position = position_stack(vjust = 1.03))+      
            theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
                  axis.text.y = element_blank(),            
                  axis.ticks.y = element_blank(),            
                  axis.text = element_text(size = 12),            
                  axis.title = element_text(size = 14),            
                  plot.title = element_text(size = 16, face="bold"))+      
            labs(title = sprintf('Frecuency plot of the variable %s', input$var_to_freq),           
                 x = sprintf('%s', input$var_to_freq), y = 'Count')
        }
      }
      else if (input$filter_check_box == FALSE) {
        if (input$var_to_freq == 'client_id'){
          top_10<-as.data.frame(dplyr::count(df(), client_id, sort = TRUE))
          top_10 <- top_10 %>% top_n(10)
          ggplot(data = top_10, aes(x = client_id, y = n))+      
            geom_bar(stat="identity", fill = 'mediumseagreen',                
                     width = 0.5)+
            geom_text(aes(label=n), vjust=-0.3, size=3.5) + 
            theme(axis.text.x = element_text(angle=0, hjust=1, vjust=.5),
                  axis.text.y = element_blank(),            
                  axis.ticks.y = element_blank(),            
                  axis.text = element_text(size = 12),            
                  axis.title = element_text(size = 14),            
                  plot.title = element_text(size = 16, face="bold"))+      
            labs(title = sprintf('Top 10 frecuency plot of the variable %s', input$var_to_freq),           
                 x = sprintf('%s', input$var_to_freq), y = 'Count')}
        else if (input$var_to_freq == 'month') {
          month_freq<-as.data.frame(dplyr::count(df(), month, sort = TRUE))
          month_freq <- month_freq %>%arrange(match(month, c("1", "2", "3", "4", "5", "6", "7", "8", "9","10", "11", "12")), desc(n))
          # month_freq$month <- as.integer(month_freq$month)
          # month_freq <- month_freq[with(month_freq, order(month)),]
          # month_freq$month <- as.character(month_freq$month)
          ggplot(data = month_freq, aes(x = month, y = n))+      
            geom_bar(stat="identity", fill = 'mediumseagreen',                
                     width = 0.5)+
            geom_text(aes(label=n), vjust=-0.3, size=3.5) + 
            theme(axis.text.x = element_text(angle=0, hjust=1, vjust=.5),
                  axis.text.y = element_blank(),            
                  axis.ticks.y = element_blank(),            
                  axis.text = element_text(size = 12),            
                  axis.title = element_text(size = 14),            
                  plot.title = element_text(size = 16, face="bold"))+      
            labs(title = sprintf('Top 10 frecuency plot of the variable %s', input$var_to_freq),           
                 x = sprintf('%s', input$var_to_freq), y = 'Count')
        }
        else {
          ggplot(data = df(), aes(x = df()[[input$var_to_freq]]))+      
            geom_bar(stat = 'count', fill = 'mediumseagreen',                
                     width = 0.5)+      
            stat_count(geom = 'text', size = 4,                 
                       aes(label = ..count..),                 
                       position = position_stack(vjust = 1.03))+      
            theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
                  axis.text.y = element_blank(),            
                  axis.ticks.y = element_blank(),            
                  axis.text = element_text(size = 12),            
                  axis.title = element_text(size = 14),            
                  plot.title = element_text(size = 16, face="bold"))+      
            labs(title = sprintf('Frecuency plot of the variable %s', input$var_to_freq),           
                 x = sprintf('%s', input$var_to_freq), y = 'Count')
        }
      }
    })
    
    # dashboard page
    output$filter1 <- renderUI({
      req(df())
      selectInput('filter_industry', 'Select an industry:', c(unique(df()$industry)))
    })
    
    output$filter2 <- renderUI({
      req(df())
      selectInput('filter_platform', 'Select a platform:', c(unique(df()$channel_grouping___template)))
    })
    
    output$filter3 <- renderUI({
      req(df())
      selectInput('filter_tactic', 'Select a tactic:', c(unique(df()$campaign_tactic___ecommerce___template)))
    })
    
    output$scatterplot1 <- renderPlot({
      req(df())
      if (input$filter_check_box == TRUE) {
        df_2() %>% filter(industry == input$filter_industry & 
                        channel_grouping___template == input$filter_platform & 
                        campaign_tactic___ecommerce___template == input$filter_tactic) %>%
          ggplot(aes(x = ad_spend_ln, y = conversions_value_ln)) +
          geom_point(size = 2, shape = 23, color = 'blue') +
          geom_smooth(method = lm, linetype = 'dashed', color = 'darkred') +
          labs(title = 'ad_spend_ln vs conversion_value_ln', x = 'ad_spend_ln', y = 'conversion_value_ln') +
          theme(plot.title = element_text(hjust = 0.5))
      }
      else {
        df() %>% filter(industry == input$filter_industry & 
                          channel_grouping___template == input$filter_platform & 
                          campaign_tactic___ecommerce___template == input$filter_tactic) %>%
          ggplot(aes(x = ad_spend_ln, y = conversions_value_ln)) +
          geom_point(size = 2, shape = 23, color = 'blue') +
          geom_smooth(method = lm, linetype = 'dashed', color = 'darkred') +
          labs(title = 'ad_spend_ln vs conversion_value_ln', x = 'ad_spend_ln', y = 'conversion_value_ln') +
          theme(plot.title = element_text(hjust = 0.5))
      }
    })
    
    output$scatterplot2 <- renderPlot({
      req(df())
      if (input$filter_check_box == TRUE) {
        df_2() %>% filter(industry == input$filter_industry & 
                              channel_grouping___template == input$filter_platform & 
                              campaign_tactic___ecommerce___template == input$filter_tactic) %>%
          ggplot(aes(x = ad_spend_ln, y = roas)) +
          geom_point(size = 2, shape = 23, color = 'blue') +
          geom_smooth(method = lm, linetype = 'dashed', color = 'darkred') +
        labs(title = 'ad_spend_ln vs roas', x = 'ad_spend_ln', y = 'roas') +
          theme(plot.title = element_text(hjust = 0.5))
      }
      else {
        df() %>% filter(industry == input$filter_industry & 
                          channel_grouping___template == input$filter_platform & 
                          campaign_tactic___ecommerce___template == input$filter_tactic) %>%
          ggplot(aes(x = ad_spend_ln, y = roas)) +
          geom_point(size = 2, shape = 23, color = 'blue') +
          geom_smooth(method = lm, linetype = 'dashed', color = 'darkred') +
          labs(title = 'ad_spend_ln vs roas', x = 'ad_spend_ln', y = 'roas') +
          theme(plot.title = element_text(hjust = 0.5))
      }
    })
    
    # model page
    output$Box1 = renderUI(
      if (input$cb == FALSE) {
        selectInput("industry","Select an industry",
                                       c('Not-specified', sort(unique(df()$industry))))
      })
      
    output$Box2 = renderUI(
      if (input$cb == FALSE) {
        selectInput("platform","Select a platform",
                                       c('Not-specified', sort(unique(df()$channel_grouping___template))))
      })
      
    output$Box3 = renderUI(
      if (input$cb == FALSE){
        selectInput("tactic","Select a tactic",
                                       c('Not-specified', sort(unique(df()$campaign_tactic___ecommerce___template))))
      })
      
    output$Box4 = renderUI(
      if (input$cb == FALSE){
        selectInput("price_level","Select a price level",
                                       c('Not-specified', levels(df()$price_level)))
      })
      
    output$Box5 = renderUI(
      if (input$cb == TRUE){
        selectInput("client_id","Select your client id (Only for existing clients)",
                                       c('Not-specified', sort(unique(df()$client_id))))
      })
      
    opt <- reactive({
      req(df())
      if(input$filter_check_box == TRUE){
        var1 <- c('industry','channel_grouping___template',
                  'campaign_tactic___ecommerce___template','price_level','client_id')
        value_ls <- c(input$industry, input$platform,input$tactic,input$price_level,
                    input$client_id)
        var_index1 <- which(value_ls!='Not-specified')
        if (length(var1[var_index1]) == 0) {
          lr_formula <- 'conversions_value_ln ~ ad_spend_ln'
      } 
        else {
          lr_formula <- paste0('conversions_value_ln ~ ad_spend_ln+',paste(var1[var_index1],collapse = '+'))
      }
        lr <- lm(lr_formula, data = df_2())
        results.df <- tidy(lr,conf.int = T)
        result_ls <- paste(var1[var_index1], value_ls[var_index1], sep ="")
        result_df <-data.frame(term = result_ls)
        final_result_df <- merge(results.df[, c('term', 'estimate', 'conf.low', 'conf.high')], result_df, by = 'term', all = FALSE)
        
        #optimal ad spend
        beta0<-results.df$estimate[1]
        beta1<-results.df$estimate[2]
        coef_total <- sum(final_result_df$estimate)
        
        # low bound
        beta0_l<-results.df$conf.low[1]
        beta1_l<-results.df$conf.low[2]
        low_bound_total <- sum(final_result_df$conf.low)

        # high bound
        beta0_h<-results.df$conf.high[1]
        beta1_h<-results.df$conf.high[2]
        high_bound_total <- sum(final_result_df$conf.high)

        (beta1_l*exp(beta0_l)/input$target)^(1/(1-beta1_l))
        
        optimal_ad_spend_daily <- round((beta1*exp(beta0+coef_total)/input$target)^(1/(1-beta1)),2)
        optimal_ad_spend <- round((beta1*exp(beta0+coef_total)/input$target)^(1/(1-beta1))*30,2)
        optimal_ad_spend_l <- round((beta1_l*exp(beta0_l+low_bound_total)/input$target)^(1/(1-beta1_l))*30,2)
        optimal_ad_spend_h <- round((beta1_h*exp(beta0_h+high_bound_total)/input$target)^(1/(1-beta1_h))*30,2)

        result_to_print <- cat( 'Daily optimal ad spend is $', optimal_ad_spend_daily, 
                                  '. \nMonthly optimal ad spend is $', optimal_ad_spend, '. \nConfidence interval is between $', optimal_ad_spend_l, ' and $', optimal_ad_spend_h, '.',sep='')

        return(invisible(result_to_print))
      }
      else {
        var1 <- c('industry','channel_grouping___template',
                  'campaign_tactic___ecommerce___template','price_level','client_id')
        value_ls <- c(input$industry, input$platform,input$tactic,input$price_level,
                      input$client_id)
        var_index1 <- which(value_ls!='Not-specified')
        if (length(var1[var_index1]) == 0) {
          lr_formula <- 'conversions_value_ln ~ ad_spend_ln'
        } 
        else {
          lr_formula <- paste0( 'conversions_value_ln ~ ad_spend_ln+',paste(var1[var_index1],collapse = '+'))
        }
        lr <- lm(lr_formula, data = df())
        results.df <- tidy(lr,conf.int = T)
        result_ls <- paste(var1[var_index1], value_ls[var_index1], sep ="")
        result_df <-data.frame(term = result_ls)
        final_result_df <- merge(results.df[, c('term', 'estimate', 'conf.low', 'conf.high')], result_df, by = 'term', all = FALSE)
        
        #optimal ad spend
        beta0<-results.df$estimate[1]
        beta1<-results.df$estimate[2]
        coef_total <- sum(final_result_df$estimate)
        
        # low bound
        beta0_l<-results.df$conf.low[1]
        beta1_l<-results.df$conf.low[2]
        low_bound_total <- sum(final_result_df$conf.low)

        # high bound
        beta0_h<-results.df$conf.high[1]
        beta1_h<-results.df$conf.high[2]
        high_bound_total <- sum(final_result_df$conf.high)

        (beta1_l*exp(beta0_l)/input$target)^(1/(1-beta1_l))
        
        optimal_ad_spend_daily <- round((beta1*exp(beta0+coef_total)/input$target)^(1/(1-beta1)),2)
        optimal_ad_spend <- round((beta1*exp(beta0+coef_total)/input$target)^(1/(1-beta1))*30,2)
        optimal_ad_spend_l <- round((beta1_l*exp(beta0_l+low_bound_total)/input$target)^(1/(1-beta1_l))*30,2)
        optimal_ad_spend_h <- round((beta1_h*exp(beta0_h+high_bound_total)/input$target)^(1/(1-beta1_h))*30,2)

        result_to_print <- cat( 'Daily optimal ad spend is $', optimal_ad_spend_daily, 
                                '. \nMonthly optimal ad spend is $', optimal_ad_spend, '. \nConfidence interval is between $', optimal_ad_spend_l, ' and $', optimal_ad_spend_h, '.',sep='')
        
        return(invisible(result_to_print))
      }
    })
    
    # print parameters
    output$choice <- renderPrint({
      req(df())
      value_ls <- c(input$industry, input$platform,input$tactic,input$price_level,
                    input$client_id)
      var_index1 <- which(value_ls!='Not-specified')
      paste('Input selected:', value_ls[var_index1], sep = ' ')
      })
    
    output$optimal <- renderPrint({
      req(df())
      opt()
      })
    
    # filtered dataset
    df_show<- reactive({
      req(df())
      if(input$filter_check_box == TRUE) {
        df_1 <- df_2()
        if (input$cb == FALSE) {
      
          if (input$industry=='Not-specified'){
              df_1<-df_1
          } else {df_1<-df_2()[which(df_2()$industry==input$industry),]}
          if (input$platform=='Not-specified'){
              df_1<-df_1
          } else {df_1<-df_1[which(df_1$channel_grouping___template==input$platform),]}
          if (input$tactic=='Not-specified'){
              df_1<-df_1
          } else {df_1<-df_1[which(df_1$campaign_tactic___ecommerce___template==input$tactic),]}
          if (input$price_level=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_1$price_level==input$price_level),]}
          # if (input$client_id=='Not-specified'){
          #   df_1<-df_1
          # } else {df_1<-df_1[which(df_1$client_id==input$client_id),]}
    
        } else {

          if (input$client_id=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_2()[which(df_1$client_id==input$client_id),]}
          
          if (input$industry=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_2()$industry==input$industry),]}
          if (input$platform=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_1$channel_grouping___template==input$platform),]}
          if (input$tactic=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_1$campaign_tactic___ecommerce___template==input$tactic),]}
          if (input$price_level=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_1$price_level==input$price_level),]}
          
        df_show <- df_1
        
        }
      } else {
        df_1 <- df()
        if (input$cb == FALSE) {
            
          if (input$industry=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df()[which(df()$industry==input$industry),]}
          if (input$platform=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_1$channel_grouping___template==input$platform),]}
          if (input$tactic=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_1$campaign_tactic___ecommerce___template==input$tactic),]}
          if (input$price_level=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_1$price_level==input$price_level),]}
          #if (input$client_id=='Not-specified'){
           # df_1<-df_1
          #} else {df_1<-df_1[which(df_1$client_id==input$client_id),]}
          #df_show <- df_1
        } else {
          if (input$client_id=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df()[which(df_1$client_id==input$client_id),]}
          
          if (input$industry=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_2()$industry==input$industry),]}
          if (input$platform=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_1$channel_grouping___template==input$platform),]}
          if (input$tactic=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_1$campaign_tactic___ecommerce___template==input$tactic),]}
          if (input$price_level=='Not-specified'){
            df_1<-df_1
          } else {df_1<-df_1[which(df_1$price_level==input$price_level),]}
        }
        df_show <- df_1}
        })
    
    # display filtered dataset rows and columns
    output$contents <- renderPrint({
      req(df())
      paste('There are', nrow(df_show()), 'records meeting the selection criteria.',sep = ' ')
    })
    
    # scatter plot of filtered dataset ad_spend vs conversion value
    # output$plot1 <- renderPlot({
    #   req(df())
    #   ggplot(df_show(), aes(x=ad_spend_ln, y=conversions_value_ln)) +
    #     geom_point(size=2, shape='o') + 
    #     geom_smooth(se=TRUE, method="lm")
    # })
    
    output$plot2 <- renderPlot({
      req(df())
      
      if(input$filter_check_box == TRUE){
        var1 <- c('industry','channel_grouping___template',
                  'campaign_tactic___ecommerce___template','price_level','client_id')
        value_ls <- c(input$industry, input$platform,input$tactic,input$price_level,
                      input$client_id)
        var_index1 <- which(value_ls!='Not-specified')
        if (length(var1[var_index1]) == 0) {
          lr_formula <- 'conversions_value_ln ~ ad_spend_ln'
        } 
        else {
          lr_formula <- paste0('conversions_value_ln ~ ad_spend_ln+',paste(var1[var_index1],collapse = '+'))
        }
        lr <- lm(lr_formula, data = df_2())
        results.df <- tidy(lr,conf.int = T)
        result_ls <- paste(var1[var_index1], value_ls[var_index1], sep ="")
        result_df <-data.frame(term = result_ls)
        final_result_df <- merge(results.df[, c('term', 'estimate', 'conf.low', 'conf.high')], result_df, by = 'term', all = FALSE)
        
        #optimal ad spend
        beta0<-results.df$estimate[1]
        beta1<-results.df$estimate[2]
        coef_total <- sum(final_result_df$estimate)
        optimal_ad_spend_daily <- round((beta1*exp(beta0+coef_total)/input$target)^(1/(1-beta1)),2)
    
      }
      else {
        var1 <- c('industry','channel_grouping___template',
                  'campaign_tactic___ecommerce___template','price_level','client_id')
        value_ls <- c(input$industry, input$platform,input$tactic,input$price_level,
                      input$client_id)
        var_index1 <- which(value_ls!='Not-specified')
        if (length(var1[var_index1]) == 0) {
          lr_formula <- 'conversions_value_ln ~ ad_spend_ln'
        } 
        else {
          lr_formula <- paste0( 'conversions_value_ln ~ ad_spend_ln+',paste(var1[var_index1],collapse = '+'))
        }
        lr <- lm(lr_formula, data = df())
        results.df <- tidy(lr,conf.int = T)
        result_ls <- paste(var1[var_index1], value_ls[var_index1], sep ="")
        result_df <-data.frame(term = result_ls)
        final_result_df <- merge(results.df[, c('term', 'estimate', 'conf.low', 'conf.high')], result_df, by = 'term', all = FALSE)
        
        #optimal ad spend
        beta0<-results.df$estimate[1]
        beta1<-results.df$estimate[2]
        coef_total <- sum(final_result_df$estimate)
        optimal_ad_spend_daily <- round((beta1*exp(beta0+coef_total)/input$target)^(1/(1-beta1)),2)
      }
      
      optimal_point = data.frame(c(optimal_ad_spend_daily,exp(beta0+coef_total)* optimal_ad_spend_daily^beta1))
      
      ggplot(df_show()) +
        geom_point(aes(x = ad_spend, y=conversions_value), size=2, shape='o') + 
        geom_smooth(aes(x = ad_spend, y=conversions_value), se=TRUE)+
        geom_line(aes(x = ad_spend, y=exp(beta0+coef_total) * ad_spend^beta1), color = 3, size = 2) + 
        geom_point(aes(x = optimal_ad_spend_daily, y = exp(beta0+coef_total)* optimal_ad_spend_daily^beta1), color = 2, size = 5, shape = 17) + 
        # geom_text(aes(x = optimal_ad_spend_daily, y = exp(beta0+coef_total)* optimal_ad_spend_daily^beta1), label = 'Diminishing Marginal Return', color = 2) +
        scale_x_continuous(limits = c(0, max(2000, optimal_ad_spend_daily*2))) +
        scale_y_continuous(limits = c(0, max(4000, 2*exp(beta0+coef_total)* optimal_ad_spend_daily^beta1)))
    })
    
    # performance metrics of moedel
    perf <-reactive({
      req(df())
      if(input$filter_check_box == TRUE){
        var <- c('industry','channel_grouping___template',
             'campaign_tactic___ecommerce___template','price_level','client_id')
        var_index <- which(c(input$industry, input$platform,input$tactic,input$price_level,
                         input$client_id)!='Not-specified')
        if(length(var[var_index]) == 0 ){
          lr_formula <- 'conversions_value_ln ~ ad_spend_ln'
          }
        else{
          lr_formula <- paste0('conversions_value_ln ~ ad_spend_ln+',paste(var[var_index],collapse = '+'))}
        lr <- lm(lr_formula, data = df_2())
        performance.df <- data.frame(R2 = summary(lr)$r.squared,
                                   RMSE = rmse(lr, data = df_2()),
                                   MAE = mae(lr, data = df_2()))
        return(performance.df)
      }
      else {
        var <- c('industry','channel_grouping___template',
                 'campaign_tactic___ecommerce___template','price_level','client_id')
        var_index <- which(c(input$industry, input$platform,input$tactic,input$price_level,
                             input$client_id)!='Not-specified')
        if(length(var[var_index]) == 0 ){
          lr_formula <- 'conversions_value_ln ~ ad_spend_ln'
        }
        else{
          lr_formula <- paste0('conversions_value_ln ~ ad_spend_ln+',paste(var[var_index],collapse = '+'))}
        lr <- lm(lr_formula, data = df())
        performance.df <- data.frame(R2 = summary(lr)$r.squared,
                                     RMSE = rmse(lr, data = df()),
                                     MAE = mae(lr, data = df()))
        return(performance.df)
      }
    })
    
    output$performance <- renderPrint({
      req(df())
      perf()
    })
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)

