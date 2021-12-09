library(shiny)
library(rsconnect)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidybayes)
library(tidymodels)
library(rstan)
library(rstanarm)
library(bayesplot)
library(bayesrules)
library(broom.mixed)
library(pracma)
library(probably)
library(DT)

#log_hs_model_updated<-readRDS("/Users/Apple/Desktop/454_project/log_hs_model_updated.rds")
log_model<-readRDS("log_model.rds")
log_hs_model<-readRDS("log_hs_model.rds")
log_hs_model_updated<-readRDS("log_hs_model_updated.rds")
bayes_lasso_log_updated<-readRDS("bayes_lasso_log_updated.rds")

#R Shiny ui
ui<-fluidPage(
  titlePanel("Hotel Cancellation Explorer"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             selectInput(inputId = "userchoice1", 
                                         label = "Input Hotel type Here", 
                                         choices = list("City Hotel","Resort Hotel"), 
                                         multiple = FALSE),
                             selectInput(inputId = "userchoice2", 
                                         "Input Country Here", 
                                         choices = list("PRT","ESP","GBR","NLD","AUT","FRA","BEL",
                                                        "BRA","USA","ITA","DEU","IRL","CN","POL",
                                                        "RUS","NOR","CHN","CHE","ISR","SWE"),
                                         selected=list("PRT"),
                                         multiple = FALSE), 
                             selectInput(inputId = "userchoice3", 
                                         label = "Input Year Here", 
                                         choices= list("2015","2016","2017"),
                                         selected=list("2016"),
                                         multiple = FALSE),
                             selectInput(inputId = "userchoice4", 
                                         label = "Input Month Here", 
                                         choices= list("January","February","March","April",
                                                       "May","June","July","August","September",
                                                       "October","November","December"),
                                         selected=list("January"),
                                         multiple = FALSE),
                             selectInput(inputId="userchoice5",
                                         label="arrival date week number",
                                         choices= list("1","2","3",
                                                       "4","5","6","7","8",
                                                       "9","10","11","12","13","14","15","16","17",
                                                       "18","19","20","21","22","23",
                                                       "24","25","26","27","28",
                                                       "29","30","31","32","33","34","35","36","37",
                                                       "38","39","40","41","42","43",
                                                       "44","45","46","47","48","49","50","51","52","53"),
                                         selected=list("1"),
                                         multiple = FALSE),
                             sliderInput(inputId="userchoice6",
                                         "number of adults",
                                         min = 0,
                                         max = 4,
                                         value = 2),
                             sliderInput(inputId="userchoice7",
                                         "number of children",
                                         min = 0,
                                         max = 2,
                                         value = 1),
                             sliderInput(inputId="userchoice8",
                                         "number of babies",
                                         min = 0,
                                         max = 1,
                                         value = 0),
                             selectInput(inputId = "userchoice9", 
                                         label = "Input Market Segment Here", 
                                         choices= list("Groups","Online TA","Corporate","Direct",
                                                       "Offline TA/TO","Complementary","Aviation"),
                                         selected=list("Groups"),
                                         multiple = FALSE),
                             selectInput(inputId = "userchoice10", 
                                         label = "Input Distribution Channel Here", 
                                         choices= list("TA/TO","GDS","Corporate","Direct"),
                                         selected=list("GDS"),
                                         multiple = FALSE),
                            
                             selectInput(inputId = "userchoice11", 
                                         label = "Input if repeated guest or not Here", 
                                         choices= list("0","1"),
                                         selected=list("0"),
                                         multiple = FALSE),
                             selectInput(inputId = "userchoice12", 
                                         label = "Input if cancelled previously or not Here", 
                                         choices= list("0","1"),
                                         selected=list("0"),
                                         multiple = FALSE),
                        
                             selectInput(inputId = "userchoice13", 
                                         label = "Input Assigned Room Type Here", 
                                         choices= list("A","B","C","D",
                                                       "E","F","G","H","I","K"),
                                         selected=list("A"),
                                         multiple = FALSE),
                             selectInput(inputId = "userchoice14", 
                                         label = "Input Deposit Type Here", 
                                         choices= list("Non Refund","No Deposit","Refundable"),
                                         selected=list("Refundable"),
                                         multiple = FALSE),
                             selectInput(inputId = "userchoice15", 
                                         label = "Input Customer Type Here", 
                                         choices= list("Transient","Transient-Party",
                                                       "Contract","Group"),
                                         selected=list("Contract"),
                                         multiple = FALSE),
                             sliderInput(inputId="userchoice16",
                                         "Input if required parking spaces or not Here",
                                         min = 0,
                                         max = 1,
                                         value = 0),
                             sliderInput(inputId="userchoice17",
                                         "Input total number of special requests",
                                         min = 0,
                                         max = 5,
                                         value = 1),
                             selectInput(inputId = "userchoice18", 
                                         label = "Input Reserved Room Type Here", 
                                         choices= list("A","B","C","D",
                                                       "E","F","G","H","I","K"),
                                         selected=list("A"),
                                         multiple = FALSE),
                             submitButton(text = "Get Predictions")),
                mainPanel("by Xinyi Wang and Yiyang Shi",
                          p("About the data:This data set contains booking information for a city hotel and a resort hotel, and includes information such as when the booking was made, length of stay, the number of adults, children, and/or babies, and the number of available parking spaces, among other things."),
                          p("Who creates the data:The data is originally from the article Hotel Booking Demand Datasets, written by Nuno Antonio, Ana Almeida, and Luis Nunes for Data in Brief, Volume 22, February 2019."),
                          p("Data source:The data was downloaded and cleaned by Thomas Mock and Antoine Bichat for #TidyTuesday during the week of February 11th, 2020.source: https://www.kaggle.com/jessemostipak/hotel-booking-demand"),
                          strong("Model building:"),
                          p("Bayesian Logistic Regression Model"),
                          withMathJax(helpText(p("To model the binary response variable $$Y\\in \\{0,1\\}$$ by predictors $$X_1,X_2...X_{26}$$, suppose we collect n data points. Let $$(Y_i, X_i)$$ denotes the observed data on each case $$i\\in \\{1, 2, ..., n\\}$$. Thus the Bayesian Logistic Regression model is as follows:"))),
                          withMathJax(helpText(p("$$Y_i | \\beta_0, \\beta_1,...,\\beta_{26}, \\sigma \\stackrel{ind}{\\sim} \\text{Bern}(\\pi_i)  \\text{where } \\log\\left(\\frac{\\pi_i}{1 - \\pi_i}\\right) = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 +...+\\beta_{26} X_{26}$$"))),
                          withMathJax(helpText(p("$$\\text{equivalently, } \\frac{\\pi_i}{1 - \\pi_i} = e^{\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 +...+\\beta_{26} X_{26}}$$"))),
                          withMathJax(helpText(p("$$\\text{ and } \\pi_i = \\frac{e^{\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 +...+\\beta_{26} X_{26}}}{e^{\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 +...+\\beta_{26} X_{26}} + 1}$$"))),
                          withMathJax(helpText(p("$$\\beta_{0c}  \\sim N(m_0, s_0^2)$$"))),
                          withMathJax(helpText(p("$$\\beta_1    \\sim N(m_1, s_1^2)$$"))),
                          withMathJax(helpText(p("$$\\beta_2   \\sim N(m_2, s_2^2)$$"))),
                          withMathJax(helpText(p("$$text{ ... }$$"))),
                          withMathJax(helpText(p("$$\\beta_{26}    \\sim N(m_{26}, s_{26}^2)$$"))),
                          withMathJax(helpText(p(""))),
                          withMathJax(helpText(p(""))),
                          withMathJax(helpText(p(""))),
                          p("Bayesian Lasso Logistic Regression Model"),
                          p("Horseshoe Prior"),
                          withMathJax(helpText(p("$$\\pi$$"))),
                          verticalLayout(plotOutput("hs_histgram"),
                                         textOutput("cancel_probability_hs"),
                                         plotOutput("lasso_log_histgram"),
                                         textOutput("cancel_probability_lasso")
                                      ))))

server <- function(input, output){
  output$hs_histgram <- renderPlot({
    cancel_prediction_hs<-posterior_predict(
      log_hs_model_updated,newdata=data.frame(hotel=input$userchoice1,
                                              arrival_date_year=input$userchoice3,
                                              arrival_date_month=input$userchoice4,
                                              arrival_date_week_number=input$userchoice5,
                                              adults=input$userchoice6,children=input$userchoice7,
                                              babies=input$userchoice8,
                                              country=input$userchoice2,market_segment=input$userchoice9,
                                              distribution_channel=input$userchoice10,
                                              is_repeated_guest=input$userchoice11,
                                              previous_cancellations=input$userchoice12,
                                              assigned_room_type=input$userchoice13,
                                              deposit_type=input$userchoice14,
                                              customer_type=input$userchoice15,
                                              required_car_parking_spaces=input$userchoice16,
                                              total_of_special_requests=input$userchoice17))
    cancel_prediction_hs %>% 
      as.data.frame() %>% 
      rename(y = `1`) %>%
      count(y) %>% 
      mutate(prop = n/sum(n)) %>% 
      ggplot(aes(x = y, y = prop)) +
      geom_col()
    })
  output$lasso_log_histgram <- renderPlot({
    cancel_prediction_lasso<-posterior_predict(
      bayes_lasso_log_updated,newdata=data.frame(hotel=input$userchoice1,
                                              arrival_date_year=input$userchoice3,
                                              arrival_date_month=input$userchoice4,
                                              arrival_date_week_number=input$userchoice5,
                                              children=input$userchoice7,
                                              babies=input$userchoice8,
                                              country=input$userchoice2,market_segment=input$userchoice9,
                                              distribution_channel=input$userchoice10,
                                              is_repeated_guest=input$userchoice11,
                                              previous_cancellations=input$userchoice12,
                                              assigned_room_type=input$userchoice13,
                                              reserved_room_type=input$userchoice18,
                                              deposit_type=input$userchoice14,
                                              customer_type=input$userchoice15,
                                              required_car_parking_spaces=input$userchoice16,
                                              total_of_special_requests=input$userchoice17))
    cancel_prediction_lasso %>% 
      as.data.frame() %>% 
      rename(y = `1`) %>%
      count(y) %>% 
      mutate(prop = n/sum(n)) %>% 
      ggplot(aes(x = y, y = prop)) +
      geom_col()
    })
  output$cancel_probability_hs<-renderText({
    cancel_prediction_hs<-posterior_predict(
      log_hs_model_updated,newdata=data.frame(hotel=input$userchoice1,
                                              arrival_date_year=input$userchoice3,
                                              arrival_date_month=input$userchoice4,
                                              arrival_date_week_number=input$userchoice5,
                                              adults=input$userchoice6,children=input$userchoice7,
                                              babies=input$userchoice8,
                                              country=input$userchoice2,market_segment=input$userchoice9,
                                              distribution_channel=input$userchoice10,
                                              is_repeated_guest=input$userchoice11,
                                              previous_cancellations=input$userchoice12,
                                              assigned_room_type=input$userchoice13,
                                              deposit_type=input$userchoice14,
                                              customer_type=input$userchoice15,
                                              required_car_parking_spaces=input$userchoice16,
                                              total_of_special_requests=input$userchoice17))
    cancel_prediction_hs_table<-cancel_prediction_hs %>% 
      as.data.frame() %>% 
      rename(y = `1`) %>%
      count(y) %>% 
      mutate(prop = n/sum(n))%>%
      filter(y==1)%>%
      select(prop)
    paste("cancel probability from HS model:",cancel_prediction_hs_table)
  })
  output$cancel_probability_lasso<-renderText({
      cancel_prediction_lasso<-posterior_predict(
        bayes_lasso_log_updated,newdata=data.frame(hotel=input$userchoice1,
                                                   arrival_date_year=input$userchoice3,
                                                   arrival_date_month=input$userchoice4,
                                                   arrival_date_week_number=input$userchoice5,
                                                   children=input$userchoice7,
                                                   babies=input$userchoice8,
                                                   country=input$userchoice2,market_segment=input$userchoice9,
                                                   distribution_channel=input$userchoice10,
                                                   is_repeated_guest=input$userchoice11,
                                                   previous_cancellations=input$userchoice12,
                                                   assigned_room_type=input$userchoice13,
                                                   reserved_room_type=input$userchoice18,
                                                   deposit_type=input$userchoice14,
                                                   customer_type=input$userchoice15,
                                                   required_car_parking_spaces=input$userchoice16,
                                                   total_of_special_requests=input$userchoice17))
    cancel_prediction_lasso_table<-cancel_prediction_lasso %>% 
      as.data.frame() %>% 
      rename(y = `1`) %>%
      count(y) %>% 
      mutate(prop = n/sum(n))%>%
      filter(y==1)%>%
      select(prop)
    paste("cancel probability from LASSO model:",cancel_prediction_lasso_table)
    
  })
  }



shinyApp(ui = ui, server = server)
                
  













