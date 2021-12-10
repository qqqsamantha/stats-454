library(shinydashboard)
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

# model loading
log_model<-readRDS("log_model.rds")
log_hs_model<-readRDS("log_hs_model.rds")
log_hs_model_updated<-readRDS("log_hs_model_updated.rds")
bayes_lasso_log_updated<-readRDS("bayes_lasso_log_updated.rds")
hotel <- read.csv("https://raw.githubusercontent.com/yiyangshi-hub/STAT454-Project/main/hotel_bookings.csv")

hotel_clean <- hotel %>% 
  select(-reservation_status, -reservation_status_date,-company, -arrival_date_day_of_month, -days_in_waiting_list, -agent) %>% 
  filter(country %in%
           c("AUT","BEL","BRA","CHE","CHN","CN","DEU","ESP","FRA","GBR","IRL",
             "ISR","ITA","NLD","NOR","POL","PRT","RUS","SWE","USA")) %>% 
  mutate(is_canceled = factor(is_canceled),
         hotel = factor(hotel),
         arrival_date_year = factor(arrival_date_year),
         arrival_date_month = factor(arrival_date_month),
         arrival_date_week_number = factor(arrival_date_week_number),
         meal = factor(meal),
         market_segment = factor(market_segment),
         distribution_channel = factor(distribution_channel),
         is_repeated_guest = factor(is_repeated_guest),
         reserved_room_type = factor(reserved_room_type),
         assigned_room_type = factor(assigned_room_type),
         deposit_type = factor(deposit_type),
         customer_type = factor(customer_type),
         previous_cancellations = factor(previous_cancellations),
         country = factor(country),
  ) %>% 
  na.omit()

set.seed(123)
hotel_sub <- hotel_clean %>% sample_n(1000)



head <- dashboardHeader(title = "My Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Background",
             tabName = "Background",
             icon = icon("th")),
    menuItem("Bayesian_Logistic_Notation",
             tabName = "Bayesian_Logistic_Notation",
             icon = icon("th")),
    menuItem("Bayesian_Logistic_Lasso_Notation",
             tabName = "Bayesian_Logistic_Lasso_Notation",
             icon = icon("th")),
    menuItem("Horseshoe_Notation",
             tabName = "Horseshoe_Notation",
             icon = icon("th")),
    menuItem("Data_Visualization",
             tabName = "Data_Visualization",
             icon = icon("th")),
    menuItem("Bayes_Logistic_Lasso", 
             tabName = "Bayes_Logistic_Lasso", 
             icon = icon("dashboard")),
    menuItem("Horseshoe", 
             tabName = "Horseshoe", 
             icon = icon("th")),
    menuItem("Model_Evaluation", 
             tabName = "Model_Evaluation", 
             icon = icon("th"))
    )
  )


body <- dashboardBody(
  tabItems(
    tabItem(tabName ="Background",
            fluidRow(
                box(
                  title = "About the Data", status = "primary", solidHeader = TRUE,
                  "This data set contains booking information for a city hotel and a resort hotel, and includes information such as when the booking was made, length of stay, the number of adults, children, and/or babies, and the number of available parking spaces, among other things."
                ),
                box(
                  title = "Author", solidHeader = TRUE, background = "light-blue",
                  "Who creates the data:The data is originally from the article Hotel Booking Demand Datasets, written by Nuno Antonio, Ana Almeida, and Luis Nunes for Data in Brief, Volume 22, February 2019."
                ),
                box(
                  title = "Data source", solidHeader = TRUE, background = "yellow",
                  "The data was downloaded and cleaned by Thomas Mock and Antoine Bichat for #TidyTuesday during the week of February 11th, 2020.source: https://www.kaggle.com/jessemostipak/hotel-booking-demand"
                ),
                box(
                  plotOutput("plot1")
                ),
                box(
                  plotOutput("plot2")
                ),
                box(
                  plotOutput("plot3")
              )
              
            )),
    tabItem(tabName = "Bayesian_Logistic_Notation",
            fluidRow(
              box(
                withMathJax(helpText("To model the binary response variable $$Y\\in \\{0,1\\}$$ by predictors $$X_1,X_2...X_{26}$$, suppose we collect n data points. Let $$(Y_i, X_i)$$ denotes the observed data on each case $$i\\in \\{1, 2, ..., n\\}$$. Thus the Bayesian Logistic Regression model is as follows:")),
                withMathJax(helpText("$$Y_i | \\beta_0, \\beta_1,...,\\beta_{26}, \\sigma \\stackrel{ind}{\\sim} \\text{Bern}(\\pi_i)  \\text{where } \\log\\left(\\frac{\\pi_i}{1 - \\pi_i}\\right) = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 +...+\\beta_{26} X_{26}$$")),
                withMathJax(helpText("$$\\text{equivalently, } \\frac{\\pi_i}{1 - \\pi_i} = e^{\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 +...+\\beta_{26} X_{26}}$$")),
                withMathJax(helpText("$$\\text{ and } \\pi_i = \\frac{e^{\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 +...+\\beta_{26} X_{26}}}{e^{\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 +...+\\beta_{26} X_{26}} + 1}$$")),
                withMathJax(helpText("$$\\beta_{0c}  \\sim N(m_0, s_0^2)$$")),
                withMathJax(helpText("$$\\beta_1    \\sim N(m_1, s_1^2)$$")),
                withMathJax(helpText("$$\\beta_2   \\sim N(m_2, s_2^2)$$")),
                withMathJax(helpText("$$\\text{ ... }$$")),
                withMathJax(helpText("$$\\beta_{26}    \\sim N(m_{26}, s_{26}^2)$$"))
              ),
              box(
                "Note:",
                withMathJax(helpText("The priors on $$(\\beta_0, \\beta_1, ... \\beta_{26})$$ are independent of one another.")),
                withMathJax(helpText("For interpretability, we state our prior understanding of the model intercept $$\\beta_0$$ through the **centered intercept** $$\\beta_{0c}$$.")),
                withMathJax(helpText("When $$X = 0$$, $$\\beta_0$$ is the expected *logged* odds for the hotel booking is canceled and $$e^{\\beta_0}$$ is the expected *odds* for the hotel booking is canceled")),
                withMathJax(helpText("For each 1-unit increase in $$X_1$$, $$\\beta_1$$ is the expected change in the *logged odds of the hotel booking is canceled* and $$e^{\\beta_1}$$ is the expected *multiplicative* change in *odds*.")),
                withMathJax(helpText("$$\\beta_1 = \\log(\\text{odds}_{x_1+1}) - \\log(\\text{odds}_{x_1}) \\;\\; \\text{ and } \\;\\; e^{\\beta_1} = \\frac{\\text{odds}_{x_1+1}}{\\text{odds}_{x_1}}$$")),
                withMathJax(helpText("Same formula will apply to the remaining predictors $$\\beta_2, ..., \\beta_{26}$$"))
              )
            )
    ),
    
    tabItem(tabName = "Bayesian_Logistic_Lasso_Notation",
            fluidRow(
              box(
                withMathJax(helpText("$$Y_i | \\beta_0, \\beta_1,...,\\beta_{26}, \\sigma  \\stackrel{ind}{\\sim} \\text{Bern}(\\pi_i)$$")),
                withMathJax(helpText("$$\\beta_{k}  \\sim \\text{Laplace}(0, 1/\\lambda)$$")),
                withMathJax(helpText("The priors on $$\\beta_k$$ are independent of one another. We set a laplace prior because the laplace distribution centered at 0, meaning if the coefficient shrink to 0, the corresponding variable will not be considered as one of the predictors in the model. ")),
                withMathJax(helpText("$$\\beta_k$$ represent the coefficient of predictors $$X_{1},X_{2}...X_{26}$$ and we also assume that each of the variable follow a laplace distribution."))
              )
            )
    ),
    tabItem(tabName = "Horseshoe_Notation",
            fluidRow(
              box(
                withMathJax(helpText("$$Y_i | \\beta_0, \\beta_1,...,\\beta_{26}, \\sigma  \\stackrel{ind}{\\sim} \\text{Bern}(\\pi_i)$$")),
                withMathJax(helpText("$$\\beta_{k}|\\tau,\\lambda  \\sim \\text{N}(0, \\tau\\lambda_k)$$")),
                withMathJax(helpText("$$\\lambda_k  \\sim \\text{HalfCauchy}(0,1)$$")),
                withMathJax(helpText("$$\\tau  \\sim \\text{HalfCauchy}(0,\\tau_0)$$")),
                withMathJax(helpText("The prior on $$\\beta_k$$ is a model which under the condition of $$\\lambda_k$$ and $$\\tau$$")),
                withMathJax(helpText("The heavy-tailed Cauchy prior on $$\\lambda_k$$ allows individual coefficients to offset and global shrinkage.")),
                withMathJax(helpText("$$\\tau$$ shares the similar heavy-tailed cauchy prior with $$\\lambda_k$$, but we need to choose $$\\tau_0$$ to determinate how fast the model shrink to 0."))
              )
            )
    ),
    tabItem(tabName = "Bayes_Logistic_Lasso",
            fluidRow(
              box(
                width = 3,height = 2,
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
                            multiple = FALSE)),
              
              box(
                width = 5, height = 5,
                plotOutput("lasso_log_histgram")),
              
              box(
                width = 3, height = 3,
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
                            max = 10,
                            value = 1),
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
                submitButton(text = "Get Predictions")
              )
            )
    ),
    
    tabItem(tabName = "Horseshoe",
            fluidRow(
              box(
                width = 3,height = 2,
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
                selectInput(inputId = "userchoice9", 
                            label = "Input Market Segment Here", 
                            choices= list("Groups","Online TA","Corporate","Direct",
                                          "Offline TA/TO","Complementary","Aviation"),
                            selected=list("Groups"),
                            multiple = FALSE)),
              
              box(
                width = 5, height = 5,
                plotOutput("hs_histgram")),
              
              box(
                width = 3, height = 3,
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
                            max = 10,
                            value = 1),
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
                submitButton(text = "Get Predictions")
              )
            )
    ),
    tabItem(tabName = "Model_Evaluation",
            fluidRow(
              column( width = 12,
                box(
                  plotOutput("plot4")
                ),
                box(
                  plotOutput("plot5")
                )
              )
            ))
  )
)

ui <- dashboardPage(head, sidebar, body)
server <- function(input, output) {
  output$lasso_log_histgram <- renderPlot({
    cancel_prediction_lasso<-posterior_predict(
      bayes_lasso_log_updated,newdata=data.frame(hotel=input$userchoice1,
                                                 arrival_date_year=input$userchoice3,
                                                 arrival_date_month=input$userchoice4,
                                                 arrival_date_week_number=input$userchoice5,
                                                 children=input$userchoice7,
                                                 babies=input$userchoice8,
                                                 country=input$userchoice2,
                                                 market_segment=input$userchoice9,
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
  
  output$hs_histgram <- renderPlot({
    cancel_prediction_hs<-posterior_predict(
      log_hs_model_updated,newdata=data.frame(hotel=input$userchoice1,
                                              arrival_date_year=input$userchoice3,
                                              arrival_date_month=input$userchoice4,
                                              arrival_date_week_number=input$userchoice5,
                                              adults=input$userchoice6,
                                              children=input$userchoice7,
                                              country=input$userchoice2,
                                              market_segment=input$userchoice9,
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
  
  output$plot1 <- renderPlot({
    hotel_sub %>% 
      ggplot(aes(x = hotel, fill = is_canceled)) +
      geom_bar()+
      scale_fill_brewer(palette="greens")
  })
  
  output$plot2 <- renderPlot({
    hotel_sub %>% 
      ggplot(aes(x = previous_cancellations, fill = is_canceled)) +
      geom_bar()+
      scale_fill_brewer(palette="greens")
  })
  
  output$plot3 <- renderPlot({
    ggplot(hotel_sub, aes(x = hotel, y = is_canceled, color = previous_cancellations)) +
      geom_jitter(height = 0.25)
  })
  
  output$plot4 <- renderPlot({
    pp_check(log_hs_model_updated)
  })
  
  output$plot5 <- renderPlot({
    pp_check(bayes_lasso_log_updated)
  })
}

shinyApp(ui, server)