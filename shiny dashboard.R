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

lasso_matrix<-classification_summary(
  model = bayes_lasso_log_updated, 
  data = hotel_lasso,
  cutoff = 0.5)$confusion_matrix %>% 
  as.data.frame()


lasso_table <- data.frame(
  metrics = c("sensitivity","specificity","accuracy"),
  value = classification_summary(
    model = bayes_lasso_log_updated, 
    data = hotel_lasso,
    cutoff = 0.5)$accuracy_rate)

hs_matrix <- classification_summary(
  model = log_hs_model_updated, 
  data = hotel_hs,
  cutoff = 0.5)$confusion_matrix %>% 
  as.data.frame()

hs_table <- data.frame(
  metrics = c("sensitivity","specificity","accuracy"),
  value = classification_summary(
    model = log_hs_model_updated, 
    data = hotel_hs,
    cutoff = 0.5)$accuracy_rate)



head <- dashboardHeader(title = "Hotel Cancellation Prediction")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About",
             tabName = "About",
             icon = icon("book")),
    menuItem("Model Notation",
             tabName = "Model_Notation",
             icon = icon("buffer")),
    menuItem("Bayes Logistic Lasso", 
             tabName = "Bayes_Logistic_Lasso", 
             icon = icon("battle-net")),
    menuItem("Horseshoe", 
             tabName = "Horseshoe", 
             icon = icon("atom")),
    menuItem("Model Evaluation", 
             tabName = "Model_Evaluation", 
             icon = icon("balance-scale"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName ="About",
            fluidRow(
              box(
                title = "About the Data", status = "primary", solidHeader = TRUE, width = 6,
                p("This data set contains booking information for a city hotel and a resort hotel, 
                    and includes information such as when the booking was made, length of stay, 
                    the number of adults, children, and/or babies, and the number of available parking spaces, 
                    among other things."),
                p("The data is originally from the article Hotel Booking Demand Datasets, 
                    written by Nuno Antonio, Ana Almeida, and Luis Nunes for Data in Brief, Volume 22, February 2019."),
                p("The data was downloaded and cleaned by Thomas Mock and Antoine Bichat for 
                    #TidyTuesday during the week of February 11th, 2020.source: 
                    https://www.kaggle.com/jessemostipak/hotel-booking-demand")
              ),
              tabBox(
                title = "Motivation", width = 6, height = "230px",
                tabPanel("Trend", 
                         "Why are we doing this project. Why should we care about who will 
                         cancel the hotel reservation? There has been a growing trend for cancellation. 
                         Booking. Com and expedia have cancellation rates of 57% and 26% compared to the
                         official hotel websites with an average cancellations rate of 14%. 
                         However, such growing in cancellations is not a good thing for the hotels. 
                         A loss of income occurs as a result of no show or unsold rooms."),
                tabPanel("Engagement",
                         "It has been noted that if  hotel staff contacts the customer, 
                         the cancellation probability will reduce by 30%. That’s been said 
                         if the hotel staff can correctly identify those who are likely to 
                         cancel and reach out to them, then the loss in revenue can be reduced. 
                         Therefore, we set our ultimate goal is to produce a shiny app for the hotel 
                         representative to identify those who are more likely to cancel. ")
              ),
              box(
                title = "Hotel type vs Cancellation", status = "primary", width = 4,
                plotOutput("plot1"),
                p("In general, there are more city hotel than resort hotel 
                  in our dataset. However, when looking at the hotels that have been canceled, 
                  it is more likely for the City Hotel to be canceled than Resort Hotel"),
                p("Then we may wondering if this can be explained by the customers' own behavior, 
                  eg. some customers are more likely to cancel the hotel than others. 
                  Then we look at previous cancellations which may reflect the customers behaviors")
              ),
              box(
                title = "Previous Cancellation vs Cancellation", status = "warning", width = 4,
                plotOutput("plot2"),
                p("From the graph above, we can see that those who did not cancel last time are also 
                  less likely to cancel this time than those who did cancel. "),
                p("It may be better to combine the previous two graphs into a jitter plot, so it contains
                    all three parameters we have discussed.")
              ),
              box(
                title = "Hotel Type vs Previous Cancellation vs Cancellation",
                status = "success",
                width = 4,
                plotOutput("plot3"),
                p("We can see a clear cluster that those which is a City Hotel and who has previously 
                    canceled the hotel would devote the most cases to the hotel cancellations. 
                    Hotel type and previous_cancellations are 2 strong predictors."),
                p("So we may wondering what other predictors, like Hotel type and previous_cancellations, 
                    can be used to model the hotel cancellations. Then we would dig deeper into model building.")
              )
              
              
            )),
    
    tabItem(tabName = "Model_Notation",
            fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                     column(
                       br(),
                       br(),
                       box(title = "Bayesian Logistic", status = "primary", solidHeader = TRUE,
                           withMathJax(),
                           "$$Y_i | \\beta_0, \\beta_1,...,\\beta_{26}, \\sigma \\stackrel{ind}{\\sim} \\text{Bern}(\\pi_i)$$
                         $$\\beta_{0c}\\sim N(m_0, s_0^2)$$
                         $$\\beta_1\\sim N(m_1, s_1^2)$$
                         $$\\beta_2\\sim N(m_2, s_2^2)$$
                         $$\\text{ ... }$$
                         $$\\beta_{26}\\sim N(m_{26}, s_{26}^2)$$"
                       ),
                       br(),
                       p("The first row of the model notation above represents the model we choose for the 
                         response variable Y, which is the hotel booking is canceled or not,
                         For interpretability, we state our prior understanding of the model intercept 
                         through the centered intercept. When x = 0, the intercept is the expected 
                         logged odds for the hotel booking is canceled and the exponentiated intercept is the expected odds
                         for the hotel booking is canceled. For each 1-unit increase in a predictor, its corresponding coefficient 
                         represents the expected change in the logged odds of the hotel booking is canceled and 
                         the exponentiated coefficient represents the expected multiplicative change in odds."),
                       width=8,style="background-color:light-blue;border-radius: 10px")
            ),
            fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                     column(
                       br(),
                       br(),
                       box(title = "Bayesian Lasso Logistic", status = "primary", solidHeader = TRUE,
                           "$$Y_i | \\beta_0, \\beta_1,...,\\beta_{26}, \\sigma  \\stackrel{ind}{\\sim} \\text{Bern}(\\pi_i)$$
                          $$\\beta_{k}  \\sim \\text{Laplace}(0, 1/\\lambda)$$"),
                       br(),
                       p("The priors on the coefficients are independent of one another. 
                         We set a laplace prior because the laplace distribution centered at 0, 
                         meaning if the coefficient shrink to 0, 
                         the corresponding variable will not be considered as one of the predictors in the model. 
                         The second row of the model notation above represents the model we choose for each predictor's coefficient. 
                         and we also assume that each of the variable follow a laplace distribution."),
                       width=8,style="background-color:light-blue;border-radius: 10px")
            ),
            fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                     column(
                       br(),
                       br(),
                       box(title = "Horseshoe", status = "primary", solidHeader = TRUE,
                           "$$Y_i | \\beta_0, \\beta_1,...,\\beta_{26}, \\sigma  \\stackrel{ind}{\\sim} \\text{Bern}(\\pi_i)$$
                           $$\\beta_{k}|\\tau,\\lambda  \\sim \\text{N}(0, \\tau\\lambda_k)$$
                           $$\\lambda_k  \\sim \\text{HalfCauchy}(0,1)$$
                           $$\\tau  \\sim \\text{HalfCauchy}(0,\\tau_0)$$"),
                       br(),
                       p("The first row of the above model notation represents
                         the Bernoulli distribution of the outcome variable Y, 
                         since the hotel booking is either canceled or not canceled. 
                         we also give the penalty term a heavy-tailed Cauchy prior,
                         so it allows individual coefficients to shrink to zero 
                         based on its contribution to the prediction accuracy 
                         The second row above represents the distribution of each coefficient of each variable.
                         Besides the penalty term, we also need to choose similar half Cauchy distribution
                         for the variable on the fourth row, which determine how fast a variable shrink to zero"),
                       width=8,style="background-color:light-blue;border-radius: 10px")
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
                selectInput(inputId = "userchoice_1", 
                            label = "Input Hotel type Here", 
                            choices = list("City Hotel","Resort Hotel"), 
                            multiple = FALSE),
                selectInput(inputId = "userchoice_2", 
                            "Input Country Here", 
                            choices = list("PRT","ESP","GBR","NLD","AUT","FRA","BEL",
                                           "BRA","USA","ITA","DEU","IRL","CN","POL",
                                           "RUS","NOR","CHN","CHE","ISR","SWE"),
                            selected=list("PRT"),
                            multiple = FALSE), 
                selectInput(inputId = "userchoice_3", 
                            label = "Input Year Here", 
                            choices= list("2015","2016","2017"),
                            selected=list("2016"),
                            multiple = FALSE),
                selectInput(inputId = "userchoice_4", 
                            label = "Input Month Here", 
                            choices= list("January","February","March","April",
                                          "May","June","July","August","September",
                                          "October","November","December"),
                            selected=list("January"),
                            multiple = FALSE),
                selectInput(inputId="userchoice_5",
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
                sliderInput(inputId="userchoice_7",
                            "number of children",
                            min = 0,
                            max = 2,
                            value = 1),
                selectInput(inputId = "userchoice_9", 
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
                selectInput(inputId = "userchoice_10", 
                            label = "Input Distribution Channel Here", 
                            choices= list("TA/TO","GDS","Corporate","Direct"),
                            selected=list("GDS"),
                            multiple = FALSE),
                
                selectInput(inputId = "userchoice_11", 
                            label = "Input if repeated guest or not Here", 
                            choices= list("0","1"),
                            selected=list("0"),
                            multiple = FALSE),
                selectInput(inputId = "userchoice_12", 
                            label = "Input if cancelled previously or not Here", 
                            choices= list("0","1"),
                            selected=list("0"),
                            multiple = FALSE),
                
                selectInput(inputId = "userchoice_13", 
                            label = "Input Assigned Room Type Here", 
                            choices= list("A","B","C","D",
                                          "E","F","G","H","I","K"),
                            selected=list("A"),
                            multiple = FALSE),
                selectInput(inputId = "userchoice_14", 
                            label = "Input Deposit Type Here", 
                            choices= list("Non Refund","No Deposit","Refundable"),
                            selected=list("Refundable"),
                            multiple = FALSE),
                selectInput(inputId = "userchoice_15", 
                            label = "Input Customer Type Here", 
                            choices= list("Transient","Transient-Party",
                                          "Contract","Group"),
                            selected=list("Contract"),
                            multiple = FALSE),
                sliderInput(inputId="userchoice_16",
                            "Input if required parking spaces or not Here",
                            min = 0,
                            max = 10,
                            value = 1),
                sliderInput(inputId="userchoice_17",
                            "Input total number of special requests",
                            min = 0,
                            max = 5,
                            value = 1),
                submitButton(text = "Get Predictions")
              )
            )
    ),
    tabItem(tabName = "Model_Evaluation",
            fluidRow(
              column(
                width = 12,
                box(
                  width = 4,
                  title = "Lasso Model Evaluation", status = "warning", solidHeader = TRUE,
                  tableOutput("table1"),
                  tableOutput("table2")
                ),
                box(
                  plotOutput("plot4")
                )
              ),
              column( width = 12,
                      box(
                        width = 4,
                        title = "Horseshoe Model Evaluation", status = "success", solidHeader = TRUE,
                        tableOutput("table3"),
                        tableOutput("table4")
                      ),
                      box(
                        plotOutput("plot5")
                      ))
              
            ))
  ))


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
      mutate(prop = n/sum(n),
             y = factor(y)) %>% 
      ggplot(aes(x = y, y = prop, fill = y)) +
      geom_col()+
      scale_fill_manual(values = c("#FF651D","#3487D5"))+
      theme(legend.position = "none",
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))+
      labs(title = "Probability of hotel Cancellation") +
      xlab("Hotel canceled or not")+
      ylab("Probability")
  })
  
  output$hs_histgram <- renderPlot({
    cancel_prediction_hs<-posterior_predict(
      log_hs_model_updated,newdata=data.frame(hotel=input$userchoice_1,
                                              arrival_date_year=input$userchoice_3,
                                              arrival_date_month=input$userchoice_4,
                                              arrival_date_week_number=input$userchoice_5,
                                              children=input$userchoice_7,
                                              country=input$userchoice_2,
                                              market_segment=input$userchoice_9,
                                              distribution_channel=input$userchoice_10,
                                              is_repeated_guest=input$userchoice_11,
                                              previous_cancellations=input$userchoice_12,
                                              assigned_room_type=input$userchoice_13,
                                              deposit_type=input$userchoice_14,
                                              customer_type=input$userchoice_15,
                                              required_car_parking_spaces=input$userchoice_16,
                                              total_of_special_requests=input$userchoice_17))
    cancel_prediction_hs %>% 
      as.data.frame() %>% 
      rename(y = `1`) %>%
      count(y) %>% 
      mutate(prop = n/sum(n),
             y = factor(y)) %>% 
      ggplot(aes(x = y, y = prop, fill = y)) +
      geom_col() +
      scale_fill_manual(values = c("#FF651D","#3487D5"))+
      theme(legend.position = "none",
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))+
      labs(title = "Probability of hotel Cancellation") +
      xlab("Hotel canceled or not")+
      ylab("Probability")
  })
  
  output$plot1 <- renderPlot({
    hotel_sub %>% 
      ggplot(aes(x = hotel, fill = is_canceled)) +
      geom_bar()+
      scale_fill_manual(values = c("#FF651D","#3487D5"))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
  })
  
  output$plot2 <- renderPlot({
    hotel_sub %>% 
      ggplot(aes(x = previous_cancellations, fill = is_canceled)) +
      geom_bar()+
      scale_fill_manual(values = c("#FF651D","#3487D5"))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
  })
  
  output$plot3 <- renderPlot({
    ggplot(hotel_sub, aes(x = hotel, y = is_canceled, color = previous_cancellations)) +
      geom_jitter(height = 0.25) +
      scale_color_manual(values = c("#FF651D","#3487D5"))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
  })
  
  output$plot4 <- renderPlot({
    pp_check(log_hs_model_updated)
  })
  
  output$plot5 <- renderPlot({
    pp_check(bayes_lasso_log_updated)
  })
  
  output$table1 <- renderTable(lasso_matrix)
  output$table2 <- renderTable(lasso_table)
  output$table3 <- renderTable(hs_matrix)
  output$table4 <- renderTable(hs_table)
}

shinyApp(ui, server)