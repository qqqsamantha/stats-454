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
log_hs_model_updated<-readRDS("log_hs_model_updated.rds")
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
                             sliderInput(inputId="userchoice11",
                                         "Input if repeated guest or not Here",
                                         min = 0,
                                         max = 1,
                                         value = 0),
                             sliderInput(inputId="userchoice12",
                                         "Input if cancelled previously or not Here",
                                         min = 0,
                                         max = 1,
                                         value = 0),
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
                                         "total number of special requests",
                                         min = 0,
                                         max = 5,
                                         value = 1),
                             submitButton(text = "Get Predictions")),
                mainPanel("main panel",plotOutput("histgram"))))

server <- function(input, output){
  output$histgram <- renderPlot({
    cancel_prediction<-posterior_predict(
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
    mcmc_hist(cancel_prediction, prob = 0.9)})}



shinyApp(ui = ui, server = server)
                
  













