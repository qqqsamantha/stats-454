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



head <- dashboardHeader(title = "My Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
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
                submitButton(text = "Get Predictions")
              )
            )
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
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
}

shinyApp(ui, server)