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
library(shinydashboard)

log_model<-readRDS("log_model.rds")
log_hs_model<-readRDS("log_hs_model.rds")
log_hs_model_updated<-readRDS("log_hs_model_updated.rds")
bayes_lasso_log_updated<-readRDS("bayes_lasso_log_updated.rds")


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Background", tabName = "Background", icon = icon("Background")),
    menuItem("Data", tabName = "Data", icon = icon("Data")),
    menuItem("Bayesian_Logistic", tabName = "Bayesian_Logistic", icon = icon("Bayesian_Logistic")),
    menuItem("Bayesian Lasso Logistic", tabName = "Bayesian Lasso Logistic", icon = icon("Bayesian Lasso Logistic")),
    menuItem("Horseshoe Prior", tabName = "Horseshoe Prior", icon = icon("Horseshoe Prior")),
    menuItem("Make Predictions", tabName = "Make Predictions", icon = icon("Make Predictions")),
    menuItem("Model Evaluation", tabName = "Model Evaluation", icon = icon("Model Evaluation"))
  )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName ="Background",
              fluidRow(
                box("About the data:This data set contains booking information for a city hotel and a resort hotel, and includes information such as when the booking was made, length of stay, the number of adults, children, and/or babies, and the number of available parking spaces, among other things.Who creates the data:The data is originally from the article Hotel Booking Demand Datasets, written by Nuno Antonio, Ana Almeida, and Luis Nunes for Data in Brief, Volume 22, February 2019.Data source:The data was downloaded and cleaned by Thomas Mock and Antoine Bichat for #TidyTuesday during the week of February 11th, 2020.source: https://www.kaggle.com/jessemostipak/hotel-booking-demand")
              )),
      tabItem(tabName ="Bayesian_Logistic",
              fluidRow(
                box(withMathJax(helpText(p("$$\\tau  \\sim \\text{HalfCauchy}(0,\\tau_0)$$"))))
              ))
    )

  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  output$Background<-renderText({
    "About the data:This data set contains booking information for a city hotel and a resort hotel, and includes information such as when the booking was made, length of stay, the number of adults, children, and/or babies, and the number of available parking spaces, among other things.Who creates the data:The data is originally from the article Hotel Booking Demand Datasets, written by Nuno Antonio, Ana Almeida, and Luis Nunes for Data in Brief, Volume 22, February 2019.Data source:The data was downloaded and cleaned by Thomas Mock and Antoine Bichat for #TidyTuesday during the week of February 11th, 2020.source: https://www.kaggle.com/jessemostipak/hotel-booking-demand"
  })
  output$Bayesian_Logistic<-renderText(withMathJax({
    return(formula("\\pi"))
  })
  )
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)






