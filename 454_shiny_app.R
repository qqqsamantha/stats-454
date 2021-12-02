library(shiny)
library(tidyverse)
library(rsconnect)
library(shinythemes)


#R Shiny ui
ui<-fluidPage(
  titlePanel("Hotel Cancellation Exployer"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             selectInput(inputId = "userchoice1", 
                                         label = "Input  Here", 
                                         choices = c(Female = "F", Male = "M"), 
                                         multiple = FALSE),
                             selectInput(inputId = "userchoice2", 
                                         "Input Country Here", 
                                         choices = list("PRT","ESP","GBR","NLD","AUT","FRA","BEL",
                                                        "BRA","USA","ITA","DEU","IRL","CN","POL",
                                                        "RUS","NOR","CHN","CHE","ISR","SWE"),
                                         selected=list("PRT"),
                                         multiple = FALSE), 
                             selectInput(inputId = "userchoice1", 
                                         label = "Input  Here", 
                                         choices = c(Female = "F", Male = "M"), 
                                         multiple = FALSE),
                             
                
  













