library(shiny)
library(tidyverse)
library(rsconnect)
library(shinythemes)


log_hs_model_updated<-readRDS("/Users/Apple/Desktop/454_project/models/log_hs_model_updated.rds")

#R Shiny ui
ui<-fluidPage(
  titlePanel("Hotel Cancellation Exployer"),
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
                                         selected=list("2015"),
                                         multiple = FALSE),
                             selectInput(inputId = "userchoice4", 
                                         label = "Input Month Here", 
                                         choices= list("January","February","March","April",
                                                       "May","June","July","August","September",
                                                       "October","November","December"),
                                         selected=list("January"),
                                         multiple = FALSE),
                             sliderInput(inputId="userchoice5",
                                         "arrival date week number",
                                         min = 1,
                                         max = 50,
                                         value = 30),
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
                                         "number of children",
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
                                         choices = c(Yes = "1", No = "0"), 
                                         multiple = FALSE),
                             selectInput(inputId = "userchoice12", 
                                         label = "Input if cancelled previously or not Here", 
                                         choices = c(Yes = "1", No = "0"), 
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
                             selectInput(inputId = "userchoice16", 
                                         label = "Input if required parking spaces or not Here", 
                                         choices = c(Yes = "1", No = "0"), 
                                         multiple = FALSE),
                             sliderInput(inputId="userchoice17",
                                         "total number of special requests",
                                         min = 0,
                                         max = 5,
                                         value = 1),
                             submitButton(text = "Get Predictions")),
                mainPanel("main panel",textOutput("text1"))))

server <- function(input, output){
  output$mapping <- renderPlot({
    main %>%
      group_by(county) %>%
      mutate(total_contribs = n()) %>%
      filter(total_contribs > 25) %>%
      summarize(mean_amt = mean(Amount)) %>%
      ggplot() + 
      geom_map(map = mn_county, aes(map_id = county, fill = mean_amt)) +
      labs(x="long",y="lat",title = "Mean amount of donations for each county") +
      expand_limits(x = mn_county$long, y = mn_county$lat)})
  output$timeplot <- renderPlot({
    main %>% 
      filter(Amount > 0) %>%
      filter(Gender %in%  input$userchoice1, county == input$userchoice2) %>% 
      group_by(county) %>%
      mutate(avg = mean(Amount)) %>%
      ungroup() %>% 
      ggplot(aes(x = Amount, fill=county)) +
      geom_histogram(color = "white") +
      facet_wrap(~county, scales="free_y") +
      geom_vline(aes(xintercept = avg),
                 color="royalblue1", linetype="dashed", size=1) +
      scale_x_log10(labels = scales::comma) +
      scale_fill_brewer(palette="Blues") +
      labs(title = "Minnesota Political Donations by County and Sex",
           x = "",
           y = "") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
            strip.text = element_text(size=15), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())})}



shinyApp(ui = ui, server = server)
                
  













