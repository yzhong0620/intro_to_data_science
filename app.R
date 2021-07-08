library(tidyverse)     # for data cleaning and plotting
library(shiny)         # for creating interactive apps
library(tidytuesdayR)
theme_set(theme_minimal())

tuesdata <- tidytuesdayR::tt_load('2020-07-07')
tuesdata <- tidytuesdayR::tt_load(2020, week = 28)

coffee_ratings <- tuesdata$coffee_ratings

ui <- fluidPage(
  selectInput("country", 
              "country", 
              choices = as.list(coffee_ratings$country_of_origin),
              multiple = TRUE),
  selectInput("processingmethod",
              "processing method",
              choices = as.list(coffee_ratings$processing_method),
              multiple = TRUE),
  submitButton(text = "Create my plot!"),
  tabsetPanel(
    tabPanel("rating", plotOutput(outputId = "plot1")), 
    tabPanel("moisture", plotOutput(outputId = "plot2")), 
    tabPanel("aroma", plotOutput(outputId = "plot3")),
    tabPanel("aroma by color", plotOutput(outputId = "plot4")), 
    tabPanel("aroma by country", plotOutput(outputId = "plot5")))
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    coffee_ratings %>% 
      filter(!is.na(variety),
             total_cup_points > 0,
             country_of_origin %in% input$country) %>% 
      ggplot(aes(y = country_of_origin,
                 x = total_cup_points)) +
      geom_violin() +
      geom_point(aes(color = variety), size = 2) +
      labs(x = "",
           y = "",
           title = "Total rating for each country",
           color = "Bean variety") +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "right")
  })
  output$plot2 <- renderPlot({
    coffee_ratings %>% 
      filter(processing_method == c("Washed / Wet","Semi-washed / Semi-pulped","Pulped natural / honey", "Other", "Natural / Dry"),
             processing_method %in% input$processingmethod) %>% 
      ggplot(aes(y = processing_method, 
                 x = moisture)) + 
      geom_boxplot() + 
      labs(title = "How processing method affects moisture of coffee beans",
           x = "Moisture",
           y = "")
  })
  output$plot3 <- renderPlot({
    coffee_ratings %>% 
      filter(processing_method == c("Washed / Wet","Semi-washed / Semi-pulped","Pulped natural / honey", "Other", "Natural / Dry"),
             processing_method %in% input$processingmethod) %>% 
      ggplot(aes(y = processing_method, 
                 x = aroma)) + 
      geom_boxplot() + 
      labs(title = "How processing method affects aroma of coffee beans",
           x = "Aroma",
           y = "")
  })
  output$plot4 <- renderPlot({
    coffee_ratings %>% 
      filter(processing_method == c("Washed / Wet","Semi-washed / Semi-pulped","Pulped natural / honey", "Other", "Natural / Dry"),
             processing_method %in% input$processingmethod) %>% 
      ggplot(aes(y = processing_method, 
                 fill = color)) + 
      geom_bar() + 
      labs(title = "How processing methods compared with the color of coffee beans",
           x = "Aroma",
           y = "")
  })
  output$plot5 <- renderPlot({
    coffee_ratings %>% 
      filter(processing_method == c("Washed / Wet","Semi-washed / Semi-pulped","Pulped natural / honey", "Other", "Natural / Dry"),
             country_of_origin %in% input$country) %>% 
      ggplot(aes(y = processing_method, 
                 fill = country_of_origin)) + 
      geom_bar() + 
      labs(title = "How processing methods compared with the color of coffee beans",
           x = "Aroma",
           y = "")
    
  })
}

shinyApp(ui = ui, server = server)