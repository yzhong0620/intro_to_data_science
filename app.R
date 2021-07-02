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
  submitButton(text = "Create my plot!"),
  plotOutput(outputId = "timeplot")
)

server <- function(input, output) {
  output$timeplot <- renderPlot({
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
}

shinyApp(ui = ui, server = server)