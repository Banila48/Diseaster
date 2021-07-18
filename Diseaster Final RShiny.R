if(!require(shinythemes)){
  install.packages("shinythemes")
  library(shinythemes)
}

if(!require(leaflet)){
  install.packages("leaflet")
  library(leaflet)
}

library(shiny)
library(dplyr)

### R
# setwd("C:\\Users\\night\\Desktop")
virusIcon <- makeIcon(
  iconUrl = "123.png",
  iconWidth = 16, iconHeight = 16,
  iconAnchorX = NULL, iconAnchorY = NULL,
  shadowUrl = NULL, shadowWidth = NULL, shadowHeight = NULL, shadowAnchorX = NULL, shadowAnchorY = NULL
)


### UI

ui <- fluidPage(theme=shinytheme("simplex"),
                fluidRow(
                  column(3, offset = 0,
                         h3("Player names"),
                         br(),br(),
                         textInput(inputId="player1name", 
                                   label="Player 1: ", 
                                   value = "Player 1", 
                                   width = NULL,
                                   placeholder = 'Player 1 Name'),
                         textInput(inputId="player2name", 
                                   label="Player 2: ", 
                                   value = "Player 2", 
                                   width = NULL,
                                   placeholder = 'Player 2 Name'),
                         textInput(inputId="player3name", 
                                   label="Player3: ", 
                                   value = "Player 3", 
                                   width = NULL,
                                   placeholder = 'Player 3 Name'),
                         textInput(inputId="player4name", 
                                   label="Player 4: ", 
                                   value = "Player 4", 
                                   width = NULL,
                                   placeholder = 'Player 4 Name')
                  ),
                  column(7,offset=1,
                         h3("Player State"), br(), br(),
                         tableOutput(outputId = "health_table")
                  )
                ),
                fluidRow(
                  column(4, offset=0,h3("Update Player State"),
                         selectInput("player", "Player:",
                                     c("Player 1" = 1,
                                       "Player 2" = 2,
                                       "Player 3" = 3,
                                       "Player 4" = 4)), br(),
                         actionButton("increaseHealth", "+ health", style='width:40%; margin: 4px 4px'),
                         actionButton("decreaseHealth", "- health", style='width:40%; margin: 4px 4px'),
                         actionButton("increaseFood", "+ food", style='width:40%; margin: 4px 4px'),
                         actionButton("decreaseFood", "- food", style='width:40%; margin: 4px 4px'), 
                         actionButton("increaseDrink", "+ drink", style='width:40%; margin: 4px 4px'),
                         actionButton("decreaseDrink", "- drink", style='width:40%; margin: 4px 4px'), 
                  ),
                  column(6, offset=0,h3("Affected Cities"),
                         leafletOutput('map'))
                
                )
                          
)



health <- rep(10,4)
food <- rep(5,4)
drink <- rep(5,4)
log_update <- c()


### Server

server <- function(input, output) {
  observeEvent(input$increaseHealth,{
    health[as.numeric(input$player)] <<- health[as.numeric(input$player)] + 1
  })
  
  observeEvent(input$increaseFood,{
    food[as.numeric(input$player)] <<- food[as.numeric(input$player)] + 1
  })
  
  observeEvent(input$increaseDrink,{
    drink[as.numeric(input$player)] <<- drink[as.numeric(input$player)] + 1
  })
  
  observeEvent(input$decreaseDrink,{
    drink[as.numeric(input$player)] <<- drink[as.numeric(input$player)] - 1
  })
  
  observeEvent(input$decreaseFood,{
    food[as.numeric(input$player)] <<- food[as.numeric(input$player)] - 1
  })
  
  observeEvent(input$decreaseHealth,{
    health[as.numeric(input$player)] <<- health[as.numeric(input$player)] - 1
  })

  
  output$text <- renderText(log_update)
  output$health_table <- renderTable({
    input$updateName
    input$increaseHealth
    input$increaseDrink
    input$increaseFood
    input$decreaseHealth
    input$decreaseDrink
    input$decreaseFood
    names <- (c(input$player1name,
                input$player2name,
                input$player3name,
                input$player4name))
    
    df <- data.frame(names, health,food, drink)
    
    
    df}, digits = 0, bordered = TRUE, width = "100%", spacing="m")
  
  
  output$map <- renderLeaflet({
    leaflet() %>% 
    addTiles()})


  observeEvent(input$map_click, {
    click = input$map_click
    leafletProxy('map') %>% 
      addMarkers(icon = virusIcon, lng = click$lng, lat = click$lat, popup = click$date)
      print("City has been infected")
  })

}

shinyApp(ui, server)
