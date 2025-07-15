library(tidyverse)
library(shiny)
library(leaflet)
library(jsonlite)

states_geo <- fromJSON("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", simplifyVector = FALSE)



ui <- fluidPage(
  titlePanel("Interactive US Map in Shiny"),
  leafletOutput("usmap", height = "600px"),
  verbatimTextOutput("click_info")
)

server <- function(input, output, session) {
  
  output$usmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addGeoJSON(states_geo,
                 layerId = ~properties$name,
                 label = ~properties$name,
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = TRUE),
                 popup = ~paste("<strong>", properties$name, "</strong><br/>Population:", properties$population))
  })
  
  output$click_info <- renderPrint({
    click <- input$usmap_shape_click
    if (is.null(click)) {
      "Click on a state to see more info"
    } else {
      paste("You clicked on:", click$id)
    }
  })
}

shinyApp(ui, server)



library(shiny)
library(leaflet)
library(jsonlite)

# Load GeoJSON
states_geo <- fromJSON("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", simplifyVector = FALSE)


ui <- fluidPage(
  titlePanel("Interactive US Map in Shiny"),
  leafletOutput("usmap", height = "600px"),
  verbatimTextOutput("click_info")
)

server <- function(input, output, session) {
  
  output$usmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(states_geo,
                 layerId = ~properties$name,
                 label = ~properties$name,
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = TRUE),
                 popup = ~paste("<strong>", properties$name, "</strong><br/>Population:", properties$population))
  })
  
  output$click_info <- renderPrint({
    click <- input$usmap_shape_click
    if (is.null(click)) {
      "Click on a state to see more info"
    } else {
      paste("You clicked on:", click$id)
    }
  })
}

shinyApp(ui, server)


