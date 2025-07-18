library(leaflet)
library(tidyverse)
library(tigris)
library(sf)
library(RColorBrewer)
library(shiny)
library(leaflet)
library(jsonlite)


raw_data <- read.csv("C:/Users/yangh/Downloads/workforce_development_project/raw_Workforce_ACS_Data.csv")
#creates geo id for mapping
raw_data<- raw_data %>%
  mutate(
    GEOID = sprintf("%02d%03d", statefip, countyfip)
  )

#subsets data and creates a count 
micro_electronics <- raw_data %>%
  filter(Occupation == "Electrical, Electronics, and Electromechanical Assemblers" |
           Occupation == "Electrical and Electronic Engineering Technologists And Technicians"|
           Occupation == "Other Engineering Technologists And Technicians, Except Drafters"|
           Occupation == "Computer Numerically Controlled Tool Operators And Programmers"|
           Occupation == "Chemical Technicians"|
           Occupation == "Avionics Technicians") %>%
  group_by(GEOID) %>%
  summarise(
    n_microelectronics_workers = n()
  )


ui <- fluidPage(
  titlePanel("Microelectronics Workers by County"),
  leafletOutput("map", height = 600)
)

# Define Server
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(subset_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(n_microelectronics_workers),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        popup = ~paste0(NAME, " County<br>",
                        "Number of Workers: ", n_microelectronics_workers)
      ) %>%
      addLegend(pal = pal, values = ~n_microelectronics_workers, title = "Number of workers")
  })
}

# Run the app
shinyApp(ui, server)


