library(shiny)
library(leaflet)
library(dplyr)
ships <- read.csv(file = "ships.csv")


# Define UI ----
ui <- fluidPage(
  titlePanel(h2("Marine Shiny App")),

  sidebarLayout(
    sidebarPanel(
      h3("Please select a vessel:"),
      uiOutput("vesselTypeInput"),
      uiOutput("vesselNameInput")
    ),
    mainPanel(
      h3("A map"),
      leafletOutput("mymap",height = 500),
      p("The longest distance")
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  output$vesselTypeInput <- renderUI({
    selectInput("vesselType", "Type of vessel:", sort(unique(ships[,"ship_type"])))
  })
  
  output$vesselNameInput <- renderUI({
    selectInput("vesselName", "Name of vessel:", unique(subset(ships, ship_type == input$vesselType, select = c(SHIPNAME))))
  })
  
  output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng=18.580730, lat=54.387248 , zoom=10)
    m
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)