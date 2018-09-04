library(shiny)
library(leaflet)
library(dplyr)
library(geosphere)
library(data.table)
ships <- read.csv(file = "ships.csv", stringsAsFactors=FALSE)


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
      leafletOutput("map",height = 500),
      textOutput("longestDistanceOutput")
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  output$vesselTypeInput <- renderUI({
    selectInput("vesselType", "Type of vessel:", sort(unique(ships[,"ship_type"])))
  })
  
  output$vesselNameInput <- renderUI({
    selectInput("vesselName", "Name of vessel:", unique(unlist(subset(ships, ship_type == input$vesselType, select = c(SHIPNAME)))))
  })
  
  output$map <- renderLeaflet({
    maximumDistanceRow <- calculateMaximumDistance(input$vesselName)
    m <- leaflet(maximumDistanceRow) %>%
      addTiles() %>%
      addMarkers(lng = c(maximumDistanceRow$LON, maximumDistanceRow$LON_TO), lat=c(maximumDistanceRow$LAT, maximumDistanceRow$LAT_TO))
    m
  })
  
  output$longestDistanceOutput <- renderText({
    maximumDistanceRow <- calculateMaximumDistance(input$vesselName)
    paste("The maximum distance covered between two consecutive observations is ", round(maximumDistanceRow$DISTANCE, 2), "m.")
  })
  
  calculateMaximumDistance <- function(vesselName) {
    singleShipDf <- subset(ships, SHIPNAME == vesselName, select = c(LON, LAT, DATETIME, SHIP_ID))
    setDT(singleShipDf)
    singleShipDf[, `:=`(LON_TO = shift(LON, type = "lead"), LAT_TO = shift(LAT, type = "lead"))]
    singleShipDf[, `:=`(DISTANCE = distVincentyEllipsoid(singleShipDf[,1:2], singleShipDf[,5:6]))]
    maxDistanceRow <- singleShipDf[singleShipDf$DISTANCE == max(singleShipDf$DISTANCE, na.rm = TRUE),]
    if (length(maxDistanceRow) > 1) {
      maxDistanceRow <- maxDistanceRow[maxDistanceRow$DATETIME == max(maxDistanceRow$DATETIME, na.rm = TRUE),]
    }
    return (maxDistanceRow)
  }
}

# Run the app ----
shinyApp(ui = ui, server = server)
