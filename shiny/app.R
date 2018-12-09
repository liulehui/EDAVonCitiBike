library(shiny)
library(leaflet)
library(htmltools)

source("helpers.R")

fileName = "data_201806.csv"
df = readData(fileName)

stations <- unique(df$start_name)

server <- function(input, output) {
 
  df1 <- df[sample(nrow(df), 200),]
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map 
  output$map<- renderLeaflet({
    leaflet(df1) %>%
      addTiles() %>%
      addMarkers(lng=~start_long, lat=~start_lat, layerId=~start_id, label = ~htmlEscape(start_name), clusterOptions = markerClusterOptions())
    #addCircleMarkers(data=data, ~x , ~y, layerId=~id, popup=~id, radius=8 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8)
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
    # When map is clicked, show a popup with city info
  })
  
  output$selected_startStation <- renderText({ 
    paste("You have selected", input$startStation)
  })
  output$selected_endStation <- renderText({ 
    paste("You have selected", input$endStation)
  })
  
  # Make a barplot of top popular end stations based on flag
  output$plota=renderPlot({
    #print()
    g1 = nameTopEnd(df,input$startStation,input$topNum)
    g1
    #barplot(rnorm(23), col=rgb(0.1,0.4,0.9,0.3))
  })
  
  output$plotb=renderPlot({
    plotTimeDuration(df, input$startStation, input$endStation)
    # barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
  })
  
  output$plotc=renderPlot({
    #barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
    start = as.numeric(data_of_click$clickedMarker$id)
    stationTimeSlot(df, start)
  })
}

ui <- fluidPage(
  br(),

  column(8,br(),br(),leafletOutput("map", height="600px")),
  fluidRow(
    column(4,plotOutput("plota", height="200px")),
    column(4,br(),plotOutput("plotb", height="200px")),
    column(4,br(),plotOutput("plotc", height="200px"))
  ),
  
  fluidRow(
    
    column(4,sliderInput("topNum", h5("Top Popular Stations"),
                         min = 1, max = 30, value = 10)),
    fluidRow(
      column(4,selectInput("startStation", h5("Start Station"), 
                           choices = stations, selected = 1)),
      column(4,selectInput("endStation", h5("End Station"), 
                           choices = stations, selected = 1))
    ),
    column(4,textOutput("selected_startStation"),textOutput("selected_endStation"))
  )
)

shinyApp(ui = ui, server = server)






