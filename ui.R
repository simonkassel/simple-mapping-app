# MUSA 620 Final Project
# Simon Kassel

# Shiny server

# package(s)
library(shiny)
library(markdown)

# Define the UI
# using Bootstrap UI elements
shinyUI(fluidPage(theme = "bootstrap.css",
                  tags$header(
                    tags$style(
                        HTML("h1 {
                        color: #FD7156;
                        text-align: right;
                        }")),
  
  # title
  headerPanel("a simple mapping app"),
  
  # sidebal
  sidebarLayout(
    sidebarPanel(
      # link to GeoJSON
      textInput("link", "link to a point or polygon geojson from the web", value = NULL),
      
      # unique ID field to define the points/polygons
      selectInput("idField", choices = NULL, label = "pick an id field"),
      
      # the field to visualize
      selectInput("vizField", choices = NULL, label = "choose what you want to map"),
      
      # enter a map tile
      textInput("mapTitle", "give your map a title", value = ""),
      
      # user input to determine if she need a basemap or not
      radioButtons("basemap", label = "basemap?", choices = c("light", "dark"), selected = FALSE, inline = TRUE)
    ),
    
    # main panel with map
    mainPanel(
      tabsetPanel(
        tabPanel("Map", plotOutput("nobm")),
        tabPanel("About", includeMarkdown("about.md"))
      )
    )
  )
)))
