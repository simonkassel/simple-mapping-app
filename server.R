# MUSA 620 Final Project
# Simon Kassel

# Shiny server

# packages
library(shiny)
library(rgdal)
library(tidyverse)
library(ggmap)
library(maps)
library(maptools)
library(markdown)
library(rgeos)
library(gpclib)

# note: the vast majority of the code that powers this app is in the accompanying
# global.R file in a series of helper functions

# server functionality
shinyServer(function(input, output, session) {
  
  # when available, read in the geojson from the link provided
  # use the fields in it's attribute table to populate dropdowns
  observe({
    spObj <- readGeoJson(input$link)
    fields <- getFields(spObj)
    if (length(fields) > 0) {
      updateSelectInput(session, "idField",
                        label = "Select an ID field",
                        choices = fields,
                        selected = fields[1]
      )
      updateSelectInput(session, "vizField",
                        choices = fields,
                        selected = fields[1]
      )
    }
    
    # reproject in order to match sp objects with basemaps
    spProj <- wgsProj(spObj)
    
    # render a map
    output$nobm <- renderPlot({
      
      # if it is a polygon geojson file
      if (class(spProj)[1] == "SpatialPolygonsDataFrame") {
        polygonPlot(spProj, input$idField, input$vizField, input$mapTitle, input$basemap)
        
      # or a point file
      } else if (class(spProj)[1] == "SpatialPointsDataFrame") {
        pointPlot(spProj, input$idField, input$vizField, input$mapTitle, input$basemap)
        
      # otherwise use this placeholder map from maptools
      } else {
        data("wrld_simpl")
        plot(wrld_simpl, main = "make a map")
      }
    })
  })
})



