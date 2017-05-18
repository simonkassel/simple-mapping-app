# MUSA 620 Final Project
# Simon Kassel

# Global: helper functions

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

# get gpclibPermit
gpclibPermit()

# read a geojson from a web link
# - takes the link (string)
# and returns
# - an sp object
readGeoJson <- function(link) {
  if (grepl("json", link)) {
    return(readOGR(link, "OGRGeoJSON"))
  }
}

# gets the field names of the sp object
# - takes an sp object
# and returns
# - a vector of field names
getFields <- function(spObj) {
  if (!is.null(spObj)) {
    return(spObj@data %>% names())
  }
}

# reprojects and sp object
# - takes an sp object
# and returns
# - the same object reprojected to WGS84
wgsProj <- function(spObj) {
  if (!is.null(spObj)) {
    return(spTransform(spObj, CRS("+proj=longlat +datum=WGS84")))
  }
}

# tidies sp object for plotting with ggplot
# - takes an sp object
# - and a string name of a unique id field
# and returns
# - a tidied data frame
tidyPoly <- function(sp, id) {
  if (!is.null(sp) && id != "") {
    
    # tidy the sp object
    spTidy <- broom::tidy(sp, region = id)
    
    # convert the id to numeric
    spTidy$var <- as.numeric(spTidy$id)
    
    # rename a variable for joining
    names(spTidy)[ncol(spTidy)] <- id
    
    # join the original features back to the tidied dataset
    spTidy <- left_join(spTidy, sp@data)
    
    # return the tidied dataset
    return(spTidy)
  }
}

# converts spatialpointsdataframe to regular data frame with long/lat fields
# - takes a spPoints object
# and returns
# - a data frame with long/lat fields
ptJsonToDf <- function(spPoints) {
  newDat <- cbind(spPoints@data, spPoints@coords)
  names(newDat)[(ncol(newDat) - 1):ncol(newDat)] <- c("long", "lat")
  return(newDat)
}

# plot polygons
# - takes a polygon sp object
# - a string identifying the id field
# - a string identifying the field to be visualized
# - title, if the user wants to add one
# - a response indicating what kind of basemap to use
# and returns:
# - a ggplot
polygonPlot <- function(sp, idField, vizField, mapTitle, addBasemap) {
  
  if(is.numeric(sp@data[ ,vizField])) {
    sp@data <- quintileRamp(sp, vizField)
  } else {
    sp@data$fillVar <- sp@data[ ,vizField]
  }
  
  print(head(sp@data))
  
  spTidy <- tidyPoly(sp, idField)
    
  if (!is.null(addBasemap)) {
    
    if (addBasemap == "dark") {
      theBasemap <- invert_basemap(get_bm(sp))
    } else {
      theBasemap <- get_bm(sp)
    }
    
    plot <- ggmap(theBasemap) + 
      geom_polygon(data = spTidy,aes(x = long, y = lat, group = group, fill = fillVar)) +
      ggtitle(mapTitle) +
      coord_map() +
      mapTheme() +
      scale_fill_manual(vizField, values = skRamp)
  } else {
    plot <- ggplot() + 
      geom_polygon(data = spTidy,aes(x = long, y = lat, group = group, fill = fillVar)) +
      ggtitle(mapTitle) +
      coord_map() +
      mapTheme() +
      scale_fill_manual(vizField, values = skRamp)
  }
  
  return(plot)
}

# plot polygons
# - takes a point sp object
# - a string identifying the id field
# - a string identifying the field to be visualized
# - title, if the user wants to add one
# - a response indicating what kind of basemap to use
# and returns:
# - a ggplot
pointPlot <- function(sp, idField, vizField, mapTitle, addBasemap) {
  if(is.numeric(sp@data[ ,vizField])) {
    sp@data <- quintileRamp(sp, vizField)
  } else {
    sp@data$fillVar <- sp@data[ ,vizField]
  }
  
  print(head(sp@data))
  
  spTidy <- ptJsonToDf(sp)
  
  if (!is.null(addBasemap)) {
    
    if (addBasemap == "dark") {
      theBasemap <- invert_basemap(get_bm(sp))
    } else {
      theBasemap <- get_bm(sp)
    }
    
    plot <- ggmap(theBasemap) + 
      geom_point(data = spTidy, aes(x = long, y = lat, color = fillVar), size = 2) +
      ggtitle(mapTitle) +
      coord_map() +
      mapTheme() +
      scale_color_manual(vizField, values = skRamp)
  } else {
    plot <- ggplot() + 
      geom_point(data = spTidy, aes(x = long, y = lat, color = fillVar), size = 2) +
      ggtitle(mapTitle) +
      coord_map() +
      mapTheme() +
      scale_color_manual(vizField, values = skRamp)
  }
  
  return(plot)
}

# defines a legend-ready field to use for a quintile color schemed
# - takes an sp object 
# - string columns name from the sp object
# and returns
# - the sp object's original data frame with new variables
quintileRamp <- function(sp, col) {
  
  # assign each row to a quintile
  sp$quints <- ntile(sp@data[ ,col], 5)
  
  # define legend labels
  labels <- unname(quantile(sp@data[ ,col], seq(.2, .8, .2))) %>% round(digits = 0)
  
  # define string range labels 
  newLabels <- c()
  for (i in c(1:5)) {
    if (i == 1) {
      newLabels[i] <- paste0("< ", labels[1])
    } else if (i == 5) {
      newLabels[i] <- paste0("> ", labels[4])
    } else {
      newLabels[i] <- paste0(labels[i - 1], " - ", labels[i])
    }
  }
  
  sp$quints <- newLabels[sp$quints]
  sp$fillVar <- factor(sp$quints, levels = newLabels)
  
  return(sp@data)
}

# get a stamen basemap
# - takes an sp object
# and returns
# - a basemap
# this function calls getBbox
get_bm <- function(sp) {
  basemap <- get_map(getBbox(sp, .15), 
                     zoom = 11, 
                     maptype = "toner",
                     force = FALSE)
  return(basemap)
}

# gets a bounding box that extends evenly beyond the 
# margins of what I'm mapping
# - takes a spatial object (sp)
# - and a decimal (padding) that defines what proportion
#   of the objects width and height should be used as margins
# and returns
# - a named bounding box vector
getBbox <- function(sp, padding) {
  bb <- sp@bbox %>% data.frame()
  
  xPad <- abs((abs(bb[1,1]) - abs(bb[1,2]))) * padding
  yPad <- abs((abs(bb[2,1]) - abs(bb[2,2]))) * padding
  
  bbox_padded <- c(left = bb$min[1] - xPad, 
                   bottom = bb$min[2] - yPad, 
                   right = bb$max[1] + xPad, 
                   top = bb$max[2] + yPad)
  return(bbox_padded)
}

# invert colors of a basemap
# - takes a basemap as input
# and returns
# - a new basemap with the colors inverted
invert_basemap <- function(basemap) {
  inverted <- apply(basemap, 2, function(x) rgb(t(255-col2rgb(x))/255)) %>%
    as.raster()
  
  class(inverted) <- class(basemap)
  attr(inverted, "bb") <- attr(basemap, "bb")
  
  return(inverted)
}

# the color palette I made for these maps
skRamp <- c("#1375BE", "#88738A", "#FD7156", "#FCA554", "#FCDA52")

# maptheme
# defines theme parameters for the maps
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text(color = "black", family = "Helvetica"),
    plot.title = element_text(size = 24, colour = "black", face = "bold"),
    plot.subtitle=element_text(face="italic", size = 16),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    strip.text = element_text(size=12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}