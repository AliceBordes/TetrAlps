#### PhD TetraAlps project ####

# Alice Bordes #

# December 2024 #

# Description:

# Formatting the raster "leks"



### Loading libraries ---- 
#********************************************************************
library(move2)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth) #needs rnaturalearthhires also installed
library(ggmap)
library(ggspatial) ## needs prettymapr also installed
library(gganimate)
library(plotly)
library(mapview)
library(units)
library(lubridate)
# library(moveVis)
library(raster)
library(terra)
library(future.apply)
library(tidyterra)
library(ggnewscale)
library(broom)
library(janitor)
library(lubridate)
library(openxlsx)
library(tidyverse)
library(dplyr)
#********************************************************************


### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD"
#********************************************************************


# Loading rasters ----
#********************************************************************
### RASTERS

# elevation
mnt_9 <- terra::rast(file.path(base, "Tetralps/2_DATA/mnt_9_mean_ign.tif"))
mnt_9 <- project(mnt_9, "EPSG:2154")

# leks
leks <- st_read(file.path(base,"TetrAlps","1_RAW_DATA","display_sites","place_chant_02_07_2024.gpkg")) # read the geometry object
r_leks <- terra::rast(ext(mnt_9), resolution = 1, crs = "EPSG:2154") # create a raster template 
r_leks <- rasterize(leks, r_leks) # transform the geometry into a raster 
r_leks[is.na(r_leks)] <- 0 # Replace NA with 0
r_leks <- as.factor(r_leks)
#********************************************************************


# Creation of the raster "leks"
#********************************************************************
# Buffer around leks sites
# Convert sf object to SpatVector
leks_vect <- vect(leks)
# Create a raster template with 9 meters res!olution
raster_template <- rast(ext(mnt_9), resolution = 1, crs = terra::crs(mnt_9))
# Rasterize the buffered polygon (1 for inside the polygon, NA for outside)
polygon_raster <- rasterize(leks_vect, raster_template, field = 1, background = NA)
# assign 0 to NA (areas outside the polygon)
polygon_raster[is.na(polygon_raster)] <- 0
# plot(polygon_raster)
# Calculate distance to the nearest border of the polygon
distance_to_border <- terra::distance(polygon_raster, target = polygon_raster, unit = "m")
# terra::plot(distance_to_border)
# Create a 600-meter buffer around the polygon (on the SpatVector)
buffer_vect <- buffer(leks_vect, width = 600)
# Create a raster for masking (optional: if you want to restrict calculations to certain areas)
# For example, creating a mask raster of the buffer area if needed
buffer_raster <- rasterize(buffer_vect, raster_template, field = 1, background = NA)
# Mask the distance raster to only include values within the buffer (if masking is necessary)
distance_buffer <- mask(distance_to_border, buffer_raster)
# terra::plot(distance_buffer)
# Normalize the distance values to range from 0 to 1
# Ensure non-NA values are normalized
min_distance <- min(values(distance_buffer), na.rm = TRUE)
max_distance <- max(values(distance_buffer), na.rm = TRUE)
r_leks_dist <- (distance_buffer - min_distance) / (max_distance - min_distance)
r_leks_dist <- 1-r_leks_dist
r_leks_dist[is.na(r_leks_dist)] <- 0
# terra::plot(r_leks_dist, main = "Proxy of the proximity to lek places")

terra::writeRaster(r_leks_dist, file = file.path(base,"TetrAlps/2_DATA/environmental_raster/r_leks_dist.gpkg"), overwrite=TRUE)
#********************************************************************
