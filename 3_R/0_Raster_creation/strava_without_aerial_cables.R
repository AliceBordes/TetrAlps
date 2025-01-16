#### PhD TetraAlps project ####

# Alice Bordes #

# December 2024 #

# Description:

# Formatting the raster "strava" and "strava_backountry_ski"



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

# Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/Formatting_data/formatting_environment_data.R")
#********************************************************************


### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD"
#********************************************************************


# Loading rasters ----
#********************************************************************
### RASTERS

# strava
strava <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_3V_winter_sports_rgb_single_3Vallees_25_06_2024.tif"))
if(length(names(strava))!=1)
{
  strava <- strava::as_numeric(strava)
}

#if strava = rgb with 4 layers : 
# strava <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_winter_2024_10_03.tif"))
# strava_rgb <- terra::plotRGB(strava)
# strava_rgb <- terra::plot(strava::as_numeric(strava), col = viridis::magma(256))

# cables
ski_lift_traffic_3V <- terra::vect("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_lift_traffic_3V.gpkg")
# ggplot()+geom_spatvector(data = st_as_sf(ski_lift_traffic_3V), aes(color = aerialway))+ggtitle("Ski lifts Open Street Map by type")

# 3V winter trails (osm)
osm_winter <- terra::vect(file.path(base,"TetrAlps","2_DATA", "osm_ski_piste.gpkg"))
# ggplot()+geom_spatvector(data = osm_winter, aes(color = piste.type))+ggtitle("Ski trails Open Street Map by type")

#********************************************************************






# 1_Creation of the raster "strava without risky cables" ----
#********************************************************************
# We always work in the base projection of the raster to avoid distance distortion
ski_lift_traffic_3V <- project(ski_lift_traffic_3V, crs(strava))
osm_winter <- project(osm_winter, crs(strava))

# 1.1_Creation of the raster "osm aerial cables" ----
#********************************************************************

# 1.1.1_Defining "risky" versus "aerial cables" ----
#********************************************************************
# Define the categories
aerial_cables <- c("cable_car", "gondola", "chair_lift", "zip_line")
risky_cables <- c("drag_lift", "platter", "rope_tow", "magic_carpet", "goods")

# Add a new grouping column to the data
ski_lift_traffic_3V <- ski_lift_traffic_3V %>%
  mutate(
    cable_type = case_when(
      aerialway %in% aerial_cables ~ "Aerial",
      aerialway %in% risky_cables ~ "Risky",
      TRUE ~ "Other"  # Catch-all for unclassified types
    )
  )

ski_lift_traffic_3V_sf <- st_as_sf(ski_lift_traffic_3V)
# Create a buffer of 30 meters around the cables for data visualization
ski_lift_traffic_3V_sf$geometry_buffer <- st_buffer(ski_lift_traffic_3V_sf$geometry, dist = 30)
#********************************************************************


# 1.1.2_Visualizing cables by type (risky VS aerial) ----
#********************************************************************
# Plot with grouped colors
ggplot() +
  geom_spatvector(data = ski_lift_traffic_3V, aes(color = cable_type)) +
  scale_color_manual(name = "Cable types",
                     values = c(
                       "Aerial" = "blue",
                       "Risky" = "red",
                       "Other" = "gray"
                     )
  ) +
  ggtitle("Ski lifts by type")


ggplot() +
  geom_spatvector(data = ski_lift_traffic_3V_sf, aes(geometry = geometry_buffer, color = cable_type, fill = cable_type)) +
  scale_color_manual(name = "Cable types",
                     values = c(
                       "Aerial" = "blue",
                       "Risky" = "red",
                       "Other" = "gray"
                     )
  ) +
  scale_fill_manual(name = "Cable types",
                    values = c(
                      "Aerial" = "blue",
                      "Risky" = "red",
                      "Other" = "gray"
                    )
  ) +
  ggtitle("Ski lifts by type\nBuffer = 30m")
#********************************************************************



# 1.2_Creation of the new strava raster ----
#********************************************************************

# Step 1: Mask the Strava raster to exclude the ski lift areas (set them to NA)
# Mask the Strava mean raster using the ski lift raster to keep only "Aerial" ski lift locations (and keeping risky ski lift original strava value)
# Convert sf geometry buffer to terra SpatVector
buffer_mask_aerial <- vect(ski_lift_traffic_3V_sf[ski_lift_traffic_3V_sf$cable_type == "Aerial", ]$geometry_buffer)
# Reproject buffer_mask to match the Strava raster CRS
buffer_mask_aerial <- project(buffer_mask_aerial, terra::crs(strava))
# Mask the Strava raster, keeping the values under the ski lifts and setting everything outside to NA
masked_strava_aerial <- mask(strava, buffer_mask_aerial, inverse = TRUE)


# Step 2: Apply a huge smoothing the masked raster (to "removed" ski lifts)
# Apply a focal function to smooth the raster (mean of neighbors)
# Here, we'll use a 3x3 moving window (you can adjust the window size)
smoothed_strava <- focal(masked_strava_aerial, w = matrix(1, 9, 9), fun = mean, na.policy = "all", na.rm = TRUE)
# because my raster has a resolution of 26m per cell, this window would cover an area of approximately 234m×234m around each cell (9*26=650).


# Step 3: Combine the smoothed raster with the original raster
# Keep original Strava values where not under ski lift, and fill the NA areas with the smoothed values
strava_without_aerial <- masked_strava_aerial
strava_without_aerial[is.na(strava_without_aerial)] <- smoothed_strava[is.na(strava_without_aerial)]


# Plot the result
par(mfrow=c(1,2))
plot(strava_without_aerial, main = "Smoothed Strava Raster with Ski Lift Areas Removed")
plot(strava, main = "Original Strava Raster")


# Focus on an area to see in details the new raster 
  # Define the extent of the area of interest (xmin, xmax, ymin, ymax)
  small_area <- ext(977000, 984000, 6470000, 6476000)  # Example coordinates
  small_area_poly <- as_spatvector(as.polygons(small_area))
  crs(small_area_poly) <- "epsg:2154"
  small_area <- project(small_area_poly, crs(strava))
  # Crop the raster to this extent
  cropped_strava <- crop(strava, small_area)
  cropped_strava_without_aerial <- crop(strava_without_aerial, small_area)
  # Plot the cropped rasters
  par(mfrow=c(1,2))
  plot(cropped_strava_without_aerial, main = "Smoothed Strava Raster with Ski Lift Areas Removed\nBuffer around ski lift = 30m")
  plot(cropped_strava, main = "Original Strava Raster")
#********************************************************************



# 1.3_Improve the new strava raster by reseting real strava value to ski trails ----
#********************************************************************
# Reset the real values of strava intensity on ski trails when smoothed because under a ski lift

osm_winter_sf <- as_sf(osm_winter)
# Create a buffer of 30 meters around the cables for data visualization
osm_winter_sf$geometry_buffer <- st_buffer(osm_winter_sf$geometry, dist = 30)

# Step 1: Mask the Strava raster to exclude the ski lift areas (set them to NA)
# Mask the Strava mean raster using the ski lift raster to keep only "Aerial" ski lift locations (and keeping risky ski lift original strava value)
# Convert sf geometry buffer to terra SpatVector
buffer_mask_trails <- vect(osm_winter_sf[osm_winter_sf$piste.type == "downhill", ]$geometry_buffer)
# Reproject buffer_mask to match the Strava raster CRS
buffer_mask_trails <- project(buffer_mask_trails, terra::crs(strava))
# Mask the Strava raster, keeping the values under the ski lifts and setting everything outside to NA
masked_strava_trails <- mask(strava, buffer_mask_trails)

# Step 2: Identify non-NA values in the mask
valid_values <- !is.na(masked_strava_trails)

# Step 3: Replace values in the smoothed raster with real values from the mask
strava_without_aerial_corr <- strava_without_aerial
strava_without_aerial_corr[valid_values] <- masked_strava_trails[valid_values]

# Plot the result
par(mfrow=c(1,2))
plot(strava_without_aerial_corr, main = "Final Strava Raster with Ski Lift Areas Removed")
plot(strava, main = "Original Strava Raster")


# Focus on an area to see in details the new raster 
# Crop the raster to this extent
cropped_strava <- crop(strava, small_area)
cropped_strava_without_aerial_corr <- crop(strava_without_aerial_corr, small_area)
# Plot the cropped rasters
par(mfrow=c(1,2))
plot(cropped_strava_without_aerial_corr, main = "Final Strava Raster with Ski Lift Areas Removed\nBuffer around ski lift = 30m")
plot(cropped_strava, main = "Original Strava Raster")


terra::writeRaster(strava_without_aerial_corr, file = file.path(base,"TetrAlps/2_DATA/environmental_raster/r_strava_without_aerial_cables.gpkg"), overwrite=TRUE)
#********************************************************************


# 
# 
# 
# # 2_Creation of the raster "backountry winter trails" = "strava without risky cables" - osm downhill ski trails ----
# #********************************************************************
# 
# # 2.1_Method 1 : Creation of the raster strava without risky cables and without osm downhill ski trails ----
# #********************************************************************
# 
# strava_without_aerial <- terra::rast(file.path(base,"TetrAlps/2_DATA/environmental_raster/r_strava_without_aerial_cables.gpkg"))
# 
# # Create a buffer of 30 meters around the cables for data visualization
# osm_winter_sf <- as_sf(osm_winter)
# osm_winter_sf$geometry_buffer <- st_buffer(osm_winter_sf$geometry, dist = 100) # 100 m = distance of flight from disturbance + estimated area often small "off-trail" 
# 
# # Step 1: Mask the Strava raster to exclude the ski lift areas (set them to NA)
# # Mask the Strava mean raster using the ski lift raster to keep only "Aerial" ski lift locations (and keeping risky ski lift original strava value)
# # Convert sf geometry buffer to terra SpatVector
# buffer_mask_backountry <- vect(osm_winter_sf[osm_winter_sf$piste.type == "downhill", ]$geometry_buffer)
# # Reproject buffer_mask to match the Strava raster CRS
# buffer_mask_backountry <- project(buffer_mask_backountry, crs(strava_without_aerial))
# 
# # Mask the Strava raster: High values within the buffer are suppressed
# masked_strava_backountry <- mask(strava_without_aerial, buffer_mask_backountry, inverse = TRUE)  # Apply the mask to the Strava raster
# masked_strava_backountry[masked_strava_backountry > threshold] <- NA  # Suppress high values in the masked areas
# # Mask the Strava raster, keeping the values under the ski lifts and setting everything outside to NA
# # masked_strava_backountry <- mask(strava_without_aerial, buffer_mask_backountry, inverse = TRUE)
# 
# 
# # Step 2: Apply a huge smoothing the masked raster (to "removed" ski lifts)
# # Apply a focal function to smooth the raster (mean of neighbors)
# # Here, we'll use a 3x3 moving window (you can adjust the window size)
# smoothed_strava_backountry <- focal(masked_strava_backountry, w = matrix(1, 25, 25), fun = mean, na.policy = "all", na.rm = TRUE)
# 
# 
# 
# # Step 3: Combine the smoothed raster with the original raster
# # Keep original Strava values where not under ski lift, and fill the NA areas with the smoothed values
# strava_backountry <- masked_strava_backountry
# strava_backountry[is.na(strava_backountry)] <- smoothed_strava_backountry[is.na(strava_backountry)]
# 
# # Plot the result
# par(mfrow=c(1,2))
# plot(strava_backountry, main = "Smoothed Strava Raster with osm ski trails removed")
# plot(strava, main = "Original Strava Raster")
# 
# 
# # Focus on an area to see in details the new raster 
# # Define the extent of the area of interest (xmin, xmax, ymin, ymax)
# small_area <- ext(977000, 984000, 6470000, 6476000)  # Example coordinates
# # Crop the raster to this extent
# cropped_strava <- crop(strava, small_area)
# cropped_strava_backountry <- crop(strava_backountry, small_area)
# # Plot the cropped rasters
# par(mfrow=c(1,2))
# plot(cropped_strava_backountry, main = "Smoothed Strava Raster with Ski Lift Areas Removed\nBuffer around ski lift = 50m")
# plot(cropped_strava, main = "Original Strava Raster")
# 
# terra::writeRaster(strava_backountry, file = file.path(base,"TetrAlps/2_DATA/environmental_raster/r_strava_backountry.gpkg"), overwrite=TRUE)
# #********************************************************************
# 
# 
# # 2.2_Method 2 : Creation of the raster strava without risky cables and without osm downhill ski trails ----
# #********************************************************************
# 
# strava_without_aerial <- terra::rast(file.path(base,"TetrAlps/2_DATA/environmental_raster/r_strava_without_aerial_cables.gpkg"))
# 
# # Create a buffer of 30 meters around the cables for data visualization
# osm_winter_sf <- as_sf(osm_winter)
# osm_winter_sf$geometry_buffer <- st_buffer(osm_winter_sf$geometry, dist = 100) # 100 m = distance of flight from disturbance + estimated area often small "off-trail" 
# 
# # Define the intensity threshold (e.g., suppress values above 50)
# threshold <- 60
# strava_without_aerial[strava_without_aerial > threshold] <- NA
# plot(strava_without_aerial)
# 
# # Visualizing
# downhill <- project(vect(osm_winter_sf[osm_winter_sf$piste.type == "downhill", ]$geometry), crs(strava_without_aerial))
# ggplot() +
#   geom_spatraster(data = masked_strava_backountry) +
#   scale_fill_viridis_c(option = "viridis") +  # Correct way to apply Viridis color scale
#   theme_minimal()
# 
# # Finding the first quantile = value to replace NA? 
# # Déterminer les breaks de Jenks pour les valeurs non nulles
# # num_classes <- 4 # Nombre de classes Jenks
# # jenks_breaks <- classInt::classIntervals(values(masked_strava_backountry), n = num_classes, style = "jenks")
# # print(jenks_breaks)
# 
# ggplot()+
#   geom_histogram(data = values(masked_strava_backountry), aes(y = values(masked_strava_backountry)))
# 
# # Plot the result
# par(mfrow=c(1,2))
# plot(strava_backountry, main = "Smoothed Strava Raster with osm ski trails removed")
# plot(strava, main = "Original Strava Raster")
# 
# 
# # Focus on an area to see in details the new raster 
# # Define the extent of the area of interest (xmin, xmax, ymin, ymax)
# small_area <- ext(977000, 984000, 6470000, 6476000)  # Example coordinates
# # Crop the raster to this extent
# cropped_strava <- crop(strava, small_area)
# cropped_strava_backountry <- crop(strava_backountry, small_area)
# # Plot the cropped rasters
# par(mfrow=c(1,2))
# plot(cropped_strava_backountry, main = "Smoothed Strava Raster with Ski Lift Areas Removed\nBuffer around ski lift = 50m")
# plot(cropped_strava, main = "Original Strava Raster")
# 
# terra::writeRaster(strava_backountry, file = file.path(base,"TetrAlps/2_DATA/environmental_raster/r_strava_backountry.gpkg"), overwrite=TRUE)
# #********************************************************************
# 
# 
# 
# # 2.2_Improve the new strava raster by reseting real strava value to secondary ski trails ---- 
# #********************************************************************
# #BAD CORRECTION
# 
# # Reset the real values of strava intensity on ski trails when smoothed because under a ski lift
# 
# # 
# # # Step 1: Mask the Strava raster to exclude the ski lift areas (set them to NA)
# # # Mask the Strava mean raster using the ski lift raster to keep only "Aerial" ski lift locations (and keeping risky ski lift original strava value)
# # # Convert sf geometry buffer to terra SpatVector
# # buffer_mask_trails_secondary <- vect(osm_winter_sf[osm_winter_sf$piste.type != "downhill", ]$geometry_buffer)
# # # Reproject buffer_mask to match the Strava raster CRS
# # buffer_mask_trails_secondary <- project(buffer_mask_trails_secondary, crs(strava))
# # # Mask the Strava raster, keeping the values under the ski lifts and setting everything outside to NA
# # masked_strava_trails_secondary <- mask(strava, buffer_mask_trails_secondary)
# # 
# # # Step 2: Identify non-NA values in the mask
# # valid_values2 <- !is.na(masked_strava_trails_secondary)
# # 
# # # Step 3: Replace values in the smoothed raster with real values from the mask
# # strava_backountry_corr <- strava_backountry
# # strava_backountry_corr[valid_values2] <- masked_strava_trails_secondary[valid_values2]
# # 
# # # Plot the result
# # par(mfrow=c(1,2))
# # plot(strava_backountry_corr, main = "Final Strava Raster with Ski Lift Areas Removed")
# # plot(strava, main = "Original Strava Raster")
# # 
# # 
# # # Focus on an area to see in details the new raster 
# # # Define the extent of the area of interest (xmin, xmax, ymin, ymax)
# # small_area <- ext(977000, 984000, 6470000, 6476000)  # Example coordinates
# # # Crop the raster to this extent
# # cropped_strava <- crop(strava, small_area)
# # cropped_strava_backountry_corr <- crop(strava_backountry_corr, small_area)
# # # Plot the cropped rasters
# # par(mfrow=c(1,2))
# # plot(cropped_strava_backountry_corr, main = "Final Strava Raster with Ski Lift Areas Removed\nBuffer around ski lift = 30m")
# # plot(cropped_strava, main = "Original Strava Raster")
# # 
# # 
# # terra::writeRaster(strava_backountry, file = file.path(base,"TetrAlps/2_DATA/environmental_raster/r_strava_backountry.gpkg"), overwrite=TRUE)
# #********************************************************************
# 
# 
# 
# 




