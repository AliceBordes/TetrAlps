#### PhD TetraAlps project ####

# Alice Bordes #

# August 2024 #

# Description:

# Formatting environment dataset for RSF and SSF



### Loading libraries ---- 
#********************************************************************
library(sf)
library(raster)
library(terra)
library(dplyr)
#********************************************************************


### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD"
#********************************************************************


# Loading rasters ----
#********************************************************************
### GRID

grid_10m <- terra::rast(terra::vect(file.path(base, "Tetralps","1_RAW_DATA","grid_10m.gpkg")))
res(grid_10m) <- 10


### RASTERS

# habitat cartography
carto_habitats_3V_winter <- terra::rast(file.path(base, "TetrAlps","2_DATA","environmental_raster","carto_habitats_3V_winter_5classes_tree.tif"))
#********************************************************************



### 1_Scale the environment stack included binary rasters ----
#********************************************************************
#'Dealing with categorical raster 

# dummy method : we can create as many dummy variables as the categorical raster has classes. dummy variable = takes a binary value (0 or 1) 
# Dummy variables are commonly used in regression analysis to represent categorical variables that have more than two levels, such as education level or occupation. 
# In this case, multiple dummy variables would be created to represent each level of the variable, and only one dummy variable would take on a value of 1 for each observation.

# Align the rasters carto_habitats_3V_winter and mnt
# carto_habitats_3V_winter <- project(carto_habitats_3V_winter, y = mnt, method = "near")
carto_habitats_3V_winter <- raster::raster(carto_habitats_3V_winter)
terra::crs(carto_habitats_3V_winter) <- "EPSG:2154"

# Assume scaled_env_RL_list[["carto_habitats_winter"]] is your raster with 5 classes
# carto_habitats_winter_bin <- scaled_env_RL_list[["carto_habitats_winter"]]
carto_habitats_winter_bin <- carto_habitats_3V_winter

# Identify the unique classes in the raster
classes <- unique(values(carto_habitats_winter_bin))

# Initialize a list to store the binary rasters
carto_habitats_winter_bins <- list()

# Loop to create a binary raster for each class
for (class in classes[!(is.nan(classes) | is.na(classes))]) {
  
  # Create a binary raster where pixels of the specified class are 1, others are 0
  carto_habitats_winter_binary <- calc(carto_habitats_winter_bin, fun = function(x) { as.integer(x == class) })
  
  # Assign a meaningful name based on the class
  new_name <- case_when(
    class == 1 ~ "Soils_low_vegetation",
    class == 2 ~ "Shrubs",
    class == 3 ~ "Trees",
    class == 4 ~ "Cliffs",
    class == 5 ~ "Buildings"
  )
  
  # Set the name of the binary raster
  names(carto_habitats_winter_binary) <- new_name
  
  carto_habitats_winter_binary <- aggregate(carto_habitats_winter_binary, fact = 10, fun = "mean")
  
  terra::writeRaster(carto_habitats_winter_binary,
                     filename = file.path(base,paste0("TetrAlps/2_DATA/environmental_raster/scaled_bin_",new_name,".tif")),
                     overwrite = TRUE)
  
  # Add the binary raster to the list
  carto_habitats_winter_bins[[new_name]] <- carto_habitats_winter_binary
}
#********************************************************************





