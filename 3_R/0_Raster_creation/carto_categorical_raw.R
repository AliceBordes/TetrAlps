#### PhD TetraAlps project ####

# Alice Bordes #

# December 2024 #

# Description:

# Formatting environment dataset for RSF and SSF



### Loading libraries ---- 
#********************************************************************
library(terra)
#********************************************************************

### Settings
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD"
#********************************************************************

# Loading rasters ----
#********************************************************************
### RASTERS
carto_habitats_3V <- terra::rast(file.path(base,"TetrAlps","2_DATA","carto_habitats_clara_3V.tif")) #carto Clara
#********************************************************************



# Create the raster carto habitats winter
#********************************************************************
# Set the levels explicitly (from 1 to 16)
  # levels_hab <- levels(carto_habitats_3V)[[1]]
  # hab_categories = data.frame(ID=c(20,21,22,23,30,31,32,40,50,51,52,60,92,93,94,100), categories = c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified"))
  # levels(carto_habitats_3V) <- hab_categories


# Aggregate habitat categories
carto_habitats_3V_winter <- carto_habitats_3V


# Reclassify categories to create a raster of winter vegetation
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(20, 1)) #Unclassified soil
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(21, 1)) #Fine mineral soil
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(22, 1)) #Coarse mineral soil
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(30, 1)) #Dry or rocky grassland
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(31, 1)) #Herbaceous
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(32, 1)) #Low ligneous
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(92, 1)) #Natural pond
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(93, 1)) #Artificial pond
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(94, 1)) #Waterway
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(100, 1)) #Unclassified

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(40, 2)) #Shrubs

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(50, 3)) #Unclassified trees
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(51, 3)) #Deciduous trees
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(52, 3)) #Resinous trees

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(23, 4)) #Cliff

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(60, 5)) #Buildings


carto_habitats_3V_winter <- as.factor(carto_habitats_3V_winter)

# Renaming categories
levels(carto_habitats_3V_winter) <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Label = c(
    "Soils_low_vegetation",
    "Shrubs",
    # "Deciduous_trees",
    # "Resinous_trees",
    "Trees",
    "Cliffs",
    "Buildings"
    # "Unclassified_trees"
  )
)


is.factor(carto_habitats_3V_winter)




writeRaster(carto_habitats_3V_winter, filename = file.path(base,"TetrAlps", "2_DATA", "environmental_raster", "carto_habitats_3V_winter_5classes_tree.tif"), overwrite=TRUE)
#********************************************************************


