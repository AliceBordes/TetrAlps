#### PhD TetraAlps project ####

# Alice Bordes #

# January 2025 #

# Description:

# Formatting environment dataset for RSF and SSF



### Loading libraries ---- 
#********************************************************************
library(terra)
library(sf)
library(ggplot2)
library(tidyterra)
library(ggnewscale)
library(gridExtra)
library(grid)
#********************************************************************

### Settings
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD"
#********************************************************************

### Loading data ----
#********************************************************************
### RASTER

# habitat cartography
carto_habitats_3V_winter <- terra::rast(file.path(base,"TetrAlps", "2_DATA", "environmental_raster", "carto_habitats_3V_winter_7classes_tree.tif"))
# r_Trees <- terra::rast(file.path(base,paste0("TetrAlps/2_DATA/environmental_raster/scaled_bin_Trees.tif")))

# Geoservices cartography
soil_cover_geoservices <- terra::rast(file.path(base, "TetrAlps","1_RAW_DATA","environment","habitats","OCS_GE_soil_cover_2019_winter.tif"))
crs(soil_cover_geoservices) <- crs(carto_habitats_3V_winter)

### VECTORS

# 3V borders 
borders_3V_vect <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect <- terra::vect(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
#********************************************************************


# Creating the soil_cover raster ----
#********************************************************************
# Loading the gpkg
# soil_cover_geoservices <- st_read(file.path(base, "TetrAlps","1_RAW_DATA","environment","habitats","OCS_GE_soil_cover_2019.gpkg"))
# 
# 
# bbox <- st_bbox(soil_cover_geoservices)
# resolution <- 1  # Set your desired resolution (in CRS units)
# rast <- rast(xmin = bbox$xmin, xmax = bbox$xmax,
#              ymin = bbox$ymin, ymax = bbox$ymax,
#              resolution = resolution, crs = crs(soil_cover_geoservices))
# 
# # Loading the winter legend
# soil_cover_geoservices$LEGEND_WINTER <- as.factor(soil_cover_geoservices$LEGEND_WINTER)
# 
# # Creating the new raster
# soil_cover_geoservices_r <- rasterize(vect(soil_cover_geoservices), rast, field = "LEGEND_WINTER")
# 
# # 0 = 1 = Deciduous_trees 2 = 3 = 4 = 5 = 
# 
# writeRaster(soil_cover_geoservices_r, file = file.path(base, "TetrAlps","1_RAW_DATA","environment","habitats","OCS_GE_soil_cover_2019_winter.tif"), overwrite=TRUE)
#********************************************************************

# 1_Visualization soil cover ----
#********************************************************************
terra::plot(soil_cover_geoservices, main = "Winter categories of habitats based on the soil cover from geoservices")
terra::plot(borders_3V_vect, add=TRUE)
#********************************************************************

# 2_Comparing winter habitats categories based on Clara VS Geoservices ----
#********************************************************************
carto_cl <- carto_habitats_3V_winter

ggplot()+
  geom_spatraster(data = carto_cl)+
  scale_fill_manual(name = "Habitat classes",
                      values = c(
                        "Soils_low_vegetation"="#CCCCCC",
                        "Shrubs"="#FF99CC",
                        "Deciduous_trees"="#99FF99",
                        "Resinous_trees"="#339966",
                        "Cliffs"="#333333",
                        "Buildings"="#993300",
                        "Unclassified_trees" = "#CCFF33"),
                        na.value ="transparent")+
  geom_spatvector(data = borders_3V_vect,
                  fill = NA)+
  ggtitle("From Clara")

ggplot() +
  geom_spatraster(data = soil_cover_geoservices) +
  scale_fill_manual(
    name = "Habitat classes",
    values = c(
      "Soils_low_vegetation" = "#CCCCCC",
      "Shrubs_and_Low_ligneous" = "#FF99CC",
      "Deciduous_trees" = "#99FF99",
      "Resinous_trees" = "#339966",
      "Mixted_trees" = "#CCFF33",
      "Buildings" = "#993300"
    ),
    na.value = "transparent"
  )+
  geom_spatvector(data = borders_3V_vect,
                  fill = NA)+
  ggtitle("From Geoservices")

# Aligned the rasters
soil_cover_geoservices_aligned <- resample(soil_cover_geoservices, carto_cl) 
# Check alignment
compareGeom(carto_cl, soil_cover_geoservices_aligned)

# Get unique categories from both rasters
levels(soil_cover_geoservices)[[1]]$LEGEND_WINTER

# Initialize a data frame to store results
cat_match <- data.frame(Categories_Cl = c(levels(carto_cl)[[1]]$Label,
                                          
                                          "Deciduous_trees",
                                          "Resinous_trees",
                                          
                                          "Unclassified_trees",
                                          "Unclassified_trees",
                                          
                                          "Deciduous_trees",
                                          "Resinous_trees"), 
                        Categories_Geoservices = c( "Soils_low_vegetation",
                                                    "Shrubs_and_Low_ligneous",
                                                    "Mixted_trees",
                                                    "Deciduous_trees",
                                                    "Resinous_trees",
                                                    "NA",
                                                    "Buildings",
                                                    
                                                    "Mixted_trees",
                                                    "Mixted_trees",
                                                    
                                                    "Deciduous_trees",
                                                    "Resinous_trees",
                                                    
                                                    "Resinous_trees",
                                                    "Deciduous_trees"), 
                        PercentMatch = NA)
# Remove rows where 'Categories_Cl' equals "Cliffs"
cat_match <- cat_match[cat_match$Categories_Cl != "Cliffs", ]

# Loop through each category
for (cat in 1:nrow(cat_match)) {
    # Create masks for the category in each raster
    mask1 <- carto_cl == cat_match$Categories_Cl[cat]
    mask2 <- soil_cover_geoservices_aligned == cat_match$Categories_Geoservices[cat]
    
    # Calculate the total number of pixels for the category in raster1
    total_pixels <- terra::global(mask1, "sum", na.rm = TRUE)[1]
    
    # Calculate the number of matching pixels in both rasters
    matching_pixels <- terra::global(mask1 & mask2, "sum", na.rm = TRUE)[1]
    
    # Calculate the percentage match
    percent_match <- round((matching_pixels / total_pixels) * 100,1)
    
    # Store the result
    cat_match[cat, "PercentMatch"] <- percent_match
  }

cat_match





# Visualizing the match, zooming on a specific zone
# choosing the zone : 
writeVector(as.polygons(ext(970000,973000,6479500, 6481000)), file = file.path(base, "TetrAlps","1_RAW_DATA"))
writeVector(as.polygons(ext(963981,1002357,6464369, 6495464)), file = file.path(base, "TetrAlps","1_RAW_DATA","mini_sect2.gpkg"))

# Visualizing the differences between rasters
g_cl <- ggplot()+
  geom_spatraster(data = terra::crop(carto_cl, ext(970000,973000,6479500, 6481000)))+
  scale_fill_manual(name = "Habitat classes",
                    values = c(
                      "Soils_low_vegetation"="#CCCCCC",
                      "Shrubs"="#FF99CC",
                      "Deciduous_trees"="#99FF99",
                      "Resinous_trees"="#339966",
                      "Cliffs"="#333333",
                      "Buildings"="#993300", 
                      "Unclassified_trees"="#CCFF33"),
                    na.value ="transparent")+
  geom_spatvector(data = borders_3V_vect,
                  fill = NA)+
  xlim(970000,973000)+
  ylim(6479500, 6481000)+
  ggtitle("From Clara")

g_geo <- ggplot() +
  geom_spatraster(data = terra::crop(soil_cover_geoservices_aligned, ext(970000,973000,6479500, 6481000))) +
  scale_fill_manual(
    name = "Habitat classes",
    values = c(
      "Soils_low_vegetation" = "#CCCCCC",
      "Shrubs_and_Low_ligneous" = "#FF99CC",
      "Deciduous_trees" = "#99FF99",
      "Resinous_trees" = "#339966",
      "Mixted_trees" = "#CCFF33",
      "Buildings" = "#993300"
    ),
    na.value = "transparent"
  )+
  geom_spatvector(data = borders_3V_vect,
                  fill = NA)+
  xlim(970000,973000)+
  ylim(6479500, 6481000)+
  ggtitle("From Geoservices")

habitats_r <- arrangeGrob(g_cl, g_geo, ncol = 1, nrow = 2)
plot(habitats_r)
#********************************************************************


# 3_Modify the raster winter habitats with more accurate tree categories ----
#********************************************************************
# Resinous_trees --> tree cat of Geoservices or let unchanged 
# Decidious_trees --> tree cat of Geoservices or let unchanged 
# Unclassified_trees --> tree cat of Geoservices or let unchanged 

# Define the tree categories
tree_categories_r_cl <- c("Deciduous_trees", "Resinous_trees", "Unclassified_trees")
tree_categories_r_geo <- c("Deciduous_trees", "Resinous_trees", "Mixed_trees")

# Create masks for tree categories in both rasters
tree_categories_r_cl <- carto_cl %in% tree_categories_r_cl
tree_categories_r_geo <- soil_cover_geoservices_aligned %in% tree_categories_r_geo

# Identify pixels where both rasters have tree categories
tree_overlap_mask <- tree_categories_r_cl & tree_categories_r_geo

# Replace tree categories in raster1 with those from raster2 where both are tree categories
carto_cl_modified <- ifel(tree_overlap_mask, soil_cover_geoservices_aligned, carto_cl)

# Verify the changes
par(mfrow=c(1,1))
terra::plot(carto_cl_modified, main = "Modified Raster with Tree Categories from Raster2")
# par(mfrow = c(1,2))
# plot(carto_cl=="Deciduous_trees")
# plot(carto_cl_modified==4)

               
# Rename the legend 
level_mapping <- data.frame(
  ID = 1:7,
  Label = c(
    "Soils_low_vegetation", # Class 1 ok
    "Deciduous_trees",      # Class 2 ok 
    "Resinous_trees",       # Class 3 ok
    "Mixed_trees",          # Class 4 ok
    "Shrubs",               # Class 5 ok
    "Cliffs",               # Class 6 ok
    "Buildings"             # Class 7 ok
  )
)

# Assign levels to the raster
levels(carto_cl_modified) <- list(level_mapping)


ggplot() +
  # geom_spatraster(data = carto_cl_modified)  +
  geom_spatraster(data = terra::crop(carto_cl_modified, ext(970000,973000,6479500, 6481000)))+
  scale_fill_manual(
    name = "Habitat classes",
    values = c(
      "Soils_low_vegetation" = "#CCCCCC",
      "Shrubs" = "#FF99CC",
      "Deciduous_trees" = "#99FF99",
      "Resinous_trees" = "#339966",
      "Mixed_trees" = "#CCFF33",
      "Buildings" = "#993300",
      "Cliffs" = "#333333"
    ),
    na.value = "transparent"
  )+
  geom_spatvector(data = borders_3V_vect,
                  fill = NA)+
  # xlim(970000,973000)+
  # ylim(6479500, 6481000)+
  ggtitle("New cartography winter habitats (tree categories modified)")


# Save the modified raster
writeRaster(carto_cl_modified, file = file.path(base, "TetrAlps","2_DATA","environmental_raster", "carto_habitats_3V_ocsge.tif"), overwrite = TRUE)
#********************************************************************


