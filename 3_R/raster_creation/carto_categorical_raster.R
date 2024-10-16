carto_habitats_3V <- terra::rast(file.path(base,"Tetralps/2_DATA/carto_habitats_clara_3V.tif")) #carto Clara

is.factor(carto_habitats_3V)

# Set the levels explicitly (from 1 to 16)
levels_hab <- levels(carto_habitats_3V)[[1]]
hab_categories = data.frame(ID=c(20,21,22,23,30,31,32,40,50,51,52,60,92,93,94,100), categories = c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified"))

levels(carto_habitats_3V)<-hab_categories

writeRaster(carto_habitats_3V, "carto_habitats_3V_test.tif", overwrite=TRUE)

carto_habitats_3V_test <- terra::rast(file.path(base,"Tetralps/3_R/carto_habitats_3V_test.tif"))
terra::plot(carto_habitats_3V)



