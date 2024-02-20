######################################################################

#### PhD Tetras_test project ####

# Alice Bordes #

# November 2023 #

##############
# Description:

# Tools for spatial analyses
# Application for the 3V site

######################################################################

###'Loading packages

library(tidyverse)
library(remotes)
library(devtools)
#remotes::install_github("AniMoveCourse/animove_R_package")
#install.packages("maptools", repos="http://R-Forge.R-project.org")
#install.packages("rgeos", repos="http://R-Forge.R-project.org")
library(maptools)
#devtools::install_github('oswaldosantos/ggsn')
library(rgeos)
library(ggsn)
library(usethis)
library(animove)
library(ctmm)
library(sf)
library(mgcv)
library(mvtnorm)
library(lubridate)
library(tidyr)
library(dplyr)
library(raster)
library(animove)
library(lubridate)
library(lidR)
library(tmap)
library(ggplot2)
library(cowplot)
library(gridExtra)

#charging functions
#--------------------------------------
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
#--------------------------------------

#charging data
#--------------------------------------
rawdata.path<-"C:/Users/albordes/Documents/PhD/TetrAlps/1_RAW_DATA/"


# GPS locations of black grouses
data.bg.3V<-readRDS(paste0(rawdata.path,"tot.ind.trois_vallees.rds"))

# Polygones by habitat type - 3 vallees
polygones_field_3V <- sf::st_read(paste0(rawdata.path,"Numerisation_habitat_terrain_11-10-23_modif.gpkg"))

# study sector
sect_3V <- sf::st_read(paste0(rawdata.path,"secteur_etude_3V.gpkg"))

# lidar data
lidar_map <- paste0(rawdata.path, "Test11_carto_classification2_supra_10m_tot.tif") 

# slope 3V
raster3V.slope.brute<-paste0(rawdata.path,"raster.3V.slope.tif")

# carte d'occupation des sols OSO (produite par le Centre d'Expertise Scientifique sur l'occupation des sols (CES OSO))
oso <- terra::rast("M:/CESBIO/OSO_20220101_RASTER_V1-0/DATA/OCS_2022.tif") 

# Analyse à 9m
r_analyse_9 <- terra::rast(paste0(rawdata.path, "Test11_carto_classification2_supra_10m_tot.tif"))

#--------------------------------------
map <-
  # Polygones d'habitat
  tmap::tm_shape(polygones_field_3V, name="Polygones habitat")+
  tmap::tm_polygons(border.col = "red", lwd = 1, alpha = 0)+
  #
  # Catalogue lidar
  tmap::tm_polygons(border.col = "beige", lwd = 1, alpha = 0)+
  
  # Intensite
  
  
  
  # Fond de carte
  tmap::tm_basemap(leaflet::providers$GeoportailFrance.orthos)
#
map



## Shape de la zone d'etude restreinte
e <- extent(975000,981000,6480000,6487000)
sect <- sf::st_bbox(e) %>% sf::st_as_sfc()


## Construction de la grille d'analyse de l'habitat a partir d'OSO
oso<- terra::crop(oso,sf::st_buffer(sect, 20))
plot(oso)

## Grille vide d'analyse
template_1m <- terra::rast(ext=e, resolution=1) # resolution 1m 
template_1m <- terra::rast(ext=(oso), resolution=1) # ici e defini pour correspondre aux memes limites que le secteur 20 sur la map oso : e = ext(oso)
#rast (from terra package) create a SpatRaster = a spatially referenced surface divided into 3 dimensional cells (rows, columns, and layers)
crs(template_1m) <- crs(oso) #crs() retrieve coordinate reference system from an object

## visualysing field polygones
polygones_seuls_100 <- sf::st_union(sf::st_geometry(sf::st_buffer(polygones_field_3V, 100))) # st_buffer() function encircles a geometry object at a specified distance (here 50) and returns a geometry object
polygones_seuls_10 <- sf::st_union(sf::st_geometry(sf::st_buffer(polygones_field_3V, 10)))
par(mfrow=c(1,2))
plot(polygones_seuls_100)
plot(polygones_seuls_10)

tmap::tmap_mode("view") #carte interactive
map <-
  # Polygones d'habitat
  tmap::tm_shape(polygones_field_3V, name="Polygones habitat")+
  tmap::tm_polygons(border.col = "red", lwd = 1, alpha = 0)+
  # Fond de carte
  tmap::tm_basemap(leaflet::providers$GeoportailFrance.orthos)

map


par(mfrow=c(1,1))
plot(polygones_field_3V[,colnames(polygones_field_3V)=="Type_hab"])

polygones_field_zoom_3V_1m <- terra::rasterize(terra::vect(polygones_field_3V), template_1m, "Type_hab") #resolution 1m + recentre sur le secteur d'interet
plot(polygones_field_zoom_3V_1m) # zoom on the interest study area
