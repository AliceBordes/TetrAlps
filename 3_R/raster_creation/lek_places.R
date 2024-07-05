# Loading libraries ---- 
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
library(gganimate)
library(units)
library(lubridate)
library(moveVis)
library(terra)
library(future.apply)
#********************************************************************


### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD"
#********************************************************************

# Loading data ----
#********************************************************************

### DATASET
birds_bg_dt <- read.csv2(file.path(base,"Tetralps/2_DATA/pretelemetry_bg_3V_WGS84.csv"),sep=",") #upload the file from a csv, not a move2 object


### VECTORS

# 3V borders 
borders_3V_vectlayer <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect <- terra::vect(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
# borders_3V_vect_WGS84<- project(borders_3V_vect,y="+proj=longlat +datum=WGS84")

# mnt
mnt <- terra::rast(file.path(base, "Tetralps/2_DATA/mnt_ign.tif"))
mnt_9 <- terra::rast(file.path(base, "Tetralps/2_DATA/mnt_9_mean_ign.tif"))
# mnt_9_WGS84<- project(mnt_9,y="+proj=longlat +datum=WGS84")

# slope 3V
slope_3V <- terra::rast(file.path(base,"Tetralps/2_DATA/slope_3V_ign.tif"))
crs(slope_3V) <- "+init=epsg:2154"
slope_3V_9 <- terra::rast(file.path(base,"Tetralps/2_DATA/slope_3V_9_ign.tif"))
crs(slope_3V_9) <- "+init=epsg:2154"
# slope_3V_9_WGS84 <- project(slope_3V_9,y="+proj=longlat +datum=WGS84")

# habitat cartography
carto_habitats_3V <- terra::rast(file.path(base,"Tetralps/2_DATA/carto_habitats_clara_3V.tif")) #carto Clara
carto_habitats_3V_9 <- terra::rast(file.path(base,"Tetralps/2_DATA/carto_habitats_3V_9_clara.tif")) #carto Clara
# carto_habitats_3V_9_WGS84 <- project(carto_habitats_3V_9,y="+proj=longlat +datum=WGS84")

# strava
strava <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_3V_winter_sports_rgb.tif_single.tif"))
strava <- project(strava,y="+init=epsg:2154")
# strava_WGS84 <- project(strava,y="+proj=longlat +datum=WGS84")

# extents of the study area
e <- c(963981.7, 1002351.7 ,6464374.9 ,6495464.1)

#********************************************************************



### Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/mean_size_area.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/visu_home_range.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/distance_home_range_capture_site.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/SVF_indiv.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multi_graph_home_range.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/plot_check_RSF_results.R")
#********************************************************************


### Loading data ----
#********************************************************************
load(paste0(base,"/Animove2024/my_project/heavy_models/birds_sample_bg_telemetry_seasons.RData"))
load(paste0(base,"/Animove2024/my_project/heavy_models/birds_sample_bg_ctmm_fit_seasons.RData"))
load(paste0(base,"/Animove2024/my_project/heavy_models/birds_sample_bg_akde_seasons.RData"))
#********************************************************************


#'Loading packages
#********************************************************************
#+  results='hide', message=FALSE, warning=FALSE
library(animove)
library(ctmm)
library(sf)
library(mvtnorm)
library(terra)
library(raster)
library(tidyterra)
#********************************************************************

setwd(base)

### We read in this dataset downloaded from Movebank directly as a move2 object
bg <- vroom::vroom("Tetralps/2_DATA/pretelemetry_bg_3V_WGS84.csv") # with vroom package, timestamps get recognized, are assumed to be in UTC

# now create a move2 object from a data.frame
bg_move2 <- mt_as_move2(bg, 
                        coords = c("location.long","location.lat"),
                        crs = "EPSG:4326",
                        time_column = "timestamp",
                        track_id_column = "animal.ID",  # name of the column which will be define your tracks
                        na.fail = F) # allows or not empty coordinates


# singing period : between 3:00 am UTC (5:00 am French time) and 7:00 am UTC (9:00 am French time)  
birds_morning_bg <- birds_bg_dt %>% filter(heure>="03:00:00" & heure<="07:00:00")
birds_morning_bg$timestamp <- as.POSIXct(birds_sample_bg$timestamp)

birds_morning_bg_move2 <- mt_as_move2(birds_morning_bg, 
                          coords = c("location.long","location.lat"),
                          crs = "EPSG:4326",
                          time_column = "timestamp",
                          track_id_column = "animal.ID",  # name of the column which will be define your tracks
                          na.fail = F) # allows or not empty coordinates


# change the coordinate system into the projection of my raster layers
# projection(birds_sample_bg_telemetry) <- crs(mnt) 


ggplot()+
  geom_spatraster(data=mnt)+
  scale_fill_gradientn(name = "Altitude (m)", colors = c("#CCFFCC", "#FFFFCC", "#FFCC99", "#FF9966", "#FF6600"))+
  geom_sf(data=borders_3V_vect,fill="transparent")+
  geom_sf(data=birds_morning_bg_move2,aes(color = `animal.ID`))+
  theme(legend.position="none") +
  ggtitle("Black grouse GPS-tagged in the 3 Vallées area (Nothern Alps)\nfrom 2017 to 2024\nGPS points between 3:00 am and 7:00 am UTC")

