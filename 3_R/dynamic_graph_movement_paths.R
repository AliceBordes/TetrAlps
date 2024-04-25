#### PhD Tetras_test project ####

# Alice Bordes #

# November 2023 #

# Description:

# Visualization GPS-tagged Black grouse, Data from OFB
# RSF what happens next
#' Model the relative density of animals (also called range distribution or utilisation distribution) as a function of environmental predictors.



### Loading packages ----
#********************************************************************
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
library(terra)
library(tidyterra)
library(animove)
library(lubridate)
library(lidR)
library(tmap)
library(ggplot2)
library(sjmisc)
library(here)
library(ggspatial)
library(ggforce)
library(ggnewscale)
# devtools::install_github("16EAGLE/basemaps")
library(basemaps)
#devtools::install_github("16EAGLE/moveVis")
library(moveVis)
#********************************************************************

### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD/TetrAlps"
season="hiver"
# sex="male"
sex=c("male","femelle")

date1='2024-02-01'
graph_title= " day (0am-0pm) "   #if heure (UTC) : 9h-14h (UTC +1) = 8h-13h (UTC)
UTC1='00:00:00'
UTC2='24:00:00'
#********************************************************************




### Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
#********************************************************************



### Loading data ----
#********************************************************************

### DATASETS

# GPS locations of black grouses
data_bg_3V<-readRDS(file.path(base,"1_RAW_DATA/tot.ind.trois_vallees2.rds"))

# Main characteristics of tegged-black grouse
# synth_bg_all_sites<-read.csv2(file.path(base,"1_RAW_DATA/bilan_captures_tetras_all_sites_feb2024.csv"),sep=",")[,-1]
synth_bg_all_sites<-read.csv2(file.path(base,"1_RAW_DATA/bilan_captures_tetras_all_sites_mar2024_x_y_coord.csv"),sep=",")[,-1]

# Union of data_bg_3V and synth_bg_all_sites dataset
data_bg_3V_synth_fusion<- dplyr::left_join(data_bg_3V ,synth_bg_all_sites %>% filter(zone_etude=="trois_vallees") %>% select(tag_id,marque_tag,energy,sexe,age), by="tag_id")

# formatting synth_bg_all_sites to retrieve capture positions coordinates
synth_bg_all_sites$x_capture_coord<-as.numeric(synth_bg_all_sites$x_capture_coord)
synth_bg_all_sites$y_capture_coord<-as.numeric(synth_bg_all_sites$y_capture_coord)

synth_bg_all_sites$x_capture_coord2<-ifelse(is.na(synth_bg_all_sites$x_capture_coord), 1,synth_bg_all_sites$x_capture_coord)
synth_bg_all_sites$y_capture_coord2<-ifelse(is.na(synth_bg_all_sites$y_capture_coord), 1,synth_bg_all_sites$y_capture_coord)

synth_bg_all_sites <- st_as_sf(as.data.frame(synth_bg_all_sites), coords = c("x_capture_coord2", "y_capture_coord2"), crs = 4326)

# supress absurd GPS values
for(i in 1: nrow(synth_bg_all_sites))
{
  if(st_bbox(synth_bg_all_sites$geometry[i])$xmin >100)
  {
    synth_bg_all_sites$geometry[i]<-NA
  }
}      

# select all the points equal to POINT (1 1)
# st_bbox(points$geometry[1])==1
synth_bg_3V<-synth_bg_all_sites %>% filter(zone_etude=="trois_vallees")
synth_bg_3V<-st_as_sf(synth_bg_3V)
synth_bg_3V$geometry_lambert<-st_transform(synth_bg_3V$geometry,crs="+init=epsg:2154")
synth_bg_3V<-synth_bg_3V %>% mutate(x_capture_lambert = st_coordinates(geometry_lambert)[,1],y_capture_lambert = st_coordinates(geometry_lambert)[,2])

### RASTERS

# slope 3V
slope_3V<-terra::rast(file.path(base,"2_DATA/slope_3V_ign.tif"))
slope_3V_9<-terra::rast(file.path(base,"2_DATA/slope_3V_9_ign.tif"))

# Analyse à 9m
# carto_habitats_3V <- terra::rast(file.path(base,"1_RAW_DATA/landcover_T2L_1m_trois_vallees.tif")) #carto Clara
# carto_habitats_3V<-as.factor(carto_habitats_3V) #indicate discrete values for my map categories
#raster high vegetation
# carto_habitats_3V <- terra::crop(carto_habitats_3V,e)
# carto_habitats_3V_9<- terra::aggregate(carto_habitats_3V,9,fun="modal") # fact = 9 =  number of cells (pixels) in each direction (horizontally and vertically)
# fun = "modal" for a categorial raster = retains the majoritary class 
# disagg = to disaggregate
# writeRaster(x=carto_habitats_3V, filename=file.path(output_folder_zone,"carto_habitats_clara_3V.tif"),overwrite=TRUE)

carto_habitats_3V <- terra::rast(file.path(base,"2_DATA/carto_habitats_clara_3V.tif")) #carto Clara
carto_habitats_3V_9 <- terra::rast(file.path(base,"2_DATA/carto_habitats_3V_9_clara.tif")) #carto Clara

# strava
strava <- terra::rast(file.path(base, "2_DATA/strava/strava_3V_winter_sports_rgb.tif_single.tif"))
strava <- project(strava,y="+proj=longlat +datum=WGS84")
strava_lambert <- project(strava,y="+init=epsg:2154")

# mnt
mnt<-terra::rast(file.path(base, "2_DATA/mnt_ign.tif"))
mnt_9<-terra::rast(file.path(base, "2_DATA/mnt_9_mean_ign.tif"))


### VECTORS

# 3V borders 
borders_3V_vect <- terra::vect(paste0(base,"/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect_WGS84<- project(borders_3V_vect,y="+proj=longlat +datum=WGS84")

# 3V cables from Open street Map
cables_3V <- terra::vect(file.path(base,"2_DATA/ski_lift_osm.gpkg"))
cables_3V_WGS84<- project(cables_3V,y="+proj=longlat +datum=WGS84")

# 3V cables from OGM (Marc Montadert), NO NAMES 
cables_3V_no_id <- terra::vect(file.path(base,"1_RAW_DATA/human_traffic_in_ski_resorts/cables/cables.gpkg"))
cables_3V_no_id_WGS84<- project(cables_3V_no_id,y="+proj=longlat +datum=WGS84")

# lek sites 
lek_locations_vect <- terra::vect(file.path(base,"1_RAW_DATA/place_de_chant/places_de_chant.shp"))
lek_locations_vect <- project(lek_locations_vect,y="+proj=longlat +datum=WGS84")
lek_locations_vect_lambert <- project(lek_locations_vect,y="+init=epsg:2154")
# transform lek_locations_vect in spatial object with metadata
lek_sites<-as_sf(lek_locations_vect)
lek_sites_lambert<-as_sf(lek_locations_vect_lambert)
# to apply a buffer around the lek sites
lek_sites$larger_lek<-st_buffer(lek_sites$geometry, 100) # 100m
lek_sites_lambert$larger_lek<-st_buffer(lek_sites_lambert$geometry, 100) # 100m
#********************************************************************


### Shape the study area ----
#********************************************************************
#e <- extent(971000,985000,6471000,6486000)
e1<-ext(borders_3V_vect)
e2<-ext(as.polygons(ext(data_bg_3V), crs=crs(data_bg_3V)))

e<-c(min(e1[1],e2[1])-1000,max(e1[2],e2[2])+1000,min(e1[3],e2[3])-1000,max(e1[4],e2[4])+1000) 
e_poly<-(as.polygons(ext(e), crs=crs(data_bg_3V)))
# ext(e_poly)

# change the coordinate system of the SpatVector from (9..,9..,6..,6..) to (6..,6..,45..,45..)
# borders_3V_vect_lat_long<-project(borders_3V_vect, y="+proj=longlat +datum=WGS84")
# e<-ext(borders_3V_vect_lat_long)
#********************************************************************


### Loading heavy saved models ----
#********************************************************************
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/best_model_saved_hiver.RData")
assign("best_model_saved_hiver",best_model)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_akde_saved_hiver.RData")
assign("grouse_winter_akde_saved_hiver",grouse_winter_akde)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_telemetry_hiver.RData")
assign("grouse_winter_telemetry_hiver",grouse_winter_telemetry)
#********************************************************************

#### 1_Loading birds locations and formatting telemetry data ----
#********************************************************************

#focus on bird locations in winter season
grouse_winter_raw<-as.data.frame(data_bg_3V_synth_fusion%>%filter(saison==season)%>%filter(sexe %in% sex))

#create a list of data.frames for each animal
grouse_winter<-multiple_dt_indiv(grouse_winter_raw,"nom")


#' Create a dt formatted like a telemetry object
grouse_winter_pretelemetry<- lapply(grouse_winter,pre_telemetry)


# # Removing the first day of location data as it often shows some unrealistic movements
for(i in 1:length(grouse_winter_pretelemetry))
{
  grouse_winter_pretelemetry[[i]] <- grouse_winter_pretelemetry[[i]] %>% filter (study.local.timestamp >= (first(grouse_winter_pretelemetry[[i]]["study.local.timestamp"]) + ddays(1) ))
}

# Removing the bird with less than 50 GPS positions data after removing the first day of movements
grouse_winter_pretelemetry<- grouse_winter_pretelemetry[sapply(grouse_winter_pretelemetry, function(x) dim(x)[1]) > 50]


grouse_winter_telemetry<-grouse_winter_pretelemetry
for(i in 1:length(grouse_winter_telemetry))
{
  grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_telemetry[[i]],datum="WGS84",keep=c("individual.local.identifier"))
  # grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_telemetry[[i]],datum="WGS84")
  grouse_winter_telemetry[[i]]["x"] <- grouse_winter_pretelemetry[[i]]["X_GPS_lambert93"]
  grouse_winter_telemetry[[i]]["y"] <- grouse_winter_pretelemetry[[i]]["Y_GPS_lambert93"]
}


# create a list with bird's names to plot the correct legend
vect_nicknames<-list()
for(i in (1:length(grouse_winter_pretelemetry)))
{
  vect_nicknames[[i]]<-unique(grouse_winter_pretelemetry[[i]]["individual.local.identifier"])
}
vect_nicknames<-unlist(vect_nicknames)
vect_nicknames<-as.vector(vect_nicknames)

# renamed each data frame from the list of data frames by the name of each bird 
names(grouse_winter_pretelemetry)<-vect_nicknames
names(grouse_winter_telemetry)<-vect_nicknames

grouse_winter_pretelemetry_all<-pre_telemetry(data_bg_3V)
colnames(grouse_winter_pretelemetry_all)[colnames(grouse_winter_pretelemetry_all)=="individual.local.identifier"]<-"nom"
grouse_winter_telemetry_all<-as.telemetry(as.data.frame(grouse_winter_pretelemetry_all[,-c(1,3)]),datum="WGS84",keep=c("nom"))
#********************************************************************




#### 1_Dynamic map for movements vizualisation ----

#focus on bird locations in winter season ; if heure (UTC) : 9h-14h (UTC +1) = 8h-13h (UTC)
move_all_hiv2024_3V<-as.data.frame(grouse_winter_telemetry_all[1:5032,])

move_all_hiv2024_3V<-df2move(move_all_hiv2024_3V, proj="+init=epsg:4326 ",x="longitude",y="latitude",time = "timestamp",track_id = "nom") # conversion in a move class object

# align move_data to a uniform time scale
temp = 1
unit = "days"
m <- align_move(move_all_hiv2024_3V, res = temp, unit = unit)  #only work in WGS84 with longitude and latitude coordinates

# create spatial frames with a OpenStreetMap watercolour map

# change coordinates system to plot the habitat map behind the animation
# project(carto_habitats_3V,crs(move_loteck_hiv2024_3V))


frames <- frames_spatial(m, path_colours = c("red","blue"),
                         alpha = 0.5, trace_show = TRUE,margin_factor = 1.2) %>% 
  add_labels(x = "Longitude", y = "Latitude", 
             title = paste0("February 2024 movements of Abel black grouse during",graph_title,"\nStudy site = Trois Vallées ski resort", 
                            paste(subtitle = "\nTemporal resolution =",temp,unit))) %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(type = "label") %>%
  add_progress()

frames[[34]] # preview one of the frames, e.g. the 100th frame


# animate frames
animate_frames(frames, out_file = "moveVis_Abel.gif", width = 1000, height = 1000,overwrite = T)
