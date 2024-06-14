#### PhD Tetras_test project ####

# Alice Bordes #

# June 2023 #

# Description:

# Creation telemetry object


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
library(ggbreak)
library(raster)
library(amt)
library(sp)
library(gridExtra)
library(ggnewscale)
#********************************************************************

### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD/TetrAlps"
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

# Union of data_bg_3V and synth_bg_all_sites dataset ATTENTION : join by the couple tag_id, ani_name otherwise, wrong match!
data_bg_3V_synth_fusion<- dplyr::left_join(data_bg_3V ,synth_bg_all_sites %>% filter(zone_etude=="trois_vallees") %>% select(ani_nom,tag_id,marque_tag,energy,sexe,age), by=c("tag_id"="tag_id","ani_nom"="ani_nom"))

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


### Shape the study area ----
#********************************************************************
#e <- extent(971000,985000,6471000,6486000)
e1<-ext(borders_3V_vect)
e2<-ext(as.polygons(ext(data_bg_3V), crs=crs(data_bg_3V)))

e<-c(min(e1[1],e2[1])-1000,max(e1[2],e2[2])+1000,min(e1[3],e2[3])-1000,max(e1[4],e2[4])+1000) 
e_poly<-(as.polygons(ext(e), crs=crs(data_bg_3V)))
# ext(e_poly)

# change the coordinate system of the SpatVector from (9..,9..,6..,6..) to (6..,6..,45..,45..)
# e_poly_WGS84<-project(e_poly, y="+proj=longlat +datum=WGS84")
# e_WGS84<-ext(e_poly_WGS84)
# e<- as.numeric(as.vector(e_WGS84))
#********************************************************************



#### 1_Creation of telemetry with birds locations and akde objects for home range estimation ####
#### 1.1_Creation of the telemetry object with birds locations ----
#********************************************************************



formatting_telemetry<-function(data,file_path="C:/Users/albordes/Downloads/telemetry",projection="Lambert93")
{
  
  
#focus on bird locations in winter season
# grouse_winter_raw<-as.data.frame(data_bg_3V_synth_fusion%>%filter(saison==season)%>%filter(sexe %in% sex))
grouse_winter_raw<-as.data.frame(data)

#create a list of data.frames for each animal
grouse_winter<-multiple_dt_indiv(grouse_winter_raw,"nom")


#' Create a dt formatted like a telemetry object
grouse_winter_pretelemetry<- lapply(grouse_winter,pre_telemetry,projection)


# # Removing the first day of location data as it often shows some unrealistic movements
for(i in 1:length(grouse_winter_pretelemetry))
{
  grouse_winter_pretelemetry[[i]] <- grouse_winter_pretelemetry[[i]] %>% filter (study.local.timestamp >= (first(grouse_winter_pretelemetry[[i]]["study.local.timestamp"]) + ddays(1) ))
  
}

# Removing the bird with less than 50 GPS positions data after removing the first day of movements
# The estimation of the temporal autocorrelation between points, using plot(variogram(SVF_object),CTMM=ctmm_object) in ctmm, cannot be estimated with Escartefigue (53 locations) but with Dameur (83 locations it is ok)
# often the programmation is 1 loc/h between 4:00 am and 8:00 pm UTC --> so at least 15 loc/day * 7 days = 105 loc ~ at least 100 loc are necessary to be able to do estimations
grouse_winter_pretelemetryetry<- grouse_winter_pretelemetry[sapply(grouse_winter_pretelemetry, function(x) dim(x)[1]) > 100]




for(i in 1:length(grouse_winter_pretelemetry))
{
  # keep only necessary variables
  grouse_winter_pretelemetry[[i]] <- grouse_winter_pretelemetry[[i]] %>% 
    select(individual.local.identifier,
           timestamp,
           location.long,
           location.lat,
           WGS84,
           sensor.type,marque_tag,energy,
           sexe,age,
           saison, saison2,
           period_jour, height.above.mean.sea.level
    )
}


# create a list with bird's names to plot the correct legend
vect_nicknames<-list()
for(i in (1:length(grouse_winter_pretelemetry)))
{
  vect_nicknames[[i]]<-unique(grouse_winter_pretelemetry[[i]]["individual.local.identifier"])
}
vect_nicknames<-unlist(vect_nicknames)
vect_nicknames<-as.vector(vect_nicknames)



# Create an empty list to store the results
bird_season_pretelemetry <- list()

# Loop through each bird's telemetry data
for (i in seq_along(grouse_winter_pretelemetry)) {
  # Extract the unique seasons for the current bird
  seasons <- unique(grouse_winter_pretelemetry[[i]]$saison2)
  
  # Create a nested list for each bird to store the seasonal data
  bird_season_pretelemetry[[i]] <- list()
  
  # Loop through each season
  for (s in seq_along(seasons)) {
    # Filter the data for the current season
    bird_season_pretelemetry[[i]][[s]] <- grouse_winter_pretelemetry[[i]] %>% filter(saison2 == seasons[s])
  }
  
  # Optionally, assign names to the inner list
  names(bird_season_pretelemetry[[i]]) <- seasons
}

# Optionally, assign names to the outer list
names(bird_season_pretelemetry) <- vect_nicknames




grouse_winter_telemetry<-list()
for(i in 1:length(bird_season_pretelemetry))
{ 
  # Extract the unique seasons for the current bird
  seasons <- unique(names(bird_season_pretelemetry[[i]]))
  print(seasons)
  
  # Create a nested list for each bird to store the seasonal data
  grouse_winter_telemetry[[i]]<-list()
  
  for(s in seq_along(seasons))
  {
    grouse_winter_telemetry[[i]][[s]]<- as.telemetry(bird_season_pretelemetry[[i]][[s]],datum="WGS84",keep=c("saison","saison2","period_jour","sexe","age"))
    # grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_pretelemetry[[i]],datum="WGS84")
    grouse_winter_telemetry[[i]][[s]]["x"] <- grouse_winter_telemetry[[i]][[s]]["longitude"]
    grouse_winter_telemetry[[i]][[s]]["y"] <- grouse_winter_telemetry[[i]][[s]]["latitude"]
  }
  # Optionally, assign names to the inner list
  names(grouse_winter_telemetry[[i]]) <- seasons
}

# Optionally, assign names to the outer list
names(grouse_winter_telemetry) <- vect_nicknames


View(grouse_winter_telemetry)

save(grouse_winter_telemetry, file=paste0(file_path,".RData"))

return()

}


