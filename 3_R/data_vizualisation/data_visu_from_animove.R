#### Animove 2024 course ####

# Alice Bordes #

# June 2024 #

# Historic 

### Script by Kami Safi, Martina Scacco & Anne Scharf ###

# Description:
# Data visualization

library(vroom)
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
#devtools::install_github("16EAGLE/moveVis")
library(moveVis)

### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD/TetrAlps"
#********************************************************************


### Loading data ----
#********************************************************************
### DATASETS

# GPS locations of black grouses
data_bg_3V<-readRDS(file.path(base,"1_RAW_DATA/tot.ind.trois_vallees2_10_06_24.rds"))

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
#********************************************************************

#___________________________
# MAPPING MOVEMENT DATA ####
#___________________________
setwd(file.path(base,"3_R/heavy_saved_models"))

move_data_bg_3V_dt<-read.csv2("grouse_win.csv",sep=",")
move_data_bg_3V <- vroom::vroom("grouse_win.csv",delim=",") #pb for geometry and long and lat data
move_data_bg_3V<-move_data_bg_3V[,-1]
class(move_data_bg_3V)

### Basic plotting ####

#### basic plots colored by individual (with graphics)
plot(move_data_bg_3V["individual.local.identifier"], 
     max.plot=1, pch=16, cex=0.5, axes=T) #max.plot=1 : to have a single plot with all indiv of my data

#### basic plots colored by individual (with ggplot2)
ggplot() + theme_void() +
  geom_sf(data = move_data_bg_3V) +
  geom_sf(data = mt_track_lines(move_data_bg_3V), aes(color = `individual.local.identifier`)) # to transform data into lines


