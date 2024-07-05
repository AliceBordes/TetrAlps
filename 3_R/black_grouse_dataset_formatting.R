#### PhD Tetras_test project ####

# Alice Bordes #

# June 2023 #

# Description:

# Formatting Black grouse dataset

### Loading packages ----
#********************************************************************
library(tidyverse)
#********************************************************************

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
colnames(synth_bg_3V["geometry"])<-"geometry_WGS84"
synth_bg_3V$geometry_capture<-st_transform(synth_bg_3V$geometry,crs="+init=epsg:2154")
synth_bg_3V<-as.data.frame(synth_bg_3V)

data_bg_3V_synth<-dplyr::left_join(as.data.frame(data_bg_3V) ,synth_bg_3V %>% select(ani_nom,tag_id,espece,marque_tag,energy,geometry_capture), by=c("tag_id"="tag_id","ani_nom"="ani_nom"))

# data_bg_3V_synth<-data_bg_3V_synth %>% mutate(x_geometry_capture = st_coordinates(geometry_capture)[,1],y_geometry_capture = st_coordinates(geometry_capture)[,2])
# data_bg_3V_synth <- st_as_sf(data_bg_3V_synth[ , !names(data_bg_3V_synth)== "geometry_capture"])

saveRDS(data_bg_3V_synth,file=file.path(base,"2_DATA","black_grouse_dataset.rds"))
