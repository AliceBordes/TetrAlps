# Loading libraries ---- 
#********************************************************************
library(tidyverse) # contains dplyr
library(sf)
library(ggplot2)
library(rnaturalearth) #needs rnaturalearthhires also installed
library(ggmap)
library(ggspatial) ## needs prettymapr also installed
library(units)
library(lubridate)
library(terra)
library(ctmm)
library(move2)
library(janitor)
library(openxlsx)
#********************************************************************


### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD"
outputfolder <- paste0(base,"/PhD/TetrAlps/3_R/heavy_saved_models/birds_3V/")
#********************************************************************


### Loadind functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
#********************************************************************



# Loading data ----
#********************************************************************
### DATASETS

# GPS locations of black grouses
data_bg_3V <- readRDS(file.path(base,"TetrAlps/1_RAW_DATA/tot.ind.trois_vallees2_10_06_24.rds"))

# Main characteristics of tagged-black grouse
synth_bg_all_sites <- read.csv2(file.path(base,"TetrAlps/1_RAW_DATA/bilan_captures_tetras_all_sites_mar2024_x_y_coord.csv"),sep=",")[,-1]


### VECTORS

# 3V borders 
borders_3V_vectlayer <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
#********************************************************************







#### 1_Add additional data
#********************************************************************

#### 1.1_Add and clean data on the capture site ----
#********************************************************************
# formatting synth_bg_all_sites to retrieve capture positions coordinates
synth_bg_all_sites$x_capture_coord <- as.numeric(synth_bg_all_sites$x_capture_coord)
synth_bg_all_sites$y_capture_coord <- as.numeric(synth_bg_all_sites$y_capture_coord)

synth_bg_all_sites$x_capture_coord2 <- ifelse(is.na(synth_bg_all_sites$x_capture_coord), 1,synth_bg_all_sites$x_capture_coord)
synth_bg_all_sites$y_capture_coord2 <- ifelse(is.na(synth_bg_all_sites$y_capture_coord), 1,synth_bg_all_sites$y_capture_coord)

synth_bg_all_sites <- st_as_sf(as.data.frame(synth_bg_all_sites), coords = c("x_capture_coord2", "y_capture_coord2"), crs = 4326)

# supress absurd GPS values
for(i in 1: nrow(synth_bg_all_sites))
{
  if(st_bbox(synth_bg_all_sites$geometry[i])$xmin >100)
  {
    synth_bg_all_sites$geometry[i] <- NA
  }
}      

# select all the points equal to POINT (1 1)
synth_bg_3V <- synth_bg_all_sites %>% filter(zone_etude=="trois_vallees")
colnames(synth_bg_3V["geometry"]) <- "geometry_WGS84"
synth_bg_3V$geometry_capture <- st_transform(synth_bg_3V$geometry,crs="EPSG:2154")
synth_bg_3V <- as.data.frame(synth_bg_3V)

data_bg_3V_synth <- dplyr::left_join(as.data.frame(data_bg_3V) ,synth_bg_3V %>% select(ani_nom,tag_id,espece,marque_tag,energy,geometry_capture), by=c("tag_id"="tag_id","ani_nom"="ani_nom"))

#### 1.2_Add data on visitor numbers ----
#********************************************************************
# Ski resort visitors
#********************************************************************






#### 2_Creation of the pretelemetry object (clean from erratic data of the first day) ----
#********************************************************************

#' Create a dt formatted for importation in movebank 
data_bg_pretelemetry <- pre_telemetry(data_bg_3V_synth,"WGS84")
data_bg_pretelemetry <- data_bg_pretelemetry%>% filter((animal.ID != "Eros") & (animal.ID !="Emilien")) # the pb for 2021-05-14 = Emilien and the pb for 2021-06-11 = Eros



# # Removing the first day of location data as it often shows some unrealistic movements
data_bg_pretelemetry <- data_bg_pretelemetry %>%
  group_by(animal.ID) %>%
  filter(as.Date(study.local.timestamp) > min(as.Date(study.local.timestamp)))


# Removing the bird with less than 100 GPS positions data after removing the first day of movements
# The estimation of the temporal autocorrelation between points, using plot(variogram(SVF_object),CTMM=ctmm_object) in ctmm, cannot be estimated with Escartefigue (53 locations) but with Dameur (83 locations it is ok)
# often the programmation is 1 loc/h between 4:00 am and 8:00 pm UTC --> so at least 15 loc/day * 7 days = 105 loc ~ at least 100 loc are necessary to be able to do estimations
data_bg_pretelemetry <- data_bg_pretelemetry %>%
  group_by(animal.ID) %>%
  filter(n() >= 100) %>%
  ungroup() 


write.csv(data_bg_pretelemetry[,!names(data_bg_pretelemetry) %in% c("geometry","geometry_capture")], row.names=FALSE, file="C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/data_bg_pretelemetry.csv")
#********************************************************************

