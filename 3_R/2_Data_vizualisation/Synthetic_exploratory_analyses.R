#### 1_Loading objects ----

### Loading libraries ---- 
#********************************************************************
library(move2)
library(sf)
library(ggplot2)
library(rnaturalearth) #needs rnaturalearthhires also installed
library(ggmap)
library(ggspatial) ## needs prettymapr also installed
library(gganimate)
library(plotly)
library(mapview)
library(units)
library(lubridate)
library(moveVis)
library(terra)
library(future.apply)
library(tidyterra)
library(ggnewscale)
library(gridExtra)
library(grid)
library(openxlsx)
library(janitor)
library(dplyr)
library(ctmm)
#********************************************************************


### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD"
#********************************************************************

# Loading data ----
#********************************************************************

### DATASET
birds_bg_dt<-read.csv2(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry_2024_10.csv"),sep=",") #upload the file from a csv, not a move2 object
# Main characteristics of tagged-black grouse
synth_bg_all_sites<-read.csv2(file.path(base,"Tetralps/1_RAW_DATA/bilan_captures_tetras_all_sites_feb2024.csv"),sep=",")[,-1]
synth_bg_all_sites <- synth_bg_all_sites %>% filter(zone_etude %in% c("foret_blanche", "le_fournel" , "les_arcs", "trois_vallees", "valcenis"))

### VECTORS

# 3V borders 
borders_3V_vect <- st_read(file.path(base,"Tetralps/1_RAW_DATA/3V/borders_3V.gpkg"))
borders_3V_vect <- terra::vect(file.path(base,"Tetralps/1_RAW_DATA/3V/borders_3V.gpkg"))

# Environment stack
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/env_RL_list.RData"))
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/scaled_env_RL_list.RData"))

#********************************************************************

#### 1_Data description ####

#### 1.1_3V tag type ####

#total
g_GPS_tag_tot<-
  ggplot(data=synth_bg_all_sites,aes(x=zone_etude, fill = "zone_etude"))+
  geom_bar()+
  scale_fill_manual(values = "lightblue")+
  labs(x="Study sites",y="Number of tags", title="GPS-tag characteristics")+
  geom_text(aes(label = ..count..),position=position_stack(vjust = 1), stat = "count", vjust = 1.5, colour = "black")+
  theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=12),strip.text.x = element_text(size = 14))

g_GPS_tag_tot
# ggsave(plot=g_GPS_tag_tot,file=paste0(base,"/6_FIGURES/data_exploration/","g_GPS_tags_total_",format(Sys.time(), "%d.%b%Y"),".png"))



### Basic plotting ####
ext_bird <- extent(l_telemetry_winter[["Abel"]][[1]][c("x","y")])

ggplot() + theme_void() +
  geom_spatraster(data = terra::rast(scaled_env_RL_list[["strava_winter_sports"]])) +
  scale_fill_viridis() +
  geom_point(data = l_telemetry_winter[["Abel"]][[1]], aes(x = x, y = y), color = "coral", size = 1) + # to transform data into lines
  xlim(ext_bird[1,1]-1000,ext_bird[2,1]+1000) +
  ylim(ext_bird[1,2]-1000,ext_bird[2,2]+1000)


