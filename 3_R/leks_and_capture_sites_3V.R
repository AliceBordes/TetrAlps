#### PhD Tetras_test project ####

# Alice Bordes #

# March 2023 #

# Description:

# Visualization GPS-tagged Black grouse, Data from OFB
# RSF what happens next
#' Model the relative density of animals (also called range distribution or utilisation distribution) as a function of environmental predictors.



### Loading packages 
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
#********************************************************************


### Loading functions
#********************************************************************
#********************************************************************


### Loading data
#********************************************************************
base<-here()

### DATASETS

# GPS locations of black grouses
data_bg_3V<-readRDS(paste0(base,"/1_RAW_DATA/tot.ind.trois_vallees2.rds"))

# Main characteristics of tagged-black grouse
# synth_bg_all_sites<-read.csv2(paste0(base,"/1_RAW_DATA/bilan_captures_tetras_all_sites_feb2024.csv"),sep=",")[,-1]
synth_bg_all_sites<-read.csv2(paste0(base,"/1_RAW_DATA/bilan_captures_tetras_all_sites_mar2024_x_y_coord.csv"),sep=",")[,-1]

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

### VECTORS

# 3V borders 
borders_3V_vect <- terra::vect(paste0(base,"/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect_crs_SI <- project(borders_3V_vect,y="+proj=longlat +datum=WGS84")

# lek sites 
lek_locations_vect <- terra::vect(paste0(base,"/1_RAW_DATA/place_de_chant/places_de_chant.shp"))
lek_locations_vect <- project(lek_locations_vect,y="+proj=longlat +datum=WGS84")
#********************************************************************



### Settings
#********************************************************************
## Shape the study area
#e <- extent(971000,985000,6471000,6486000)
e1<-ext(borders_3V_vect)
e2<-ext(as.polygons(ext(data_bg_3V), crs=crs(data_bg_3V)))

e<-c(min(e1[1],e2[1])-1000,max(e1[2],e2[2])+1000,min(e1[3],e2[3])-1000,max(e1[4],e2[4])+1000) 
e_poly<-(as.polygons(ext(e), crs=crs(data_bg_3V)))
ext(e_poly)
e_WGS84<-project(e_poly,y="+proj=longlat +datum=WGS84")
# change the coordinate system of the SpatVector from (9..,9..,6..,6..) to (6..,6..,45..,45..)
# borders_3V_vect_lat_long<-project(borders_3V_vect, y="+proj=longlat +datum=WGS84")
# e<-ext(borders_3V_vect_lat_long)
#********************************************************************

# transform lek_locations_vect in spatial object with metadata
lek_sites<-as_sf(lek_locations_vect)


# When GPS positions of the capture site are missing --> 
# affect the position of the centroid of the singing place associated with the name of the capture site

lek_sites_targetted<-list()
for(i in 1:nrow(synth_bg_3V))
  {
    if((is.na(synth_bg_3V$x_capture_coord[i]) | is.na(synth_bg_3V$y_capture_coord[i]))==TRUE)
    {
      if(str_contains(lek_sites$plc_chant,synth_bg_3V$site_capture[i])==TRUE)

        print("+1 GPS position modified")
        lek_sites_targetted[[i]]<-lek_sites %>% filter(str_detect(lek_sites$plc_chant,synth_bg_3V$site_capture[i]))
      
      # retrieve the centroid of each singing place (several for Arpasson : north, sud, west, est)
        a<-(st_centroid(lek_sites_targetted[[i]]))$geometry
      # create a polygon between all the centroid of each singing place (several for Arpasson : north, sud, west, est)
        b<-st_union(a)
      # to retrieve the centroid point of this polygon
        c<-st_centroid(b)
        
      # save the centroid point everywhere where geometry is missing
        synth_bg_3V$geometry[i]<-c
      
    }
  }



# to apply a buffer around the lek sites
lek_sites$larger_lek<-st_buffer(lek_sites$geometry, 100) # 100m


# intersect of birds capture positions with the singing site
# overlapp points and polygone 
lek_sites$intersect_100m_buffer<-st_intersects(lek_sites$larger_lek,synth_bg_3V %>% drop_na(x_capture_coord,y_capture_coord) %>% select(geometry)) # buffer at 100m, consider only positions exact (not the ones reconstituted)
lek_sites$intersect_strict<-st_intersects(lek_sites$geometry,synth_bg_3V %>% drop_na(x_capture_coord,y_capture_coord) %>% select(geometry)) # buffer at 1m, consider only positions exact (not the ones reconstituted)
lek_sites$nb_pts_100m_buffer<-lapply(lek_sites$intersect_100m_buffer,length)
lek_sites$nb_pts_strict<-lapply(lek_sites$intersect_strict,length)


# To display the proportion capture position inside the singing place
proportion_points_inside<-data.frame("Strict area"=(sum(unlist(lek_sites$nb_pts_strict))/nrow(synth_bg_3V))*100,
                                     "Buffer=100m"=(sum(unlist(lek_sites$nb_pts_100m_buffer))/nrow(synth_bg_3V))*100,
                                     row.names = "Birds captured inside a lek site (%)")
proportion_points_inside



# Visualizing the lek sites and the GPS positions of bird captures 
ggplot()+
  geom_sf(data = lek_sites, aes(geometry = geometry,fill=plc_chant,colour=plc_chant))+
  geom_sf(data = borders_3V_vect_crs_SI,fill=NA,color="black",lwd =2)+
  # geom_sf(data = synth_bg_3V %>% filter(!(is.na(x_capture_coord) | is.na(y_capture_coord))), aes(geometry = geometry, color = site_capture), size = 1)+
  coord_sf(xlim=c(st_bbox(lek_sites)$xmin,st_bbox(lek_sites)$xmax),ylim=c(st_bbox(lek_sites)$ymin,st_bbox(lek_sites)$ymax))+
  scale_fill_discrete(name = "Lek sites")+
  guides(colour="none")+
  ggtitle("Lek sites in the Trois Vallées skiing area\nand GPS positions of bird captures")+
  xlab("Longitude")+
  ylab("Latitude")


# Visualizing the lek sites
ggplot()+
  geom_sf(data = lek_sites, aes(geometry = geometry),colour="black")+
  geom_sf(data = lek_sites, aes(geometry = larger_lek),colour="red",fill=NA)+
  geom_sf(data = borders_3V_vect_crs_SI,fill=NA,color="black",lwd =2)+
  geom_sf(data = synth_bg_3V %>% filter(!(is.na(x_capture_coord) | is.na(y_capture_coord))), aes(geometry = geometry, color = site_capture), size = 1)+
  coord_sf(xlim=c(st_bbox(lek_sites)$xmin,st_bbox(lek_sites)$xmax),ylim=c(st_bbox(lek_sites)$ymin,st_bbox(lek_sites)$ymax))+
  scale_fill_discrete(name = "Lek sites")+
  guides(colour="none")+
  ggtitle("Lek sites in the Trois Vallées skiing area\nand GPS positions of bird captures")+
  xlab("Longitude")+
  ylab("Latitude")



