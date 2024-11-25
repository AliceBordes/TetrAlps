#### PhD Tetras_test project ####
#'''''''''''''''''''''''''''''''''''
# Alice Bordes #

# March 2024 #


# Historic #
#'''''''''''''''''''''''''''''''''''
# Made by Clara Leblanc
#'''''''''''''''''''''''''''''''''''


# Description:
#'''''''''''''''''''''''''''''''''''
### Import Open Street Map data of ski trails (downhill, nordic, skitour)
#### Run begin of script "vignettes/script_source.R" 
# to set paths to folder and study zone

# ----  OpenStreetMap data ----
# key and value OSM, see : 
# https://wiki.openstreetmap.org/wiki/Map_features#Primary_features
#'''''''''''''''''''''''''''''''''''



# ---- Loading packages  ----
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
#********************************************************************

# Loading data ----
#********************************************************************
# Environment stack
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/env_RL_list.RData"))
#********************************************************************

# ---- Settings ---- 
#********************************************************************
# extents of the environment
e <- ext(env_RL_list[["elevation"]])
sect <- as.polygons(e)
crs(sect) <- crs(env_RL_list[["elevation"]])
sect <- st_as_sf(sect)

# e<-c(min(e1[1],e2[1])-1000,max(e1[2],e2[2])+1000,min(e1[3],e2[3])-1000,max(e1[4],e2[4])+1000) 
# e_poly<-(as.polygons(ext(e), crs=crs(data_bg_3V)))
# timeout : max time to make a request
# attention timeout a changer pour grand secteur
tmo <- 300
# sect<-as_sf(e_poly)
output_crs<-4326
# output_crs<-2154
output_folder_zone<-file.path(here(),"2_DATA/osm")
#********************************************************************

# ---- download osm function ---- 

#' @param key character see https://wiki.openstreetmap.org/wiki/Map_features#Primary_features
#' @param value character or list of char. See key.
#' @param shape sf study zone
#' @param geometry_type among one sf objects c("POINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")
#' @param timeout time to run the request of osm data
#' 
#' @return sf 
dwld_osm <- function(key = "piste:type", 
                     value = "downhill", 
                     shape = sect, 
                     geometry_type = "LINESTRING", 
                     timeout = tmo) {
  
  # change crs
  if (sf::st_crs(shape) != 4326) {
    shape4326 <- sf::st_transform(shape, crs=4326)
  }
  
  # dowload osm data
  osm_ll <- osmdata::opq(bbox = sf::st_bbox(shape4326),
                         timeout = tmo) %>%
    osmdata::add_osm_feature(key = key, value = value) %>%
    osmdata::osmdata_sf()
  
  # get features of interest
  ll_geom_sf <- c("POINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")
  ll_geom_osm <- c("osm_points", "osm_lines", "osm_multilines", "osm_polygons", "osm_miltipolygons")
  
  osm_feature <- osm_ll[[ll_geom_osm[grep(geometry_type, ll_geom_sf)][1]]]  %>%
    sf::st_transform(output_crs) %>%
    sf::st_intersection(shape4326)
  
}

# --- download ski osm ----
# downhill winter trails
osm_ski_alp <- dwld_osm(key = "piste:type", 
                        value = "downhill", 
                        shape = sect, 
                        geometry_type = "LINESTRING", 
                        timeout = tmo)
mapview::mapView(osm_ski_alp[,1])

# nordic ski 
osm_ski_nor <- dwld_osm(key = "piste:type", 
                        value = "nordic", 
                        shape = sect, 
                        geometry_type = "LINESTRING", 
                        timeout = tmo)
mapview::mapView(osm_ski_nor[,1])

# ski touring
osm_ski_rando <- dwld_osm(key = "piste:type", 
                          value = "skitour", 
                          shape = sect, 
                          geometry_type = "LINESTRING", 
                          timeout = tmo)
mapview::mapView(osm_ski_rando[,1])

# snow hiking
osm_ski_hike <- dwld_osm(key = "piste:type", 
                         value = "hike", 
                         shape = sect, 
                         geometry_type = "LINESTRING", 
                         timeout = tmo)
mapview::mapView(osm_ski_hike[,1])

# connection
osm_ski_co <- dwld_osm(key = "piste:type", 
                         value = "connection", 
                         shape = sect, 
                         geometry_type = "LINESTRING", 
                         timeout = tmo)
mapview::mapView(osm_ski_co[,1])

# ---- compute & save ski osm data ----
l_piste <- c("name", "highway", "piste.difficulty", "piste.grooming", "piste.type")
piste_osm <- bind_rows(
  dplyr::select(osm_ski_alp, all_of(l_piste)),
  dplyr::select(osm_ski_nor, all_of(l_piste)),
  osm_ski_rando %>% select(any_of(l_piste)),
  dplyr::select(osm_ski_hike, all_of(l_piste)),
  osm_ski_co %>% dplyr::select(any_of(l_piste))
)

table(piste_osm$piste.type)

# write
sf::st_write(piste_osm, file.path(output_folder_zone, "osm_ski_piste.gpkg"),
             append=F)


# --- download ski lift osm ----

# aerial cables ski
osm_aerial_cables <- dwld_osm(key = "aerialway", 
                        value = "cable_car", 
                        shape = sect, 
                        geometry_type = "LINESTRING", 
                        timeout = tmo)
mapview::mapView(osm_aerial_cables[,1])

osm_aerial_gondola <- dwld_osm(key = "aerialway", 
                              value = "gondola", 
                              shape = sect, 
                              geometry_type = "LINESTRING", 
                              timeout = tmo)
mapview::mapView(osm_aerial_gondola[,1])

# osm_aerial_mixed_lift <- dwld_osm(key = "aerialway", 
#                                value = "mixed_lift", 
#                                shape = sect, 
#                                geometry_type = "LINESTRING", 
#                                timeout = tmo)
# mapview::mapView(osm_aerial_mixed_lift[,1])
#NULL

osm_aerial_chair_lift <- dwld_osm(key = "aerialway", 
                                  value = "chair_lift", 
                                  shape = sect, 
                                  geometry_type = "LINESTRING", 
                                  timeout = tmo)
mapview::mapView(osm_aerial_chair_lift[,1])

osm_aerial_drag_lift <- dwld_osm(key = "aerialway", 
                                  value = "drag_lift", 
                                  shape = sect, 
                                  geometry_type = "LINESTRING", 
                                  timeout = tmo)
mapview::mapView(osm_aerial_drag_lift[,1])

# osm_aerial_t_bar <- dwld_osm(key = "aerialway", 
#                                  value = "t-bar", 
#                                  shape = sect, 
#                                  geometry_type = "LINESTRING", 
#                                  timeout = tmo)
# mapview::mapView(osm_aerial_t_bar[,1])
#NULL

osm_aerial_j_bar <- dwld_osm(key = "aerialway", 
                             value = "j-bar", 
                             shape = sect, 
                             geometry_type = "LINESTRING", 
                             timeout = tmo)
mapview::mapView(osm_aerial_j_bar[,1])

osm_aerial_platter <- dwld_osm(key = "aerialway", 
                             value = "platter", 
                             shape = sect, 
                             geometry_type = "LINESTRING", 
                             timeout = tmo)
mapview::mapView(osm_aerial_platter[,1])

osm_aerial_rope_tow <- dwld_osm(key = "aerialway", 
                             value = "rope_tow", 
                             shape = sect, 
                             geometry_type = "LINESTRING", 
                             timeout = tmo)
mapview::mapView(osm_aerial_rope_tow[,1])

osm_aerial_magic_carpet <- dwld_osm(key = "aerialway", 
                                value = "magic_carpet", 
                                shape = sect, 
                                geometry_type = "LINESTRING", 
                                timeout = tmo)
mapview::mapView(osm_aerial_magic_carpet[,1])

osm_aerial_zip_line <- dwld_osm(key = "aerialway", 
                                    value = "zip_line", 
                                    shape = sect, 
                                    geometry_type = "LINESTRING", 
                                    timeout = tmo)
mapview::mapView(osm_aerial_zip_line[,1])

osm_aerial_goods <- dwld_osm(key = "aerialway", 
                                value = "goods", 
                                shape = sect, 
                                geometry_type = "LINESTRING", 
                                timeout = tmo)
mapview::mapView(osm_aerial_goods[,1])

# osm_aerial_pylon <- dwld_osm(key = "aerialway", 
#                              value = "pylon", 
#                              shape = sect, 
#                              geometry_type = "LINESTRING", 
#                              timeout = tmo)
# mapview::mapView(osm_aerial_pylon[,1])
#NULL

# osm_aerial_station <- dwld_osm(key = "aerialway", 
#                              value = "station", 
#                              shape = sect, 
#                              geometry_type = "LINESTRING", 
#                              timeout = tmo)
# mapview::mapView(osm_aerial_station[,1])
#NULL

# ---- compute & save ski lift data ----
l_ski_lift <- c("name", "aerialway.capacity", "aerialway.occupancy", "aerialway.length", "aerialway","opening_hours")
ski_lift_osm <- bind_rows(
  osm_aerial_cables %>% dplyr::select(any_of(l_ski_lift)),
  osm_aerial_gondola %>% dplyr::select(any_of(l_ski_lift)),
  osm_aerial_chair_lift %>% dplyr::select(any_of(l_ski_lift)),
  osm_aerial_drag_lift %>% dplyr::select(any_of(l_ski_lift)),
  osm_aerial_j_bar %>% dplyr::select(any_of(l_ski_lift)),
  osm_aerial_platter %>% dplyr::select(any_of(l_ski_lift)),
  osm_aerial_rope_tow %>% dplyr::select(any_of(l_ski_lift)),
  osm_aerial_magic_carpet %>% dplyr::select(any_of(l_ski_lift)),
  osm_aerial_zip_line %>% dplyr::select(any_of(l_ski_lift)),
  osm_aerial_goods %>% dplyr::select(any_of(l_ski_lift))
)

table(ski_lift_osm$aerialway)

# write
sf::st_write(ski_lift_osm, file.path(output_folder_zone, "ski_lift_osm.gpkg"),
             append=F)






