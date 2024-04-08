#### PhD Tetras_test project ####
#'''''''''''''''''''''''''''''''''''
# Alice Bordes #

# April 2024 #


# Description:
#'''''''''''''''''''''''''''''''''''
### Visualizing ski cables
### Association ski cables and human traffic
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
library(readxl)
library(stringi)
#********************************************************************


# ---- Settings ---- 
#********************************************************************
base<-here()
output_folder_zone<-file.path(here(),"2_DATA")
#********************************************************************


# load(file.path(here(),'myRasterEnvironment.RData'))

# ---- Loading data ---- 
#********************************************************************
### RASTER

# strava
strava <- terra::rast(paste0(base, "/2_DATA/strava/strava_3V_winter_sports_rgb.tif_single.tif"))
strava <- project(strava,y="+proj=longlat +datum=WGS84")


### VECTORS

# 3V borders 
borders_3V_vect <- terra::vect(paste0(base,"/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect_WGS84<- project(borders_3V_vect,y="+proj=longlat +datum=WGS84")

# 3V winter trails (osm)
osm_winter<-vect(file.path(output_folder_zone, "osm_ski_piste.gpkg"))
osm_winter_sf<-as_sf(osm_winter)

# Loading 3V ski lift traffic (ski lift traffic in 2022-2023 associated with ski lift geometry from Open Street Map)
cables_3V_traffic <- terra::vect(file.path(base,"2_DATA/ski_lift_traffic_3V.gpkg"))



### DATASETS

# GPS locations of black grouses
data_bg_3V<-readRDS(paste0(base,"/1_RAW_DATA/tot.ind.trois_vallees2.rds"))


#******************************************************************** 





# ---- Shaping the study area ---- 
#********************************************************************
e1<-ext(borders_3V_vect)
e2<-ext(as.polygons(ext(data_bg_3V), crs=crs(data_bg_3V)))

e<-c(min(e1[1],e2[1])-1000,max(e1[2],e2[2])+1000,min(e1[3],e2[3])-1000,max(e1[4],e2[4])+1000) 
e_poly<-(as.polygons(ext(e), crs=crs(data_bg_3V)))
sect<-as_sf(e_poly)
#********************************************************************

try <- extract(strava,cables_3V_traffic,fun="mean",na.rm=T) #and median ?
data_ID_try<-data.frame("ID"=seq_len(nrow(cables_3V_traffic)),"cable"=cables_3V_traffic$name,"sum_visitors"=cables_3V_traffic$sum_visitors)

strava_cable<-left_join(try,data_ID_try,by="ID")

strava_cable2<-strava_cable%>%drop_na(sum_visitors)

glm_mean<-nls(data=strava_cable2, lyr.1 ~ a*sum_visitors+b, start=list(a=2,b=10),algorithm = "default",trace=F)
gam_mean<-gam(lyr.1 ~ s(sum_visitors),data=strava_cable2)
summary(gam_mean)

strava_cable <- strava_cable %>% mutate("lyr.1_std"=scale(lyr.1))

plot(gam_mean,col="red")
points(strava_cable$sum_visitors,strava_cable$lyr.1_std)

ggplot(data=strava_cable)+
  geom_point(aes(x=sum_visitors,y=lyr.1))+
  stat_smooth(aes(x=sum_visitors,y=lyr.1),method="loess",span=1)
  

summary(glm_mean)
plot(glm_mean)


# try$ID<-
as.vector(cables_3V_traffic$name)

try_2<- rasterize(cables_3V_traffic,strava)


try_sf<-st_sf(try)

try
strava
plot(try)
extract(try$length)
cables_3V_traffic
plot(try)
strava
strava[6,45]
strava[45.41,6.6]
