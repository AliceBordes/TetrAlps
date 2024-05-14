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
library(ggbreak)
library(raster)
#********************************************************************

### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD/TetrAlps"
season="hiver"
# sex="male"
sex=c("male","femelle")
#********************************************************************




### Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/mean_size_area.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/visu_home_range.R")
#********************************************************************

### Loading heavy saved models ----
#********************************************************************
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/best_model_saved_hiver_malefemelle.RData")
assign("best_model_saved_hiver_malefemelle",best_model)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_akde_saved_hiver_malefemelle.RData")
assign("grouse_winter_akde_saved_hiver_malefemelle",grouse_winter_akde)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_telemetry_hiver_malefemelle.RData")
assign("grouse_winter_telemetry_hiver_malefemelle",grouse_winter_telemetry)

load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/best_model_saved_ete_malefemelle.RData")
assign("best_model_saved_ete_malefemelle",best_model)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_akde_saved_ete_malefemelle.RData")
assign("grouse_winter_akde_saved_ete_malefemelle",grouse_winter_akde)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_telemetry_ete_malefemelle.RData")
assign("grouse_winter_telemetry_ete_malefemelle",grouse_winter_telemetry)

load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/best_model_saved_automne_malefemelle.RData")
assign("best_model_saved_automne_malefemelle",best_model)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_akde_saved_automne_malefemelle.RData")
assign("grouse_winter_akde_saved_automne_malefemelle",grouse_winter_akde)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_telemetry_automne_malefemelle.RData")
assign("grouse_winter_telemetry_automne_malefemelle",grouse_winter_telemetry)

load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/best_model_saved_printemps_malefemelle.RData")
assign("best_model_saved_printemps_malefemelle",best_model)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_akde_saved_printemps_malefemelle.RData")
assign("grouse_winter_akde_saved_printemps_malefemelle",grouse_winter_akde)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_telemetry_printemps_malefemelle.RData")
assign("grouse_winter_telemetry_printemps_malefemelle",grouse_winter_telemetry)

load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/best_model_saved_hiver_male.RData")
assign("best_model_saved_hiver_male",best_model)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_akde_saved_hiver_male.RData")
assign("grouse_winter_akde_saved_hiver_male",grouse_winter_akde)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_telemetry_hiver_male.RData")
assign("grouse_winter_telemetry_hiver_male",grouse_winter_telemetry)

load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/best_model_saved_hiver_femelle.RData")
assign("best_model_saved_hiver_femelle",best_model)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_akde_saved_hiver_femelle.RData")
assign("grouse_winter_akde_saved_hiver_femelle",grouse_winter_akde)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/grouse_winter_telemetry_hiver_femelle.RData")
assign("grouse_winter_telemetry_hiver_femelle",grouse_winter_telemetry)
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

### RASTERS

# slope 3V
slope_3V<-terra::rast(file.path(base,"2_DATA/slope_3V_ign.tif"))
crs(slope_3V)<-"+init=epsg:2154"
slope_3V_9<-terra::rast(file.path(base,"2_DATA/slope_3V_9_ign.tif"))
crs(slope_3V_9)<-"+init=epsg:2154"

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
carto_habitats_3V_9_WGS84<- project(carto_habitats_3V_9,y="+proj=longlat +datum=WGS84")

# strava
strava <- terra::rast(file.path(base, "2_DATA/strava/strava_3V_winter_sports_rgb.tif_single.tif"))
strava <- project(strava,y="+proj=longlat +datum=WGS84")
strava_lambert <- project(strava,y="+init=epsg:2154")

# mnt
mnt<-terra::rast(file.path(base, "2_DATA/mnt_ign.tif"))
mnt_9<-terra::rast(file.path(base, "2_DATA/mnt_9_mean_ign.tif"))
mnt_9_WGS84<- project(mnt_9,y="+proj=longlat +datum=WGS84")

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
  # grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_telemetry[[i]],datum="WGS84",keep=c("saison","period_jour","sexe","age"))
  grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_telemetry[[i]],datum="WGS84")
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

# save(grouse_winter_telemetry, file=paste0(base,"/3_R/heavy_saved_models/grouse_winter_telemetry_",season,"_",paste0(sex,collapse=""),".RData"))


grouse_winter_pretelemetry_all<-pre_telemetry(data_bg_3V)
grouse_winter_telemetry_all<-as.telemetry(as.data.frame(grouse_winter_pretelemetry_all[,-c(1,2,3)]))
#********************************************************************



#### 2_Visulising GPS locations of multiple birds ####


#### 2.1_Viewing imported maps ----
#********************************************************************

#raster::readAll(slope_3V) # to save a raster in the RAM (intern memory of the computer), to save time

# carte d'occupation des sols OSO (produite par le Centre d'Expertise Scientifique sur l'occupation des sols (CES OSO))
# oso <- terra::rast("M:/CESBIO/OSO_20220101_RASTER_V1-0/DATA/OCS_2022.tif") 
# oso <- terra::crop(oso,e)

par(mfrow=c(2,4))

plot(slope_3V,main="Slope(°)\nresolution=1m",col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"))
plot(borders_3V_vect,add=T) # add 3V borders
plot(slope_3V_9,main="Slope(°)\nresolution=9m",col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"))
plot(borders_3V_vect,add=T) # add 3V borders
plot(carto_habitats_3V,main="Habitat cartography\nresolution=1m")
plot(borders_3V_vect,add=T) # add 3V borders
# plot(oso,main="OSO\nresolution=10m")
# plot(borders_3V_vect,add=T) # add 3V borders
plot(strava,main="Strava, 4 attendance levels\nresolution=1m")
plot(borders_3V_vect,add=T) # add 3V borders
plot(mnt,main="MNT\nresolution=1m",col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"))
plot(borders_3V_vect,add=T)
plot(mnt_9,main="MNT\nresolution=9m",col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"))
plot(borders_3V_vect,add=T)
plot(mnt,main="MNT\nresolution=1m",col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"))
plot(lek_locations_vect,main="Leks",add=T)
plot(borders_3V_vect,add=T)
plot(borders_3V_vect_WGS84,border=as.factor(borders_3V_vect_WGS84$NOM),lwd=2)

# plot mnt with a mask around 3V
# plot(mnt_9,borders_3V_vect,mask=T)

par(mfrow=c(1,1))

#View habitat cartography realised by Clara Leblanc
ggplot()+
  geom_spatraster(data=carto_habitats_3V)+
  geom_sf(data = borders_3V_vect,fill=NA,color="black",lwd =2)+
  scale_fill_manual(
    values = c("#CC9966","#CCCCCC","#666666","#333333","#99CC99","#FFFF66","#FFCCCC","#FF99CC","#99FF99","#99FF00","#339966","#993300","#99CCFF","#99FFFF","#0066FF","white","white"),
    breaks = c("20","21","22","23","30","31","32","40","50","51","52","60","92","93","94","100"),
    # labels = c("sol non classé",sol mineral fin","sol mineral grossier","falaise", "pelouse seche ou rocheuse","herbacées,"ligneux bas","arbustes","arbres non classés,"arbres feuillus","arbres resineux","bati","plan d'eau naturel","plan d'eau artificiel","cours d'eau",  "non classe" ))+
    labels = c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified" ))+
  labs( title="Habitat cartography",
        x = "Longitude",
        y = "Latitude",
        fill = "Legend")
#********************************************************************







#### 2.2_Visulising GPS winter locations of 3V birds ####
#********************************************************************


windows()
#View habitat cartography realised by Clara Leblanc
ggplot()+
  geom_spatraster(data=carto_habitats_3V)+
  geom_point(data = bind_rows(grouse_winter_pretelemetry, .id="df"),aes(x=X_GPS_lambert93,y=Y_GPS_lambert93,colour = "black"))+
  scale_colour_manual(values="black")+
  geom_sf(data = borders_3V_vect,fill=NA,color="black",lwd =2)+
  scale_fill_manual(
    values = c("#CC9966","#CCCCCC","#666666","#333333","#99CC99","#FFFF66","#FFCCCC","#FF99CC","#99FF99","#99FF00","#339966","#993300","#99CCFF","#99FFFF","#0066FF","white","white"),
    breaks = c("20","21","22","23","30","31","32","40","50","51","52","60","92","93","94","100"),
    # labels = c("sol non classé",sol mineral fin","sol mineral grossier","falaise", "pelouse seche ou rocheuse","herbacées,"ligneux bas","arbustes","arbres non classés,"arbres feuillus","arbres resineux","bati","plan d'eau naturel","plan d'eau artificiel","cours d'eau",  "non classe" ))+
    labels = c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified" ))+
  labs( title="Habitat cartography",
        x = "Longitude",
        y = "Latitude",
        fill = "Legend")



# plot the birds
g_positions_birds<-ggplot()+
  #geom_raster(data=map_df,aes(x=X_GPS, y=Y_GPS,fill=slope))+
  geom_spatraster(data=slope_3V_9)+
  geom_sf(data = borders_3V_vect,fill=NA,color="black",lwd =2)+
  
  #display birds all positions
  geom_point(data = bind_rows(grouse_winter_pretelemetry, .id="df"),aes(x=X_GPS_lambert93,y=Y_GPS_lambert93,colour = df))+
  
  theme_bw() +
  #coord_equal() +
  xlim(e[1],e[2])+
  ylim(e[3],e[4])+
  scale_fill_gradientn("Slope (°)", limits=c(0,90),colours=c("#FFFFFF","#CCCCCC" ,"#666666","#333333","#000000")) +
  theme(plot.title = element_text(size=22, face="bold"),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank())+
  scale_color_discrete(name = "Animals",labels=vect_nicknames)+
  ggtitle("All positions of the 38 GPS-tagged birds at the 3 Valleys site (winter)")+
  xlab("Longitude")+
  ylab("Latitude")

windows()
g_positions_birds



# plot capture sites
g_positions_capture_sites<-ggplot()+
  #geom_raster(data=map_df,aes(x=X_GPS, y=Y_GPS,fill=slope))+
  geom_spatraster(data=slope_3V_9)+
  geom_sf(data = borders_3V_vect,fill=NA,color="black",lwd =2)+
  
  #display bird capture positions
  geom_sf(data = lek_sites,aes(geometry = larger_lek), fill="green")+
   
  theme_bw() +
  #coord_equal() +
  xlim(e[1],e[2])+
  ylim(e[3],e[4])+
  scale_fill_gradientn("Slope (°)", limits=c(0,90),colours=c("#FFFFFF","#CCCCCC" ,"#666666","#333333","#000000")) +
  theme(plot.title = element_text(size=22, face="bold"),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank())+
  # scale_colour_manual(values="black")+
  # scale_color_discrete(name = "Capture zones")+
  ggtitle("Lek sites in the Trois Vallées skiing area")+
  xlab("Longitude")+
  ylab("Latitude")

windows()
g_positions_capture_sites
#********************************************************************




#### 3_Describing the spatial correlations between observation data ####
#********************************************************************

#### 3.1_Sampling schedule ####

# Pooling Variograms : If multiple individuals exhibit similar movement behaviors

# To prospect if the sampling there is a regular or irregular sampling schedule 
# or to visualyze data when the sampling rate changes during data collection
par(mfrow=c(1,1))
dt.plot(grouse_winter_telemetry,main="Log-scale plot of the sorted sampling interval between 2 consecutive GPS positions") #here the sampling is semi-irregular 

# visualizing the irregular sampling schedule 

dt_box<-data.frame("birds"=1:length(grouse_winter_telemetry),"interval_hr"=summary(grouse_winter_telemetry)$interval,"animal"=vect_nicknames)

ggplot(data=dt_box[-33,],aes(x=birds,y=interval_hr))+
  geom_boxplot()+
  # scale_y_continuous(trans = 'log10')+
  geom_dotplot(binaxis="y", stackdir='center', dotsize=0.5)+
  xlab("Birds")+
  ylab("Time interval between 2 positions (hours)")+
  ggtitle("Visualizing the sampling schedule of GPS positions")+
  theme(plot.title = element_text(size=22, face="bold"),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))

windows()
ggplot(data=bind_rows(grouse_winter_pretelemetry, .id="df"),aes(x=fct_reorder(individual.local.identifier,(dt),.na_rm=T),y=(dt)))+
  geom_boxplot()+
  #geom_dotplot(binaxis="y", stackdir='center', dotsize=0.2)+
  xlab("Birds")+
  ylab("Time interval between 2 positions (day:hr:min)")+
  ggtitle("Visualizing the sampling schedule of GPS positions")+
  scale_y_time(limits=c(0,60000))+
  theme(plot.title = element_text(size=22, face="bold"),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14,angle=90,vjust=0.5),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))


#### 3.2_Positions autocorrelation ####

par(mfrow=c(1,2))
# population variogram assuming homogenous sampling schedule (which is not the case)
SVF_0 <- lapply(grouse_winter_telemetry,variogram) # population variogram
SVF_0<- mean(SVF_0)
plot(SVF_0,fraction=0.005,level=c(0.5,0.95))
title("Population variogram\n(assuming homogenous sampling schedule)")

# population variogram considering the irregular sampling schedule
timelags <- c(1,2,4,8) %#% "hour" # the order has no importance
SVF <- lapply(grouse_winter_telemetry,variogram,dt=timelags) # population variogram considering the GPS were programmed to cycle between 1, 12, 24 hour sampling intervals
SVF<- mean(SVF)
plot(SVF,fraction=0.005,level=c(0.5,0.95),main="Population variogram \n(considering the irregular sampling schedule with the most common time intervals : 1h, 2h, 4h, 8h)")

#************************************** not working anymore
# #  HISTOGRAM of time between 2 locations 
# par(mfrow=c(1,1))
# windows()
# 
# ggplot(data = bind_rows(grouse_winter_pretelemetry, .id="df"),aes(x=as.POSIXct(dt,format="%d %H:%M:%S")))+
#   geom_histogram(fill="#FF6666", bins = 50)+
#   facet_grid(df ~ .)+
#   scale_x_datetime(date_labels = "%d d %H h")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   xlab("Time between 2 locations")
# 
# ggplot(data = bind_rows(grouse_winter_pretelemetry, .id="df"),aes(x=as.POSIXct(dt,format="%d %H:%M:%S")))+
#   geom_histogram(fill="#FF6666", bins = 50)+
#   facet_grid(df ~ .)+
#   scale_x_datetime(date_labels = "%d d %H h %M min",breaks ="3 hours",expand = c(0, 0))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   xlab("Time between 2 locations")
#**************************************




#### 4_Fitting a RSF ####




#### 4.1_Data preparation, for home-range and RSF estimations on each bird of the Trois Vallées ski resort ####
#********************************************************************
#' Fit ctmm model : Continuous-Time Movement Modeling

# Model fitting and selection first requires a prototype model with guesstimated parameters 
#' ctmm.guess --> large approximation of the maximum likelyhood, inaccurate estimation of the parameters (VS ctmm.fit() Performs maximum likelihood parameter and interval estimation of ctmm models using each model’s exact likelihood function)
grouse_winter_guess <- lapply(grouse_winter_telemetry,ctmm.guess, CTMM=ctmm(isotropic = TRUE), interactive = FALSE) #isotropic = TRUE => s'éloigne du centre de manière identique dans toutes les directions

#plots a variogram object overlayed with a continuous-time movement model guesstimated from the variogram's shape
# isotropic = TRUE beacuse we consider the home range (espace vital) 
# as a sphere (attractor center), 
# even if an ellipse is more realistic (anisotropy) 
# but the function is not optized with (isotropic=F)

#model selected (approximation of the parameters)
grouse_winter_guess_summary<-lapply(grouse_winter_guess,summary)

# selection of the 5 best model structures
fitted_models_grouse_winter<-lapply(grouse_winter_telemetry,ctmm.select,CTMM=grouse_winter_guess, verbose=TRUE)
#CTMM = GUESS marche pas toujours, CTMM = A ctmm movement-model object containing the initial parameter guesses
fitted_models_grouse_winter_summary<-lapply(fitted_models_grouse_winter,summary,unit=F)
# "OUF anisotropic" is the "best" model, IID is the conventional model 
best_model<-list()
for (i in 1:length(grouse_winter_guess))
{
  best_model[[i]]<-fitted_models_grouse_winter[[i]][1]
}

names(best_model)<-vect_nicknames

save(best_model, file=paste0(base,"/3_R/heavy_saved_models/best_model_saved_",season,"_",paste0(sex,collapse=""),".RData"))


# Home range calculation                                                                                                                                       

grouse_winter_akde<-list()
#' Fit akde (take into account the autocorrelation of the positions in the dataset)
for (i in 1:length(grouse_winter_guess)) 
{
  grouse_winter_akde[[i]]<-akde(grouse_winter_telemetry[[i]],CTMM=best_model[[i]])
}

names(grouse_winter_akde)<-vect_nicknames

save(grouse_winter_akde, file=paste0(base,"/3_R/heavy_saved_models/grouse_winter_akde_saved_",season,"_",paste0(sex,collapse=""),".RData"))
#********************************************************************




#### 4.2_Home range vizualization on each bird of the Trois Vallées ski resort ####
#********************************************************************
# Visualizing the SVF of the guess model and comparison of the 2 best fitted models on variogram
# and save
for (i in 1:length(grouse_winter_guess)) {
  if (i %% 5 == 1) { # Start a new page for every 5 plots
    png(filename = paste0(base, "/5_OUTPUTS/RSF/variograms/indiv_variogram", i, "_", i + 4, ".png"),height = 1000,width = 2400) # Naming files correctly
    par(mfcol = c(2,5))
  }
  
  plot(SVF, CTMM = grouse_winter_guess[[i]], col.CTMM = i, new = FALSE, fraction = 0.2, level = c(0.5, 0.95),
       main = paste0(vect_nicknames[i], "\n", grouse_winter_guess_summary[[i]]$name),
       sub = paste("\narea estimated =", round((grouse_winter_guess_summary[[i]]$CI[1, 2])/1000000, 3)," km^2"),
       cex.sub = 1.2, font.sub = 2, cex.lab = 1.5, cex.main = 1.5)
  
  plot(SVF, CTMM = grouse_winter_guess[[i]], col.CTMM = i, new = FALSE, fraction = 0.0005, level = c(0.5, 0.95),
       cex.sub = 2, cex.lab = 1.5)
  
  if (i %% 5 == 0 || i == length(grouse_winter_guess)) {
    dev.off() # Close the device after every 5 plots or at the end
  }
}



# and save

for (i in 1:length(grouse_winter_guess)) {
  if (i %% 9 == 1) { # Start a new page for every 5 plots
    jpeg(filename = paste0(base, "/5_OUTPUTS/RSF/home_range_akde/home_range_akde", i, "_", i + 8, ".png"), units="in", width=15, height = 10, res =300) # Naming files correctly
    par(mfcol = c(3,3))
  }
  
  plot(grouse_winter_telemetry[[i]],UD=grouse_winter_akde[[i]],main=vect_nicknames[[i]],
       units=F,
       sub = paste("\narea estimated =", 
                   round((summary(grouse_winter_akde[[i]])$CI[,"est"])/1000000, 3)," km^2",
                   "             CI=[",
                   round((summary(grouse_winter_akde[[i]])$CI[,"low"])/1000000, 3),
                   ",",
                   round((summary(grouse_winter_akde[[i]])$CI[,"high"])/1000000, 3),
                   "]"),
       cex.sub=1.2,col.sub="blue")
  
  if (i %% 9 == 0 || i == length(grouse_winter_guess)) {
    dev.off() # Close the device after every 5 plots or at the end
  }
}


ci<-c()
for (i in 1:length(grouse_winter_akde))
{
  # print(summary(grouse_winter_akde[[i]])$CI)
  ci[[i]]<-(summary(grouse_winter_akde[[i]],unit=F)$CI[,"est"])/1000000
}

round(mean(unlist(ci)),3)





# visualizing the home range density estimates against the position data of each bird of the Trois Vallées

source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/visu_home_range.R")
visu_HR(colorby="4seasons")

# visualizing the home range density estimates per sex in the Trois Vallées
visu_HR("mnt","hiver",colorby="sex",showpoints=TRUE,writeplot = TRUE) 
visu_HR("strava","hiver",colorby="sex",showpoints=TRUE,writeplot = TRUE) 
visu_HR("strava","hiver",showpoints=TRUE,writeplot = TRUE) 

visu_HR("mnt","hiver",showleks=TRUE,showpoints=TRUE,writeplot = TRUE) 
visu_HR("mnt","automne",showleks=TRUE,showpoints=TRUE,writeplot = TRUE)
visu_HR("mnt","ete",showleks=TRUE,showpoints=TRUE,writeplot = TRUE)
visu_HR("mnt","printemps",showleks=TRUE,showpoints=TRUE,writeplot = TRUE)

# visualizing the home range density estimates and the capture site for each bird of the Trois Vallées
color_birds_hiver<-read.table("C:/Users/albordes/Documents/PhD/TetrAlps/5_OUTPUTS/RSF/home_range_akde/mean_size_HR_season/color_birds_hiver.txt", sep = "", header = TRUE) # Color dataframe from plot_mean_area_HR("hiver") to associate a specific color with each home range and capture location for a given bird.
visu_HR(season="hiver",colorby="indiv",color_palette=color_birds_hiver,writeplot = TRUE)


# Summary of mean size home-range for a given season

plot_mean_area_HR(season="hiver",colorby="sex",synth_bg_3V,writeplot = TRUE)
plot_mean_area_HR(season="hiver",data_sex=synth_bg_3V,writeplot = TRUE)

plot_mean_area_HR(season="automne",data_sex=synth_bg_3V,writeplot = TRUE)

plot_mean_area_HR(season="ete",data_sex=synth_bg_3V,writeplot = TRUE)

plot_mean_area_HR(season="printemps",data_sex=synth_bg_3V,writeplot = TRUE)


# funnel(x=grouse_winter_akde_saved_hiver_malefemelle,y=grouse_winter_telemetry_hiver_malefemelle,sort=TRUE,col=c($colour,"black"),main="Home-range size estimations at 95% in winter")
# cluster(grouse_winter_akde_saved_automne_malefemelle,sort=TRUE,main="Home-range size estimations at 95% in Automne n\Membership test for subpopulation")


# Home range VS capture sites 
par(oma = c(1,1,1,1))
plot(borders_3V_vect,ext=e,border="black",lwd=2,
     main="Winter home-ranges at 95% for all resident black grouse\n in the Trois Vallées ski resort",
     xlab="Longitude",
     ylab="Latitude",
     cex.main=2,
     cex.lab = 1.5,
     plg = list(title = "DEM (m)",title.cex = 1.5,cex=1.2))

if (i %% 6 == 1 || i == length(grouse_winter_akde_saved_hiver_malefemelle)) {
    
  jpeg(filename = paste0(base, "/5_OUTPUTS/RSF/home_range_akde/distance_to_capture_site/HR_distance_to_capture_site_",season,"_", i, "_", i + 5, ".png"), units="in", width=15, height = 10, res =300) # Naming files correctly
  
  par(mfcol = c(2,3))
  
  plot(grouse_winter_akde_saved_hiver_malefemelle[[i]],
       units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),col.grid=NA,bty="n",col.UD=color_dt$colour[i],col.level=color_dt$colour[i],level=NA,
       main=paste("bird:",grouse_winter_akde_saved_hiver_malefemelle[[i]]@info$identity),cex.main=1.2,col.sub="blue") 
  
  points(color_dt$x_capture_lambert[color_dt$ani_name==grouse_winter_akde_saved_hiver_malefemelle[[i]]@info$identity],color_dt$y_capture_lambert[color_dt$ani_name==grouse_winter_akde_saved_hiver_malefemelle[[i]]@info$identity],col="black",cex=1,type="p",pch=20)
  plot(borders_3V_vect,ext=e,border="black",lwd=2,add=TRUE)
  
  if (i %% 6 == 0 || i == length(grouse_winter_akde_saved_hiver_malefemelle)) {
    dev.off() # Close the device after every 5 plots or at the end
  }
}
#********************************************************************





#### 4.3_Fitting an RSF
#********************************************************************

# raster3V_slope_brute<-paste0("C:/Users/albordes/Documents/PhD/TetrAlps_old/1_RAW_DATA/raster.3V.slope.tif")
# raster_slope_3V<-raster(raster3V_slope_brute)  # nothing --> RasterLayer object
# raster_slope_3V_Abel <- raster::crop(raster_slope_3V,e) # Cut out a geographic subset and save the raster with the new extents --> reduce the limits of the raster
# raster_slope_3V_Abel_10<-aggregate(raster_slope_3V_Abel,10)
# 
# plot(raster_slope_3V_Abel_10) 
# be <- list("slope" = raster_slope_3V_Abel_10)

#pb : my raster is not in background so the function extract()[--> extracting values of the raster under my telemetry points] inside rsf.fit() can not work.
plot(grouse_winter_telemetry_hiver_malefemelle[[1]],raster_list[[1]])
plot(raster_list[[1]])

#' Create named list of rasters in the WGS84 format
r_mnt_9<-raster(mnt_9)
raster_list <- list("mnt" = r_mnt_9)
# raster_list <- list("carto_hab" = raster(carto_habitats_3V_9))

grouse_winter_rsf_riemann<-rsf.fit(grouse_winter_telemetry_hiver_malefemelle[[1]], grouse_winter_akde_saved_hiver_malefemelle[[1]], R=raster_list,  integrator = "Riemann")


raster::readAll(r_slope_3V_9) # to save the raster (not Spatraster) in the RAM and save time 

rsf_list <- lapply(1:length(grouse_winter_telemetry_hiver_malefemelle), function(i){
  ctmm::rsf.fit(grouse_winter_telemetry_hiver_malefemelle[[i]], grouse_winter_akde_saved_hiver_malefemelle[[i]],  integrator = "Riemann")
})

#' The integrator = "Riemann" option is still experimental, we use it here because it is much faster
grouse_winter_rsf_riemann<-list()
for (i in 1: length(grouse_winter_telemetry_hiver_malefemelle))
{
  grouse_winter_rsf_riemann[[i]]<-rsf.fit(grouse_winter_telemetry_hiver_malefemelle[[i]], grouse_winter_akde_saved_hiver_malefemelle[[i]],  integrator = "Riemann")
}

# R = must be a list of rasters to fit Poisson regression coefficients to (under a log link)

grouse_winter_rsf_riemann_summary<-lapply(grouse_winter_rsf_riemann,summary)


#' Range distribution (includes the ranging behaviour) (étendue de la distribution)
agde_grouse_winter<-list()
for (i in 1: length(grouse_winter_telemetry))
{
  agde_grouse_winter[[i]]<-agde(CTMM = grouse_winter_rsf_riemann[[i]],raster_list)
}


windows()
par(mfrow=c(1,5))
for ( i in (1:length(grouse_winter_telemetry)))
{
  plot(agde_grouse_winter[[i]],main=vect_nicknames[[i]])
}



mean_rsf<-ctmm:mean(grouse_winter_rsf_riemann)
mean_akde<-ctmm:mean(grouse_winter_akde)

#********************************************************************

#rsf with more raster than the slope

#' Create named list of rasters
be2 <- list("slope1" = slope_3V_9,"high_vegetation"=raster_high_vege_classif_9m_9)

#' The integrator = "Riemann" option is still experimental, we use it here because it is much faster
grouse_winter_rsf_riemann2<-list()
for (i in 1: length(grouse_winter_telemetry))
{
  grouse_winter_rsf_riemann2[[i]]<-rsf.fit(grouse_winter_telemetry[[i]], grouse_winter_akde[[i]], R = be2, integrator = "Riemann")
}

# R = must be a list of rasters to fit Poisson regression coefficients to (under a log link)

grouse_winter_rsf_riemann2_summary<-lapply(grouse_winter_rsf_riemann2,summary)













#### 4.2_Fitting a RSF on all the population as a single indiv ####


#' Fit ctmm model : Continuous-Time Movement Modeling

# Model fitting and selection first requires a prototype model with guesstimated parameters 
#' ctmm.guess --> large approximation of the maximum likelyhood, inaccurate estimation of the parameters (VS ctmm.fit() Performs maximum likelihood parameter and interval estimation of ctmm models using each model’s exact likelihood function)
grouse_winter_guess_all <- ctmm.guess(grouse_winter_telemetry_all, CTMM=ctmm(isotropic = TRUE), interactive = FALSE) #isotropic = TRUE => s'éloigne du centre de manière identique dans toutes les directions

#model selected (approximation of the parameters)
grouse_winter_guess_summary_all<-summary(grouse_winter_guess_all)

# selection of the 5 best model structures
fitted_models_grouse_winter_all<-ctmm.select(grouse_winter_telemetry_all,CTMM=grouse_winter_guess_all, verbose=TRUE)
#CTMM = GUESS marche pas toujours, CTMM = A ctmm movement-model object containing the initial parameter guesses
fitted_models_grouse_winter_summary_all<-summary(fitted_models_grouse_winter_all)
# "OUF anisotropic" is the "best" model, IID is the conventional model 
best_model_all<-fitted_models_grouse_winter_all[[1]][1]



# Visualizing the SVF of the guess model and comparison of the 2 best fitted models on variogram

plot(SVF_pop, CTMM = grouse_winter_guess_all, col.CTMM = i, new = FALSE, fraction = 0.2, level = c(0.5, 0.95),
     sub = paste("\narea estimated =", round(grouse_winter_guess_summary_all$CI[1, 2], 3)),
     cex.sub = 1.2, font.sub = 2, cex.lab = 1.5, cex.main = 1.5)


#visualizing the home range density estimates against the position data                                                                                                                                         

#' Fit akde (take into account the autocorrelation of the positions in the dataset)
grouse_winter_akde_all<-akde(grouse_winter_telemetry_all,CTMM=best_model)

windows()
plot(grouse_winter_telemetry_all,UD=grouse_winter_akde_all,
     units=F,
     sub = paste("\narea estimated =", 
                 round(summary(grouse_winter_akde_all,unit=F)$CI[,"est"], 3),
                 "             CI=[",
                 round(summary(grouse_winter_akde_all,unit=F)$CI[,"low"], 3),
                 ",",
                 round(summary(grouse_winter_akde_all,unit=F)$CI[,"high"], 3),
                 "]"),
     cex.sub=1.2,col.sub="blue")



# visualizing the home range density estimates against the position data of each bird of the Trois Vallées
windows()
plot(mnt,ext=e,col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"),
     main="Winter home-ranges at 95% for all resident black grouse\n in the Trois Vallées ski resort",
     xlab="Longitude",
     ylab="Latitude",
     cex.main=2,
     cex.lab = 1.5,
     plg = list(title = "MNT (m)",title.cex = 1.5,cex=1.2))
plot(borders_3V_vect,ext=e,add=T)

plot(grouse_winter_telemetry_all,UD=grouse_winter_akde_all,
     units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=T,col.grid=NA,bty="n") 

#********************************************************************


















############### brouillon area

par(oma = c(1,1,1,1))
plot(mnt_9,ext=e,col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"),
     main="title_graph",
     xlab="Longitude",
     ylab="Latitude",
     cex.main=2,
     cex.lab = 1.5,
     plg = list(title = "DEM (m)",title.cex = 1.5,cex=1.2))
plot(borders_3V_vect,ext=e,add=TRUE,border="black",lwd=2)


for (i in 1:length(grouse_winter_telemetry_hiver_malefemelle))
{
  plot(grouse_winter_telemetry_hiver_malefemelle[[i]],UD=grouse_winter_akde_saved_hiver_malefemelle[[i]],
       units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n",col.UD="green")
  
  #col.grid=NA --> to removed the white grid in background
  # plot(grouse_winter_akde[[i]],units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n") #col.grid=NA --> to removed the white grid in background
}

plot(grouse_winter_telemetry[[1]],UD=grouse_winter_akde_saved_hiver_malefemelle[[1]])

plot(grouse_winter_telemetry[[1]])
plot(grouse_winter_akde_saved_hiver_malefemelle[[1]])
grouse_winter_telemetry[[1]]@info$projection<-"+proj=longlat +datum=WGS84"

View(grouse_winter_telemetry_hiver_malefemelle[[1]])
View(grouse_winter_akde_saved_hiver_malefemelle[[1]])

grouse_winter_telemetry_hiver_malefemelle[[1]]$x<-grouse_winter_telemetry_hiver_malefemelle[[1]]$longitude
grouse_winter_telemetry_hiver_malefemelle[[1]]$y<-grouse_winter_telemetry_hiver_malefemelle[[1]]$latitude

View(grouse_winter_telemetry_automne_malefemelle[[1]])


load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/old/best_model_saved_hiver_femelle.RData")
assign("best_model_saved_hiver_femelle",best_model)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/old/grouse_winter_akde_saved_hiver_femelle.RData")
assign("grouse_winter_akde_saved_hiver_femelle",grouse_winter_akde)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/old/grouse_winter_telemetry_hiver_femelle.RData")
assign("grouse_winter_telemetry_hiver_femelle",grouse_winter_telemetry)

View(grouse_winter_telemetry_hiver_femelle[[1]])
View(grouse_winter_akde_saved_hiver_femelle[[1]])

plot(grouse_winter_telemetry_hiver_femelle[[1]],UD=grouse_winter_akde_saved_hiver_femelle[[1]])
