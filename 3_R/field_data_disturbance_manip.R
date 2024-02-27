#### PhD Tetras_test project ####

# Alice Bordes #

# February 2023 #

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
library(ggforce)
library(ggnewscale)
#********************************************************************


### Loading functions
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
#********************************************************************


### Loading data
#********************************************************************
base<-here()

# GPS locations of black grouses
data_bg_3V<-readRDS(paste0(base,"/1_RAW_DATA/tot.ind.trois_vallees2.rds"))

# GPS locations of black grouses
loteck_hiv2024_3V_raw<-readRDS(paste0(base,"/1_RAW_DATA/field_data_birds/loteck.hiv2024.rds"))


# 3V borders 
borders_3V <- sf::st_read(paste0(base,"/1_RAW_DATA/borders_3V.gpkg"))
borders_3V<-borders_3V$geom
borders_3V<- fortify(borders_3V)

# slope 3V
raster3V_slope<-terra::rast(paste0(base,"/1_RAW_DATA/raster.3V.slope.tif"))

# Analyse à 9m
#carto_habitats_3V_brute <- paste0(base,"/1_RAW_DATA/landcover_T2L_1m_trois_vallees.tif") #carto Clara
carto_habitats_3V <- terra::rast(paste0(base,"/1_RAW_DATA/landcover_T2L_1m_trois_vallees.tif")) #carto Clara
#********************************************************************





#### 1_Visulising GPS locations of multiple birds ####


### Loading maps of the study site
#********************************************************************

## Shape de la zone d'etude restreinte
#e <- extent(971000,985000,6471000,6486000)
e<-ext(loteck_hiv2024_3V_raw)
# visualyze the raster of the slope for the study area
raster_slope_3V<-terra::crop(raster3V_slope,e)

# here the resolution of the raster slope = 1m 
# to save time for the next analyses --> create raster slope with resolution at 10m
raster_slope_3V_10<-terra::aggregate(raster_slope_3V,fact=10)

#raster::readAll(raster_slope_3V) # to save a raster in the RAM (intern memory of the computer), to save time

#raster high vegetation
carto_habitats_3V <- terra::crop(carto_habitats_3V,e)
carto_habitats_3V<-as.factor(carto_habitats_3V) #indicate discrete values for my map categories


#### 1.1_Viewing imported maps ####
par(mfrow=c(2,2))

plot(raster_slope_3V)
plot(borders_3V,add=T) # add 3V borders
plot(raster_slope_3V_10)
plot(borders_3V,add=T) # add 3V borders
plot(carto_habitats_3V)
plot(borders_3V,add=T) # add 3V borders
plot(borders_3V,add=T) # add 3V borders

par(mfrow=c(1,1))

#View habitat cartography realised by Clara Leblanc
ggplot()+
  geom_spatraster(data=carto_habitats_3V)+
  geom_sf(data = borders_3V,fill=NA,color="black",lwd =2)+
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




#Settings
#********************************************************************
graph_title= " day (0am-0pm) "
UTC1='00:00:00'
UTC2='24:00:00'

#********************************************************************

#Automatic settings
#********************************************************************
graph_name=paste0(gsub(" ", "",graph_title),"_")
#********************************************************************



### Loading birds locations (lotrack sensors)
#********************************************************************

#focus on bird locations in winter season ; if heure (UTC) : 9h-14h (UTC +1) = 8h-13h (UTC)
data_loteck_hiv2024_3V<-as.data.frame(loteck_hiv2024_3V_raw%>%filter(saison=="hiver")) %>%  filter(date >= as.Date('2024-02-01')) %>%  filter(heure >= hms(UTC1) & heure <= hms(UTC2))

#create a list of data.frames for each animal
loteck_hiv2024_3V<-multiple_dt_indiv(data_loteck_hiv2024_3V,"nom")

#******* not working yet
#' #' The first day of location data often shows some unrealistic movements. We remove those observations
#' for(i in 1:length(loteck_hiv2024_3V))
#' {
#'   loteck_hiv2024_3V[[i]]<- loteck_hiv2024_3V[[i]][(loteck_hiv2024_3V[[i]]["date"]) > (first(loteck_hiv2024_3V[[i]]["date"]) + ddays(1)),]
#' }
#*******

#pr 1 animal:
#loteck_hiv2024_3V <- loteck_hiv2024_3V[(loteck_hiv2024_3V$date) > min(loteck_hiv2024_3V$date) + days(1), ]


#' Create a dt formatted like a telemetry object
loteck_hiv2024_3V_pretelemetry<-loteck_hiv2024_3V
for(i in 1:length(loteck_hiv2024_3V_pretelemetry))
{
  loteck_hiv2024_3V_pretelemetry[[i]]<- pre_telemetry(loteck_hiv2024_3V_pretelemetry[[i]])
}

loteck_hiv2024_3V_telemetry<-loteck_hiv2024_3V_pretelemetry
for(i in 1:length(loteck_hiv2024_3V_telemetry))
{
  loteck_hiv2024_3V_telemetry[[i]]<- as.telemetry(loteck_hiv2024_3V_telemetry[[i]])
}

# loteck_hiv2024_3V_pretelemetry_all<-pre_telemetry(loteck_hiv2024_3V)
#******* not working yet
# loteck_hiv2024_3V_telemetry_all<-as.telemetry(loteck_hiv2024_3V_pretelemetry_all[,-1])
#******* 
#*
#********************************************************************




#### 1.2_Visulising GPS winter locations of 3V birds ####
#********************************************************************


# create a list with bird's names to plot the correct legend
vect_nicknames<-list()
for(i in (1:length(loteck_hiv2024_3V_pretelemetry)))
{
  vect_nicknames[[i]]<-unique(loteck_hiv2024_3V_pretelemetry[[i]]["individual.local.identifier"])
}
vect_nicknames<-unlist(vect_nicknames)

# renamed each data frame from the list of data frames by the name of each bird 
names(loteck_hiv2024_3V_pretelemetry)<-vect_nicknames


# plot the birds
g_positions_birds<-ggplot()+
  geom_spatraster(data=raster_slope_3V_10)+
  scale_fill_gradientn("Slope (°)", limits=c(0,90),colours=c("#FFFFFF","#CCCCCC" ,"#666666","#333333","#000000")) +
  new_scale("fill") + #to supply an new fill scale 
  geom_sf(data = borders_3V,fill=NA,color="black",lwd =2)+
  geom_point(data = bind_rows(loteck_hiv2024_3V_pretelemetry, .id="df"),aes(x=X_GPS_lambert93,y=Y_GPS_lambert93,colour = df))+
  scale_color_discrete(name = "Bird name")+
  geom_mark_ellipse(data = bind_rows(loteck_hiv2024_3V_pretelemetry, .id="df"),aes(x=X_GPS_lambert93,y=Y_GPS_lambert93,fill=df,colour=df),expand=0,inherit.aes = FALSE)+
  # draw the the smallest possible ellipse that can enclose the points in each group
  scale_fill_discrete("Strict movement area")+
  theme_bw() +
  xlim(e[1],e[2])+
  ylim(e[3],e[4])+
  theme(plot.title = element_text(size=18, face="bold"),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank())+
  ggtitle(paste0("2024 February",graph_title,"positions of \n6 GPS-tagged (biotrack) birds at the 3 Valleys site"))+
  xlab("x coordinate (Lambert 93)")+
  ylab("y coordinate (Lambert 93)")

g_positions_birds

ggsave(plot=g_positions_birds,file=paste0(base,"/6_FIGURES/field_data/","6_biotracks_g_positions_birds_",graph_name,"_ellipses_",format(Sys.time(), "%d.%b%Y"),".png"),scale=3)

#********************************************************************


# plot the birds
g_positions_birds_2<-ggplot()+
  
  geom_spatraster(data=carto_habitats_3V)+
  geom_sf(data = borders_3V,fill=NA,color="black",lwd =2)+
  scale_fill_manual(
       values = c("#CC9966","#CCCCCC","#666666","#333333","#99CC99","#FFFF66","#FFCCCC","#FF99CC","#99FF99","#99FF00","#339966","#993300","#99CCFF","#99FFFF","#0066FF","white","white"),
       breaks = c("20","21","22","23","30","31","32","40","50","51","52","60","92","93","94","100"),
       labels = c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified" ))+
    labs( title="Habitat cartography",
          x = "Longitude",
          y = "Latitude",
          fill = "Landcover")+

  geom_point(data = bind_rows(loteck_hiv2024_3V_pretelemetry, .id="df"),aes(x=X_GPS_lambert93,y=Y_GPS_lambert93, shape = df))+
  theme_bw() +
  #coord_equal() +
  xlim(e[1],e[2])+
  ylim(e[3],e[4])+
   theme(plot.title = element_text(size=18, face="bold"),
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
  scale_shape_manual(name="Bird name",values=c(2,4,8,12,1,13))+
  ggtitle(paste0("2024 February",graph_title,"positions of \n6 GPS-tagged (biotrack) birds at the 3 Valleys site"))+
  xlab("x coordinate (Lambert 93)")+
  ylab("y coordinate (Lambert 93)")

g_positions_birds_2

ggsave(plot=g_positions_birds_2,file=paste0(base,"/6_FIGURES/field_data/","6_biotracks_g_positions_birds_2_",graph_name,format(Sys.time(), "%d.%b%Y"),".png"),scale=3)

#********************************************************************

