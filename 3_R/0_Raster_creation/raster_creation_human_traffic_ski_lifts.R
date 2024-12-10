#### PhD Tetras_test project ####
#'''''''''''''''''''''''''''''''''''
# Alice Bordes #

# March 2024 #


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
# library(maptools)
#devtools::install_github('oswaldosantos/ggsn')
# library(rgeos)
# library(ggsn)
library(usethis)
# library(animove)
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
library(lubridate)
library(lidR)
library(tmap)
library(ggplot2)
library(sjmisc)
library(here)
library(ggspatial)
library(readxl)
library(stringi)
library(ggnewscale)
library(stringdist)
#********************************************************************


# Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/Formatting_data/formatting_environment_data.R")
#********************************************************************


# ---- Settings ---- 
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD"
#********************************************************************


# load(file.path(base,'myRasterEnvironment.RData'))

# ---- Loading data ---- 
#********************************************************************
### VECTORS

# 3V borders 
borders_3V_vect <- terra::vect(file.path(base,"TetrAlps","1_RAW_DATA","borders_3V.gpkg"))

# 3V winter trails (osm)
osm_winter <- terra::vect(file.path(base,"TetrAlps","2_DATA", "osm_ski_piste.gpkg"))
osm_winter <- project(osm_winter, "epsg:2154")
osm_winter_sf <- as_sf(osm_winter)


# 3V ski cables from Open street Map
cables_3V_osm <- vect(file.path(base,"TetrAlps","2_DATA", "ski_lift_osm.gpkg"))
cables_3V_osm <- project(cables_3V_osm,y="epsg:2154")
cables_3V_osm_sf <- as_sf(cables_3V_osm)
cables_3V_osm_sf$name <- stri_trans_general(cables_3V_osm_sf$name, id = "Latin-ASCII")  # delete accents

# 3V cables from OGM (Marc Montadert), NO NAMES 
cables_3V_no_id <- terra::vect(file.path(base,"TetrAlps","1_RAW_DATA","human_traffic_in_ski_resorts", "cables","cables.gpkg"))
cables_3V_no_id<- project(cables_3V_no_id,y="epsg:2154")
# cables_3V_ogm <- vect(file.path(base,"TetrAlps","1_RAW_DATA","human_traffic_in_ski_resorts" , "cables_ogm28resume_troncons_actuel_alpes.gpkg"))

# Visitor numbers
visitor_meribel <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/meribel_visitors.csv", sep=",")
visitor_meribel$Date <- as.Date(visitor_meribel$Date)
visitor_meribel$Total_std <- scale(visitor_meribel$Total)

visitor_valtho <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/valtho_visitors.csv", sep=",")
visitor_valtho$Date <- as.Date(visitor_valtho$Date)
visitor_valtho$Total_std <- scale(visitor_valtho$Total)

visitor_courch <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/courchevel_visitors.csv", sep=",")
visitor_courch$Date <- as.Date(visitor_courch$Date)
visitor_courch$Total_std <- scale(visitor_courch$Total)

visitor_menui <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/menuires_visitors.csv", sep=",")
visitor_menui$Date <- as.Date(visitor_menui$Date)
visitor_menui <- visitor_menui %>% mutate(across(-c(Date, ski_season), as.numeric))
visitor_menui$Total_std <- scale(visitor_menui$Total)


### DATASET
birds_bg_dt<-read.csv2(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry_2024_10.csv"),sep=",") #upload the file from a csv, not a move2 object


# Environment stack
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/env_RL_list.RData"))
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/scaled_env_RL_list.RData"))
#******************************************************************** 



# ---- Shaping the study area ---- 
#********************************************************************
e <- ext(env_RL_list[["elevation"]])
e_poly <- as.polygons(e)
sect <- as_sf(e_poly)
#********************************************************************



#### 1_Calculate human traffic and assign it to individual ski lifts ----
#********************************************************************

#### 1.1_Calculate human traffic and clean ski lifts names ----
#*************
meribel_nb <- cleaning_visitor_data(visitor_meribel)
valtho_nb <- cleaning_visitor_data(visitor_valtho)
courch_nb <- cleaning_visitor_data(visitor_courch)
menui_nb <- cleaning_visitor_data(visitor_menui)


#### 1.2_Tools to check to compatibility between the ski lift names in visitor number data and the osm layer ----
#*************
# To check the differences between names in the data on visitor numbers gave by ski resorts and names in the osm layer
  names_dt <- unique(results[order(results$ski_lift),]$ski_lift)
  names_osm_layer <- cables_3V_osm_sf[order(cables_3V_osm_sf$name), ]$name
  # elements of difference
  names_dt[!names_dt %in% names_osm_layer]


  

  # Function to find the most similar string in a list
  find_most_similar <- function(target_string, string_list) {
    # Calculate the Levenshtein distance between the target string and each string in the list
    distances <- stringdist::stringdist(target_string, string_list, method = "lv")
    
    # Find the index of the string with the minimum distance
    closest_match_index <- which.min(distances)
    
    # Return the closest string and its distance
    return(list(
      closest_match = string_list[closest_match_index],
      distance = distances[closest_match_index]
    ))
  }
  
  # Example of usage
  string_list <- names_osm_layer
  target_string <- "Lac Noir"
  
  result_l <- find_most_similar(target_string, string_list)
  result_l
  

# Plot the ski lifts of the osm layer by name
plot(cables_3V_osm_sf$geometry, col = "blue")
cables_3V_osm_sf_cable1 <- cables_3V_osm_sf[grepl("Montolivet", cables_3V_osm_sf$name, ignore.case = TRUE), ]
plot(add=TRUE, cables_3V_osm_sf_cable1, col = "red")
plot(add=TRUE, borders_3V_vect)



# Summary of the corrections 


# correction when there is a same ski lift name in two different resort
# cables_3V_sf_fusion$sum_visitors[(cables_3V_sf_fusion$name=="Jardin d'Enfants" & cables_3V_sf_fusion$aerialway=="gondola")]<-courch_human_traffic_sum$sum_visitors[courch_human_traffic_sum$cable=="Jardin d'Enfants"]
# cables_3V_sf_fusion$resort[(cables_3V_sf_fusion$name=="Jardin d'Enfants" & cables_3V_sf_fusion$aerialway=="gondola")]<-courch_human_traffic_sum$resort[courch_human_traffic_sum$cable=="Jardin d'Enfants"]
# cables_3V_sf_fusion$sum_visitors[(cables_3V_sf_fusion$name=="Jardin d'Enfants" & cables_3V_sf_fusion$aerialway=="platter")]<-lesmenuires_human_traffic_sum$sum_visitors[lesmenuires_human_traffic_sum$cable=="Jardin d'enfants"]
# cables_3V_sf_fusion$resort[(cables_3V_sf_fusion$name=="Jardin d'Enfants" & cables_3V_sf_fusion$aerialway=="platter")]<-lesmenuires_human_traffic_sum$resort[lesmenuires_human_traffic_sum$cable=="Jardin d'enfants"]



# Missing names : in valtho_human_traffic_sum --> in cables_3V_osm_WGS84_sf : 
# - Orelle --> "Telecabine d'Orelle OK
# - Orelle Caron --> "Telecabine d'Orelle-Caron OK
# - Peclet --> "Funitel Peclet OK
# - Plateau = "Plateau 1" = Plateau 2" OK
# - 3 Vallées --> "VT FU 3 Vallées" = "Funitel 3 Vallees"
# - 2 lacs --> Les 2 Lacs OK
# - Plan eau --> "Plan de l'Eau"
# - Tyrolienne Bee --> Bee Flying OK
# - Cosmojet (sledge trail)

# Missing names : in courch_human_traffic_sum --> in cables_3V_osm_WGS84_sf : 
# - Saulire --> different from "Saulire 1" and "Saulire 2" OK
# - Jardin-Alpin --> "Jardin Alpin" OK
# - Tania --> "La Tania" OK
# - Aiguille Fruit --> "Aiguille du Fruit" OK
# - Dou Lanches --> "Dou des Lanches" OK
# - Jardin Enf. --> "Jardin d'Enfants"  OK
# - Envolee --> ?
# - "Loze" --> "Loze A" = "Loze B" = "Loze Express" OK
# - Pyramides = "Pyramides 1" = "Pyramides 2" OK
# - Source --> "Sources" OK
# - Gros Murger --> "TKD Gros Murger" OK
# - Rocher Ombre --> "Rocher de l'Ombre" OK
# - TK TROIKA --> ?

# Missing names : in lesmenuires_human_traffic_sum --> in cables_3V_osm_WGS84_sf : 
# - Biolley --> Biolley 1 and Biolley 2 OK
# - Jardin d'enfants --> Jardin d'Enfants ; Attention same name in Couchevel OK
# - Preyerand.1 --> Attention Preyerand and Preyerand.1 = two different ski lifts named "Preyerand" OK 
# - ROC'N BOB --> sledding trail
# - Speed Mountain --> sledding trail
# - Bruyeres1 --> Bruyeres 1 OK
# - Bruyeres2 --> Bruyeres 2 OK
# - Masse1 --> "Masse 1" OK
# - Saint Martin --> "Saint Martin 1" OK
# - Reberty.1 --> Attention Reberty and Reberty.1 = two different ski lifts named "Reberty" OK
#*************



#### 1.3_Creation of the ski_lift_visitor_nb raster by binding visitor numbers to each ski lift ----
#*************
#### Settings
# season_of_interest = unique(ski_lift_visitor_nb$ski_season)
season_of_interest = "2017_2018"
####


# bind dataset all 3V 
ski_lift_visitor_nb <- bind_rows(meribel_nb,courch_nb,valtho_nb,menui_nb)
ski_lift_visitor_nb <- ski_lift_visitor_nb %>% filter(ski_season %in% season_of_interest)

# merging sum visitors and osm raster --> spatialize the information 
cables_3V_sf_fusion <- dplyr::left_join(cables_3V_osm_sf,ski_lift_visitor_nb,by = c("name" = "ski_lift"),copy=TRUE)

# creation vector with ski lift traffic
cables_visitors <- vect(cables_3V_sf_fusion)
# saving the vector 
writeVector(x=cables_visitors, filename=file.path(base, "TetrAlps","2_DATA", "ski_resorts_visitor_numbers", paste0("cables_visitors_2024_11_", season_of_interest,".gpkg")),overwrite=TRUE)
#********************************************************************






















#### 2_Visualizing data ----
#********************************************************************
#### 2.1_Visualizing osm ski trails in the Trois Vallées ski resort 

table(cables_3V_osm_WGS84_sf$aerialway)

osm_winter_sf$piste.type <- factor(osm_winter_sf$piste.type, levels = c("downhill", "nordic", "skitour","hike","connection","NA"))

png(filename = paste0(base, "/5_OUTPUTS/human_traffic.png"),height = 4200,width = 4600,res=300) # Naming files correctly
ggplot()+
  geom_sf(data=osm_winter_sf,aes(geometry=geometry,color=piste.type))+
  geom_sf(data = borders_3V_vect,fill=NA,color="black",lwd =2)+
  annotation_north_arrow(height = unit(0.6, "cm"), width = unit(0.6, "cm"),pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),)+
  annotation_scale(width_hint = 0.125,pad_x = unit(1, "cm")) +
  scale_color_manual(values=c("orange","blue","turquoise","green","red","grey"))+
  labs( title="Human traffic in winter (Open Street Map data)",
        x = "Longitude",
        y = "Latitude",
        color = "Trail type")+
  theme_classic()
dev.off()


#### 2.2_Visualizing osm ski trails overlapping strava ski trails in the Trois Vallées ski resort 

png(filename = paste0(base, "/5_OUTPUTS/winter_trails.png"),height = 4200,width = 4600,res=300) # Naming files correctly
par(oma = c(1,1,1,1))
plot(strava,ext=e,
     # col=colorRampPalette(c("#333333","#FF6633","#CCCCCC11"),alpha=T)(25),
     col=colorRampPalette(c("#CCCCCC11","#FF6600","#FF3333"),alpha=T)(25),
     main="Winter trails in the Trois Vallées ski resort",
     xlab="Longitude",
     ylab="Latitude",
     cex.main=2,
     cex.lab = 1.5,
     plg = list(title = "Strava users \ntrafic \n(intensity)",title.cex = 1.5,cex=1.2))
plot(borders_3V_vect_WGS84,add=T,border="black",lwd=2)
plot(osm_winter,color=osm_winter$piste.type,color="black",add=TRUE)
dev.off()


#### 2.3_Visualizing osm ski cables in the Trois Vallées ski resort 

png(filename = paste0(base, "/5_OUTPUTS/ski_lifts.png"),height = 3200,width = 4600,res=300) # Naming files correctly
ggplot()+
  geom_sf(data=cables_3V_osm_WGS84 ,aes(geometry=geometry,color=aerialway))+
  geom_sf(data = borders_3V_vect_WGS84,fill=NA,color="black",lwd =2)+
  annotation_north_arrow(height = unit(0.6, "cm"), width = unit(0.6, "cm"),pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),)+
  annotation_scale(width_hint = 0.125,pad_x = unit(1, "cm")) +
  scale_color_manual(values=c("deeppink","blue","green","red","grey","turquoise","darkorange","black","darkgreen"))+
  labs( title="Cables in the Trois Vallées ski resort (Open Street Map data)",
        x = "Longitude",
        y = "Latitude",
        color = "Ski lift type")+
  guides(color = guide_legend(override.aes = list(size = 2)))+
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
  theme_classic()
dev.off()


#### 2.4_Visualizing osm ski cables overlapping strava ski cables in the Trois Vallées ski resort 

plot(strava,ext=e,
     # col=colorRampPalette(c("#333333","#FF6633","#CCCCCC11"),alpha=T)(25),
     col=colorRampPalette(c("#CCCCCC11","#FF6600","#FF3333"),alpha=T)(25),
     main="Winter trails in the Trois Vallées ski resort",
     xlab="Longitude",
     ylab="Latitude",
     cex.main=2,
     cex.lab = 1.5,
     plg = list(title = "Strava users \ntrafic \n(intensity)",title.cex = 1.5,cex=1.2))
plot(borders_3V_vect_WGS84,add=T,border="black",lwd=2)
# plot(borders_3V_vect_WGS84,border=as.factor(borders_3V_vect_WGS84$NOM),lwd=2)
plot(cables_3V_osm_WGS84,color="black",add=TRUE)



#### 2.5_Vizualizing the final raster ----
png(filename = paste0(base, "/5_OUTPUTS/ski_lift_traffic.png"),height = 4200,width = 4600,res=300) # Naming files correctly
par(oma = c(1,1,1,1))
ggplot()+
  geom_sf(data=cables_3V_traffic ,aes(geometry=geometry,color=sum_visitors))+
  scale_color_gradient(low = "yellow", high = "red",na.value="grey",n.breaks=6)+
  new_scale_color()+
  geom_sf(data = borders_3V_vect_WGS84,fill=NA,aes(color=as.factor(borders_3V_vect_WGS84$NOM)),lwd =1.3)+
  scale_color_manual(values=c("#0033CC","#0099FF", "#99FFFF"))+
  annotation_north_arrow(height = unit(0.6, "cm"), width = unit(0.6, "cm"),pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),)+
  annotation_scale(width_hint = 0.125,pad_x = unit(1, "cm")) +
  labs( title="Ski lift traffic in the Trois Vallées ski area (Open Street Map data)",
        x = "Longitude",
        y = "Latitude",
        color = "Number of winter visitors\n2022-2023")+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(plot.title = element_text(size=22, face="bold"),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.background = element_rect(fill = '#666666'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank())
  
dev.off()
#********************************************************************

