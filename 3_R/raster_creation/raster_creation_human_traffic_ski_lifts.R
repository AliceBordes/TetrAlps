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
library(ggnewscale)
#********************************************************************


# ---- Settings ---- 
#********************************************************************
base<-here()
output_folder_zone<-file.path("C:/Users/albordes/Documents/PhD/TetrAlps/","2_DATA")
#********************************************************************


# load(file.path(base,'myRasterEnvironment.RData'))

# ---- Loading data ---- 
#********************************************************************
### VECTORS

# 3V borders 
borders_3V_vect <- terra::vect(paste0(base,"/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect_WGS84<- project(borders_3V_vect,y="+proj=longlat +datum=WGS84")

# 3V winter trails (osm)
osm_winter<-vect(file.path(output_folder_zone, "osm_ski_piste.gpkg"))
osm_winter_sf<-as_sf(osm_winter)


# 3V ski cables from Open street Map
cables_3V_osm <- vect(file.path(output_folder_zone, "ski_lift_osm.gpkg"))
cables_3V_osm_WGS84<- project(cables_3V_osm,y="+proj=longlat +datum=WGS84")
cables_3V_osm_WGS84_sf<-as_sf(cables_3V_osm_WGS84)
cables_3V_osm_WGS84_sf$name<-stri_trans_general(cables_3V_osm_WGS84_sf$name, id = "Latin-ASCII")  # delete accents

# 3V cables from OGM (Marc Montadert), NO NAMES 
cables_3V_no_id <- terra::vect(paste0(base,"/1_RAW_DATA/human_traffic_in_ski_resorts/cables/cables.gpkg"))
cables_3V_no_id_WGS84<- project(cables_3V_no_id,y="+proj=longlat +datum=WGS84")

# 3V human traffic
  # Méribel Mottaret
meribel_human_traffic<-read_excel(sheet="Passage Détail",file.path(base,"1_RAW_DATA/human_traffic_in_ski_resorts/Meribel_mottaret_RM_Historique_des_passages_2017_22/Passage Mensuel par RM Mottaret 22-23.xlsx"))
meribel_human_traffic<-meribel_human_traffic %>% fill(Mois) # `fill()` defaults to replacing missing data from top to bottom
meribel_human_traffic<-meribel_human_traffic %>% replace(is.na(.), 0) # assuming all the NA = 0 visitors

  # Val Thorens
valtho_human_traffic<-read_excel(sheet="2022-23",file.path(base,"1_RAW_DATA/human_traffic_in_ski_resorts/ValThorens_Orelle_RM_Historique_des_passages_2017_22.xlsx"))

  # Les Ménuires
lesmenuires_human_traffic<-read_excel(skip=0,sheet="2022-2023",file.path(base,"1_RAW_DATA/human_traffic_in_ski_resorts/Les_Menuires_RM_Historique_des_passages_2017_22.xlsx"))
lesmenuires_human_traffic<-lesmenuires_human_traffic[-2,]
lesmenuires_human_traffic<-lesmenuires_human_traffic[,-3]
lesmenuires_human_traffic<-as.data.frame(t(lesmenuires_human_traffic))
colnames(lesmenuires_human_traffic)<-c("Date",lesmenuires_human_traffic[2,2:ncol(lesmenuires_human_traffic)])
lesmenuires_human_traffic<-lesmenuires_human_traffic %>% drop_na(Date) # drop rows if there is no date associated with
lesmenuires_human_traffic <- lesmenuires_human_traffic[,colSums(is.na(lesmenuires_human_traffic))<nrow(lesmenuires_human_traffic)] # delete columns with only NA
lesmenuires_human_traffic<-lesmenuires_human_traffic[,-c(ncol(lesmenuires_human_traffic)-1,ncol(lesmenuires_human_traffic))]
lesmenuires_human_traffic<-lesmenuires_human_traffic[-c(nrow(lesmenuires_human_traffic)-1,nrow(lesmenuires_human_traffic)),]
# Convert number of days to date format
lesmenuires_human_traffic$Date <- as.Date(as.numeric(lesmenuires_human_traffic$Date), origin = "1899-12-30")
lesmenuires_human_traffic[,2:ncol(lesmenuires_human_traffic)]<-lapply(lesmenuires_human_traffic[, 2:ncol(lesmenuires_human_traffic)], as.numeric)

  # Courchevel
courch_human_traffic<-read_excel(sheet="Details",file.path(base,"1_RAW_DATA/human_traffic_in_ski_resorts/Courchevel_RM_Historique_des_passages_2017_22/Comptages_Passages_2022_2023.xlsx"))
colnames(courch_human_traffic)<-c("Day","Date",courch_human_traffic[3,-c(1,2)])
courch_human_traffic <- courch_human_traffic %>% filter(Day %in% c("lun.","mar.","mer.","jeu.","ven.","sam.","dim."))
# Convert number of days to date format
courch_human_traffic$Date <- as.Date(as.numeric(courch_human_traffic$Date), origin = "1899-12-30")
# Format the date as dd/mm/yyyy
# courch_human_traffic$Date <- format(date, "%d/%m/%Y")
courch_human_traffic[,3:ncol(courch_human_traffic)]<-lapply(courch_human_traffic[, 3:ncol(courch_human_traffic)], as.numeric)

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








#### 1_Calculate human traffic ----
#********************************************************************
#Méribel
# sum
meribel_human_traffic_sum <- apply(meribel_human_traffic[,3:ncol(meribel_human_traffic)],2,sum,na.rm=TRUE) 

# Union of cables_3V_osm_WGS84_sf and meribel_human_traffic_sum dataset
meribel_human_traffic_sum<-data.frame("cable"=names(meribel_human_traffic_sum),"sum_visitors"=meribel_human_traffic_sum,"resort"=rep("Méribel-Mottaret",length(meribel_human_traffic_sum)))
meribel_human_traffic_sum$cable<-stri_trans_general(meribel_human_traffic_sum$cable, id = "Latin-ASCII")  # delete accents

find.list.mermott <- list("MO TC ", "MO TK ","MO TSD ")
# in REGEX, | also represents "OR"
find.string.mermott  <- paste(unlist(find.list.mermott ), collapse = "|")

meribel_human_traffic_sum$cable <- gsub(find.string.mermott ,"",meribel_human_traffic_sum$cable) 






# Val Thorens
# sum
# valtho_human_traffic_sum <- apply(valtho_human_traffic[1:(nrow(valtho_human_traffic)-1),4:ncol(valtho_human_traffic)],1,sum,na.rm=T) 
valtho_human_traffic_sum<-as.data.frame(valtho_human_traffic[2:(nrow(valtho_human_traffic)-1),c(2,3)])
colnames(valtho_human_traffic_sum)<-c("Nom de la RM","Total")

# Union of cables_3V_osm_WGS84_sf and meribel_human_traffic_sum dataset
valtho_human_traffic_sum<-data.frame("cable"=valtho_human_traffic_sum$`Nom de la RM`,"sum_visitors"=as.numeric(valtho_human_traffic_sum$Total),"resort"=rep("Val Thorens-Orelle",nrow(valtho_human_traffic_sum)))
rownames(valtho_human_traffic_sum)<-valtho_human_traffic_sum$"cable"
valtho_human_traffic_sum$cable<-stri_trans_general(valtho_human_traffic_sum$cable, id = "Latin-ASCII")  # delete accents

find.list.valtho <- list("OR TC ","OR TS ","OR TSD ", "VT FU ","VT TC ","VT TK ","VT TSD ","VT ","TPH ")
# in REGEX, | also represents "OR"
find.string.valtho <- paste(unlist(find.list.valtho), collapse = "|")

valtho_human_traffic_sum$cable <- gsub(find.string.valtho,"",valtho_human_traffic_sum$cable) 





# Corrections 

# Calcul visitor number of OR TC Orelle Caron + OR TC Caron (descente) --> only a black trail to downhill from Cime Caron to Orelle slope
valtho_human_traffic_sum$sum_visitors[valtho_human_traffic_sum$cable=="Orelle Caron"] <- (valtho_human_traffic_sum$sum_visitors[valtho_human_traffic_sum$cable=="Orelle Caron"] + valtho_human_traffic_sum$sum_visitors[valtho_human_traffic_sum$cable=="Caron (descente)"])
valtho_human_traffic_sum<-valtho_human_traffic_sum[-(valtho_human_traffic_sum$cable=="Caron (descente)"),]
# In valtho_human_traffic_sum Plateau --> "Plateau 1" = Plateau 2" in cables_3V_osm_WGS84_sf
valtho_human_traffic_sum$cable[valtho_human_traffic_sum$cable=="Plateau"] <- "Plateau 1"
# In valtho_human_traffic_sum 2 lacs --> "Les 2 lacs" in cables_3V_osm_WGS84_sf
valtho_human_traffic_sum$cable[valtho_human_traffic_sum$cable=="2 Lacs"] <- "Les 2 Lacs"
# In valtho_human_traffic_sum Tyrolienne Bee --> "Bee Flying" in cables_3V_osm_WGS84_sf
valtho_human_traffic_sum$cable[valtho_human_traffic_sum$cable=="Tyrolienne Bee"] <- "Bee Flying"
# In valtho_human_traffic_sum Peclet --> "Funitel Peclet" in cables_3V_osm_WGS84_sf
valtho_human_traffic_sum$cable[valtho_human_traffic_sum$cable=="Peclet"] <- "Funitel Peclet"
# In valtho_human_traffic_sum Orelle Caron --> "Telecabine d'Orelle-Caron" in cables_3V_osm_WGS84_sf
valtho_human_traffic_sum$cable[valtho_human_traffic_sum$cable=="Orelle Caron"] <- "Telecabine d'Orelle-Caron"
# In valtho_human_traffic_sum Orelle --> "Telecabine d'Orelle" in cables_3V_osm_WGS84_sf
valtho_human_traffic_sum$cable[valtho_human_traffic_sum$cable=="Orelle"] <- "Telecabine d'Orelle"
# In valtho_human_traffic_sum Orelle --> "Telecabine d'Orelle" in cables_3V_osm_WGS84_sf
valtho_human_traffic_sum$cable[row.names(valtho_human_traffic_sum)=="VT FU 3 Vallées"] <- "Funitel 3 Vallees"
# In valtho_human_traffic_sum Plan eau --> "Plan de l'Eau" in cables_3V_osm_WGS84_sf
valtho_human_traffic_sum$cable[valtho_human_traffic_sum$cable=="Plan Eau"] <- "Plan de l'Eau"


#Les Ménuires
# sum
lesmenuires_human_traffic_sum <- apply(lesmenuires_human_traffic[,2:ncol(lesmenuires_human_traffic)],2,sum,na.rm=TRUE) 

# Union of cables_3V_osm_WGS84_sf and meribel_human_traffic_sum dataset
lesmenuires_human_traffic_sum<-data.frame("cable"=names(lesmenuires_human_traffic_sum),"sum_visitors"=lesmenuires_human_traffic_sum,"resort"=rep("Les Ménuires",length(lesmenuires_human_traffic_sum)))
lesmenuires_human_traffic_sum$cable<-stri_trans_general(lesmenuires_human_traffic_sum$cable, id = "Latin-ASCII")  # delete accents

# Corrections 

# In lesmenuires_human_traffic_sum Biolley --> "Biolley 1" and "Biolley 2" in cables_3V_osm_WGS84_sf
lesmenuires_human_traffic_sum$cable[lesmenuires_human_traffic_sum$cable=="Biolley"] <- "Biolley 1"
dt<-data.frame("cable"="Biolley 2","sum_visitors"=lesmenuires_human_traffic_sum$sum_visitors[lesmenuires_human_traffic_sum$cable=="Biolley 1"],"resort"="Les Ménuires")
lesmenuires_human_traffic_sum<-bind_rows(lesmenuires_human_traffic_sum,dt)
# In lesmenuires_human_traffic_sum Masse1 --> "Masse 1" in cables_3V_osm_WGS84_sf
lesmenuires_human_traffic_sum$cable[lesmenuires_human_traffic_sum$cable=="Masse1"] <- "Masse 1"
# In lesmenuires_human_traffic_sum Saint Martin --> "Saint Martin 1" in cables_3V_osm_WGS84_sf
lesmenuires_human_traffic_sum$cable[lesmenuires_human_traffic_sum$cable=="Saint Martin"] <- "Saint Martin 1"
# In lesmenuires_human_traffic_sum Bruyeres1 --> "Bruyeres 1" in cables_3V_osm_WGS84_sf ; idem for Bruyeres2
lesmenuires_human_traffic_sum$cable[lesmenuires_human_traffic_sum$cable=="Bruyeres1"] <- "Bruyeres 1"
lesmenuires_human_traffic_sum$cable[lesmenuires_human_traffic_sum$cable=="Bruyeres2"] <- "Bruyeres 2"





# Courchevel

courch_human_traffic_sum<-as.data.frame(colSums(courch_human_traffic[,3:(ncol(courch_human_traffic)-2)], na.rm = TRUE))
courch_human_traffic_sum<-data.frame("cable"=row.names(courch_human_traffic_sum),"sum_visitors"=as.vector(colSums(courch_human_traffic[,3:(ncol(courch_human_traffic)-2)], na.rm = TRUE)),"resort"=rep("Courchevel",nrow(courch_human_traffic_sum)))
rownames(courch_human_traffic_sum)<-courch_human_traffic_sum$cable

# Union of cables_3V_osm_WGS84_sf and courch_human_traffic_sum dataset
courch_human_traffic_sum$cable<-stri_trans_general(courch_human_traffic_sum$cable, id = "Latin-ASCII")  # delete accents

find.list.courch <- list("TK ","TC ","TS ","TB ","CO TK ","CO TC ","CO TS ","CO TSD ","CO TPH ")
# in REGEX, | also represents "OR"
find.string.courch <- paste(unlist(find.list.courch), collapse = "|")

courch_human_traffic_sum$cable <- str_to_title(gsub(find.string.courch,"",courch_human_traffic_sum$cable))


# Corrections 

# Calcul visitor number of OR TC Orelle Caron + OR TC Caron (descente) --> only a black trail to downhill from Cime Caron to Orelle slope
courch_human_traffic_sum$cable[courch_human_traffic_sum$cable=="Jardin-Alpin"] <- "Jardin Alpin"
courch_human_traffic_sum$cable[courch_human_traffic_sum$cable=="Tania"] <- "La Tania"
courch_human_traffic_sum$cable[courch_human_traffic_sum$cable=="Aiguille Fruit"] <- "Aiguille du Fruit"
courch_human_traffic_sum$cable[courch_human_traffic_sum$cable=="Dou Lanches"] <- "Dou des Lanches"
courch_human_traffic_sum$cable[courch_human_traffic_sum$cable=="Jardin Enf."] <- "Jardin d'Enfants"
courch_human_traffic_sum$cable[courch_human_traffic_sum$cable=="Loze"] <- "Loze Express"
courch_human_traffic_sum$cable[courch_human_traffic_sum$cable=="Pyramides"] <- "Pyramides 1"
courch_human_traffic_sum$cable[courch_human_traffic_sum$cable=="Source"] <- "Sources"
courch_human_traffic_sum$cable[courch_human_traffic_sum$cable=="Gros Murger"] <- "TKD Gros Murger"
courch_human_traffic_sum$cable[courch_human_traffic_sum$cable=="Rocher Ombre"] <- "Rocher de l'Ombre"



# bind dataset all 3V 
human_traffic_sum_3V <- bind_rows(meribel_human_traffic_sum,valtho_human_traffic_sum,courch_human_traffic_sum,lesmenuires_human_traffic_sum)

# merging sum visitors and osm raster --> spatialize the information 
cables_3V_sf_fusion<- dplyr::left_join(cables_3V_osm_WGS84_sf,human_traffic_sum_3V,by = c("name" = "cable"),copy=TRUE)

# correction when there is a same ski lift name in two different resort
cables_3V_sf_fusion$sum_visitors[(cables_3V_sf_fusion$name=="Jardin d'Enfants" & cables_3V_sf_fusion$aerialway=="gondola")]<-courch_human_traffic_sum$sum_visitors[courch_human_traffic_sum$cable=="Jardin d'Enfants"]
cables_3V_sf_fusion$resort[(cables_3V_sf_fusion$name=="Jardin d'Enfants" & cables_3V_sf_fusion$aerialway=="gondola")]<-courch_human_traffic_sum$resort[courch_human_traffic_sum$cable=="Jardin d'Enfants"]
cables_3V_sf_fusion$sum_visitors[(cables_3V_sf_fusion$name=="Jardin d'Enfants" & cables_3V_sf_fusion$aerialway=="platter")]<-lesmenuires_human_traffic_sum$sum_visitors[lesmenuires_human_traffic_sum$cable=="Jardin d'enfants"]
cables_3V_sf_fusion$resort[(cables_3V_sf_fusion$name=="Jardin d'Enfants" & cables_3V_sf_fusion$aerialway=="platter")]<-lesmenuires_human_traffic_sum$resort[lesmenuires_human_traffic_sum$cable=="Jardin d'enfants"]

cables_3V_sf_fusion$sum_visitors[(cables_3V_sf_fusion$name=="Preyerand" & cables_3V_sf_fusion$aerialway=="magic_carpet")]<-lesmenuires_human_traffic_sum$sum_visitors[lesmenuires_human_traffic_sum$cable=="Preyerand"]
cables_3V_sf_fusion$sum_visitors[(cables_3V_sf_fusion$name=="Preyerand" & cables_3V_sf_fusion$aerialway=="cable_car")]<-lesmenuires_human_traffic_sum$sum_visitors[lesmenuires_human_traffic_sum$cable=="Preyerand.1"]

cables_3V_sf_fusion$sum_visitors[(cables_3V_sf_fusion$name=="Reberty" & cables_3V_sf_fusion$aerialway=="magic_carpet")]<-lesmenuires_human_traffic_sum$sum_visitors[lesmenuires_human_traffic_sum$cable=="Reberty"]
cables_3V_sf_fusion$sum_visitors[(cables_3V_sf_fusion$name=="Reberty" & cables_3V_sf_fusion$aerialway=="chair_lift")]<-lesmenuires_human_traffic_sum$sum_visitors[lesmenuires_human_traffic_sum$cable=="Reberty.1"]



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


# creation vector with ski lift traffic
cables_3V_traffic<-vect(cables_3V_sf_fusion)
# saving the vector 
writeVector(x=cables_3V_traffic, filename=file.path(output_folder_zone,"ski_lift_traffic_3V.gpkg"),overwrite=TRUE)


# Loading 3V ski lift traffic 
cables_3V_traffic <- terra::vect(file.path(base,"2_DATA/ski_lift_traffic_3V.gpkg"))
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

