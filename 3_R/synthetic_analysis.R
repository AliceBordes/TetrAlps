#### PhD Tetras_test project ####

# Alice Bordes #

# November 2023 #

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
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
#********************************************************************


### Loading data
#********************************************************************
# GPS locations of black grouses
synth_bg_all_sites<-read.csv2(paste0(base,"/1_RAW_DATA/bilan_captures_tetras_all_sites_feb2024.csv"),sep=",")[,-1]
#********************************************************************



### Settings
#********************************************************************
base<-here()

# chosing sites
#********************************************************************
synth_bg_all_sites<- synth_bg_all_sites %>% filter(zone_etude %in% c("foret_blanche", "le_fournel" , "les_arcs", "trois_vallees", "valcenis"))
#********************************************************************




#### 1_Data description ####

#### 1.1_3V tag type ####

ggplot(data=synth_bg_all_sites,aes(x=marque_tag,fill=energy))+
  geom_bar()+
  scale_fill_manual(breaks=c("batterie","solaire"),labels=c("battery","solar"),values=c("#99CCFF","#FFCC00"))+
  labs(x="GPS type",y="Number of tags", title="GPS-tag characteristics")+
  geom_text(aes(label = ..count..),position=position_stack(vjust = 0.1), stat = "count", vjust = 1.5, colour = "black")+
  theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=12),strip.text.x = element_text(size = 14))+
  facet_wrap(~zone_etude)


#### 1.2_3V sex ratio ####

ggplot(data=synth_bg_all_sites,aes(sexe))+
  geom_bar(aes(fill=sexe),show.legend = FALSE)+
  scale_fill_manual(breaks=c("","femelle","male"),labels=c("NA","female","male"),values=c("grey","#FF9999","#99CCFF"))+
  scale_x_discrete(labels=c("NA", "femelle" = "female","male" = "male"))+
  labs(x="Sex",y="Number of tagged-birds", title="Sex-ratio of tagged-black grouse in the Trois Vallées ski resort")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")+
  theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=12),strip.text.x = element_text(size = 14))+
  facet_wrap(~zone_etude)


# ggplot(data=synth_bg_all_sites,aes(x=age,fill=sexe))+
#   geom_bar(position = position_dodge(width = 0.8))+
#   scale_fill_manual(breaks=c("","femelle","male"),labels=c("NA","female","male"),values=c("grey","#FF9999","#99CCFF"))+
#   scale_x_discrete(labels=c("NA", "femelle" = "female","male" = "male"))+
#   labs(x="Sex",y="Number of tagged-birds", title="Sex-ratio of tagged-black grouse in the Trois Vallées ski resort")+
#   geom_text(aes(label = ..count..),position=position_dodge(width=.8), stat = "count", vjust = 1.5, colour = "black")+
#   theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=12))

ggplot(data=synth_bg_all_sites,aes(x=age,fill=sexe))+
  geom_bar()+
  scale_fill_manual(breaks=c("","femelle","male"),labels=c("NA","female","male"),values=c("grey","#FF9999","#99CCFF"))+
  scale_x_discrete(labels=c("adulte"="adult","immature"="immature", "indetermine" = "indetermined"))+
  labs(x="Sex",y="Number of tagged-birds", title="Sex-ratio of tagged-black grouse in the Trois Vallées ski resort")+
  geom_text(aes(label = ..count..),position=position_stack(vjust = 0.1), stat = "count", vjust = 0, colour = "black")+
  theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=12),strip.text.x = element_text(size = 14))+
  facet_wrap(~zone_etude)

ggplot(data=synth_bg_all_sites,aes(x=sexe,fill=age))+
  geom_bar()+
  scale_fill_manual(breaks=c("adulte","immature","indetermine"),labels=c("adult","immature","indetermined"),values=c("#CC3333","#FF99CC","grey"))+
  scale_x_discrete(labels=c("NA", "femelle" = "female","male" = "male"))+
  labs(x="Sex",y="Number of tagged-birds", title="Sex-ratio of tagged-black grouse in the Trois Vallées ski resort")+
  geom_text(aes(label = ..count..),position=position_stack(vjust = 0.1), stat = "count", vjust = 0, colour = "black")+
  theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=12),strip.text.x = element_text(size = 14))+
  facet_wrap(~zone_etude)















