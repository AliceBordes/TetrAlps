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
library(camtrapR)
library(overlap)
#********************************************************************


### Loading functions
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
#********************************************************************

### Settings
#********************************************************************
base<-here()


### Loading data
#********************************************************************
# Main characteristics of tegged-black grouse
synth_bg_all_sites<-read.csv2(paste0(base,"/1_RAW_DATA/bilan_captures_tetras_all_sites_feb2024.csv"),sep=",")[,-1]
# GPS locations of black grouse
data_bg_3V<-readRDS(paste0(base,"/1_RAW_DATA/tot.ind.trois_vallees2.rds"))
#********************************************************************



# chosing sites
#********************************************************************
synth_bg_all_sites<- synth_bg_all_sites %>% filter(zone_etude %in% c("foret_blanche", "le_fournel" , "les_arcs", "trois_vallees", "valcenis"))
#********************************************************************



#### 1_Data description ####

#### 1.1_3V tag type ####

#total
g_GPS_tag_tot<-
  ggplot(data=synth_bg_all_sites,aes(x=marque_tag,fill=energy))+
  geom_bar()+
  scale_fill_manual(breaks=c("batterie","solaire"),labels=c("battery","solar"),values=c("#99CCFF","#FFCC00"))+
  labs(x="GPS type",y="Number of tags", title="GPS-tag characteristics")+
  geom_text(aes(label = ..count..),position=position_stack(vjust = 0.1), stat = "count", vjust = 1.5, colour = "black")+
  theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=12),strip.text.x = element_text(size = 14))

g_GPS_tag_tot
# ggsave(plot=g_GPS_tag_tot,file=paste0(base,"/6_FIGURES/data_exploration/","g_GPS_tags_total_",format(Sys.time(), "%d.%b%Y"),".png"))

#by site 
g_GPS_tag_by_site<-
  ggplot(data=synth_bg_all_sites,aes(x=marque_tag,fill=energy))+
    geom_bar()+
    scale_fill_manual(breaks=c("batterie","solaire"),labels=c("battery","solar"),values=c("#99CCFF","#FFCC00"))+
    labs(x="GPS type",y="Number of tags", title="GPS-tag characteristics")+
    geom_text(aes(label = ..count..),position=position_stack(vjust = 0.1), stat = "count", vjust = 1.5, colour = "black")+
    theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=12),strip.text.x = element_text(size = 14))+
    facet_wrap(~zone_etude)

g_GPS_tag_by_site
# ggsave(plot=g_GPS_tag_by_site,file=paste0(base,"/6_FIGURES/data_exploration/","g_GPS_tags_by_site_",format(Sys.time(), "%d.%b%Y"),".png"))

#### 1.2_3V sex ratio ####

#total 
sex_ratio_tags_tot<-
  ggplot(data=synth_bg_all_sites,aes(sexe))+
    geom_bar(aes(fill=sexe),show.legend = FALSE)+
    scale_fill_manual(breaks=c("","femelle","male"),labels=c("NA","female","male"),values=c("grey","#FF9999","#99CCFF"))+
    scale_x_discrete(labels=c("NA", "femelle" = "female","male" = "male"))+
    labs(x="Sex",y="Number of tagged-birds", title="Sex-ratio of tagged-black grouse in the Trois Vallées ski resort")+
    geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")+
    theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=11),strip.text.x = element_text(size = 14))

sex_ratio_tags_tot
# ggsave(plot=sex_ratio_tags_tot,file=paste0(base,"/6_FIGURES/data_exploration/","sex_ratio_tags_tot_",format(Sys.time(), "%d.%b%Y"),".png"))

#by sites 
sex_ratio_tags_by_site<-
  ggplot(data=synth_bg_all_sites,aes(sexe))+
  geom_bar(aes(fill=sexe),show.legend = FALSE)+
  scale_fill_manual(breaks=c("","femelle","male"),labels=c("NA","female","male"),values=c("grey","#FF9999","#99CCFF"))+
  scale_x_discrete(labels=c("NA", "femelle" = "female","male" = "male"))+
  labs(x="Sex",y="Number of tagged-birds", title="Sex-ratio of tagged-black grouse in the Trois Vallées ski resort")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")+
  theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=11),strip.text.x = element_text(size = 14))+
  facet_wrap(~zone_etude)

sex_ratio_tags_by_site
# ggsave(plot=sex_ratio_tags_by_site,file=paste0(base,"/6_FIGURES/data_exploration/","sex_ratio_tags_by_site_",format(Sys.time(), "%d.%b%Y"),".png"))


# ggplot(data=synth_bg_all_sites,aes(x=age,fill=sexe))+
#   geom_bar(position = position_dodge(width = 0.8))+
#   scale_fill_manual(breaks=c("","femelle","male"),labels=c("NA","female","male"),values=c("grey","#FF9999","#99CCFF"))+
#   scale_x_discrete(labels=c("NA", "femelle" = "female","male" = "male"))+
#   labs(x="Sex",y="Number of tagged-birds", title="Sex-ratio of tagged-black grouse in the Trois Vallées ski resort")+
#   geom_text(aes(label = ..count..),position=position_dodge(width=.8), stat = "count", vjust = 1.5, colour = "black")+
#   theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=12))

#total by age
sex_ratio_tags_age_tot<-
  ggplot(data=synth_bg_all_sites,aes(x=age,fill=sexe))+
    geom_bar()+
    scale_fill_manual(breaks=c("","femelle","male"),labels=c("NA","female","male"),values=c("grey","#FF9999","#99CCFF"))+
    scale_x_discrete(labels=c("adulte"="adult","immature"="immature", "indetermine" = "indetermined"))+
    labs(x="Sex",y="Number of tagged-birds", title="Sex-ratio of tagged-black grouse in the Trois Vallées ski resort")+
    geom_text(aes(label = ..count..),position=position_stack(vjust = 0.1), stat = "count", vjust = 0, colour = "black")+
    theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=11),strip.text.x = element_text(size = 14))

sex_ratio_tags_age_tot
# ggsave(plot=sex_ratio_tags_age_tot,file=paste0(base,"/6_FIGURES/data_exploration/","aged_sex_ratio_tags_tot_",format(Sys.time(), "%d.%b%Y"),".png"))

#by site by age
sex_ratio_tags_age_by_site<-
  ggplot(data=synth_bg_all_sites,aes(x=age,fill=sexe))+
    geom_bar()+
    scale_fill_manual(breaks=c("","femelle","male"),labels=c("NA","female","male"),values=c("grey","#FF9999","#99CCFF"))+
    scale_x_discrete(labels=c("adulte"="adult","immature"="immature", "indetermine" = "indetermined"))+
    labs(x="Sex",y="Number of tagged-birds", title="Sex-ratio of tagged-black grouse in the Trois Vallées ski resort")+
    geom_text(aes(label = ..count..),position=position_stack(vjust = 0.1), stat = "count", vjust = 0, colour = "black")+
    theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=11),strip.text.x = element_text(size = 14))+
    facet_wrap(~zone_etude)

sex_ratio_tags_age_by_site
# ggsave(plot=sex_ratio_tags_age_by_site,file=paste0(base,"/6_FIGURES/data_exploration/","aged_sex_ratio_tags_by_site_",format(Sys.time(), "%d.%b%Y"),".png"))


#tot by sex
sex_ratio_tags_age_tot2<-
  ggplot(data=synth_bg_all_sites,aes(x=sexe,fill=age))+
  geom_bar()+
  scale_fill_manual(breaks=c("adulte","immature","indetermine"),labels=c("adult","immature","indetermined"),values=c("#CC3333","#FF99CC","grey"))+
  scale_x_discrete(labels=c("NA", "femelle" = "female","male" = "male"))+
  labs(x="Sex",y="Number of tagged-birds", title="Sex-ratio of tagged-black grouse in the Trois Vallées ski resort")+
  geom_text(aes(label = ..count..),position=position_stack(vjust = 0.1), stat = "count", vjust = 0, colour = "black")+
  theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=11),strip.text.x = element_text(size = 14))

sex_ratio_tags_age_tot2
# ggsave(plot=sex_ratio_tags_age_tot2,file=paste0(base,"/6_FIGURES/data_exploration/","aged_sex_ratio_tags_tot_2_",format(Sys.time(), "%d.%b%Y"),".png"))


#by site by sex
sex_ratio_tags_age_by_site2<-
  ggplot(data=synth_bg_all_sites,aes(x=sexe,fill=age))+
    geom_bar()+
    scale_fill_manual(breaks=c("adulte","immature","indetermine"),labels=c("adult","immature","indetermined"),values=c("#CC3333","#FF99CC","grey"))+
    scale_x_discrete(labels=c("NA", "femelle" = "female","male" = "male"))+
    labs(x="Sex",y="Number of tagged-birds", title="Sex-ratio of tagged-black grouse in the Trois Vallées ski resort")+
    geom_text(aes(label = ..count..),position=position_stack(vjust = 0.1), stat = "count", vjust = 0, colour = "black")+
    theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=11),strip.text.x = element_text(size = 14))+
    facet_wrap(~zone_etude)

sex_ratio_tags_age_by_site2
# ggsave(plot=sex_ratio_tags_age_by_site2,file=paste0(base,"/6_FIGURES/data_exploration/","aged_sex_ratio_tags_by_site_2_",format(Sys.time(), "%d.%b%Y"),".png"))







#### 1.2_Flights characteristics ####

data_bg_3V_synth_fusion_winter <- dplyr::left_join(data_bg_3V%>% filter(saison=="hiver"),synth_bg_all_sites %>% filter(zone_etude=="trois_vallees") %>% select(tag_id,marque_tag,energy,sexe,age), by="tag_id")



#### 1.2.1_Vizualization of the density of flight during winter #### 

# data formating 

data_bg_3V_synth_fusion_winter <- data_bg_3V_synth_fusion_winter %>%  separate(jour, sep="-", into = c("year", "month", "day"),remove=F)
data_bg_3V_synth_fusion_winter$year <- "2000"

for (i in 1:nrow(data_bg_3V_synth_fusion_winter))
{
  if((as.Date(data_bg_3V_synth_fusion_winter$date[i])>= as.Date('2017-10-01'))&(as.Date(data_bg_3V_synth_fusion_winter$date[i])<= as.Date('2018-04-01')))
  {
    data_bg_3V_synth_fusion_winter$period[i]<-"winter_2017-2018"
  } 
  if((as.Date(data_bg_3V_synth_fusion_winter$date[i])>= as.Date('2018-10-01'))&(as.Date(data_bg_3V_synth_fusion_winter$date[i])<= as.Date('2019-04-01')))
  {
    data_bg_3V_synth_fusion_winter$period[i]<-"winter_2018-2019"
  }
  if((as.Date(data_bg_3V_synth_fusion_winter$date[i])>= as.Date('2019-10-01'))&(as.Date(data_bg_3V_synth_fusion_winter$date[i])<= as.Date('2020-04-01')))
  {
    data_bg_3V_synth_fusion_winter$period[i]<-"winter_2019-2020"
  }
  if((as.Date(data_bg_3V_synth_fusion_winter$date[i])>= as.Date('2020-10-01'))&(as.Date(data_bg_3V_synth_fusion_winter$date[i])<= as.Date('2021-04-01')))
  {
    data_bg_3V_synth_fusion_winter$period[i]<-"winter_2020-2021"
  }
  if((as.Date(data_bg_3V_synth_fusion_winter$date[i])>= as.Date('2021-10-01'))&(as.Date(data_bg_3V_synth_fusion_winter$date[i])<= as.Date('2022-04-01')))
  {
    data_bg_3V_synth_fusion_winter$period[i]<-"winter_2021-2022"
  }
  if((as.Date(data_bg_3V_synth_fusion_winter$date[i])>= as.Date('2022-10-01'))&(as.Date(data_bg_3V_synth_fusion_winter$date[i])<= as.Date('2023-04-01')))
  {
    data_bg_3V_synth_fusion_winter$period[i]<-"winter_2022-2023"
  }
  if((as.Date(data_bg_3V_synth_fusion_winter$date[i])>= as.Date('2023-10-01'))&(as.Date(data_bg_3V_synth_fusion_winter$date[i])<= as.Date('2024-04-01')))
  {
    data_bg_3V_synth_fusion_winter$period[i]<-"winter_2023-2024"
  }
  if(data_bg_3V_synth_fusion_winter$month[i] %in% c("01","02","03"))
  {
    data_bg_3V_synth_fusion_winter$year[i]<-"2001"
  }
  
}

data_bg_3V_synth_fusion_winter <- data_bg_3V_synth_fusion_winter %>% unite ("month_day",month:day, sep = "-",remove=F)
data_bg_3V_synth_fusion_winter <- data_bg_3V_synth_fusion_winter %>% unite ("month_day",year:month_day, sep = "-",remove=F)


# Using geom_density in ggplot()
flight_density_months<-
  ggplot(data=data_bg_3V_synth_fusion_winter %>% filter(period!="winter_2023-2024")  ,aes(x=ymd(month_day),color=(period)))+
  geom_density(alpha=c(0.6), position = 'identity',na.rm = T)+
  # scale_x_date(limits=c(2000-10-01,2000-))+
  labs(x="Time",y="Density of GPS positions over winter", title="Density of winter GPS positions of tagged-black grouse\nin the Trois Vallées ski resort, by sex according to the GPS device")+
  theme(axis.text = element_text(size=11),axis.title = element_text(size=11),title = element_text(size=12),strip.text = element_text(size = 12))+
  facet_wrap(~marque_tag + energy)

flight_density_months

# ggsave(plot=flight_density_months, file=paste0(base,"/6_FIGURES/data_exploration/","flight_density_months_per_sensor_",format(Sys.time(), "%d.%b%Y"),".png"))


#### 1.2.2_Vizualization of the density of flight during a day #### 

# Using geom_density in ggplot()
flight_density<-
  ggplot(data=data_bg_3V_synth_fusion_winter,aes(x=as.POSIXct(heure),fill=sexe))+
  # geom_histogram(alpha=c(0.6), position = 'identity')+
  geom_density(alpha=c(0.6), position = 'identity')+
  scale_fill_manual(breaks=c("NA","femelle","male"),labels=c("NA","female","male"),values=c("grey","#FF9999","#99CCFF"))+
  scale_x_datetime(date_labels = "%H h")+
  labs(x="Time",y="Number of GPS positions", title="Number of winter GPS positions of tagged-black grouse in the Trois Vallées ski resort, by sex according to the GPS device")+
  theme(axis.text = element_text(size=16),axis.title = element_text(size=18),title = element_text(size=20),strip.text = element_text(size = 18))+
  facet_wrap(~marque_tag + energy,scales = "free_y")

flight_density

# ggsave(plot=flight_density,file=paste0(base,"/6_FIGURES/data_exploration/","flight_hist_per_sensor_",format(Sys.time(), "%d.%b%Y"),".png"))
# ggsave(plot=flight_density,file=paste0(base,"/6_FIGURES/data_exploration/","flight_density_per_sensor_",format(Sys.time(), "%d.%b%Y"),".png"))

 
# Using activityDensity()

  # data formatting for activityDensity()
  data_bg_3V_synth_fusion_winter_act<- data_bg_3V_synth_fusion_winter
  names(data_bg_3V_synth_fusion_winter_act)[7]<-"Time"
  names(data_bg_3V_synth_fusion_winter_act)[4]<-"DateTimeOriginal"
  data_bg_3V_synth_fusion_winter_act<-data_bg_3V_synth_fusion_winter_act %>% dplyr::mutate("Species" = paste(marque_tag, energy),remove=F)
  data_bg_3V_synth_fusion_winter_act<-as.data.frame(data_bg_3V_synth_fusion_winter_act)
  data_bg_3V_synth_fusion_winter_act[,4]<-as.character(data_bg_3V_synth_fusion_winter_act[,4])

par(mfcol=c(2,2))
flight_density_daily_activity<-activityDensity(recordTable=data_bg_3V_synth_fusion_winter_act,allSpecies=T,speciesCol="Species",recordDateTimeCol="DateTimeOriginal",writePNG = F,plotDirectory=paste0(base,"/6_FIGURES/data_exploration/"))
flight_density_daily_activity



#### 1.2.3_Vizualization of the distances covered over a day #### 

# Using geom_density in ggplot()
flight_distance_months<-
  ggplot(data=data_bg_3V_synth_fusion_winter,aes(x=ymd(month_day),y=log(dist),fill="coral"))+
  geom_boxplot(aes(group=cut(x=ymd(month_day), "7 days")))+
  labs(x="Date",y="Average distance covered between two consecutive positions (log transformed)", title="Average distance covered between two consecutive positions over the winter of tagged-black grouse\nin the Trois Vallées ski resort, according to the GPS device")+
  theme(legend.position = "none",axis.text = element_text(size=11),axis.title = element_text(size=11),title = element_text(size=12),strip.text = element_text(size = 12))+
  facet_wrap(~marque_tag + energy,scales = "free_y")

flight_distance_months

# ggsave(plot=flight_distance_months,file=paste0(base,"/6_FIGURES/data_exploration/","flight_distance_months_7days_per_sensor_",format(Sys.time(), "%d.%b%Y"),".png"))




#### 1.2.4_Vizualization of the distances covered over a day #### 

# Using geom_density in ggplot()
flight_distance<-
  ggplot(data=data_bg_3V_synth_fusion_winter,aes(x=as.POSIXct(heure),y=log(dist),fill="coral"))+
  geom_boxplot(aes(group=cut(x=as.POSIXct(heure), "1 hour")))+
  scale_x_datetime(date_labels = "%H h")+
  labs(x="Time",y="Average distance covered between two consecutive positions (log transformed)", title="Average distance covered between two consecutive positions over the day of tagged-black grouse\nin the Trois Vallées ski resort, according to the GPS device")+
  theme(legend.position = "none",axis.text = element_text(size=11),axis.title = element_text(size=11),title = element_text(size=12),strip.text = element_text(size = 12))+
  facet_wrap(~marque_tag + energy,scales = "free_y")

flight_distance

# ggsave(plot=flight_distance,file=paste0(base,"/6_FIGURES/data_exploration/","flight_distance_1h_per_sensor_",format(Sys.time(), "%d.%b%Y"),".png"))







# density(as.POSIXct(data_bg_3V_synth_fusion_winter$date))
