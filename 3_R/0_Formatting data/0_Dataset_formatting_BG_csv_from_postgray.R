# Loading libraries ---- 
#********************************************************************
library(tidyverse) # contains dplyr
library(sf)
library(ggplot2)
library(rnaturalearth) #needs rnaturalearthhires also installed
library(ggmap)
library(ggspatial) ## needs prettymapr also installed
library(units)
library(lubridate)
library(terra)
library(ctmm)
library(move2)
library(janitor)
library(openxlsx)
#********************************************************************


### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD"
outputfolder <- paste0(base,"/PhD/TetrAlps/3_R/heavy_saved_models/birds_3V/")
#********************************************************************


### Loadind functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
#********************************************************************



# Loading data ----
#********************************************************************
### DATASETS

# GPS locations of black grouses
data_bg_3V <- readRDS(file.path(base,"TetrAlps/1_RAW_DATA/tot.ind.trois_vallees2_10_06_24.rds"))

# Sensor features
GPS <- read.csv2(file.path(base,"from_postgray_data_based/GPS_sensor_tag.csv"), sep=",", row.names = 1)

# Capture features
capture <- read.csv2(file.path(base,"from_postgray_data_based/capture_table.csv"), sep=",", row.names = 1)

# Animal features
animal <- read.csv2(file.path(base,"from_postgray_data_based/animal_features_trois_vallees.csv"), sep=",", row.names = 1)






# Main characteristics of tagged-black grouse
synth_bg_all_sites <- read.csv2(file.path(base,"TetrAlps/1_RAW_DATA/bilan_captures_tetras_all_sites_mar2024_x_y_coord.csv"),sep=",")[,-1]


### VECTORS

# 3V borders 
borders_3V_vectlayer <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
#********************************************************************







#### 1_Add additional data
#********************************************************************

#### 1.1_Fusion GPS and animal features ----
#********************************************************************
capture_synth <- capture %>% select(ani_nom, 
                                    tag_id, 
                                    cpt_date_capture, 
                                    age_libelle, 
                                    cpt_poids_animal, 
                                    cpt_x, 
                                    cpt_y,
                                    ela_libelle,
                                    zet_nom) %>% arrange(ani_nom)

gps_synth <- GPS %>% select(tag_id, mtg_libelle, etg_libelle)

deployment <- inner_join(gps_synth, capture_synth, by = "tag_id") %>% arrange(ani_nom) # inner_join() returns only the rows that have matching values in both data frames

# check the anomalies between data_bg_3V from Marc and the tables from the database
    global <- inner_join(deployment %>% select(tag_id, ani_nom), data_bg_3V, by = join_by(tag_id, ani_nom)) 
    
    data_bg_3V_unique <- as.data.frame(data_bg_3V) %>%
      select(ani_nom, tag_id) %>%
      distinct()
    
    global_unique <- as.data.frame(global) %>%
      select(ani_nom, tag_id) %>%
      distinct()
    
    setdiff(data_bg_3V_unique$ani_nom, global_unique$ani_nom) # "Barjot" "Galile" are in data_bg_3V_unique but not in global_unique
    # Results : 
    # Abel and Barjot have the same tag & Filou and Galile also
    # Galilee in capture VS Galile in data_bg_3V

# Repare the anomalies
    # Replace "17154" with "171336" in the 'tag_id' column of the ani_nom == Barjot
    data_bg_3V <- data_bg_3V %>% 
      mutate(tag_id = if_else(ani_nom == "Barjot" & tag_id == "17154", "171336", tag_id))
    
    # Replace "Galile" with "Galilee" in the 'ani_nom' column
    data_bg_3V <- data_bg_3V %>%
      mutate(ani_nom = if_else(ani_nom == "Galile", "Galilee", ani_nom))

# add info on GPS manifacturer and energy of the device + capture features 
bg_3V_synth <- inner_join(as.data.frame(data_bg_3V) %>% select(-age), deployment, by = join_by("tag_id","ani_nom")) 


# check the match between ani_nom and tag_id (1 ani_nom for serval tag_id is possible but not the contrary)
as.data.frame(bg_3V_synth) %>%
  select(ani_nom, tag_id) %>%
  distinct()
#********************************************************************


#### 1.2_Add data on visitor numbers ----
#********************************************************************
# Ski resort visitors
#********************************************************************






#### 2_Creation of the pretelemetry object (clean from erratic data of the first day) ----
#********************************************************************

#' Create a dt formatted for importation in movebank 
data_bg_pretelemetry <- pre_telemetry(bg_3V_synth,"WGS84")
# data_bg_pretelemetry <- data_bg_pretelemetry%>% filter((animal.ID != "Eros") & (animal.ID !="Emilien")) # the pb for 2021-05-14 = Emilien and the pb for 2021-06-11 = Eros



# # Removing the first day of location data as it often shows some unrealistic movements
data_bg_pretelemetry <- data_bg_pretelemetry %>%
  group_by(animal.ID) %>%
  filter(as.Date(study.local.timestamp) > min(as.Date(study.local.timestamp)))


# Removing the bird with less than 100 GPS positions data after removing the first day of movements
# The estimation of the temporal autocorrelation between points, using plot(variogram(SVF_object),CTMM=ctmm_object) in ctmm, cannot be estimated with Escartefigue (53 locations) but with Dameur (83 locations it is ok)
# often the programmation is 1 loc/h between 4:00 am and 8:00 pm UTC --> so at least 15 loc/day * 7 days = 105 loc ~ at least 100 loc are necessary to be able to do estimations
data_bg_pretelemetry <- data_bg_pretelemetry %>%
  group_by(animal.ID) %>%
  filter(n() >= 100) %>%
  ungroup() 


write.csv(data_bg_pretelemetry[,!names(data_bg_pretelemetry) %in% c("geometry","geometry_capture")], row.names=FALSE, file=paste0("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/data_bg_pretelemetry_",format(Sys.Date(), format = "%Y_%m_%d"),".csv"))
#********************************************************************

