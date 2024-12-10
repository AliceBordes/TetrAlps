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


### VECTORS

# 3V borders 
borders_3V_vectlayer <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
#********************************************************************







#### 1_Add additional data
#********************************************************************

#### 1.1_Fusion GPS and animal features ----
#********************************************************************
capture_synth <- capture %>% dplyr::select(ani_nom, 
                                    tag_id, 
                                    cpt_date_capture, 
                                    age_libelle, 
                                    cpt_poids_animal, 
                                    cpt_x, 
                                    cpt_y,
                                    ela_libelle,
                                    zet_nom) %>% arrange(ani_nom)

gps_synth <- GPS %>% dplyr::select(tag_id, mtg_libelle, etg_libelle)

deployment <- inner_join(gps_synth, capture_synth, by = "tag_id") %>% arrange(ani_nom) # inner_join() returns only the rows that have matching values in both data frames

# check the anomalies between data_bg_3V from Marc and the tables from the database
    global <- dplyr::inner_join(deployment %>% dplyr::select(tag_id, ani_nom), data_bg_3V, by = join_by(tag_id, ani_nom)) 
    
    data_bg_3V_unique <- as.data.frame(data_bg_3V) %>%
      dplyr::select(ani_nom, tag_id) %>%
      distinct()
    
    global_unique <- as.data.frame(global) %>%
      dplyr::select(ani_nom, tag_id) %>%
      distinct()
    
    dplyr::setdiff(data_bg_3V_unique$ani_nom, global_unique$ani_nom) # "Barjot" "Galile" are in data_bg_3V_unique but not in global_unique
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
bg_3V_synth <- inner_join(as.data.frame(data_bg_3V) %>% dplyr::select(-age), deployment, by = join_by("tag_id","ani_nom")) 


# check the match between ani_nom and tag_id (1 ani_nom for serval tag_id is possible but not the contrary)
as.data.frame(bg_3V_synth) %>%
  dplyr::select(ani_nom, tag_id) %>%
  distinct()
#********************************************************************


#### 1.2_Add data on visitor numbers ----
#********************************************************************
# Ski resort visitors

# Snow depth
#********************************************************************




#### 2_Creation of the pretelemetry object (clean from erratic data of the first day) ----
#********************************************************************

#' Create a dt formatted for importation in movebank 
data_bg_pretelemetry <- pre_telemetry(bg_3V_synth,"WGS84")
# data_bg_pretelemetry <- data_bg_pretelemetry%>% filter((animal.ID != "Eros") & (animal.ID !="Emilien")) # the pb for 2021-05-14 = Emilien and the pb for 2021-06-11 = Eros



# # Removing the first day of location data as it often shows some unrealistic movements
data_bg_pretelemetry <- data_bg_pretelemetry %>%
  group_by(animal.ID) %>%
  filter(as.Date(timestamp) > min(as.Date(timestamp)))


# Removing the bird with less than 100 GPS positions data after removing the first day of movements
# The estimation of the temporal autocorrelation between points, using plot(variogram(SVF_object),CTMM=ctmm_object) in ctmm, cannot be estimated with Escartefigue (53 locations) but with Dameur (83 locations it is ok)
# often the programmation is 1 loc/h between 4:00 am and 8:00 pm UTC --> so at least 15 loc/day * 7 days = 105 loc ~ at least 100 loc are necessary to be able to do estimations
data_bg_pretelemetry <- data_bg_pretelemetry %>%
  group_by(animal.ID) %>%
  filter(n() >= 100) %>%
  ungroup() 

write.csv(data_bg_pretelemetry[,!names(data_bg_pretelemetry) %in% c("geometry","geometry_capture")], row.names=FALSE, file=paste0("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/data_bg_pretelemetry_",format(Sys.Date(), format = "%Y_%m"),".csv"))
#********************************************************************




### 3_Add info on the Valley and resort to allow the assignation between bird and visitor numbers and snow depth ----
#********************************************************************
### Load DATASET
birds_bg_dt<-read.csv2(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry_2024_10.csv"),sep=",") #upload the file from a csv, not a move2 object

### We read in this dataset downloaded from Movebank directly as a move2 object
bg <- vroom::vroom(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry_2024_10.csv")) # with vroom package, timestamps get recognized, are assumed to be in UTC

# now create a move2 object from a data.frame
bg_move2 <- mt_as_move2(bg, 
                        coords = c("location.long","location.lat"),
                        crs = "EPSG:4326",
                        time_column = "timestamp",
                        track_id_column = "animal.ID",  # name of the column which will be define your tracks
                        na.fail = F) # allows or not empty coordinates


# Identification of the Valley and resorts
birds_sample_bg_winter <- bg_move2 %>%filter(saison %in% c(season1))
dt_resorts <- data.frame("animal" = character(), "valley" = character(), "resort" = character())
for(bg in seq_along(unique(birds_sample_bg_winter$animal.ID)))
{
  birds_sample_bg_winter_ani <- bg_move2 %>% filter(animal.ID %in% c(unique(birds_sample_bg_winter$animal.ID)[bg]))

  gg_obj <-
    ggplot() + theme_void() +
    ggtitle("Black grouse GPS-tagged in the 3 Vallées region (Nothern Alps)")+
    geom_spatvector(data = borders_3V_vect,fill = NA, aes(color = NOM))+
    scale_color_manual(
      values = c("Courchevel" = "blue", "Les Allues" = "turquoise", "Les Belleville" = "darkblue"),
      labels = c("Courchevel" = "Courchevel (1)", "Les Allues" = "Les Allues (2)", "Les Belleville" = "Les Belleville (3)")) +
    labs(color = "Valley")+
    new_scale_color()+
    geom_sf(data = ski_lift_traffic_3V, aes(color = resort))+
    scale_color_manual(
      values = c("Val Thorens-Orelle" = "orange", "Les Ménuires" = "purple", "NA" = "grey","Courchevel" = "lightgreen","Méribel-Mottaret" = "red"),
      labels = c("Val Thorens-Orelle" = "Val Thorens-Orelle (4)", "Les Ménuires" = "Les Ménuires (2)", "NA" = "Méribel (and NA) (5)","Courchevel" = "Courchevel (1)","Méribel-Mottaret" = "Méribel-Mottaret (3)"))+
    geom_sf(data = birds_sample_bg_winter_ani) +
    new_scale_color()+
    geom_sf(data = mt_track_lines(birds_sample_bg_winter_ani), aes(color = `animal.ID`)) # to transform data into lines
  plot(gg_obj)

  # Prompting the user for input in R
  resp1 <- readline(prompt = "Valley: ")
  resp1 <- case_when(
    resp1 == "1" ~ "Courchevel",      # If input is "1", set resp1 to "Les Allues"
    resp1 == "2" ~ "Les Allues",      # If input is "2", set resp1 to "Courchevel"
    resp1 == "3" ~ "Les Belleville",  # If input is "3", set resp1 to "Les Belleville"
    TRUE ~ "Unknown Valley"           # Default case for any other input
  )

  resp2 <- readline(prompt = "Resort: ")
  resp2 <- case_when(
    resp2 == "1" ~ "Courchevel",          # If input is "1", set resp1 to "Courchevel"
    resp2 == "2" ~ "Les Ménuires",        # If input is "2", set resp1 to "Courchevel"
    resp2 == "3" ~ "Méribel-Mottaret",    # If input is "3", set resp1 to "Les Belleville"
    resp2 == "4" ~ "Val Thorens-Orelle",  # If input is "3", set resp1 to "Les Belleville"
    resp2 == "5" ~ "Méribel",                  # If input is "3", set resp1 to "Les Belleville"
    TRUE ~ "Unknown Valley"               # Default case for any other input
  )

  dt_resorts_to_bind <- data.frame("animal" = unique(bg_move2$animal.ID)[bg], "valley" = resp1, "resort" = resp2)
  dt_resorts <- rbind(dt_resorts, dt_resorts_to_bind)
}
write.csv2(dt_resorts, file = file.path(base,"Tetralps","2_Data","bg_winter_assign_valley_resort.csv"), row.names = FALSE)
dt_resorts <- read.csv2(file.path(base,"Tetralps","2_Data","bg_winter_assign_valley_resort.csv"))

birds_bg_dt <- left_join(birds_bg_dt, dt_resorts, by = c("animal.ID" = "animal")) # ; summary_res <- birds_bg_dt %>% group_by(animal.ID, valley, resort)  %>% summarise(count = n(), .groups = "drop") 


# Cesar, Evoila, Fangio : not really associated with a specific resort

write.csv(birds_bg_dt, file = file.path(base,"Tetralps","2_Data","data_bg_pretelemetry_2024_10.csv"), row.names=FALSE)
#********************************************************************







