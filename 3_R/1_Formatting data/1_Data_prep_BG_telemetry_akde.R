# Loading libraries ---- 
#********************************************************************
library(tidyverse)
library(dplyr)
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

# Main characteristics of tagged-black grouse
synth_bg_all_sites <- read.csv2(file.path(base,"TetrAlps/1_RAW_DATA/bilan_captures_tetras_all_sites_mar2024_x_y_coord.csv"),sep=",")[,-1]


### VECTORS

# 3V borders 
borders_3V_vectlayer <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
#********************************************************************







#### 1_Add and clean data on the capture site ----
#********************************************************************

# formatting synth_bg_all_sites to retrieve capture positions coordinates
synth_bg_all_sites$x_capture_coord <- as.numeric(synth_bg_all_sites$x_capture_coord)
synth_bg_all_sites$y_capture_coord <- as.numeric(synth_bg_all_sites$y_capture_coord)

synth_bg_all_sites$x_capture_coord2 <- ifelse(is.na(synth_bg_all_sites$x_capture_coord), 1,synth_bg_all_sites$x_capture_coord)
synth_bg_all_sites$y_capture_coord2 <- ifelse(is.na(synth_bg_all_sites$y_capture_coord), 1,synth_bg_all_sites$y_capture_coord)

synth_bg_all_sites <- st_as_sf(as.data.frame(synth_bg_all_sites), coords = c("x_capture_coord2", "y_capture_coord2"), crs = 4326)

# supress absurd GPS values
for(i in 1: nrow(synth_bg_all_sites))
{
  if(st_bbox(synth_bg_all_sites$geometry[i])$xmin >100)
  {
    synth_bg_all_sites$geometry[i] <- NA
  }
}      

# select all the points equal to POINT (1 1)
synth_bg_3V <- synth_bg_all_sites %>% filter(zone_etude=="trois_vallees")
colnames(synth_bg_3V["geometry"]) <- "geometry_WGS84"
synth_bg_3V$geometry_capture <- st_transform(synth_bg_3V$geometry,crs="EPSG:2154")
synth_bg_3V <- as.data.frame(synth_bg_3V)

data_bg_3V_synth <- dplyr::left_join(as.data.frame(data_bg_3V) ,synth_bg_3V %>% select(ani_nom,tag_id,espece,marque_tag,energy,geometry_capture), by=c("tag_id"="tag_id","ani_nom"="ani_nom"))
#********************************************************************






#### 2_Creation of the pretelemetry object (clean from erratic data of the first day) ----
#********************************************************************

#' Create a dt formatted for importation in movebank 
data_bg_pretelemetry <- pre_telemetry(data_bg_3V_synth,"WGS84")
data_bg_pretelemetry <- data_bg_pretelemetry%>% filter((animal.ID != "Eros") & (animal.ID !="Emilien")) # the pb for 2021-05-14 = Emilien and the pb for 2021-06-11 = Eros



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


write.csv(data_bg_pretelemetry[,!names(data_bg_pretelemetry) %in% c("geometry","geometry_capture")], row.names=FALSE, file="C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/data_bg_pretelemetry.csv")
#********************************************************************




#### 3_Loading data ----
#********************************************************************

### We read in this dataset downloaded from Movebank directly as a move2 object
bg <- vroom::vroom(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry.csv")) # with vroom package, timestamps get recognized, are assumed to be in UTC


list_of_animals <- unique(bg$animal.ID)


# now create a move2 object from a data.frame
bg_move2 <- mt_as_move2(bg, 
                        coords = c("location.long","location.lat"),
                        crs = "EPSG:4326",
                        time_column = "timestamp",
                        track_id_column = "animal.ID",  # name of the column which will be define your tracks
                        na.fail = F) # allows or not empty coordinates


birds_sample_bg <- bg_move2 %>% filter(animal.ID %in% list_of_animals)


#### basic plots colored by individual (with ggplot2)
ggplot() + theme_void() +
  ggtitle("4 Black grouse GPS-tagged in the 3 Vallées region (Nothern Alps)")+
  geom_sf(data=borders_3V_vectlayer)+
  geom_sf(data = birds_sample_bg) +
  geom_sf(data = mt_track_lines(birds_sample_bg), aes(color = `animal.ID`)) # to transform data into lines
#********************************************************************







#### 4_Creation of telemetry, guess, fit and akde objects ----
#********************************************************************

### 4.1_Creation of a telemetry object ----
#********************************************************************
# data_bg_pretelemetry <- read.csv2(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry.csv"),sep=",")

#### Creation of a global telemetry object (for all birds)
telemetry <- as.telemetry(data_bg_pretelemetry,projection="EPSG:2154",keep=c("saison","saison2","period_jour","animal.sex","animal.life.stage"))

# change the coordinate system into the projection of my environmental raster layers
# projection(birds_sample_bg_telemetry) <- crs(mnt) 
ctmm::projection(telemetry) <- "EPSG:2154" #Lambert93
#********************************************************************











### 4.3_Sub-setting the telemetry object by the season (management point of view) ----
#********************************************************************
#### creation of a list() where each element will corresponds to a season (winter 1, 2 or 3) for a bird i in grouse_winter_telemetry.

for(bird in seq_along(telemetry))
{
  telemetry_seasons <- list()
  saison <- unique(telemetry[[bird]]$saison2)
  
  for(season in seq_along(saison))
  {
    telemetry_seasons[[season]] <- telemetry[[bird]][telemetry[[bird]]$saison2==saison[season],] # The telemetry_seasons list() will contain as many dataframes as seasons the bird lived. 
  }
  names(telemetry_seasons) <- unique(telemetry[[bird]]$saison2)
  telemetry[[bird]] <- telemetry_seasons  # Save the list() of dataframes by seasons the bird lived into the list of dataframes telemmetry objects for each bird. 
}
telemetry_dt <- telemetry

save(telemetry_dt, file = file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/birds_3V/telemetry_dt.RData"))
#********************************************************************




### 4.2_Creation of guess, fit and akde objects ----
#********************************************************************
akde_dt <- fit_dt <- guess_dt <- telemetry_dt

for(bird in seq_along(telemetry))
{
  saison<-unique(names(telemetry[[bird]]))
  
  for(season in seq_along(saison))
  {
    telemetry_bird<-telemetry[[bird]][[season]]
    
    guess_dt[[bird]][[season]] <- guess <- ctmm.guess(telemetry_bird,
                                                      CTMM = ctmm(isotropic = TRUE),
                                                      interactive = FALSE)
    
    fit_dt[[bird]][[season]]   <- fit <- ctmm.select(telemetry_bird,
                                                     guess_dt[[bird]][[season]],
                                                     cores = -1)
    
    akde_dt[[bird]][[season]]  <- akde <- akde(telemetry_bird, fit_dt[[bird]][[season]]) #don t work with the argument: grid = reference_grid
    
  }
}



save(guess_dt, file = file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/birds_3V/guess_dt.RData"))
save(akde_dt, file = file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/birds_3V/akde_dt.RData"))
#********************************************************************
#********************************************************************


plot(telemetry_dt[[1]][["hiver1"]],UD=akde_dt[[1]][["hiver1"]])


