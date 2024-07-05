# Loading libraries ---- 
#********************************************************************
library(dplyr)
library(ggplot2)
library(rnaturalearth) #needs rnaturalearthhires also installed
library(ggmap)
library(ggspatial) ## needs prettymapr also installed
library(units)
library(lubridate)
library(terra)
library(ctmm)
#********************************************************************


### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD"
outputfolder=paste0(base,"/PhD/TetrAlps/3_R/heavy_saved_models/birds_3V/")
#********************************************************************

# Loading data ----
#********************************************************************

### DATASET

birds_bg_dt <- read.csv2(file.path(base,"Tetralps/2_DATA/pretelemetry_bg_3V_WGS84.csv"),sep=",") #upload the file from a csv, not a move2 object


### VECTORS

# 3V borders 
borders_3V_vectlayer <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
#********************************************************************




setwd(base)
# list_of_animals <- c("Abel, Alpha")
list_of_animals <- unique(birds_bg_dt$animal.ID)

### We read in this dataset downloaded from Movebank directly as a move2 object
bg <- vroom::vroom("Tetralps/2_DATA/pretelemetry_bg_3V_WGS84.csv") # with vroom package, timestamps get recognized, are assumed to be in UTC

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




###############################################################################################
############ Sampling the birds to study and creation of a global telemetry table #############
###############################################################################################


#### Creation of a global telemetry object (for all birds)

birds_sample_bg <- birds_bg_dt %>% filter(animal.ID %in% list_of_animals)
telemetry <- as.telemetry(birds_sample_bg,projection="+init=epsg:2154",keep=c("saison","saison2","period_jour","animal.sex","animal.life.stage"))

# change the coordinate system into the projection of my raster layers
# projection(birds_sample_bg_telemetry) <- crs(mnt) 




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

save(telemetry_dt, file=paste0(base,"/Animove2024/my_project/heavy_models/telemetry_dt.RData"))




#############################################################################
############ Creation of telemetry, guess, fit and akde objects #############
#############################################################################

akde_dt<-fit_dt<-guess_dt<-telemetry_dt

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



save(guess_dt, file=paste0(base,"/Animove2024/my_project/heavy_models/guess_dt.RData"))
save(akde_dt, file=paste0(base,"/Animove2024/my_project/heavy_models/akde_dt.RData"))




plot(telemetry_dt[[1]][["hiver1"]],UD=akde_dt[[1]][["hiver1"]])


