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
#********************************************************************

# Loading data ----
#********************************************************************

### DATASET
fox_corentin <- read.csv2(file.path(base,"Tetralps/2_DATA/corentin_fox_pre_treat.csv"),sep=",")


### VECTORS

# 3V borders 
borders_3V_vectlayer <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
#********************************************************************

setwd(base)


# list_of_animals <- c("Abel, Alpha")
list_of_animals <- unique(fox_corentin$animal.ID)

# period covered by the GPS monitoring
date.limite.suivi <- fox_corentin %>% slice(1, n()) %>% dplyr::select(animal.ID, animal.sex, animal.life.stage, UTC_date)
nb.day.suivi <- fox_corentin %>% group_by(UTC_date) %>% dplyr::select(UTC_date) %>% n_distinct()
date.limite.suivi

### We read in this dataset downloaded from Movebank directly as a move2 object
fox <- vroom::vroom(file.path(base,"Tetralps/2_DATA/corentin_fox_pre_treat.csv")) # with vroom package, timestamps get recognized, are assumed to be in UTC

# now create a move2 object from a data.frame
fox_move2 <- mt_as_move2(fox, 
                         coords = c("location.long","location.lat"),
                         crs = "EPSG:4326",
                         time_column = "timestamp",
                         track_id_column = "animal.ID",  # name of the column which will be define your tracks
                         na.fail = F) # allows or not empty coordinates

fox_sample <- fox_move2 %>% filter(animal.ID %in% list_of_animals)

#### basic plots colored by individual (with ggplot2)
ggplot() + theme_void() +
  ggtitle("Corentin : GPS-tagged fox in the 3 Vallées region (Nothern Alps)")+
  geom_sf(data=borders_3V_vectlayer)+
  geom_sf(data = fox_sample) +
  geom_sf(data = mt_track_lines(fox_sample), aes(color = `animal.ID`)) # to transform data into lines






###############################################################################################
############ Sampling the birds to study and creation of a global telemetry table #############
###############################################################################################


#### Creation of a global telemetry object (for all birds)
fox_sample_dt <- fox_corentin %>% filter(animal.ID %in% list_of_animals)
fox_telemetry <- as.telemetry(fox_sample_dt,projection="+init=epsg:2154",keep=c("animal.sex","animal.life.stage"))

# change the coordinate system into the projection of my raster layers
# projection(birds_sample_bg_telemetry) <- crs(mnt) 



#############################################################################
############ Creation of telemetry, guess, fit and akde objects #############
#############################################################################

#' Fit ctmm model
#' We start with a guesstimate of parameter values; we demand a
#' isotropic ctmm model, because this is a requirement for rsf.fit
fox_guess <- ctmm.guess(fox_telemetry, 
                        CTMM = ctmm(isotropic = TRUE), 
                        interactive = FALSE)
# fast, give the starting point for rsf.fit. isotropic = TRUE : we are only interested in a circular HR (otherwise complicated)

# Fit ctmm model and perform model selection, using all but one core
fox_fit <- ctmm.select(fox_telemetry, fox_guess, cores = -1)

#' Fit akde
fox_akde <- akde(fox_telemetry, fox_fit) #akde calculated for the reference grid


plot(fox_telemetry,UD=fox_akde)


