# Loading libraries ---- 
#********************************************************************
library(dplyr)
library(sf)
library(sjmisc)

library(move2)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth) #needs rnaturalearthhires also installed
library(ggmap)
library(ggspatial) ## needs prettymapr also installed
library(gganimate)
library(plotly)
library(mapview)
library(units)
library(lubridate)
library(moveVis)
library(terra)
library(future.apply)
library(tidyterra)
library(ggnewscale)
#********************************************************************

### Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
#********************************************************************


### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD"
#********************************************************************


# Loading data ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/Tetralps/4_FUNCTIONS/formatting_csv_ornitela.R")
#********************************************************************


# Loading data ----
#********************************************************************
### DATASET
renard_raw <- read.csv2(file.path(base,"Tetralps/1_RAW_DATA/renards/corentin_renard.csv"),sep=",") #upload the file from a csv, not a move2 object
#********************************************************************

# Formatting fox data
renard_dt<-ornitela_formating(renard_raw,name="Corentin",sex="male",species="fox",age="adult",energy="solar",brand="ornitela")
# write.csv(renard_dt,file = file.path(base,"Tetralps/2_DATA/corentin_fox_pre_treat.csv"),row.names = FALSE)













####################################
#### Fitting an rsf on fox data ----
####################################

#### 1_Loading objects ----

### Loading libraries ---- 
#********************************************************************
library(move2)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth) #needs rnaturalearthhires also installed
library(ggmap)
library(ggspatial) ## needs prettymapr also installed
library(gganimate)
library(plotly)
library(mapview)
library(units)
library(lubridate)
library(moveVis)
library(terra)
library(future.apply)
library(tidyterra)
library(ggnewscale)
library(viridis)
library(ctmm)
#********************************************************************


# Loading data ----
#********************************************************************

### DATASET
fox_corentin <- read.csv2(file.path(base,"Tetralps/2_DATA/corentin_fox_pre_treat.csv"),sep=",")

# GPS locations of black grouses
bg_3V_data <- readRDS(file.path(base,"TetrAlps/2_DATA/black_grouse_dataset.rds"))

### VECTORS

# 3V borders 
borders_3V_vectlayer <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect <- terra::vect(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
# borders_3V_vect_WGS84<- project(borders_3V_vect,y="+proj=longlat +datum=WGS84")

# mnt
mnt <- terra::rast(file.path(base, "Tetralps/2_DATA/mnt_ign.tif"))
mnt_9 <- terra::rast(file.path(base, "Tetralps/2_DATA/mnt_9_mean_ign.tif"))
# mnt_9_WGS84<- project(mnt_9,y="+proj=longlat +datum=WGS84")

# slope 3V
slope_3V <- terra::rast(file.path(base,"Tetralps/2_DATA/slope_3V_ign.tif"))
crs(slope_3V)<-"+init=epsg:2154"
slope_3V_9 <- terra::rast(file.path(base,"Tetralps/2_DATA/slope_3V_9_ign.tif"))
crs(slope_3V_9) <- "+init=epsg:2154"
# slope_3V_9_WGS84 <- project(slope_3V_9,y="+proj=longlat +datum=WGS84")

# habitat cartography
carto_habitats_3V <- terra::rast(file.path(base,"Tetralps/2_DATA/carto_habitats_clara_3V.tif")) #carto Clara
levels(carto_habitats_3V)[[1]][["landcover_1m"]] <- c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified")
carto_habitats_3V_9 <- terra::rast(file.path(base,"Tetralps/2_DATA/carto_habitats_3V_9_clara.tif")) #carto Clara
# carto_habitats_3V_9_WGS84<- project(carto_habitats_3V_9,y="+proj=longlat +datum=WGS84")

# strava
strava <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_3V_winter_sports_rgb.tif_single.tif"))
strava <- project(strava,y="+init=epsg:2154")
# strava_WGS84 <- project(strava,y="+proj=longlat +datum=WGS84")

# extents of the study area
e <- c(963981.7, 1002351.7 ,6464374.9 ,6495464.1)
#********************************************************************


setwd(base)

goupil = "Corentin"

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

fox_sample <- fox_move2 %>% filter(animal.ID %in% c(goupil))

#### basic plots colored by individual (with ggplot2)
ggplot() + theme_void() +
  ggtitle("Corentin : GPS-tagged fox in the 3 Vallées region (Nothern Alps)")+
  geom_sf(data=borders_3V_vectlayer)+
  geom_sf(data = fox_sample) +
  geom_sf(data = mt_track_lines(fox_sample), aes(color = `animal.ID`)) # to transform data into lines




#### 2_Outliers detection ----
#********************************************************************

list_of_animals = goupil

#### Creation of a global telemetry object (for all birds)
fox_sample_dt <- fox_corentin %>% filter(animal.ID %in% list_of_animals)
fox_telemetry <- as.telemetry(fox_sample_dt,projection="+init=epsg:2154",keep=c("animal.sex","animal.life.stage"))


# Identify outliers consistent/inconsistent with an error model

DATA <- fox_telemetry

# plot GPS positions with error circles
plot(DATA)

# help file for outlie function ; note the 'by' argument in particular
?outlie 

#! calculate outlier statistics and generate plot
OUT <- outlie(DATA,main=paste0(list_of_animals))
# red segments are for speed
# blue points are for proximity
# the speed and proximity estimates are error informed (if your data are calibrated)

# There is no apparent outliers  


# some useful error-informed statistics were calculated
head(OUT)
# you may also have some other columns, like altitude

# outlier statistics (used for coloring)
plot(OUT,main=paste0(list_of_animals))
# the minimum speed required to explain the data
# deviation from the geometric median
# other axes can be plotted if available


# biological decision to reject all distance > 10 km
MAX_dist <- 5000 %#% 'm'
?'%#%' # convenient units function

#! index of fix with highest speed
GOOD <- OUT$distance < MAX_dist

#
GOOD
false_count <- sum(!GOOD)
false_count
# 257 segments > 5 km : not impossible but the 33 first tracks > 5 km --> might be GPS locations in the car, before the devices was put on the animal --> suppress
# + better to suppress the first day of data because the capture might be stressful for the animal
# # Removing the first day of location data as it often shows some unrealistic movements
fox_sample_dt$timestamp <- as.POSIXct(fox_sample_dt$timestamp, tz = "UTC")
fox_sample_dt <- fox_sample_dt %>% filter (timestamp >= (first(fox_sample_dt$timestamp) + ddays(1) ))

fox_telemetry <- as.telemetry(fox_sample_dt,projection="+init=epsg:2154",keep=c("animal.sex","animal.life.stage"))

DATA <- fox_telemetry

#! calculate outlier statistics and generate plot
OUT <- outlie(DATA,main=paste0(list_of_animals))

# Looking at plot(OUT) we noticed previously 1 extreme distance > 10 km + few distance > 7 km
# biological decision to reject all distance > 10 km
MAX_dist <- 6000 %#% 'm'

#! index of fix with highest speed
GOOD <- OUT$distance < MAX_dist

false_count <- sum(!GOOD)
false_count # indeed 14 extreme distances

#! remove the outliers from the dataset
DATA <- DATA[GOOD,]

# plot data with outlier removed
plot(DATA)

# look for outliers again
#! calculate outlier statistics and generate plot
OUT <- outlie(DATA,main=paste0(list_of_animals))
plot(OUT)
#********************************************************************

fox_telemetry <- DATA

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




#### 3_Home range estimation : Fitting an RSF for 1 bird ----
#********************************************************************
### Loading packages for RSF ----
#+  results='hide', message=FALSE, warning=FALSE
library(animove)
library(ctmm)
library(sf)
library(mvtnorm)
library(terra)

### Setting the limit of the study area for the bird ----

e_myfox<-c(
  min(fox_telemetry$x),
  max(fox_telemetry$x),
  min(fox_telemetry$y),
  max(fox_telemetry$y))


### 3.1_Visualization "raw" HR (without environmental effects) ----

plot(fox_telemetry,UD=fox_akde, main = "Corentin, the fox") #95% HR + incertities around this 95% HR

# source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multi_graph_home_range.R")
# outputfile="C:/Users/albordes/Documents/PhD/Animove2024/my_project/rsf/outputs/season_HR/"
# multi_graph_HR(birds_sample_bg_telemetry_seasons,birds_sample_bg_akde,outputfile,writeplot = TRUE)

par(mar = c(5, 4, 4, 2) + 0.1) # Set margins back to default
r_myfox_akde<-ctmm::raster(fox_akde, DF = "PMF") # PMF  = probability mass function = proba to find the animal in 1 pixel, so the value sum up to 1
terra::plot(r_myfox_akde,ext=extent(e_myfox)*2)

UD_myfox_spatial <- SpatialPolygonsDataFrame.UD(fox_akde,level.UD=.95,level=.95)
UD_myfox_spatial_akde_contours<-mask(r_myfox_akde,UD_myfox_spatial)
SR_myfox_akde<-project(rast(UD_myfox_spatial_akde_contours), y = mnt_9)
# writeRaster(SR_myfox_akde, file = file.path(base,"TetrAlps/2_DATA/fox_Corentin_simple_akde_PMF.tif"), overwrite=TRUE)
# SR_myfox_akde <- terra::rast(file.path(base,"TetrAlps/2_DATA/fox_Corentin_simple_akde_PMF.tif"))


terra::plot(r_myfox_akde,ext=extent(e_myfox)*2)
terra::plot(add=TRUE,UD_myfox_spatial)



### 3.2_Setting environment variables ----

# Formatting the environmental rasters
r_mnt_9<-raster::raster(mnt_9)
r_mnt_9_squared<-raster::raster(mnt_9^2)
r_slope_3V_9<-raster::raster(slope_3V_9)
r_strava<-raster::raster(strava)
r_carto_habitats_3V_9<-raster::raster(carto_habitats_3V_9)

mnt_9_cropped<-crop(r_mnt_9,extent(r_strava))
slope_3V_9_cropped<-crop(r_slope_3V_9,extent(r_strava))
strava_cropped <- crop(r_strava,extent(r_strava))
carto_habitats_3V_9_cropped<-crop(r_carto_habitats_3V_9,extent(r_strava))


# Rieman integrator of the rsf.fit function need ALIGNED RASTERS
#' resample to set everything to the resolution and extent of the layer 'mnt' (so we don't resample the mnt raster)
strava_res_avg <- terra::resample(strava, mnt_9, method = "average",names="strava")
mnt_9_squared_avg <- terra::resample(mnt_9^2, mnt_9, method = "average",names="squared_elevation_9")
slope_3V_9_avg <- terra::resample(slope_3V_9, mnt_9, method = "average",names="slope_3V_9")
carto_habitats_3V_9_near <- terra::project(carto_habitats_3V_9, mnt_9, method = "near",names="carto_habitats_3V_9")
#is.factor(carto_habitats_3V_9_near)
names(mnt_9)<-"elevation_9"

#' stacking it all in an env layer 
env_stack <- c(strava_res_avg,mnt_9_squared_avg,mnt_9,slope_3V_9_avg,carto_habitats_3V_9_near)

#' cropping to the extent of the bird data locations *2
env_crop <- terra::crop(env_stack, extent(e_myfox)*2)

# cheking environment slacks
# terra::plot(env_crop)
# terra::plot(env_crop[["carto_habitats_3V"]])



bg_env_list<-list("strava" =raster::raster(env_crop[["strava"]]),
                  "elevation_9" = raster::raster(env_crop[["elevation_9"]]),
                  "squared_elevation_9" = raster::raster(env_crop[["squared_elevation_9"]]),
                  "slope_3V_9" = raster::raster(env_crop[["slope_3V_9"]]))
# "carto_habitats_3V_9" = as.factor(raster::raster(env_crop[["carto_habitats_3V_9"]])))  
# no significative effect in rsf.fit when carto_habitats_3V is tested, because too many categories --> in winter no sens to have as much categories

#checking : plot the map + bird for each layer
# bg_env_list[["carto_habitats_3V_9"]]
# save(bg_env_list,file=paste0(base,"/Animove2024/my_project/heavy_models/bg_env_list.RData"))


#' #' Definition of reference grid the akde predictions should get aligned to
reference_grid <- bg_env_list[[1]] #RasterLayer

# crs(reference_grid)<-crs(mnt)
ctmm::projection(fox_telemetry)<-crs(reference_grid) # nead to project my telemetry data in the projection of my rasters

#### projection()!! only once for a raster, to avoid distortion, + change the resolution (aggreate) in the same time i change the projection


### RSF function 
#' The integrator = "Riemann" option is much faster
myfox_rsf_riemann <- rsf.fit(fox_telemetry, fox_akde,  
                             R = bg_env_list,
                             integrator = "Riemann") #Riemann = faster option

# reference = c(1,1,1,1,6)) NOT WORKING
# formula= ~ elev2+elev) #if i don t put interaction effects, better not to use the formula because with the formula, 
#the raster used to account for env effects are not always in the same scale VS if i don t use the formula --> rasters are standardized between them
#akde = give info on how to weight each raster = same weight used for the autocorr of the points ; each obs is given the proper weight for akde (instead of giving a value of 1 to each if they have been indep) 
# all the beauty "weighted" thing of akde is putting into rsf
# save(mybird_rsf_riemann, file=paste0(base,"/Animove2024/my_project/heavy_models/mybird_rsf_riemann.RData"))
# with integrator = "Riemann", raster need to be in the same projection, but if you choose integrator = Monte Carlo you can use different projection but takes longer

# Estimates and confidence intervals for all model parameters
# - Selection: The three environmental layers
# - Movement: home range area,time scales of position and velocity autocorrelation;
# derived quantities speed and diffusion
summary(myfox_rsf_riemann)
# est of the env effect < 0 --> animals tempt to avoid
# but look at the confidence innterval : high > 0 --> so not significant avoidance, maybe be a selection
# bc we have a lot of info, lot of uncertainties
# values depend on units



### 3.3_Suitability map ----

#' A suitability map - with 95% confidence interval
# mybird_suitability_riemann <- ctmm::suitability(mybird_rsf_riemann, R = bg_env_list, grid=reference_grid) ## error, why?
myfox_suitability_riemann <- ctmm::suitability(myfox_rsf_riemann, 
                                               R = bg_env_list, 
                                               grid=reference_grid) 
# save(mybird_suitability_riemann, file = paste0(base,"/Animove2024/my_project/rsf/heavy_models_rsf/",bird,"_",season,"_suitability_map.RData"))


# suitability = we remove the home ranging effect --> we only take into account the env raster
terra::plot(myfox_suitability_riemann)
terra::plot(myfox_suitability_riemann$est,main=paste(list_of_animals,"suitability map"))
plot(borders_3V_vectlayer,add=TRUE,col=NA)

ggplot()+
  geom_spatraster(data=terra::rast(myfox_suitability_riemann$est))+
  # scale_fill_viridis()+
  scale_fill_gradientn(name = "Probability of\n intensity of use", colors = c( alpha("#6699FF",0.3),"#6699FF", "blue","#000099"),na.value = "transparent")+
  ggtitle(paste(list_of_animals,"suitability map"))


### 3.4_ Range distribution (includes the ranging behaviour) ----
myfox_agde <- agde(CTMM = myfox_rsf_riemann, 
                   R = bg_env_list, 
                   grid = bg_env_list[[1]])
# save(mybird_agde, file = paste0(base,"/Animove2024/my_project/rsf/heavy_models_rsf/",bird,"_",season,"_agde.RData"))

# now integrated HR info --> what the proba of finding cilla in the given point of the landscape? 
# = we plot the suitability + we incorporate the squared dist to home range to ponderate the proba 
# = rsf model that shows both the selection (size of the home range) and the suitability
# = run with 6 parameters : 3 para env + x, y and HR size para
# we only take into account the suitability and the HR center, not all the animal positions

# Plot raster of range distribution = rsf.fit based
r_myfox_agde <- ctmm::raster(myfox_agde, DF = "PMF")
terra::plot(r_myfox_agde)
plot(borders_3V_vectlayer,col=NA,add=TRUE)

ggplot()+
  geom_spatraster(data=terra::rast(r_myfox_agde))+
  scale_fill_viridis()+
  ggtitle(paste(list_of_animals,"Range distribution map"))

UD_myfox_spatial_agde <- SpatialPolygonsDataFrame.UD(myfox_agde,level.UD=.95,level=.95)
UD_myfox_spatial_agde_contours <- mask(r_myfox_agde,UD_myfox_spatial_agde)


#### 3.5_Selection-informed akde : akde_rsf ----

# Selection-informed akde = create a new akde taking into account env preferences + actual points where the animals are
myfox_akde_rsf <- akde(fox_telemetry, 
                       CTMM = myfox_rsf_riemann, 
                       R = bg_env_list, 
                       grid = bg_env_list[[1]])
# save(mybird_akde_rsf, file = paste0(base,"/Animove2024/my_project/rsf/heavy_models_rsf/",bird,"_",season,"_akde_rsf.RData"))

r_myfox_akde_rsf<-ctmm::raster(myfox_akde_rsf, DF = "PMF")
terra::plot(r_myfox_akde_rsf)
plot(borders_3V_vectlayer,col=NA,add=TRUE)

ggplot()+
  geom_spatraster(data=terra::rast(r_myfox_akde_rsf))+
  scale_fill_viridis()+
  ggtitle(paste(list_of_animals,"Range distribution map taking into account the environment"))

UD_myfox_spatial_akde_rsf <- SpatialPolygonsDataFrame.UD(myfox_akde_rsf,level.UD=.95,level=.95)
UD_myfox_spatial_akde_rsf_contours<-mask(r_myfox_agde,UD_myfox_spatial_akde_rsf)

# compared to agde, akde_rsf take into account all the telemetry locations in addition will all the other para, 
# --> we take the info "we know where are our 3000 GPS points"
# it is better than agde, bc for ex, if we don t have a raster for river, the model do not know there is a river, 
# but bc we take into account telemetry locations, we take indirectly the presence of the river into account = the animals are not inside it


### 4_HR vizualisation ----

gg_elev<-ggplot()+
  geom_spatraster(data=mnt)+
  scale_fill_gradientn(name = "Altitude (m)", colors = c("#CCFFCC", "#FFFFCC", "#FFCC99", "#FF9966", "#FF6600"))+
  geom_sf(data=borders_3V_vect,fill="transparent")+
  theme_classic()+
  xlim((extent(e_myfox)*4)[1],(extent(e_myfox)*4)[2])+
  ylim((extent(e_myfox)*4)[3],(extent(e_myfox)*4)[4])+
  xlab("longitude")+
  ylab("latitude")

gg_strav<-ggplot()+
  geom_spatraster(data=strava)+
  scale_fill_gradientn(name = "Strava intensity", colors = c("#CCCCCC11","#FF6600","#FF3333"))+
  geom_sf(data=borders_3V_vect,fill="transparent")+
  theme_classic()+
  xlim((extent(e_myfox)*4)[1],(extent(e_myfox)*4)[2])+
  ylim((extent(e_myfox)*4)[3],(extent(e_myfox)*4)[4])+
  xlab("longitude")+
  ylab("latitude")


graph_akdeHR_strava<-
  gg_strav +
  ggtitle(paste0(list_of_animals,"'s simple home range (akde)"))+
  new_scale_fill() +
  geom_spatraster(data=terra::rast(UD_myfox_spatial_akde_contours))+ # akde: r_mybird ; agde: agde_raster
  geom_sf(data=st_as_sf(UD_myfox_spatial),fill=NA)+
  scale_fill_gradientn(name = "Probability of\n intensity of use", colors = c( alpha("#6699FF",0.3),"#6699FF", "blue","#000099"),na.value = "transparent")

graph_envHR_strava<-
  gg_strav +
  ggtitle(paste0(list_of_animals,"'s home range taking into account the human pressure (agde)"))+
  new_scale_fill() +
  geom_spatraster(data=terra::rast(UD_myfox_spatial_agde_contours))+ # akde: r_mybird ; agde: agde_raster
  geom_sf(data=st_as_sf(UD_myfox_spatial_agde),fill=NA)+
  scale_fill_gradientn(name = "Probability of\n intensity of use", colors = c( alpha("#6699FF",0.3),"#6699FF", "blue","#000099"),na.value = "transparent")
### weird result !!!

graph_env_useHR_strava<-
  gg_strav +
  ggtitle(paste0(list_of_animals,"'s home range taking into account the human pressure \nand animal GPS positions (akde_rsf)"))+
  new_scale_fill() +
  geom_spatraster(data=terra::rast(UD_myfox_spatial_akde_rsf_contours))+ # akde: r_mybird ; agde: agde_raster
  geom_sf(data=st_as_sf(UD_myfox_spatial_akde_rsf),fill=NA)+
  scale_fill_gradientn(name = "Probability of\n intensity of use", colors = c( alpha("#6699FF",0.3),"#6699FF", "blue","#000099"),na.value = "transparent")


graph_ptsGPS_strava<-
  gg_strav +
  ggtitle(paste0(list_of_animals,"'s GPS positions (telemetry data)"))+
  new_scale_fill() +
  geom_point(data=fox_telemetry,aes(x=fox_telemetry$x,y=fox_telemetry$y)) # akde: r_mybird ; agde: agde_raster

ggsave(plot=graph_akdeHR_strava,filename="graph_akdeHR_strava.png",path="C:/Users/albordes/Documents/PhD/Animove2024/my_project/rsf/outputs/",width = 20, height = 20, units = "cm")
ggsave(plot=graph_envHR_strava,filename="graph_envHR_strava.png",path="C:/Users/albordes/Documents/PhD/Animove2024/my_project/rsf/outputs/",width = 20, height = 20, units = "cm")
ggsave(plot=graph_env_useHR_strava,filename="graph_env_useHR_strava.png",path="C:/Users/albordes/Documents/PhD/Animove2024/my_project/rsf/outputs/",width = 20, height = 20, units = "cm")
ggsave(plot=graph_ptsGPS_strava,filename="graph_ptsGPS_strava.png",path="C:/Users/albordes/Documents/PhD/Animove2024/my_project/rsf/outputs/",width = 20, height = 20, units = "cm")


# Plot all three probability mass functions with a common color scale
pmf <- raster::stack(ctmm::raster(birds_sample_bg_akde_seasons[[bird]][[season]], DF = "PMF"),
                     ctmm::raster(mybird_agde, DF = "PMF"),
                     ctmm::raster(mybird_akde_rsf, DF = "PMF"))
names(pmf) <- c("akde", "rsf.fit", "akde_rsf.fit")
plot(pmf, zlim=c(0, max(raster::getValues(pmf)))) # use zlim to force same color scale

par(mfrow=c(2,2))
plot(pmf$akde, zlim=c(0, max(raster::getValues(pmf))),main=names(pmf)[1])
plot(borders_3V_vectlayer,col=NA,add=TRUE)
plot(pmf$rsf.fit, zlim=c(0, max(raster::getValues(pmf))),main=names(pmf)[2])
plot(borders_3V_vectlayer,col=NA,add=TRUE)
plot(pmf$akde_rsf.fit, zlim=c(0, max(raster::getValues(pmf))),main=names(pmf)[3])
plot(borders_3V_vectlayer,col=NA,add=TRUE)







### 5.Bonus_Animate tracks with gganimate overlapping the habitat ----

#### 5.1.Selection of birds monitored over the same period of Corentin ----

bg_3V_data_selection <- bg_3V_data %>% filter(jour >= "2023-12-30") %>% filter (jour <= "2024-07-15") 
bg_3V_data_selection <- bg_3V_data_selection[,-ncol(bg_3V_data_selection)] # remove capture location
list_of_birds <- unique(bg_3V_data_selection$ani_nom)



### We read in this dataset downloaded from Movebank directly as a move2 object
bg_3V_data_pretelemetry<-pre_telemetry(bg_3V_data,"WGS84")
# write.table(bg_3V_data_pretelemetry,file=file.path(base,"Tetralps/2_DATA/pre_tele4_bg_3V_10_06_24.csv"),sep="\t",dec=".", row.names = FALSE)

bg <- vroom::vroom(file.path(base,"Tetralps/2_DATA/pre_tele4_bg_3V_10_06_24.csv")) # with vroom package, timestamps get recognized, are assumed to be in UTC

# now create a move2 object from a data.frame
bg_move2 <- mt_as_move2(bg, 
                        coords = c("location.long","location.lat"),
                        crs = "EPSG:4326",
                        time_column = "timestamp",
                        track_id_column = "animal.ID",  # name of the column which will be define your tracks
                        na.fail = F) # allows or not empty coordinates

birds_sample_bg <- bg_move2 %>% filter(animal.ID %in% list_of_birds)



#create points
data_interpolated <- birds_sample_bg[!sf::st_is_empty(birds_sample_bg), ] |>  # careful, interpolation is just a visualization tool, not ecological correct for analyses
  mt_interpolate(
    seq(
      as.POSIXct("2023-12-30"),
      as.POSIXct("2024-03-15"), "1 day" 
    ),
    max_time_lag = units::as_units(1, "day"),
    omit = TRUE
  )

# Create lines connecting the points for each individual
data_interpolated <- data_interpolated %>%
  filter(!is.na(st_coordinates(.)[,1]) & !is.na(st_coordinates(.)[,2])) # Ensure data_interpolated is free of NAs in the coordinates

# lines_data <- data_interpolated %>%
#   group_by(animal.ID) %>%
#   arrange(timestamp) %>%
#   # mutate(alpha = seq(0, 1, length.out = n())) %>%  # Create a custom alpha value 
#   summarize(do_union = FALSE) %>%
#   st_cast("LINESTRING")



animation <- 
  graph_env_useHR_strava +
  annotation_scale() +
  # geom_sf(data = lines_data, aes(color = `animal.ID`), size = 1, alpha = 0.5) +  # Add lines
  geom_sf(data = data_interpolated, size = 2,
          aes(color = `animal.ID`)) +
  # geom_path(aes(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2], group = animal.ID, color = animal.ID), size = 1) +  # Add lines
  # geom_point(aes(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2], group = animal.ID, color = animal.ID), size = 2) +  # Add points
  transition_manual(timestamp) +
  # transition_reveal(along = timestamp) +
  labs(
    title = "Black grouse, winter 2023-2024",
    subtitle = "Time: {current_frame}",
    color = "Individual"
  )


# Animate with gradual fade
gg_anim <- gganimate::animate(animation,
                              nframes = length(unique(data_interpolated$timestamp)),
                              fps = 10,
                              renderer = gganimate::gifski_renderer(loop = TRUE))
                              


gganimate::anim_save(filename = file.path(base,"TetrAlps","5_OUTPUTS","data_exploration","birds_winter23_24_CorentinHR_1day.gif"),
                     animation = gg_anim)

## see more gganimate examples here: https://bartk.gitlab.io/move2/articles/albatross.html
#********************************************************************


#********************************************************************
### 6. Home range overlaps between Corentin the fox and birds ----

# This script details methods that can be used for studying
# interactions between individuals. This includes:
# - Home-range overlap
# - Encounter location distributions (CDE)
# - Pairwise distances
# - Proximity ratios
# - Encounter rates


# These analyses are conditional on fitted movement models and HR estimates
# (see: https://github.com/ctmm-initiative/ctmmlearn/blob/main/ctmm_akde.R)
library(ctmm)

# Load data *******************************************************
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/season_default_coord_syst/best_model_fit_akde/best_model_3V.RData")
assign("best_model_saved_hiver_malefemelle",best_model_3V)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/season_default_coord_syst/akde/grouse_3V_akde.RData")
assign("grouse_winter_akde_saved_hiver_malefemelle",grouse_3V_akde)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/season_default_coord_syst/telemetry/grouse_3V_telemetry.RData")
assign("grouse_winter_telemetry_hiver_malefemelle",grouse_3V_telemetry)
# *****************************************************************

# bg_3V_data_telemetry <- as.telemetry(bg_3V_data_pretelemetry)
# projection(bg_3V_data_telemetry) <- median(bg_3V_data_telemetry)


#-----------------------------------------------------'
# Home-range overlap
#-----------------------------------------------------'

# Do individuals share the same space?
# Relevant paper: https://doi.org/10.1111/2041-210X.13027
help("overlap")

#Estimate HR overlap for all pairs
#Note: these all must have compatible resolutions and alignments
#this works because HRs are estimated simultaneously (and consistently)

overlap_fox_bird <- overlap(akde( list(grouse_3V_telemetry$Fleau[["printemps1"]],fox_telemetry),
                                  list(best_model_3V$Fleau[["printemps1"]], fox_fit)))

# the grid can be unalign but must have the same resolution (so we have to take the hightest resolution of both indiv) and projection 
# overlap in % (1 = 100% overlap)


# look at everything
overlap_fox_bird

# pairwise CIs 
overlap_fox_bird$CI["Fleau","Corentin",]

# point estimates
overlap_fox_bird$CI[,,"est"]

# overlap_fox_bird give how much the distributions of animals are simalar
# rq : metaphore package allows to implement incertitude in models
# Log(x,variable="area",debias=TRUE...)
# and then use methaphore package and then mixed effects can be integrated in models

#-----------------------------------------------------'
# Encounter location distributions (CDE)
#-----------------------------------------------------'

# where encounters are expected to take place

# Relevant paper: https://doi.org/10.1111/2041-210X.13597
help("cde")


#Plot the data and HR estimates
plot(fox_telemetry,
     UD = fox_akde, 
     col = "#264653",col.DF="#2a9d8f",col.grid = NA)
plot(grouse_3V_telemetry$Fleau[["printemps1"]],
     UD = grouse_3V_akde$Fleau[["printemps1"]], 
     col = "#e76f51",col.DF="#f4a261",col.grid = NA,
     add = TRUE)


#Estimate the CDE
#calculate encounter probabilities and the conditional location distribution of where encounters take place
cde_fox_bird <- cde(akde( list(grouse_3V_telemetry$Fleau[["printemps1"]],fox_telemetry),
                          list(best_model_3V$Fleau[["printemps1"]], fox_fit)))

#Visualise the CDE
plot(fox_telemetry,
     col = "#264653")
plot(cde_fox_bird,
     col.DF="#f4a261",col.grid = NA,
     add = TRUE)
plot(grouse_3V_telemetry$Fleau[["printemps1"]],
     col = "#e76f51",
     add = TRUE)


#-----------------------------------------------------'
# Pairwise proximity and distance metrics
#-----------------------------------------------------'

# metrics that takes time into account (paper coming)
help("proximity")

#= how far away are animals from each other compared to if there where independent movers (if close to 1= close to independent movers?)
# but must be interpreted differently according if animals are spatially close or not 

#Pairwise separation distances
dist_fox_bird <- distances( list(grouse_3V_telemetry$Fleau[["printemps1"]],fox_telemetry),
                            list(best_model_3V$Fleau[["printemps1"]], fox_fit))

# period covered by the GPS monitoring
date.limite.suivi.birdFL <- c(min(grouse_3V_telemetry$Fleau[["printemps1"]][["timestamp"]]),max(grouse_3V_telemetry$Fleau[["printemps1"]][["timestamp"]]))
date.limite.suivi.birdFL

#OUPS 2022!!


#Visualise the separation distances
plot(dist_fox_bird$est ~ dist_fox_bird$timestamp,
     type = "l",
     col = "#5e548e")


#Internal plotting function (work in progress)
ctmm:::ts.plot(dist_fox_bird)


bird_sim <- simulate(best_model_3V$Fleau[["printemps1"]], t = grouse_3V_telemetry$Fleau[["printemps1"]]$t)
fox_sim <- simulate(fox_fit, t = fox_telemetry$t)


sim_dists <- distances(list(bird_sim, fox_sim),
                       list(best_model_3V$Fleau[["printemps1"]], fox_fit))













#Plot the data
par(mfrow = c(2,2))
plot(buffalo[c("Cilla", "Mvubu")],
     col = c("#e76f51", "#264653"),
     main = "Empirical data")

plot(list(cilla_sim, mvubu_sim),
     col = c("#e76f51", "#264653"),
     main = "Simulated data")

plot(DISTS$est ~ DISTS$timestamp,
     type = "l",
     col = "#5e548e",
     main = "Empirical distances",
     ylab = "Distance (m)",
     xlab = "Time",
     ylim = c(0,max(sim_dists$est)))

plot(sim_dists$est ~ sim_dists$timestamp,
     type = "l",
     col = "#5e548e",
     main = "Simulated distances",
     ylab = "Distance (m)",
     xlab = "Time",
     ylim = c(0,max(sim_dists$est)))



# Proximity ratio (note: can be slow)
PROXIMITY <- proximity(buffalo[c("Cilla","Mvubu")],
                       FITS[c("Cilla","Mvubu")])
load("data/buffalo_proximity.rda")
PROXIMITY

# Proximity ratio for simulated animals
SIM_PROXIMITY <- proximity(list(cilla_sim, mvubu_sim),
                           FITS[c("Cilla","Mvubu")])
load("data/simulated_proximity.rda")
SIM_PROXIMITY

#-----------------------------------------------------'
# Encounters
#-----------------------------------------------------'

help("encounter")
# Relevant paper: https://doi.org/10.1101/2023.06.07.544097

#Empirical encounters
DISTS$encounter <- ifelse(DISTS$est <= 100, 1, 0)

#Visualise the results
par(mfrow = c(1,1))
plot(DISTS$encounter ~ DISTS$timestamp)
cdplot(as.factor(DISTS$encounter) ~ DISTS$timestamp)

#Empirical Encounter rate (n/day)
n <- sum(DISTS$encounter)
t <- "day" %#% (DISTS$t[nrow(DISTS)] - DISTS$t[1])
n/t


#If you do this, run a sensitivity analysis
enc_rad <- 1:1000
N <- vector("numeric", 1000)
for(i in 1:length(enc_rad)){
  N[i] <- sum(ifelse(DISTS$est <= enc_rad[i], 1, 0))
}

#visualise the results
plot(N ~ enc_rad,
     ylab = "Encounters",
     xlab = "Encounter radius",
     type = "l",
     col = "#5e548e")


#Estimate relative encounter rates
RATES <- encounter(AKDES)
RATES$CI["Cilla","Mvubu",] * 100^2 # good for small distances
tanh(sqrt(RATES$CI["Cilla","Mvubu",])*100)^2 # more reliable
#********************************************************************