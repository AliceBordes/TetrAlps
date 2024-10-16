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
#********************************************************************


### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD"
#********************************************************************

# Loading data ----
#********************************************************************

### DATASET
birds_bg_dt<-read.csv2(file.path(base,"Tetralps/2_DATA/pretelemetry_bg_3V_WGS84.csv"),sep=",") #upload the file from a csv, not a move2 object


### VECTORS

# 3V borders 
borders_3V_vectlayer <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect <- terra::vect(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
# borders_3V_vect_WGS84<- project(borders_3V_vect,y="+proj=longlat +datum=WGS84")

### RASTERS

# mnt <- terra::rast(file.path(base, "Tetralps/2_DATA/mnt_ign.tif"))
# mnt <- project(mnt, "EPSG:2154")
mnt_9 <- terra::rast(file.path(base, "Tetralps/2_DATA/mnt_9_mean_ign.tif"))
mnt_9 <- project(mnt_9, "EPSG:2154")

# slope 3V
slope_3V <- terra::rast(file.path(base,"Tetralps/2_DATA/slope_3V_ign.tif"))
crs(slope_3V) <- "EPSG:2154"
slope_3V <- project(slope_3V, y = mnt_9, method = "bilinear") # resolution of slope_3V is set at 9m such as mnt_9

# strava
strava <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_3V_winter_sports_rgb.tif_single.tif"))
strava <- project(strava, y = mnt_9, method = "bilinear") # y = a raster to align on (avoid to use the function resample after), "EPSG:2154" can be specify if there is no argument y. pas besoin de mettre de focal (zone d'influence), method="belinear" advised for continuous raster
          # resolution of strava is set artificially at 9m such as mnt_9, but the true resolution is 38.21851m

# habitat cartography
carto_habitats_3V <- terra::rast(file.path(base,"Tetralps/2_DATA/carto_habitats_clara_3V.tif")) #carto Clara
levels(carto_habitats_3V)[[1]][["landcover_1m"]]<-c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified")
carto_habitats_3V <- project(carto_habitats_3V, y = mnt_9, method = "near")

# aggregate habitat categories

carto_habitats_3V_winter <- carto_habitats_3V

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(20, 1))

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(20, 1)) #Unclassified soil
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(21, 1)) #Fine mineral soil
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(22, 1)) #Coarse mineral soil
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(30, 1)) #Dry or rocky grassland
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(31, 1)) #Herbaceous
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(32, 1)) #Low ligneous

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(40, 2)) #Shrubs

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(50, 3)) #Unclassified trees
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(51, 3)) #Deciduous trees
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(52, 3)) #Resinous trees

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(23, 4)) #Cliff

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(60, 5)) #Buildings
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(92, 5)) #Natural pond
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(93, 5)) #Artificial pond
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(94, 5)) #Waterway
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(100, 5)) #Unclassified

terra::plot(carto_habitats_3V_winter)
carto_habitats_3V_winter <- as.factor(carto_habitats_3V_winter)

# extents of the study area
e<-c(963981.7, 1002351.7 ,6464374.9 ,6495464.1)

#********************************************************************



### Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/mean_size_area.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/visu_home_range.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/distance_home_range_capture_site.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/SVF_indiv.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multi_graph_home_range.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/plot_check_RSF_results.R")
#********************************************************************


#********************************************************************
setwd(base)

#select the bird and the season
bird="Alpha"
season="hiver2"
list_of_animals = bird
#********************************************************************



### Loading data for rsf ----
#********************************************************************
load(paste0(base,"/Animove2024/my_project/heavy_models/birds_sample_bg_telemetry_seasons.RData"))
load(paste0(base,"/Animove2024/my_project/heavy_models/birds_sample_bg_ctmm_fit_seasons.RData"))
load(paste0(base,"/Animove2024/my_project/heavy_models/birds_sample_bg_akde_seasons.RData"))

#### Creation of a global telemetry object (for all birds)

birds_sample_bg_pretele <- birds_bg_dt %>% filter(animal.ID %in% list_of_animals)
telemetry <- as.telemetry(birds_sample_bg_pretele,projection="EPSG:2154",keep=c("saison","saison2","period_jour","animal.sex","animal.life.stage"))

#### projection()!! only once for a two-dimension object (such as a raster), to avoid distortion, + change the resolution (aggreate) in the same time i change the projection
ctmm::projection(telemetry)<-"EPSG:2154" # nead to project my telemetry data in the projection of my rasters

# winter telemetry
telemetry_winter <- telemetry[telemetry$saison2==season,]

# home range akde
guess_winter <- ctmm.guess(telemetry_winter,
                    CTMM = ctmm(isotropic = TRUE),
                    interactive = FALSE)

fit_winter <- ctmm.select(telemetry_winter,
                  guess)

akde_winter <- akde(telemetry_winter,
                          fit_winter)

#********************************************************************


### We read in this dataset downloaded from Movebank directly as a move2 object
bg <- vroom::vroom(file.path(base,"Tetralps/2_DATA/pretelemetry_bg_3V_WGS84.csv")) # with vroom package, timestamps get recognized, are assumed to be in UTC

# now create a move2 object from a data.frame
bg_move2 <- mt_as_move2(bg, 
                        coords = c("location.long","location.lat"),
                        crs = "EPSG:4326",
                        time_column = "timestamp",
                        track_id_column = "animal.ID",  # name of the column which will be define your tracks
                        na.fail = F) # allows or not empty coordinates

birds_sample_bg <- bg_move2 %>% filter(animal.ID %in% c(bird))%>%filter(saison2 %in% c(season))

#### basic plots colored by individual (with ggplot2)
ggplot() + theme_void() +
  ggtitle("4 Black grouse GPS-tagged in the 3 Vallées region (Nothern Alps)")+
  geom_sf(data=borders_3V_vectlayer)+
  geom_sf(data = birds_sample_bg) +
  geom_sf(data = mt_track_lines(birds_sample_bg), aes(color = `animal.ID`)) # to transform data into lines



#### 2_Outliers detection ----
#********************************************************************
# Identify outliers consistent/inconsistent with an error model

DATA <- birds_sample_bg_telemetry_seasons[[bird]][[season]]

# plot GPS positions with error circles
plot(DATA)

# help file for outlie function ; note the 'by' argument in particular
?outlie 

#! calculate outlier statistics and generate plot
OUT <- outlie(DATA,main=paste0(bird," (",season,")"))
# red segments are for speed
# blue points are for proximity
# the speed and proximity estimates are error informed (if your data are calibrated)

# There is no apparent outliers  


# some useful error-informed statistics were calculated
head(OUT)
# you may also have some other columns, like altitude

# outlier statistics (used for coloring)
plot(OUT,main=paste0(bird," (",season,")"))
# the minimum speed required to explain the data
# deviation from the geometric median
# other axes can be plotted if available
#********************************************************************




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

e_mybird<-c(
  min(birds_sample_bg_telemetry_seasons[[bird]][[season]]$x),
  max(birds_sample_bg_telemetry_seasons[[bird]][[season]]$x),
  min(birds_sample_bg_telemetry_seasons[[bird]][[season]]$y),
  max(birds_sample_bg_telemetry_seasons[[bird]][[season]]$y))


### 3.1_Visualization "raw" HR (without environmental effects) ----

plot(birds_sample_bg_telemetry_seasons[[bird]][[season]],UD=birds_sample_bg_akde_seasons[[bird]][[season]]) #95% HR + incertities around this 95% HR

# source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multi_graph_home_range.R")
# outputfile="C:/Users/albordes/Documents/PhD/Animove2024/my_project/rsf/outputs/season_HR/"
# multi_graph_HR(birds_sample_bg_telemetry_seasons,birds_sample_bg_akde,outputfile,writeplot = TRUE)


par(mar = c(5, 4, 4, 2) + 0.1) # Set margins back to default
r_mybird_akde<-ctmm::raster(birds_sample_bg_akde_seasons[[bird]][[season]], DF = "PMF") # PMF  = probability mass function = proba to find the animal in 1 pixel, so the value sum up to 1
terra::plot(r_mybird_akde,ext=extent(e_mybird)*2)

UD_mybird_spatial <- SpatialPolygonsDataFrame.UD(birds_sample_bg_akde_seasons[[bird]][[season]],level.UD=.95,level=.95)
UD_mybird_spatial_akde_contours<-mask(r_mybird_akde,UD_mybird_spatial)

terra::plot(r_mybird_akde,ext=extent(e_mybird)*2)
terra::plot(add=TRUE,UD_mybird_spatial)



### 3.2_Setting environment variables ----

#' stacking it all in an env layer 
envir_stack <- c(mnt_9,mnt_9^2,strava,slope_3V,carto_habitats_3V_winter)

#' cropping to the extent of the bird data locations *2
envir_crop <- terra::crop(envir_stack, extent(e_mybird)*2)


env_RL_list <- lapply(envir_crop,raster::raster)
names(env_RL_list) <- c("elevation", "square_elevation", "strava", "slope", "carto_habitats_winter")

save(env_RL_list,file=file.path(base,"/Animove2024/my_project/heavy_models/env_RL_list.RData"))
load(file.path(base,"/Animove2024/my_project/heavy_models/env_RL_list.RData"))

# cheking environment slacks
# terra::plot(envir_crop)
# terra::plot(envir_crop[["carto_habitats_winter"]])



### 3.2.aside_Predictors' collinearity ----
#********************************************************************
# Correlation between layers
# https://statnmap.com/2018-01-27-spatial-correlation-between-rasters/ 
cor(values(bg_env_list[["elevation_9"]]),
    values(bg_env_list[["strava"]]),
    use = "na.or.complete", method = "pearson")
# Pearson's coefficient is an index reflecting a linear relationship between two continuous variables.
# Kendall's or Spearman's coefficients recommended if the data do not necessarily come from a bivariate normal distribution. 
# Kendall's or Spearman's for ordinal variables (= categorical but hierarchical!)

lm1<-lm(values(bg_env_list[["elevation_9"]]) ~ values(bg_env_list[["strava"]]))
summary(lm1)
lm2<-lm(values(bg_env_list[["strava"]]) ~ values(bg_env_list[["elevation_9"]]))
summary(lm2)

rast_resid <- bg_env_list[["elevation_9"]]
values(rast_resid) <- lm1$residuals
rast_resid2 <- bg_env_list[["strava"]]
values(rast_resid2) <- lm2$residuals

par(mfrow=c(2,2))
terra::plot(bg_env_list[["elevation_9"]],main="elevation_9")
terra::plot(rast_resid,main="elevation_9 residuals")
terra::plot(bg_env_list[["strava"]],main="strava")
terra::plot(rast_resid2,main="strava residuals")
# " elevation_9 is underestimated at the south (green part of the graph elevation_9 residuals) and overestimated in the north when using strava as linear predictor
# "strava is underestimated where the strava intensity is high (green part of the graph strava residuals) and overestimated where strava intensity is low when using elevation_9 as linear predictor
# But this is barely relevant as the correlation between elevation_9 and Strava is really weak.
#********************************************************************


### RSF function 
#' The integrator = "Riemann" option is much faster
mybird_rsf_riemann <- rsf.fit(telemetry_winter, akde_winter,  
                              R = env_RL_list,
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
summary(mybird_rsf_riemann)
# est of the env effect < 0 --> animals tempt to avoid
# but look at the confidence innterval : high > 0 --> so not significant avoidance, maybe be a selection
# bc we have a lot of info, lot of uncertainties
# values depend on units

env_RL_list2 = list(env_RL_list[["elevation"]],env_RL_list[["square_elevation"]],env_RL_list[["slope"]],env_RL_list[["carto_habitats_winter"]])
names(env_RL_list2) <- c("elevation","square_elevation","slope","carto_habitats_winter")

### RSF function 
#' The integrator = "Riemann" option is much faster
mybird_rsf_riemann2 <- rsf.fit(telemetry_winter, akde_winter,  
                              R = env_RL_list2,
                              integrator = "Riemann") #Riemann = faster option

summary(mybird_rsf_riemann2)




### 3.3_Suitability map ----

#' A suitability map - with 95% confidence interval
# mybird_suitability_riemann <- ctmm::suitability(mybird_rsf_riemann, R = bg_env_list, grid=reference_grid) ## error, why?
mybird_suitability_riemann <- ctmm::suitability(mybird_rsf_riemann, R = bg_env_list, grid=reference_grid) 
# save(mybird_suitability_riemann, file = paste0(base,"/Animove2024/my_project/rsf/heavy_models_rsf/",bird,"_",season,"_suitability_map.RData"))


# suitability = we remove the home ranging effect --> we only take into account the env raster
terra::plot(mybird_suitability_riemann)
terra::plot(mybird_suitability_riemann$est,main=paste(bird,"suitability map"))
plot(borders_3V_vectlayer,add=TRUE,col=NA)

ggplot()+
  geom_spatraster(data=terra::rast(mybird_suitability_riemann$est))+
  # scale_fill_viridis()+
  scale_fill_gradientn(name = "Probability of\n intensity of use", colors = c( alpha("#6699FF",0.3),"#6699FF", "blue","#000099"),na.value = "transparent")+
  ggtitle(paste(bird,"suitability map"))


### 3.4_ Range distribution (includes the ranging behaviour) ----
mybird_agde <- agde(CTMM = mybird_rsf_riemann, R = bg_env_list, 
                    grid = bg_env_list[[1]])
# save(mybird_agde, file = paste0(base,"/Animove2024/my_project/rsf/heavy_models_rsf/",bird,"_",season,"_agde.RData"))

# now integrated HR info --> what the proba of finding cilla in the given point of the landscape? 
# = we plot the suitability + we incorporate the squared dist to home range to ponderate the proba 
# = rsf model that shows both the selection (size of the home range) and the suitability
# = run with 6 parameters : 3 para env + x, y and HR size para
# we only take into account the suitability and the HR center, not all the animal positions

# Plot raster of range distribution = rsf.fit based
r_mybird_agde <- ctmm::raster(mybird_agde, DF = "PMF")
plot(r_mybird_agde)
plot(borders_3V_vectlayer,col=NA,add=TRUE)

ggplot()+
  geom_spatraster(data=terra::rast(r_mybird_agde))+
  scale_fill_viridis()+
  ggtitle(paste(bird,"Range distribution map"))

UD_mybird_spatial_agde <- SpatialPolygonsDataFrame.UD(mybird_agde,level.UD=.95,level=.95)
UD_mybird_spatial_agde_contours<-mask(r_mybird_agde,UD_mybird_spatial_agde)


#### 3.5_Selection-informed akde : akde_rsf ----

# Selection-informed akde = create a new akde taking into account env preferences + actual points where the animals are
mybird_akde_rsf <- akde(birds_sample_bg_telemetry_seasons[[bird]][[season]], CTMM = mybird_rsf_riemann, R = bg_env_list, 
                        grid = bg_env_list[[1]])
# save(mybird_akde_rsf, file = paste0(base,"/Animove2024/my_project/rsf/heavy_models_rsf/",bird,"_",season,"_akde_rsf.RData"))

r_mybird_akde_rsf<-ctmm::raster(mybird_akde_rsf, DF = "PMF")
plot(r_mybird_akde_rsf)
plot(borders_3V_vectlayer,col=NA,add=TRUE)

ggplot()+
  geom_spatraster(data=terra::rast(r_mybird_akde_rsf))+
  scale_fill_viridis()+
  ggtitle(paste(bird,"Range distribution map taking into account the environment"))

UD_mybird_spatial_akde_rsf <- SpatialPolygonsDataFrame.UD(mybird_akde_rsf,level.UD=.95,level=.95)
UD_mybird_spatial_akde_rsf_contours<-mask(r_mybird_agde,UD_mybird_spatial_akde_rsf)

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
  xlim((extent(e_mybird)*4)[1],(extent(e_mybird)*4)[2])+
  ylim((extent(e_mybird)*4)[3],(extent(e_mybird)*4)[4])+
  xlab("longitude")+
  ylab("latitude")

gg_strav<-ggplot()+
  geom_spatraster(data=strava)+
  scale_fill_gradientn(name = "Strava intensity", colors = c("#CCCCCC11","#FF6600","#FF3333"))+
  geom_sf(data=borders_3V_vect,fill="transparent")+
  theme_classic()+
  xlim((extent(e_mybird)*4)[1],(extent(e_mybird)*4)[2])+
  ylim((extent(e_mybird)*4)[3],(extent(e_mybird)*4)[4])+
  xlab("longitude")+
  ylab("latitude")


graph_akdeHR_strava<-
  gg_strav +
  ggtitle(paste0(bird,"'s simple home range (akde)"))+
  new_scale_fill() +
  geom_spatraster(data=terra::rast(UD_mybird_spatial_akde_contours))+ # akde: r_mybird ; agde: agde_raster
  geom_sf(data=st_as_sf(UD_mybird_spatial),fill=NA)+
  scale_fill_gradientn(name = "Probability of\n intensity of use", colors = c( alpha("#6699FF",0.3),"#6699FF", "blue","#000099"),na.value = "transparent")

graph_envHR_strava<-
  gg_strav +
  ggtitle(paste0(bird,"'s home range taking into account the human pressure (agde)"))+
  new_scale_fill() +
  geom_spatraster(data=terra::rast(UD_mybird_spatial_agde_contours))+ # akde: r_mybird ; agde: agde_raster
  geom_sf(data=st_as_sf(UD_mybird_spatial_agde),fill=NA)+
  scale_fill_gradientn(name = "Probability of\n intensity of use", colors = c( alpha("#6699FF",0.3),"#6699FF", "blue","#000099"),na.value = "transparent")

graph_env_useHR_strava<-
  gg_strav +
  ggtitle(paste0(bird,"'s home range taking into account the human pressure \nand animal GPS positions (akde_rsf)"))+
  new_scale_fill() +
  geom_spatraster(data=terra::rast(UD_mybird_spatial_akde_rsf_contours))+ # akde: r_mybird ; agde: agde_raster
  geom_sf(data=st_as_sf(UD_mybird_spatial_akde_rsf),fill=NA)+
  scale_fill_gradientn(name = "Probability of\n intensity of use", colors = c( alpha("#6699FF",0.3),"#6699FF", "blue","#000099"),na.value = "transparent")


graph_ptsGPS_strava<-
  gg_strav +
  ggtitle(paste0(bird,"'s GPS positions (telemetry data)"))+
  new_scale_fill() +
  geom_point(data=birds_sample_bg_telemetry_seasons[[bird]][[season]],aes(x=birds_sample_bg_telemetry_seasons[[bird]][[season]]$x,y=birds_sample_bg_telemetry_seasons[[bird]][[season]]$y)) # akde: r_mybird ; agde: agde_raster

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







### 4.bonus_Animate tracks with gganimate overlapping the habitat 

data_interpolated <- birds_sample_bg[!sf::st_is_empty(birds_sample_bg), ] |>  # careful, interpolation is just a visualization tool, not ecological correct for analyses
  mt_interpolate(
    seq(
      as.POSIXct("2018-02-05"),
      as.POSIXct("2018-02-15"), "3 hour" 
    ),
    max_time_lag = units::as_units(3, "hour"),
    omit = TRUE
  )
animation <- 
  graph_env_useHR_strava +
  # ggplot() +
  # annotation_map_tile(zoom = 9, progress = "none") +
  # geom_spatraster(data=mnt)+
  # scale_fill_gradientn(name = "Altitude (m)", colors = c("#CCFFCC", "#FFFFCC", "#FFCC99", "#FF9966", "#FF6600"))+
  # geom_sf(data=borders_3V_vect,fill="transparent")+
  # xlab("longitude")+
  # ylab("latitude")+
  # theme_classic() +
  annotation_scale() +
  geom_sf(
    data = data_interpolated, size = 2,
    aes(color = `animal.ID`), color = "green"
    # aes(color = `animal.ID`)
  ) +
  transition_manual(timestamp) +
  labs(
    title = "Alpha, 2nd winter",
    subtitle = "Time: {current_frame}",
    color = "Individual"
  )
gg_anim <- gganimate::animate(animation,
                              nframes = length(unique(data_interpolated$timestamp))
)

gganimate::anim_save(filename = file.path(base,"5_OUTPUTS","data_exploration","Alpha_hiver2.gif"),
                     animation = gg_anim)

## see more gganimate examples here: https://bartk.gitlab.io/move2/articles/albatross.html
#********************************************************************







#### 5_Home range prediction for 1 bird ---- 
#********************************************************************

#' # Traditional RSF with downweighted Poisson regression
#' Functions to generate quadrature points and predict with the model
rsf_points <- function(x, UD, R = NULL, n = 1e5, k = 1e6, type = "Riemann",
                       rmax = 6*sqrt(UD@CTMM$sigma[1,1]),
                       interpolation = FALSE) {
  # Samples background points from a 2D normal distribution fitted to the relocation data, and extracts environmental
  # information from a raster object "R"
  # x: telemetry object
  # UD: UD object
  # R: terra object
  # n: number of background points to sample
  # k: weight of presence points
  # rmax: maximum distance for Riemann-type integration
  # interpolation: do interpolation when sampling the grid
  # When type 0´= "MonteCarlo", importance sampling is done
  stopifnot(UD@CTMM$isotropic)
  stopifnot(type %in% c("Riemann", "MonteCarlo"))
  if (type == "Riemann") {
    quadrature_pts <- values(R) #raster::getValues(R)
    xy <- as.data.frame(xyFromCell(R, 1:ncell(R)))
    xy <- sf::st_as_sf(xy, coords = c("x", "y"), crs = crs(R))
    xy <- st_transform(xy, UD@info$projection)
    xy <- st_coordinates(xy)
    r <- sqrt(((xy[,1] - UD@CTMM$mu[1]))^2 + ((xy[,2] - UD@CTMM$mu[2]))^2)
    bg <- data.frame(case_ = 0,
                     x_ = xy[r<rmax,1], y_ = xy[r<rmax,2],  w_ = prod(res(R)), k_ = k)
    bg <- cbind(bg, quadrature_pts[r<rmax,])
    bg <- sf::st_as_sf(bg, coords = c("x_", "y_"), crs = UD@info$projection)
    xx <- data.frame(case_ = 1, x_ = x$x, y_ = x$y,
                     w_ = 1/k * UD$weights * mean(UD$DOF.area), k_ = k)
    xx <- sf::st_as_sf(xx, coords = c("x_", "y_"), crs = UD@info$projection)
    xx[names(R)] <- as.data.frame(extract(R, st_transform(xx, crs(R)), 
                                          ID = FALSE,
                                          method = ifelse(interpolation, "bilinear", "simple")))
    xx <- rbind(bg, xx)
  } else {
    quadrature_pts <- MASS::mvrnorm(n, mu = UD@CTMM$mu, Sigma = UD@CTMM$sigma)
    xx <- data.frame(case_ = 0, x_ = quadrature_pts[, 1], y_ = quadrature_pts[, 2], w_ = UD@CTMM$sigma[1,1]/n, k_ = k)
    xx <- rbind(xx, data.frame(case_ = 1, x_ = x$x, y_ = x$y,
                               w_ = 1/k * UD$weights * mean(UD$DOF.area), k_ = k
    ))
    xx <- sf::st_as_sf(xx, coords = c("x_", "y_"), crs = UD@info$projection)
    xx[names(R)] <- as.data.frame(extract(R, st_transform(xx, crs(R)), 
                                          ID = FALSE,
                                          method = ifelse(interpolation, "bilinear", "simple")))
  }
  xy <- st_coordinates(xx)
  colnames(xy) <- c("x_", "y_")
  sd <- sqrt(UD@CTMM$sigma[1,1])
  xy[,1] <- (xy[,1] - UD@CTMM$mu[1])/sd
  xy[,2] <- (xy[,2] - UD@CTMM$mu[2])/sd
  xx <- cbind(xy, xx)
  xx
}

predict_rsf <- function(model, UD, object, include_avail = TRUE, data_crs = UD@info$projection) {
  if(include_avail) {
    xy <- as.data.frame(xyFromCell(object, 1:ncell(object)))
    xy <- sf::st_as_sf(xy, coords = c("x", "y"), crs = crs(object))
    xy <- st_transform(xy, data_crs)
    xy <- st_coordinates(xy)
    colnames(xy) <- c("x_", "y_")
    
    sd <- sqrt(UD@CTMM$sigma[1,1])
    xy[,1] <- (xy[,1] - UD@CTMM$mu[1])/sd
    xy[,2] <- (xy[,2] - UD@CTMM$mu[2])/sd
    
  } else {
    xy <- cbind("x_" = rep(0, ncell(object)), "y_" = rep(0, ncell(object)))
  }
  newdata <- as.data.frame(cbind(xy, values(object)))
  lambda <- as.numeric(predict(model, newdata, type = "link"))
  r <- rast(object[[1]])
  r[] <- exp(lambda - max(lambda, na.rm = TRUE))
  r <- r / sum(values(r), na.rm = TRUE)
  r
}
















#' ## A minimal "classic" example
#' Generate quadrature points ("background points")
set.seed(2)
rsf_mybird_df <- rsf_points(birds_sample_bg_telemetry_seasons[[bird]][[season]], birds_sample_bg_akde_seasons[[bird]][[season]], bg_env_list[["elev"]], interpolation = TRUE)
rsf_cilla_df <- rsf_cilla_df[!is.na(rsf_cilla_df$slope),] # Remove lines with NA values
#' Fit a downweighted Poisson regression
#' the homeranging behaviour is represented by x_ + y_ + I(-(x_^2 + y_^2)/2)
m_rsf_cilla <- glm(case_*k_ ~ x_ + y_ + I(-(x_^2 + y_^2)/2) + elev + slope + var_NDVI,
                   family = poisson(), data= rsf_cilla_df, weights = w_)
#' Summary of model and confidence intervals for parameter estimates
#+ message=FALSE, warning=FALSE
summary(m_rsf_cilla)
confint(m_rsf_cilla)

#' Map of suitability, including the home ranging behaviour
suitability_glm <- predict_rsf(m_rsf_cilla, cilla_akde, 
                               crop(buffalo_env, extent(reference_grid)))
plot(suitability_glm)

#' Map of suitability, without the home ranging behaviour
suitability_no_avail_glm <- predict_rsf(m_rsf_cilla, cilla_akde, 
                                        crop(buffalo_env, extent(reference_grid)),
                                        include_avail = FALSE)
plot(suitability_no_avail_glm)

#' Comparison of estimates from the two approaches (ctmm and "classic" via glm)
vars <- c("elev", "slope", "var_NDVI")

# Estimates are not identical but similar
cilla_rsf_riemann$beta[vars]
coef(m_rsf_cilla)[vars]

# Confidence intervals are of similar width, but location is different
#+ message=FALSE, warning=FALSE
ci_glm <- confint(m_rsf_cilla)
ci_glm[vars, ]
summary(cilla_rsf_riemann)$CI[sapply(vars, function(x) grep(x, rownames(summary(cilla_rsf_riemann)$CI))), ]

#' Create maps of suitability based on parameter estimates
M <- terra::values(buffalo_env)
eta_glm <- M[,vars] %*% coef(m_rsf_cilla)[vars]
eta_rsf <- M[,vars] %*% cilla_rsf_riemann$beta[vars]

suit_r_glm <- buffalo_env[[1]]
suit_r_glm[] <- exp(eta_glm)
suit_r_rsf <- buffalo_env[[1]]
suit_r_rsf[] <- exp(eta_rsf)

suit_raster <- c(suit_r_rsf, suit_r_glm)
names(suit_raster) <- c("ctmm", "classic")
plot(suit_raster)
plot(suit_raster, ext = extent(reference_grid))


#' ## Use vs availability
#' h_avail: Histogram of elevation values within the reference grid (here taken as available elevations)
#' h_use: Histogram of elevation values at locations used by Cilla
h_avail <- hist(values(reference_grid), prob = TRUE, col=rgb(0,0,1,1/4), main = "",
                xlab = "Elevation (m a.s.l.)", ylim = c(0,0.015))
h_use <- hist(extract(reference_grid, cilla_mv), prob = TRUE,
              breaks = h_avail$breaks, add = TRUE, col=rgb(1,0,0,1/4))
legend("topright",
       fill = c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)),
       legend = c("available","use"),
       bty = "n"
)

# A simple rsf model containing only selection for elevation, but with
# a quadratic relationship. No homeranging is included
m_rsf_cilla_elev <- glm(case_*k_ ~ poly(elev, 2), family = poisson(), 
                        data= rsf_cilla_df, weights = w_)

# Estimate of density of available elevation. 
# We need this to calculate the integration constant to convert the 
# habitat selection values (i.e. the prediction of the habitat model)
# to the use/availability ratio.

d_avail <- density(values(reference_grid)) 
elev_pred <- data.frame("elev" = d_avail$x)
elev_pred$w <- predict(m_rsf_cilla_elev, 
                       newdata = elev_pred,
                       type = "response")

#' Calculate the integration constant
#' $$u(x) = \frac{w(x) a(x)}{\int_{E}w(X) a(X)dX} $$
#' Where u(x): use of resource level x ; a(x): availablility of resource level x
#' w(x): selection of resource level x, E: the range of resource levels.
#' K is the integration constant $\int_{E}w(X) a(X)dX$.
#' 
K <- sum(elev_pred$w * d_avail$y * diff(d_avail$x[1:2]))

plot(h_avail$mids, h_use$density/h_avail$density,
     xlab = "Elevation (m a.s.l.)",
     ylab = "use/availability ratio")
abline(h = 1, lty = 3)
lines(elev_pred$elev, elev_pred$w / K, col = "green", lwd = 2)
legend("topright", 
       col = c("black", "green"), 
       lty = c(NA,1), lwd = 2, pch = c(1, NA),
       legend = c("use/availability","rsf: poly(elev, 2)/K"),
       bty = "n"
)

#' ## Multiple animals
#'
#' - with rsf.fit: you can use the mean function on a list of rsf.fit objects
#' - "classic" approach: use glmmTMB and a mixed-effects Poisson regression: Muff, Signer & Fieberg (2020) J Anim Ecol 89: 80-92.
#'










