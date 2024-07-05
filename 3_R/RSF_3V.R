#### PhD Tetras_test project ####

# Alice Bordes #

# June 2023 #

# Description:

# Creation telemetry object



### Loading packages ----
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
library(ggbreak)
library(raster)
library(amt)
library(sp)
library(gridExtra)
library(ggnewscale)
library(classInt)
#********************************************************************

### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD/TetrAlps"
#********************************************************************


### Loading data ----
#********************************************************************

### DATASETS

# GPS locations of black grouses
bg_3V_data<-readRDS(file.path(base,"2_DATA/black_grouse_dataset.rds"))

### RASTERS

# slope 3V
slope_3V<-terra::rast(file.path(base,"2_DATA/slope_3V_ign.tif"))
crs(slope_3V)<-"+init=epsg:2154"
slope_3V_9<-terra::rast(file.path(base,"2_DATA/slope_3V_9_ign.tif"))
crs(slope_3V_9)<-"+init=epsg:2154"

# Analyse à 9m
# carto_habitats_3V <- terra::rast(file.path(base,"1_RAW_DATA/landcover_T2L_1m_trois_vallees.tif")) #carto Clara
# carto_habitats_3V<-as.factor(carto_habitats_3V) #indicate discrete values for my map categories
#raster high vegetation
# carto_habitats_3V <- terra::crop(carto_habitats_3V,e)
# carto_habitats_3V_9<- terra::aggregate(carto_habitats_3V,9,fun="modal") # fact = 9 =  number of cells (pixels) in each direction (horizontally and vertically)
# fun = "modal" for a categorial raster = retains the majoritary class 
# disagg = to disaggregate
# writeRaster(x=carto_habitats_3V, filename=file.path(output_folder_zone,"carto_habitats_clara_3V.tif"),overwrite=TRUE)

carto_habitats_3V <- terra::rast(file.path(base,"2_DATA/carto_habitats_clara_3V.tif")) #carto Clara
carto_habitats_3V_9 <- terra::rast(file.path(base,"2_DATA/carto_habitats_3V_9_clara.tif")) #carto Clara
carto_habitats_3V_9_WGS84<- project(carto_habitats_3V_9,y="+proj=longlat +datum=WGS84")


# strava
strava <- terra::rast(file.path(base, "2_DATA/strava/strava_3V_winter_sports_rgb.tif_single.tif"))
strava <- project(strava,y="+init=epsg:2154")
strava_WGS84 <- project(strava,y="+proj=longlat +datum=WGS84")

# mnt
mnt<-terra::rast(file.path(base, "2_DATA/mnt_ign.tif"))
mnt_9<-terra::rast(file.path(base, "2_DATA/mnt_9_mean_ign.tif"))
mnt_9_WGS84<- project(mnt_9,y="+proj=longlat +datum=WGS84")

### VECTORS

# 3V borders 
borders_3V_vect <- terra::vect(paste0(base,"/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect_WGS84<- project(borders_3V_vect,y="+proj=longlat +datum=WGS84")

# 3V cables from Open street Map
cables_3V <- terra::vect(file.path(base,"2_DATA/ski_lift_osm.gpkg"))
cables_3V_WGS84<- project(cables_3V,y="+proj=longlat +datum=WGS84")

# 3V cables from OGM (Marc Montadert), NO NAMES 
cables_3V_no_id <- terra::vect(file.path(base,"1_RAW_DATA/human_traffic_in_ski_resorts/cables/cables.gpkg"))
cables_3V_no_id_WGS84<- project(cables_3V_no_id,y="+proj=longlat +datum=WGS84")

# lek sites 
lek_locations_vect <- terra::vect(file.path(base,"1_RAW_DATA/place_de_chant/place_chant_02_07_2024.gpkg"))
lek_locations_vect <- project(lek_locations_vect,y="+proj=longlat +datum=WGS84")
lek_locations_vect_lambert <- project(lek_locations_vect,y="+init=epsg:2154")
# transform lek_locations_vect in spatial object with metadata
lek_sites<-st_as_sf(lek_locations_vect)
lek_sites_lambert<-st_as_sf(lek_locations_vect_lambert)
# to apply a buffer around the lek sites
lek_sites$larger_lek<-st_buffer(lek_sites$geometry, 100) # 100m
lek_sites_lambert$larger_lek<-st_buffer(lek_sites_lambert$geometry, 100) # 100m
#********************************************************************


### Shape the study area ----
#********************************************************************
#e <- extent(971000,985000,6471000,6486000)
e1<-ext(borders_3V_vect)
e2<-ext(as.polygons(ext(data_bg_3V), crs=crs(data_bg_3V)))

e<-c(min(e1[1],e2[1])-1000,max(e1[2],e2[2])+1000,min(e1[3],e2[3])-1000,max(e1[4],e2[4])+1000) 
e_poly<-(as.polygons(ext(e), crs=crs(data_bg_3V)))
# ext(e_poly)

# change the coordinate system of the SpatVector from (9..,9..,6..,6..) to (6..,6..,45..,45..)
# e_poly_WGS84<-project(e_poly, y="+proj=longlat +datum=WGS84")
# e_WGS84<-ext(e_poly_WGS84)
# e<- as.numeric(as.vector(e_WGS84))
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

### Loading heavy saved models ----
#********************************************************************
# WGS84 files

# load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/WGS84_models/best_model_saved_hiver_malefemelle_WGS84.RData")
# assign("best_model_saved_hiver_malefemelle_WGS84",best_model)
# load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/WGS84_models/grouse_winter_akde_saved_hiver_malefemelle_WGS84.RData")
# assign("grouse_winter_akde_saved_hiver_malefemelle_WGS84",grouse_winter_akde)
# load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/WGS84_models/grouse_winter_telemetry_hiver_malefemelle_WGS84.RData")
# assign("grouse_winter_telemetry_hiver_malefemelle_WGS84",grouse_winter_telemetry)

# # Lambert93 files
# 
# load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/Lambert93_models/best_model_saved_hiver_malefemelle.RData")
# assign("best_model_saved_hiver_malefemelle",best_model)
# load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/Lambert93_models/grouse_winter_akde_saved_hiver_malefemelle.RData")
# assign("grouse_winter_akde_saved_hiver_malefemelle",grouse_winter_akde)
# load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/Lambert93_models/grouse_winter_telemetry_hiver_malefemelle.RData")
# assign("grouse_winter_telemetry_hiver_malefemelle",grouse_winter_telemetry)

# No coordinate system specify (data=Lambert93 and WGS84 transformation in functions) 
# 
# load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/season_default_coord_syst/best_model_fit_akde/grouse_best_model_fit_akde.RData")
# assign("best_model_saved_hiver_malefemelle",best_model_3V)
# load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/season_default_coord_syst/akde/grouse_akde.RData")
# assign("grouse_winter_akde_saved_hiver_malefemelle",grouse_3V_akde)
# load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/season_default_coord_syst/telemetry/grouse_telemetry.RData")
# assign("grouse_winter_telemetry_hiver_malefemelle",grouse_3V_telemetry)

load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/season_default_coord_syst/best_model_fit_akde/grouse_best_model_fit_akde.RData")
assign("best_model_saved_hiver_malefemelle",best_model)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/season_default_coord_syst/akde/grouse_akde.RData")
assign("grouse_winter_akde_saved_hiver_malefemelle",grouse_winter_akde)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/season_default_coord_syst/telemetry/grouse_telemetry.RData")
assign("grouse_winter_telemetry_hiver_malefemelle",grouse_winter_telemetry)
#********************************************************************



#### 1.3.1_Creation of the telemetry object with birds locations ----
#********************************************************************

#focus on bird locations in winter season
# grouse_winter_raw<-as.data.frame(data_bg_3V_synth_fusion%>%filter(saison==season)%>%filter(sexe %in% sex))
grouse_winter_raw<-as.data.frame(bg_3V_data)

#' Create a dt formatted for importation in movebank 
# grouse_winter_pretelemetry<-pre_telemetry(grouse_winter_raw,"Lambert93")
grouse_winter_pretelemetry<-pre_telemetry(grouse_winter_raw,"WGS84")
grouse_winter_pretelemetry<- grouse_winter_pretelemetry%>% filter((animal.ID != "Eros") & (animal.ID !="Emilien")) # the pb for 2021-05-14 = Emilien and the pb for 2021-06-11 = Eros
write.csv(as.data.frame(grouse_winter_pretelemetry[,!names(grouse_winter_pretelemetry) %in% c("geometry","geometry_capture")]), row.names=FALSE, file="C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/pretelemetry_bg_3V_WGS84.csv")

#create a list of data.frames for each animal
grouse_winter<-multiple_dt_indiv(grouse_winter_raw,"nom")


not<-negate('%in%')


# # Removing the first day of location data as it often shows some unrealistic movements
for(i in 1:length(grouse_winter_pretelemetry))
{
  grouse_winter_pretelemetry[[i]] <- grouse_winter_pretelemetry[[i]] %>% filter (study.local.timestamp >= (first(grouse_winter_pretelemetry[[i]]["study.local.timestamp"]) + ddays(1) ))
  
}

# Removing the bird with less than 50 GPS positions data after removing the first day of movements
# The estimation of the temporal autocorrelation between points, using plot(variogram(SVF_object),CTMM=ctmm_object) in ctmm, cannot be estimated with Escartefigue (53 locations) but with Dameur (83 locations it is ok)
# often the programmation is 1 loc/h between 4:00 am and 8:00 pm UTC --> so at least 15 loc/day * 7 days = 105 loc ~ at least 100 loc are necessary to be able to do estimations
grouse_winter_pretelemetryetry<- grouse_winter_pretelemetry[sapply(grouse_winter_pretelemetry, function(x) dim(x)[1]) > 100]




for(i in 1:length(grouse_winter_pretelemetry))
{
  # keep only necessary variables
  grouse_winter_pretelemetry[[i]] <- grouse_winter_pretelemetry[[i]] %>% 
    select(individual.local.identifier,
           timestamp,
           location.long,
           location.lat,
           WGS84,
           sensor.type,marque_tag,energy,
           sexe,age,
           saison, saison2,
           period_jour, height.above.mean.sea.level
    )
}

save(grouse_winter_telemetry, file=paste0(base,"/3_R/heavy_saved_models/Lambert93_models/grouse_winter_telemetry_",season,"_",paste0(sex,collapse=""),"_Lambert93.RData"))

grouse_winter_telemetry<-list()
for(i in 1:length(grouse_winter_pretelemetry))
{ 
  # grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_pretelemetry[[i]],datum="WGS84",keep=c("saison","saison2","period_jour","sexe","age"))
  grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_pretelemetry[[i]],projection="+init=epsg:2154",keep=c("saison","saison2","period_jour","sexe","age"))
  # grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_pretelemetry[[i]],datum="WGS84")
  grouse_winter_telemetry[[i]]["x"] <- grouse_winter_telemetry[[i]]["longitude"]
  grouse_winter_telemetry[[i]]["y"] <- grouse_winter_telemetry[[i]]["latitude"]
}


# create a list with bird's names to plot the correct legend
vect_nicknames<-list()
for(i in (1:length(grouse_winter_pretelemetry)))
{
  vect_nicknames[[i]]<-unique(grouse_winter_pretelemetry[[i]]["individual.local.identifier"])
}
vect_nicknames<-unlist(vect_nicknames)
vect_nicknames<-as.vector(vect_nicknames)

# renamed each data frame from the list of data frames by the name of each bird 
names(grouse_winter_pretelemetry)<-vect_nicknames
names(grouse_winter_telemetry)<-vect_nicknames

# creation of a list() where each element will corresponds to a season (winter 1, 2 or 3) for a bird i in grouse_winter_telemetry.

for(i in 1:length(grouse_winter_telemetry))
{
  grouse_winter_telemetry_saison2<-list()
  for(j in 1:length(unique(grouse_winter_telemetry[[i]]$saison2)))
  {
    # The list() will contain as many dataframes as seasons the bird i lived. 
    grouse_winter_telemetry_saison2[[j]]<-grouse_winter_telemetry[[i]][grouse_winter_telemetry[[i]]$saison2==unique(grouse_winter_telemetry[[i]]$saison2)[j],]
  }
  names(grouse_winter_telemetry_saison2)<-unique(grouse_winter_telemetry[[i]]$saison2)
  # Save the list() of dataframes by seasons the bird i lived into the list of dataframes telemmetry objects for each bird i. 
  grouse_winter_telemetry[[i]]<-grouse_winter_telemetry_saison2
}
grouse_3V_telemetry<-grouse_winter_telemetry

# save(grouse_3V_telemetry, file=paste0(base,"/3_R/heavy_saved_models/season_default_coord_syst/telemetry/black_grouse_telemetry.RData"))



#### 1.3.2_Creation of the akde object, for home-range and RSF estimations on each bird of the Trois Vallées ski resort ####
#********************************************************************
#' Fit ctmm model : Continuous-Time Movement Modeling

# Model fitting and selection first requires a prototype model with guesstimated parameters 
#' ctmm.guess --> large approximation of the maximum likelyhood, inaccurate estimation of the parameters (VS ctmm.fit() Performs maximum likelihood parameter and interval estimation of ctmm models using each model’s exact likelihood function)

grouse_winter_guess <- lapply(grouse_winter_telemetry, function(df) {
  lapply(df, function(sub_df) {
    ctmm.guess(sub_df, CTMM = ctmm(isotropic = TRUE), interactive = FALSE)
  })
})

#plots a variogram object overlayed with a continuous-time movement model guesstimated from the variogram's shape
# isotropic = TRUE beacuse we consider the home range (espace vital) 
# as a sphere (attractor center), 
# even if an ellipse is more realistic (anisotropy) 
# but the function is not optized with (isotropic=F)

#model selected (approximation of the parameters)
grouse_winter_guess_summary<-lapply(grouse_winter_guess,function(guess_list_sumup) # to apply the function describe after to each element of "grouse_winter_guess"
{
  lapply(guess_list_sumup, summary) # to apply the function "summary()" at each element of "guess_list_sumup"
})


# selection of the 5 best model structures
fitted_models_grouse_winter <- lapply(grouse_winter_telemetry, function(fit_model_ctmm) {
  lapply(seq_along(fit_model_ctmm), function(j) {
    ctmm.select(fit_model_ctmm[[j]], CTMM = grouse_winter_guess[[j]], verbose = TRUE)
  })
})

# summary
fitted_models_grouse_winter_summary<-lapply(fitted_models_grouse_winter,function(fit_sumup)
{
  lapply(fit_sumup,summary,unit=F) 
})

# retrieve the best model for each seasonal dataset
best_model_3V <- lapply(fitted_models_grouse_winter, function(fitted_models) {
  lapply(fitted_models, function(sub_fitted_model) {
    sub_fitted_model[[1]]
  })
})

#set names
for(i in 1:length(best_model_3V))
{
  names(best_model_3V[[i]])<-names(grouse_3V_telemetry[[i]])
}

save(best_model_3V, file=paste0(base,"/3_R/heavy_saved_models/season_default_coord_syst/best_model_fit_akde/best_model_3V.RData"))




# Home range calculation                                                                                                                                       

# grouse_3V_akde <- lapply(grouse_winter_telemetry, function(guess_model) {
#   lapply(seq_along(guess_model), function(j) {
#     akde(guess_model[[j]], CTMM = best_model_3V[[j]])
#   })
# })

# Home range calculation                                                                                                                                       

grouse_3V_akde <- lapply(seq_along(grouse_3V_telemetry), function(i) {
  lapply(seq_along(grouse_3V_telemetry[[i]]), function(j) {
    akde(grouse_3V_telemetry[[i]][[j]], CTMM = best_model_3V[[i]][[j]])
  })
})

#set names
names(grouse_3V_akde)<-names(grouse_3V_telemetry)

#set names
for(i in 1:length(grouse_3V_akde))
{
  names(grouse_3V_akde[[i]])<-names(grouse_3V_telemetry[[i]])
}

save(grouse_3V_akde, file=paste0(base,"/3_R/heavy_saved_models/season_default_coord_syst/akde/grouse_3V_akde.RData"))


#********************************************************************









#### 2_Fitting an RSF ----
#********************************************************************

# MNT 

#' Create named list of rasters in the WGS84 format (do not accept the format RasterLayer)
r_mnt_9_WGS84<-raster(mnt_9_WGS84)
# check NA values
plot(is.na(r_mnt_9_WGS84))

raster_list <- list("mnt" = r_mnt_9_WGS84,"mnt_squared" = r_mnt_9_WGS84^2)
# raster_list <- list("habitat"=as.factor(r_carto_habitats_3V_9_WGS84))

#pb : my raster is not in background so the function extract()[--> extracting values of the raster under my telemetry points] inside rsf.fit() can not work.
bird=1

plot(raster_list[[1]])
plot(grouse_winter_telemetry_hiver_malefemelle_WGS84[[bird]],raster_list[[1]])
# poly_bird<-SpatialPolygonsDataFrame.UD(grouse_winter_akde_saved_hiver_malefemelle_WGS84[[bird]], level.UD = 0.95, level = 0.95)
# plot(poly_bird,add=T)


grouse_winter_rsf_riemann<-rsf.fit(grouse_winter_telemetry_hiver_malefemelle_WGS84[[bird]], 
                                   grouse_winter_akde_saved_hiver_malefemelle_WGS84[[bird]], 
                                   R=raster_list,  
                                   integrator = "Riemann",
                                   reference=c(1,1))
summary(grouse_winter_rsf_riemann)
grouse_winter_rsf_riemann$beta # coefs beta of the model

# check distribution of bird positions depending on the altitude 
g_ani<-ggplot()+
  geom_histogram(data=as.data.frame(data_bg_3V) %>% filter(ani_nom==grouse_winter_telemetry_hiver_malefemelle_WGS84[[bird]]@info$identity),aes(mnt_altitude))+
  ggtitle(grouse_winter_telemetry_hiver_malefemelle_WGS84[[bird]]@info$identity)+
  xlab("Altitude DEM (m)")

# check distribution of mnt value in the study area 
g_raster<-ggplot()+
  geom_histogram(data=as.data.frame(mnt_9),aes(mnt_ign))+
  ggtitle("Study area")+
  xlab("Altitude DEM (m)")

grid.arrange(g_ani,g_raster,ncol=2)

# HABITAT CARTOGRAPHY

#' Create named list of rasters in the WGS84 format (do not accept the format RasterLayer)
r_carto_habitats_3V_9_WGS84<-raster(carto_habitats_3V_9_WGS84) # convert the Spat Raster into a Raster Layer
r_carto_habitats_3V_9_WGS84<-raster::ratify(r_carto_habitats_3V_9_WGS84) # Convert the values to factors (categorical data) : ratify() helps create the Raster Attribute Table

# Set the levels explicitly (from 1 to 16)
rat <- levels(r_carto_habitats_3V_9_WGS84)[[1]]
hab_categories = data.frame(ID=c(20,21,22,23,30,31,32,40,50,51,52,60,92,93,94,100), categories = c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified"))
r_hab_categories<-left_join(rat,hab_categories,by="ID")
r_hab_categories$ID <- 1:nrow(rat)
levels(r_carto_habitats_3V_9_WGS84) <- r_hab_categories

dataType(r_carto_habitats_3V_9_WGS84) # "FLT4S" = 4-byte (32-bit) floating point numbers, for continuous numbers VS "INT4U" = 4-byte (32-bit) unsigned integer

# check NA values
plot(is.na(r_carto_habitats_3V_9_WGS84))

raster_list <- list("habitat"=r_carto_habitats_3V_9_WGS84)


bird=1

plot(raster_list[[1]])
plot(grouse_winter_telemetry_hiver_malefemelle_WGS84[[bird]],raster_list[[1]])



grouse_winter_rsf_riemann<-rsf.fit(grouse_winter_telemetry_hiver_malefemelle_WGS84[[bird]], 
                                   grouse_winter_akde_saved_hiver_malefemelle_WGS84[[bird]], 
                                   R=raster_list,  
                                   integrator = "Riemann",
                                   reference=6) # 6 = Herbaceous

summary(grouse_winter_rsf_riemann)
grouse_winter_rsf_riemann$beta # coefs beta of the model

ggplot()+
  geom_histogram(data=as.data.frame(data_bg_3V) %>% filter(ani_nom==grouse_winter_telemetry_hiver_malefemelle_WGS84[[bird]]@info$identity),aes(mnt_altitude))+
  ggtitle(grouse_winter_telemetry_hiver_malefemelle_WGS84[[bird]]@info$identity)+
  xlab("Altitude DEM (m)")





# STRAVA

#' Create named list of rasters in the WGS84 format (do not accept the format RasterLayer)
r_strava<-raster(strava)
r_strava_WGS84<-raster(strava_WGS84)

#extract the polygon shape of the akde (https://groups.google.com/g/ctmm-user/c/wtBXI4P7-7k)
poly_95_bird<-SpatialPolygonsDataFrame.UD(grouse_3V_akde[[bird]][["hiver1"]],level.UD=0.95,level=0.95)
poly_95_bird<-st_as_sf(poly_95_bird)
# crop the raster to the polygon extents
r_strava_cropped <- crop(r_strava, extent(poly_95_bird))

# check NA values
plot(is.na(r_strava_cropped))
plot(is.na(r_strava))

raster_list <- list("strava" = r_strava_WGS84)
plot(raster_list[[1]])

#pb : my raster is not in background so the function extract()[--> extracting values of the raster under my telemetry points] inside rsf.fit() can not work.
bird=1

plot(grouse_3V_telemetry[[bird]][["hiver1"]],grouse_3V_akde[[bird]][["hiver1"]])
plot(grouse_3V_telemetry[[bird]][["hiver1"]],raster_list[[1]])
# poly_bird<-SpatialPolygonsDataFrame.UD(grouse_winter_akde_saved_hiver_malefemelle_WGS84[[bird]], level.UD = 0.95, level = 0.95)
# plot(poly_bird,add=T)


grouse_winter_rsf_riemann<-rsf.fit(grouse_3V_telemetry[[bird]][["hiver1"]], 
                                   grouse_3V_akde[[bird]][["hiver1"]], 
                                   R=raster_list,  
                                   integrator = "Riemann")

summary(grouse_winter_rsf_riemann)
grouse_winter_rsf_riemann$beta # coefs beta of the model




# MULTIPLE RASTER
r_mnt<-raster(mnt_9)
r_carto_habitats_3V_9<-raster(carto_habitats_3V_9)
raster_list <- list("mnt" = r_mnt,"habitat"=r_carto_habitats_3V_9)

grouse_winter_rsf_riemann<-rsf.fit(grouse_winter_telemetry_hiver_malefemelle_Lambert93[[bird]], 
                                   grouse_winter_akde_saved_hiver_malefemelle[[bird]], 
                                   R=raster_list,  
                                   integrator = "Riemann",
                                   reference=c(1,6)) # 6 = Herbaceous




grouse_winter_rsf_riemann<-rsf.fit(grouse_winter_telemetry[[1]], grouse_winter_akde[[1]], R=raster_list,  integrator = "Riemann")
summary(grouse_winter_rsf_riemann) 


raster::readAll(r_slope_3V_9) # to save the raster (not Spatraster) in the RAM and save time 

rsf_list <- lapply(1:length(grouse_winter_telemetry_hiver_malefemelle), function(i){
  ctmm::rsf.fit(grouse_winter_telemetry_hiver_malefemelle[[i]], grouse_winter_akde_saved_hiver_malefemelle[[i]],  integrator = "Riemann")
})

#' The integrator = "Riemann" option is still experimental, we use it here because it is much faster
grouse_winter_rsf_riemann<-list()
for (i in 1: length(grouse_winter_telemetry_hiver_malefemelle))
{
  grouse_winter_rsf_riemann[[i]]<-rsf.fit(grouse_winter_telemetry_hiver_malefemelle[[i]], grouse_winter_akde_saved_hiver_malefemelle[[i]],  integrator = "Riemann")
}

# R = must be a list of rasters to fit Poisson regression coefficients to (under a log link)

grouse_winter_rsf_riemann_summary<-lapply(grouse_winter_rsf_riemann,summary)




# RSF at population-level

# RSF.list <- list( RSF.1,  RSF.2) # with RSF.1 = RSF for Abel, RSF.2 = RSF for Alpha...
# RSF.population <- mean(RSF.list)
# summary(RSF.population)




#### histogram mnt positions

ggplot()+
  geom_histogram(data=data_bg_3V_synth_fusion,aes(mnt_altitude),fill="red",alpha=0.4)+
  xlab("Altitude (m) from DEM")+ #DEM = Digital Elevation Model 
  ggtitle("Distribution of DEM values related to bird locations")



#' Range distribution (includes the ranging behaviour) (étendue de la distribution)
agde_grouse_winter<-list()
for (i in 1: length(grouse_winter_telemetry))
{
  agde_grouse_winter[[i]]<-agde(CTMM = grouse_winter_rsf_riemann[[i]],raster_list)
}


windows()
par(mfrow=c(1,5))
for ( i in (1:length(grouse_winter_telemetry)))
{
  plot(agde_grouse_winter[[i]],main=vect_nicknames[[i]])
}



mean_rsf<-ctmm:mean(grouse_winter_rsf_riemann)
mean_akde<-ctmm:mean(grouse_winter_akde)

#********************************************************************

#rsf with more raster than the slope

#' Create named list of rasters
be2 <- list("slope1" = slope_3V_9,"high_vegetation"=raster_high_vege_classif_9m_9)

#' The integrator = "Riemann" option is still experimental, we use it here because it is much faster
grouse_winter_rsf_riemann2<-list()
for (i in 1: length(grouse_winter_telemetry))
{
  grouse_winter_rsf_riemann2[[i]]<-rsf.fit(grouse_winter_telemetry[[i]], grouse_winter_akde[[i]], R = be2, integrator = "Riemann")
}

# R = must be a list of rasters to fit Poisson regression coefficients to (under a log link)

grouse_winter_rsf_riemann2_summary<-lapply(grouse_winter_rsf_riemann2,summary)

#********************************************************************





#### 3_Visualising GPS locations and home ranges of multiple birds ####
#### 3.1_Viewing imported maps ----
#********************************************************************

#raster::readAll(slope_3V) # to save a raster in the RAM (intern memory of the computer), to save time

# carte d'occupation des sols OSO (produite par le Centre d'Expertise Scientifique sur l'occupation des sols (CES OSO))
# oso <- terra::rast("M:/CESBIO/OSO_20220101_RASTER_V1-0/DATA/OCS_2022.tif") 
# oso <- terra::crop(oso,e)

par(mfrow=c(2,4))

plot(slope_3V,main="Slope(°)\nresolution=1m",col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"))
plot(borders_3V_vect,add=T) # add 3V borders
plot(slope_3V_9,main="Slope(°)\nresolution=9m",col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"))
plot(borders_3V_vect,add=T) # add 3V borders
plot(carto_habitats_3V,main="Habitat cartography\nresolution=1m")
plot(borders_3V_vect,add=T) # add 3V borders
# plot(oso,main="OSO\nresolution=10m")
# plot(borders_3V_vect,add=T) # add 3V borders
plot(strava,main="Strava, 4 attendance levels\nresolution=1m")
plot(borders_3V_vect,add=T) # add 3V borders
plot(mnt,main="MNT\nresolution=1m",col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"))
plot(borders_3V_vect,add=T)
plot(mnt_9,main="MNT\nresolution=9m",col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"))
plot(borders_3V_vect,add=T)
plot(mnt,main="MNT\nresolution=1m",col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"))
plot(lek_locations_vect,main="Leks",add=T)
plot(borders_3V_vect,add=T)
plot(borders_3V_vect_WGS84,border=as.factor(borders_3V_vect_WGS84$NOM),lwd=2)

# plot mnt with a mask around 3V
# plot(mnt_9,borders_3V_vect,mask=T)

par(mfrow=c(1,1))

#View habitat cartography realised by Clara Leblanc
ggplot()+
  geom_spatraster(data=carto_habitats_3V)+
  geom_sf(data = borders_3V_vect,fill=NA,color="black",lwd =2)+
  scale_fill_manual(
    values = c("#CC9966","#CCCCCC","#666666","#333333","#99CC99","#FFFF66","#FFCCCC","#FF99CC","#99FF99","#99FF00","#339966","#993300","#99CCFF","#99FFFF","#0066FF","white","white"),
    breaks = c("20","21","22","23","30","31","32","40","50","51","52","60","92","93","94","100"),
    # labels = c("sol non classé",sol mineral fin","sol mineral grossier","falaise", "pelouse seche ou rocheuse","herbacées,"ligneux bas","arbustes","arbres non classés,"arbres feuillus","arbres resineux","bati","plan d'eau naturel","plan d'eau artificiel","cours d'eau",  "non classe" ))+
    labels = c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified" ))+
  labs( title="Habitat cartography",
        x = "Longitude",
        y = "Latitude",
        fill = "Legend")
#********************************************************************







#### 3.2_Visulising GPS winter locations of 3V birds ----
#********************************************************************


windows()
#View habitat cartography realised by Clara Leblanc
ggplot()+
  geom_spatraster(data=carto_habitats_3V)+
  geom_point(data = bind_rows(grouse_winter_pretelemetry, .id="df"),aes(x=X_GPS_lambert93,y=Y_GPS_lambert93,colour = "black"))+
  scale_colour_manual(values="black")+
  geom_sf(data = borders_3V_vect,fill=NA,color="black",lwd =2)+
  scale_fill_manual(
    values = c("#CC9966","#CCCCCC","#666666","#333333","#99CC99","#FFFF66","#FFCCCC","#FF99CC","#99FF99","#99FF00","#339966","#993300","#99CCFF","#99FFFF","#0066FF","white","white"),
    breaks = c("20","21","22","23","30","31","32","40","50","51","52","60","92","93","94","100"),
    # labels = c("sol non classé",sol mineral fin","sol mineral grossier","falaise", "pelouse seche ou rocheuse","herbacées,"ligneux bas","arbustes","arbres non classés,"arbres feuillus","arbres resineux","bati","plan d'eau naturel","plan d'eau artificiel","cours d'eau",  "non classe" ))+
    labels = c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified" ))+
  labs( title="Habitat cartography",
        x = "Longitude",
        y = "Latitude",
        fill = "Legend")



# plot the birds
g_positions_birds<-ggplot()+
  #geom_raster(data=map_df,aes(x=X_GPS, y=Y_GPS,fill=slope))+
  geom_spatraster(data=slope_3V_9)+
  geom_sf(data = borders_3V_vect,fill=NA,color="black",lwd =2)+
  
  #display birds all positions
  geom_point(data = bind_rows(grouse_winter_pretelemetry, .id="df"),aes(x=X_GPS_lambert93,y=Y_GPS_lambert93,colour = df))+
  
  theme_bw() +
  #coord_equal() +
  xlim(e[1],e[2])+
  ylim(e[3],e[4])+
  scale_fill_gradientn("Slope (°)", limits=c(0,90),colours=c("#FFFFFF","#CCCCCC" ,"#666666","#333333","#000000")) +
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
  scale_color_discrete(name = "Animals",labels=vect_nicknames)+
  ggtitle("All positions of the 38 GPS-tagged birds at the 3 Valleys site (winter)")+
  xlab("Longitude")+
  ylab("Latitude")

windows()
g_positions_birds



# plot capture sites
g_positions_capture_sites<-ggplot()+
  #geom_raster(data=map_df,aes(x=X_GPS, y=Y_GPS,fill=slope))+
  geom_spatraster(data=slope_3V_9)+
  geom_sf(data = borders_3V_vect,fill=NA,color="black",lwd =2)+
  
  #display bird capture positions
  geom_sf(data = lek_sites,aes(geometry = larger_lek), fill="green")+
  
  theme_bw() +
  #coord_equal() +
  xlim(e[1],e[2])+
  ylim(e[3],e[4])+
  scale_fill_gradientn("Slope (°)", limits=c(0,90),colours=c("#FFFFFF","#CCCCCC" ,"#666666","#333333","#000000")) +
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
  # scale_colour_manual(values="black")+
  # scale_color_discrete(name = "Capture zones")+
  ggtitle("Lek sites in the Trois Vallées skiing area")+
  xlab("Longitude")+
  ylab("Latitude")

windows()
g_positions_capture_sites
#********************************************************************





#### 3.3_Home range vizualization on each bird of the Trois Vallées ski resort ----
#********************************************************************

# visualizing the home range density estimates against the position data of each bird of the Trois Vallées

source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multi_graph_home_range.R")
HR_statistics<-multi_graph_HR(writeplot = FALSE, proj = "+init=epsg:2154")

source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/visu_home_range.R")
visu_HR(colorby="4seasons",writeplot=TRUE)

# visualizing the home range density estimates per sex in the Trois Vallées
visu_HR("mnt","hiver",colorby="sex",showpoints=TRUE,writeplot = TRUE) 
visu_HR("strava","hiver",colorby="sex",showpoints=TRUE,writeplot = TRUE) 
visu_HR("strava","hiver",showpoints=TRUE,writeplot = TRUE) 

visu_HR("mnt","hiver",showleks=TRUE,showpoints=TRUE,writeplot = TRUE) 
visu_HR("mnt","automne",showleks=TRUE,showpoints=TRUE,writeplot = TRUE)
visu_HR("mnt","ete",showleks=TRUE,showpoints=TRUE,writeplot = TRUE)
visu_HR("mnt","printemps",showleks=TRUE,showpoints=TRUE,writeplot = TRUE)

# visualizing the home range density estimates and the capture site for each bird of the Trois Vallées
color_birds_hiver<-read.table("C:/Users/albordes/Documents/PhD/TetrAlps/5_OUTPUTS/RSF/home_range_akde/mean_size_HR_season/color_birds_hiver.txt", sep = "", header = TRUE) # Color dataframe from plot_mean_area_HR("hiver") to associate a specific color with each home range and capture location for a given bird.
visu_HR(season="hiver",colorby="indiv",color_palette=color_birds_hiver,writeplot = TRUE)


# Summary of mean size home-range for a given season

plot_mean_area_HR(season="hiver",colorby="sex",synth_bg_3V,writeplot = TRUE)
plot_mean_area_HR(season="hiver",data_sex=synth_bg_3V,writeplot = TRUE)

plot_mean_area_HR(season="automne",data_sex=synth_bg_3V,writeplot = TRUE)

plot_mean_area_HR(season="ete",data_sex=synth_bg_3V,writeplot = TRUE)

plot_mean_area_HR(season="printemps",data_sex=synth_bg_3V,writeplot = TRUE)


# funnel(x=grouse_winter_akde_saved_hiver_malefemelle,y=grouse_winter_telemetry_hiver_malefemelle,sort=TRUE,col=c($colour,"black"),main="Home-range size estimations at 95% in winter")
# cluster(grouse_winter_akde_saved_automne_malefemelle,sort=TRUE,main="Home-range size estimations at 95% in Automne n\Membership test for subpopulation")


################ Home range VS capture site
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/distance_home_range_capture_site.R")
HR_dist_capture(season="hiver",sex="malefemelle",writeplot = TRUE)
HR_dist_capture(season="hiver",sex="femelle")
HR_dist_capture(season="hiver",sex="male")
HR_dist_capture(season="automne",sex="malefemelle",writeplot = TRUE)
HR_dist_capture(season="printemps",sex="malefemelle",writeplot = TRUE)
HR_dist_capture(season="ete",sex="malefemelle",writeplot = TRUE)

HR_dist_capture(season="hiver",sex="malefemelle",writeplot = FALSE)
HR_dist_capture(season="automne",sex="malefemelle",writeplot = FALSE)
HR_dist_capture(season="printemps",sex="malefemelle",writeplot = FALSE)
HR_dist_capture(season="ete",sex="malefemelle",writeplot = FALSE)
#********************************************************************



#### 4_Describing the spatial correlations between observation data ####
#### 4.1_Sampling schedule ####
#********************************************************************
# Pooling Variograms : If multiple individuals exhibit similar movement behaviors

# To prospect if the sampling there is a regular or irregular sampling schedule 
# or to visualyze data when the sampling rate changes during data collection
par(mfrow=c(1,1))
dt.plot(grouse_winter_telemetry,main="Log-scale plot of the sorted sampling interval between 2 consecutive GPS positions") #here the sampling is semi-irregular 

# visualizing the irregular sampling schedule 

dt_box<-data.frame("birds"=1:length(grouse_winter_telemetry),"interval_hr"=summary(grouse_winter_telemetry)$interval,"animal"=vect_nicknames)

ggplot(data=dt_box[-33,],aes(x=birds,y=interval_hr))+
  geom_boxplot()+
  # scale_y_continuous(trans = 'log10')+
  geom_dotplot(binaxis="y", stackdir='center', dotsize=0.5)+
  xlab("Birds")+
  ylab("Time interval between 2 positions (hours)")+
  ggtitle("Visualizing the sampling schedule of GPS positions")+
  theme(plot.title = element_text(size=22, face="bold"),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))

windows()
ggplot(data=bind_rows(grouse_winter_pretelemetry, .id="df"),aes(x=fct_reorder(individual.local.identifier,(dt),.na_rm=T),y=(dt)))+
  geom_boxplot()+
  #geom_dotplot(binaxis="y", stackdir='center', dotsize=0.2)+
  xlab("Birds")+
  ylab("Time interval between 2 positions (day:hr:min)")+
  ggtitle("Visualizing the sampling schedule of GPS positions")+
  scale_y_time(limits=c(0,60000))+
  theme(plot.title = element_text(size=22, face="bold"),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14,angle=90,vjust=0.5),
        axis.text.y = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))


#### 4.2_Positions autocorrelation ----

# Visualizing the SVF of the guess model and comparison of the 2 best fitted models on variogram
# and save

# The variogram, or semivariogram, plots time lags on the x-axis for all pairs of observations
# against their semi-variance (half of the variance for the distance each observation pair) on the y-axis. (Animove 2022)


# POPULATION VARIOGRAM
png(filename = paste0(base, "/5_OUTPUTS/RSF/variograms/population_variogram_timelags=",paste(timelags, collapse = "_"),".png"),height = 1000,width = 2400) # Naming files correctly

par(mfrow=c(1,2))
# assuming homogenous sampling schedule (which is not the case)
SVF_0 <- lapply(grouse_winter_telemetry_hiver_malefemelle_WGS84,variogram) # population variogram
SVF_0<- mean(SVF_0)
plot(SVF_0,fraction=0.1,level=c(0.5,0.95))
title("Population variogram - winter\n(assuming homogenous sampling schedule)",cex.main=3,line = -2)

# considering the irregular sampling schedule
timelags <- c(1,8)
timelags_sec <- timelags %#% "hour" # the order has no importance
SVF <- lapply(grouse_winter_telemetry_hiver_malefemelle_WGS84,variogram,dt=timelags_sec) # population variogram considering the GPS were programmed to cycle between 1, 12, 24 hour sampling intervals
SVF<- mean(SVF)
plot(SVF,fraction=0.1,level=c(0.5,0.95),main=paste0("Population variogram - winter\n(considering the irregular sampling schedule\n with the most common time intervals : ",paste(timelags, collapse = "h, "),"h)"),cex.main=3,line = -5)

dev.off()


# INDIVIDUAL VARIOGRAM
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/SVF_indiv.R")
SVF_indiv(season="hiver",timelags_vect=c(1,2,4,8),writeplot=FALSE,proj="+proj=longlat +datum=WGS84")
SVF_indiv(season="hiver",timelags_vect=c(1,8),writeplot=FALSE,proj="+proj=longlat +datum=WGS84")

#************************************** not working anymore
# #  HISTOGRAM of time between 2 locations 
# par(mfrow=c(1,1))
# windows()
# 
# ggplot(data = bind_rows(grouse_winter_pretelemetry, .id="df"),aes(x=as.POSIXct(dt,format="%d %H:%M:%S")))+
#   geom_histogram(fill="#FF6666", bins = 50)+
#   facet_grid(df ~ .)+
#   scale_x_datetime(date_labels = "%d d %H h")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   xlab("Time between 2 locations")
# 
# ggplot(data = bind_rows(grouse_winter_pretelemetry, .id="df"),aes(x=as.POSIXct(dt,format="%d %H:%M:%S")))+
#   geom_histogram(fill="#FF6666", bins = 50)+
#   facet_grid(df ~ .)+
#   scale_x_datetime(date_labels = "%d d %H h %M min",breaks ="3 hours",expand = c(0, 0))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   xlab("Time between 2 locations")
#**************************************





























############### brouillon area

par(oma = c(1,1,1,1))
plot(mnt_9,ext=e,col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"),
     main="title_graph",
     xlab="Longitude",
     ylab="Latitude",
     cex.main=2,
     cex.lab = 1.5,
     plg = list(title = "DEM (m)",title.cex = 1.5,cex=1.2))
plot(borders_3V_vect,ext=e,add=TRUE,border="black",lwd=2)

for (i in 1:length(grouse_winter_telemetry_automne_malefemelle))
{
  
  plot(grouse_winter_telemetry_automne_malefemelle[[i]],UD=grouse_winter_akde_saved_automne_malefemelle[[i]],
       units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),col.grid=NA,bty="n",add=T,col.UD="green")
  
  # plot(grouse_winter_telemetry_automne_malefemelle[[i]],UD=grouse_winter_akde_saved_automne_malefemelle[[i]],
  #      units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),col.grid=NA,bty="n",col.UD="green")
}



View(grouse_winter_telemetry_hiver_malefemelle_WGS84[[1]])



load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/old/best_model_saved_hiver.RData")
assign("best_model_saved_hiver",best_model)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/old/grouse_winter_akde_saved_hiver.RData")
assign("grouse_winter_akde_saved_hiver",grouse_winter_akde)
load("C:/Users/albordes/Documents/PhD/TetrAlps/3_R/heavy_saved_models/old/grouse_winter_telemetry_hiver.RData")
assign("grouse_winter_telemetry_hiver",grouse_winter_telemetry)

View(grouse_winter_telemetry_automne_malefemelle[[1]])
plot(grouse_winter_telemetry_automne_malefemelle[[1]])
plot(grouse_winter_akde_saved_automne_malefemelle[[1]])

plot(borders_3V_vect,ext=e,border="black",lwd=2)
plot(grouse_winter_telemetry_automne_malefemelle[[2]],UD=grouse_winter_akde_saved_automne_malefemelle[[2]],add=TRUE,units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),col.grid=NA,bty="n",col.UD="green")





#### 1_Creation of telemetry with birds locations and akde objects for home range estimation ####
#### 1.3.1_Creation of the telemetry object with birds locations ----
#********************************************************************

# creation of a list() where each element will corresponds to a season (winter 1, 2 or 3) for a bird i in grouse_winter_telemetry.
for (i in seq_along(grouse_winter_telemetry)) {
  unique_saison2 <- unique(grouse_winter_telemetry[[i]]$saison2)
  grouse_winter_telemetry_saison2 <- vector("list", length = length(unique_saison2))
  
  for (j in seq_along(unique_saison2)) {
    grouse_winter_telemetry_saison2[[j]] <- grouse_winter_telemetry[[i]][grouse_winter_telemetry[[i]]$saison2 == unique_saison2[j], ]
  }
  
  grouse_winter_telemetry[[i]] <- grouse_winter_telemetry_saison2
}

grouse_winter_telemetry[[1]][grouse_winter_telemetry[[1]]$saison2 == unique_saison2[1], ]

grouse_winter_telemetry_saison2<-list()

for(i in seq_along(grouse_winter_telemetry))
{
  
  for(j in seq_along(unique(grouse_winter_telemetry[[i]]$saison2)))
  {
    # The list() will contain as many dataframes as seasons the bird i lived. 
    grouse_winter_telemetry_saison2[[j]]<-grouse_winter_telemetry[[i]][,grouse_winter_telemetry[[i]]$saison2==unique(grouse_winter_telemetry[[i]]$saison2)[j]]
  }
  # Save the list() of dataframes by seasons the bird i lived into the list of dataframes telemmetry objects for each bird i. 
  grouse_winter_telemetry[[i]]<-grouse_winter_telemetry_saison2
}



#### 1.3.2_Creation of the akde object, for home-range and RSF estimations on each bird of the Trois Vallées ski resort ####
#********************************************************************
#' Fit ctmm model : Continuous-Time Movement Modeling

# Model fitting and selection first requires a prototype model with guesstimated parameters 
#' ctmm.guess --> large approximation of the maximum likelyhood, inaccurate estimation of the parameters (VS ctmm.fit() Performs maximum likelihood parameter and interval estimation of ctmm models using each model’s exact likelihood function)

grouse_winter_guess <- lapply(grouse_winter_telemetry, function(df) {
  lapply(df, function(sub_df) {
    ctmm.guess(sub_df, CTMM = ctmm(isotropic = TRUE), interactive = FALSE)
  })
})

#plots a variogram object overlayed with a continuous-time movement model guesstimated from the variogram's shape
# isotropic = TRUE beacuse we consider the home range (espace vital) 
# as a sphere (attractor center), 
# even if an ellipse is more realistic (anisotropy) 
# but the function is not optized with (isotropic=F)

#model selected (approximation of the parameters)
grouse_winter_guess_summary<-lapply(grouse_winter_guess,function(guess_list_sumup) # to apply the function describe after to each element of "grouse_winter_guess"
{
  lapply(guess_list_sumup, summary) # to apply the function "summary()" at each element of "guess_list_sumup"
})


# selection of the 5 best model structures
fitted_models_grouse_winter <- lapply(grouse_winter_telemetry, function(fit_model_ctmm) {
  lapply(seq_along(fit_model_ctmm), function(j) {
    ctmm.select(fit_model_ctmm[[j]], CTMM = grouse_winter_guess[[j]], verbose = TRUE)
  })
})

# summary
fitted_models_grouse_winter_summary<-lapply(fitted_models_grouse_winter,function(fit_sumup)
{
  lapply(fit_sumup,summary,unit=F) 
})

# retrieve the best model for each seasonal dataset
best_model <- lapply(fitted_models_grouse_winter, function(fitted_models) {
  lapply(fitted_models, function(sub_fitted_model) {
    sub_fitted_model[[1]]
  })
})


# save(best_model, file=paste0(base,"/3_R/heavy_saved_models/Lambert93_models/best_model_saved_",season,"_",paste0(sex,collapse=""),"_Lambert93.RData"))


# Home range calculation                                                                                                                                       

grouse_winter_akde <- lapply(grouse_winter_telemetry, function(guess_model) {
  lapply(seq_along(guess_model), function(j) {
    akde(guess_model[[j]], CTMM = best_model[[j]])
  })
})

windows()
par(mfrow=c(2,2))
plot(grouse_winter_telemetry[[3]][[1]],UD=grouse_winter_akde[[3]][[1]])
plot(grouse_winter_telemetry[[3]][[2]],UD=grouse_winter_akde[[3]][[2]])

save(grouse_winter_akde, file=paste0(base,"/3_R/heavy_saved_models/Lambert93_models/grouse_winter_akde_saved_",season,"_",paste0(sex,collapse=""),"_Lambert93.RData"))
#********************************************************************


















### Brouillon area ----

#*************************************************************

#birds 1,2,6,8 have an home range for "hiver1"
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/plot_check_RSF_results.R")
plot_check_RSF_res(6,"habitats",analysis_object="study_area",subseason="hiver1",writeplot=TRUE)
plot_check_RSF_res(6,"habitats",analysis_object="HR",subseason="hiver1",writeplot=TRUE)
plot_check_RSF_res(8,"strava",analysis_object="study_area",subseason="hiver1",data_visu="continuous",writeplot=TRUE)
plot_check_RSF_res(8,"strava",analysis_object="HR",subseason="hiver1",data_visu="continuous",writeplot=TRUE)



# data exploration 
bird=1

#extract the polygon shape of the akde (https://groups.google.com/g/ctmm-user/c/wtBXI4P7-7k)
poly_95_bird<-SpatialPolygonsDataFrame.UD(grouse_winter_akde[[bird]][["hiver1"]],level.UD=0.95,level=0.95)
crs(poly_95_bird)<-"+init=epsg:2154"
poly_95_bird<-st_as_sf(poly_95_bird)

#subset the CI's and extract shape of middle contour
low<-paste(grouse_winter_akde[[bird]][["hiver1"]]@info$identity,"95% low")
high<-paste(grouse_winter_akde[[bird]][["hiver1"]]@info$identity,"95% high")
est<-paste(grouse_winter_akde[[bird]][["hiver1"]]@info$identity,"95% est")

poly_95_bird_est <- subset(poly_95_bird, name == est)
poly_95_bird_low <- subset(poly_95_bird, name == low)
poly_95_bird_high <- subset(poly_95_bird, name == high)

# Extract the bounding box coordinates
bbox <- st_bbox(poly_95_bird)
# Create a polygon from the bounding box coordinates
bbox_polygon <- st_as_sfc(st_bbox(poly_95_bird))
# Convert the polygon to an sf object
bbox_polygon_sf <- st_sf(geometry = bbox_polygon)
# Assign CRS to the bounding box polygon
st_crs(bbox_polygon_sf) <- st_crs(poly_95_bird)


#plot the home range polygon object for the bird
fgh<-ggplot()+
  geom_sf(data=poly_95_bird$geometry$`Abel 95% est`,color="blue",fill="blue",alpha=0.1)+  # incertitude : 95% low 
  geom_sf(data=poly_95_bird$geometry$`Abel 95% low`,color="black",fill=NA)+
  geom_sf(data=poly_95_bird$geometry$`Abel 95% high`,color="black",fill=NA)
fgh
#plot the home range polygon object for the bird
fgh2<-ggplot()+
  geom_sf(data = poly_95_bird_est, color = "blue", fill = "blue", alpha = 0.1) +  # incertitude: 95% low 
  geom_sf(data=poly_95_bird_low,color="red",fill=NA)+
  geom_sf(data=poly_95_bird_high,color="black",fill=NA)
fgh2

#plot the 95% estimated home range polygon object for the bird overlapping the habitat map
ggplot()+
  geom_spatraster(data=carto_habitats_3V)+
  geom_sf(data = borders_3V_vect,fill=NA,color="black",lwd =2)+
  scale_fill_manual(
    values = c("#CC9966","#CCCCCC","#666666","#333333","#99CC99","#FFFF66","#FFCCCC","#FF99CC","#99FF99","#99FF00","#339966","#993300","#99CCFF","#99FFFF","#0066FF","white","white"),
    breaks = c("20","21","22","23","30","31","32","40","50","51","52","60","92","93","94","100"),
    # labels = c("sol non classé",sol mineral fin","sol mineral grossier","falaise", "pelouse seche ou rocheuse","herbacées,"ligneux bas","arbustes","arbres non classés,"arbres feuillus","arbres resineux","bati","plan d'eau naturel","plan d'eau artificiel","cours d'eau",  "non classe" ))+
    labels = c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified" ))+
  # geom_sf(data=poly_95_bird,fill="red")+
  geom_sf(data=poly_95_bird_est,color="blue",fill="blue",alpha=0.1)+
  coord_sf(xlim = c(st_bbox(poly_95_bird)["xmin"], st_bbox(poly_95_bird)["xmax"]),
           ylim = c(st_bbox(poly_95_bird)["ymin"], st_bbox(poly_95_bird)["ymax"]))+
  labs( title="Habitat cartography",
        x = "Longitude",
        y = "Latitude",
        fill = "Legend")





# RSF ON HABITAT 
# crop the raster to the polygon extents
cropped_r_habitat <- crop(carto_habitats_3V, extent(poly_95_bird))



# Visualize the bounding box polygon
plot(bbox_polygon, col = "transparent", border = "red")


# Mask the raster using the polygon to get the values within the polygon only
masked_raster <- mask(cropped_r_habitat, poly_95_bird)

# Plot the mask
ggplot()+
  geom_spatraster(data=masked_raster)+
  scale_fill_manual(
    values = c("20"="#CC9966","21"="#CCCCCC","22"="#666666","23"="#333333","30"="#99CC99","31"="#FFFF66","40"="#FFCCCC","50"="#FF99CC","51"="#99FF99","52"="#99FF00","60"="#339966","92"="#993300","93"="#99CCFF","94"="#99FFFF","100"="#0066FF"),na.value ="transparent",
    labels = c("20"="Unclassified soil","21"="Fine mineral soil","22"="Coarse mineral soil","23"="Cliff","30"="Dry or rocky grassland","31"="Herbaceous", "40"="Low ligneous","50"="Shrubs","51"="Unclassified trees","52"="Deciduous trees","60"="Resinous trees","92"="Buildings","93"="Natural pond","94"="Artificial pond","100"="Waterway"))+
  labs( title=paste(grouse_winter_akde_saved_hiver_malefemelle[[bird]]@info$identity,"\nHabitat cartography"),
        x = "Longitude",
        y = "Latitude",
        fill = "Legend")+
  geom_point(data=grouse_winter_telemetry_hiver_malefemelle[[bird]],aes(x=longitude,y=latitude))

# Extract the values from the masked raster:
values_r_habitat <- getValues(raster(carto_habitats_3V))

# Remove NA values 
values_r_habitat <- values_r_habitat[!is.na(values_r_habitat)]

# Count the occurrence of each class
class_counts <- table(values_r_habitat)

# Calculate the proportion of each class:
total_pixels <- sum(class_counts)
class_proportions <- round((class_counts / total_pixels),3)
# Define class names
class_names <- c(
  "20" = "Unclassified soil",
  "21" = "Fine mineral soil",
  "22" = "Coarse mineral soil",
  "23" = "Cliff",
  "30" = "Dry or rocky grassland",
  "31" = "Herbaceous",
  "32" = "Low ligneous",
  "40" = "Shrubs",
  "50" = "Unclassified trees",
  "51" = "Deciduous trees",
  "52" = "Resinous trees",
  "60" = "Buildings",
  "92" = "Natural pond",
  "93" = "Artificial pond",
  "94" = "Waterway",
  "100" = NA
)

# Match the class names to the proportions
class_proportions_named <- as.data.frame(setNames(class_proportions, class_names[names(class_proportions)]))
colnames(class_proportions_named)<-c("Class","Proportion")
# Display the results
print(class_proportions_named)
# sum(class_proportions) # to check the sum = 1 = 100%

# Convert the class proportions to a data frame


# Create a pie chart using ggplot2
ggplot(class_proportions_named, aes(x="", y=Proportion, fill=Class)) +
  geom_bar(stat="identity", width=1, color="black") +
  scale_fill_manual(
    values = c("Unclassified soil"="#CC9966","Fine mineral soil"="#CCCCCC","Coarse mineral soil"="#666666","Cliff"="#333333","Dry or rocky grassland"="#99CC99","Herbaceous"="#FFFF66","Low ligneous"="#FFCCCC","Shrubs"="#FF99CC","Unclassified trees"="#99FF99","Deciduous trees"="#99FF00","Resinous trees"="#339966","Buildings"="#993300","Natural pond"="#99CCFF","Artificial pond"="#99FFFF","Waterway"="#0066FF"),na.value ="transparent")+
  coord_polar(theta="y") +
  theme_void() +
  labs(title=paste(grouse_winter_akde_saved_hiver_malefemelle[[bird]]@info$identity,"\nProportion of Habitat Classes")) 








# RSF ON STRAVA 
# apply a buffer on the raster
# strava_buffed<-buffer(strava,50)

# crop the raster to the polygon extents
cropped_r <- crop(strava, extent(poly_95_bird))

# Mask the raster using the polygon to get the values within the polygon only
masked_raster <- mask(cropped_r_habitat, poly_95_bird)

# Plot the mask
ggplot()+
  geom_spatraster(data=masked_raster)+
  scale_fill_gradientn(colors=c("#CCCCCC11","#FF6600","#FF3333"))+
  labs( title=paste(grouse_winter_akde_saved_hiver_malefemelle[[bird]]@info$identity,"\nStrava visitor intensity"),
        x = "Longitude",
        y = "Latitude",
        fill = "Legend")+
  geom_point(data=grouse_winter_telemetry_hiver_malefemelle[[bird]],aes(x=longitude,y=latitude))

# Extract the values from the masked raster:
values_r_habitat <- getValues(raster(strava))

# Remove NA values 
values_r_habitat <- values_r_habitat[!is.na(values_r_habitat)]

library(classInt)
# Séparer les valeurs égales à 0 des autres valeurs
values_zero <- values_r_habitat[values_r_habitat == 0]
values_non_zero <- values_r_habitat[values_r_habitat != 0]

# Déterminer les breaks de Jenks pour les valeurs non nulles
num_classes <- 3 # Nombre de classes Jenks pour les valeurs non nulles
jenks_breaks <- classInt::classIntervals(values_non_zero, n = num_classes, style = "jenks")

# Assigner chaque valeur non nulle à une classe
classified_values_non_zero <- cut(values_non_zero, breaks = jenks_breaks$brks, include.lowest = TRUE, labels = FALSE)

# Créer un vecteur complet pour les valeurs classifiées, en ajoutant une classe pour les valeurs égales à 0
classified_values <- rep(NA, length(values_r_habitat))
classified_values[values_r_habitat == 0] <- 1  # Classe 1 pour les valeurs égales à 0
classified_values[values_r_habitat != 0] <- classified_values_non_zero + 1  # Décalage des classes Jenks pour laisser la classe 1 aux valeurs égales à 0

# Convertir les valeurs classifiées en facteur
classified_values <- factor(classified_values, levels = 1:(num_classes + 1))

# Count the occurrences of each class
class_counts <- table(classified_values)

# Calculate the proportion of each class:
total_pixels <- sum(class_counts)
class_proportions <- round((class_counts / total_pixels),3)
# Define class names
class_names <- c(
  "1" = "No strava trails",
  "2" = "Low strava visitors intensity",
  "3" = "Medium strava visitors intensity",
  "4" = "High strava visitors intensity"
)

# Match the class names to the proportions
class_proportions_named <- as.data.frame(setNames(class_proportions, class_names[names(class_proportions)]))
colnames(class_proportions_named)<-c("Class","Proportion")
# Display the results
print(class_proportions_named)
# sum(class_proportions) # to check the sum = 1 = 100%

# Convert the class proportions to a data frame


# Create a pie chart using ggplot2
ggplot(class_proportions_named, aes(x="", y=Proportion, fill=Class)) +
  geom_bar(stat="identity", width=1, color="black") +
  geom_text(aes(label = Proportion), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(
    values = c("No strava trails"="transparent","Low strava visitors intensity"="#FFCC99","Medium strava visitors intensity"="#FF9966","High strava visitors intensity"="#FF3300"),guide = guide_legend(ncol = 2))+
  coord_polar(theta="y") +
  theme_void() +
  labs(title=paste(grouse_winter_akde_saved_hiver_malefemelle[[bird]]@info$identity,"\nProportion of Strava disturbance Classes")) 


# Histogram
g_histo_HR_or_study_area<-ggplot() +
  geom_histogram(aes(x=values_r_habitat),fill="red",alpha=0.4)+
  ggtitle("Strava intensity in the study area (proxy of human disturbance)")+
  xlab("Strava intensity")
  # theme_classic()
g_histo_HR_or_study_area




# CLASS PROPORTIONS COVERED BY TELEMETRY POINTS

# Determine which telemetry points fall within the masked area
# Create an sf object from the longitude and latitude columns
# Create a geometry column
grouse_winter_telemetry_hiver_malefemelle[[bird]][["hiver1"]]$geometry <- st_sfc(
  lapply(seq_len(nrow(grouse_winter_telemetry_hiver_malefemelle[[bird]][["hiver1"]])), function(i) {
    st_point(c(grouse_winter_telemetry_hiver_malefemelle[[bird]][["hiver1"]]$longitude[i], 
               grouse_winter_telemetry_hiver_malefemelle[[bird]][["hiver1"]]$latitude[i]))
  }),
  crs = st_crs(poly_95_bird)
)

# Convert the data frame to an sf object
telemetry_sf <- st_as_sf(grouse_winter_telemetry_hiver_malefemelle[[bird]][["hiver1"]]$geometry)

# Filter the points that fall within the polygon
telemetry_inside_mask <- telemetry_sf[st_within(telemetry_sf, poly_95_bird, sparse = FALSE), ]

# Assuming masked_raster is a SpatRaster object from the terra package
# Convert sf object to SpatVector for extraction
# telemetry_inside_mask_vect <- vect(st_geometry(telemetry_inside_mask))

# Extract the values of the habitat classes at those telemetry points
telemetry_values <- terra::extract(cropped_r, telemetry_inside_mask)

# Combine the extracted values with the original data
telemetry_inside_mask <- cbind(telemetry_inside_mask, telemetry_values)

# Convert the values to a data frame
telemetry_inside_mask <- as.data.frame(telemetry_inside_mask)

print(paste("NA values",sum(is.na(telemetry_inside_mask))))
# Remove NA values
# telemetry_df <- na.omit(telemetry_df)


# Count the occurrences of each class
telemetry_class_counts <-table(telemetry_inside_mask$lyr.1)

# Calculate the proportion of each class:
telemetry_total_pixels <- sum(telemetry_class_counts)
telemetry_class_proportions <- round((telemetry_class_counts / telemetry_total_pixels),3)

# Match the class names to the proportions
telemetry_class_proportions_named <- as.data.frame(setNames(telemetry_class_proportions, class_names[names(telemetry_class_proportions)]))
colnames(telemetry_class_proportions_named)<-c("Class","Proportion")
# Display the results
print(telemetry_class_proportions_named)
# sum(class_proportions) # to check the sum = 1 = 100%

# Convert the class proportions to a data frame


# Create a pie chart using ggplot2
ggplot(telemetry_class_proportions_named, aes(x="", y=Proportion, fill=Class)) +
  geom_bar(stat="identity", width=1, color="black") +
  geom_text(aes(label = Proportion), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(
    values = c("No strava trails"="transparent","Low strava visitors intensity"="#FFCC99","Medium strava visitors intensity"="#FF9966","High strava visitors intensity"="#FF3300"),guide = guide_legend(ncol = 2))+
  coord_polar(theta="y") +
  theme_void() +
  labs(title=paste(data_akde[[bird]]@info$identity,"\nProportion of Strava disturbance Classes")) 

























# Calculate the proportion of each habitat class based on the total number of telemetry points
total_telemetry_points <- nrow(telemetry_df)
telemetry_class_proportions <- round((telemetry_class_counts / total_telemetry_points), 3)

# Match the class names to the proportions
telemetry_class_proportions_named <- as.data.frame(setNames(class_proportions, class_names[names(class_proportions)]))
colnames(telemetry_class_proportions_named)<-c("Class","Proportion")
# Display the results
print(telemetry_class_proportions_named)
# sum(class_proportions) # to check the sum = 1 = 100%

# Create a pie chart using ggplot2
g_pie_telemetry<-ggplot(telemetry_class_proportions_named, aes(x="", y=Proportion, fill=Class)) +
  geom_bar(stat="identity", width=1, color="black") +
  scale_fill_manual(
    values = c("Unclassified soil"="#CC9966","Fine mineral soil"="#CCCCCC","Coarse mineral soil"="#666666","Cliff"="#333333","Dry or rocky grassland"="#99CC99","Herbaceous"="#FFFF66","Low ligneous"="#FFCCCC","Shrubs"="#FF99CC","Unclassified trees"="#99FF99","Deciduous trees"="#99FF00","Resinous trees"="#339966","Buildings"="#993300","Natural pond"="#99CCFF","Artificial pond"="#99FFFF","Waterway"="#0066FF"),na.value ="transparent",
    guide = guide_legend(ncol = 2))+
  coord_polar(theta="y") +
  theme_void() +
  labs(title=paste(data_akde[[bird]]@info$identity,"\nProportion of Habitat Classes\nin the bird's home range"))

