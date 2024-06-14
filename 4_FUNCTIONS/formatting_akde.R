#### PhD Tetras_test project ####

# Alice Bordes #

# June 2023 #

# Description:

# Creation akde object


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
#********************************************************************

### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD/TetrAlps"
#********************************************************************


### Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
#********************************************************************


### Loading data ----
#********************************************************************

### DATASETS

# GPS locations of black grouses
data_bg_3V<-readRDS(file.path(base,"1_RAW_DATA/tot.ind.trois_vallees2.rds"))

# Main characteristics of tegged-black grouse
# synth_bg_all_sites<-read.csv2(file.path(base,"1_RAW_DATA/bilan_captures_tetras_all_sites_feb2024.csv"),sep=",")[,-1]
synth_bg_all_sites<-read.csv2(file.path(base,"1_RAW_DATA/bilan_captures_tetras_all_sites_mar2024_x_y_coord.csv"),sep=",")[,-1]

# Union of data_bg_3V and synth_bg_all_sites dataset ATTENTION : join by the couple tag_id, ani_name otherwise, wrong match!
data_bg_3V_synth_fusion<- dplyr::left_join(data_bg_3V ,synth_bg_all_sites %>% filter(zone_etude=="trois_vallees") %>% select(ani_nom,tag_id,marque_tag,energy,sexe,age), by=c("tag_id"="tag_id","ani_nom"="ani_nom"))

# formatting synth_bg_all_sites to retrieve capture positions coordinates
synth_bg_all_sites$x_capture_coord<-as.numeric(synth_bg_all_sites$x_capture_coord)
synth_bg_all_sites$y_capture_coord<-as.numeric(synth_bg_all_sites$y_capture_coord)

synth_bg_all_sites$x_capture_coord2<-ifelse(is.na(synth_bg_all_sites$x_capture_coord), 1,synth_bg_all_sites$x_capture_coord)
synth_bg_all_sites$y_capture_coord2<-ifelse(is.na(synth_bg_all_sites$y_capture_coord), 1,synth_bg_all_sites$y_capture_coord)

synth_bg_all_sites <- st_as_sf(as.data.frame(synth_bg_all_sites), coords = c("x_capture_coord2", "y_capture_coord2"), crs = 4326)

# supress absurd GPS values
for(i in 1: nrow(synth_bg_all_sites))
{
  if(st_bbox(synth_bg_all_sites$geometry[i])$xmin >100)
  {
    synth_bg_all_sites$geometry[i]<-NA
  }
}      

# select all the points equal to POINT (1 1)
# st_bbox(points$geometry[1])==1
synth_bg_3V<-synth_bg_all_sites %>% filter(zone_etude=="trois_vallees")
synth_bg_3V<-st_as_sf(synth_bg_3V)
synth_bg_3V$geometry_lambert<-st_transform(synth_bg_3V$geometry,crs="+init=epsg:2154")
synth_bg_3V<-synth_bg_3V %>% mutate(x_capture_lambert = st_coordinates(geometry_lambert)[,1],y_capture_lambert = st_coordinates(geometry_lambert)[,2])


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



#### 1_Creation of telemetry with birds locations and akde objects for home range estimation ####
#### 1.1_Creation of the telemetry object with birds locations ----
#********************************************************************



formatting_akde<-function(data,file_path="C:/Users/albordes/Downloads/",projection="Lambert93")
{
  
  grouse_winter_telemetry<-data
  

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
  
  
  save(best_model, file=paste0(file_path,"best_model.RData"))
  
  
  # Home range calculation                                                                                                                                       
  
  grouse_winter_akde <- lapply(grouse_winter_telemetry, function(guess_model) {
    lapply(seq_along(guess_model), function(j) {
      akde(guess_model[[j]], CTMM = best_model[[j]])
    })
  })
  

  View(grouse_winter_akde)
  
  save(grouse_winter_akde, file=paste0(file_path,"akde.RData"))
  
  return()
  
}



# Define the formatting_akde function
formatting_akde <- function(data, file_path = "C:/Users/albordes/Downloads/", projection = "Lambert93") {
  grouse_winter_telemetry <- data
  
  # Model fitting and selection first requires a prototype model with guesstimated parameters
  grouse_winter_guess <- lapply(grouse_winter_telemetry, function(df_list) {
    lapply(df_list, function(sub_df) {
      ctmm.guess(sub_df, CTMM = ctmm(isotropic = TRUE), interactive = FALSE)
    })
  })
  
  # Apply summary to the guessed models
  grouse_winter_guess_summary <- lapply(grouse_winter_guess, function(guess_list_sumup) {
    lapply(guess_list_sumup, summary)
  })
  
  # Fit models using ctmm.select
  fitted_models_grouse_winter <- lapply(seq_along(grouse_winter_telemetry), function(i) {
    lapply(seq_along(grouse_winter_telemetry[[i]]), function(j) {
      ctmm.select(grouse_winter_telemetry[[i]][[j]], CTMM = grouse_winter_guess[[i]][[j]], verbose = TRUE)
    })
  })
  
  # Apply summary to the fitted models
  fitted_models_grouse_winter_summary <- lapply(fitted_models_grouse_winter, function(fit_sumup) {
    lapply(fit_sumup, summary, unit = FALSE)
  })
  
  # Retrieve the best model for each seasonal dataset
  best_model <- lapply(fitted_models_grouse_winter, function(fitted_models) {
    lapply(fitted_models, function(sub_fitted_model) {
      sub_fitted_model[[1]]
    })
  })
  
  save(best_model, file = paste0(file_path, "best_model.RData"))
  
  # Home range calculation
  grouse_winter_akde <- lapply(seq_along(grouse_winter_telemetry), function(i) {
    lapply(seq_along(grouse_winter_telemetry[[i]]), function(j) {
      akde(grouse_winter_telemetry[[i]][[j]], CTMM = best_model[[i]][[j]])
    })
  })
  
  save(grouse_winter_akde, file = paste0(file_path, "akde.RData"))
  
  return()
}

# Example usage
# Assuming 'data' is your list of lists of data frames
# result <- formatting_akde(data)


