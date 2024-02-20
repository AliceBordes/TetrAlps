######################################################################

#### PhD Tetras_test project ####

# Alice Bordes #

# November 2023 #

##############
# Description:

# Visualization GPS-tagged Black grouse, Data from OFB
# SSF 
#'  SSFs compare environmental attributes of observed steps (the linear segment between two consecutive observations of position) 
#'  with alternative random steps taken from the same starting point. SSFs have been used to study habitat selection, human-wildlife interactions, movement corridors, and dispersal behaviours in animals.



#' Estimating mixed effects models for step selection functions is not trivial.
#' The computations can take a long time, and the model may not converge.
#'
#' Also, we do not always need mixed effects models when analysing movement data for multiple animals.
#' Often we have enough data for each animal to estimate the movement parameters.
#' In a mixed effects model, we make the
#' assumption that the variation in parameter values across individuals follows a normal distribution.
#' This may not be appropriate.
#' The advantage of the mixed effects model is that we get estimates of the average population-level effect and
#' a measure of the variation between individuals.
#'
#' If the data are sufficient to fit a model to each individual, I would recommend fitting individual models as well and looking
#' at the distribution of the parameter estimates, to see if they are reasonably close to a normal distribution.

######################################################################

###'Loading packages

library(animove)
library(dplyr)
library(amt)
library(move)
library(glmmTMB)
library(sf)
library(tidyverse)
library(ggplot2)
library(ctmm)

#loading functions
#--------------------------------------
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
#--------------------------------------

# loading environment
#--------------------------------------

## Shape de la zone d'etude restreinte
#e <- extent(975000,981000,6480000,6487000)
#sect <- sf::st_bbox(e) %>% sf::st_as_sfc()

# visualyze the raster of the slope for the study area

#raster.slope.3V <- terra::rast(raster.slope.3V) # RasterLayer --> SpatRaster
raster_slope_3V<-raster(raster3V_slope_brute)  # nothing --> RasterLayer object
raster_slope_3V_Abel <- raster::crop(raster_slope_3V,e) # Cut out a geographic subset and save the raster with the new extents --> reduce the limits of the raster

plot(raster_slope_3V_Abel) # zoom on the interest study area (without saving the raster new extents)

# here the resolution of the raster slope = 1m 
# to save time for the next analyses --> create raster slope with resolution at 10m
raster_slope_3V_Abel_10<-aggregate(raster_slope_3V_Abel,10)

par(mfrow=c(1,2))
plot(raster_slope_3V_Abel)
plot(raster_slope_3V_Abel_10)

#raster::readAll(raster_slope_3V_Abel) # to save a raster in the RAM (intern memory of the computer), to save time

#raster high vegetation
raster_high_vege_classif_9m <- raster::crop(r_analyse_9$focal_vege_haute_70,e)
plot(raster_high_vege_classif_9m)

# carte d'occupation des sols OSO (produite par le Centre d'Expertise Scientifique sur l'occupation des sols (CES OSO))
oso <- terra::rast("M:/CESBIO/OSO_20220101_RASTER_V1-0/DATA/OCS_2022.tif") 
oso <- raster::crop(oso,e)
plot(oso)
#--------------------------------------

#loading data
#--------------------------------------
rawdata_path<-"C:/Users/albordes/Documents/PhD/TetrAlps/1_RAW_DATA/"


# GPS locations of black grouses
data_bg_3V<-readRDS(paste0(rawdata_path,"tot.ind.trois_vallees.rds"))

# study sector
sect_3V <- sf::st_read(paste0(rawdata_path,"secteur_etude_3V.gpkg"))

# slope 3V
raster3V_slope_brute<-paste0(rawdata_path,"raster.3V.slope.tif")

# Analyse à 9m
r_analyse_9 <- terra::rast(paste0(rawdata_path, "Test11_carto_classification2_supra_10m_tot.tif"))
#--------------------------------------

#######################################
####          data analysis        ####
#######################################

# Formatting data
#--------------------------------------
# work on winter data
bird_winter<-as.data.frame(data_bg_3V%>%filter(saison=="hiver"))

#' Create a dt formatted like a telemetry object

bird_winter_pretelemetry<-pre_telemetry(bird_winter) # altitude --> z


#' Convert MoveStack to amt::track object.
#'
#' Currently, there is no conversion function from move::moveStack to amt::track implemented, so we do it by hand
bird_winter_tracks <- amt::mk_track(as.data.frame(bird_winter_pretelemetry),
                                X_GPS_lambert93, Y_GPS_lambert93, study.local.timestamp,
                                id = individual.local.identifier,
                                crs = st_crs(bird_winter_pretelemetry))
#--------------------------------------



step_duration <- 3

# data preparation
#--------------------------------------

# Before fitting a SSF we have to do some data preparation. First, we change from a point representation to a step representation,
    bird_winter <- bird_winter_tracks |> nest(track = -"id") #nest() creates data frames in the main data frame! 
        # : each bird (id) is associated with a data frame containing several x and y positions at time t

    bird_winter <- bird_winter |> mutate(steps = map(track, function(x) {
      x |> dplyr::filter(t_ > min(t_) + days(1)) |>                         # discard the steps of the 1st day of measures
        track_resample(hours(step_duration), tolerance = minutes(15)) |>    # keep step of 3h + or - 15 min
        filter_min_n_burst(3) |>                                            # minimum number of segments/steps for each animal
        steps_by_burst()})) |> dplyr::select(id, steps)
        # : each bird (id) is now associated with a data frame containing the segments (steps) represented by x1, x2 and y1 and y2 positions at time t1 and t2

# - Here, we reduced the data set to observations that are within a certain time step range. The SSF assumes Brownian motion, so we should thin sufficiently, so that the velocities of successive steps are uncorrelated. Here we go for 3 hours.
# - There is some tolerance around the target time interval of 3 hours. When two observations are separated by less than the threshold (tolerance = 2h45), the second observation is removed
# - When two observations are separated by more than the upper threshold (> 3h15), the observations are assigned to different bursts.
    # ex: if location at 2:00 am , 4:10 am , 5:08 am , 9:00 am --> 4:10 am is discarded and 2:00 am and 5:08 am --> burst 1 ; 9:am --> burst 2

# here, we keep only 5 individuals with only 1 variable data frame , otherwise ssf_birds didn't work
    bird_winter<-bird_winter %>% filter(id %in% c("Alpha","Abel","Caramel","Calu","Diot"))
    
    
# bind 
birds_all_steps<- bird_winter |> unnest(cols = steps)
# calculation of parameters
combined_sl_distr_birds <- fit_distr(steps_all$sl_, "gamma")
combined_ta_distr_birds <- fit_distr(steps_all$ta_, "vonmises")  # von Mises law (distribution) for an angle x
#--------------------------------------


# creation of random steps?
#--------------------------------------
set.seed(23) # guarantee that the same random values are produced each time you run the code. # 23 is like a tag identifier 

ssf_birds <- bird_winter |> mutate(steps = map(steps, random_steps, n_control = 100,            # n_control =  steps that share the same starting location but have different turn angles and step lengths
                                               sl_distr = combined_sl_distr_birds,
                                               ta_distr = combined_ta_distr_birds)) |> unnest(cols = steps)

# We need to modify the values for burst_ and step_id_ to ensure that not two individuals are assigned the same values for step_id_
ssf_birds <- ssf_birds |> mutate(burst_ = paste(id, burst_),
                                       step_id_ = paste(id, step_id_))
table(table(ssf_birds$step_id_)) # Now OK. step_id_ is used in the statistical modelling, so we need to make sure that the right observations are combined.



#--------------------------------------




