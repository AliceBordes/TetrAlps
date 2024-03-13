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
library(here)

### Loading functions
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
#********************************************************************


### Loading data
#********************************************************************
base<-here()

# GPS locations of black grouses
data_bg_3V<-readRDS(paste0(base,"/1_RAW_DATA/tot.ind.trois_vallees2.rds"))

# 3V borders 
borders_3V_vect <- terra::vect(paste0(base,"/1_RAW_DATA/borders_3V.gpkg"))

# slope 3V
slope_3V<-terra::rast(paste0(base,"/2_DATA/slope_3V_ign.tif"))

# Analyse à 9m
#carto_habitats_3V_brute <- paste0(base,"/1_RAW_DATA/landcover_T2L_1m_trois_vallees.tif") #carto Clara
carto_habitats_3V <- terra::rast(paste0(base,"/1_RAW_DATA/landcover_T2L_1m_trois_vallees.tif")) #carto Clara
carto_habitats_3V<-as.factor(carto_habitats_3V) #indicate discrete values for my map categories

# strava
strava <- terra::rast(paste0(base, "/2_DATA/strava/strava_Trois_Vallees_winter_single.tif"))

# mnt
mnt<-terra::rast(paste0(base, "/2_DATA/mnt_ign.tif"))
# mnt_9<-terra::rast(paste0(base, "/2_DATA/mnt_9_mean_ign.tif"))

#********************************************************************




### Settings
#********************************************************************
## Shape the study area
#e <- extent(971000,985000,6471000,6486000)
e1<-ext(borders_3V_vect)
e2<-ext(as.polygons(ext(data_bg_3V), crs=crs(data_bg_3V)))

e<-c(min(e1[1],e2[1])-1000,max(e1[2],e2[2])+1000,min(e1[3],e2[3])-1000,max(e1[4],e2[4])+1000) 
# e_poly<-ext(as.polygons(ext(e), crs=crs(data_bg_3V)))
# ext(e_poly)


# change the coordinate system of the SpatVector from (9..,9..,6..,6..) to (6..,6..,45..,45..)
# borders_3V_vect_lat_long<-project(borders_3V_vect, y="+proj=longlat +datum=WGS84")
# e<-ext(borders_3V_vect_lat_long)
#********************************************************************



#######################################
####          data analysis        ####
#######################################

# Formatting data
#********************************************************************
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
                                crs = st_crs(data_bg_3V))  # coordinate system of data_bg_3V = Lambert-93 = (french) metric system
#********************************************************************



#********************************************************************
#' description of the spatial (geographic area containing all the points (x and y coordinates min and max)) and temporal window 
summary(bird_winter_tracks)

hist(step_lengths(bird_winter_tracks),xlab="Step lengths (m)",main="Histogram of step lengths in the Trois Vallées Black grouse population")
#********************************************************************


# Settings
#********************************************************************
step_duration <- 3
#********************************************************************

# data preparation
#********************************************************************

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
#********************************************************************


# creation of random steps?
#********************************************************************
set.seed(23) # guarantee that the same random values are produced each time you run the code. # 23 is like a tag identifier 

ssf_birds <- bird_winter |> mutate(steps = map(steps, random_steps, n_control = 100,            # n_control =  steps that share the same starting location but have different turn angles and step lengths
                                               sl_distr = combined_sl_distr_birds,
                                               ta_distr = combined_ta_distr_birds)) |> unnest(cols = steps)

# We need to modify the values for burst_ and step_id_ to ensure that not two individuals are assigned the same values for step_id_
ssf_birds <- ssf_birds |> mutate(burst_ = paste(id, burst_),
                                       step_id_ = paste(id, step_id_))
table(table(ssf_birds$step_id_)) # Now OK. step_id_ is used in the statistical modelling, so we need to make sure that the right observations are combined.



#********************************************************************




