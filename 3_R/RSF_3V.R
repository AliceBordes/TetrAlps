######################################################################

#### PhD Tetras_test project ####

# Alice Bordes #

# November 2023 #

##############
# Description:

# Visualization GPS-tagged Black grouse, Data from OFB
# RSF what happens next
#' Model the relative density of animals (also called range distribution or utilisation distribution) as a function of environmental predictors.

######################################################################

###'Loading packages

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
library(animove)
library(lubridate)
library(lidR)
library(tmap)
library(ggplot2)
library(sjmisc)

#charging functions
#--------------------------------------
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
#--------------------------------------

#charging data
#--------------------------------------
rawdata.path<-"C:/Users/albordes/Documents/PhD/TetrAlps/1_RAW_DATA/"


# GPS locations of black grouses
data.bg.3V<-readRDS(paste0(rawdata.path,"tot.ind.trois_vallees.rds"))

# study sector
sect_3V <- sf::st_read(paste0(rawdata.path,"secteur_etude_3V.gpkg"))

# slope 3V
raster3V.slope.brute<-paste0(rawdata.path,"raster.3V.slope.tif")
#--------------------------------------

#################################
# visulising GPS locations of multiple birds
#################################

# create the raster of the slope for the study area
raster.slope.3V<-raster(raster3V.slope.brute)  # nothing --> RasterLayer object
raster.slope.3V <- terra::rast(raster.slope.3V) # RasterLayer --> SpatRaster

#plot in red the bird locations in winter
grouse_winter.raw<-as.data.frame(data.bg.3V%>%filter(saison=="hiver"))

#create a list of data.frames for each animal
grouse_winter<-multiple_dt_indiv(grouse_winter.raw,"nom")

#' The first day of location data often shows some unrealistic movements. We remove those observations
for(i in 1:length(grouse_winter))
{
  grouse_winter[[i]]<- grouse_winter[[i]][(grouse_winter[[i]]["date"]) > (first(grouse_winter[[i]]["date"]) + ddays(1)),]
}

#pr 1 animal:
#grouse_winter <- grouse_winter[(grouse_winter$date) > min(grouse_winter$date) + days(1), ]



#' Create a dt formatted like a telemetry object
grouse_winter_pretelemetry<-grouse_winter
for(i in 1:length(grouse_winter_pretelemetry))
{
  grouse_winter_pretelemetry[[i]]<- pre_telemetry(grouse_winter_pretelemetry[[i]])
}

grouse_winter_telemetry<-grouse_winter_pretelemetry
for(i in 1:length(grouse_winter_telemetry))
{
  grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_telemetry[[i]])
}

grouse_winter_pretelemetry_all<-pre_telemetry(data.bg.3V)
grouse_winter_telemetry_all<-as.telemetry(grouse_winter_pretelemetry_all[,-1])

#View(grouse_winter_telemetry[[1]])

e= c(min(grouse_winter_pretelemetry[[1]]["X_GPS_lambert93"])-10000,max(grouse_winter_pretelemetry[[1]]["X_GPS_lambert93"])+5000,min(grouse_winter_pretelemetry[[1]]["Y_GPS_lambert93"])-10000,max(grouse_winter_pretelemetry[[1]]["Y_GPS_lambert93"])+10000)



windows()
plot(raster.slope.3V,
     ext=e,
     legend.args = list(text = 'Slope'),
     xlab="x coordinate (Lambert 93)",
     ylab="y coordinate (Lambert 93)",
     main="Locations of tagged-black grouses at the 3 valleys site (winter)")

points(grouse_winter_pretelemetry$X_GPS_lambert93,grouse_winter_pretelemetry$Y_GPS_lambert93,
       col="red",pch=20,cex=0.5)
#--------------------------------------


# Describing the spatial correlations between observation data
#----------------------------------------------------------------

#' Fit ctmm model : Continuous-Time Movement Modeling

# Pooling Variograms : If multiple individuals exhibit similar movement behaviors

# To prospect if the sampling there is a regular or irregular sampling schedule 
# or to visualyze data when the sampling rate changes during data collection
dt.plot(grouse_winter_telemetry) #here the sampling is semi-irregular 

# visualizing the irregular sampling schedule 

dt_box<-data.frame("birds"=1:length(grouse_winter_telemetry),"interval_hr"=summary(grouse_winter_telemetry)$interval)

ggplot(data=dt_box,aes(x=birds,y=interval_hr))+
  geom_boxplot()+
  geom_dotplot(binaxis="y", stackdir='center', dotsize=1)+
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



par(mfrow=c(1,2))
# population variogram assuming homogenous sampling schedule (which is not the case)
SVF <- lapply(grouse_winter_telemetry,variogram)
SVF<- mean(SVF)
plot(SVF,fraction=0.005,level=c(0.5,0.95))
title("Population variogram")


#  HISTOGRAM of time between 2 locations 
par(mfrow=c(1,1))
windows()

ggplot(grouse_winter_pretelemetry[(1:nrow(grouse_winter_pretelemetry)-1),],aes(x=as.POSIXct(dt,format="%d %H:%M:%S")))+
  geom_histogram(fill="#FF6666", bins = 50)+
  scale_x_datetime(date_labels = "%d d %H h")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Time between 2 locations")

ggplot(grouse_winter_pretelemetry[(1:nrow(grouse_winter_pretelemetry)-1),],aes(x=as.POSIXct(dt,format="%d %H:%M:%S")))+
  geom_histogram(fill="#FF6666", bins = 50)+
  scale_x_datetime(date_labels = "%d d %H h %M min",breaks ="3 hours",expand = c(0, 0))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Time between 2 locations")



#' ctmm.guess --> large approximation of the maximum likelyhood, inaccurate estimation of the parameters 

grouse_winter_guess <- ctmm.guess(grouse_winter_telemetry, CTMM=ctmm(isotropic = TRUE), interactive = FALSE) #isotropic = TRUE => s'éloigne du centre de manière identique dans toutes les directions
#plots a variogram object overlayed with a continuous-time movement model guesstimated from the variogram's shape
# isotropic = TRUE beacuse we consider the home range (espace vital) 
# as a sphere (attractor center), 
# even if an ellipse is more realistic (anisotropy) 
# but the function is not optized with (isotropic=F)

#model selected (approximation)
summary(grouse_winter_guess)


# selection of the 5 best model structures
fitted_models_grouse_winter <- ctmm.select(grouse_winter_telemetry,CTMM=grouse_winter_guess, verbose=TRUE) #verbose=TRUE return additionnal info
#CTMM = GUESS marche pas toujours, CTMM = A ctmm movement-model object containing the initial parameter guesses
summary(fitted_models_grouse_winter)
# "OUF anisotropic" is the "best" model, IID is the conventional model 

# best model selected, with the best estimation of the parameters
best.model<-fitted_models_grouse_winter[[1]] ;     second.best.model<-fitted_models_grouse_winter[[2]]
summary(best.model)


# Visualizing the SVF of the guess model and comparison of the 2 best fitted models on variogram
plot(vg.grouse, CTMM=grouse_winter_guess, col.CTMM="#FF6666")
par(mfrow=c(2,2))
plot(vg.grouse, CTMM=best.model, col.CTMM="#1b9e77") #variogram.fit(vg.grouse) is more approximative
plot(vg.grouse, CTMM=best.model, col.CTMM="#1b9e77", fraction=0.005)

plot(vg.grouse, CTMM=second.best.model, col.CTMM="#6666FF") #variogram.fit(vg.grouse) is more approximative
plot(vg.grouse, CTMM=second.best.model, col.CTMM="#6666FF", fraction=0.005)

#visualizing the home range density estimates against the position data                                                                                                                                         

#' Fit akde (take into account the autocorrelation of the positions in the dataset)
windows()
grouse_winter_akde <- akde(grouse_winter_telemetry, CTMM=best.model)
grouse_winter_akde2 <- akde(grouse_winter_telemetry, CTMM=second.best.model)
par(mfrow=c(2,2))
plot(grouse_winter_telemetry,UD=grouse_winter_akde) #we could have estimaded the conventional KDE estimated IID model 
plot(grouse_winter_telemetry,UD=grouse_winter_akde2)

#--------------------------------------



#' # Minimal example of rsf.fit
#--------------------------------------

#' Create named list of rasters
be <- list("slope1" = raster.slope.3V,
           "slope2" = raster.slope.3V)

#' The integrator = "Riemann" option is still experimental, we use it here because it is much faster
grouse_winter_rsf_riemann <- rsf.fit(grouse_winter_telemetry, grouse_winter_akde, R = be, integrator = "Riemann")
# R = must be a list of rasters to fit Poisson regression coefficients to (under a log link)

summary(grouse_winter_rsf_riemann)


#' A suitability map
suitability_riemann <- suitability(grouse_winter_rsf_riemann, be, crop(be[[1]], extent=e,raster.slope.3V))
raster::plot(suitability_riemann)


#' Range distribution (includes the ranging behaviour) (étendue de la distribution)
agde_grouse_winter <- agde(grouse_winter)
plot(agde_grouse_winter)

#builing 



list.raster.slope.3V<-list("slope"=raster.slope.3V)
grouse_winter_slope_RSF <- rsf.fit(grouse_winter_telemetry, grouse_winter_akde, R = list.raster.slope.3V, integrator = "Riemann")


