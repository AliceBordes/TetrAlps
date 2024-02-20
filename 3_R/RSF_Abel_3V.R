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
library(lubridate)
library(lidR)
library(tmap)
library(ggplot2)
library(cowplot)
library(gridExtra)

#loading functions
#--------------------------------------
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
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


# Vizualizing data
#--------------------------------------


#################################
# loading maps of the study site
#################################

## Shape de la zone d'etude restreinte
e <- extent(975000,981000,6480000,6487000)
sect <- sf::st_bbox(e) %>% sf::st_as_sfc()

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

#####################################################
## SPATIAL ESTIMATION : Where is the animal? 
#####################################################

#-------------------------
# visulising GPS locations
#-------------------------

#plot in red the bird named "Abel"
bird_abel_winter<-as.data.frame(data_bg_3V%>%filter(ani_nom=="Abel",saison=="hiver"))

#' The first day of Abel shows some unrealistic movements. We remove those observations
bird_abel_winter <- bird_abel_winter[(bird_abel_winter$date) > min(bird_abel_winter$date) + days(1), ]



#' Create a dt formatted like a telemetry object

bird_abel_winter_pretelemetry<-pre_telemetry(bird_abel_winter) # altitude --> z
bird_abel_winter_telemetry<-as.telemetry(bird_abel_winter_pretelemetry)




# plot the study ite with plot()
    windows()
    plot(raster_slope_3V_Abel_10,
         ext=e,
         legend.args = list(text = 'Slope'),
         xlab="x coordinate (Lambert 93)",
         ylab="y coordinate (Lambert 93)",
         main="Locations of the bird Abel at the 3 valleys site (winter)")
    
    points(bird_abel_winter_pretelemetry$X_GPS_lambert93,bird_abel_winter_pretelemetry$Y_GPS_lambert93,
           col="red",pch=20,cex=0.5,
           xlim=c(975000,981000),
           ylim=c(6480000,6487000))


# plot the study site with ggplot()

    #convert the raster to points for plotting
    map <- rasterToPoints(raster_slope_3V_Abel_10)
    #Make the points a dataframe for ggplot
    map_df <- data.frame(map)
    #Make appropriate column headings
    colnames(map_df) <- c("X_GPS", "Y_GPS", "slope")
    
    g_positions_abel<-ggplot(map_df,aes(x=X_GPS, y=Y_GPS))+
      geom_raster(aes(fill=slope))+
      geom_point(data = bird_abel_winter_pretelemetry,aes(x=X_GPS_lambert93,y=Y_GPS_lambert93,colour = factor(period_jour)))+
      theme_bw() +
      coord_equal() +
      xlim(975000,981000)+
      ylim(6480000,6487000)+
      scale_fill_gradientn("Slope (°)", limits=c(0,90),colours=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600")) +
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
      scale_color_discrete(name = "Period of the day")+
      ggtitle("Locations of the bird Abel at the 3 valleys site (winter)")+
      xlab("x coordinate (Lambert 93)")+
      ylab("y coordinate (Lambert 93)")
      #scale_fill_distiller(palette = "Spectral")
      #geom_raster(aes(x = "X_GPS", y = "Y_GPS", fill = "slope"))
    
    g_positions_abel

#or lines() to plot Abel movements
#--------------------------------------


# Describing the spatial correlations between observation data
#----------------------------------------------------------------

#' Fit ctmm model : Continuous-Time Movement Modeling

# Variogram

# To prospect if the sampling there is a regular or irregular sampling schedule 
# or to visualyze data when the sampling rate changes during data collection
dt.plot(bird_abel_winter_telemetry) #here the sampling is semi-irregular 

# variogram without considering the irregular sampling schedule 
vg_abel<-variogram(bird_abel_winter_telemetry)
# variogram considering the GPS were programmed to cycle between 1, 12, 24 hour sampling intervals
timelags <- c(1,12,24,36) %#% "hour" # the order has no importance
SVF_Abel <- variogram(bird_abel_winter_telemetry,dt=timelags)

windows()
par(mfrow=c(2,1))
plot(vg_abel,fraction=0.5,level=c(0.5,0.95), main="Variogram of Abel's bird suggesting an irregular sampling schedule")
legend("topleft", pch=c(15,15), legend=c("95% CI", "50% CI"), col=c("lightgrey", "darkgrey"), lty=1:2, cex=0.8)
  # level <- c(0.5,0.95) # 50% and 95% CIs
plot(SVF_Abel,fraction=0.5, dt=timelags, level=c(0.5,0.95), main="Variogram of Abel's bird taking into account the irregular sampling schedule \nprogramming for a location tag every 1h, 12h, 24h or 36h")
legend("topleft", pch=c(15,15), legend=c("95% CI", "50% CI"), col=c("lightgrey", "darkgrey"), lty=1:2, cex=0.8)


#Plot up to 50% of the maximum lag in the data
plot(vg_abel)
variogram.fit(vg_abel) # SVF of the fitted models 

#Zoom in on the shortest lags
plot(vg_abel, fraction=0.005)


#  HISTOGRAM of time between 2 locations 
windows()
g1<-ggplot(bird_abel_winter_pretelemetry[(1:nrow(bird_abel_winter_pretelemetry)-1),],aes(x=as.POSIXct(dt,format="%d %H:%M:%S")))+
  geom_histogram(fill="#FF6666", bins = 10)+
  scale_x_datetime(date_labels = "%d d %H h")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Time between 2 locations")+
  ggtitle("")

g2<-ggplot(bird_abel_winter_pretelemetry[(1:nrow(bird_abel_winter_pretelemetry)-1),],aes(x=as.POSIXct(dt,format="%d %H:%M:%S")))+
  geom_histogram(fill="#FF6666", bins = 50)+
  scale_x_datetime(date_labels = "%d d %H h %M min",breaks ="3 hours",expand = c(0, 0))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Time between 2 locations")+
  ggtitle("")

grid.arrange(g1,g2,nrow=2,top = "Time between two consecutive positions of the Abel bird in winter")

#' ctmm.guess --> large approximation of the maximum likelyhood, inaccurate estimation of the parameters 

bird_abel_winter_guess <- ctmm.guess(bird_abel_winter_telemetry, CTMM=ctmm(isotropic = TRUE), interactive = FALSE) #isotropic = TRUE => s'éloigne du centre de manière identique dans toutes les directions
#plots a variogram object overlayed with a continuous-time movement model guesstimated from the variogram's shape
# isotropic = TRUE beacuse we consider the home range (espace vital) 
# as a sphere (attractor center), 
# even if an ellipse is more realistic (anisotropy) 
# but the function is not optized with (isotropic=F)

#model selected (approximation)
summary(bird_abel_winter_guess)


# selection of the 5 best model structures
fitted_models_bird_abel_winter <- ctmm.select(bird_abel_winter_telemetry,CTMM=bird_abel_winter_guess, verbose=TRUE) #verbose=TRUE return additionnal info
#CTMM = GUESS marche pas toujours, CTMM = A ctmm movement-model object containing the initial parameter guesses
summary(fitted_models_bird_abel_winter)
# "OUF anisotropic" is the "best" model, IID is the conventional model 

# best model selected, with the best estimation of the parameters
best_model<-fitted_models_bird_abel_winter[[1]] ;     second_best_model<-fitted_models_bird_abel_winter[[2]]
summary(best_model)


# Visualizing the SVF of the guess model and comparison of the 2 best fitted models on variogram
plot(vg_abel, CTMM=bird_abel_winter_guess, col.CTMM="#FF6666")
par(mfrow=c(2,2))
plot(vg_abel, CTMM=best_model, col.CTMM="#1b9e77") #variogram.fit(vg.abel) is more approximative
plot(vg_abel, CTMM=best_model, col.CTMM="#1b9e77", fraction=0.005)

plot(vg_abel, CTMM=second_best_model, col.CTMM="#6666FF") #variogram.fit(vg.abel) is more approximative
plot(vg_abel, CTMM=second_best_model, col.CTMM="#6666FF", fraction=0.005)

#visualizing the home range density estimates against the position data                                                                                                                                         

#' Fit akde (calculate individual and population-level autocorrelated kernel density home-range
#' from telemetry data and a corresponding continuous-time movement model)
#' Conventionaly, Kernel density estimation (KDE) assumes that the data are independent and identically distributed (IID), 
#' here with akde,other theorical model than IID can be set 
windows()
bird_abel_winter_akde <- akde(bird_abel_winter_telemetry, CTMM=best_model)
bird_abel_winter_akde2 <- akde(bird_abel_winter_telemetry, CTMM=second_best_model)
par(mfrow=c(2,2))
plot(bird_abel_winter_telemetry,UD=bird_abel_winter_akde) #we could have estimaded the conventional KDE estimated IID model 
plot(bird_abel_winter_telemetry,UD=bird_abel_winter_akde2)

#--------------------------------------

#####################################################
## INFERENCE : Why the animal occurs there? 
#####################################################

#' # Minimal example of rsf.fit 
#--------------------------------------
plot(raster_high_vege_classif_9m) # pb : NA values in sampled variables high.vegetation not tolerated by rsf.fit(R=)

#' Create named list of rasters
plot(raster_slope_3V_Abel_10)
# crs(raster_slope_3V_Abel_10) #to verify the coordinate system

be <- list("slope" = raster_slope_3V_Abel_10)

#' The integrator = "Riemann" option is still experimental, we use it here because it is much faster
bird_abel_winter_rsf_riemann <- rsf.fit(bird_abel_winter_telemetry, bird_abel_winter_akde,  be, integrator = "Riemann")
      # R = must be a list of RasterLayers to fit Poisson regression coefficients to (under a log link)

summary(bird_abel_winter_rsf_riemann)


#' A suitability map
suitability_riemann <- suitability(bird_abel_winter_rsf_riemann, be, crop(be[[1]], e*2,raster_slope_3V_Abel_10))
raster::plot(suitability_riemann)


#' Range distribution (includes the ranging behaviour) (étendue de la distribution)
#my_grid <- crop(be[[1]], e)
agde_bird_abel_winter <- agde(CTMM = bird_abel_winter_rsf_riemann,be) #prend pas en compte les pts here (model d'autocorr seulement)
plot(agde_bird_abel_winter)


#####################################################
## PREDICTION : Where else the animal might occur ? 
#####################################################

#' # Predict function
# Predict function

prediction<-predict(bird_abel_winter_rsf_riemann)








rsf.predict <- function(model, object, include_avail = TRUE) {
  # Initialize log_avail to 0
  log_avail <- 0
  
  # If include_avail is set to TRUE, it calculates availability (log_avail) based on spatial coordinates
  if (include_avail) {
    # Extract spatial coordinates and transform to appropriate format
    xy <- as.data.frame(xyFromCell(object, 1:ncell(object)))
    xy <- sf::st_as_sf(xy, coords = c("x", "y"), crs = crs(object))
    xy <- st_transform(xy, CRS(model@info$projection))
    xy <- st_coordinates(xy)
    xy <- cbind(xy, -(xy[, 1]^2 + xy[, 2]^2) / 2)
    
    # Calculate log_avail using specified model
    beta_rr <- 1 / model$sigma[1, 1]
    beta_xyr <- c(model$mu * beta_rr, beta_rr)
    log_avail <- xy %*% beta_xyr
  }
  
  # Prepare new data for prediction
  newdata <- as.data.frame(getValues(object))
  attr(newdata, "na.action") <- "na.pass"
  
  # Construct model matrix (X)
  X <- stats::model.matrix.default(model$formula, data = newdata, na.action = na.pass)
  
  # Compute lambda (prediction) by multiplying model matrix with beta and adding log_avail
  lambda <- X[, -1] %*% model$beta + log_avail
  
  # Create a raster object and populate it with exponential values of the prediction
  r <- raster(object, 1)
  r[] <- exp(lambda - max(lambda, na.rm = TRUE))
  
  # Normalize raster values to obtain probabilities
  r <- r / sum(getValues(r), na.rm = TRUE)
  
  # Return the predicted raster
  r
}


abel_winter_predict <- rsf.predict(bird_abel_winter_rsf_riemann, raster_slope_3V_Abel_10, include_avail = FALSE)
raster::plot(abel_winter_predict)
