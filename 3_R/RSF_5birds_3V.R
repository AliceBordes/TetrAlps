

#### PhD Tetras_test project ####

# Alice Bordes #

# November 2023 #

#*******************************************************************************************
# Description:

# Visualization GPS-tagged Black grouse, Data from OFB
# RSF what happens next
#' Model the relative density of animals (also called range distribution or utilisation distribution) as a function of environmental predictors.



# Loading packages
#*******************************************************************************************
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
#*******************************************************************************************



# Loading functions
#*******************************************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
#*******************************************************************************************



# Loading data
#*******************************************************************************************
rawdata_path<-"C:/Users/albordes/Documents/PhD/TetrAlps/1_RAW_DATA/"


# GPS locations of black grouses
data_bg_3V<-readRDS(paste0(rawdata_path,"tot.ind.trois_vallees.rds"))

# study sector
sect_3V <- sf::st_read(paste0(rawdata_path,"secteur_etude_3V.gpkg"))

# slope 3V
preraster3V_slope_brute<-paste0(rawdata_path,"raster.3V.slope.tif")

# Analyse à 9m
r_analyse_9 <- paste0(rawdata_path, "Test11_carto_classification2_supra_10m_tot.tif")
#*******************************************************************************************



# little analyses of data
#*******************************************************************************************
#number of tagged-birds
length(unique(data_bg_3V$ani_nom))
#average number of observations per bird
(data_bg_3V %>% group_by(ani_nom) %>% summarise(n = n()) %>% summarise(mean = mean(n)))$mean

#tag-positions during day
ggplot(data_bg_3V,aes(x=as.POSIXct(heure)))+
  geom_histogram(color = "white",binwidth =3600,position = "identity")+
  scale_x_datetime(breaks = seq(as.POSIXct("1970-01-01 00:00:01"), as.POSIXct("1970-01-01 23:59:59"), by = "1 hour"), date_labels="%H", expand = c(0,0),limits = c(as.POSIXct("1970-01-01 00:00:01"), as.POSIXct("1970-01-01 23:59:59")))+
  #scale_x_continuous(name = "Hour of Day", breaks = 0:23) +
  xlab("Hour of the day")

#ggplot(data_bg_3V,aes(x=(heure),colour = "white"))+
#  geom_bar()+
#  stat_bin(bins=23)+
#  xlab("Hour of the day")

#*******************************************************************************************

#----------------------------------------------#
# visulising GPS locations of multiple birds 
#----------------------------------------------#


# loading maps of the study site
#*******************************************************************************************
## Shape de la zone d'etude restreinte
e <- extent(971000,985000,6471000,6490000)

# visualyze the raster of the slope for the study area

#raster_slope_3V <- terra::rast(raster_slope_3V) # RasterLayer --> SpatRaster
raster_slope_3V_brute<-raster(preraster3V_slope_brute)  # nothing --> RasterLayer object
raster_slope_3V <- raster::crop(raster_slope_3V_brute,e) # Cut out a geographic subset and save the raster with the new extents --> reduce the limits of the raster
#crs(raster_slope_3V) #display the coordinate system

plot(raster_slope_3V) # zoom on the interest study area (without saving the raster new extents)

# here the resolution of the raster slope = 1m 
# to save time for the next analyses --> create raster slope with resolution at 10m
raster_slope_3V_10<-aggregate(raster_slope_3V,10)
par(mfrow=c(1,2))

plot(raster_slope_3V)
plot(raster_slope_3V_10)

#raster::readAll(raster_slope_3V) # to save a raster in the RAM (intern memory of the computer), to save time

#raster high vegetation
raster_vege_height_classif_9m <- raster(r_analyse_9)
raster_vege_height_classif_9m <- crop(raster_vege_height_classif_9m,e)
raster_vege_height_classif_9m_10<-aggregate(raster_vege_height_classif_9m,10)
par(mfrow=c(1,2))
plot(raster_vege_height_classif_9m)
plot(raster_vege_height_classif_9m_10)

# carte d'occupation des sols OSO (produite par le Centre d'Expertise Scientifique sur l'occupation des sols (CES OSO))
oso <- terra::rast("M:/CESBIO/OSO_20220101_RASTER_V1-0/DATA/OCS_2022.tif") 
oso <- raster::crop(oso,e)
plot(oso)
#*******************************************************************************************








#*******************************************************************************************


#plot in red the bird locations in winter
grouse_winter_raw<-as.data.frame(data_bg_3V%>%filter(saison=="hiver"))

#create a list of data.frames for each animal
grouse_winter<-multiple_dt_indiv(grouse_winter_raw,"nom")

# keep only 5 animals to simplify the analyses
grouse_winter_5<-grouse_winter[1:5]

#' The first day of location data often shows some unrealistic movements. We remove those observations
for(i in 1:length(grouse_winter))
{
  grouse_winter_5[[i]]<- grouse_winter_5[[i]][(grouse_winter_5[[i]]["date"]) > (first(grouse_winter_5[[i]]["date"]) + ddays(1)),]
}

#pr 1 animal:
#grouse_winter <- grouse_winter[(grouse_winter$date) > min(grouse_winter$date) + days(1), ]



#' Create a dt formatted like a telemetry object
grouse_winter_5_pretelemetry<-grouse_winter_5
for(i in 1:length(grouse_winter_5_pretelemetry))
{
  grouse_winter_5_pretelemetry[[i]]<- pre_telemetry(grouse_winter_5_pretelemetry[[i]])
}

grouse_winter_5_telemetry<-grouse_winter_5_pretelemetry
for(i in 1:length(grouse_winter_5_telemetry))
{
  grouse_winter_5_telemetry[[i]]<- as.telemetry(grouse_winter_5_telemetry[[i]])
}


#View(grouse_winter_telemetry[[1]])

color.vect<-c("black","red","purple","blue","lightblue")

windows()
plot(raster_slope_3V,
     ext=e,
     legend.args = list(text = 'Slope'),
     xlab="x coordinate (Lambert 93)",
     ylab="y coordinate (Lambert 93)",
     main="Locations of tagged-black grouses at the 3 valleys site (winter)")

for (i in (1:length(grouse_winter_5_pretelemetry)))
  {
  points(grouse_winter_5_pretelemetry[[i]]$X_GPS_lambert93, # x coordinate
         grouse_winter_5_pretelemetry[[i]]$Y_GPS_lambert93, # y coordinate
         col=color.vect[i],pch=20,cex=0.5)                  # points visual features 
  }



# plot the study site with ggplot()

#convert the raster to points for plotting
map <- rasterToPoints(raster_slope_3V_10)
#Make the points a dataframe for ggplot
map_df <- data.frame(map)
#Make appropriate column headings
colnames(map_df) <- c("X_GPS", "Y_GPS", "slope")

# create a list with bird's names to plot the correct legend
vect_nicknames<-list()
for(i in (1:length(grouse_winter_5_pretelemetry)))
  {
  vect_nicknames[[i]]<-unique(grouse_winter_5_pretelemetry[[i]]["individual.local.identifier"])
  }
vect_nicknames<-unlist(vect_nicknames)

# renamed each data frame from the list of data frames by the name of each bird 
names(grouse_winter_5_pretelemetry)<-vect_nicknames


# plot the birds
g_positions_5birds<-ggplot(map_df,aes(x=X_GPS, y=Y_GPS))+
  geom_raster(aes(fill=slope))+
  geom_point(data = bind_rows(grouse_winter_5_pretelemetry, .id="df"),aes(x=X_GPS_lambert93,y=Y_GPS_lambert93,colour = df))+
  theme_bw() +
  coord_equal() +
  xlim(e[1],e[2])+
  ylim(e[3],e[4])+
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
  scale_color_discrete(name = "Animal")+
  ggtitle("Locations of the bird Abel at the 3 valleys site (winter)")+
  xlab("x coordinate (Lambert 93)")+
  ylab("y coordinate (Lambert 93)")
#scale_fill_distiller(palette = "Spectral")
#geom_raster(aes(x = "X_GPS", y = "Y_GPS", fill = "slope"))

windows()
g_positions_5birds


#*******************************************************************************************


# Describing the spatial correlations between observation data
#*******************************************************************************************

#' Fit ctmm model : Continuous-Time Movement Modeling

# Pooling Variograms : If multiple individuals exhibit similar movement behaviors

# To prospect if the sampling there is a regular or irregular sampling schedule 
# or to visualyze data when the sampling rate changes during data collection
par(mfrow=c(1,1))
dt.plot(grouse_winter_5_telemetry) #here the sampling is semi-irregular 

# visualizing the irregular sampling schedule 

dt_box<-data.frame("birds"=1:length(grouse_winter_5_telemetry),"interval_hr"=summary(grouse_winter_5_telemetry)$interval,"animal"=vect_nicknames)

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
SVF_0 <- lapply(grouse_winter_5_telemetry,variogram) # population variogram
SVF_0<- mean(SVF_0)
plot(SVF_0,fraction=0.005,level=c(0.5,0.95))
title("Population variogram\n(assuming homogenous sampling schedule)")

# population variogram considering the irregular sampling schedule
timelags <- c(1,12,24,36) %#% "hour" # the order has no importance
SVF <- lapply(grouse_winter_5_telemetry,variogram,dt=timelags) # population variogram considering the GPS were programmed to cycle between 1, 12, 24 hour sampling intervals
SVF<- mean(SVF)
plot(SVF,fraction=0.005,level=c(0.5,0.95),main="Population variogram \n(considering the irregular sampling schedule)")

#  HISTOGRAM of time between 2 locations 
par(mfrow=c(1,1))
windows()

ggplot(data = bind_rows(grouse_winter_5_pretelemetry, .id="df"),aes(x=as.POSIXct(dt,format="%d %H:%M:%S")))+
  geom_histogram(fill="#FF6666", bins = 50)+
  facet_grid(df ~ .)+
  scale_x_datetime(date_labels = "%d d %H h")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Time between 2 locations")

ggplot(data = bind_rows(grouse_winter_5_pretelemetry, .id="df"),aes(x=as.POSIXct(dt,format="%d %H:%M:%S")))+
  geom_histogram(fill="#FF6666", bins = 50)+
  facet_grid(df ~ .)+
  scale_x_datetime(date_labels = "%d d %H h %M min",breaks ="3 hours",expand = c(0, 0))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Time between 2 locations")


#' ctmm.guess --> large approximation of the maximum likelyhood, inaccurate estimation of the parameters 
grouse_winter_5_guess <- lapply(grouse_winter_5_telemetry,ctmm.guess, CTMM=ctmm(isotropic = TRUE), interactive = FALSE) #isotropic = TRUE => s'éloigne du centre de manière identique dans toutes les directions

#plots a variogram object overlayed with a continuous-time movement model guesstimated from the variogram's shape
# isotropic = TRUE beacuse we consider the home range (espace vital) 
# as a sphere (attractor center), 
# even if an ellipse is more realistic (anisotropy) 
# but the function is not optized with (isotropic=F)

#model selected (approximation)
grouse_winter_5_guess_summary<-lapply(grouse_winter_5_guess,summary)


# selection of the 5 best model structures
fitted_models_grouse_winter_5<-lapply(grouse_winter_5_telemetry,ctmm.select,CTMM=grouse_winter_5_guess, verbose=TRUE)
#CTMM = GUESS marche pas toujours, CTMM = A ctmm movement-model object containing the initial parameter guesses
fitted_models_grouse_winter_5_summary<-lapply(fitted_models_grouse_winter_5,summary)
# "OUF anisotropic" is the "best" model, IID is the conventional model 
best_model<-fitted_models_grouse_winter_5[[1]][1]

# Visualizing the SVF of the guess model and comparison of the 2 best fitted models on variogram
windows()
par(mfcol=c(2,5))
for ( i in (1:length(grouse_winter_5_guess)))
  {
  plot(SVF, CTMM=grouse_winter_5_guess[[i]], col.CTMM=i,new=F, fraction=0.2,level=c(0.5,0.95),main=grouse_winter_5_guess_summary[[i]]$name,sub=paste(" \narea estimated =",round(grouse_winter_5_guess_summary[[i]]$CI[1,2],3)),cex.sub=1.2,font.sub=2,cex.lab=1.5,cex.main=2.2) #variogram.fit(vg.grouse) is more approximative
  plot(SVF, CTMM=grouse_winter_5_guess[[i]], col.CTMM=i,new=F, fraction=0.0005,level=c(0.5,0.95),cex.sub=2,cex.lab=1.5) #variogram.fit(vg.grouse) is more approximative
  }

#visualizing the home range density estimates against the position data                                                                                                                                         

#' Fit akde (take into account the autocorrelation of the positions in the dataset)
grouse_winter_5_akde<-lapply(grouse_winter_5_telemetry,akde,CTMM=best_model)

windows()
par(mfrow=c(1,5))
for ( i in (1:length(grouse_winter_5_guess)))
{
  plot(grouse_winter_5_telemetry[[i]],UD=grouse_winter_5_akde[[i]],main=vect_nicknames[[i]])
}



#*******************************************************************************************



#' # Minimal example of rsf.fit
#*******************************************************************************************

#' Create named list of rasters
raster::readAll(raster_slope_3V_10) # to save the raster in the RAM and save time
be <- list("slope1" = raster_slope_3V_10)

#' The integrator = "Riemann" option is still experimental, we use it here because it is much faster
grouse_winter_5_rsf_riemann<-list()
for (i in 1: length(grouse_winter_5_telemetry))
{
  grouse_winter_5_rsf_riemann[[i]]<-rsf.fit(grouse_winter_5_telemetry[[i]], grouse_winter_5_akde[[i]], R = be, integrator = "Riemann")
}

# R = must be a list of rasters to fit Poisson regression coefficients to (under a log link)

grouse_winter_5_rsf_riemann_summary<-lapply(grouse_winter_5_rsf_riemann,summary)


#' Range distribution (includes the ranging behaviour) (étendue de la distribution)
agde_grouse_5_winter<-list()
for (i in 1: length(grouse_winter_5_telemetry))
{
  agde_grouse_5_winter[[i]]<-agde(CTMM = grouse_winter_5_rsf_riemann[[i]],be)
}


windows()
par(mfrow=c(1,5))
for ( i in (1:length(grouse_winter_5_telemetry)))
{
  plot(agde_grouse_5_winter[[i]],main=vect_nicknames[[i]])
}



mean_rsf<-ctmm:mean(grouse_winter_5_rsf_riemann)
mean_akde<-ctmm:mean(grouse_winter_5_akde)

#*******************************************************************************************

#rsf with more raster than the slope

#' Create named list of rasters
be2 <- list("slope1" = raster_slope_3V_10,"high_vegetation"=raster_high_vege_classif_9m_10)

#' The integrator = "Riemann" option is still experimental, we use it here because it is much faster
grouse_winter_5_rsf_riemann2<-list()
for (i in 1: length(grouse_winter_5_telemetry))
{
  grouse_winter_5_rsf_riemann2[[i]]<-rsf.fit(grouse_winter_5_telemetry[[i]], grouse_winter_5_akde[[i]], R = be2, integrator = "Riemann")
}

# R = must be a list of rasters to fit Poisson regression coefficients to (under a log link)

grouse_winter_5_rsf_riemann2_summary<-lapply(grouse_winter_5_rsf_riemann2,summary)

