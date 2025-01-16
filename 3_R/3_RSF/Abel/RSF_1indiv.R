#### PhD Tetralpes project ####

# Alice Bordes #

# September 2024 #

# Description:

# RSF on 1 indiv



#### 1_Loading objects ----

### Loading libraries ---- 
#********************************************************************
library(move2)
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
# library(moveVis)
library(terra)
library(future.apply)
library(tidyterra)
library(ggnewscale)
library(gridExtra)
library(grid)
library(openxlsx)
library(janitor)
library(dplyr)
library(ctmm)
library(adehabitatHR)
library(sjmisc)
library(raster)
#********************************************************************


### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD"
#********************************************************************

# Loading data ----
#********************************************************************

### DATASET
birds_bg_dt<-read.csv2(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry_2024_10.csv"),sep=",") #upload the file from a csv, not a move2 object


### VECTORS

# 3V borders 
borders_3V_vect <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect <- terra::vect(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))

# Environment stack
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/env_RL_list.RData"))
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/scaled_env_RL_list.RData"))

# Visitor numbers
visitor_meribel <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/meribel_visitors.csv", sep=",")
visitor_meribel$Date <- as.Date(visitor_meribel$Date)
visitor_meribel$Total_std <- scale(visitor_meribel$Total)

# Snow deph
snow_meribel <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/snow_depth/meribel_snow_depth.csv", sep=",")
snow_meribel$Date <- as.Date(snow_meribel$Date)
snow_meribel <- snow_meribel %>% group_by(Date) %>% summarise(snow.depth = mean(cumul.H.neige.cm))
snow_meribel$snow.depth_std <- as.vector(scale(snow_meribel$snow.depth))

# ski resort identification
ski_lift_traffic_3V <- st_read("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_lift_traffic_3V.gpkg")
dt_resorts <- read.csv2(file.path(base,"Tetralps","2_Data","bg_winter_assign_valley_resort.csv"))
#********************************************************************



### Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/Homerange_visu/mean_size_area.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/Homerange_visu/visu_home_range.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/Homerange_visu/distance_home_range_capture_site.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/Homerange_visu/multi_graph_home_range.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/RSF/plot_check_RSF_results.R")
#********************************************************************


#********************************************************************
setwd(base)

#select the bird and the season
bird="Alpha"
season1="hiver"
season="hiver2"
list_of_animals = bird
#********************************************************************



### Loading data for rsf ----
#********************************************************************
#### Creation of a global telemetry object (for all birds)

birds_sample_bg_pretele <- birds_bg_dt %>% filter(animal.ID %in% list_of_animals)

## add visitor number and snow depth according to the bird's location (Meribel, Courchevel or Les Ménuires)
# Alpha is in Méribel
birds_sample_bg_pretele <- birds_sample_bg_pretele %>% mutate("Date" = as.Date(timestamp))
birds_sample_bg_pretele <- left_join(birds_sample_bg_pretele, visitor_meribel %>% dplyr::select(Date,Total_std), by = "Date" )
birds_sample_bg_pretele <- birds_sample_bg_pretele %>% rename(total.visitors.meribel = Total_std)
birds_sample_bg_pretele <- left_join(birds_sample_bg_pretele, snow_meribel %>% dplyr::select(Date,snow.depth_std), by = "Date" )
birds_sample_bg_pretele <- birds_sample_bg_pretele %>% rename(snow.depth = snow.depth_std)



# # First, join both snow datasets
# birds_sample_bg_pretele2 <- birds_sample_bg_pretele %>%
#   left_join(snow_meribel %>% rename(snow.depth.meribel = snow.depth_std) %>% dplyr::select(Date, snow.depth.meribel), by = "Date") %>%
#   left_join(snow_courch %>% rename(snow.depth.courchevel = snow.depth_std) %>% dplyr::select(Date, snow.depth.courchevel), by = "Date") %>%
#   left_join(visitor_meribel %>% dplyr::select(Date, Total_std) %>% rename(total.visitors.meribel = Total_std), by = "Date") %>%
#   left_join(visitor_courch %>% dplyr::select(Date, Total_std) %>% rename(total.visitors.courch = Total_std), by = "Date") %>%
#   left_join(visitor_valtho %>% dplyr::select(Date, Total_std) %>% rename(total.visitors.valtho = Total_std), by = "Date") %>%
#   
#   # Use case_when to set snow.depth based on valley
#   mutate(snow.depth = case_when(
#     valley == "Courchevel" ~ snow.depth.courchevel,
#     valley == "Les Allues" ~ snow.depth.meribel,
#     valley == "Les Belleville" ~ snow.depth.meribel,
#     TRUE ~ NA_real_  # If valley is neither, set NA
#   ),
#   total.visitors = case_when(
#     resort == "Méribel" ~ total.visitors.meribel,
#     resort == "Méribel-Mottaret" ~ total.visitors.meribel,
#     resort == "Courchevel" ~ total.visitors.courch,
#     resort == "Les Ménuires" ~ total.visitors.valtho,
#     TRUE ~ NA_real_
#   )) %>%
#   
#   # Clean up by removing intermediary columns
#   dplyr::select(-total.visitors.meribel, -total.visitors.courch,-total.visitors.valtho, -snow.depth.meribel, -snow.depth.courchevel)


telemetry <- as.telemetry(birds_sample_bg_pretele,projection="EPSG:2154",
                          keep=c("saison",
                                 "saison2",
                                 "period_jour",
                                 "animal.sex",
                                 "animal.life.stage", 
                                 "total.visitors.meribel",
                                 "snow.depth"))


#### projection()!! only once for a two-dimension object (such as a raster), to avoid distortion, + change the resolution (aggreate) in the same time i change the projection
ctmm::projection(telemetry)<-"EPSG:2154" # nead to project my telemetry data in the projection of my rasters

# winter telemetry
telemetry_winter <- telemetry[telemetry$saison2==season,]
telemetry_winter$snow.depth <- scale(telemetry_winter$snow.depth)
telemetry_winter$total.visitors.meribel <- scale(telemetry_winter$total.visitors.meribel)

# home range akde
guess_winter <- ctmm.guess(telemetry_winter,
                           CTMM = ctmm(isotropic = TRUE),
                           interactive = FALSE)

fit_winter <- ctmm.select(telemetry_winter,
                          guess_winter)

akde_winter <- akde(telemetry_winter,
                    fit_winter)
#********************************************************************



### 1_Data visualization ----
#********************************************************************
### We read in this dataset downloaded from Movebank directly as a move2 object
bg <- vroom::vroom(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry_2024_10.csv")) # with vroom package, timestamps get recognized, are assumed to be in UTC

# now create a move2 object from a data.frame
bg_move2 <- mt_as_move2(bg, 
                        coords = c("location.long","location.lat"),
                        crs = "EPSG:4326",
                        time_column = "timestamp",
                        track_id_column = "animal.ID",  # name of the column which will be define your tracks
                        na.fail = F) # allows or not empty coordinates


#### basic plots colored by individual (with ggplot2)
animal = "Ferie"
birds_sample_bg <- bg_move2 %>% filter(animal.ID %in% c(animal)) %>%filter(saison2 %in% c("hiver1"))

# telemetry points and trajectories by season
ggplot()+ 
  geom_sf(data = mt_track_lines(birds_sample_bg))+
  geom_sf(data = birds_sample_bg, aes(color = saison), size = 1)+ # to transform data into lines
  scale_color_manual(values = c("hiver" = "blue", 
                                "ete" = "deeppink", 
                                "printemps" = "lightgreen", 
                                "automne" = "orange"))


# strava in background
UD_mybird_spatial <- st_as_sf(SpatialPolygonsDataFrame.UD(l_akde_winter[[animal]][["all"]],level.UD=.95,level=.95))
ggplot()+
  geom_spatraster(data = crop(rast(env_RL_list[["strava"]]), ext(st_bbox(UD_mybird_spatial))*3))+
  geom_sf(data = birds_sample_bg, aes(geometry = geometry) , color = "red", size = 1)+ # to transform data into lines
  labs(title = animal)


# animal location in the ski domain
ggplot() + theme_void() +
  ggtitle("Black grouse GPS-tagged in the 3 Vallées region (Nothern Alps)")+
  geom_spatvector(data = borders_3V_vect,fill = NA, aes(color = NOM))+
  scale_color_manual(values = c("Courchevel" = "blue", "Les Allues" = "turquoise", "Les Belleville" = "darkblue")) +
  labs(color = "Valley")+
  new_scale_color()+
  geom_sf(data = ski_lift_traffic_3V, aes(color = resort))+
  scale_color_manual(values = c("Val Thorens-Orelle" = "orange", "Les Ménuires" = "purple", "NA" = "grey","Courchevel" = "lightgreen","Méribel-Mottaret" = "red"))+
  geom_sf(data = birds_sample_bg) +
  new_scale_color()+
  geom_sf(data = mt_track_lines(birds_sample_bg), aes(color = `animal.ID`)) # to transform data into lines


# period covered by the activity monitoring
date.limite.suivi <- birds_sample_bg %>% group_by(animal.ID) %>% slice(1,n())
date.limite.suivi

period.suivi <- birds_sample_bg %>% group_by(animal.ID) %>%
  summarize(nb_points=n(),  "period covered (days)" = as.numeric(difftime(max(study.local.timestamp), min(study.local.timestamp), units = "days")))
period.suivi
#********************************************************************



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


#### 3.1_Required assumptions to calculate and interprete an home range ----
#*****
# The animal must show a resident behevior on the considered scale : 
# "Finally, at larger scales, most animals will exhibit a tendency to remain in a defined region or ‘home range’". (Calabrese, 2016)


# The variogram, or semivariogram, plots time lags on the x-axis for all pairs of observations
# against their semi-variance (half of the variance for the distance each observation pair) on the y-axis. (Animove 2022)


# Bird's VARIOGRAM
# assuming homogenous sampling schedule (which is not the case)
SVF_0 <- variogram(telemetry_winter) # bird's variogram
plot(SVF_0,fraction=0.9,level=c(0.5,0.95),CTMM=fit_winter)
title(paste0(bird,"'s empirical variogram (90%) - winter\n"),cex.main=1,line = -2) 
# The velocity autocorrelation timescale visually corresponds to width of the concave bowl shape at the beginning of the variogram
# variogram = stationarity autocorrelation structure
# variations are smooth when irregular sampling schedule, assume regular sampling schedule (positions are not weighet according to the time interval)


### 3.2_Setting the limit of the study area for the bird ----
#*****
# calculating the 99% HR
r_mybird_akde_99 <- SpatialPolygonsDataFrame.UD(akde_winter,level.UD=.99,level=.95) # UD area at 99% with a 95% confidence level for the magnitude of the above area

# calculating the mcp 
subset_df <- telemetry_winter[, c("x", "y")]
class(subset_df) <- "data.frame"
coordinates(subset_df) <- ~x + y # Perform the Minimum Convex Polygon calculation
mcp_result <- mcp(subset_df, percent = 100) # Create a SpatialPoints object


max(ext(r_mybird_akde_99),ext(mcp_result))
min(ext(r_mybird_akde_99),ext(mcp_result))

e_mybird <- c(min(ext(r_mybird_akde_99),ext(mcp_result))[1],
              max(ext(r_mybird_akde_99),ext(mcp_result))[1],
              min(ext(r_mybird_akde_99),ext(mcp_result))[2],
              max(ext(r_mybird_akde_99),ext(mcp_result))[2])

# e_mybird<-c(
#   min(telemetry_winter$x),
#   max(telemetry_winter$x),
#   min(telemetry_winter$y),
#   max(telemetry_winter$y))


plot(e_mybird[1:2],e_mybird[3:4],type="n")
terra::plot(mcp_result,add=T) ; terra::plot(telemetry_winter,add=T) # plot results to check
terra::plot(r_mybird_akde_99,add=T, border="blue")


# Apply a buffer of 200 units
polygon <- as.polygons(ext(e_mybird))
crs(polygon) <- "EPSG:2154"
buffered_polygon <- terra::buffer(polygon, width = 200, joinstyle = "mitre") # joinstyle = "mitre" : to correctly represent the limit of the area # width = Unit is meter if x has a longitude/latitude CRS, or in the units of the coordinate reference system in other cases (typically also meter)
ggplot()+geom_sf(data=buffered_polygon)+geom_sf(data=polygon)

e_mybird_buff <- as.vector(ext(buffered_polygon))

### 3.3_Visualization "raw" HR (without environmental effects) ----
#********************************************************************
# plot(birds_sample_bg_telemetry_seasons[[bird]][[season]],UD=birds_sample_bg_akde_seasons[[bird]][[season]]) #95% HR + incertities around this 95% HR

# source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multi_graph_home_range.R")
# outputfile="C:/Users/albordes/Documents/PhD/Animove2024/my_project/rsf/outputs/season_HR/"
# multi_graph_HR(birds_sample_bg_telemetry_seasons,birds_sample_bg_akde,outputfile,writeplot = TRUE)


par(mar = c(5, 4, 4, 2) + 0.1) # Set margins back to default
r_mybird_akde<-ctmm::raster(akde_winter, DF = "PMF") # PMF  = probability mass function = proba to find the animal in 1 pixel, so the value sum up to 1
terra::plot(r_mybird_akde,ext=extent(e_mybird)*2)

UD_mybird_spatial <- SpatialPolygonsDataFrame.UD(akde_winter,level.UD=.95,level=.95)
UD_mybird_spatial_akde_contours<-mask(r_mybird_akde,UD_mybird_spatial)

terra::plot(r_mybird_akde,ext=extent(e_mybird)*2)
terra::plot(add=TRUE,UD_mybird_spatial)
#********************************************************************


### 3.4.1_Expected results of the RSF ----
#********************************************************************
#birds 1,2,6,8 have an home range for "hiver1"
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/RSF/plot_check_RSF_results.R")
plot_check_RSF_res(telemetry_winter,akde_winter,"habitats",analysis_object="study_area",writeplot=TRUE)
plot_check_RSF_res(telemetry_winter,akde_winter,"strava",analysis_object="study_area",data_visu="continuous",writeplot=TRUE)
#********************************************************************


### 3.4.2_RSF ----
#********************************************************************

# Selection of the rasters to use in the RSF
env_RL_list_selection <- scaled_env_RL_list[!names(scaled_env_RL_list) %in% "slope"]
env_RL_list_selection <- env_RL_list[!names(env_RL_list) %in% "slope"]


### RSF function 
#' The integrator = "Riemann" option is much faster
### Crop the environment stack around the bird of interest
#' cropping the stack environment to the extent of the bird data locations *2
env_RL_list_cropped <- lapply(env_RL_list_selection, function(raster) {
  terra::crop(raster, extent(e_mybird)*2)
})


# Loop through each raster in the list and apply a function
par(mfrow = c(2,ceiling(length(env_RL_list_selection)/2)))
for (i in seq_along(env_RL_list_selection)) {
  # Apply a function to the raster (example: plotting)
  plot_layers <- function(x) { terra::plot(x, main = names(x)) } # Define the function
  plot_layers(env_RL_list_selection[[i]]) # Call the function
}

set.seed(3)
mybird_rsf <- rsf.fit(l_telemetry_winter[["Daisy"]][[1]],
                      l_akde_winter[["Daisy"]][[1]],
                      R = scaled_env_RL_list_selection,
                      integrator = "MonteCarlo",   #Riemann = faster option but only for spatial variables (rasters); MonteCarlo = for spatial and temporal variables
                      formula = ~ elevation + squared_elevation + strava + 
                                  strava:total.visitors.std +
                                  leks +
                                  Shrubs +
                                  Trees +
                                  Cliffs_water +
                                  Buildings )
summary(mybird_rsf)
# negative effect of strava


set.seed(3)
mybird_rsf_mc_strava <- rsf.fit(telemetry_winter,
                                akde_winter,
                                R = env_RL_list_cropped,
                                integrator = "MonteCarlo",   #Riemann = faster option but only for spatial variables (rasters); MonteCarlo = for spatial and temporal variables
                                formula = ~ elevation + elevation^2 + leks + strava + carto_habitats_winter)
summary(mybird_rsf_mc_strava)
# negative effect of strava



set.seed(3)
mybird_rsf_mc_strava <- rsf.fit(telemetry_winter, 
                                akde_winter,  
                                R = env_RL_list_selection,
                                integrator = "MonteCarlo",   #Riemann = faster option but only for spatial variables (rasters); MonteCarlo = for spatial and temporal variables
                                formula = ~ elevation + squared_elevation + strava*total.visitors.meribel) 
summary(mybird_rsf_mc_strava)


set.seed(3)
mybird_rsf_mc_strava.visitors <- rsf.fit(telemetry_winter, 
                                         akde_winter,  
                                         R = env_RL_list_selection, # scale raster to add an interaction
                                         integrator = "MonteCarlo",   #Riemann = faster option but only for spatial variables (rasters); MonteCarlo = for spatial and temporal variables
                                         formula = ~ elevation + squared_elevation + strava*total.visitors.meribel) # to estimate the interaction, necessary to put the strava effect alone to estimate the intercept (but not necessary to put the temporal variable alone bc the intercept will be suppressed anyway)
summary(mybird_rsf_mc_strava.visitors)

set.seed(3)
mybird_rsf_mc_strava_hab <- rsf.fit( telemetry_winter, 
                                     akde_winter,  
                                     R = env_RL_list_selection,
                                     integrator = "MonteCarlo",   #Riemann = faster option but only for spatial variables (rasters); MonteCarlo = for spatial and temporal variables
                                     formula = ~ elevation + squared_elevation + leks + strava + 
                                       Shrubs +
                                       Trees +
                                       Cliffs_water +
                                       Buildings) 
summary(mybird_rsf_mc_strava_hab)


set.seed(3)
mybird_rsf_mc_hab_snow <- rsf.fit( telemetry_winter, 
                                     akde_winter,  
                                     R = env_RL_list_selection,
                                     integrator = "MonteCarlo",   #Riemann = faster option but only for spatial variables (rasters); MonteCarlo = for spatial and temporal variables
                                     formula = ~ elevation + squared_elevation +
                                     # strava*total.visitors.meribel +
                                     Shrubs*snow.depth +
                                     Trees*snow.depth +
                                     Cliffs_water*snow.depth +
                                     Buildings*snow.depth) 


set.seed(3)
mybird_rsf_all <- rsf.fit( telemetry_winter, 
                                   akde_winter,  
                                   R = env_RL_list_selection,
                                   integrator = "MonteCarlo",   #Riemann = faster option but only for spatial variables (rasters); MonteCarlo = for spatial and temporal variables
                                   formula = ~ elevation + squared_elevation +
                                     leks +
                                     Shrubs*snow.depth +
                                     Trees*snow.depth +
                                     Cliffs_water*snow.depth +
                                     Buildings*snow.depth +
                                     strava*total.visitors.meribel) 
summary(mybird_rsf_all)







set.seed(3)
mybird_rsf_mc <- rsf.fit(telemetry_winter, 
                         akde_winter,  
                         R = env_RL_list_cropped_rsf,
                         integrator = "MonteCarlo",   #Riemann = faster option but only for spatial variables (rasters); MonteCarlo = for spatial and temporal variables
                         formula = ~ elevation + squared_elevation + leks + strava:total.visitors.meribel + carto_habitats_winter:snow.depth) 



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

env_RL_list_cropped_rsf2 = list(env_RL_list_cropped[["elevation"]],env_RL_list_cropped[["squared_elevation"]],env_RL_list_cropped[["leks"]],env_RL_list_cropped[["strava"]])
names(env_RL_list_cropped_rsf2) <- c("elevation","square_elevation","leks", "strava")


### RSF function 
#' The integrator = "Riemann" option is much faster
set.seed(3)
mybird_rsf_riemann2 <- rsf.fit(telemetry_winter, akde_winter,  
                               R = env_RL_list_cropped_rsf2,
                               integrator = "Riemann") #Riemann = faster option

summary(mybird_rsf_riemann2)

set.seed(3)
mybird_rsf_riemann2 <- rsf.fit(telemetry_winter, akde_winter,  
                               R = env_RL_list_cropped_rsf2,
                               integrator = "Riemann") #Riemann = faster option

summary(mybird_rsf_riemann2)





# plot estimated parameters beta ~ the integrated area 
ggplot()+
  geom_point(aes(y = summary(mybird_rsf_riemann2)$CI[1:(length(summary(mybird_rsf_riemann2)$CI[,"est"])-2),"est"], 
                 x = "AI", 
                 group = summary(mybird_rsf_riemann2)$CI[1:(length(summary(mybird_rsf_riemann2)$CI[,"est"])-2),"est"],
                 color = names(summary(mybird_rsf_riemann2)$CI[1:(length(summary(mybird_rsf_riemann2)$CI[,"est"])-2),"est"])))+
  labs( title=paste("Impact of the integration area size"),
        x = "Integration area",
        y = "Coefficient value",
        color = "RSF Estimated coefficients")







### 3.5_Suitability map ----

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


### 3.6_ Range distribution (includes the ranging behaviour) ----
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


#### 3.7_Selection-informed akde : akde_rsf ----

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
  geom_spatraster(data=rast(env_RL_list[["strava"]]))+
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





ggplot()+
  geom_spatraster(data = rast(env_RL_list[[5]]))+
  scale_fill_manual(name = "Winter habitat\nclasses",
                    values = c("1"="#CCCCCC","2"="#FF99CC","3"="#99FF99","4"="#993300","5"="#99CCFF"),na.value ="#666666",
                    labels = c("1"="Soils","2"="Shrubs","3"="Trees","4"="Buildings","5"="Others"))+
  labs( title=paste("Habitat cartography"),
        x = "Longitude",
        y = "Latitude",
        fill = "Legend")+
  theme_classic()



ggplot()+
  geom_spatraster(data = rast(env_RL_list[[6]]))+
  scale_fill_manual(values = c("0"="white","1"="blue"), labels=c("0"="No lek","1"="Lek place"))+
  # scale_fill_manual(name = "Winter habitat\nclasses",
  #                   values = c("0"="#CCCCCC","1"="#FF99CC"),na.value ="#666666",
  #                   labels = c("0"="Soils","1"="Shrubs"))+
  labs( title=paste("Lek places"),
        x = "Longitude",
        y = "Latitude",
        fill = "Lek places")+
  theme_classic()


