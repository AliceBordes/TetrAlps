#### PhD TetraAlps project ####

# Alice Bordes #

# August 2024 #

# Description:

# Formatting environment dataset for RSF and SSF



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
library(raster)
library(terra)
library(future.apply)
library(tidyterra)
library(ggnewscale)
library(broom)
library(janitor)
library(lubridate)
library(openxlsx)
library(tidyverse)
library(dplyr)
#********************************************************************

# Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/Formatting_data/formatting_environment_data.R")
#********************************************************************


### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD/Tetralps"
grid_dim = "modal"
scaled = "yes"
#********************************************************************


# Loading rasters ----
#********************************************************************
### GRID

# Define extent (same as mnt but rounded properly)
xmin <- 963980
xmax <- 1002350
ymin <- 6464370
ymax <- 6495470

# The total width and height:
  # Width = 1002350 - 963980 = 38370 meters
  # Height = 6495470 - 6464370 = 31100 meters


if(grid_dim == "10m" || grid_dim == "modal")
{
  # Create a 10m resolution grid matching the extent
  grid <- terra::rast(ext(c(xmin, xmax, ymin, ymax)), resolution = c(10, 10), crs = "EPSG:2154")
}
if(grid_dim == "1m")
{
  # Create a 1m resolution grid matching the extent
  grid <- terra::rast(ext(c(xmin, xmax, ymin, ymax)), resolution = c(1, 1), crs = "EPSG:2154")
}

# grid_10m <- terra::rast(terra::vect(file.path(base,"1_RAW_DATA","grid_10m.gpkg")))
# res(grid_10m) <- 10


### RASTERS

# elevation
mnt <- terra::rast(file.path(base,"2_DATA","environmental_raster","mnt_ign.tif"))

# slope 3V
slope_3V <- terra::rast(file.path(base,"2_DATA","environmental_raster","slope_3V_ign.tif"))
crs(slope_3V) <- crs(mnt)

# strava
strava_without_aerial <- terra::rast(file.path(base,"2_DATA","environmental_raster","r_strava_without_aerial_cables.gpkg"))

strava_backcountry <- terra::rast(file.path(base,"1_RAW_DATA","strava","3Vallees","strava_sport_BackcountrySki_2025_01_09.tif"))
if(length(names(strava_backcountry))!=1)
{
  strava_backcountry <- strava::as_numeric(strava_backcountry)
}

strava <- terra::rast(file.path(base, "1_RAW_DATA","strava","3Vallees","strava_3V_winter_sports_rgb_single_3Vallees_25_06_2024.tif"))
if(length(names(strava))!=1)
{
  strava <- strava::as_numeric(strava)
}
#if strava = rgb with 4 layers : 
# strava <- terra::rast(file.path(base, "2_DATA/strava/3Vallees/strava_winter_2024_10_03.tif"))
# strava_rgb <- terra::plotRGB(strava)
# strava_rgb <- terra::plot(strava::as_numeric(strava), col = viridis::magma(256))
# resolution of strava is set artificially at 9m such as mnt_9, but the true resolution is 38.21851m

# leks
# r_leks_dist <- terra::rast(file.path(base,"2_DATA","environmental_raster","r_leks_dist.gpkg"))

# 3V winter trails (osm)
# osm_winter <- terra::vect(file.path(base,"2_DATA", "osm_ski_piste.gpkg"))
# ggplot()+geom_spatvector(data = osm_winter, aes(color = piste.type))+ggtitle("Ski trails Open Street Map by type")
# osm_winter_sf <- as_sf(osm_winter)


# habitat cartography
carto_habitats_3V_winter <- terra::rast(file.path(base, "2_DATA","environmental_raster","carto_habitats_3V_winter_5classes_tree.tif"))
# r_Trees <- terra::rast(file.path(base,"2_DATA","environmental_raster",paste0("scaled_bin_Trees_",grid_dim,".tif")))
# r_Shrubs <- terra::rast(file.path(base,"2_DATA","environmental_raster",paste0("scaled_bin_Shrubs_",grid_dim,".tif")))
# r_Buildings <- terra::rast(file.path(base,"2_DATA","environmental_raster", paste0("scaled_bin_Buildings_",grid_dim,".tif")))
# r_Cliffs <- terra::rast(file.path(base,"2_DATA","environmental_raster",paste0("scaled_bin_Cliffs_",grid_dim,".tif")))
#To ensure the raster is in memory: use terra::readAll

r_Trees <- terra::rast(file.path(base,"2_DATA","environmental_raster",paste0("scaled_bin_Trees_",grid_dim,".tif")))
r_Shrubs <- terra::rast(file.path(base,"2_DATA","environmental_raster",paste0("scaled_bin_Shrubs_",grid_dim,".tif")))
r_Buildings <- terra::rast(file.path(base,"2_DATA","environmental_raster", paste0("scaled_bin_Buildings_",grid_dim,".tif")))
r_Cliffs <- terra::rast(file.path(base,"2_DATA","environmental_raster",paste0("scaled_bin_Cliffs_",grid_dim,".tif")))

# carto_habitats_3V_ocsge <- terra::rast(file.path(base,"2_DATA","environmental_raster","carto_habitats_3V_ocsge.tif"))
#********************************************************************



# 1_Formatting rasters ----
#********************************************************************

# Predation : Corentin the fox (probability of presence over the area)
# fox_Corentin_sakde <- terra::rast(file.path(base,"2_DATA/fox_Corentin_simple_akde_PMF.tif")) # simple akde over a period of 7 months

# 1.1_Creating backcountry ski raster for the model ----

lm_backcountry <- lm(values(strava_backcountry) ~ values(strava))
summary(lm_backcountry)
sqrt(summary(lm_backcountry)$adj.r.squared)

values(strava_backcountry) <- lm_backcountry$residuals

terra::plot(strava_backcountry,main="strava_backcountry residuals")


# 1.2_Ski resort visitors ----
meribel_mottaret_visitors <- meribel_mottaret_formatting(save = TRUE)
courchevel_visitors <- courchevel_formatting(save = TRUE)
valtho_visitors <- valtho_formatting(save = TRUE)
menuires_visitors <- menuires_formatting(save = TRUE)

        
# 1.3_Snow depth    
snow_meribel <- meribel_snow_formatting(save = TRUE)
snow_courchevel <- courchevel_snow_formatting(save = TRUE)
#********************************************************************




### 2_Create an environment stack for predictors ----
#********************************************************************

### Reprojecting raster to homogenize the resolutions and to align them

# numeric rasters
if(grid_dim == "10m" || grid_dim == "modal")
{
  mnt <- aggregate(mnt, fact = 10, fun = "mean")
}

mnt <- project(mnt, y = grid, "bilinear")
# if project(y = mnt_10) --> all projection will be aggregated at 10m such as mnt_10
# slope_3V <- project(slope_3V, y = grid, method = "bilinear") # resolution of slope_3V is set at 9m such as mnt_9
strava_without_aerial <- project(strava_without_aerial, y = grid, method = "bilinear") # y = a raster to align on (avoid to use the function resample after), "EPSG:2154" can be specify if there is no argument y. pas besoin de mettre de focal (zone d'influence), method="belinear" advised for continuous raster
# strava_backcountry <- project(strava_backcountry, y = grid, method = "bilinear") 
# r_leks_dist <- project(r_leks_dist, y = grid, method = "bilinear")

if(grid_dim == "10m")
{
  r_Trees <- project(r_Trees, y = grid, method = "bilinear")
  r_Shrubs <- project(r_Shrubs, y = grid, method = "bilinear")
  r_Buildings <- project(r_Buildings, y = grid, method = "bilinear")
  r_Cliffs <- project(r_Cliffs, y = grid, method = "bilinear")
}

if(grid_dim == "modal")
{
r_Trees <- project(r_Trees, y = grid, method = "near")
r_Shrubs <- project(r_Shrubs, y = grid, method = "near")
r_Buildings <- project(r_Buildings, y = grid, method = "near")
r_Cliffs <- project(r_Cliffs, y = grid, method = "near")
}

# categorical rasters
carto_habitats_3V_winter <- project(carto_habitats_3V_winter, y = grid, method = "near")


# Changing names
mnt_squared <- mnt^2
names(mnt_squared) <- "squared_elevation"
names(mnt) <- "elevation"
names(strava_without_aerial) <- "strava_winter_sports"
# names(strava_backcountry) <- "strava_backcountry"
# names(slope_3V) <- "slope"
names(carto_habitats_3V_winter) <- "carto_habitats_winter"
# names(r_leks_dist) <- "leks"


#' stacking it all in an env layer 
envir_stack <- c(mnt,
                 mnt_squared,
                 strava_without_aerial,
                 # strava_backcountry,
                 # slope_3V,
                 carto_habitats_3V_winter)
                 # r_leks_dist)

env_RL_list <- lapply(envir_stack,raster::raster)
names(env_RL_list) <- names(envir_stack)

# Set the Lambert crs (EPSG:2154) to each raster element in env_RL_list
env_RL_list <- lapply(env_RL_list, function(x) {
  crs(x) <- "EPSG:2154"
  return(x)
})

env_RL_list[["carto_habitats_winter"]] <- as.factor(env_RL_list[["carto_habitats_winter"]])

save(env_RL_list,file=file.path(base,"3_R","0_Heavy_saved_models","environment_3V",paste0("env_RL_list_",grid_dim,"02_04_2025.RData")))
load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V",paste0("env_RL_list_",grid_dim,"02_04_2025.RData")))
#********************************************************************





### 3_Scale the environment stack ----
#********************************************************************

# Scaling of the rasters : needed for rsf with interaction in ctmm

  # Scaling of the rasters : needed for rsf with interaction in ctmm
  
  # Users are responsible for standardizing rasters when interactions are supplied!!!!!!
  # scaling raster for interactions : 
  # Scale all rasters in the list using the scale function
  scaled_env_RL_list_1 <- lapply(envir_stack, function(raster_item) {
    # Check if the raster is categorical (e.g., by name or checking levels)
    # Here, we assume the categorical raster has "carto" in its name
    if (grepl("carto_habitats_winter", names(raster_item)) || grepl("leks", names(raster_item)) || grepl("squared_elevation", names(raster_item)) ) {
      return(raster_item)  # Return the categorical raster unchanged
    } else {
      # If it's not categorical, scale the values
      scaled_raster <- scale(raster_item, center = TRUE, scale = TRUE)
    
      # writeRaster(scaled_raster, filename = file.path(base,"2_DATA","environmental_raster",paste0("scaled_", names(raster_item),"_",grid_dim, ".tif")), overwrite = TRUE)
      
      gc()  # Force garbage collection to free up memory
      
      return(scaled_raster)  # Return the scaled raster
    }
  })
  names(scaled_env_RL_list_1) <- names(envir_stack)
  
  scaled_env_RL_list_1[["squared_elevation"]] <- scaled_env_RL_list_1[["elevation"]]^2
  names( scaled_env_RL_list_1[["squared_elevation"]]) <- "squared_elevation"
  
  #' stacking it all in an env layer 
  envir_list_hab <- list( r_Trees,
                          r_Shrubs,
                          r_Buildings,
                          r_Cliffs)
  names(envir_list_hab) <- lapply(envir_list_hab, names)
  
  envir_list <- c(scaled_env_RL_list_1,envir_list_hab)
  
  # Suppress carto_habitats_winter and replace it by multiple binary raster
  envir_list <- envir_list[!names(envir_list) %in% c("carto_habitats_winter")] # supress the unsclaled raster
  
  
  # Set the Lambert crs (EPSG:2154) to each raster element in env_RL_list
  envir_list <- lapply(envir_list, function(x) {
    crs(x) <- "EPSG:2154"
    return(x)
  })
  
  
  
  
  # save raster in the memory
  # envir_list <- lapply(envir_list, readAll)

  

  scaled_env_RL_list <- lapply(envir_list, raster::raster)
  
  scaled_env_RL_list[["Trees"]] <- as.factor(scaled_env_RL_list[["Trees"]])
  scaled_env_RL_list[["Shrubs"]] <- as.factor(scaled_env_RL_list[["Shrubs"]])
  scaled_env_RL_list[["Cliffs"]] <- as.factor(scaled_env_RL_list[["Cliffs"]])
  scaled_env_RL_list[["Buildings"]] <- as.factor(scaled_env_RL_list[["Buildings"]])
  
  # # Define the output directory (modify the path as needed)
  # output_dir <- file.path(base,"2_DATA","RSF_model","2_DATA","heavy_rasters")
  # # Create the directory if it doesn't exist
  # dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  # # Apply writeRaster to each raster in the list
  # lapply(names(envir_list_r), function(name) {
  #   raster_obj <- envir_list_r[[name]]  # Get the raster
  #   
  #   # Define the output filename
  #   filename <- file.path(output_dir, paste0(name, ".tif"))
  #   
  #   # Write the raster to disk
  #   writeRaster(raster_obj, filename, overwrite = TRUE)
  #   
  #   return(filename)  # Return the filename (optional, for debugging)
  # })
  # 
  
  
  # scaled_env_RL_list <- lapply(envir_list_r,raster::readAll) # NOT WORKING
  
  
# cat 1) Soils and low vegetation : Unclassified soil, Fine mineral soil, Coarse mineral soil, Dry or rocky grassland, Herbaceous, Low ligneous
# cat 2) Shrubs
# cat 3) Trees : Unclassified trees, Deciduous trees, Resinous trees
# cat 4) Buildings
# cat 5) Cliffs and water : Cliff, Natural pond, Artificial pond, Waterway, Unclassified

save(scaled_env_RL_list,file=file.path(base,"3_R","0_Heavy_saved_models","environment_3V",paste0("scaled_env_RL_list_",grid_dim,"02_04_2025.RData")))
load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V",paste0("scaled_env_RL_list_",grid_dim,"02_04_2025.RData")))
#********************************************************************

# 
# load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V",paste0("scaled_env_RL_list_",grid_dim,"_partial.RData")))
# 
# load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V",paste0("scaled_env_RL_list_",grid_dim,"_partial2.RData")))
# 
# <- c(envir_list_hab_r,


### 4_Predictors' collinearity ----
#********************************************************************
# Correlation between layers


# 4.1_Collinearity between two continuous rasters, calculation of Pearson coefficient 

# https://statnmap.com/2018-01-27-spatial-correlation-between-rasters/ 
cor(values(env_RL_list[["strava"]]),
    values(env_RL_list[["slope"]]),
    use = "na.or.complete", method = "pearson")
# Pearson's coefficient is an index reflecting a linear relationship between two continuous variables.
# Kendall's or Spearman's coefficients recommended if the data do not necessarily come from a bivariate normal distribution. 
# Kendall's or Spearman's for ordinal variables (= categorical but hierarchical!)


# 4.2_Collinearity between two continuous rasters, calculation of a lm model (sqrt(R-squared) equivalent to Pearson coefficient) 

lm1<-lm(values(env_RL_list[["elevation"]]) ~ values(env_RL_list[["strava"]]))
summary(lm1)
sqrt(summary(lm1)$adj.r.squared) # equivalent to Pearson coefficient 
lm2<-lm(values(env_RL_list[["strava"]]) ~ values(env_RL_list[["elevation"]]))
summary(lm2)
sqrt(summary(lm2)$adj.r.squared) # equivalent to Pearson coefficient 

rast_resid <- env_RL_list[["elevation"]]
values(rast_resid) <- lm1$residuals
rast_resid2 <- env_RL_list[["strava"]]
values(rast_resid2) <- lm2$residuals 

par(mfrow=c(2,2))
terra::plot(env_RL_list[["elevation"]],main="elevation")
terra::plot(rast_resid,main="elevation residuals")
terra::plot(env_RL_list[["strava"]],main="strava")
terra::plot(rast_resid2,main="strava residuals")

# " elevation_9 is underestimated at the south (green part of the graph elevation_9 residuals) and overestimated in the north when using strava as linear predictor
# "strava is underestimated where the strava intensity is high (green part of the graph strava residuals) and overestimated where strava intensity is low when using elevation_9 as linear predictor
# But this is barely relevant as the correlation between elevation_9 and Strava is really weak.

# Now we want to know if strava and strava_backcountry provide the same info 
lm1<-lm(values(strava) ~ values(strava_backcountry))
summary(lm1)
sqrt(summary(lm1)$adj.r.squared)
# Yes! the correlation between both is 80%!

# but we can rather put the residuals of strava_backcountry ~ strava in the model = what is not not well explained by strava 
lm2<-lm(values(strava_backcountry) ~ values(strava))
summary(lm2)
sqrt(summary(lm2)$adj.r.squared)

rast_resid <- strava
values(rast_resid) <- lm1$residuals
rast_resid2 <- strava_backcountry
values(rast_resid2) <- lm2$residuals 

par(mfrow=c(2,2))
terra::plot(strava,main="strava")
terra::plot(rast_resid,main="strava residuals")
terra::plot(strava_backcountry,main="strava_backcountry")
terra::plot(rast_resid2,main="strava_backcountry residuals")

# the model over estimates backcountry ski trails relative to strava  --> so this raster is intersting as a predictor of backcountry ski 
# check the correlation between strava and backcountry ski
lm_resid<-lm(values(strava) ~ values(rast_resid2))
summary(lm_resid)
sqrt(summary(lm_resid)$adj.r.squared) # equivalent to Pearson coefficient 


# 4.3_Collinearity between a continuous raster and a factorial raster

aov1<-aov(values(env_RL_list[["strava"]]) ~ values(env_RL_list[["carto_habitats_winter"]])==5 )
# aov1<-aov(values(scaled_env_RL_list[["strava"]]) ~ values(scaled_env_RL_list[["Cliffs_water"]]) ) # same results if all predictors are scaled
summary(aov1)

tidy_aov1 <- tidy(aov1)
R <- sqrt(tidy_aov1$sumsq[1] / (tidy_aov1$sumsq[1] + tidy_aov1$sumsq[2])) # R-square = Sum Sq(values) / (Total Sum of squares (sq) = Sum Sq(values)+Sum Sq(residuals))
R
# here, R is equivalent to the Pearson coefficient
#********************************************************************



### 5_Crop the environment stack around the bird of interest ----
#********************************************************************
#' cropping the stack environment to the extent of the bird data locations *2
env_RL_list_cropped <- lapply(env_RL_list, function(raster) {
  terra::crop(raster, extent(e_mybird)*2)
})
#********************************************************************



### 6_Predictors visualization ----
#********************************************************************
graph_options <- ggplot()+
  labs( x = "Longitude",
        y = "Latitude")+
  theme(plot.title = element_text(size = 18, face = "bold"), 
        legend.title = element_text(size = 12, face = "bold"), 
        axis.title = element_text(size = 14), 
        panel.background = element_rect(fill=NA, linetype = "solid"),
        axis.line = element_line(colour = "black"))


# Elevation
graph_options+
  geom_spatraster(data = mnt_9)+
  scale_fill_gradientn(name = "DEM (m)",colors = c("#CCFFCC", "#FFFFCC", "#FFCC99", "#FF9966", "#FF6600"),na.value ="transparent")+
  new_scale_fill()+
  geom_spatvector(data = borders_3V_vect,fill = NA, color = "black")+
  ggtitle("Elevation")

# Starva
graph_options+
  geom_spatraster(data=strava_without_aerial)+
  scale_fill_gradientn(name = "Strava intensity",colors=c("#CCCCCC11","#FF6600","#FF3333"),na.value ="transparent")+
  new_scale_fill()+
  geom_spatvector(data = borders_3V_vect,fill = NA, color = "black")+
  labs( title=paste("Strava visitor intensity"),
        fill = "Strava intensity")

# Leks sites
graph_options+
  geom_spatraster(data=r_leks_dist,aes(fill=layer)) +
  geom_spatvector(data = borders_3V_vect,fill = NA, color = "black")+
  scale_fill_gradientn(colors = rev(terrain.colors(10))) +
  labs( title=paste("Leks sites"),
        fill = "Normalized \ndistance \nto the lek")
  
# Winter habitats
graph_options+
  geom_spatraster(data=carto_habitats_3V_winter)+
  scale_fill_manual(name = "Habitat classes",
                    values = c("1"="#CCCCCC","2"="#FF99CC","3"="#99FF99","4"="#993300","5"="#99CCFF"),na.value ="transparent",
                    labels = c("1"="Soils","2"="Shrubs","3"="Trees","4"="Buildings","5"="Others"))+
  geom_spatvector(data = borders_3V_vect,fill = NA, color = "black")+
  labs( title=paste("Habitat cartography"),
        fill = "Habitats")

# Snow
ggplot()+
  geom_line(data = snow_meribel, aes(x = Date, y = H.neige.cm), color = "#3399FF", size=1.5)+
  labs( title=paste("Daily snow depth in Méribel ski resort, winter 2018 - 2019"),
        subtitle = paste("From",noquote(min(snow_meribel$Date)), "to", noquote(max(snow_meribel$Date))),
        x = "Date",
        y = "Snow depth (cm)")+
  theme(plot.title = element_text(size = 18, face = "bold"), 
        legend.title = element_text(size = 12, face = "bold"), 
        axis.title = element_text(size = 14), 
        # panel.background = element_rect(fill=NA, linetype = "solid"),
        axis.line = element_line(colour = "black"))

# Visitors meribel_human_traffic_18_19 <- meribel_human_traffic_18_19[meribel_human_traffic_18_19$Date >= as.Date("2018-11-15") & meribel_human_traffic_18_19$Date <= as.Date("2019-02-14"), ]
meribel_visitors_18_19 <- meribel_visitors[meribel_visitors$Date >= as.Date("2018-11-15") & meribel_visitors$Date <= as.Date("2019-02-14"), ]

ggplot()+
  geom_line(data = meribel_visitors_18_19, aes(x = Date, y = Total), color = "orange", size=1.5)+
  labs( title=paste("Daily visitor numbers in Méribel ski resort, winter 2018 - 2019"),
        subtitle = paste("From",noquote(min(meribel_visitors_18_19$Date)), "to", noquote(max(meribel_visitors_18_19$Date))),
        x = "Date",
        y = "Total visitor numbers")+
  theme(plot.title = element_text(size = 18, face = "bold"), 
        legend.title = element_text(size = 12, face = "bold"), 
        axis.title = element_text(size = 14), 
        # panel.background = element_rect(fill=NA, linetype = "solid"),
        axis.line = element_line(colour = "black"))

#********************************************************************





