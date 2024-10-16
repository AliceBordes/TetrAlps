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
library(moveVis)
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
#********************************************************************


### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD"
#********************************************************************

# Loading data ----
#********************************************************************

### DATASET
birds_bg_dt<-read.csv2(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry.csv"),sep=",") #upload the file from a csv, not a move2 object


### VECTORS

# 3V borders 
borders_3V_vect <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect <- terra::vect(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))

# Environment stack
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/env_RL_list.RData"))
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
                          guess_winter)

akde_winter <- akde(telemetry_winter,
                    fit_winter)
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
#********************************************************************
# calculating the 99% HR
r_mybird_akde_99 <- SpatialPolygonsDataFrame.UD(akde_winter,level.UD=.99,level=.95) # UD area at 99% with a 95% confidence level for the magnitude of the above area

# calculating the mcp 
subset_df <- telemetry_winter[, c("x", "y")]
class(subset_df) <- "data.frame"
mcp(subset_df, percent = 100) # Create a SpatialPoints object
coordinates(subset_df) <- ~x + y # Perform the Minimum Convex Polygon calculation
mcp_result <- mcp(subset_df, percent = 100)


max(ext(r_mybird_akde_99),ext(mcp_result))
min(ext(r_mybird_akde_99),ext(mcp_result))

e_mybird <- c(min(ext(r_mybird_akde_99),ext(mcp_result))[1],
              max(ext(r_mybird_akde_99),ext(mcp_result))[1],
              min(ext(r_mybird_akde_99),ext(mcp_result))[2],
              max(ext(r_mybird_akde_99),ext(mcp_result))[2])

# plot area
plot(e_mybird[1:2],e_mybird[3:4],type="n")
terra::plot(mcp_result,add=T) ; terra::plot(telemetry_winter,add=T) # plot results to check
terra::plot(r_mybird_akde_99,add=T, border="blue")
#********************************************************************




### RSF function 
#********************************************************************

# Predictor selection 
env_RL_list <- env_RL_list[c("elevation", "square_elevation", "strava", "carto_habitats_winter","leks")]



# cropping the predictors to the size chosen for the integration area
buff_vector <- c(1,100,200,500,1000,10000) # different size for the integration area
rsf_summary <- list()
 

for(i in seq_along(buff_vector))
{
# Apply a buffer of 200 units
polygon <- as.polygons(ext(e_mybird))
crs(polygon) <- "EPSG:2154"
buffered_polygon <- terra::buffer(polygon, width = buff_vector[i], joinstyle = "mitre") # joinstyle = "mitre" : to correctly represent the limit of the area # width = Unit is meter if x has a longitude/latitude CRS, or in the units of the coordinate reference system in other cases (typically also meter)
# ggplot()+geom_sf(data=buffered_polygon)+geom_sf(data=polygon)

e_mybird_buff <- as.vector(ext(buffered_polygon))


### Crop the environment stack around the bird of interest
#' cropping the stack environment to the extent of the bird data locations *2
env_RL_list_cropped <- lapply(env_RL_list, function(raster) {
  terra::crop(raster, extent(e_mybird)*2)
})
# cheking environment slacks
# terra::plot(envir_crop)
# terra::plot(envir_crop[["carto_habitats_winter"]])


# RSF
set.seed(3)
mybird_rsf_riemann <- rsf.fit(telemetry_winter, akde_winter,  
                              R = env_RL_list_cropped_rsf,
                              integrator = "Riemann") #Riemann = faster option


sum_rsf <- as.data.frame(summary(mybird_rsf_riemann)$CI, row.names = NULL)
sum_rsf$covariates <- rownames(sum_rsf)
sum_rsf$IA <- paste0("IA_",buff_vector[i],"_m")

rsf_summary[[i]]<- sum_rsf

}

rsf_table <- do.call(cbind,rsf_summary)


rsf_table

#********************************************************************






# plot estimated parameters beta ~ the integrated area 
ggplot()+
  geom_point(aes(y = rsf_table, 
                 x = "AI", 
                 group = summary(mybird_rsf_riemann2)$CI[1:(length(summary(mybird_rsf_riemann2)$CI[,"est"])-2),"est"],
                 color = names(summary(mybird_rsf_riemann2)$CI[1:(length(summary(mybird_rsf_riemann2)$CI[,"est"])-2),"est"])))+
  labs( title=paste("Impact of the integration area size"),
        x = "Integration area",
        y = "Coefficient value",
        color = "RSF Estimated coefficients")







if(!exists("rsf_table")){
  rsf_table <- data.frame("covariates" = rownames(rsf_summary[[1]]$CI), 
                          "IA_1_m" = rsf_summary[[1]]$CI[,"est"])
}else{
  col_name <- paste0("IA_",buff_vector[i],"_m")
  rsf_table[[col_name]] <- rsf_summary[[i]]$CI[,"est"]
}

