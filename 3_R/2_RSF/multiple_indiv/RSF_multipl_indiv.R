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

visitor_valtho <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/valtho_visitors.csv", sep=",")
visitor_valtho$Date <- as.Date(visitor_valtho$Date)
visitor_valtho$Total_std <- scale(visitor_valtho$Total)

visitor_courch <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/courchevel_visitors.csv", sep=",")
visitor_courch$Date <- as.Date(visitor_courch$Date)
visitor_courch$Total_std <- scale(visitor_courch$Total)

# Snow deph
snow_meribel <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/snow_depth/meribel_snow_depth.csv", sep=",")
snow_meribel$Date <- as.Date(snow_meribel$Date)
snow_meribel <- snow_meribel %>% group_by(Date) %>% summarise(snow.depth = mean(cumul.H.neige.cm))
snow_meribel$snow.depth_std <- as.vector(scale(snow_meribel$snow.depth))

snow_courch <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/snow_depth/courchevel_snow_depth.csv", sep=",")
snow_courch$Date <- as.Date(snow_courch$Date)
snow_courch <- snow_courch %>% group_by(Date) %>% summarise(snow.depth = mean(cumul.H.neige.cm))
snow_courch$snow.depth_std <- as.vector(scale(snow_courch$snow.depth))

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
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/RSF/rsf_functions.R")
#********************************************************************


#********************************************************************
setwd(base)

#select the bird and the season
season1 = "hiver"
bird = "Fast"
#********************************************************************
  
  
### Data creation of telemetry, guess, fit and akde objects for rsf ----
#********************************************************************
#### Creation of a global telemetry object (for all birds)

# Ensure `jour` is a Date object
birds_bg_dt <- birds_bg_dt %>%
  mutate(jour = as.Date(jour)) # Ensure `jour` is in Date format

# Add "Day of the Year" (`yday`) and a custom filter for the range
filtered_birds_bg_dt <- birds_bg_dt %>%
  mutate(
    month_day = format(jour, "%m-%d"), # Optional: For debugging or reference
    # yday = yday(jour)                 # Extract the day of the year
  ) %>%
  filter(
    (month(jour) == 11 & day(jour) >= 15) |   # After November 15
      (month(jour) <= 4 & !(month(jour) == 4 & day(jour) > 15)) # Before April 15
  )

head(filtered_birds_bg_dt)



tele_akde(data = birds_bg_dt,
          # birds_vect = names(l_telemetry_winterS),
          season = "hiver",
          subset_category = "saison2",
          outputfolder = file.path(base, "Tetralps", "3_R", "0_Heavy_saved_models", "birds_3V"),
          write = TRUE)
#********************************************************************  


### Loading data for rsf ----
#********************************************************************
  load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_telemetry_winter_saison2.Rdata"))
  load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_guess_winter_saison2.Rdata"))
  load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_fit_winter_saison2.Rdata"))
  load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_akde_winter_saison2.Rdata"))
  
  # load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_telemetry_winter_saison.Rdata"))
  # load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_guess_winter_saison.Rdata"))
  # load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_fit_winter_saison.Rdata"))
  # load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_akde_winter_saison.Rdata"))
#********************************************************************

#********************************************************************  
# Filter to keep only elements with nested lists of length > 1
# l_telemetry_winterS <- Filter(function(x) length(x) > 1, l_telemetry_winter)
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
bird_variogram(l_telemetry_winter, 
               # dt_hour = c(1, 6, 12), 
               outputfolder = file.path(base, "Tetralps", "5_OUTPUTS","RSF","variograms","multipl_variograms"), 
               write = TRUE)

  


### 3.3_Visualization "raw" HR (without environmental effects) ----
#********************************************************************
# 
# UD_mybird_spatial <- SpatialPolygonsDataFrame.UD(akde_winter,level.UD=.95,level=.95)
# UD_mybird_spatial2 <- SpatialPolygonsDataFrame.UD(l_akde_winter[[bird]][["hiver2"]],level.UD=.95,level=.95)
# 
# 
# ggplot() + theme_void() +
#   ggtitle("Black grouse GPS-tagged in the 3 Vallées region (Nothern Alps)")+
#   geom_spatvector(data = borders_3V_vect,fill = NA, aes(color = NOM))+
#   scale_color_manual(values = c("Courchevel" = "darkgreen", "Les Allues" = "lightgreen", "Les Belleville" = "darkgrey")) +
#   labs(color = "Valley")+
#   new_scale_color()+
#   geom_sf(data = st_as_sf(UD_mybird_spatial),fill=alpha("blue",0.1), color = "blue")+
#   geom_sf(data = st_as_sf(UD_mybird_spatial2),fill=alpha("turquoise",0.1), color = "turquoise")
# 
# 
# 
# 
# # Create a data frame to map the "hiver1" and "hiver2" names
# legend_data <- rbind(
#   st_as_sf(UD_mybird_spatial) %>% mutate(winter = "hiver1"),
#   st_as_sf(UD_mybird_spatial2) %>% mutate(winter = "hiver2")
# )
# 
# 
# # Plot with the updated legend
# ggplot() + 
#   theme_void() + theme(plot.title = element_text(size = 16), legend.text = element_text(size=12), legend.title = element_text(size=14)) +
#   ggtitle("Fast winter home ranges based on winter GPS locations in the 3 Vallées region (Northern Alps)") +
#   
#   # Plot the vector borders with valley names
#   geom_spatvector(data = borders_3V_vect, fill = NA, aes(color = NOM)) +
#   scale_color_manual(values = c("Courchevel" = "darkgreen", "Les Allues" = "lightgreen", "Les Belleville" = "darkgrey")) +
#   labs(color = "Valley") +
#   
#   # Start a new scale for the winter layers
#   new_scale_color() +
#   geom_sf(data = legend_data, aes(fill = winter), color = NA, alpha = 0.1) +
#   
#   # Define the colors and labels for the winter legend
#   scale_fill_manual(name = "Winters",
#                     values = c("hiver1" = "blue", "hiver2" = "turquoise"),
#                     labels = c("hiver1" = "First winter", "hiver2" = "Second winter")) +
#   
#   # Add borders for the winter regions
#   geom_sf(data = st_as_sf(UD_mybird_spatial), color = "blue", fill = NA) +
#   geom_sf(data = st_as_sf(UD_mybird_spatial2), color = "turquoise", fill = NA)
# 


#********************************************************************


### 3.4.1_Expected results of the RSF ----
#********************************************************************
#birds 1,2,6,8 have an home range for "hiver1"
# source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/plot_check_RSF_results.R")
# plot_check_RSF_res(telemetry_winter,akde_winter,"habitats",analysis_object="study_area",writeplot=TRUE)
# plot_check_RSF_res(telemetry_winter,akde_winter,"strava",analysis_object="study_area",data_visu="continuous",writeplot=TRUE)
#********************************************************************


### 3.4.2_RSF ----
#********************************************************************

# Selection of the rasters to use in the RSF
scaled_env_RL_list_selection <-  scaled_env_RL_list[!(names(scaled_env_RL_list) %in% c("slope", "leks"))]
# scaled_env_RL_list_selection <-  scaled_env_RL_list[c("elevation", "squared_elevation", "strava","leks")]
  

RSF_birds <- function(telemetry_list, 
                           akde_list,
                           env_raster_list,
                           outputfolder = file.path(base, "Tetralps", "3_R", "0_Heavy_saved_models", "birds_3V"),
                           write = TRUE)
{
  warning("The arguments telemetry_list and akde_list must be unested lists.")
  
  start_time <- proc.time()
  
  
  sum_rsf_multipl <- list()
  
  for(bg in seq_along(akde_list))
  {
    
    ### Setting the limit of the study area for the bird
    # calculating the 99% HR
    r_mybird_akde_99 <- SpatialPolygonsDataFrame.UD(akde_list[[bg]][[1]],level.UD=.99,level=.95) # UD area at 99% with a 95% confidence level for the magnitude of the above area
    
    # calculating the mcp 
    subset_df <- telemetry_list[[bg]][[1]][, c("x", "y")]
    class(subset_df) <- "data.frame"
    coordinates(subset_df) <- ~x + y # Perform the Minimum Convex Polygon calculation
    mcp_result <- mcp(subset_df, percent = 100) # Create a SpatialPoints object
    
    
    max(ext(r_mybird_akde_99),ext(mcp_result))
    min(ext(r_mybird_akde_99),ext(mcp_result))
    
    e_mybird <- c(min(ext(r_mybird_akde_99),ext(mcp_result))[1],
                  max(ext(r_mybird_akde_99),ext(mcp_result))[1],
                  min(ext(r_mybird_akde_99),ext(mcp_result))[2],
                  max(ext(r_mybird_akde_99),ext(mcp_result))[2])
    
    plot(e_mybird[1:2],e_mybird[3:4],type="n")
    terra::plot(mcp_result,add=T) ; terra::plot(telemetry_list[[bg]][[1]],add=T) # plot results to check
    terra::plot(r_mybird_akde_99,add=T, border="blue")
    
    # readline("Look at the plot. If it is ok enter whatever you want to next.")
    
    ### Crop the environment stack around the bird of interest
    #' cropping the stack environment to the extent of the bird data locations *2
    env_RL_list_cropped <- lapply(env_raster_list, function(raster) {
      terra::crop(raster, extent(e_mybird)*2)
    })
    
    
    
    ### RSF function 
    #' The integrator = "Riemann" option is much faster
    set.seed(3)
    mybird_rsf_mc_strava <- rsf.fit(telemetry_list[[bg]][[1]], 
                                    akde_list[[bg]][[1]],  
                                    R = env_RL_list_cropped,
                                    integrator = "MonteCarlo",   #Riemann = faster option but only for spatial variables (rasters); MonteCarlo = for spatial and temporal variables
                                    formula = ~ elevation + squared_elevation + strava + 
                                      # leks +
                                      Shrubs +
                                      Trees +
                                      Cliffs_water +
                                      Buildings)
    
    sum_rsf <- summary(mybird_rsf_mc_strava)
    
    sum_rsf_multipl[[bg]] <- sum_rsf
    
  }
  names(sum_rsf_multipl) <- names(telemetry_list)
  
  if(write == TRUE)
  {
    save(sum_rsf_multipl, file = file.path(outputfolder, "RSF_results", paste0(deparse(substitute(telemetry_list)),".Rdata")))
  }
  
  
  
  end_time <- proc.time()
  
  # Calculate elapsed time
  execution_time <- end_time - start_time
  print(execution_time)
  
  
  
  return(sum_rsf_multipl)
}

system.time(
RSF_results_multpl_birds <- RSF_birds(  l_telemetry_winter[1:25], 
                                        l_akde_winter[1:25],
                                        scaled_env_RL_list_selection,
                                        outputfolder = file.path(base, "Tetralps", "3_R", "0_Heavy_saved_models", "birds_3V"),
                                        write = TRUE
                                        ) 
)

load(file = file.path(base, "Tetralps", "3_R", "0_Heavy_saved_models", "birds_3V", "RSF_results", paste0("l_telemetry_winter.Rdata")))
#********************************************************************



### 3.5_Vizualising RSF results for multiple birds ----
#********************************************************************
rsf_results_table <- data.frame()

for(bg in seq_along(RSF_results_multpl_birds))
{
  rsf_results_dt <- as.data.frame(RSF_results_multpl_birds[[bg]]$CI) 
  rsf_results_dt$covariates <- row.names(rsf_results_dt) 
  rsf_results_dt$bird <- names(RSF_results_multpl_birds)[[bg]] 
  rsf_results_dt <- rsf_results_dt %>%
    mutate(
      covariates = sapply(strsplit(covariates, " "), `[[`, 1), # Split the string at spaces
      covariates = case_when(
        covariates == "area" ~ "area (km^2)",
        covariates == "τ[position]" ~ "τ[position] (days)",
        covariates == "diffusion" ~ "diffusion (ha/day)",
        TRUE ~ covariates # This ensures that any values in covariates that do not match the specified conditions remain unchanged.
      )
    )
  # bind the summary of the different birds
  rsf_results_table <- rbind(rsf_results_table, rsf_results_dt)
}


ggplot(data = rsf_results_table %>% filter(covariates %in% c("strava", "leks", "elevation")), aes(y = covariates, x = est))+
  geom_boxplot(aes(color = covariates), fill = alpha("grey", 0.2))+
  geom_jitter(color = alpha("black",0.6))+
  theme(axis.text.x = element_text(hjust = 1),  panel.background = element_blank()) +
  labs(y = "Covariates", x = "Estimates", title = "Estimated coeficients by covariate")

#********************************************************************



