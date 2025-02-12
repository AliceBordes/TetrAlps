## PhD Tetralpes project ##

# Alice Bordes #

# September 2024 #

# Description:

# RSF on multiple indiv



#### Loading libraries ----
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
library(parallel)
library(meta)
library(doParallel)
library(rempsyc)
library(forcats) 
detectCores()
#********************************************************************


### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD"
#********************************************************************

# Loading data ----
#********************************************************************
### DATASET
birds_bg_dt <- read.csv2(file.path(base,"2_DATA","data_bg_pretelemetry_2024_10.csv"),sep=",") #upload the file from a csv, not a move2 object

### ENV
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/scaled_env_RL_list.RData"))

# Visitor numbers
visitor_meribel <- read.csv2(file.path(base,"2_DATA","ski_resorts_visitor_numbers","meribel_visitors.csv"), sep=",")
visitor_meribel$Date <- as.Date(visitor_meribel$Date)

visitor_valtho <- read.csv2(file.path(base,"2_DATA","ski_resorts_visitor_numbers","valtho_visitors.csv"), sep=",")
visitor_valtho$Date <- as.Date(visitor_valtho$Date)

visitor_courch <- read.csv2(file.path(base,"2_DATA","ski_resorts_visitor_numbers","courchevel_visitors.csv"), sep=",")
visitor_courch$Date <- as.Date(visitor_courch$Date)

visitor_menui <- read.csv2(file.path(base,"2_DATA","ski_resorts_visitor_numbers","menuires_visitors.csv"), sep=",")
visitor_menui$Date <- as.Date(visitor_menui$Date)
visitor_menui$Total <- as.integer(visitor_menui$Total)

# Snow deph
snow_meribel <- read.csv2(file.path(base,"2_DATA","snow_depth","meribel_snow_depth.csv"), sep=",")
snow_meribel$Date <- as.Date(snow_meribel$Date)
snow_meribel <- snow_meribel %>% group_by(Date) %>% summarise(snow.depth = mean(cumul.H.neige.cm))
snow_meribel <- as.data.frame(snow_meribel)

snow_courch <- read.csv2(file.path(base,"2_DATA","snow_depth","courchevel_snow_depth.csv"), sep=",")
snow_courch$Date <- as.Date(snow_courch$Date)
snow_courch <- snow_courch %>% group_by(Date) %>% summarise(snow.depth = mean(cumul.H.neige.cm))
snow_courch <- as.data.frame(snow_courch)
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
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/Formatting_data/formatting_environment_data.R")
#********************************************************************



### 1.1_Add visitor numbers and snow depth to birds_bg_dt ----
#********************************************************************
birds_bg_dt <- assigning_visitors_depth(birds_bg_dt)
#********************************************************************

### 1.2_Creation of a variable fact.visitor.nb and a variable for ski lift opening hours ----
#********************************************************************
birds_bg_dt <- add_variables_visit_open(birds_bg_dt)
#********************************************************************





#********************************************************************
# Load the outputs of tele_akde with visitor number as continuous variable
load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", "multipl_telemetry_winter_saison2_2025_01_23.Rdata"))
load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", "multipl_guess_winter_saison2_2025_01_23.Rdata"))
load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", "multipl_fit_winter_saison2_2025_01_23.Rdata"))
load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", "multipl_akde_winter_saison2_2025_01_23.Rdata"))

l_telemetry_winter <- list_of_one(l_telemetry_winter)
l_akde_winter <- list_of_one(l_akde_winter)
l_guess_winter <- list_of_one(l_guess_winter)
l_fit_winter <- list_of_one(l_fit_winter)

l_telemetry_winter <- covariates_NAN_cleaned(l_telemetry_winter)

l_akde_winter <- l_akde_winter[names(l_akde_winter) %in% names(l_telemetry_winter)]
l_guess_winter <- l_guess_winter[names(l_guess_winter) %in% names(l_telemetry_winter)]
l_fit_winter <- l_fit_winter[names(l_fit_winter) %in% names(l_telemetry_winter)]
#********************************************************************  


#### 3_Home range estimation : Fitting an RSF for 1 bird ----
#********************************************************************

### Loading packages for RSF ----
#******************************************************************** 
# library(animove)
library(ctmm)
library(sf)
library(mvtnorm)
library(terra)
#******************************************************************** 


### 3.4_RSF ----
#********************************************************************

# Selection of the rasters to use in the RSF
scaled_env_RL_list_selection <-  scaled_env_RL_list[!(names(scaled_env_RL_list) %in% c("slope", "leks"))]
# scaled_env_RL_list_selection <-  scaled_env_RL_list[c("elevation", "squared_elevation", "strava","leks")]

# Define the formula
model_formula <- ~ elevation + 
  squared_elevation + 
  strava_winter_sports +
  strava_winter_sports:sl_open +
  strava_winter_sports:sl_open:total.visitors.std +
  strava_winter_sports:total.visitors.std +
  Shrubs +
  Cliffs +
  Buildings +
  Trees 

# Define the formula
model_formula <- ~ elevation + 
  squared_elevation + 
  strava_winter_sports +
  Shrubs +
  Cliffs +
  Trees 

# physico-climatic related variables
# ~ elevation +
# squared_elevation +

# human perturbations related variables
# strava +
# strava:sl_open +
# strava:visitor_breaksLow:sl_open +
# strava:visitor_breaksHigh:sl_open +
# strava:visitor_breaksVery_high:sl_open +
# strava:total.visitors.std +

# habitat-related variables and human perturbations related variables (Buildings)
# Shrubs +
# Cliffs +
# Buildings +
# Trees
# Deciduous_trees +
# Resinous_trees +
# Mixed_trees +
# Shrubs:sl_open +
# Cliffs:sl_open +
# Buildings:sl_open +
# Trees:sl_open
# Cliffs:total.visitors.std

system.time(
  RSF_results_multpl_birds <- RSF_birds(  #telemetry_list = l_telemetry_winter[!names(l_telemetry_winter)%in%covid_all], 
    #akde_list = l_akde_winter[!names(l_akde_winter)%in%covid_all],
    telemetry_list = l_telemetry_winter, 
    akde_list = l_akde_winter,
    env_raster_list = scaled_env_RL_list_selection,
    rsf_formula = model_formula,
    rsf_integrator = "Riemann", # "MonteCarlo", 
    # grid = "full",
    outputfolder = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results"),
    write = TRUE
  ) 
)
#********************************************************************















