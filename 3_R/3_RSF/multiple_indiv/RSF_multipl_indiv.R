## PhD Tetralpes project ##

# Alice Bordes #

# September 2024 #

# Description:

# RSF on multiple indiv



#### Loading libraries ----
#********************************************************************
# library(move2)
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
base <- "C:/Users/albordes/Documents/PhD/Tetralps"
telemetry_file <- "multipl_telemetry_winter_saison2_2025_02_21"
akde_file <- "multipl_akde_winter_saison2_2025_02_21"
covid <- c("Caramel_2", "Daisy","Dalton","Dameur","Dario","Darkvador","Darwin","Dede","Destroy","Diot","Djal","Django","Donald","Durite","Dynamite","Dyonisos")
#********************************************************************

# Loading data ----
#********************************************************************
### DATASET
birds_bg_dt <- read.csv2(file.path(base,"2_DATA","data_bg_pretelemetry_2024_10.csv"),sep=",") #upload the file from a csv, not a move2 object

### VECTORS

# 3V borders 
borders_3V_vect <- st_read(file.path(base,"1_RAW_DATA","3V","borders_3V.gpkg"))
borders_3V_vect <- terra::vect(file.path(base,"1_RAW_DATA","3V","borders_3V.gpkg"))

# Environment stack
# load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","env_RL_list_10m.RData"))
load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","scaled_env_RL_list_10m.RData"))
load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","scaled_env_RL_list_1m.RData"))
load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","scaled_env_RL_list_modal.RData"))

strava_without_aerial_corr <- terra::rast(file.path(base,"2_DATA/environmental_raster/r_strava_without_aerial_cables.gpkg"))
strava_without_aerial_corr <- scale(strava_without_aerial_corr)
scaled_env_RL_list[["strava_winter_sports"]] <- strava_without_aerial_corr

# scaled_env_RL_list_new <- scaled_env_RL_list
# load(file.path(base,"TetrAlps_old/3_R/0_Heavy_saved_models/environment_3V/scaled_env_RL_list.RData"))
# scaled_env_RL_list_old <- scaled_env_RL_list

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

# ski resort identification
ski_lift_traffic_3V <- st_read(file.path(base,"2_DATA","ski_resorts_visitor_numbers","osm", "ski_lift_traffic_3V.gpkg"))
dt_resorts <- read.csv2(file.path(base,"2_Data","ski_resorts_visitor_numbers","bg_winter_assign_valley_resort.csv"))
#********************************************************************



### Loading functions ----
#********************************************************************
source(file.path("4_FUNCTIONS","my_telemetry_transfo_data.R"))
source(file.path("4_FUNCTIONS","Homerange_visu/mean_size_area.R"))
source(file.path("4_FUNCTIONS","Homerange_visu/visu_home_range.R"))
source(file.path("4_FUNCTIONS","Homerange_visu","distance_home_range_capture_site.R"))
source(file.path("4_FUNCTIONS","Homerange_visu","multi_graph_home_range.R"))
source(file.path("4_FUNCTIONS","RSF","plot_check_RSF_results.R"))
source(file.path("4_FUNCTIONS","RSF","rsf_functions.R"))
source(file.path("4_FUNCTIONS","Formatting_data/formatting_environment_data.R"))
#********************************************************************



### 1.1_Add visitor numbers and snow depth to birds_bg_dt ----
#********************************************************************
birds_bg_dt <- assigning_visitors_depth(birds_bg_dt)
#********************************************************************

### 1.2_Creation of a variable fact.visitor.nb and a variable for ski lift opening hours ----
#********************************************************************
birds_bg_dt <- add_variables_visit_open(birds_bg_dt)

birds_bg_dt <- birds_bg_dt %>%
  mutate(day_category = case_when(
    hms(time) > hms("04:30:00") & hms(time) < hms("09:30:00") ~ "displaying",
    hms(time) > hms("09:30:00") & hms(time) < hms("19:00:00") ~ "day",
    TRUE ~ "night"  # Default to "night" for all other times
  )) %>%
  mutate(value = 1) %>%  # Add a column for pivoting
  pivot_wider(names_from = day_category, values_from = value, values_fill = list(value = 0))
#********************************************************************

  
# ### 2_Data telemetry, guess, fit and akde objects for rsf ----
# #********************************************************************

### 2.1_Data creation of telemetry, guess, fit and akde objects for rsf ----
#********************************************************************
# birds_bg_dt_nodisplay <- birds_bg_dt[birds_bg_dt$day != "displaying", ]

tele_akde(data = birds_bg_dt,
          # birds_vect = c("Alpha", "Caramel", "Dalton","Dario","Donald","Dynamite","Dyonisos","Ecolo","Eros","Fast","Ficelle","Flambeur","Fleau","Foliedouce"),
          season = "hiver",
          subset_category = "saison2",
          outputfolder = file.path(base, "3_R", "0_Heavy_saved_models", "birds_3V"),
          write = TRUE)

# tele_akde(data = bird_winter_outcovid,
#           season = "hiver",
#           subset_category = "saison",
#           outputfolder = file.path(base, "3_R", "0_Heavy_saved_models", "birds_3V"),
#           write = TRUE)
#********************************************************************


### 2.2_Data Loading of telemetry, guess, fit and akde objects for rsf ----
#********************************************************************
# to run with telemetry, guess, fit, akde
  # load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", "raw_lists", "multipl_akde_winter_hiver.Rdata"))
  # l_akde_winter_singlew <- l_akde_winter
  # load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", "raw_lists", "multipl_akde_winter_saison2.Rdata"))
  # l_akde_winter <- c(l_akde_winter_singlew[!(names(l_akde_winter_singlew) %in% names(l_akde_winter))], list_of_one(l_akde_winter))
  # # Identify elements with empty names
  # unnamed_indices <- which(names(l_akde_winter) == "")
  # # Remove these unnamed elements
  # if (length(unnamed_indices) > 0) {
  #   l_akde_winter <- l_akde_winter[-unnamed_indices]
  # }
  # save(l_akde_winter, file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", "multipl_akde_winter.Rdata"))

  
# Load the outputs of tele_akde with visitor number as continuous variable
load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", paste0(telemetry_file,".Rdata")))
load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", paste0(telemetry_file,".Rdata")))

l_telemetry_winter <- list_of_one(l_telemetry_winter)
l_akde_winter <- list_of_one(l_akde_winter)

  l_telemetry_winter <- covariates_NAN_cleaned(l_telemetry_winter)
  
  l_akde_winter <- l_akde_winter[names(l_akde_winter) %in% names(l_telemetry_winter)]
#********************************************************************  

### 2.3_Identifying bird monitored during covid ----
#********************************************************************
  # # Combine into a single data frame with animal names added
  # combined_telemetry <- do.call(rbind, 
  #                               lapply(names(l_telemetry_winter), function(bird_name) {
  #                                 # For each bird, add the animal ID as a new column
  #                                 bird_data <- do.call(rbind, l_telemetry_winter[[bird_name]])
  #                                 bird_data$animal.ID <- bird_name  # Add the animal name as a new column
  #                                 return(bird_data)
  #                               })
  # )
  # combined_telemetry$Jour <- as.Date(as.POSIXct(combined_telemetry$timestamp))
  # 
  # # official lockdown Tuesday 17 March 2020 at 12:00 - Monday 3 May 2021 
  # # On 4 December 2020, the Prime Minister banned public access to ski lifts in ski resorts, with the exception of professionals and children who are members of an association affiliated to the French Ski Federation.
  # 
  # covid <- combined_telemetry %>% filter(saison=="hiver") %>% filter(as.Date(Jour) < as.Date("2021-06-01")) %>% filter(as.Date(Jour) > as.Date("2020-10-01"))
  # covid <- unique(covid$animal.ID)
  # 
  # deb_covid <- combined_telemetry %>% filter(saison=="hiver") %>% filter(as.Date(Jour) < as.Date("2020-06-01")) %>% filter(as.Date(Jour) > as.Date("2020-03-16"))
  # deb_covid <- unique(deb_covid$animal.ID)
  # 
  # covid_all <- c(covid) #c(covid,"Calu", "Cesar", "Caramel" )
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

#### 3.1_Required assumptions to calculate and interprete an home range ----
#********************************************************************
# The animal must show a resident behevior on the considered scale : 
# "Finally, at larger scales, most animals will exhibit a tendency to remain in a defined region or ‘home range’". (Calabrese, 2016)
# The variogram, or semivariogram, plots time lags on the x-axis for all pairs of observations
# against their semi-variance (half of the variance for the distance each observation pair) on the y-axis. (Animove 2022)

# Bird's VARIOGRAM
bird_variogram(l_telemetry_winter, 
               l_fit_winter,
               # dt_hour = c(1, 6, 12), 
               outputfolder = file.path(base, "5_OUTPUTS","RSF","variograms"), 
               write = TRUE)
  
  
  
bird_sampling_sche(telemetry_list = l_telemetry_winter, 
                   outputfolder = file.path(base, "5_OUTPUTS","RSF","sampling"), 
                   write = TRUE)
#********************************************************************
  


### 3.2_Visualization "raw" HR (without environmental effects) ----
#********************************************************************
bird = "Fanfoue"
  
UD_mybird_spatial <- SpatialPolygonsDataFrame.UD(l_akde_winter[[bird]][["hiver1"]],level.UD=.95,level=.95)
UD_mybird_spatial2 <- SpatialPolygonsDataFrame.UD(l_akde_winter[[bird]][["hiver2"]],level.UD=.95,level=.95)

# Create a data frame to map the "hiver1" and "hiver2" names
legend_data <- rbind(
  st_as_sf(UD_mybird_spatial) %>% mutate(winter = "hiver1"),
  st_as_sf(UD_mybird_spatial2) %>% mutate(winter = "hiver2")
)


# Plot with the updated legend
ggplot() +
  theme_void() + theme(plot.title = element_text(size = 16), legend.text = element_text(size=12), legend.title = element_text(size=14)) +
  ggtitle("Fast winter home ranges based on winter GPS locations in the 3 Vallées region (Northern Alps)") +

  # Plot the vector borders with valley names
  geom_spatvector(data = borders_3V_vect, fill = NA, aes(color = NOM)) +
  scale_color_manual(values = c("Courchevel" = "darkgreen", "Les Allues" = "lightgreen", "Les Belleville" = "darkgrey")) +
  labs(color = "Valley") +

  # Start a new scale for the winter layers
  new_scale_color() +
  geom_sf(data = legend_data, aes(fill = winter), color = NA, alpha = 0.1) +

  # Define the colors and labels for the winter legend
  scale_fill_manual(name = "Winters",
                    values = c("hiver1" = "blue", "hiver2" = "turquoise"),
                    labels = c("hiver1" = "First winter", "hiver2" = "Second winter")) +

  # Add borders for the winter regions
  geom_sf(data = st_as_sf(UD_mybird_spatial), color = "blue", fill = NA) +
  geom_sf(data = st_as_sf(UD_mybird_spatial2), color = "turquoise", fill = NA)
#********************************************************************


### 3.3_Expected results of the RSF ----
#********************************************************************
#birds 1,2,6,8 have an home range for "hiver1"
# source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/plot_check_RSF_results.R")
# plot_check_RSF_res(telemetry_winter,akde_winter,"habitats",analysis_object="study_area",writeplot=TRUE)
# plot_check_RSF_res(telemetry_winter,akde_winter,"strava",analysis_object="study_area",data_visu="continuous",writeplot=TRUE)
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


# Define the formula
model_formula <- ~ elevation + 
  squared_elevation + 
  strava_winter_sports +
  strava_winter_sports:sl_open +
  strava_winter_sports:sl_open:total.visitors.std +
  strava_winter_sports:total.visitors.std +
  Shrubs +
  Cliffs +
  Trees 

model_formula <- ~elevation + 
  squared_elevation + 
  strava_winter_sports +
  strava_winter_sports:day + 
  strava_winter_sports:displaying + 
  Trees +
  Trees:day + 
  Trees:displaying + 
  Shrubs +
  Shrubs:day + 
  Shrubs:displaying + 
  Cliffs +
  Cliffs:day + 
  Cliffs:displaying 

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
RSF_results_multpl_birds <- RSF_birds(  # telemetry_list = l_telemetry_winter[!names(l_telemetry_winter)%in%covid], 
                                        # akde_list = l_akde_winter[!names(l_akde_winter)%in%covid],
                                        telemetry_list = l_telemetry_winter, 
                                        akde_list = l_akde_winter,
                                        clusters = 1,
                                        env_raster_list = scaled_env_RL_list_selection,
                                        rsf_formula = model_formula,
                                        rsf_integrator = "MonteCarlo", #"Riemann", 
                                        # grid = "full",
                                        outputfolder = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results"),
                                        write = TRUE
                                        ) 
)



# # to link the different samples : 
#     load(file = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", paste0("rsf[31_59]_individual_2025_01_15.Rdata")))
#     sum_rsf_multipl3 <- sum_rsf_multipl
#     sum_rsf_multipl <- c(sum_rsf_multipl1, sum_rsf_multipl2, sum_rsf_multipl3)
#     # Identify elements with empty names
#     unnamed_indices <- which(names(sum_rsf_multipl) == "")
#     # Remove these unnamed elements
#     if (length(unnamed_indices) > 0) {
#       l_akde_winter <- l_akde_winter[-unnamed_indices]
#     }
#     save(sum_rsf_multipl, file = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", paste0("rsf_59birds_individual_2025_01_15.Rdata")))




# # debugging RSF_birds function
# # Check for valid values in each raster
# lapply(scaled_env_RL_list_selection, function(raster) {
#   print(names(raster))
#   hasValues(raster)
# })

#combine() to combine 2 nested lists

# note : 
# in summary(sum_rsf_multipl[[1]]) --> τ[position] (hours) = time to reach 63% of the variogram asympote (= autocorr between positions)
#********************************************************************



### 3.5_Identifying birds monitored during covid ----
#********************************************************************
# Combine into a single data frame with animal names added
combined_telemetry <- do.call(rbind, 
                              lapply(names(l_telemetry_winter), function(bird_name) {
                                # For each bird, add the animal ID as a new column
                                bird_data <- do.call(rbind, l_telemetry_winter[[bird_name]])
                                bird_data$animal.ID <- bird_name  # Add the animal name as a new column
                                return(bird_data)
                              })
)
combined_telemetry$Jour <- as.Date(as.POSIXct(combined_telemetry$timestamp))

# official lockdown Tuesday 17 March 2020 at 12:00 - Monday 3 May 2021 
# On 4 December 2020, the Prime Minister banned public access to ski lifts in ski resorts, with the exception of professionals and children who are members of an association affiliated to the French Ski Federation.

covid <- combined_telemetry %>% filter(saison=="hiver") %>% filter(as.Date(Jour) < as.Date("2021-06-01")) %>% filter(as.Date(Jour) > as.Date("2020-10-01"))
covid <- unique(covid$animal.ID)

deb_covid <- combined_telemetry %>% filter(saison=="hiver") %>% filter(as.Date(Jour) < as.Date("2020-06-01")) %>% filter(as.Date(Jour) > as.Date("2020-03-16"))
deb_covid <- unique(deb_covid$animal.ID)

covid_all <- c(covid) #c(covid,"Calu", "Cesar", "Caramel" )
#********************************************************************



### 3.6_Visualizing visitor number time serie
#********************************************************************
birds_bg_dt_J <- birds_bg_dt %>%
  filter(saison == "hiver") %>%
  distinct(jour, resort, .keep_all = TRUE) %>% 
  drop_na(resort) %>% drop_na(total.visitors.std) %>%
  arrange(jour)

birds_bg_dt_J$index <- as.factor(seq_along(birds_bg_dt_J$jour)) 

# Select which labels to display (e.g., every 5th index)
skip_labels <- seq(1, length(birds_bg_dt_J$index), by = 30)  # Skip every 5th label

# Plot using ggplot
ggplot(birds_bg_dt_J, aes(x = index, y = total.visitors.std, color = as.factor(resort), group = resort)) +
  geom_line(size = 1) +  # Line plot for visitors over the index
  facet_wrap(~resort, scales = "free_x", ncol = 1) +  # One graph per resort
  labs(
    title = "Winter total visitors time serie",
    x = "Winter days (Sequential)",
    y = "Total visitors (Standardized)",
    color = "Resort"
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_x_discrete(
    labels = birds_bg_dt_J$jour[skip_labels],  # Map the index to the actual "jour" values
    breaks = birds_bg_dt_J$index[skip_labels]   # Make sure every index value has a corresponding label
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    strip.text = element_text(size = 12, face = "bold"),  # Bold facet titles
    axis.title = element_text(size = 16), 
    title = element_text(size = 18),
    legend.text = element_text(size = 13)
  )



ggplot(birds_bg_dt_J %>% 
         group_by(index, jour) %>% 
         summarise(Tot_resort = sum(total.visitors.std), .groups = "drop") %>%
         distinct(jour, .keep_all = TRUE), 
       aes(x = index, y = Tot_resort, group = 1)) +  # Use Tot_resort instead of total.visitors.std
  geom_line(size = 1) +  # Line plot for visitors over the index
  labs(
    title = "Winter total visitors time series (all resorts)",
    x = "Winter days (Sequential)",
    y = "Total visitors (Standardized)"
  ) +
  scale_x_discrete(
    labels = birds_bg_dt_J$jour[skip_labels],  # Map the index to the actual "jour" values
    breaks = birds_bg_dt_J$index[skip_labels]   # Make sure every index value has a corresponding label
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    strip.text = element_text(size = 12, face = "bold"),  # Bold facet titles
    axis.title = element_text(size = 16), 
    title = element_text(size = 18),
    legend.text = element_text(size = 13)
  )



boxplot(birds_bg_dt_J$total.visitors.std, ylab = "Total visitors (std)", xlab = "All 3 Vallées resorts", main = "Visitor number variable")
text(y = round(fivenum(birds_bg_dt_J$total.visitors.std),2)[c(1,3,5)], labels = round(fivenum(birds_bg_dt_J$total.visitors.std),2)[c(1,3,5)], x=0.5)
#********************************************************************






### 3.6_Suitability map and range distribution ---- # NOT WORKING
#********************************************************************
#' A suitability map
#' 
#' 
#' 
# r_mybird_akde_99 <- SpatialPolygonsDataFrame.UD(l_akde_winter[[1]][[1]], level.UD = 0.99, level = 0.95)
# # Calculate MCP
# subset_df <- l_telemetry_winter[[1]][[1]][, c("x", "y")]
# class(subset_df) <- "data.frame"
# coordinates(subset_df) <- ~x + y
# mcp_result <- mcp(subset_df, percent = 100)
# 
# # Extent calculation
# e_mybird <- c(
#   min(ext(r_mybird_akde_99), ext(mcp_result))[1],
#   max(ext(r_mybird_akde_99), ext(mcp_result))[1],
#   min(ext(r_mybird_akde_99), ext(mcp_result))[2],
#   max(ext(r_mybird_akde_99), ext(mcp_result))[2]
# )
# extent(e_mybird)*2

# extent_values <- extent(l_telemetry_winter_meta[[1]])
# grid_extent <- extent(c(extent_values["min","x"], extent_values["max","x"], extent_values["min","y"], extent_values["max","y"]))*2


#' 
# NOT WORKING 
suitability_bird <- ctmm::suitability(sum_rsf_multipl[[1]],
                                      R = scaled_env_RL_list_selection,
                                      grid = crop(scaled_env_RL_list_selection[[1]], grid_extent* 2))

suitability_bird <- ctmm::suitability(sum_rsf_multipl[[1]],
                                      R = scaled_env_RL_list_selection,
                                      grid = crop(scaled_env_RL_list_selection[[1]], extent(e_mybird)*2))
 
#' raster::plot(suitability_bird)
#' 
#' #' Range distribution (includes the ranging behaviour)
#' agde_bird <- agde(CTMM = sum_rsf_multipl[[1]], R = scaled_env_RL_list_selection) #, grid = crop(scaled_env_RL_list_selection[[1]], grid_extent* 2))
#' terra::plot(agde_bird)
#' 
#' # Plot raster of range distribution
#' agde_raster_bird <- ctmm::raster(agde_bird, DF = "PMF")
#' plot(agde_raster_bird)
#********************************************************************



### 3.7_Predictions ----
#********************************************************************
animal = 2

summary(sum_rsf_multipl[[animal]])

prediction <- simulate( sum_rsf_multipl[[animal]],
                        l_telemetry_winter_meta[[animal]])
plot(prediction)
plot(l_telemetry_winter_meta[[animal]], col = "blue", add = TRUE)

ggplot()+
  geom_spatraster(data = terra::rast(scaled_env_RL_list_selection[["strava"]]))+
  scale_fill_gradientn(name = "Strava intensity",colors=c("#CCCCCC11","#FF6600","#FF3333"))+
  xlim(c(min(l_telemetry_winter_meta[[animal]][["x"]]-1000),
         max(l_telemetry_winter_meta[[animal]][["x"]]+1000)))+
  ylim(c(min(l_telemetry_winter_meta[[animal]][["y"]]-1000),
         max(l_telemetry_winter_meta[[animal]][["y"]]+1000)))+
  geom_point(aes(x = prediction[["x"]],
                 y = prediction[["y"]],
                 color = "Prediction"),
             size = 3)+
  geom_point(aes(x = l_telemetry_winter_meta[[animal]][["x"]],
                 y = l_telemetry_winter_meta[[animal]][["y"]],
                 color = "Observation"),
             size = 1)+
  scale_color_manual(name = paste0("Bird: ", names(l_telemetry_winter_meta)[animal]),
                     values = c("turquoise", "black"),
                     labels = c("turquoise" = "Prediction", "black" = "Observation"))+
  labs(y = "Latitude", 
       x = "Longitude", 
       title = paste0("Telemetry observations VS Predictions based on the model RSF"))+
  theme(axis.text.x = element_text(hjust = 1),  
        panel.background = element_blank(),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12))
#********************************************************************



### 4_Metamodel ----
#********************************************************************
rsf_beta <- sapply(sum_rsf_multipl, "[[", "beta")
rsf_var <- sapply(sum_rsf_multipl, function(x) diag(x[["COV"]])[1:nrow(rsf_beta)])

is_outlier <- function(x) {
  x %in% boxplot.stats(x)$out
}

write.csv(cbind(as.data.frame(t(rsf_beta)), 
                "any_outlier" = apply(apply(rsf_beta, 1, is_outlier), 1, any),
                "strava_visit_outlier" = is_outlier(rsf_beta["strava:total.visitors.std",])),
          file.path(base, "5_OUTPUTS", "RSF", "meta_model", "individual_parameters_metamodel=59birds_individual_2025_01_13.csv"))

# Fixed effect and random effects meta-analysis based on estimates (e.g. log hazard ratios) and their standard errors. The inverse variance method is used for pooling.
meta_models <- lapply(1:nrow(rsf_beta), function(x) {
  ok <- !is_outlier(rsf_beta[x, ]) # remove outliers 
  meta_df <- metagen(TE = rsf_beta[x, ok],  # beta estimates
                     seTE = sqrt(rsf_var[x, ok]), # var estimates
                     studlab = names(sum_rsf_multipl)[ok]) # labels
  metareg(meta_df, ~1)
  # metareg(meta_df, ~period) ->
})

meta_model_coef <- data.frame(t(sapply(meta_models, 
                                       function(x) c("coef" = x$beta[1], 
                                                     "se" = x$se, 
                                                     "pval" = x$pval,
                                                     "ci.lb" = x$ci.lb,
                                                     "ci.ub" = x$ci.ub,
                                                     "tau" = sqrt(x$tau2)))))
rownames(meta_model_coef) <- names(sum_rsf_multipl[[1]]$beta)
write.csv(meta_model_coef, file.path(base, "5_OUTPUTS", "RSF", "meta_model","metamodel=59birds_individual_2025_01_13.csv"))



rsf_results_table2 <- data.frame("covariates" = rownames(meta_model_coef), 
                                 "est" = meta_model_coef$coef,
                                 "low" = meta_model_coef$ci.lb, # lower bound of the conf interval = meta_model_coef$coef - 1.96*meta_model_coef$se
                                 "high" = meta_model_coef$ci.ub, 
                                 "bird" = "Meta", 
                                 "period" = NA)

rsf_results_table <- rbind(rsf_results_table, rsf_results_table2[, -which(colnames(rsf_results_table2) == "period")])



# Filter the data for boxplot and Meta
boxplot_data <- rsf_results_table %>% filter(!is.na(period))
meta_data <- rsf_results_table %>% filter(bird == "Meta")


# Supress Fiasco estimation for Buildings (outlier) 
# rsf_results_table[rsf_results_table$covariates == "Buildings" & rsf_results_table$bird == "Fiasco", "est"] <- NA
rsf_results_table[rsf_results_table$covariates == "Buildings" & rsf_results_table$bird == "Dynamite_2", "est"] <- NA
rsf_results_table[rsf_results_table$covariates == "Cliffs" & rsf_results_table$bird == "Dameur", "est"] <- NA



# Plot1

ggplot(data = rsf_results_table %>% filter(covariates %in% c("strava","strava:total.visitors.std" , "Cliffs", "Trees", "Shrubs", "Buildings")), aes(y = covariates, x = est))+
  # ggplot(data = rsf_results_table %>% filter(covariates %in% c("leks")), aes(y = covariates, x = est))+
  geom_boxplot(aes(color = covariates), fill = alpha("grey", 0.2), notch = TRUE)+
  new_scale_color()+
  geom_jitter(color = alpha("black",0.6))+
  scale_color_manual(
    name = "Bird groups", # Legend title
    values = c(
      "Monitored during covid" = "red", # Group "covid_all" in black
      "Others" = "black"        # Others in red
    ),
    labels = c("covid_all" = "Covid All Birds", "Other" = "Other Birds") # Custom labels
  ) +
  # geom_text(
  #   aes(
  #     label = bird,             # Add bird names as labels
  #     color = ifelse(bird %in% covid_all, "covid_all", "Other")
  #   ),
  #   hjust = -0.2,               # Horizontal adjustment to move labels slightly
  #   vjust = 0.5,                # Vertical adjustment to align with points
  #   size = 4                    # Text size
  # ) +
  # Add blue segments for Meta estimates
  geom_segment(data = meta_data %>% filter(covariates %in% c("strava", "strava:total.visitors.std" , "Cliffs", "Trees", "Shrubs", "Buildings")), 
               aes(x = est, xend = est, y = as.numeric(as.factor(covariates)) - 0.4, yend = as.numeric(as.factor(covariates)) + 0.4, color = "Meta estimates"),
               size = 1.2) +
  # Manual color adjustments
  scale_color_manual(values = c("Meta estimates" = "blue")) +
  # Labels and theme adjustments
  labs(y = "Covariates", 
       x = "Estimates", 
       title = paste0("Results of the RSF performed on ", length(unique(rsf_results_table$bird))-1, " birds \n(estimated coefficients by covariate)"),
       color = NULL) +  # Remove "color" title from legend
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(hjust = 1),  
        panel.background = element_blank(),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))





# Plot2
ggplot(data = boxplot_data %>% filter(covariates %in% c("strava", "strava:total.visitors.std", "Cliffs", "Trees", "Shrubs", "Buildings")), 
       aes(y = covariates, x = est)) +
  # Boxplot colored by covariates, grouped by period
  geom_boxplot(aes(color = covariates, fill = factor(period)), 
               alpha = 0.2, 
               position = position_dodge(width = 0.8),
               notch = TRUE) +
  new_scale_color()+
  # Add blue segments for Meta estimates
  geom_segment(data = meta_data %>% filter(covariates %in% c("strava", "strava:total.visitors.std", "Cliffs", "Trees", "Shrubs", "Buildings")), 
               aes(x = est, xend = est, y = as.numeric(as.factor(covariates)) - 0.4, yend = as.numeric(as.factor(covariates)) + 0.4, color = "Meta estimates"),
               size = 1.2) +
  # Manual fill and color adjustments
  scale_fill_manual(name = "Period", values = c("red", "grey")) +
  scale_color_manual(values = c("Meta estimates" = "blue")) +
  # Labels and theme adjustments
  labs(y = "Covariates", 
       x = "Estimates", 
       title = paste0("Results of the RSF performed on ", length(unique(rsf_results_table$bird))-1, " birds \n(estimated coefficients by covariate)"),
       color = NULL) +  # Remove "color" title from legend
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(hjust = 1),  
        panel.background = element_blank(),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))


### Statistical test 

meta_model_coef <- cbind(rownames(meta_model_coef),meta_model_coef)
names(meta_model_coef) <- c("rownames(meta_model_coef)" = "covariates", "coef" = "est", "se" = "se", "pval" = "p", "ci.lb" = "low", "ci.ub" = "high", "tau" = "tau")
meta_model_table <- nice_table(meta_model_coef %>% dplyr::select(-"tau", -"se"),
                               note = c(
                                 "* p < .05, ** p < .01, *** p < .001"
                               ))
flextable::save_as_docx(meta_model_table, path = file.path(base, "5_OUTPUTS", "RSF", "meta_model","formatted_results_text" , "metamodel=59birds_individual_2025_01_13_formatted.docx"))




#' ## High suitability thresholds
#' 
#' NOT WORKING 
# home_ranges <- lapply(l_akde_rsf_winter_meta, SpatialPolygonsDataFrame.UD, 
#                       level.UD = 0.95)

sapply_suitability <- function(x, R) {
  res <- future_lapply(x, ctmm::suitability, R = R, grid = R[[1]], 
                       future.seed = TRUE)
  res <- lapply(res, "[[", "est")
  terra::rast(lapply(res, terra::rast))
}

suit_ref <- sapply_suitability(sum_rsf_multipl, scaled_env_RL_list_selection)


median_in_mask <- function(x, y) {
  median(terra::values(terra::mask(x, terra::vect(y)[1,])), na.rm = TRUE)
}
hisuit_threshold <- mapply(median_in_mask, as.list(suit_ref), home_ranges)
#********************************************************************


### 4.1_Diagnostic graphs ----
#********************************************************************
metapara <- read.csv2(file.path(base, "5_OUTPUTS","RSF" , "meta_model", "individual_model_parameters.csv"),
                      sep = ",")

# Reshape the data into long format
reshaped_metapara <- metapara %>% dplyr::select(-any_outlier,-strava_visit_outlier) %>%
  pivot_longer(
    cols = -X,                # Columns to reshape (exclude "X")
    names_to = "Covariate",   # New column for covariate names
    values_to = "Value"       # New column for values
  ) %>%
  arrange(X, Covariate)       # Optional: Sort the data

birds_bg_dt2 <- birds_bg_dt
birds_bg_dt2$ski_season <- paste0(as.numeric(year(birds_bg_dt2$timestamp))-1,"_",as.numeric(year(birds_bg_dt2$timestamp)))


#********************************************************************


### 5_Checking model ----
#********************************************************************
for (i in seq_along(l_telemetry_winter)) {
  bird <- names(l_telemetry_winter)[i]
  png(file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", "residuals", paste0("residuals_", bird, ".png")),
      res = 300, units = "cm", width = 20, height = 15)
  # par(mfrow = c(2,1))
  layout(matrix(c(1,1,2,3), ncol = 2, byrow = FALSE),
         heights=c(1,1))
  
  ### Assess the normality of the residuals 
  
  # the normality assumption implies that the model captures the main sources of variation in the data, and that the errors are random and independent.
  
  # Compute residuals
  resid <- residuals(CTMM = sum_rsf_multipl[[bird]], object = l_telemetry_winter[[bird]][["all"]])
  # normality plot
  plot(resid, main = paste0("Residuals scatterplot \nfrom the rsf model run on ", bird))
  
  
  ### Assess the correlation between residuals  (are the residuals randomnessly distributed?)
  
  # The correlogram is a commonly used tool for checking randomness in a data set. If random, autocorrelations should be near zero for any and all time-lag separations. 
  
  # calculate correlogram of residuals
  plot(correlogram(data = resid_1, res = 60),
       main = paste0(bird,"'s correlogram of residuals"))
  # Increase the discretization resolution for irregularly sampled data with res>1
  #E.g., if the sampling interval is set to 15 minutes, but can be off by a minute or two, then res=15 is a good choice.
  
  
  ### Variogram 
  
  SVF <- variogram(l_telemetry_winter[[bird]][["all"]], dt=NULL) # bird's variogram
  plot(SVF, fraction=0.9,level=c(0.5,0.95), CTMM=l_fit_winter[[bird]][["all"]], 
       main = paste0(bird,"'s empirical variogram (90%)\nWinter"), cex.main = 0.8)
  
  
  dev.off()
  
}
#********************************************************************








### 4_Meta_RSF ----
#********************************************************************

### 4.1_Visualization of home ranges ----
#********************************************************************

# AKDE without environment ----
# Unest the akde list
l_akde_winter_meta <- sapply(l_akde_winter, `[[`, 1)

# pal <- ctmm::color(l_akde_winter_meta, by = "individual") # set color to be spatially distinct (by individual)
pal <- scales::hue_pal()(length(l_akde_winter_meta))

# Plot AKDES
plot(l_akde_winter_meta,
     col.DF = pal,
     col.level = pal,
     col.grid = NA,
     level = NA, 
     main = paste0("Home ranges (", length(l_akde_winter_meta), " birds) (3 Vallées)"))
plot(add = TRUE, st_as_sf(borders_3V_vect)$geometry/1000)


# Mean buffalo home range "the old way":
mean_area <- vector("numeric", length = length(l_akde_winter_meta))
for(i in 1:length(l_akde_winter_meta))
{ mean_area[i] <- summary(l_akde_winter_meta[[i]])$CI[2] }
mean_area
mean(mean_area) # classical method 
sqrt(var(mean_area)/length(mean_area)) # Standard Error (SE)

# Meta-analysis of buffalo home-range areas:
## What is the mean home range area of an average individual:
# account for propagation incertainties of indiv in the pop

ctmm::meta(l_akde_winter_meta,
            col = c(pal,"black"),
            verbose = TRUE, # verbose output with CIs
            sort = TRUE) 
## model selection: Dirac-delta > inverse-Gaussian






# AKDE with environment ----

# Unest the akde list
l_telemetry_winter_meta <- lapply(l_telemetry_winter, `[[`, "all")
l_akde_rsf_winter_meta <- akde(l_telemetry_winter_meta, sum_rsf_multipl, trace = 2)

pal <- scales::hue_pal()(length(l_akde_rsf_winter_meta))

# Plot AKDES
plot(l_akde_rsf_winter_meta,
     col.DF = pal,
     col.level = pal,
     col.grid = NA,
     level = NA, 
     main = paste0("Home ranges (", length(l_akde_winter_meta), " birds) (3 Vallées)"))
plot(add = TRUE, st_as_sf(borders_3V_vect)$geometry/1000)


meta_result <- ctmm::meta( sum_rsf_multipl,
                           col = c(pal,"black"),
                           verbose = TRUE, # verbose output with CIs
                           sort = TRUE, 
                           variable="area") 
## model selection: Dirac-delta > inverse-Gaussian

#********************************************************************



suitability_bird <- ctmm::suitability(sum_rsf_multipl[[1]],
                                      R = scaled_env_RL_list_selection,
                                      grid = crop(scaled_env_RL_list_selection[[1]], grid_extent* 2))











### 4.2_Diagnostic graphs2 ----
#********************************************************************

# Initialize a data frame to store results
dt_years <- data.frame(
  animal = character(),
  year_range = character(),
  stringsAsFactors = FALSE
)

# Iterate through each animal in the list
for (animal in names(l_telemetry_winter)) {
  # Get the timestamps for the current animal
  timestamps <- l_telemetry_winter[[animal]][["all"]]$timestamp
  
  # Convert to POSIXct and extract months and years
  timestamps_posix <- as.POSIXct(timestamps)
  years <- year(timestamps_posix)
  months <- month(timestamps_posix)
  
  # Define year ranges based on November-May logic
  year_ranges <- ifelse(
    months %in% 11:12, 
    paste0(years, "_", years + 1),  # November-December belongs to the current year and next
    ifelse(
      months %in% 1:5, 
      paste0(years - 1, "-", years),  # January-May belongs to the previous year and current
      NA  # Other months are excluded
    )
  )
  
  # Store the results
  dt_years <- rbind(dt_years, data.frame("animal" = animal, "year" = year_ranges)) %>% distinct()
  
}



rsf_results_table3 <- left_join(rsf_results_table, dt_years, by = c("bird"="animal"))

# Define the desired order of years
year_order <- c("2017_2018", "2018_2019", "2019_2020", "2020_2021", "2021_2022", "2022_2023", "2023_2024")

# Convert the 'year' column to a factor with the specified levels
rsf_results_table3$year <- factor(rsf_results_table3$year, levels = year_order)




# Plot
ggplot(data = rsf_results_table3 %>% 
         filter(covariates %in% c("strava", "strava:total.visitors.std", "Trees", "Shrubs", "Buildings", "Cliffs")), 
       aes(y = est, x = year, color = covariates)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +  # Add jitter for better visualization
  labs(
    x = "Year",
    y = "Estimate",
    title = "Estimates Across Years",
    color = "Covariates"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
    plot.title = element_text(size = 14, face = "bold")
  )




# Plot

# Calculate the mean of `est` for each `year` and `covariates`
mean_estimates <- rsf_results_table3 %>%
  filter(covariates %in% c("strava", "strava:total.visitors.std", "Trees", "Shrubs", "Buildings", "Cliffs")) %>%
  group_by(year, covariates) %>%
  summarize(mean_est = mean(est, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(data = rsf_results_table3 %>% 
         filter(covariates %in% c("strava", "strava:total.visitors.std", "Trees", "Shrubs", "Buildings", "Cliffs")), 
       aes(x = factor(year, levels = unique(rsf_results_table3$year)), 
           y = est, 
           color = covariates)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.8), alpha = 0.7) +  # Boxplots
  geom_line(data = mean_estimates, aes(x = year, y = mean_est, group = covariates, color = covariates), 
            size = 1) +  # Add mean lines
  geom_point(data = mean_estimates, aes(x = year, y = mean_est, color = covariates), size = 2) +  # Highlight mean points
  labs(
    x = "Year",
    y = "Estimate",
    title = "Estimates Across Years with Mean Trends",
    color = "Covariates"
  ) +
  scale_x_discrete(expand = c(0.05, 0)) +  # Reduce padding around x-axis
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
    plot.title = element_text(size = 14, face = "bold")
  )
#********************************************************************















