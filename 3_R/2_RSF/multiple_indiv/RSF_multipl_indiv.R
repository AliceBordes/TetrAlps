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

visitor_valtho <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/valtho_visitors.csv", sep=",")
visitor_valtho$Date <- as.Date(visitor_valtho$Date)

visitor_courch <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/courchevel_visitors.csv", sep=",")
visitor_courch$Date <- as.Date(visitor_courch$Date)

visitor_menui <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/menuires_visitors.csv", sep=",")
visitor_menui$Date <- as.Date(visitor_menui$Date)
visitor_menui$Total <- as.integer(visitor_menui$Total)

# Snow deph
snow_meribel <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/snow_depth/meribel_snow_depth.csv", sep=",")
snow_meribel$Date <- as.Date(snow_meribel$Date)
snow_meribel <- snow_meribel %>% group_by(Date) %>% summarise(snow.depth = mean(cumul.H.neige.cm))
snow_meribel <- as.data.frame(snow_meribel)

snow_courch <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/snow_depth/courchevel_snow_depth.csv", sep=",")
snow_courch$Date <- as.Date(snow_courch$Date)
snow_courch <- snow_courch %>% group_by(Date) %>% summarise(snow.depth = mean(cumul.H.neige.cm))
snow_courch <- as.data.frame(snow_courch)

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
  


### 1_Add visitor numbers and snow depth to birds_bg_dt ----
#********************************************************************

birds_sample_bg_pretele <- birds_bg_dt 
birds_sample_bg_pretele$jour <- as.Date(birds_sample_bg_pretele$jour)

# First, join both snow datasets
birds_sample_bg_pretele2 <- birds_sample_bg_pretele %>%
  left_join(snow_meribel %>% rename(snow.depth.meribel = snow.depth) %>% dplyr::select(Date, snow.depth.meribel), by = c("jour" = "Date")) %>%
  left_join(snow_courch %>% rename(snow.depth.courchevel = snow.depth) %>% dplyr::select(Date, snow.depth.courchevel), by = c("jour" = "Date")) %>%
  left_join(visitor_meribel %>% dplyr::select(Date, Total) %>% rename(total.visitors.meribel = Total), by = c("jour" = "Date")) %>%
  left_join(visitor_courch %>% dplyr::select(Date, Total) %>% rename(total.visitors.courch = Total), by = c("jour" = "Date")) %>%
  left_join(visitor_valtho %>% dplyr::select(Date, Total) %>% rename(total.visitors.valtho = Total), by = c("jour" = "Date")) %>%
  left_join(visitor_menui %>% dplyr::select(Date, Total) %>% rename(total.visitors.menui = Total), by = c("jour" = "Date")) %>%

  # Use case_when to set snow.depth based on valley
  mutate(snow.depth = case_when(
    valley == "Courchevel" ~ snow.depth.courchevel,
    valley == "Les Allues" ~ snow.depth.meribel,
    valley == "Les Belleville" ~ snow.depth.meribel,
    TRUE ~ NA_real_  # If valley is neither, set NA
  ),
  total.visitors = case_when(
    resort == "Méribel" ~ total.visitors.meribel,
    resort == "Méribel-Mottaret" ~ total.visitors.meribel,
    resort == "Courchevel" ~ total.visitors.courch,
    resort == "Les Ménuires" ~ total.visitors.menui,
    TRUE ~ NA_real_
  )) %>%

  # Clean up by removing intermediary columns
  dplyr::select(-total.visitors.meribel, -total.visitors.courch,-total.visitors.valtho,-total.visitors.menui, -snow.depth.meribel, -snow.depth.courchevel)


#### Scaling snow and visitor data using  the data of each pair valley, resort for a given data
# the scaling is not proceeded over the all pretelemtry dataframe because data are repeated for each GPS observation

# creation of the data frame for scaling
birds_sample_bg_pretele2_for_scale <- birds_sample_bg_pretele2 %>%
  group_by(jour, valley, resort) %>%
  summarise(
    snow.depth = mean(snow.depth, na.rm = TRUE),
    total.visitors = mean(total.visitors, na.rm = TRUE)
    # .groups = "drop"  # Remove grouping after summarisation
  ) 
birds_sample_bg_pretele2_for_scale$snow.depth.std <- scale(birds_sample_bg_pretele2_for_scale$snow.depth)
birds_sample_bg_pretele2_for_scale$total.visitors.std <- scale(birds_sample_bg_pretele2_for_scale$total.visitors)

#bind all the info on a unique data frame pretelemetry
birds_sample_bg_pretele_final <- left_join(birds_sample_bg_pretele2, 
                 as.data.frame(birds_sample_bg_pretele2_for_scale) %>% dplyr::select(-snow.depth, -total.visitors),
                 by = c("jour", "valley", "resort"))


#********************************************************************



  
### 2_Data creation of telemetry, guess, fit and akde objects for rsf ----
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



# tele_akde(data = filtered_birds_bg_dt,
#           # birds_vect = names(l_telemetry_winterS),
#           season = "hiver",
#           subset_category = "all",
#           outputfolder = file.path(base, "Tetralps", "3_R", "0_Heavy_saved_models", "birds_3V"),
#           write = TRUE)

tele_akde(data = birds_sample_bg_pretele_final,
          season = "hiver",
          subset_category = "all",
          outputfolder = file.path(base, "Tetralps", "3_R", "0_Heavy_saved_models", "birds_3V"),
          write = TRUE)
#********************************************************************  



  
  


#### 3_Home range estimation : Fitting an RSF for 1 bird ----
#********************************************************************



### Loading data for rsf ----
#********************************************************************
  # load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","old","multipl_telemetry_winter_saison2.Rdata"))
  # load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_guess_winter_saison.Rdata"))
  # load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_fit_winter_saison2.Rdata"))
  # load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_akde_winter_saison2.Rdata"))
  
  load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_telemetry_winter_hiver.Rdata"))
  load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_guess_winter_hiver.Rdata"))
  load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_fit_winter_hiver.Rdata"))
  load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_akde_winter_hiver.Rdata"))


  # Filter the data set by the birds we can perform a rsf
  covariates_NAN_cleaned <- function(data)
  {
    # Remove the birds with monitored during winter
    list_multipl_winter <- sapply(l_telemetry_winter, function(x) length(unique(x[[1]][["saison2"]])))
    list_multipl_winter <- list_multipl_winter[list_multipl_winter==1]
    list_multipl_winter <- names(list_multipl_winter)
    data <- data[names(data) %in% list_multipl_winter]
    
    # Remove the birds with NaN in total.visitors.std
    list_no_NAN_visit <- sapply(data, function(x) any(is.na(x[[1]][["total.visitors.std"]])))
    list_no_NAN_visit <- list_no_NAN_visit[!list_no_NAN_visit]
    list_no_NAN_visit <- names(list_no_NAN_visit)
    data <- data[names(data) %in% list_no_NAN_visit]
    
    # Remove the birds with NaN in snow.depth.std
    # list_no_NAN_snow <- sapply(data, function(x) any(is.na(x[[1]][["snow.depth.std"]])))
    # list_no_NAN_snow <- list_no_NAN_snow[!list_no_NAN_snow]
    # list_no_NAN_snow <- names(list_no_NAN_snow)
    # data <- data[names(data) %in% list_no_NAN_snow]
    
    
    return(data)
  }
  
  
  l_telemetry_winter <- covariates_NAN_cleaned(l_telemetry_winter)
  
  l_akde_winter <- l_akde_winter[names(l_akde_winter) %in% names(l_telemetry_winter)]
  l_guess_winter <- l_guess_winter[names(l_guess_winter) %in% names(l_telemetry_winter)]
  l_fit_winter <- l_fit_winter[names(l_fit_winter) %in% names(l_telemetry_winter)]
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
bird = "Foliedouce"
  
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
scaled_env_RL_list_selection <-  scaled_env_RL_list[!(names(scaled_env_RL_list) %in% c("slope"))]
# scaled_env_RL_list_selection <-  scaled_env_RL_list[c("elevation", "squared_elevation", "strava","leks")]
  

system.time(
RSF_results_multpl_birds <- RSF_birds(  l_telemetry_winter[3:10], 
                                        l_akde_winter[3:10],
                                        scaled_env_RL_list_selection,
                                        # grid = "full",
                                        outputfolder = file.path(base, "Tetralps", "3_R", "0_Heavy_saved_models", "birds_3V"),
                                        write = TRUE
                                        ) 
)


load(file = file.path(base, "Tetralps", "3_R", "0_Heavy_saved_models", "birds_3V", "RSF_results", paste0("l_telemetry_winter.Rdata")))
load(file = file.path(base, "Tetralps", "3_R", "0_Heavy_saved_models", "birds_3V", "RSF_results", paste0("l_telemetry_winter[1_2]_individual.Rdata")))
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


ggplot(data = rsf_results_table %>% filter(covariates %in% c("strava", "strava:total.visitors.std", "Cliffs_water", "Trees", "Shrubs", "Buildings")), aes(y = covariates, x = est))+
  geom_boxplot(aes(color = covariates), fill = alpha("grey", 0.2))+
  geom_jitter(color = alpha("black",0.6))+
  labs(y = "Covariates", 
       x = "Estimates", 
       title = paste0("Results of the RSF performed on ", length(unique(rsf_results_table$bird)), " birds \n(estimated coefficients by covariate)"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme(axis.text.x = element_text(hjust = 1),  
        panel.background = element_blank(),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12))

#********************************************************************




### 3.6_Predictions ----
#********************************************************************
par(mfrow = c(1,2))
prediction <- simulate( l_akde_winter[[1]][[1]]@CTMM,
                        l_telemetry_winter[[1]][[1]])
plot(prediction)
plot(l_telemetry_winter[[1]][[1]], col = "blue", add = TRUE)

ggplot()+
  geom_spatraster(data = terra::rast(scaled_env_RL_list_selection[["strava"]]))+
  xlim(c(min(l_telemetry_winter[[1]][[1]][["x"]]-1000),
           max(l_telemetry_winter[[1]][[1]][["x"]]+1000)))+
  ylim(c(min(l_telemetry_winter[[1]][[1]][["y"]]-1000),
         max(l_telemetry_winter[[1]][[1]][["y"]]+1000)))+
  geom_point(aes(x = prediction[["x"]],
                 y = prediction[["y"]]),
             color = alpha("red", 0.2), size = 1)+
  geom_point(aes(x = l_telemetry_winter[[1]][[1]][["x"]],
                 y = l_telemetry_winter[[1]][[1]][["y"]]),
             color = alpha("yellow",0.2), size = 1)
  
  

prediction <- simulate( sum_rsf_multipl[[1]],
                        l_telemetry_winter[[1]][[1]])
plot(prediction)
plot(l_telemetry_winter[[1]][[1]], col = "blue", add = TRUE)
#********************************************************************





### 4_Meta_RSF ----
#********************************************************************

### 4.1_Visualization of home ranges ----
#********************************************************************

# Unest the akde list
l_akde_winter_meta <- sapply(l_akde_winter, `[[`, 1)
# Plot AKDES
plot(l_akde_winter_meta,
     col.DF = pal,
     col.level = pal,
     col.grid = NA,
     level = NA, 
     main = paste0("Home ranges (", length(l_akde_winter_meta), " birds) (3 Vallées)"))



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


# Population density: -----------------------------------------------------

# this is a straight mean of the individual densities that doesn't model population variance
help("mean.UD")
# note the 'sample' argument for correct CIs

# extract the ctmm objets from the akde list of UD objects
l_ctmm_winter_meta <- lapply(l_akde_winter_meta, function(ud) ud@CTMM)
l_telemetry_winter_meta <- lapply(l_telemetry_winter, function(x) x[["all"]])

# straight mean - for a population of 6 buffalo
ctmm_meta <- mean(l_ctmm_winter_meta,sample=FALSE)
summary(ctmm_meta)
#********************************************************************

