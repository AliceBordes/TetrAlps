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
#********************************************************************


### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD/Tetralps"
covid <- c("Caramel_2", "Daisy","Dalton","Dameur","Dario","Darkvador","Darwin","Dede","Destroy","Diot","Djal","Django","Donald","Durite","Dynamite","Dyonisos")
#********************************************************************


### Loading functions ----
#********************************************************************
source(file.path("4_FUNCTIONS","RSF","rsf_functions.R"))
source(file.path("4_FUNCTIONS","Formatting_data/formatting_environment_data.R"))
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
load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","env_RL_list.RData"))
load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","scaled_env_RL_list.RData"))
#********************************************************************

### Data Loading of telemetry, guess, fit and akde objects for rsf ----
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

#### 1_Data description ####

#### 1.1_3V tag type ####
#********************************************************************
#total
g_GPS_tag_tot<-
  ggplot(data=synth_bg_all_sites,aes(x=zone_etude, fill = "zone_etude"))+
  geom_bar()+
  scale_fill_manual(values = "lightblue")+
  labs(x="Study sites",y="Number of tags", title="GPS-tag characteristics")+
  geom_text(aes(label = ..count..),position=position_stack(vjust = 1), stat = "count", vjust = 1.5, colour = "black")+
  theme(axis.text.x = element_text(size=11),axis.title = element_text(size=12),title = element_text(size=12),strip.text.x = element_text(size = 14))

g_GPS_tag_tot
# ggsave(plot=g_GPS_tag_tot,file=paste0(base,"/6_FIGURES/data_exploration/","g_GPS_tags_total_",format(Sys.time(), "%d.%b%Y"),".png"))



### Basic plotting ####
ext_bird <- extent(l_telemetry_winter[["Abel"]][[1]][c("x","y")])

ggplot() + theme_void() +
  geom_spatraster(data = terra::rast(scaled_env_RL_list[["strava_winter_sports"]])) +
  scale_fill_viridis() +
  geom_point(data = l_telemetry_winter[["Abel"]][[1]], aes(x = x, y = y), color = "coral", size = 1) + # to transform data into lines
  xlim(ext_bird[1,1]-1000,ext_bird[2,1]+1000) +
  ylim(ext_bird[1,2]-1000,ext_bird[2,2]+1000)
#********************************************************************




#### 1.2_Map birds locations throughout the day  ####
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

# Convert to sf object
combined_telemetry_sf <- combined_telemetry %>%
  filter(!animal.ID %in% covid) %>%
  st_as_sf(coords = c("x", "y"), crs = 2154)  # Use the correct CRS


extent(combined_telemetry_sf)



# Convert time and define categories
combined_telemetry_sf <- combined_telemetry_sf %>%
  mutate(color_category = ifelse(hms(time) > hms("08:30:00") & hms(time) < hms("09:00:00"), "red", "turquoise"),
         size_category = ifelse(hms(time) > hms("08:30:00") & hms(time) < hms("09:00:00"), 0.6, 0.5))

ggplot() +
  theme(plot.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        legend.title = element_text(size=14)) +
  ggtitle("Bird locations in the 3 Vallées region") +
  geom_spatraster(data = terra::rast(scaled_env_RL_list[["strava_winter_sports"]])) +
  scale_fill_viridis_c(option = "viridis") +
  # Plot the vector borders with valley names
  geom_spatvector(data = borders_3V_vect, fill = NA, aes(color = NOM)) +
  scale_color_manual(values = c("Courchevel" = "darkgreen", "Les Allues" = "lightgreen", "Les Belleville" = "darkgrey")) +
  labs(color = "Valley") +
  new_scale_color() +
  theme(legend.position = "none") +
  geom_sf(data = combined_telemetry_sf, 
          aes(color = color_category, size = size_category)) +
  scale_size_continuous(range = c(0.5, 0.7)) +  # Fine-tune size scaling
  ylim(6471291, 6479247) +
  xlim(970968, 980660)


# Plot with the updated legend
ggplot() +
  theme(plot.title = element_text(size = 16), legend.text = element_text(size=12), legend.title = element_text(size=14)) +
  ggtitle("Bird locations in the 3 Vallées region") +
  geom_spatraster(data = terra::rast(scaled_env_RL_list[["strava_winter_sports"]])) +
  scale_fill_viridis_c(option = "viridis") +
  # Plot the vector borders with valley names
  geom_spatvector(data = borders_3V_vect, fill = NA, aes(color = NOM)) +
  scale_color_manual(values = c("Courchevel" = "darkgreen", "Les Allues" = "lightgreen", "Les Belleville" = "darkgrey")) +
  labs(color = "Valley") +
  new_scale_color()+
  theme(legend.position="none")+
  geom_sf(data = combined_telemetry_sf, 
          aes(color = ifelse(hms(time) > hms("08:30:00") & hms(time) < hms("09:00:00"), "red", "turquoise"),
              size = ifelse(hms(time) > hms("08:30:00") & hms(time) < hms("09:00:00"), 0.6, 0.5))
          ) +
  ylim(6471291,6479247)+
  xlim(970968,980660)
     
  
#********************************************************************




View(l_telemetry_winter[!names(l_telemetry_winter)%in%covid][[1]][[1]])





