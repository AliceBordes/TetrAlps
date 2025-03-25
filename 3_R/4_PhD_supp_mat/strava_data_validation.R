#### PhD Tetralpes project ####

# Alice Bordes #

# November 2024 #

# Description:

# Strava data validation by visitor number on ski lifts



#### 1_Loading objects ----

### Loading libraries ---- 
#********************************************************************
library(sf)
library(ggplot2)
library(terra)
library(tidyterra)
library(ggnewscale)
library(gridExtra)
library(openxlsx)
library(dplyr)
library(ctmm)
library(raster)
#********************************************************************



### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD"
#********************************************************************

# Loading data ----
#********************************************************************

### VECTORS

# 3V borders 
borders_3V_vect <- st_read(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))
borders_3V_vect <- terra::vect(file.path(base,"Tetralps/1_RAW_DATA/borders_3V.gpkg"))

# Environment stack
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/env_RL_list.RData"))
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/scaled_env_RL_list.RData"))

# Loading 3V ski lift traffic (ski lift traffic in 2022-2023 associated with ski lift geometry from Open Street Map)
cables_3V_traffic <- terra::vect(file.path(base,"TetrAlps","2_DATA", "ski_lift_traffic_3V.gpkg"))

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

visitor_menui <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/menuires_visitors.csv", sep=",")
visitor_menui$Date <- as.Date(visitor_menui$Date)
visitor_menui$Total <- as.integer(visitor_menui$Total)
visitor_menui$Total_std <- scale(visitor_menui$Total)


# dt_resorts <- read.csv2(file.path(base,"Tetralps","2_Data","bg_winter_assign_valley_resort.csv"))
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



#### 1_Vizualising the raster with visitor numbers attached to each ski lift ----
#********************************************************************
# Settings
season_of_interest = "2017_2019"

# Loading 3V ski lift traffic 
cables_visitors <- terra::vect(file.path(base, "TetrAlps","2_DATA", "ski_resorts_visitor_numbers", paste0("cables_visitors_2024_11_", season_of_interest,".gpkg")))

# Convert SpatVector to sf object for ggplot compatibility
cables_visitors_sf <- st_as_sf(cables_visitors)

# Ensure the column is numeric
cables_visitors_sf$std_yearly_visitor_nb <- as.numeric(cables_visitors_sf$std_yearly_visitor_nb)

# Create a buffer of 100 meters around the cables for data visualization
cables_visitors_sf$geometry_buffer <- st_buffer(cables_visitors_sf$geometry, dist = 100)

# Plot using ggplot
ggplot(data = cables_visitors_sf) +
  geom_spatraster(data = rast(env_RL_list[["strava"]]))+
  # scale_fill_gradientn(name = "Strava intensity",colors=c("#CCCCCC11","#FF6600","#FF3333"))+
  scale_fill_continuous()+
  geom_spatvector(data = borders_3V_vect, fill = NA, aes(color = NOM)) +
  scale_color_manual(values = c("Courchevel" = "darkgreen", "Les Allues" = "lightgreen", "Les Belleville" = "green")) +
  new_scale_fill()+
  new_scale_color()+
  geom_sf(aes(geometry = geometry_buffer, fill = yearly_visitor_nb, color = yearly_visitor_nb), size = 4) +
  # scale_fill_gradientn(name = "Strava intensity",colors=c("lightblue","blue", "black"))+
  # scale_color_gradientn(name = "Strava intensity",colors=c("lightblue","blue", "black"))+
  scale_fill_gradientn(name = "Strava intensity",colors=c("#CCCCCC11","#FF6600","#FF3333"))+
  scale_color_gradientn(name = "Strava intensity",colors=c("#CCCCCC11","#FF6600","#FF3333"))+
  theme_minimal() +
  labs(
    title = paste0("Standardized total visitor Numbers by ski lift for the season ",season_of_interest),
    subtitle = "Background : strava raster (data from June, 25 of 2024) ",
    x = "Longitude",
    y = "Latitude"
  )+
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12))


#### 3_Extracting strava value behind visitor numbers on ski lift ----
#********************************************************************
cables_visitors$ID <- seq_along(cables_visitors$name)

extract_visitor_strava_mean <- terra::extract(y = cables_visitors, 
                                            x = rast(env_RL_list[["strava"]]),
                                            fun = mean, method = "bilinear", na.rm = TRUE)

extract_visitor_strava_sd <- terra::extract(y = cables_visitors, 
                                         x = rast(env_RL_list[["strava"]]),
                                         fun = sd, method = "bilinear", na.rm = TRUE)

# cables_visitors = 286 obs = 286 ski lift names AND extract_strava = 286 ID with strava value
extract_visitor_strava <- merge(extract_visitor_strava_mean, extract_visitor_strava_sd, by = "ID", all.x = TRUE)
names(extract_visitor_strava) <- c("ID", "mean", "sd")
vect_strava_visitors <- merge(cables_visitors, extract_visitor_strava, by = "ID", all.x = TRUE)



dt_strava_visitors <- data.frame("cable" = vect_strava_visitors$name,
                                 "visitor_number" = as.numeric(vect_strava_visitors$yearly_visitor_nb),
                                 "mean_strava" = as.numeric(vect_strava_visitors$mean),
                                 "sd_strava" = as.numeric(vect_strava_visitors$sd),
                                 "ski_resort" = vect_strava_visitors$ski_resort,
                                 "ski_season" = vect_strava_visitors$ski_season)
#********************************************************************



#### 4_Validation of the correlations between strava and visitor numbers on ski lift ----
#********************************************************************
#Person correlation
dt_strava_visitors_rem_NA <- na.omit(dt_strava_visitors)
cor(dt_strava_visitors_rem_NA$mean_strava, y = dt_strava_visitors_rem_NA$visitor_number)

ggplot(data = dt_strava_visitors, aes(x = visitor_number, y = mean_strava))+
  geom_point(aes(color = ski_resort, x = visitor_number, y = mean_strava), size = 2)+
  geom_errorbar(data = dt_strava_visitors,
                aes(ymin=mean_strava-sd_strava, ymax=mean_strava+sd_strava,
                    color = ski_resort),
                width=.2,
                position=position_dodge(0.05))+
  geom_smooth(method = "gam", color = "#666666")+
  labs(title = "Relation between strava value and visitor number for each ski lift",
       subtitle = paste0("visitor data: ",season_of_interest," ; strava data: June 2023_June 2024 (winter activities only)"),
       x = "Total winter visitor number on the ski lift", 
       y = "Median strava value  on the ski lift location",
       color = "Resort of the ski lift")+
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12))
#********************************************************************




#### 5_Comparing strava values from year to year ----
#********************************************************************
# 
vect_y <- c("2017_2018", "2018_2019", "2019_2020", "2021_2022", "2022_2023", "2023_2024")

l_dt_strava_visitors <- list()
dt_strava_visitors <- c()

for(year in seq_along(vect_y))
{
 # Loading 3V ski lift traffic 
  cables_visitors <- terra::vect(file.path(base, "TetrAlps","2_DATA", "ski_resorts_visitor_numbers", paste0("cables_visitors_2024_11_", vect_y[year],".gpkg")))
 
  
  # Convert SpatVector to sf object for ggplot compatibility
  cables_visitors_sf <- st_as_sf(cables_visitors)

  # Ensure the column is numeric
  cables_visitors_sf$yearly_visitor_nb <- as.numeric(cables_visitors_sf$yearly_visitor_nb)
  
  cables_visitors$ID <- seq_along(cables_visitors$name)
  
  extract_visitor_strava_mean <- terra::extract(y = cables_visitors, 
                                                x = rast(env_RL_list[["strava"]]),
                                                fun = mean, method = "bilinear", na.rm = TRUE)
  
  extract_visitor_strava_sd <- terra::extract(y = cables_visitors, 
                                              x = rast(env_RL_list[["strava"]]),
                                              fun = sd, method = "bilinear", na.rm = TRUE)
  
  
  # cables_visitors = 286 obs = 286 ski lift names AND extract_strava = 286 ID with strava value
  extract_visitor_strava <- merge(extract_visitor_strava_mean, extract_visitor_strava_sd, by = "ID", all.x = TRUE)
  names(extract_visitor_strava) <- c("ID", "mean", "sd")

  
  vect_strava_visitors <- merge(cables_visitors, extract_visitor_strava, by = "ID", all.x = TRUE)
  
  
  dt_strava_visitors <- data.frame("cable" = vect_strava_visitors$name,
                                   "visitor_number" = as.numeric(vect_strava_visitors$yearly_visitor_nb),
                                   "mean_strava" = as.numeric(vect_strava_visitors$mean),
                                   "sd_strava" = as.numeric(vect_strava_visitors$sd),
                                   "ski_resort" = vect_strava_visitors$ski_resort,
                                   "ski_season" = vect_strava_visitors$ski_season)
  
  l_dt_strava_visitors[[year]] <- dt_strava_visitors

}
yearly_strava_visitors <- do.call(bind_rows, l_dt_strava_visitors)


season1 = "2022_2023"
season2 = "2019_2020"


# Filter data for the two seasons
data_filtered <- yearly_strava_visitors %>%
  filter(ski_season %in% c(season1, season2))

# Reshape the data for comparison
data_wide <- data_filtered %>%
  pivot_wider(names_from = ski_season, values_from = visitor_number) %>%
  rename("saison1" = season1, "saison2" = season2)  # Rename for clarity

# calculate the correlation between the 2 seasons
# data_wide <- data_wide %>% filter(!is.na(saison2))
cor_nb_tot <- round(cor(data_wide$saison1, data_wide$saison2),2)

# Plot the comparison
ggplot(data_wide, aes(x = saison1, y = saison2, color = ski_resort)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Diagonal reference line
  labs(
    x = paste0("Total visitor number by ski lift (", season1, ")"),
    y = paste0("Total visitor number by ski lift (", season2, ")"),
    title = paste0("Comparison of the total of visitor numbers between ski seasons")
  ) +
  geom_text(x = max(data_wide$saison1, na.rm = TRUE)/10,
            y = max(data_wide$saison2, na.rm = TRUE)/1.1,
            label= paste0("correlation = ", cor_nb_tot),
            hjust = 0,
            size = 5,
            color = "black")+
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12))
#*********************************************************************



#### 6_Calculation of the average annual number of visitors per ski lift ----
#********************************************************************

visitor_meribel_mean <- visitor_meribel %>%
  group_by(ski_season) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "{col}"))

visitor_valtho_mean <- visitor_valtho %>%
  group_by(ski_season) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "{col}"))

visitor_courch_mean <- visitor_courch %>%
  group_by(ski_season) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "{col}"))

visitor_menui_mean <- visitor_menui %>%
  group_by(ski_season) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "{col}"))




### Plotting 

# Assuming visitor_meribel_mean_by_season is your dataframe

# Reshape the data to plot the correlation between the mean visitor number of 2 ski seasons
l_resort <- list(visitor_valtho_mean, visitor_menui_mean, visitor_meribel_mean, visitor_courch_mean)
names(l_resort) <- c("Val Thorens", "Les Ménuires", "Méribel-Mottaret", "Courchevel")

mean_visitor <- mean_visitor_plot(l_resort, "2022_2023", "2023_2022")
#********************************************************************





#### 7_Check the differences between the different strava raster to identify the impact of strava update ----
#********************************************************************
# strava

unique_layer <- function(raster)
{
    if(length(names(raster))!=1)
    {
      raster <- strava::as_numeric(raster)
    }
    raster <- project(raster, y = "EPSG:2154", method = "bilinear") # y = a raster to align on (avoid to use the function resample after), "EPSG:2154" can be specify if there is no argument y. pas besoin de mettre de focal (zone d'influence), method="belinear" advised for continuous raster
    raster <- crop(raster, env_RL_list[["elevation"]])
    return(raster)
}

list.files(file.path(base, "Tetralps/2_DATA/strava/3Vallees"))
strava_jun_24 <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_3V_winter_sports_rgb_single_3Vallees_25_06_2024.tif"))
strava_jul_24 <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_winter_2024_07_01.tif"))
strava_aug_24 <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_winter_2024_08_12.tif"))
strava_oct_24 <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_winter_2024_10_03.tif"))
strava_oct_mid_24 <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_winter_2024_10_16.tif"))
strava_nov_24 <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_winter_2024_11_29.tif"))
strava_dec_24 <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_winter_2024_12_12.tif"))

strava_jun_24 <- unique_layer(strava_jun_24)
strava_jul_24 <- unique_layer(strava_jul_24)
strava_aug_24 <- unique_layer(strava_aug_24)
strava_oct_24 <- unique_layer(strava_oct_24)
strava_oct_mid_24 <- unique_layer(strava_oct_mid_24)
strava_nov_24 <- unique_layer(strava_nov_24)
strava_dec_24 <- unique_layer(strava_dec_24)


strava_jun_24 - strava_jul_24
strava_jun_24 - strava_aug_24
strava_aug_24 - strava_oct_24
strava_oct_24 - strava_nov_24
strava_nov_24 - strava_dec_24

# comparison of cell values to create new SpatRaster
par(mfrow = c(2,3))
plot(strava_jun_24 == strava_jul_24, main = "25jun_24 VS 1jul_24", cex.main = 2) ; plot(cables_visitors, add = TRUE, col = "red")
plot(strava_jul_24 == strava_aug_24, main = "1jul_24 VS 12aug_24", cex.main = 2) ; plot(cables_visitors, add = TRUE, col = "red")
plot(strava_aug_24 == strava_oct_24, main = "12aug_24 VS 3oct_24", cex.main = 2) ; plot(cables_visitors, add = TRUE, col = "red")
plot(strava_oct_mid_24 == strava_oct_24, main = "16oct_24 VS 3oct_24", cex.main = 2) ; plot(cables_visitors, add = TRUE, col = "red")
plot(strava_oct_24 == strava_nov_24, main = "3oct_24 VS 29nov_24", cex.main = 2) ; plot(cables_visitors, add = TRUE, col = "red")
plot(strava_nov_24 == strava_dec_24, main = "29nov_24 VS 12dec_24", cex.main = 2) ; plot(cables_visitors, add = TRUE, col = "red")

plot(env_RL_list[["strava"]])
plot(env_RL_list[["elevation"]], main = "Elevation and 3V ski lifts", cex.main = 2)
plot(cables_visitors, add = TRUE)
#********************************************************************

