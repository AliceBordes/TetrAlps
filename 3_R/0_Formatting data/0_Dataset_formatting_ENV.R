#### PhD TetraAlps project ####

# Alice Bordes #

# August 2024 #

# Description:

# Formatting environment dataset for RSF and SSF



### Loading libraries ---- 
#********************************************************************
library(move2)
library(dplyr)
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
library(broom)
library(janitor)
library(lubridate)
library(openxlsx)
library(tidyverse)
library(dplyr)
#********************************************************************


### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD"
#********************************************************************


# Loading rasters ----
#********************************************************************

### RASTERS

mnt_9 <- terra::rast(file.path(base, "Tetralps/2_DATA/mnt_9_mean_ign.tif"))
mnt_9 <- project(mnt_9, "EPSG:2154")

# slope 3V
slope_3V <- terra::rast(file.path(base,"Tetralps/2_DATA/slope_3V_ign.tif"))
crs(slope_3V) <- "EPSG:2154"
slope_3V <- project(slope_3V, y = mnt_9, method = "bilinear") # resolution of slope_3V is set at 9m such as mnt_9

# strava
strava <- terra::rast(file.path(base, "Tetralps/2_DATA/strava/3Vallees/strava_3V_winter_sports_rgb.tif_single.tif"))
strava <- project(strava, y = mnt_9, method = "bilinear") # y = a raster to align on (avoid to use the function resample after), "EPSG:2154" can be specify if there is no argument y. pas besoin de mettre de focal (zone d'influence), method="belinear" advised for continuous raster
# resolution of strava is set artificially at 9m such as mnt_9, but the true resolution is 38.21851m

# habitat cartography
carto_habitats_3V <- terra::rast(file.path(base,"Tetralps/2_DATA/carto_habitats_clara_3V.tif")) #carto Clara
levels(carto_habitats_3V)[[1]][["landcover_1m"]]<-c("Unclassified soil","Fine mineral soil","Coarse mineral soil","Cliff","Dry or rocky grassland","Herbaceous", "Low ligneous","Shrubs","Unclassified trees","Deciduous trees","Resinous trees","Buildings","Natural pond","Artificial pond","Waterway",  "Unclassified")
carto_habitats_3V <- project(carto_habitats_3V, y = mnt_9, method = "near")

# Aggregate habitat categories

carto_habitats_3V_winter <- carto_habitats_3V

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(20, 1))

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(20, 1)) #Unclassified soil
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(21, 1)) #Fine mineral soil
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(22, 1)) #Coarse mineral soil
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(30, 1)) #Dry or rocky grassland
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(31, 1)) #Herbaceous
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(32, 1)) #Low ligneous

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(40, 2)) #Shrubs

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(50, 3)) #Unclassified trees
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(51, 3)) #Deciduous trees
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(52, 3)) #Resinous trees

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(60, 4)) #Buildings

carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(23, 5)) #Cliff
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(92, 5)) #Natural pond
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(93, 5)) #Artificial pond
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(94, 5)) #Waterway
carto_habitats_3V_winter <- classify(carto_habitats_3V_winter, cbind(100, 5)) #Unclassified

terra::plot(carto_habitats_3V_winter)
carto_habitats_3V_winter <- as.factor(carto_habitats_3V_winter)


# Lek sites
leks <- st_read(file.path(base,"TetrAlps/1_RAW_DATA/place_de_chant/place_chant_02_07_2024.gpkg")) # read the geometry object
r_leks <- terra::rast(ext(mnt_9), resolution = res(mnt_9), crs = "EPSG:2154") # create a raster template 
r_leks <- rasterize(leks, r_leks) # transform the geometry into a raster 
r_leks[is.na(r_leks)] <- 0 # Replace NA with 0
r_leks <- as.factor(r_leks)


# Buffer around leks sites
    # Convert sf object to SpatVector
    leks_vect <- vect(leks)
    # Create a raster template with 1 meter resolution
    raster_template <- rast(ext(mnt_9), resolution = res(mnt_9), crs = crs(mnt_9))
    # Rasterize the buffered polygon (1 for inside the polygon, NA for outside)
    polygon_raster <- rasterize(leks_vect, raster_template, field = 1, background = NA)
    # assign 0 to NA (areas outside the polygon)
    polygon_raster[is.na(polygon_raster)] <- 0
    # plot(polygon_raster)
    # Calculate distance to the nearest border of the polygon
    distance_to_border <- terra::distance(polygon_raster, target = polygon_raster, unit = "m")
    # terra::plot(distance_to_border)
    # Create a 600-meter buffer around the polygon (on the SpatVector)
    buffer_vect <- buffer(leks_vect, width = 600)
    # Create a raster for masking (optional: if you want to restrict calculations to certain areas)
    # For example, creating a mask raster of the buffer area if needed
    buffer_raster <- rasterize(buffer_vect, raster_template, field = 1, background = NA)
    # Mask the distance raster to only include values within the buffer (if masking is necessary)
    distance_buffer <- mask(distance_to_border, buffer_raster)
    # terra::plot(distance_buffer)
    # Normalize the distance values to range from 0 to 1
    # Ensure non-NA values are normalized
    min_distance <- min(values(distance_buffer), na.rm = TRUE)
    max_distance <- max(values(distance_buffer), na.rm = TRUE)
    r_leks_dist <- (distance_buffer - min_distance) / (max_distance - min_distance)
    r_leks_dist[is.na(r_leks_dist)] <- 0
    # terra::plot(r_leks_dist)


# Cables
cables <- st_read(file.path(base,"TetrAlps/1_RAW_DATA/human_traffic_in_ski_resorts/cables_ogm28resume_troncons_actuel_alpes.gpkg")) # read the geometry object


# Predation : Corentin the fox (probability of presence over the area)
fox_Corentin_sakde <- terra::rast(file.path(base,"TetrAlps/2_DATA/fox_Corentin_simple_akde_PMF.tif")) # simple akde over a period of 7 months

# Snow deph
snow_meribel <- read.xlsx("C:/Users/albordes/Documents/PhD/TetrAlps/1_RAW_DATA/environment/enneigement/meribel_meteo_france_neige.xlsx", sheet = "Saison 2018.2019")
    colnames(snow_meribel) <- c( "Date", "H.neige.cm", "Neige.fraiche.cm", "Cumul.neige.fraiche.avant.saison.cm")
    # Fill NA values in the Date column with the value above
    snow_meribel <- snow_meribel %>% fill(Date, .direction = "down")
    snow_meribel$Date <- excel_numeric_to_date(snow_meribel$Date)
    # "saison 2" in the bird dataset : winter = 15 nov to 14 feb
    snow_meribel <- snow_meribel[snow_meribel$Date >= as.Date("2018-11-15") & snow_meribel$Date <= as.Date("2019-02-14"), ]

    
# Ski resort visitors
        # Méribel Mottaret
        meribel_formatting <- function(folderpath = file.path(base,"TetrAlps/1_RAW_DATA/human_traffic_in_ski_resorts/Meribel_mottaret_RM_Historique_des_passages_2017_22/"),sheetname = "Passage Détail", save = FALSE, folderoutpath = file.path(base,"TetrAlps/2_DATA/ski_resorts_visitor_numbers"))
        {
          resort_files <- list.files(path = folderpath,pattern = "^[^~$].*\\.xlsx$", full.names = TRUE) # function to exclude files that start with ~$ and keep those that finish with .xlsx
          
          list_dt <- list()
          
        for(file in seq_along(resort_files))
          {
          
          data <- tryCatch(
            {
            read.xlsx(xlsxFile = resort_files[file], sheet = sheetname)
            }, 
            error = function(e) 
              {
              message("Error reading file: ", resort_files[file], "\n", e)
              return(NULL)
              })
            
            names(data)[1] <- "Mois" ; names(data)[2] <- "Jour"
            data <- data %>% fill(Mois, .direction = "down") # `fill()` defaults to replacing missing data from top to bottom
            data <- data %>% replace(is.na(.), 0) # assuming all the NA = 0 visitors
            data <- data %>% mutate(Date = paste(Mois, Jour, sep = "-"))
            data$Date <- as.Date(data$Date)
            data <- data %>% dplyr::select(Date, everything()) %>% dplyr::select(-Mois) %>% dplyr::select(-Jour)
            data <- data %>% mutate(Total = rowSums(dplyr::select(., where(is.numeric)), na.rm = TRUE))
            
            list_dt[[file]] <- data
          }
          # Assuming `list_dt` contains your data frames
          merged_data <- bind_rows(list_dt)
          
          if(save == TRUE)
          {
            write.csv(merged_data, file = file.path(folderoutpath,"meribel_visitors.csv"), row.names = FALSE)
          }
            
          return(merged_data)
        }
        
        meribel_visitors <- meribel_formatting(save = TRUE)
        
        
        
        
        
        
        
        
        # Courchevel
        courchevel_formatting <- function(folderpath = file.path(base,"TetrAlps/1_RAW_DATA/human_traffic_in_ski_resorts/Courchevel_RM_Historique_des_passages_2017_22/"),sheetname = "Details", save = FALSE, folderoutpath = file.path(base,"TetrAlps/2_DATA/ski_resorts_visitor_numbers"))
        {
          resort_files <- list.files(path = folderpath,pattern = "^[^~$].*\\.xlsx$", full.names = TRUE) # function to exclude files that start with ~$ and keep those that finish with .xlsx
          list_dt <- list()
          
          for(file in seq_along(resort_files))
          {
            year <- substr(resort_files[file],nchar(resort_files[file])-8,nchar(resort_files[file])-5)
            data <- tryCatch(
              {
                read.xlsx(xlsxFile = resort_files[file], sheet = sheetname, startRow = 3)
              }, 
              error = function(e) 
              {
                message("Error reading file: ", resort_files[file], "\n", e)
                return(NULL)
              })
            
            names(data)[1] <- "Jour" ; names(data)[2] <- "Date"
            data <- data %>% filter(Jour %in% c("lun.","mar.","mer.","jeu.","ven.","sam.","dim."))
            data <- data %>% replace(is.na(.), 0) # assuming all the NA = 0 visitors
            
            data$Date <- excel_numeric_to_date(as.numeric(data$Date))

            
            # correct the year 
            for(i in 1:nrow(data))
            {
              if(lubridate::month(data$Date[i]) < 11 && is.na(data$Date[i])==FALSE)
              {
                lubridate::year(data$Date[i]) <- as.numeric(year)
              }
             if(lubridate::month(data$Date[i]) > 10 && is.na(data$Date[i])==FALSE)
              {
                lubridate::year(data$Date[i]) <- as.numeric(year)-1
              }
              if(is.na(data$Date[i])==TRUE && lubridate::day(data$Date[i-1])==28 && lubridate::month(data$Date[i-1])==2)
              {
                data$Date[i] <- data$Date[i-1]+lubridate::days(1)
              }
            }
            
            data <- data %>% dplyr::select(.,-contains("otal"),-starts_with("X"))
            # Convert all columns to numeric except for "Jour" and "Date"
            data <- data %>% mutate(across(-c(Jour, Date), as.numeric))
            data <- data %>% mutate(Total = rowSums(dplyr::select(., where(is.numeric)), na.rm = TRUE))
            
            
            # View(data)
            list_dt[[file]] <- data
          }
          # Merging data frames
          merged_data <- bind_rows(list_dt)
          
          if(save == TRUE)
          {
            write.csv(merged_data, file = file.path(folderoutpath,"courchevel_visitors.csv"), row.names = FALSE)
          }
          
          return(merged_data)
        }
        
        courchevel_visitors <- courchevel_formatting(save = TRUE)
        
        
        
        
        # ValThorens/Orelle
        valtho_formatting <- function(folderpath = file.path(base,"TetrAlps/1_RAW_DATA/human_traffic_in_ski_resorts/ValThorens_Orelle_RM_Historique_des_passages_2017_22"), save = FALSE, folderoutpath = file.path(base,"TetrAlps/2_DATA/ski_resorts_visitor_numbers"))
        {
          resort_files <- paste0(folderpath,".xlsx") # function to exclude files that start with ~$ and keep those that finish with .xlsx
          list_dt <- list()
          
          for(year in c(2017:2019,2021:2022))
          {
            data <- tryCatch(
              {
                read.xlsx(xlsxFile = resort_files, sheet = as.character(paste0(year,"-",year+1-2000)), startRow = 3)
              }, 
              error = function(e) 
              {
                message("Error reading file: ", resort_files, "\n", e)
                return(NULL)
              })
            
            data <- data[!grepl("otal", data[, 1]), ]
            data <- data[,-c(1,3)]
            
            data <- as.data.frame(t(data))
            
            lift_names <- data[1,]
            colnames(data) <- c("Date",lift_names[2:length(lift_names)])
            data <- data[-1,]
            
            data <- data %>% replace(is.na(.), 0) # assuming all the NA = 0 visitors
            data$Date <- excel_numeric_to_date(as.numeric(data$Date))
            
            # Convert all columns to numeric except for "Jour" and "Date"
            data <- data %>% mutate(across(-c(Date), as.numeric))
            data <- data %>% mutate(Total = rowSums(dplyr::select(., where(is.numeric)), na.rm = TRUE))
            
            # View(data)
            list_dt[[year]] <- data
          }
          # Merging data frames
          merged_data <- bind_rows(list_dt)

          if(save == TRUE)
          {
            write.csv(merged_data, file = file.path(folderoutpath,"valtho_visitors.csv"), row.names = FALSE)
          }
          
          return(merged_data)
        }
        
        valtho_visitors <- valtho_formatting(save = TRUE)
        
        
        # Les Ménuires
        menuires_formatting <- function(folderpath = file.path(base,"TetrAlps/1_RAW_DATA/human_traffic_in_ski_resorts/Les_Menuires_RM_Historique_des_passages_2017_22"), save = FALSE, folderoutpath = file.path(base,"TetrAlps/2_DATA/ski_resorts_visitor_numbers"))
        {
          resort_files <- paste0(folderpath,".xlsx") # function to exclude files that start with ~$ and keep those that finish with .xlsx
          list_dt <- list()
          
          for(year in c(2017:2019,2021:2023))
          {
            data <- tryCatch(
              {
                read.xlsx(xlsxFile = resort_files, sheet = as.character(paste0(year,"-",year+1)), startRow = 3)
              }, 
              error = function(e) 
              {
                message("Error reading file: ", resort_files, "\n", e)
                return(NULL)
              })
            
            View(data)
            
            data <- data[!grepl("otal", data[, 1]), ]
            data <- data[,-c(1,3)]
            
            data <- as.data.frame(t(data))
            
            lift_names <- data[1,]
            colnames(data) <- c("Date",lift_names[2:length(lift_names)])
            data <- data[-1,]
            
            data <- data %>% replace(is.na(.), 0) # assuming all the NA = 0 visitors
            data$Date <- excel_numeric_to_date(as.numeric(data$Date))
            
            # Convert all columns to numeric except for "Jour" and "Date"
            data <- data %>% mutate(across(-c(Date), as.numeric))
            data <- data %>% mutate(Total = rowSums(dplyr::select(., where(is.numeric)), na.rm = TRUE))
            
            View(data)
            list_dt[[year]] <- data
          }
          # Merging data frames
          merged_data <- bind_rows(list_dt)
          
          if(save == TRUE)
          {
            write.csv(merged_data, file = file.path(folderoutpath,"valtho_visitors.csv"), row.names = FALSE)
          }
          
          return(merged_data)
        }
        
        menuires_visitors <- menuires_formatting(save = FALSE)

        
        
        
        
        
        
        
        
        
### Snow depth    
  # Méribel      
meribel_snow_formatting <- function(folderpath = file.path(base,"TetrAlps/1_RAW_DATA/environment/enneigement/meribel_meteo_france_neige.xlsx"),
                                    folderoutpath = file.path(base,"TetrAlps/2_DATA/snow_depth"), save = FALSE)
{
  list_data <- list()
  
  for(year in c(2016:2022))
  {
    data <- tryCatch(
      {
        read.xlsx(xlsxFile = folderpath, sheet = as.character(paste0("Saison ",year,".",year+1)))
      }, 
      error = function(e) 
      {
        message("Error reading file: ", folderpath, "\n", e)
        return(NULL)
      })
    colnames(data) <- c( "Date", "cumul.H.neige.cm", "Neige.fraiche.cm", "Cumul.neige.fraiche.avant.saison.cm")
    # Fill NA values in the Date column with the value above
    data <- data %>% fill(Date, .direction = "down")
    data$Date <- excel_numeric_to_date(data$Date)
    
    list_data[[year-2015]] <- data
  }
  
  snow_data <- do.call(rbind, list_data)
  
  if(save == TRUE)
  {
    write.csv(snow_data, file = file.path(folderoutpath,"meribel_snow_depth.csv"), row.names = FALSE)
  }
  
    return(snow_data)
    
}

snow_meribel <- meribel_snow_formatting(save = TRUE)

  # Courchevel

courchevel_snow_formatting <- function(folderpath = file.path(base,"TetrAlps/1_RAW_DATA/environment/enneigement/courchevel1850_hauteurdeneige_journaliere_2019_2023_13072023.xlsx"),
                                       folderoutpath = file.path(base,"TetrAlps/2_DATA/snow_depth"), save = FALSE)
{
  list_data <- list()
  
  for(year in c(2019:2022))
  {
    data <- tryCatch(
      {
        read.xlsx(xlsxFile = folderpath, sheet = as.character(paste0("cumul ",year-2000,"-",year+1-2000)), startRow = 2)
      }, 
      error = function(e) 
      {
        message("Error reading file: ", folderpath, "\n", e)
        return(NULL)
      })
    data <- data[,1:4]
    colnames(data) <- c( "Date", "cumul.H.neige.cm", "Neige.fraiche.cm", "moyenne")
    # Fill NA values in the Date column with the value above
    data$Date <- excel_numeric_to_date(as.numeric(data$Date))
    
    list_data[[year-2018]] <- data
  }
  
  snow_data <- do.call(rbind, list_data)
  
  if(save == TRUE)
  {
    write.csv(snow_data, file = file.path(folderoutpath,"courchevel_snow_depth.csv"), row.names = FALSE)
  }
  
  return(snow_data)
  
}

snow_courchevel <- courchevel_snow_formatting(save = TRUE)
#********************************************************************




### 1.1_Create an environment stack for predictors ----
#********************************************************************

# Changing names
mnt_9_squared <- mnt_9^2
names(mnt_9_squared) <- "squared_elevation"
names(mnt_9) <- "elevation"
names(strava) <- "strava"
names(slope_3V) <- "slope"
names(carto_habitats_3V_winter) <- "carto_habitats_winter"
names(r_leks_dist) <- "leks"

#' stacking it all in an env layer 
envir_stack <- c(mnt_9,mnt_9_squared,strava,slope_3V,carto_habitats_3V_winter,r_leks_dist)

env_RL_list <- lapply(envir_stack,raster::raster)
names(env_RL_list) <- names(envir_stack)

# Set the Lambert crs (EPSG:2154) to each raster element in env_RL_list
env_RL_list <- lapply(env_RL_list, function(x) {
  crs(x) <- "EPSG:2154"
  return(x)
})


save(env_RL_list,file=file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/env_RL_list.RData"))
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/env_RL_list.RData"))
#********************************************************************





### 1.2_Scale the environment stack ----
#********************************************************************
# Scaling of the rasters : needed for rsf with interaction in ctmm

# Users are responsible for standardizing rasters when interactions are supplied!!!!!!
# scaling raster for interactions : 
# Scale all rasters in the list using the scale function
scaled_env_RL_list <- lapply(env_RL_list, function(raster) {
  # Check if the raster is categorical (e.g., by name or checking levels)
  # Here, we assume the categorical raster has "carto" in its name
  if (grepl("carto_habitats_winter", names(raster)) || grepl("leks", names(raster))) {
    return(raster)  # Return the categorical raster unchanged
  } else {
    # If it's not categorical, scale the values
    scaled_values <- scale(values(raster), center = TRUE, scale = TRUE)
    raster <- setValues(raster, scaled_values)  # Replace raster values with scaled values
    return(raster)  # Return the scaled raster
  }
})



#'Dealing with categorical raster 

# dummy method : we can create as many dummy variables as the categorical raster has classes. dummy variable = takes a binary value (0 or 1) 
# Dummy variables are commonly used in regression analysis to represent categorical variables that have more than two levels, such as education level or occupation. 
# In this case, multiple dummy variables would be created to represent each level of the variable, and only one dummy variable would take on a value of 1 for each observation.


# Assume scaled_env_RL_list[["carto_habitats_winter"]] is your raster with 5 classes
carto_habitats_winter_bin <- scaled_env_RL_list[["carto_habitats_winter"]]

# Identify the unique classes in the raster
classes <- unique(values(carto_habitats_winter_bin))

# Initialize a list to store the binary rasters
carto_habitats_winter_bins <- list()

# Loop to create a binary raster for each class
for (class in classes[!is.nan(classes)]) {
  
  # Create a binary raster where pixels of the specified class are 1, others are 0
  carto_habitats_winter_binary <- calc(carto_habitats_winter_bin, fun = function(x) { as.integer(x == class) })
  
  # Assign a meaningful name based on the class
  new_name <- case_when(
    class == 1 ~ "Soils_low_vegetation",
    class == 2 ~ "Shrubs",
    class == 3 ~ "Trees",
    class == 4 ~ "Buildings",
    class == 5 ~ "Cliffs_water"
  )
  
  # Set the name of the binary raster
  names(carto_habitats_winter_binary) <- new_name
  
  # Add the binary raster to the list
  carto_habitats_winter_bins[[new_name]] <- carto_habitats_winter_binary
}


# Suppress carto_habitats_winter and replace it by multiple binary raster
scaled_env_RL_list <- scaled_env_RL_list[!names(scaled_env_RL_list) %in% "carto_habitats_winter"] # supress the unsclaled raster
scaled_env_RL_list <- c(scaled_env_RL_list, carto_habitats_winter_bins[!names(carto_habitats_winter_bins) %in% c("Soils_low_vegetation")]) # supress the raster "Soils_low_vegetation" = reference class : by default, all 0 common of the 4 other raster are consider Soils_low_vegetation  


# cat 1) Soils and low vegetation : Unclassified soil, Fine mineral soil, Coarse mineral soil, Dry or rocky grassland, Herbaceous, Low ligneous
# cat 2) Shrubs
# cat 3) Trees : Unclassified trees, Deciduous trees, Resinous trees
# cat 4) Buildings
# cat 5) Cliffs and water : Cliff, Natural pond, Artificial pond, Waterway, Unclassified


save(scaled_env_RL_list,file=file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/scaled_env_RL_list.RData"))
load(file.path(base,"TetrAlps/3_R/0_Heavy_saved_models/environment_3V/scaled_env_RL_list.RData"))
#********************************************************************








### 2_Predictors' collinearity ----
#********************************************************************
# Correlation between layers


# 2.1_Collinearity between two continuous rasters, calculation of Pearson coefficient 

# https://statnmap.com/2018-01-27-spatial-correlation-between-rasters/ 
cor(values(env_RL_list[["strava"]]),
    values(env_RL_list[["slope"]]),
    use = "na.or.complete", method = "pearson")
# Pearson's coefficient is an index reflecting a linear relationship between two continuous variables.
# Kendall's or Spearman's coefficients recommended if the data do not necessarily come from a bivariate normal distribution. 
# Kendall's or Spearman's for ordinal variables (= categorical but hierarchical!)


# 2.2_Collinearity between two continuous rasters, calculation of a lm model (sqrt(R-squared) equivalent to Pearson coefficient) 

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



# 2.3_Collinearity between a continuous raster and a factorial raster

aov1<-aov(values(env_RL_list[["strava"]]) ~ values(env_RL_list[["carto_habitats_winter"]])==1 )
summary(aov1)

tidy_aov1 <- tidy(aov1)
R <- sqrt(tidy_aov1$sumsq[1] / (tidy_aov1$sumsq[1] + tidy_aov1$sumsq[2])) # R-square = Sum Sq(values) / (Total Sum of squares (sq) = Sum Sq(values)+Sum Sq(residuals))
# here, R is equivalent to the Pearson coefficient
#********************************************************************



### 3_Crop the environment stack around the bird of interest ----
#********************************************************************
#' cropping the stack environment to the extent of the bird data locations *2
env_RL_list_cropped <- lapply(env_RL_list, function(raster) {
  terra::crop(raster, extent(e_mybird)*2)
})
#********************************************************************



### 4_Predictors visualization ----
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
  geom_spatraster(data=strava)+
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





