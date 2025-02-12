## PhD Tetralpes project ##

# Alice Bordes #

# January 2025 #

# Description:

# RSF results



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
detectCores()

base <- "C:/Users/albordes/Documents/PhD"
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


# Loading data ----
#********************************************************************
### DATASET
birds_bg_dt<-read.csv2(file.path(base,"2_DATA","data_bg_pretelemetry_2024_10.csv"),sep=",") #upload the file from a csv, not a move2 object

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
#********************************************************************


### Settings ----
#********************************************************************
covid <- c("Caramel_2", "Daisy","Dalton","Dameur","Dario","Darkvador","Darwin","Dede","Destroy","Diot","Djal","Django","Donald","Durite","Dynamite","Dyonisos")
females <- unique((birds_bg_dt %>% filter(animal.sex == "femelle"))$animal.ID)
males <- unique((birds_bg_dt %>% filter(animal.sex == "male"))$animal.ID)

model <- "rsf_59birds_individual_2025_01_30_06h06min"
#********************************************************************



# Loading model data ----
#********************************************************************
load(file = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model, paste0(model, ".Rdata")))
#********************************************************************


### 1_Vizualising RSF results for multiple birds ----
#********************************************************************

### 1.1_Formatting RSF results for multiple birds ----
#********************************************************************
l_dt_results <- rsf_result_table(sum_rsf_multipl[!names(sum_rsf_multipl)%in%covid], coefficient = 5)
  # nb of outliers : sum(is.na(l_dt_results[[2]]$est))
  

# Supress Fiasco estimation for Buildings (outlier) 
# rsf_results_table[rsf_results_table$covariates == "Buildings" & rsf_results_table$bird == "Fiasco", "est"] <- NA
# rsf_results_table[rsf_results_table$covariates == "Buildings" & rsf_results_table$bird == "Dynamite_2", "est"] <- NA
# rsf_results_table[rsf_results_table$covariates == "Cliffs" & rsf_results_table$bird == "Dameur", "est"] <- NA

#********************************************************************  
  



#********************************************************************
rsf_meta <- metamodel(sum_rsf_multipl[!names(sum_rsf_multipl)%in%covid],
                      coefficient = 5,
                      group = NULL,
                      remove_outliers = FALSE,
                      outputfolder = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model))

# Interpreting metagen() outputs 

# 2 estimates : 
    # Using a Common Effect Model
    # - assumes the only variation is due to the sampling error 
    # - assumes the studies (here individuals) with less variance have more weight
    # ISSUE : relevant if the variance of all studies (individuals) are similar
    # 
    # Using a Random Effects Model (ESTIMATE WE KEPT)
    # MORE RELEVANT FOR IF STRONG HETEROGENEITY OF THE VARIANCE OF ALL STUDIES (INDIVIDUALS)
#********************************************************************



### 1.2_Vizualising all effets (1 point = 1 indiv) ----
#********************************************************************

points_plot_rsf(l_dt_results,
                group = "covid",
                meta_data = rsf_meta,
                boxplot_by_group = FALSE,
                list_excluded_covariables = c("elevation", "squared_elevation", "Buildings"))
points_plot_rsf(l_dt_results,
                group = "covid",
                meta_data = rsf_meta,
                boxplot_by_group = TRUE,
                list_excluded_covariables = c("elevation", "squared_elevation", "Buildings"))
points_plot_rsf(l_dt_results,
                group = "sex",
                meta_data = rsf_meta,
                boxplot_by_group = FALSE,
                list_excluded_covariables = c("elevation", "squared_elevation", "Buildings"))
points_plot_rsf(l_dt_results,
                group = "none",
                meta_data = rsf_meta,
                boxplot_by_group = FALSE,
                list_excluded_covariables = c("elevation", "squared_elevation"))

#********************************************************************











