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

base <- "C:/Users/albordes/Documents/PhD/TetrAlps"
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

# Environment stack
load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","scaled_env_RL_list.RData"))
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


### Settings ----
#********************************************************************
covid <- c("Caramel_2", "Daisy","Dalton","Dameur","Dario","Darkvador","Darwin","Dede","Destroy","Diot","Djal","Django","Donald","Durite","Dynamite","Dyonisos")
females <- unique((birds_bg_dt %>% filter(animal.sex == "femelle"))$animal.ID)
males <- unique((birds_bg_dt %>% filter(animal.sex == "male"))$animal.ID)

model <- "rsf_59birds_individual_2025_02_18_17h23min"
#********************************************************************



# Loading model data ----
#********************************************************************
load(file = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model, paste0(model, ".Rdata")))
#********************************************************************


### 1_Vizualising RSF results for multiple birds ----
#********************************************************************

### 1.1_Formatting RSF results for multiple birds ----
#********************************************************************
l_dt_results <- rsf_result_table(sum_rsf_multipl, coefficient = 5)
  # nb of outliers : sum(is.na(l_dt_results[[2]]$est))
  

# Supress Fiasco estimation for Buildings (outlier) 
# rsf_results_table[rsf_results_table$covariates == "Buildings" & rsf_results_table$bird == "Fiasco", "est"] <- NA
# rsf_results_table[rsf_results_table$covariates == "Buildings" & rsf_results_table$bird == "Dynamite_2", "est"] <- NA
# rsf_results_table[rsf_results_table$covariates == "Cliffs" & rsf_results_table$bird == "Dameur", "est"] <- NA

#********************************************************************  
  



#********************************************************************
rsf_meta_1 <- metamodel(sum_rsf_multipl,
                      coefficient = 5,
                      remove_outliers = FALSE,
                      outputfolder = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model))

rsf_meta_group <- metamodel(sum_rsf_multipl,
                        coefficient = 5,
                        group = "covid",
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
                group = "none",
                meta_data = rsf_meta_1,
                boxplot_by_group = FALSE,
                list_excluded_covariables = c("elevation", "squared_elevation"))
points_plot_rsf(l_dt_results,
                group = "covid",
                meta_data = rsf_meta_group,
                boxplot_by_group = FALSE,
                list_excluded_covariables = c("elevation", "squared_elevation", "Buildings"))
points_plot_rsf(l_dt_results,
                group = "covid",
                meta_data = rsf_meta_group,
                boxplot_by_group = TRUE,
                list_excluded_covariables = c("elevation", "squared_elevation", "Buildings"))
points_plot_rsf(l_dt_results,
                group = "sex",
                meta_data = rsf_meta_group,
                boxplot_by_group = FALSE,
                list_excluded_covariables = c("elevation", "squared_elevation", "Buildings"))


#********************************************************************



# 2_Make sure you get all the information from the model: are there any unexplained patterns? 
# rsf function from Björn
#********************************************************************
# gam(case underground y ~ s(x, y), offset = log(modeloutput), family = poisson())
# 3 var explicatives : x, y, modeloutput from rsf gam de Björn

# An offset is a term to be added to a linear predictor, such as in a generalised linear model, with known coefficient 1 rather than an estimated coefficient.


#' # Traditional RSF with downweighted Poisson regression
#' Functions to generate quadrature points and predict with the model
rsf_points <- function(x, UD, R = NULL, n = 1e5, k = 1e6, type = "Riemann", 
                       rmax =6*sqrt(UD@CTMM$sigma[1,1]),
                       interpolation = FALSE) {
  # Samples background points from a 2D normal distribution fitted to the relocation data, and extracts environmental
  # information from a raster object "R"
  # x: telemetry object
  # UD: UD object
  # R: raster* object
  # n: number of background points to sample
  # k: weight of presence points
  # rmax: maximum distance for Riemann-type integration
  # interpolation: do interpolation when sampling the grid
  # When type 0´= "MonteCarlo", importance sampling is done
  stopifnot(UD@CTMM$isotropic)
  stopifnot(type %in% c("Riemann", "MonteCarlo"))
  if (type == "Riemann") {
    quadrature_pts <- getValues(R)
    xy <- as.data.frame(xyFromCell(R, 1:ncell(R)))
    xy <- sf::st_as_sf(xy, coords = c("x", "y"), crs = crs(R))
    xy <- st_transform(xy, CRS(UD@info$projection))
    xy <- st_coordinates(xy)
    r <- sqrt(((xy[,1] - UD@CTMM$mu[1]))^2 + ((xy[,2] - UD@CTMM$mu[2]))^2)
    bg <- data.frame(case_ = 0,
                     x_ = xy[r<rmax,1], y_ = xy[r<rmax,2],  w_ = prod(res(R)), k_ = k)
    bg <- cbind(bg, quadrature_pts[r<rmax,])
    bg <- sf::st_as_sf(bg, coords = c("x_", "y_"), crs = CRS(UD@info$projection))
    xx <- data.frame(case_ = 1, x_ = x$x, y_ = x$y,
                     w_ = 1/k * UD$weights * mean(UD$DOF.area), k_ = k)
    xx <- sf::st_as_sf(xx, coords = c("x_", "y_"), crs = CRS(UD@info$projection))
    xx[names(R)] <- as.data.frame(raster::extract(R, st_transform(xx, crs(R)), method = ifelse(interpolation, "bilinear", "simple")))
    xx <- rbind(bg, xx)
  } else {
    quadrature_pts <- MASS::mvrnorm(n, mu = UD@CTMM$mu, Sigma = UD@CTMM$sigma)
    xx <- data.frame(case_ = 0, x_ = quadrature_pts[, 1], y_ = quadrature_pts[, 2], w_ = UD@CTMM$sigma[1,1]/n, k_ = k)
    xx <- rbind(xx, data.frame(case_ = 1, x_ = x$x, y_ = x$y,
                               w_ = 1/k * UD$weights * mean(UD$DOF.area), k_ = k 
    ))
    xx <- sf::st_as_sf(xx, coords = c("x_", "y_"), crs = CRS(UD@info$projection))
    xx[names(R)] <- as.data.frame(raster::extract(R, st_transform(xx, crs(R)), method = ifelse(interpolation, "bilinear", "simple")))
  }
  xy <- st_coordinates(xx)
  colnames(xy) <- c("x_", "y_")
  sd <- sqrt(UD@CTMM$sigma[1,1])
  xy[,1] <- (xy[,1] - UD@CTMM$mu[1])/sd
  xy[,2] <- (xy[,2] - UD@CTMM$mu[2])/sd
  xx <- cbind(xy, xx)
  xx
}

print(summary(sum_rsf_multipl[[1]]))


#' ## A minimal "classic" example
#' Generate quadrature points ("background points")
set.seed(2)
rsf_Abel_df <- rsf_points(l_telemetry_winter[[1]][[1]], 
                           l_akde_winter[[1]][[1]], 
                            scaled_env_RL_list[[1]], # one raster only use for settings
                           # scaled_env_RL_list[!names(scaled_env_RL_list) %in% c("Buildings", "leks", "slope", "strava_backcountry")], 
                           interpolation = TRUE)

#' Fit a downweighted Poisson regression
m_rsf_Abel <- gam(case_*k_ ~ x_ + y_ + I(-(x_^2 + y_^2)/2) + 
                    summary(sum_rsf_multipl[[1]])$CI["Trees (1/Trees)","est"],
                   family = poisson(), data= rsf_Abel_df, weights = w_)

summary(m_rsf_Abel)



# rsf_points(x = l_telemetry_winter[[1]][[1]],
#            UD = l_akde_winter[[1]][[1]],
#            R = log(5.4878193*scaled_env_RL_list[["Trees"]]))

#********************************************************************

# # Pseudo-absence points
# quadrature_pts <- MASS::mvrnorm(n, mu = UD@CTMM$mu, Sigma = UD@CTMM$sigma) # distribution = loi normale autour du centre du homerange
# xx <- data.frame(case_ = 0, x_ = quadrature_pts[, 1], y_ = quadrature_pts[, 2], w_ = UD@CTMM$sigma[1,1]/n, k_ = k) # longitude et latitude des points créé stockés dans dataframe 
# # chaque point a un poids = spatial covariance / le nombre de points générés n --> que l'on divise par le nombre de points total (présence+absence) k *
# xx <- rbind(xx, data.frame(case_ = 1, x_ = x$x, y_ = x$y,
#                            w_ = 1/k * UD$weights * mean(UD$DOF.area), k_ = k 
# ))
# xx <- sf::st_as_sf(xx, coords = c("x_", "y_"), crs = CRS(UD@info$projection))
# xx[names(R)] <- as.data.frame(raster::extract(R, st_transform(xx, crs(R)), method = ifelse(interpolation, "bilinear", "simple")))
# 
# # Bird locations
# xy <- st_coordinates(xx)
# colnames(xy) <- c("x_", "y_")
# sd <- sqrt(UD@CTMM$sigma[1,1])
# xy[,1] <- (xy[,1] - UD@CTMM$mu[1])/sd
# xy[,2] <- (xy[,2] - UD@CTMM$mu[2])/sd
# xx <- cbind(xy, xx)
# xx









