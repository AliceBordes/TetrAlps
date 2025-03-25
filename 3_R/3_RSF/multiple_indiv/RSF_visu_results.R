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
library(tibble)
library(ctmm)
library(adehabitatHR)
library(sjmisc)
library(raster)
library(parallel)
library(meta)
library(doParallel)
library(rempsyc)
library(mgcv)
# devtools::install_github("stefanocoretta/tidymv")
library(tidymv)
detectCores()

base <- "C:/Users/albordes/Documents/PhD/TetrAlps"
#********************************************************************

### Loading functions ----
#********************************************************************
source(file.path(base,"4_FUNCTIONS","my_telemetry_transfo_data.R"))
source(file.path(base,"4_FUNCTIONS","Homerange_visu/mean_size_area.R"))
source(file.path(base,"4_FUNCTIONS","Homerange_visu/visu_home_range.R"))
source(file.path(base,"4_FUNCTIONS","Homerange_visu","distance_home_range_capture_site.R"))
source(file.path(base,"4_FUNCTIONS","Homerange_visu","multi_graph_home_range.R"))
source(file.path(base,"4_FUNCTIONS","RSF","plot_check_RSF_results.R"))
source(file.path(base,"4_FUNCTIONS","RSF","rsf_functions.R"))
source(file.path(base,"4_FUNCTIONS","Formatting_data/formatting_environment_data.R"))
#********************************************************************


# Loading data ----
#********************************************************************
### DATASET
birds_bg_dt<-read.csv2(file.path(base,"2_DATA","data_bg_pretelemetry_2024_10.csv"),sep=",") #upload the file from a csv, not a move2 object
resort_bird <- read.csv2(file.path(base,"2_DATA","ski_resorts_visitor_numbers","bg_winter_assign_valley_resort.csv"))

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
# load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","scaled_env_RL_list_10m.RData"))
#********************************************************************


#********************************************************************
# Load the outputs of tele_akde with visitor number as continuous variable
load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", "multipl_telemetry_winter_saison2_2025_01_23.Rdata"))
load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", "multipl_akde_winter_saison2_2025_01_23.Rdata"))

l_telemetry_winter <- list_of_one(l_telemetry_winter)
l_akde_winter <- list_of_one(l_akde_winter)

l_telemetry_winter <- covariates_NAN_cleaned(l_telemetry_winter)

l_akde_winter <- l_akde_winter[names(l_akde_winter) %in% names(l_telemetry_winter)]
#********************************************************************  


### Settings ----
#********************************************************************
covid <- c("Caramel_2", "Daisy","Dalton","Dameur","Dario","Darkvador","Darwin","Dede","Destroy","Diot","Djal","Django","Donald","Durite","Dynamite","Dyonisos")
females <- unique((birds_bg_dt %>% filter(animal.sex == "femelle"))$animal.ID)
males <- unique((birds_bg_dt %>% filter(animal.sex == "male"))$animal.ID)

model <- "rsf_59birds_individual_2025_03_21_20h57min"
#********************************************************************



# Loading model data ----
#********************************************************************
load(file = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model, paste0(model, ".Rdata")))
#********************************************************************

### 1_AIC calculation ----
#********************************************************************
# AICc 
# (= AIC correction for small sample sizes)
# use of AICc avoid the probability of preferentially selecting a model with too many parameters

AIC_summary <- summary(sum_rsf_multipl)[,1]
cat("Sum of the AICc\n", sum(AIC_summary[is.finite(AIC_summary)]))

# Save the AICc 
clean_summary_df <- enframe(AIC_summary, name = "RSF_Bird", value = "AIC")
AIC_table <- bind_rows(clean_summary_df, data.frame(RSF_Bird = "Total_sum", AIC = sum(AIC_summary[is.finite(AIC_summary)])))
write.csv(AIC_table, file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model,"AIC_table.csv"))




# Call the AICc of all the models 
model_file_l <- list.files(file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results"), pattern = "min$")

list_AICt <- list()
for(f in seq_along(model_file_l))
{
  if(file.exists(file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model_file_l[f],"AIC_table.csv")))
  {
    list_AICt[[model_file_l[f]]] <- read.table(file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model_file_l[f],"AIC_table.csv"), sep = ",", header = TRUE, col.names = c("RSF_Bird", "AIC"))
  }
}

df_AIC <- do.call(rbind, Map(cbind, Model = names(list_AICt), list_AICt))

df_AIC <- df_AIC %>%
  arrange(Model, AIC) %>%  # Sort AIC within each Model
  mutate(RSF_Bird = factor(RSF_Bird, levels = unique(RSF_Bird)))  # Reorder factor levels


ggplot(df_AIC, aes(y = log(AIC), x = RSF_Bird)) +
  geom_point(aes(color = Model)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "AIC Comparison for Each Bird", x = "Bird", y = "AIC (log scale")

#********************************************************************




### 2_Vizualising RSF results for multiple birds ----
#********************************************************************

### 2.1_Formatting RSF results for multiple birds ----
#********************************************************************

# switch variable names (more readible)
# sum_rsf_multipl <- lapply(sum_rsf_multipl, function(x) {
#   x@.Data[[22]] <- sapply(x@.Data[[22]], swap_temporal_variable) # sapply preserve the original vector format VS sapply returns a list
#   return(x)  # Return modified object
# })


l_dt_results <- rsf_result_table(sum_rsf_multipl, 
                                 coefficient = 5)
  # nb of outliers : sum(is.na(l_dt_results[[2]]$est))
  

# Supress Fiasco estimation for Buildings (outlier) 
# rsf_results_table[rsf_results_table$covariates == "Buildings" & rsf_results_table$bird == "Fiasco", "est"] <- NA
# rsf_results_table[rsf_results_table$covariates == "Buildings" & rsf_results_table$bird == "Dynamite_2", "est"] <- NA
# rsf_results_table[rsf_results_table$covariates == "Cliffs" & rsf_results_table$bird == "Dameur", "est"] <- NA
#********************************************************************  
  



#********************************************************************
rsf_meta_1 <- metamodel(sum_rsf_multipl,
                        remove_outliers = FALSE,
                        outputfolder = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model))

rsf_meta_1 <- metamodel(sum_rsf_multipl,
                        coefficient = 1.5, # 5 fois l'écrat interquartile (1er et 3eme)
                        remove_outliers = TRUE,
                        outputfolder = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model))

rsf_meta_group <- metamodel(sum_rsf_multipl,
                        group = "covid",
                        remove_outliers = FALSE,
                        outputfolder = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model))

rsf_meta_group <- metamodel(sum_rsf_multipl,
                            coefficient = 1.5,
                            group = "covid",
                            remove_outliers = TRUE,
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



### 2.2_Vizualising all effets (1 point = 1 indiv) ----
#********************************************************************

points_plot_rsf(l_dt_results,
                group = "none",
                meta_data = rsf_meta_1,
                easy_look_results = TRUE,
                boxplot_by_group = FALSE,
                list_excluded_covariates = c("elevation", "squared_elevation"))
points_plot_rsf(l_dt_results,
                group = "covid",
                meta_data = rsf_meta_1,
                boxplot_by_group = FALSE,
                easy_look_results = TRUE,
                list_excluded_covariates = c("elevation", "squared_elevation", "Buildings"))
points_plot_rsf(l_dt_results,
                group = "covid",
                meta_data = rsf_meta_1,
                easy_look_results = TRUE,
                boxplot_by_group = TRUE,
                list_excluded_covariates = c("elevation", "squared_elevation", "Buildings"))
points_plot_rsf(l_dt_results,
                group = "sex",
                meta_data = rsf_meta_1,
                boxplot_by_group = FALSE,
                list_excluded_covariates = c("elevation", "squared_elevation", "Buildings"))


#********************************************************************



# Are the bird with the higher preference for cliffs avoiding a lot strava? 
#********************************************************************

rsf_results_corrected <- points_plot_rsf(l_dt_results,
                group = "none",
                meta_data = rsf_meta_1,
                easy_look_results = TRUE,
                boxplot_by_group = FALSE,
                list_excluded_covariates = c("elevation", "squared_elevation"))

cliffs <- rsf_results_corrected %>% filter(covariates == "Cliffs:day")
strava <- rsf_results_corrected %>% filter(covariates == "strava_winter_sports:day")

# Faire le join et renommer les colonnes d'estimation
c_s <- left_join(cliffs, strava, by = "bird" )

c_s <- left_join(c_s, resort_bird, by = c("bird"="animal") )

c_s <- c_s %>% filter(!bird %in% covid)

c_s <- c_s %>% filter(resort == "Les Ménuires")

p <- ggplot(c_s, aes(x = est.x, y = est.y, color = bird, text = bird)) +
  geom_point() +
  xlab(unique(c_s$covariates.x)) +
  ylab(unique(c_s$covariates.y)) +
  theme_minimal()
p
# Convertir en graphique interactif
ggplotly(p, tooltip = "text")


gam_m <- gam(est.y~s(est.x), data = c_s)
summary(gam_m)
plot.gam(gam_m)

glm_m <- glm(est.y~est.x, data = c_s)
summary(glm_m)
plot(c_s$est.y~c_s$est.x, 
     xlab = unique(c_s$covariates.x), 
     ylab = unique(c_s$covariates.y), 
     main = "GLM: Estimated strava value ~ estimated cliff value \n(from RSF model outputs)", 
     col = as.factor(c_s$bird), pch = 19)

# Ajouter la droite de régression
abline(glm_m, col = "red", lwd = 2)




smooth_plot <- plot_smooths(gam_m, "est.x")+  
  geom_line(color = "red", size = 1) 

smooth_plot +
  geom_abline(slope = coef(glm_m)["est.x"], intercept = coef(glm_m)["(Intercept)"], color = "blue", size = 1) +
  geom_point(data = c_s, aes(x = est.x, y = est.y, color = bird), size = 2)+
  labs(title = "Estimated strava value ~ estimated cliff value, from RSF model outputs (day)",
       subtitle = "GLM (blue) and GAM (red)",
       x = unique(c_s$covariates.x),
       y = unique(c_s$covariates.y))
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









