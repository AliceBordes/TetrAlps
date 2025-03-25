#### PhD TetraAlps project ####

# Alice Bordes #

# September 2024 #

# Description:

# Analysing the impact of the choice of the integration area size for HSFs



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

bird = "Abel"
#********************************************************************

# Loading data ----
#********************************************************************
# Environment stack
# load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","env_RL_list_10m.RData"))
load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","scaled_env_RL_list_10m.RData"))
# load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","scaled_env_RL_list_1m.RData"))
# load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","scaled_env_RL_list_modal.RData"))
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


### 2.2_Data Loading of telemetry, guess, fit and akde objects for rsf ----
#********************************************************************
# Load the outputs of tele_akde with visitor number as continuous variable
load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", paste0(telemetry_file,".Rdata")))
load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", paste0(akde_file,".Rdata")))

l_telemetry_winter <- list_of_one(l_telemetry_winter)
l_akde_winter <- list_of_one(l_akde_winter)

l_telemetry_winter <- covariates_NAN_cleaned(l_telemetry_winter)

l_akde_winter <- l_akde_winter[names(l_akde_winter) %in% names(l_telemetry_winter)]
#********************************************************************  



### 3_Choosing environmental predictors ----
#********************************************************************
# Predictor selection 
scaled_env_RL_list_selection <-  scaled_env_RL_list[!(names(scaled_env_RL_list) %in% c("slope", "leks", "Buildings", "strava_backcountry"))]
names(scaled_env_RL_list_selection[["squared_elevation"]]) <- "squared_elevation"
  
# cropping the predictors to the size chosen for the integration area
buff_vector <- c(1,100,200,500,1000,10000) # different size for the integration area
#********************************************************************


### 4_Setting the limit of the study area for the bird ----
#********************************************************************
telemetry_winter = l_telemetry_winter[[bird]][[1]]
akde_winter = l_akde_winter[[bird]][[1]]


# calculating the 99% HR
r_mybird_akde_99 <- SpatialPolygonsDataFrame.UD(akde_winter,level.UD=.99,level=.95) # UD area at 99% with a 95% confidence level for the magnitude of the above area

# calculating the mcp 
subset_df <- telemetry_winter[, c("x", "y")]
class(subset_df) <- "data.frame"
coordinates(subset_df) <- ~x + y # Perform the Minimum Convex Polygon calculation
mcp_result <- mcp(subset_df, percent = 100)


max(ext(r_mybird_akde_99),ext(mcp_result))
min(ext(r_mybird_akde_99),ext(mcp_result))

e_mybird <- c(min(ext(r_mybird_akde_99),ext(mcp_result))[1],
              max(ext(r_mybird_akde_99),ext(mcp_result))[1],
              min(ext(r_mybird_akde_99),ext(mcp_result))[2],
              max(ext(r_mybird_akde_99),ext(mcp_result))[2])

# plot area
plot(e_mybird[1:2],e_mybird[3:4],type="n")
terra::plot(mcp_result,add=T) ; terra::plot(telemetry_winter,add=T) # plot results to check
terra::plot(r_mybird_akde_99,add=T, border="blue")
#********************************************************************



### 5_Visualization of the different buffers
#********************************************************************

# Creation of the polygon for the first integration area = max(mcp, akde99)
polygon <- as.polygons(ext(e_mybird))
crs(polygon) <- "EPSG:2154"

# Creation of the different integration area sizes (buffered polygon)
buffered_polygons <- lapply(buff_vector, function(width) {
  terra::buffer(polygon, width = width, joinstyle = "mitre")
})


# Transform mcp and akde 99% in polygon
mcp_sf <- st_as_sf(mcp_result)
st_crs(mcp_sf) <- 2154
mcp_result_WGS84 <- st_transform(mcp_sf, crs = 2154)

akde99_sf <- st_as_sf(r_mybird_akde_99)
st_crs(akde99_sf) <- 2154
akde99_WGS84 <- st_transform(akde99_sf, crs = 2154)


# Visualization of the different integration areas
ggplot() +
  lapply(buffered_polygons, function(p) geom_spatvector(data = p, fill = NA)) +
  geom_sf(data = mcp_result_WGS84, fill = NA, color = "red") +
  geom_sf(data = akde99_WGS84, fill = NA, color = "blue")
#********************************************************************





### 5_RSF functions --> estimation of the environmental effects ----
#********************************************************************
######################*
# using ctmm::rsf.fit()
######################*

rsf_summary <- list()

for(i in seq_along(buff_vector))
{
  
  buffered_polygon <- terra::buffer(polygon, width = buff_vector[i], joinstyle = "mitre") # joinstyle = "mitre" : to correctly represent the limit of the area # width = Unit is meter if x has a longitude/latitude CRS, or in the units of the coordinate reference system in other cases (typically also meter)
  # ggplot()+geom_sf(data=buffered_polygon)+geom_sf(data=polygon)
  
  e_mybird_buff <- as.vector(ext(buffered_polygon))
  
  
  ### Crop the environment stack around the bird of interest
  #' cropping the stack environment to the extent of the bird data locations *2
  scaled_env_RL_list_cropped <- lapply(scaled_env_RL_list_selection, function(raster) {
    terra::crop(raster, extent(e_mybird_buff)*2)
  })
  
  
  
  # RSF
  set.seed(3)
  mybird_rsf_riemann <- rsf.fit(telemetry_winter, akde_winter,  
                                R = scaled_env_RL_list_cropped,
                                integrator = "Riemann") #Riemann = faster option
  
  
  sum_rsf <- as.data.frame(summary(mybird_rsf_riemann)$CI, row.names = NULL)
  sum_rsf$covariates <- rownames(sum_rsf)
  sum_rsf$IA <- paste0("IA_",buff_vector[i],"_m")
  
  rsf_summary[[i]]<- sum_rsf
  
}



# bind the summary of the different area of integration
rsf_table <- do.call(rbind,rsf_summary) %>%
  mutate(
    covariates = sapply(strsplit(covariates, " "), `[[`, 1), # Split the string at spaces
    covariates = case_when(
      covariates == "area" ~ "area (km^2)",
      covariates == "τ[position]" ~ "τ[position] (days)",
      covariates == "diffusion" ~ "diffusion (ha/day)",
      TRUE ~ covariates # This ensures that any values in covariates that do not match the specified conditions remain unchanged.
    )
  )


# Réinitialiser les noms de ligne pour qu'ils soient consécutifs
rownames(rsf_table) <- NULL

# ordering factors for plotting
rsf_table$IA <- factor(rsf_table$IA, levels = c("IA_1_m"   ,  "IA_100_m" ,  "IA_200_m"  , "IA_500_m"  , "IA_1000_m",  "IA_10000_m"))






######################*
# using a glm()
######################*
#' # Traditional RSF with downweighted Poisson regression
#' Functions to generate quadrature points and predict with the model
rsf_points <- function(x, UD, R = NULL, n = 1e5, k = 1e6, type = "Riemann",
                       rmax = 6*sqrt(UD@CTMM$sigma[1,1]),
                       interpolation = FALSE) {
  # Samples background points from a 2D normal distribution fitted to the relocation data, and extracts environmental
  # information from a raster object "R"
  # x: telemetry object
  # UD: UD object
  # R: terra object
  # n: number of background points to sample
  # k: weight of presence points
  # rmax: maximum distance for Riemann-type integration
  # interpolation: do interpolation when sampling the grid
  # When type 0´= "MonteCarlo", importance sampling is done
  
  # Checking that the utilization distribution (UD) is isotropic and the type of sampling is either "Riemann" or "MonteCarlo". If not, it will raise an error;
  stopifnot(UD@CTMM$isotropic)
  stopifnot(type %in% c("Riemann", "MonteCarlo"))
  if (type == "Riemann") {
    quadrature_pts <- values(R) #raster::getValues(R)
    xy <- as.data.frame(xyFromCell(R, 1:ncell(R)))  #create a dataframe with the raster values
    xy <- sf::st_as_sf(xy, coords = c("x", "y"), crs = crs(R))  # transform the dataframe in a spatial sf object
    xy <- st_transform(xy, UD@info$projection)
    xy <- st_coordinates(xy)
    
    # Then, from each point of the raster, calculate the distance to the centre of the home range:   
    r <- sqrt(((xy[,1] - UD@CTMM$mu[1]))^2 + ((xy[,2] - UD@CTMM$mu[2]))^2)   # UD@CTMM$mu = coordinates xy of mu, the center of the home range
    
    
    # bg = "background" (pseudo-absence) points data frame
    # Creates a data frame bg for "background" points where the distance r is less than rmax. The column case_ = 0 indicates background points, w_ is a weight, and k_ is the weight of presence points;
    # rmax = 6*sqrt(UD@CTMM$sigma[1,1]) and sqrt(UD@CTMM$sigma[1,1]) is the standard deviation of the home range in that direction.
    # It acts as a cutoff to limit the background points to a region around the center of the home range. Points beyond this distance are excluded because they are unlikely to fall within the animal's range. 
    # By restricting the area in this way, rmax ensures that only the most relevant background points are used for analysis, reducing unnecessary noise in the model.
    # a lot of points do not satisfy the condition r < rmax : a lot of points have an r exeeding rmax beacause the raster area is much larger than the home range, 
    # so, only close points from the area are kept for the analysis 
    # w_ is the weight assigned to each point (presence or background) in the dataset. The weights ensure that the contribution of presence points and background points are appropriately balanced in the model, reflecting the actual probability of use by the animal.
    bg <- data.frame(case_ = 0,
                     x_ = xy[r<rmax,1], y_ = xy[r<rmax,2],  w_ = prod(res(R)), k_ = k) # prod(res(R)) = raster resolution x * raster resolution y (ex: 9m*9m = 81m)
    # For background points (case_ = 0), the weight is set to a constant k. This ensures that all background points have equal influence in the model.
    # w_ is based on the size of raster cells for background points
    
    bg <- cbind(bg, quadrature_pts[r<rmax,]) # lorsque r < rmax --> bg (coordinates of the raster nearby the HR <=> r<rmax) and quadrature_pts (values of the raster) dataframes are binded 
    bg <- sf::st_as_sf(bg, coords = c("x_", "y_"), crs = UD@info$projection)
    
    
    # xx = presence points data frame
    xx <- data.frame(case_ = 1, x_ = x$x, y_ = x$y,
                     w_ = 1/k * UD$weights * mean(UD$DOF.area), k_ = k)
    # For presence points (case_ = 1), the weight is scaled by 1/k to give the presence points less influence than the background points. However, the actual weight of presence points is adjusted further by the utilization distribution (UD) and degrees of freedom area (DOF) values.
    # The multiplication by UD$weights * mean(UD$DOF.area) accounts for the relative importance of different presence points based on the animal's movement and home range = take into account the autocorrelation between points.
    # w_ is based on the utilization distribution for presence points.
    
    xx <- sf::st_as_sf(xx, coords = c("x_", "y_"), crs = UD@info$projection)
    xx[names(R)] <- as.data.frame(extract(R, st_transform(xx, crs(R)), 
                                          ID = FALSE,
                                          method = ifelse(interpolation, "bilinear", "simple")))
    xx <- rbind(bg, xx)  # bind the background points and presence points (telemetry) dataframes
  } else {
    quadrature_pts <- MASS::mvrnorm(n, mu = UD@CTMM$mu, Sigma = UD@CTMM$sigma)
    xx <- data.frame(case_ = 0, x_ = quadrature_pts[, 1], y_ = quadrature_pts[, 2], w_ = UD@CTMM$sigma[1,1]/n, k_ = k)
    xx <- rbind(xx, data.frame(case_ = 1, x_ = x$x, y_ = x$y,
                               w_ = 1/k * UD$weights * mean(UD$DOF.area), k_ = k 
                               # bind the background points and presence points (telemetry) dataframes
    ))
    xx <- sf::st_as_sf(xx, coords = c("x_", "y_"), crs = UD@info$projection)
    xx[names(R)] <- as.data.frame(extract(R, st_transform(xx, crs(R)), 
                                          ID = FALSE,
                                          method = ifelse(interpolation, "bilinear", "simple")))
  }
  
  # Final processing : Extracts the coordinates of all points (both background and presence) from the spatial object xx
  xy <- st_coordinates(xx)
  colnames(xy) <- c("x_", "y_")
  sd <- sqrt(UD@CTMM$sigma[1,1])
  # xy[,1] <- (xy[,1] - UD@CTMM$mu[1])/sd   # telemetry point coordinate x - HR center coordinate x/sd  = standardization
  # xy[,2] <- (xy[,2] - UD@CTMM$mu[2])/sd   # telemetry point coordinate y - HR center coordinate y/sd  = standardization
  xx <- cbind(xy, xx)
  xx
}


buffered_polygon1 <- terra::buffer(polygon, width = buff_vector[1], joinstyle = "mitre") # joinstyle = "mitre" : to correctly represent the limit of the area # width = Unit is meter if x has a longitude/latitude CRS, or in the units of the coordinate reference system in other cases (typically also meter)
# ggplot()+geom_sf(data=buffered_polygon)+geom_sf(data=polygon)

e_mybird_buff1 <- as.vector(ext(buffered_polygon1))


### Crop the environment stack around the bird of interest
#' cropping the stack environment to the extent of the bird data locations *2
scaled_env_RL_list_cropped <- lapply(scaled_env_RL_list_selection, function(raster) {
  terra::crop(raster, extent(e_mybird_buff1)*2)
})
names(scaled_env_RL_list_cropped[["squared_elevation"]]) <- "squared_elevation"

raster_brick_env <- stack(scaled_env_RL_list_cropped)
raster_brick_env <- brick(scaled_env_RL_list_cropped)
# scaled_env_RL_list_brick <- brick(scaled_env_RL_list_selection)

# RSF
set.seed(3)
rsf_abel_df <- rsf_points(x = telemetry_winter, UD = akde_winter, R = raster_brick_env, interpolation = TRUE)
# rsf_abel_df$carto_habitats_winter <- factor(rsf_abel_df$carto_habitats_winter, levels = c(1, 2, 3, 4, 5)) # ensuring carto_habitats_winter is a categorical raster
# rsf_abel_df <- rsf_abel_df[!is.na(rsf_abel_df$carto_habitats_winter), ] # Remove lines with NA values



































summary_rsf_glm <- list()

for(i in seq_along(buff_vector))
{
  
  buffered_polygon <- terra::buffer(polygon, width = buff_vector[i], joinstyle = "mitre") # joinstyle = "mitre" : to correctly represent the limit of the area # width = Unit is meter if x has a longitude/latitude CRS, or in the units of the coordinate reference system in other cases (typically also meter)
  # ggplot()+geom_sf(data=buffered_polygon)+geom_sf(data=polygon)
  
  e_mybird_buff <- as.vector(ext(buffered_polygon))
  
  
  ### Crop the environment stack around the bird of interest
  #' cropping the stack environment to the extent of the bird data locations *2
  scaled_env_RL_list_cropped <- lapply(scaled_env_RL_list_selection, function(raster) {
    terra::crop(raster, extent(e_mybird_buff)*2)
  })
  
  raster_brick_env <- stack(scaled_env_RL_list_cropped)
  # scaled_env_RL_list_brick <- brick(scaled_env_RL_list_brick)
  
  # RSF
  set.seed(3)
  rsf_abel_df <- rsf_points(x = telemetry_winter, UD = akde_winter, R = raster_brick_env, interpolation = TRUE)
  # print(rsf_abel_df)
  # rsf_abel_df$carto_habitats_winter <- factor(rsf_abel_df$carto_habitats_winter, levels = c(1, 2, 3, 4, 5)) # ensuring carto_habitats_winter is a categorical raster
  # rsf_abel_df <- rsf_abel_df[!is.na(rsf_abel_df$carto_habitats_winter), ] # Remove lines with NA values
  
  #' Fit a downweighted Poisson regression
  #' the homeranging behaviour is represented by x_ + y_ + I(-(x_^2 + y_^2)/2)
  m_rsf_abel <- glm(case_*k_ ~ x_ + y_ + I(-(x_^2 + y_^2)/2) + elevation + squared_elevation + strava_winter_sports + Trees + Shrubs + Cliffs,
                    family = poisson(), data= rsf_abel_df, weights = w_)
  # I isolates or insulates the contents of I( ... ) : in order to calculate the interior of I() and then to use it in the formula 
  #' Summary of model and confidence intervals for parameter estimates
  #+ message=FALSE, warning=FALSE
  
  # Interpreting the effects of the glm:
  # x_ and y_ give the model a way to detect spatial patterns in selection or avoidance of certain areas.
  # The formula x_^2 + y_^2 calculates the squared Euclidean distance from the center = the origin of coordinates 
  # to calculate this distance from the HR center we should have (x-x0)^2 + (y-y0)^2
  # Then it is negated and divided by 2, just for a convenience of interpretation of the coefficient beta estimated. C. Fleming explain this /2 in the appendix of his paper.
  
  
  sum_glm_dt <- as.data.frame(summary(m_rsf_abel)$coefficients) #$coefficients : to be certain to get back the whole value of the estimated coefficient
  
  ##################################
  # function to obtain the parameters mu_x = beta x, mu_y = beta y, and variance = 1/beta rr (beta = estimates from the glm model)
  print(model_results(sum_glm_dt))
  ##################################
  
  
  
  sum_glm_dt$covariates <- rownames(sum_glm_dt)
  
  confidence_glm_dt <- as.data.frame(confint.default(m_rsf_abel))
  colnames(confidence_glm_dt) <- c("low","high")
  
  sum_rsf_glm <- cbind(sum_glm_dt %>% dplyr::select(covariates, Estimate),confidence_glm_dt)
  colnames(sum_rsf_glm)[2] <- c("est")
  sum_rsf_glm$IA <- paste0("IA_",buff_vector[i],"_m")
  
  summary_rsf_glm[[i]] <- sum_rsf_glm
}

# bind the summary of the different area of integration
rsf_glm_table <- do.call(rbind,summary_rsf_glm)
rownames(rsf_glm_table) <- NULL

# ordering factors for plotting
rsf_glm_table$IA <- factor(rsf_glm_table$IA, levels = c("IA_1_m"   ,  "IA_100_m" ,  "IA_200_m"  , "IA_500_m"  , "IA_1000_m",  "IA_10000_m"))


rsf_glm_table <- rsf_glm_table %>% 
  mutate(covariates = case_when(
    covariates == "carto_habitats_winter2" ~ "carto_habitats_winter.2_1",
    covariates == "carto_habitats_winter3" ~ "carto_habitats_winter.3_1",
    covariates == "carto_habitats_winter4" ~ "carto_habitats_winter.4_1",
    covariates == "carto_habitats_winter5" ~ "carto_habitats_winter.5_1",
    TRUE ~ covariates  # Keep other values unchanged
  ))



######################*
# using glm without HR
######################*

summary_rsf_glm_noHR <- list()

for(i in seq_along(buff_vector))
{
  
  buffered_polygon <- terra::buffer(polygon, width = buff_vector[i], joinstyle = "mitre") # joinstyle = "mitre" : to correctly represent the limit of the area # width = Unit is meter if x has a longitude/latitude CRS, or in the units of the coordinate reference system in other cases (typically also meter)
  # ggplot()+geom_sf(data=buffered_polygon)+geom_sf(data=polygon)
  
  e_mybird_buff <- as.vector(ext(buffered_polygon))
  
  
  ### Crop the environment stack around the bird of interest
  #' cropping the stack environment to the extent of the bird data locations *2
  env_RL_list_cropped <- lapply(env_RL_list, function(raster) {
    terra::crop(raster, extent(e_mybird_buff)*2)
  })
  
  raster_brick_env <- stack(env_RL_list_cropped)
  
  # RSF
  set.seed(3)
  rsf_abel_df <- rsf_points(telemetry_winter, akde_winter, raster_brick_env, interpolation = TRUE)
  rsf_abel_df$carto_habitats_winter <- factor(rsf_abel_df$carto_habitats_winter, levels = c(1, 2, 3, 4, 5)) # ensuring carto_habitats_winter is a categorical raster
  rsf_abel_df <- rsf_abel_df[!is.na(rsf_abel_df$carto_habitats_winter), ] # Remove lines with NA values
  
  #' Fit a downweighted Poisson regression
  m_rsf_abel <- glm(case_*k_ ~  elevation + square_elevation + strava + leks + carto_habitats_winter,
                    family = poisson(), data= rsf_abel_df, weights = w_)
  #' Summary of model and confidence intervals for parameter estimates
  #+ message=FALSE, warning=FALSE
  
  
  sum_glm_dt <- as.data.frame(summary(m_rsf_abel)$coefficients)
  sum_glm_dt$covariates <- rownames(sum_glm_dt)
  
  confidence_glm_dt <- as.data.frame(confint.default(m_rsf_abel))
  colnames(confidence_glm_dt) <- c("low","high")
  
  sum_rsf_glm <- cbind(sum_glm_dt %>% dplyr::select(covariates, Estimate),confidence_glm_dt)
  colnames(sum_rsf_glm)[2] <- c("est")
  sum_rsf_glm$IA <- paste0("IA_",buff_vector[i],"_m")
  
  summary_rsf_glm_noHR[[i]] <- sum_rsf_glm
}

# bind the summary of the different area of integration
rsf_glm_table_noHR <- do.call(rbind,summary_rsf_glm_noHR)
rownames(rsf_glm_table_noHR) <- NULL

# ordering factors for plotting
rsf_glm_table_noHR$IA <- factor(rsf_glm_table_noHR$IA, levels = c("IA_1_m"   ,  "IA_100_m" ,  "IA_200_m"  , "IA_500_m"  , "IA_1000_m",  "IA_10000_m"))


rsf_glm_table_noHR <- rsf_glm_table_noHR %>% 
  mutate(covariates = case_when(
    covariates == "carto_habitats_winter2" ~ "carto_habitats_winter.2_1",
    covariates == "carto_habitats_winter3" ~ "carto_habitats_winter.3_1",
    covariates == "carto_habitats_winter4" ~ "carto_habitats_winter.4_1",
    covariates == "carto_habitats_winter5" ~ "carto_habitats_winter.5_1",
    TRUE ~ covariates  # Keep other values unchanged
  ))
#********************************************************************





### 6_Graph visualisation of the impact of the study area size (integration area) ----
#********************************************************************
# plot estimated parameters beta ~ the integrated area 
plot_IA_rsf_ctmm<-
  ggplot(data = rsf_table)+
  geom_point(aes(y = est, 
                 x = IA, 
                 group = IA,
                 color = covariates))+
  geom_errorbar(aes(x = IA, ymin = low, ymax = high, group = IA, color = covariates, width = 0.1))+
  labs( title=paste0("Impact of the integration area size (bird:", bird,", 2nd winter)"),
        x = "Integration area",
        y = "Coefficient value",
        color = "RSF Estimated coefficients")+
  # ylim(c(min(rsf_table$est-1),max(rsf_table %>% filter(covariates != "diffusion (hectares/day)") %>% pull("est"))+1.5))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        title = element_text(size = 18),
        legend.text = element_text(size = 14), 
        axis.text.x = element_text(angle = 45, size = 07, hjust=1))+
  facet_wrap(~covariates, ncol=3, scales = "free")

ggsave(plot=plot_IA_rsf_ctmm, filename="plot_IA_rsf_ctmm_abel.png",path="C:/Users/albordes/Documents/PhD/TetrAlps/5_OUTPUTS/data_exploration/integration_area",width = 40, height = 20, units = "cm")
# huge incertitude for carto_habitats_winter.4_1 bc few buildings in the area




# plot estimated parameters beta ~ the integrated area 
plot_rsf_glm <- 
  ggplot(data = rsf_glm_table)+
  geom_point(aes(y = est, 
                 x = IA, 
                 group = IA,
                 color = covariates))+
  geom_errorbar(aes(x = IA, ymin = low, ymax = high, group = IA, color = covariates, width = 0.1))+
  labs( title=paste("Impact of the integration area size (bird: Abel, 2nd winter)"),
        x = "Integration area",
        y = "Coefficient value",
        color = "RSF Estimated coefficients")+
  # ylim(c(min(rsf_table$est-1),max(rsf_table %>% filter(covariates != "diffusion (hectares/day)") %>% pull("est"))+1.5))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        title = element_text(size = 18),
        legend.text = element_text(size = 14), 
        axis.text.x = element_text(angle = 45, size = 07, hjust=1))+
  facet_wrap(~covariates, ncol=3, scales = "free")

ggsave(plot=plot_rsf_glm, filename="plot_rsf_glm_abel.png",path="C:/Users/albordes/Documents/PhD/TetrAlps/5_OUTPUTS/data_exploration/integration_area",width = 40, height = 20, units = "cm")




# plot estimated parameters beta ~ the integrated area 
plot_rsf_glm_noHR <-
  ggplot(data = rsf_glm_table_noHR)+
  geom_point(aes(y = est, 
                 x = IA, 
                 group = IA,
                 color = covariates))+
  geom_errorbar(aes(x = IA, ymin = low, ymax = high, group = IA, color = covariates, width = 0.1))+
  labs( title=paste("Impact of the integration area size (bird: Abel, 2nd winter)"),
        x = "Integration area",
        y = "Coefficient value",
        color = "RSF Estimated coefficients")+
  # ylim(c(min(rsf_table$est-1),max(rsf_table %>% filter(covariates != "diffusion (hectares/day)") %>% pull("est"))+1.5))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        title = element_text(size = 18),
        legend.text = element_text(size = 14), 
        axis.text.x = element_text(angle = 45, size = 07, hjust=1))+
  facet_wrap(~covariates, ncol=3, scales = "free")

ggsave(plot=plot_rsf_glm_noHR, filename="plot_rsf_glm_noHR_abel.png",path="C:/Users/albordes/Documents/PhD/TetrAlps/5_OUTPUTS/data_exploration/integration_area",width = 40, height = 20, units = "cm")



# plot estimated parameters beta of each of the 3 HSF methods ~ the integrated area 

rsf_table$method <- "ctmm::rsf"
rsf_glm_table$method <- "glm"
rsf_glm_table_noHR$method <- "glm_noHR"

# add the estimation of the area to the global table
rsf_glm_table2 <- list()


# Calcul des estimations pour rsf_glm_table2 
errbnd <- 0.05 # equivalent 1/20 values falls outside of my predictions
confidence <- 1-errbnd #confidence region = 95%
# Joint analysis : in 2D. We use chi-squared (chi2) distribution to compute(calculate) the confidence circle = minimal area
dims = 2 # x,y
chich = qchisq(confidence, dims) # chi2 bound, quantile function and random generation for the chi-squared



for(i in seq_along(unique(rsf_glm_table$IA))) {
  current_IA <- unique(rsf_glm_table$IA)[i]
  print(current_IA)
  
  # Create a new data frame for each unique IA
  covariate_est <- exp(rsf_glm_table[rsf_glm_table$covariates == "I(-(x_^2 + y_^2)/2)" & 
                                       rsf_glm_table$IA == current_IA, "est"])
  covariate_low <- exp(rsf_glm_table[rsf_glm_table$covariates == "I(-(x_^2 + y_^2)/2)" & 
                                       rsf_glm_table$IA == current_IA, "low"])
  covariate_high <- exp(rsf_glm_table[rsf_glm_table$covariates == "I(-(x_^2 + y_^2)/2)" & 
                                        rsf_glm_table$IA == current_IA, "high"])
  
  rsf_glm_table2[[i]] <- data.frame(
    covariates = "area (km^2)",
    IA = current_IA,
    method = "glm", 
    est = pi * chich * sqrt(det(diag(1 / (covariate_est), 2))),
    low = pi * chich * sqrt(det(diag(1 / (covariate_low), 2))),
    high = pi * chich * sqrt(det(diag(1 / (covariate_high), 2)))
  )
  
  print(rsf_glm_table2[[i]])
}

# Combine the list into a single data frame
rsf_glm_table2_combined <- do.call(rbind, rsf_glm_table2)



rsf_glm_table <- rbind(rsf_glm_table, rsf_glm_table2_combined)
rsf_table_multi <- rbind(rsf_table, rsf_glm_table)
rsf_table_multi <- rbind(rsf_table_multi, rsf_glm_table_noHR)


# option 1) Comparison effets glm VS glm_noHR (focusing on I(-(x^2+y^2)/2))
rsf_table_multi_plot <- rsf_table_multi %>% filter(method!="ctmm::rsf") %>% filter(covariates!="area (km^2)")
values_color = c("#3399FF","#00CC00")
# option 2) Comparison effets glm VS ctmm
rsf_table_multi_plot <- rsf_table_multi %>% filter(method!="glm_noHR")
values_color = c("#FF6633","#3399FF")


# plot estimated parameters beta ~ the integrated area 
plot_rsf_multi <-
  ggplot(data = rsf_table_multi_plot %>% filter(!covariates%in%c("(Intercept)", "strava_backcountry")))+
  geom_point(aes(y = est, 
                 x = IA, 
                 group = IA,
                 color = method))+
  geom_errorbar(aes(x = IA, ymin = low, ymax = high, group = IA, color = method, width = 0.1))+
  scale_color_manual(values=alpha(values_color,0.8))+
  scale_size_manual(values=values_color)+
  labs( title=paste("Impact of the integration area size (bird: Abel, 2nd winter)"),
        x = "Integration area",
        y = "Coefficient value",
        color = "RSF Estimated coefficients")+
  # ylim(c(min(rsf_table$est-1),max(rsf_table %>% filter(covariates != "diffusion (hectares/day)") %>% pull("est"))+1.5))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        title = element_text(size = 18),
        legend.text = element_text(size = 14), 
        axis.text.x = element_text(angle = 45, size = 07, hjust=1))+
  facet_wrap(~covariates, ncol=5, scales = "free")

ggsave(plot=plot_rsf_multi, filename="plot_rsf_multi_abel.png",path="C:/Users/albordes/Documents/PhD/TetrAlps/5_OUTPUTS/data_exploration/integration_area",width = 40, height = 20, units = "cm")
#********************************************************************

# metadata carto_habitat_winter

# cat 1) Soils and low vegetation : Unclassified soil, Fine mineral soil, Coarse mineral soil, Dry or rocky grassland, Herbaceous, Low ligneous
# cat 2) Shrubs
# cat 3) Trees : Unclassified trees, Deciduous trees, Resinous trees
# cat 4) Buildings
# cat 5) Cliffs and water : Cliff, Natural pond, Artificial pond, Waterway, Unclassified



### 7_Statistic analysis of the impact of the study area size (integration area) ----
#********************************************************************
# Is the variability of estimations important? 
data_rsf = rsf_table
data_rsf = rsf_glm_table
data_rsf = rsf_glm_table_noHR

bartlett.test(data_rsf$est ~ data_rsf$IA , data = data_rsf) # if p-value > 0.05 : similar variance between groups --> they are comparable using anova
model <- aov(est ~ IA, data = data_rsf)
summary(model)
TukeyHSD(model, conf.level = 0.95)

# Anova --> effect of IA but Tukey --> no significant difference between groups => so relative stability of coefficients when the integration area is varying
#********************************************************************



### 8_Vizualising the landscape composition pattern for each integration area ----
#********************************************************************
# boxplot of availiable values inside the raster crooped to the integration area extents

IA_compo <- list()
for(i in seq_along(buff_vector))
{
  # Apply a buffer of 200 units
  polygon <- as.polygons(ext(e_mybird))
  crs(polygon) <- "EPSG:2154"
  buffered_polygon <- terra::buffer(polygon, width = buff_vector[i], joinstyle = "mitre") # joinstyle = "mitre" : to correctly represent the limit of the area # width = Unit is meter if x has a longitude/latitude CRS, or in the units of the coordinate reference system in other cases (typically also meter)
  # ggplot()+geom_sf(data=buffered_polygon)+geom_sf(data=polygon)
  
  e_mybird_buff <- as.vector(ext(buffered_polygon))
  
  
  ### Crop the environment stack around the bird of interest
  #' cropping the stack environment to the extent of the bird data locations *2
  env_RL_list_cropped <- lapply(env_RL_list, function(raster) {
    terra::crop(raster, extent(e_mybird_buff)*2)
  })
  # cheking environment slacks
  # terra::plot(envir_crop)
  # terra::plot(envir_crop[["carto_habitats_winter"]])
  
  v <- list()
  for(env_rast in seq_along(env_RL_list_cropped))
  {
    v[[env_rast]] <- terra::values(env_RL_list_cropped[[env_rast]])
  }
  names(v) <- names(env_RL_list_cropped)
  
  IA_compo[[i]] <- v
  
  names(IA_compo)[i] <- paste0("IA_",buff_vector[i],"_m")
}



# Convert the list of lists into a data frame
dt_IA_compo <- IA_compo %>%
  # Convert the list to a long format tibble
  bind_rows(.id = "Area") %>%
  # Pivot the data to long format so each variable is a row
  pivot_longer(cols = -Area, names_to = "Variable", values_to = "Value")



# Graph for Comparing the patterns of landscape composition\nbetween different scales of spatial integration in HSF
dt_IA_compo_flt <- dt_IA_compo %>% filter(!is.na(Area) & !is.na(Value) & !is.na(Variable))%>% 
  mutate(Area = factor(Area, levels = c("IA_1_m"   ,  "IA_100_m" ,  "IA_200_m" ,  "IA_500_m"  , "IA_1000_m" , "IA_10000_m")))

compo_landscape <- 
  ggplot() +
  # Boxplot for numeric variables
  geom_violin(data = dt_IA_compo_flt %>% filter(Variable != "carto_habitats_winter"), 
              aes(x = Area, y = Value), fill= "#999999", alpha = 0.1, color = "#999999") +
  geom_boxplot(data = dt_IA_compo_flt %>% filter(Variable != "carto_habitats_winter"), 
               aes(x = Area, y = Value), fill= "#66CCCC", alpha = 0.4, color = "#006666") +
  
  # Bar plot for categorical variable
  geom_bar(data = dt_IA_compo_flt %>% filter(Variable == "carto_habitats_winter"), 
           aes(x = Area, fill = as.factor(Value)), position = "fill") +
  scale_fill_manual(name = "Habitat classes",
                    values = c("1"="#CCCCCC","2"="#FF99CC","3"="#99FF99","4"="#993300","5"="#99CCFF"),na.value ="transparent",
                    labels = c("1"="Soils","2"="Shrubs","3"="Trees","4"="Buildings","5"="Others"))+
  labs(title = "Comparing the patterns of landscape composition\nbetween different scales of spatial integration in HSF", 
       x = "Size of the HSF integration area", 
       y = "Distribution of raster values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 16, hjust = 1), 
        strip.text = element_text(face = "bold", size = 14),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  facet_wrap(~Variable, scales = "free_y")  # Different scales for each variable

compo_landscape

ggsave(plot=compo_landscape, filename="compo_landscape_patterns_abel.png",path="C:/Users/albordes/Documents/PhD/TetrAlps/5_OUTPUTS/data_exploration/integration_area",width = 40, height = 20, units = "cm")
#********************************************************************