#### PhD Tetralpes project ####

# Alice Bordes #

# September 2024 #

# Description:

# RSF functions
#*******************************************************************




# Function to obtain the parameters mu_x = beta x, mu_y = beta y, and variance = 1/beta rr (beta = estimates from the glm model)
#********************************************************************
model_results <- function(model_output)
{
  
  mu_x = model_output["x_","Estimate"]/model_output["I(-(x_^2 + y_^2)/2)","Estimate"]
  mu_y = model_output["y_","Estimate"]/model_output["I(-(x_^2 + y_^2)/2)","Estimate"]
  variance = 1/model_output["I(-(x_^2 + y_^2)/2)","Estimate"]
  
  dims = 2 # x,y
  errbnd <- 0.05 
  confidence <- 1-errbnd
  chich = qchisq(confidence, dims)
  CovMatrix = diag(variance,2) #CovMatrix = contains the variance of the 2 variables (x,y) in a diag matrix 
  area = (pi*sqrt(det(CovMatrix))*chich)/1000000
  
  para_est_dt <- data.frame(parameters = c("IA", "mu_x","mu_y","variance","area","x_","y_","I(-(x_^2 + y_^2)/2)"),
                            estimates = c(paste0("IA_",buff_vector[i],"_m"), mu_x, mu_y, variance, area, model_output["y_","Estimate"], model_output["x_","Estimate"], model_output["I(-(x_^2 + y_^2)/2)","Estimate"]))
  
  return(para_est_dt)
}
#********************************************************************







### Function for the creation of telemetry, guess, fit and akde objects for rsf ----
#********************************************************************

tele_akde <- function(data, 
                      birds_vect = FALSE, 
                      season = "hiver", 
                      subset_category = "saison2", 
                      outputfolder = file.path(base, "Tetralps", "3_R", "0_Heavy_saved_models", "birds_3V"),
                      write = FALSE)
{
  
  if(!isFALSE(birds_vect))
  {
    data <- data %>% filter(animal.ID %in% birds_vect)
  }
  
  #### Creation of a global telemetry object (for all birds)
  # birds_bg_dt_winter <- birds_bg_dt %>% filter(saison %in% season1)
  l_telemetry_winter <- list()
  l_guess_winter <- list()
  l_fit_winter <- list()
  l_akde_winter <- list()
  l_names <- list()
  b <- 0
  
  for (bg in seq_along(unique(data$animal.ID))) { 
    birds_sample_bg_pretele <- data %>% filter(animal.ID %in% unique(data$animal.ID)[bg])
    
    if ("hiver" %in% unique(birds_sample_bg_pretele$saison)) {
      b <- b + 1
      
      # Add visitor and snow depth data
      birds_sample_bg_pretele <- birds_sample_bg_pretele %>% 
        mutate(Date = as.Date(timestamp)) %>%
        left_join(visitor_meribel %>% dplyr::select(Date, Total_std), by = "Date") %>%
        rename(total.visitors.meribel = Total_std) %>%
        left_join(snow_meribel %>% dplyr::select(Date, snow.depth_std), by = "Date") %>%
        rename(snow.depth = snow.depth_std)
      
      # Create telemetry data
      telemetry <- as.telemetry(birds_sample_bg_pretele, projection = "EPSG:2154",
                                keep = c("saison", "saison2", "period_jour", "animal.sex", 
                                         "animal.life.stage", "total.visitors.meribel", "snow.depth"))
      ctmm::projection(telemetry) <- "EPSG:2154"
      
      # Filter telemetry data for winter season
      telemetry_winter <- telemetry[telemetry$saison == season, ]
      telemetry_winter$snow.depth <- scale(telemetry_winter$snow.depth)
      telemetry_winter$total.visitors.meribel <- scale(telemetry_winter$total.visitors.meribel)
      
      
    
      
      
      
      
      # If subset_category is "all", do not split by category
      if(subset_category != "all") {
        # Subset telemetry_winter by 'saison2'
        telemetry_winter_subsets <- split(telemetry_winter, telemetry_winter[[subset_category]])
      } else {
        # If "all", use the entire dataset as one subset
        telemetry_winter_subsets <- list("all" = telemetry_winter)
      }
      
      
      
      
      # # Subset telemetry_winter by 'saison2'
      # telemetry_winter_subsets <- split(telemetry_winter, telemetry_winter[[subset_category]])
      
      # Initialize nested lists for each bird
      l_telemetry_winter[[b]] <- list()
      l_guess_winter[[b]] <- list()
      l_fit_winter[[b]] <- list()
      l_akde_winter[[b]] <- list()
      l_names[[b]] <- telemetry_winter@info$identity  # Store bird identity
      
      # Loop over each winter subset within the bird's data
      for (saison2_category in names(telemetry_winter_subsets)) {
        subset_telemetry <- telemetry_winter_subsets[[saison2_category]]
        
        if(nrow(subset_telemetry)>50) # if there is at least 50 observations = 50 locations 
        {
          # AKDE analysis for each winter subset
          guess_winter <- ctmm.guess(subset_telemetry, CTMM = ctmm(isotropic = TRUE), interactive = FALSE)
          fit_winter <- ctmm.select(subset_telemetry, guess_winter)
          akde_winter <- akde(subset_telemetry, fit_winter)
         
          # Save results by 'saison2' category within each bird's list
          l_telemetry_winter[[b]][[saison2_category]] <- subset_telemetry
          l_guess_winter[[b]][[saison2_category]] <- guess_winter
          l_fit_winter[[b]][[saison2_category]] <- fit_winter
          l_akde_winter[[b]][[saison2_category]] <- akde_winter

        }
        
      }
    }
  } # end for bg
  
  # Naming lists by bird identity
  names(l_telemetry_winter) <- l_names
  names(l_guess_winter) <- l_names
  names(l_fit_winter) <- l_names
  names(l_akde_winter) <- l_names
  

  # Remove entries in the main lists that are empty (length == 0)
  l_telemetry_winter <- Filter(function(x) length(x) > 0, l_telemetry_winter)
  l_guess_winter <- Filter(function(x) length(x) > 0, l_guess_winter)
  l_fit_winter <- Filter(function(x) length(x) > 0, l_fit_winter)
  l_akde_winter <- Filter(function(x) length(x) > 0, l_akde_winter)
  
  if(write == TRUE)
  {
    # Saving the models
    save(l_telemetry_winter, file = file.path(outputfolder, paste0("multipl_telemetry_winter_",subset_category , ".Rdata")))
    save(l_guess_winter, file = file.path(outputfolder, paste0("multipl_guess_winter_",subset_category , ".Rdata")))
    save(l_fit_winter, file = file.path(outputfolder, paste0("multipl_fit_winter_",subset_category , ".Rdata")))
    save(l_akde_winter, file = file.path(outputfolder, paste0("multipl_akde_winter_",subset_category , ".Rdata")))
  }
    
  return()
}
#********************************************************************  



# Function for building the bird semi-variogram
#********************************************************************  
# The animal must show a resident behevior on the considered scale : 
# "Finally, at larger scales, most animals will exhibit a tendency to remain in a defined region or ‘home range’". (Calabrese, 2016)


# The variogram, or semivariogram, plots time lags on the x-axis for all pairs of observations
# against their semi-variance (half of the variance for the distance each observation pair) on the y-axis. (Animove 2022)


# Bird's VARIOGRAM

bird_variogram <- function(telemetry_list = l_telemetry_winter, dt_hour = NULL, outputfolder = file.path(base, "Tetralps", "5_OUTPUTS","RSF","variograms","multipl_variograms"), write = FALSE)
{
  for(bg in seq_along(l_telemetry_winter)) #l_telemetry_winter
  {
    # list of all the winters encountered by the bird
    l_seasons <- names(telemetry_list[[bg]])
    
    # taking into account the irregular sampling schedule
    # taking into account the most common hour sampling intervals: 1, 6, 12
    # dt <- c(1, 6, 12) %#% "hour"
    if(!is.null(dt_hour))
    {
      dt <- dt_hour %#% "hour" 
    }
    
    # If writing to file, open the JPEG device once per bird
    if (write == TRUE) 
    {
      jpeg(file = file.path(outputfolder, paste0(telemetry_list[[bg]][[l_seasons[1]]]@info$identity, "_variogram.jpeg")),
           width = 27, height = 15, unit = "cm", res = 300)
    }
    
    par(mfrow=c(1,length(l_seasons)))
    
    for(seas in seq_along(l_seasons))
    {
      SVF <- variogram(telemetry_list[[bg]][[seas]], dt=dt) # bird's variogram
      p <- plot( SVF,
                 fraction=0.9,
                 level=c(0.5,0.95),
                 CTMM=fit_winter, 
                 main = paste0(telemetry_list[[bg]][[seas]]@info$identity,"'s empirical variogram (90%)\nWinter"),
                 cex.main = 0.8)
      p
    }
    
    if(write == TRUE)
    {
      dev.off()
    }
    
  }
  
  return()
}
#********************************************************************  


# Function to calculate the overlap between the akde of all the winters encountered by 1 bird 
#********************************************************************  
overlap_winter <- function(telemetry_list = l_telemetry_winter)
{
  dt_overlap <- data.frame("animal" = character(), "estimated_overlap" = numeric())
  
  for(bg in seq_along(names(l_telemetry_winter))) #l_telemetry_winter
  {
    if(length(names(l_telemetry_winter[[bg]]))>1)
    {
        print(l_telemetry_winter[[bg]][[1]]@info$identity)
        # But this works because HRs are estimated simultaneously (and consistently)
        over <- overlap(akde(l_telemetry_winter[[bg]],l_fit_winter[[bg]]))
        print(over$CI[,,"est"])
        
        dt_to_overlap <- data.frame( "animal" = l_telemetry_winter[[bg]][[1]]@info$identity, 
                                     "estimated_overlap" = min(over$CI[,,"est"]))
        dt_overlap <- rbind(dt_overlap, dt_to_overlap)
    }

  }
  
  return(dt_overlap)
}
#********************************************************************  








