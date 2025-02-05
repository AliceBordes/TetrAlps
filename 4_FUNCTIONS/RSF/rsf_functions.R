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
      
      # Create telemetry data
      telemetry <- as.telemetry(birds_sample_bg_pretele, projection = "EPSG:2154",
                                keep = c("saison", "saison2", "period_jour", "animal.sex", 
                                         "animal.life.stage", "total.visitors", "snow.depth",
                                         "total.visitors.std", "snow.depth.std",
                                         "time", "sl_open",
                                         "visitor_breaks", "visitor_breaksNull", "visitor_breaksLow", "visitor_breaksMedium", "visitor_breaksHigh" , "visitor_breaksVery_high" ))
      ctmm::projection(telemetry) <- "EPSG:2154"
      
      # Filter telemetry data for winter season
      telemetry_winter <- telemetry[telemetry$saison == season, ]

      
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
          akde_winter <- akde(subset_telemetry, fit_winter, weights=TRUE) #weight the data to account for sampling bias
         
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
    if(subset_category == "all")
    {
      season_title = season
    }else
    {
      season_title = subset_category
    }
    
    # Saving the models
    save(l_telemetry_winter, file = file.path(outputfolder, paste0("multipl_telemetry_winter_",season_title , "_", format(Sys.time(),"%Y_%m_%d"), ".Rdata")))
    save(l_guess_winter, file = file.path(outputfolder, paste0("multipl_guess_winter_",season_title , "_", format(Sys.time(),"%Y_%m_%d"),".Rdata")))
    save(l_fit_winter, file = file.path(outputfolder, paste0("multipl_fit_winter_",season_title ,"_", format(Sys.time(),"%Y_%m_%d"), ".Rdata")))
    save(l_akde_winter, file = file.path(outputfolder, paste0("multipl_akde_winter_",season_title , "_", format(Sys.time(),"%Y_%m_%d"),".Rdata")))
  }
    
  return()
}
#********************************************************************  




# Function for displaying bird sampling schedule
#********************************************************************  

bird_sampling_sche <- function(telemetry_list = l_telemetry_winter, 
                               outputfolder = file.path(base, "Tetralps", "5_OUTPUTS","RSF","sampling"), 
                               write = FALSE)
{
  for(bg in seq_along(telemetry_list)) #l_telemetry_winter
  {
    
    # If writing to file, open the JPEG device once per bird
    if(write == TRUE) 
    {
      jpeg(file = file.path(outputfolder, paste0(telemetry_list[[bg]][[1]]@info$identity, "_sampling_schedule.jpeg")),
           width = 27, height = 15, unit = "cm", res = 300)
    }
    
    ctmm::dt.plot(telemetry_list[[bg]][[1]],
                  main = paste0(telemetry_list[[bg]][[1]]@info$identity,"'s sampling schedule\nWinter")) # bird's sampling schedule
    
    if(write == TRUE)
    {
      dev.off()
    }
    
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

bird_variogram <- function(telemetry_list = l_telemetry_winter, 
                           fit_list = l_fit_winter,
                           dt_hour = NULL, 
                           outputfolder = file.path(base, "Tetralps", "5_OUTPUTS","RSF","variograms"), 
                           write = FALSE)
{
  for(bg in seq_along(telemetry_list)) #l_telemetry_winter
  {
   
    # taking into account the irregular sampling schedule
    # taking into account the most common hour sampling intervals: 1, 6, 12
    # dt <- c(1, 6, 12) %#% "hour"
    if(!is.null(dt_hour))
    {
      dt_hour <- dt_hour %#% "hour" 
    }
    
    # If writing to file, open the JPEG device once per bird
    if(write == TRUE) 
    {
      jpeg(file = file.path(outputfolder, paste0(telemetry_list[[bg]][[1]]@info$identity, "_variogram.jpeg")),
           width = 27, height = 15, unit = "cm", res = 300)
    }
    
      SVF <- variogram(telemetry_list[[bg]][[1]], dt=dt_hour) # bird's variogram
      p <- plot( SVF,
                 fraction=0.9,
                 level=c(0.5,0.95),
                 CTMM=fit_list[[bg]][[1]], 
                 main = paste0(telemetry_list[[bg]][[1]]@info$identity,"'s empirical variogram (90%)\nWinter"),
                 cex.main = 0.8)
      p
    
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
overlap_winter <- function(telemetry_list, 
                           fit_list,
                           telemetry_list2 = NULL, 
                           fit_list2 = NULL,
                           overlap_type)
{
  dt_overlap <- data.frame("animal" = character(), "estimated_overlap" = numeric())
  
  if(overlap_type == "multiple winters")
  {
    
    
    for(bg in seq_along(names(telemetry_list))) 
    {
      if(length(names(telemetry_list[[bg]]))>1)
      {
        print(telemetry_list[[bg]][[1]]@info$identity)
        # But this works because HRs are estimated simultaneously (and consistently)
        over <- overlap(akde(telemetry_list[[bg]],fit_list[[bg]]))
        print(over$CI[,,"est"])
        
        dt_to_overlap <- data.frame( "animal" = telemetry_list[[bg]][[1]]@info$identity, 
                                     "estimated_overlap" = min(over$CI[,,"est"]))
        dt_overlap <- rbind(dt_overlap, dt_to_overlap)
      }
      
    }
  }
  
  
  if(overlap_type == "season length")
  {
    if (is.null(telemetry_list2) || is.null(fit_list2)) 
      {stop("2 telemetry lists are needed")}
      
      dt_overlap <- data.frame("animal" = character(), "estimated_overlap" = numeric())
    
      common_names <- intersect(names(l_telemetry_winter_predefined_win), names(l_telemetry_winter_large_win)) 
      
    for(bg in seq_along(common_names))
    {
      
      print(telemetry_list[[common_names[bg]]][[1]]@info$identity)
      # But this works because HRs are estimated simultaneously (and consistently)
      over <- overlap(akde(list(telemetry_list[[common_names[bg]]][[1]], telemetry_list2[[common_names[bg]]][[1]]),
                           list(fit_list[[common_names[bg]]][[1]], fit_list2[[common_names[bg]]][[1]])))
      print(over$CI[,,"est"])
      
      dt_to_overlap <- data.frame( "animal" = telemetry_list[[common_names[bg]]][[1]]@info$identity, 
                                   "estimated_overlap" = min(over$CI[,,"est"]))
      dt_overlap <- rbind(dt_overlap, dt_to_overlap)
      
      
      
    }
  }
  
  
  
  
  return(dt_overlap)
}
#********************************************************************  




# Function to apply RSF on multiple birds
#********************************************************************  
RSF_birds <- function(telemetry_list, 
                      akde_list,
                      env_raster_list,
                      rsf_formula,
                      rsf_integrator = "MonteCarlo",
                      grid = "individual",
                      outputfolder = file.path(base, "Tetralps", "5_OUTPUTS", "RSF", "rsf.fit_results"),
                      write = TRUE)
{
  warning("The arguments telemetry_list and akde_list must have a unique element in their nested list.")
  
  # Create a parallel cluster
  cl <- makeCluster(8)
  registerDoParallel(cl)
  
  # Explicitly export required variables to the cluster
  clusterExport(cl, varlist = c("telemetry_list", "akde_list", "env_raster_list", "grid", "rsf.fit"), envir = environment())
  
  
  # Use foreach for parallel processing
  sum_rsf_multipl <- foreach(bg = seq_along(telemetry_list), .packages = c("terra", "ctmm", "adehabitatHR", "sp")) %dopar% {
    start_time <- proc.time()
    
    # Print the current bird being processed
    print(names(telemetry_list)[bg])
    
    #### Size of the integration grid 
    if (grid == "full") {
      e_mybird <- extent(scaled_env_RL_list[[1]])
    } else if (grid == "individual") {
      # Calculate the 99% home range (HR)
      r_mybird_akde_99 <- SpatialPolygonsDataFrame.UD(akde_list[[bg]][[1]], level.UD = 0.99, level = 0.95)
      
      # Calculate MCP
      subset_df <- telemetry_list[[bg]][[1]][, c("x", "y")]
      class(subset_df) <- "data.frame"
      coordinates(subset_df) <- ~x + y
      mcp_result <- mcp(subset_df, percent = 100)
      
      # Extent calculation
      e_mybird <- c(
        min(ext(r_mybird_akde_99), ext(mcp_result))[1],
        max(ext(r_mybird_akde_99), ext(mcp_result))[1],
        min(ext(r_mybird_akde_99), ext(mcp_result))[2],
        max(ext(r_mybird_akde_99), ext(mcp_result))[2]
      )
    }
    
    # Crop the environment stack
    env_RL_list_cropped <- lapply(env_raster_list, function(raster) {
      terra::crop(raster, extent(e_mybird) * 2)
    })
    
    # RSF model fitting
    set.seed(3)
    mybird_rsf_mc_strava <- rsf.fit(
      telemetry_list[[bg]][[1]], 
      akde_list[[bg]][[1]],  
      R = env_RL_list_cropped,
      integrator = rsf_integrator,
      formula = rsf_formula
    )
    
    # Log elapsed time
    end_time <- proc.time()
    print(end_time - start_time)
    
    return(mybird_rsf_mc_strava)
  }
  
  # Stop the cluster
  stopCluster(cl)
  
  # Naming results
  names(sum_rsf_multipl) <- names(telemetry_list)
  
  print("The model has been run successfully")
  
  # Save results if needed
  if (write) {
    # Replace ':' with '_' and cut from '[' onward
    clean_name <- gsub(":", "_", sub(".*\\[", "[", deparse(substitute(telemetry_list))))
    clean_name <- paste0("rsf",clean_name)
    # Check if the resulting name contains '['
    if (!grepl("\\[", clean_name)) {
      clean_name <- paste0("rsf_", length(telemetry_list), "birds")
    }
    if (str_contains(clean_name,"covid") & str_contains(clean_name,"!")) {
      clean_name <- paste0("rsf_", length(telemetry_list), "birds","_outcovid")
    }
    if (str_contains(clean_name,"covid") & !str_contains(clean_name,"!")) {
      clean_name <- paste0("rsf_", length(telemetry_list), "birds","_covid")
    }
    
    print(clean_name)
    
    name_grid <- gsub('^"|"$', '', deparse(substitute(grid)))
    
    dir.create(file.path(outputfolder, paste0(clean_name, "_", name_grid,"_", format(Sys.time(),"%Y_%m_%d_%Hh%Mmin")
                                                      )
                                 )
                )
    save(sum_rsf_multipl, file = file.path(outputfolder,
                                           paste0(clean_name, "_", name_grid,"_", format(Sys.time(),"%Y_%m_%d_%Hh%Mmin")),
                                           paste0(clean_name, "_", name_grid,"_", format(Sys.time(),"%Y_%m_%d_%Hh%Mmin"), ".Rdata")
                                           )
         )
  }
  





# Write README file
readme_file <- file.path(outputfolder,
                         paste0(clean_name, "_", name_grid,"_", format(Sys.time(),"%Y_%m_%d_%Hh%Mmin")),
                         paste0(clean_name, "_", name_grid,"_", format(Sys.time(),"%Y_%m_%d_%Hh%Mmin"),"_README.txt"))
sink(readme_file)

cat("### RSF Analysis README ###\n")
cat("Generated on: ", format(Sys.time(),"%Y_%m_%d_%Hh%Mmin"), "\n\n")

# Characteristics of telemetry_list
cat("### Characteristics of telemetry_list ###\n")
cat("Telemetry input:", deparse(substitute(telemetry_list)), "\n")
cat("Length of telemetry_list: ", length(telemetry_list), "\n\n")
cat("Variable names in telemetry_list[[i]][['all']]:\n")
cat(paste(names(telemetry_list[[1]][[1]]), collapse = ", "), "\n\n\n")

# Characteristics of env_raster_list
cat("### Characteristics of env_raster_list ###\n")
for(r in 1:length(env_raster_list))
{
  example_raster <- env_raster_list[[r]]
  cat(names(example_raster), "\n")
  cat("Resolution: ", paste(res(example_raster), collapse = " x "), "\n")
  cat("CRS: ", st_crs(example_raster)$input, "\n")
  cat("Extent: ", paste(c(extent(example_raster)[1], extent(example_raster)[2], extent(example_raster)[3], extent(example_raster)[4]), collapse = ", "), "\n\n")
}
cat("\n\n")

# RSF formula
cat("### RSF Formula ###\n")
cat(deparse(rsf_formula), "\n")

sink()







  return(sum_rsf_multipl)
}
#******************************************************************* 




# 
# # Function for the creation of a variable fact.visitor.nb and a variable for ski lift opening hours in the telemetry list object
# #*******************************************************************
# # add_variables_visit_open <- function(list_telemetry) 
# # {
# #   
# #   # Combine into a single data frame with animal names added
# #   combined_telemetry <- do.call(rbind, 
# #                                 lapply(names(list_telemetry), function(bird_name) {
# #                                   # For each bird, add the animal ID as a new column
# #                                   bird_data <- do.call(rbind, list_telemetry[[bird_name]])
# #                                   bird_data$animal.ID <- bird_name  # Add the animal name as a new column
# #                                   return(bird_data)
# #                                 })
# #   )
# #   combined_telemetry$Jour <- as.Date(as.POSIXct(combined_telemetry$timestamp))
# #   
# #   # official lockdown Tuesday 17 March 2020 at 12:00 - Monday 3 May 2021 
# #   # On 4 December 2020, the Prime Minister banned public access to ski lifts in ski resorts, with the exception of professionals and children who are members of an association affiliated to the French Ski Federation.
# #   
# #   covid <- combined_telemetry %>% filter(saison=="hiver") %>% filter(as.Date(Jour) < as.Date("2021-06-01")) %>% filter(as.Date(Jour) > as.Date("2020-10-01"))
# #   covid <- unique(covid$animal.ID)
# #   
# #   deb_covid <- combined_telemetry %>% filter(saison=="hiver") %>% filter(as.Date(Jour) < as.Date("2020-06-01")) %>% filter(as.Date(Jour) > as.Date("2020-03-16"))
# #   deb_covid <- unique(deb_covid$animal.ID)
# #   
# #   covid_all <- c(covid) #c(covid,"Calu", "Cesar", "Caramel" )
# #   
# #   
# #   # select animal monitored outside covid period
# #   bird_winter_outcovid <- combined_telemetry %>% filter(!animal.ID %in% covid_all)
# #   
# #   unique(cut(bird_winter_outcovid$total.visitors.std,breaks  = quantile(bird_winter_outcovid$total.visitors.std, na.rm = TRUE)))
# #   
# #   bird_winter_outcovid$visitor_breaks <- cut(
# #     bird_winter_outcovid$total.visitors.std,
# #     breaks = quantile(bird_winter_outcovid$total.visitors.std, na.rm = TRUE),
# #     include.lowest = TRUE,
# #     labels = c("Low", "Medium", "High", "Very_high")
# #   )
# #   
# #   
# #   par(mfrow = c(1,2))
# #   
# #   boxplot(combined_telemetry$total.visitors.std) 
# #   text(y = boxplot.stats(combined_telemetry$total.visitors.std)$stats, labels = round(boxplot.stats(combined_telemetry$total.visitors.std)$stats,2), x = 1.3, pos = 2) 
# #   text(y = boxplot.stats(combined_telemetry$total.visitors.std)$stats, labels = boxplot.stats(combined_telemetry$total.visitors)$stats, x = 1.35, pos = 4)
# #   
# #   boxplot(bird_winter_outcovid$total.visitors.std) 
# #   text(y = boxplot.stats(bird_winter_outcovid$total.visitors.std)$stats, labels = round(boxplot.stats(bird_winter_outcovid$total.visitors.std)$stats,2), x = 1.3, pos = 2) 
# #   text(y = boxplot.stats(bird_winter_outcovid$total.visitors.std)$stats, labels = boxplot.stats(bird_winter_outcovid$total.visitors)$stats, x = 1.35, pos = 4)
# #   
# #   
# #   
# #   # Create one-hot encoded columns
# #   binary_m_visit_breaks_ <- model.matrix(~ visitor_breaks - 1, data = bird_winter_outcovid)
# #   binary_m_visit_breaks_ <- as.data.frame(binary_m_visit_breaks_)
# #   bird_winter_outcovid <- cbind(bird_winter_outcovid, binary_m_visit_breaks_)
# #   
# #   combined_telemetry <- left_join(combined_telemetry, bird_winter_outcovid %>% 
# #                                     dplyr::select(animal.ID, timestamp, visitor_breaks, visitor_breaksLow, visitor_breaksMedium, visitor_breaksHigh, visitor_breaksVery_high),
# #                                   by = c("animal.ID", "timestamp"))
# #   
# #   
# #   # Create a variable sl_open for ski lift opening hours (8:30am to 6:00pm)
# #   combined_telemetry <- combined_telemetry %>% mutate("time" = strftime(combined_telemetry$timestamp, format="%H:%M:%S"))
# #   combined_telemetry$sl_open <- ifelse(
# #     hms(combined_telemetry$time) > hms("08:30:00") & hms(combined_telemetry$time) < hms("18:00:00"),
# #     1,
# #     0
# #   )
# #   
# #   View(combined_telemetry)
# #   
# #   combined_telemetry2 <- as.telemetry(combined_telemetry, projection = "EPSG:2154",
# #                                       keep = c("saison", "saison2", "period_jour", "animal.sex", 
# #                                                "animal.life.stage", "total.visitors", "snow.depth",
# #                                                "total.visitors.std", "snow.depth.std",
# #                                                "visitor_breaksLow", "visitor_breaksMedium", "visitor_breaksHigh", "visitor_breaksVery_high",
# #                                                "sl_open"))
# #   
# #   # verify l_telemetry_winter[[i]][["all"]] and combined_telemetry2[[i]] contains the sam info : sum(l_telemetry_winter[[1]][[1]]$timestamp!=combined_telemetry2[[1]]$timestamp)
# #   
# #   # Replace the previous telemetry dataset to add the new variables
# #   for (i in seq_along(list_telemetry)) {
# #     list_telemetry[[i]][["all"]] <- combined_telemetry2[[i]]
# #   }
# #   
# #   return(list_telemetry)
# #   
# # }
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# 
# 
# 
# 
# 
# 
# 
# add_variables_visit_open <- function(data) 
# {
#   
#   # Combine into a single data frame with animal names added
#   data <- data %>% filter(saison=="hiver") %>% mutate("time" = strftime(timestamp, format="%H:%M:%S"))
#   
#   data$sl_open <- ifelse(
#     hms(data$time) > hms("08:30:00") & hms(data$time) < hms("18:00:00"),
#     1,
#     0
#   )
#   
#   # Extract numeric vector for non-zero visitors
#   # data2 <- data %>%
#   #   filter(total.visitors != 0) %>%
#   #   distinct(jour) %>%
#   #   dplyr::select(jour,total.visitors)
#   # 
#   vect_visitors <- data %>%
#     filter(!is.na(data$total.visitors) & data$total.visitors != 0) %>%
#     distinct(jour, total.visitors)
#   
#   # Identify quantile breaks excluding zero visitors
#   quantile_breaks <- quantile(
#     vect_visitors$total.visitors,
#     probs = seq(0, 1, length.out = 5),
#     na.rm = TRUE
#   )
#   
#   print(quantile_breaks)
#   
#   # Add 'visitor_breaks' column
#   data$visitor_breaks <- cut(
#     data$total.visitors,
#     breaks = c(0, quantile_breaks),  # Include 0 in the breaks
#     # breaks = c(min(data$total.visitors), quantile_breaks),
#     include.lowest = TRUE,
#     labels = c("Null", "Low", "Medium", "High", "Very_high"),
#     right = TRUE  # To ensure intervals are right-inclusive
#   )
#   
#   # Handle NA values by explicitly making them a separate factor level
#   data$visitor_breaks <- fct_explicit_na(data$visitor_breaks, na_level = "NA")
#   
#   # Create one-hot encoded columns
#   binary_m_visit_breaks <- model.matrix(~ visitor_breaks - 1, data = data)
#   binary_m_visit_breaks <- as.data.frame(binary_m_visit_breaks)
#   data <- cbind(data, binary_m_visit_breaks)
# 
#   
#   
#   # Plot data distribution of visitors with and without 0
#   
#   # Filter out NA and infinite values before computing min and creating the boxplot
#   cleaned_data <- birds_bg_dt2[!is.na(birds_bg_dt2$total.visitors.std) & is.finite(birds_bg_dt2$total.visitors.std), ]
#   
#   par(mfrow = c(1,2))
#   
#   boxplot(cleaned_data$total.visitors.std)
#   text(y = boxplot.stats(cleaned_data$total.visitors.std)$stats, labels = round(boxplot.stats(cleaned_data$total.visitors.std)$stats,2), x = 1.3, pos = 2) 
#   text(y = boxplot.stats(cleaned_data$total.visitors.std)$stats, labels = boxplot.stats(cleaned_data$total.visitors)$stats, x = 1.35, pos = 4)
#   title("Visitors distribution")
#   
#   # Now, create the boxplot excluding the minimum value
#   boxplot(cleaned_data[cleaned_data$total.visitors.std != min(cleaned_data$total.visitors.std), "total.visitors.std"])
#   text(y = boxplot.stats(cleaned_data[cleaned_data$total.visitors.std != min(cleaned_data$total.visitors.std), "total.visitors.std"])$stats, labels = round(boxplot.stats(cleaned_data[cleaned_data$total.visitors.std != min(cleaned_data$total.visitors.std), "total.visitors.std"])$stats,2), x = 1.3, pos = 2) 
#   text(y = boxplot.stats(cleaned_data[cleaned_data$total.visitors.std != min(cleaned_data$total.visitors.std), "total.visitors.std"])$stats, labels = boxplot.stats(cleaned_data[cleaned_data$total.visitors.std != min(cleaned_data$total.visitors.std), "total.visitors"])$stats, x = 1.35, pos = 4)
#   title("Visitors distribution exluding null values")
#    
#   return(data)
#   
# }
# #*******************************************************************
# 
# 
