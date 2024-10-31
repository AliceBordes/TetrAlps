################################################


#### PhD Tetras_test project ####

# Alice Bordes #

# April 2023 #

######################################################################

# Description:

# to visualize home-ranges of birds according several graphic options

######################################################################

multi_graph_HR <- function(telemetry_object,akde_object,outputfile=getwd(),writeplot = FALSE, proj = "+init=epsg:2154") {
  
  if (writeplot == TRUE) {
    
### GRAPHS OPTIONS
    
  # Background options
  mnt_9_graph <- project(mnt_9, y = proj)
  borders_3V_vect_graph <- project(borders_3V_vect, y = proj)
  strava_graph <- project(strava, y = proj)
  
  # Extent
  e_lambert93 <- e
  e_poly_lambert93 <- as.polygons(ext(e_lambert93), crs = "EPSG:2154")
  e_poly_lambert93 <- project(e_poly_lambert93, y = proj)
  e <- as.numeric(as.vector(ext(e_poly_lambert93)))
  
  # Season colors
  season_colors <- list(
    hiver1 = "blue",
    hiver2 = "#0099FF",
    hiver3 = "#99FFFF",
    automne1 = "orange",
    automne2 = "#FFCC66",
    automne3 = "#CCCC99",
    automne4 = "#CCCCCC",
    printemps1 = "springgreen3",
    printemps2 = "#66FF33",
    printemps3 = "#CCFF33",
    printemps4 = "springgreen",
    ete1 = "#FF6666",
    ete2 = "#FFCCCC",
    ete3 = "#CC0033",
    ete4 = "#CC9999"
  )
  
  }
  
  # DATA
  if (proj == "+proj=longlat +datum=WGS84") {
    coordsyst <- sub(".*datum=([^ ]*)", "\\1", proj)
    coordsyst_d <- paste0("_", coordsyst)
  } else if (proj == "+init=epsg:2154") {
    coordsyst <- "Lambert93"
    coordsyst_d <- ""
  }
  
  data_telemetry <- telemetry_object
  data_akde <- akde_object
  
  main_seasons <- c("hiver", "automne", "printemps", "ete")
  
  
  # Initialize lists to store polygons by main season category
  season_polygons <- list(
    hiver = list(),
    automne = list(),
    printemps = list(),
    ete = list()
  )
  
  # Calculate the area for each sub-season and mean area for each main season
  calculate_areas <- function(data_akde, main_seasons) {
    areas <- list()
    
    for (bird in seq_along(data_akde)) {
      bird_areas <- list()
      for (main_season in main_seasons) {
        # Extract areas for all sub-seasons of the current main season
        sub_season_areas <- unlist(lapply(names(data_akde[[bird]]), function(m) {
          if (sub("\\d", "", m) == main_season) {
            # Ensure the object is appropriate for summary
            if (!is.null(data_akde[[bird]][[m]])) {
              summary_obj <- summary(data_akde[[bird]][[m]], unit = FALSE)
              if (is.list(summary_obj) && "CI" %in% names(summary_obj)) {
                return(summary_obj$CI[,"est"] / 1000000)
              }
            }
          }
          return(NULL)
        }))
        
        # Calculate the mean area for the current main season if there are any areas
        if (length(sub_season_areas) > 0) {
          mean_area <- mean(sub_season_areas, na.rm = TRUE)
          bird_areas[[main_season]] <- list(sub_season_areas = sub_season_areas, mean_area = mean_area)
        } 
      }  # end loop main_seasons
      areas[[bird]] <- bird_areas
    }  # end loop bird 
    return(areas)
  } # end function calculate_areas
  
  
  
  # Calculate the confidence intervals for each main season
  #LOW
  calculate_confidence_intervals_low <- function(data_akde, main_seasons) {
    confidence_intervals_low <- list()
    
    for (bird in seq_along(data_akde)) {
      bird_intervals_low <- list()
      for (main_season in main_seasons) {
        # Extract areas for all sub-seasons of the current main season
        sub_season_low_CI <- unlist(lapply(names(data_akde[[bird]]), function(m) {
          if (sub("\\d", "", m) == main_season) {
            # Ensure the object is appropriate for summary
            if (!is.null(data_akde[[bird]][[m]])) {
              summary_obj <- summary(data_akde[[bird]][[m]], unit = FALSE)
              if (is.list(summary_obj) && "CI" %in% names(summary_obj)) {
                return(summary_obj$CI[,"low"] / 1000000)
              }
            }
          }
          return(NULL)
        }))
        
        # Calculate the mean area for the current main season if there are any areas
        if (length(sub_season_low_CI) > 0) {
          low_CI_mean <- mean(sub_season_low_CI, na.rm = TRUE)
          bird_intervals_low[[main_season]] <- list(sub_season_low_CI = sub_season_low_CI, low_CI_mean = low_CI_mean)
        }
      } # end loop main_seasons
      confidence_intervals_low[[bird]] <- bird_intervals_low
    } # end loop bird 
    return(confidence_intervals_low)
  } # end function calculate_confidence_intervals_low
  
  #HIGH
  calculate_confidence_intervals_high <- function(data_akde, main_seasons) {
    confidence_intervals_high <- list()
    
    for (bird in seq_along(data_akde)) {
      bird_intervals_high <- list()
      for (main_season in main_seasons) {
        # Extract areas for all sub-seasons of the current main season
        sub_season_high_CI <- unlist(lapply(names(data_akde[[bird]]), function(m) {
          if (sub("\\d", "", m) == main_season) {
            # Ensure the object is appropriate for summary
            if (!is.null(data_akde[[bird]][[m]])) {
              summary_obj <- summary(data_akde[[bird]][[m]], unit = FALSE)
              if (is.list(summary_obj) && "CI" %in% names(summary_obj)) {
                return(summary_obj$CI[,"high"] / 1000000)
              }
            }
          }
          return(NULL)
        })) 
        
        # Calculate the mean area for the current main season if there are any areas
        if (length(sub_season_high_CI) > 0) {
          high_CI_mean <- mean(sub_season_high_CI, na.rm = TRUE)
          bird_intervals_high[[main_season]] <- list(sub_season_high_CI = sub_season_high_CI, high_CI_mean = high_CI_mean)
        }
      } # end loop main_seasons
      confidence_intervals_high[[bird]] <- bird_intervals_high
    } # end loop bird
    return(confidence_intervals_high)
  } # end function calculate_confidence_intervals_high
  
  
  plots_mnt_capture <- list()
  areas <- calculate_areas(data_akde, main_seasons)
  confidence_intervals_low <- calculate_confidence_intervals_low(data_akde, main_seasons)
  confidence_intervals_high <- calculate_confidence_intervals_high(data_akde, main_seasons)
  
  

  
  
  
  #STATISTICS ON HOME RANGE
  
  # Initialize empty data frame to store results
  summary_df <- data.frame(
    Bird.ID = character(),
    Season = character(),
    Sex = character(),
    Mean_Area = numeric(),
    CI_Low = numeric(),
    CI_High = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Iterate over birds
  for (bird in seq_along(data_akde)) {
    
    # Graph title options
    if (any(data_telemetry[[bird]][[1]]$animal.sex == "male")) {
      sexe_ani <- "male"
      sex_color <- "turquoise3"
    } else if (any(data_telemetry[[bird]][[1]]$animal.sex == "femelle")) {
      sexe_ani <- "female"
      sex_color <- "deeppink"
    } else {
      sexe_ani <- "unknown"
      sex_color <- "gray"
    }
    
    
    # Iterate over main seasons
    for (main_season in main_seasons) {
      # Get mean area, low CI, and high CI
      mean_area <- NA
      low_CI <- NA
      high_CI <- NA
      
      if (!is.null(areas[[bird]][[main_season]])) {
        mean_area <- areas[[bird]][[main_season]]$mean_area
        low_CI <- confidence_intervals_low[[bird]][[main_season]]$low_CI
        high_CI <- confidence_intervals_high[[bird]][[main_season]]$high_CI
      }
      

      
      # Add row to summary data frame
      summary_df <- rbind(summary_df, data.frame(
        Bird.ID = data_akde[[bird]][[1]]@info$identity,
        Season = main_season,
        Sex = sexe_ani,
        Mean_Area = mean_area,
        CI_Low = low_CI,
        CI_High = high_CI
      ))
    }
  }
  
  # Print summary data frame
  View(summary_df)
  
  
  
  
  
  
  
  # HOME RANGE POLYGON CREATION 
if (writeplot == TRUE) {
  
  
  poly_95_bird_list <- list()
  
  for (bird in seq_along(data_akde)) {
    
    # Graph title options
    if (any(data_telemetry[[bird]][[1]]$animal.sex == "male")) {
      sexe_ani <- "male"
      sex_color <- "turquoise3"
    } else if (any(data_telemetry[[bird]][[1]]$animal.sex == "femelle")) {
      sexe_ani <- "female"
      sex_color <- "deeppink"
    } else {
      sexe_ani <- "unknown"
      sex_color <- "gray"
    }
    
    
    seasons <- unique(birds_bg_dt$saison2)
    
    # create a nested list
    poly_95_bird_list[[bird]] <- list()
    
    for (s in seasons) {
      if (!is.null(data_akde[[bird]]) && !is.null(data_akde[[bird]][[s]]) && inherits(data_akde[[bird]][[s]], "UD")) {
        poly_95_bird <- SpatialPolygonsDataFrame.UD(data_akde[[bird]][[s]], level.UD = 0.95, level = 0.95)
        crs(poly_95_bird) <- "+init=epsg:2154"
        poly_95_bird <- st_as_sf(poly_95_bird)
        poly_95_bird_list[[bird]][[s]] <- poly_95_bird
      }
    } #end seasons
    
    # Debugging output
    # print(paste0("Bird ", bird, ": ", length(poly_95_bird_list[[bird]]), " polygons created"))


      
      # Plot creation
      print(paste("Creating graphs for bird", bird))
      
      
      for (main_season in main_seasons) {
        season_polygons <- lapply(names(poly_95_bird_list[[bird]]), function(m) {
          if (sub("\\d", "", m) == main_season) {
            return(list(poly_95_bird_list[[bird]][[m]], season_colors[[m]]))
          } else {
            return(NULL)
          }
        }) # end function
        
        season_polygons <- Filter(Negate(is.null), season_polygons)
        
        season_text <- switch(main_season,
                              "hiver" = "winter",
                              "automne" = "autumn",
                              "printemps" = "spring",
                              "ete" = "summer"
                              )
        
        if (!is.null(areas[[bird]][[main_season]])) {
          season_areas <- areas[[bird]][[main_season]]$sub_season_areas
          season_mean_area <- areas[[bird]][[main_season]]$mean_area
        } else {
          season_areas <- NULL
          season_mean_area <- NULL
        }
        
        
        if (!is.null(confidence_intervals_low[[bird]][[main_season]])&&!is.null(confidence_intervals_high[[bird]][[main_season]])) {
          low_CI <- confidence_intervals_low[[bird]][[main_season]]$sub_season_low_CI
          high_CI <- confidence_intervals_high[[bird]][[main_season]]$sub_season_high_CI
        } else {
          low_CI <- NULL
          high_CI <- NULL
        }
        
        
        
        subtitle_text <- ""
        if (!is.null(season_mean_area)) {
          subtitle_text <- paste0("Mean ", season_text, " = ", round(season_mean_area, 2), " km^2","    CI=[", round(low_CI, 2), " - ", round(high_CI, 2), "]")
        }
        if (!is.null(season_areas) && length(season_areas) > 0) {
          subtitle_text <- paste0(subtitle_text,"\n",paste(season_text,seq_along(season_areas), "=", round(season_areas, 2), collapse = ", "))
        }
        
        g_mnt_capture <- ggplot() +
          geom_spatraster(data = mnt_9_graph, alpha = 0.8) +
          geom_sf(data = borders_3V_vect_graph, fill = NA, color = "black", lwd = 1) +
          scale_fill_gradientn(name = "Altitude (m)", colors = c("#CCFFCC", "#FFFFCC", "#FFCC99", "#FF9966", "#FF6600")) +
          coord_sf(xlim = c(e[2], e[1]), ylim = c(e[3], e[4])) +
          labs(title = paste0(str_to_title(data_akde[[bird]][[1]]@info$identity), " (", sexe_ani, ") ", season_text, " home range at 95%"),
               x = "Longitude",
               y = "Latitude",
               fill = "Altitude (m)",
               subtitle = subtitle_text) +
          new_scale_fill() +
          theme(plot.title = element_text(colour = sex_color,margin = margin(1, 0, 0, 0)),plot.subtitle = element_text(margin = margin(8, 0, 0, 0)),
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
        
        
        for (sp in season_polygons) {
          g_mnt_capture <- g_mnt_capture +
            geom_sf(data = sp[[1]], fill = sp[[2]], alpha = 0.4)+
            scale_fill_manual(name = "Seasons", values = sp[[2]], labels = names(sp[[1]]), guide = "legend")
        }
        
        
        plots_mnt_capture[[main_season]] <- g_mnt_capture
      } # end main_seasons
      
      combined_plot <- grid.arrange(grobs = plots_mnt_capture, ncol = 2)
      ggsave(paste0(outputfile,data_akde[[bird]][[1]]@info$identity,"_home_range_season.png"), plot = combined_plot, width = 15, height = 10)

    } # end loop on bird

  } #end (if writeplot==TRUE)  
  
  if (dev.cur() != 1) { dev.off() } # Ensure to close any open graphic devices
  
  return(summary_df)
}

