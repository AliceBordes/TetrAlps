#### PhD Tetralpes project ####

# Alice Bordes #

# September 2024 #

# Description:

# coordXY function plot standardized longitude and latitude over time for each animal, and display affiliated seasons according to Marc settings : 
# season dates : 15 sept to 14 nov ; 15 nov to 14 feb ; 15 feb to 14 jun ; 15 jun to 14 sept (TO CHECK IN OFB COMPUTER)

coordXY <- function(vect_names, 
                    write = FALSE,
                    outputfolder = file.path(base,"TetrAlps/5_OUTPUTS/data_exploration/season_segmentation"))
{
  for (ani in seq_along(vect_names)) # for all animals from vect_names 
  {
    # selection of the data for the bird = ani
    list_of_animals = vect_names[ani]
    bird_dt <- birds_bg_dt %>% filter(animal.ID %in% list_of_animals) 
    bird_sf <- st_as_sf(bird_dt, coords = c("location.long", "location.lat"), crs = 4326, remove = FALSE)
    
    
    # Display predefined season dates : 15 sept to 14 nov ; 15 nov to 14 feb ; 15 feb to 14 jun ; 15 jun to 14 sept (TO CHECK IN OFB COMPUTER)
    season_dates <- as.data.frame(bird_sf %>% select(saison, saison2,study.local.timestamp) %>% group_by(saison2) %>% slice(1,n())) %>% select(-geometry)
    odd_numbers <- seq(1, nrow(season_dates), by = 2)
    even_numbers <- seq(2, nrow(season_dates), by = 2)

    bird_dt$location.long <- as.numeric(bird_dt$location.long)
    bird_dt$location.lat <- as.numeric(bird_dt$location.lat)  
    
    
    # Create a data frame for the segments
    h_segment <- max((bird_dt$location.long-mean(bird_dt$location.long))/sd(bird_dt$location.long),(bird_dt$location.lat-mean(bird_dt$location.lat))/sd(bird_dt$location.lat)) - 1
    segments_df <- data.frame(
      x1 = as.POSIXct(season_dates[odd_numbers, "study.local.timestamp"]),
      xend = as.POSIXct(season_dates[even_numbers, "study.local.timestamp"]),
      y1 = h_segment,
      yend = h_segment,
      season = season_dates[odd_numbers, "saison"]  # Add season names here
    )
    
    
    
    plot_coord <- ggplot()+
      geom_point(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                                     y = (bird_dt$location.long-mean(bird_dt$location.long))/sd(bird_dt$location.long),
                                     color = "Longitude coordinate" ),
                 size= 0.3, alpha = 0.5)+
      geom_point(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                                     y = (bird_dt$location.lat-mean(bird_dt$location.lat))/sd(bird_dt$location.lat),
                                     color = "Latitude coordinate"), 
                 size= 0.3, alpha = 0.5)+
      geom_smooth(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                                      y = (bird_dt$location.long-mean(bird_dt$location.long))/sd(bird_dt$location.long),
                                      color = "Longitude coordinate"),
                  method = "gam")+
      geom_smooth(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                                      y = (bird_dt$location.lat-mean(bird_dt$location.lat))/sd(bird_dt$location.lat),
                                      color = "Latitude coordinate"), 
                  method = "gam")+
      geom_segment(data = segments_df, 
                   aes(x = x1, y = y1, xend = xend, yend = yend, color = season), size = 2)+
      scale_color_manual(values = c("Latitude coordinate" = "coral",
                                    "Longitude coordinate" = "turquoise3",
                                    "printemps" = "seagreen3",
                                    "ete" = "deeppink",
                                    "automne" = "orange",
                                    "hiver" = "blue"
      ),
      labels = c("Longitude coordinate" = "X coordinate (longitude)",
                 "Latitude coordinate" = "Y coordinate (latitude)",
                 "printemps" = "Spring",
                 "ete" = "Summer",
                 "automne" = "Autumn",
                 "hiver" = "Winter"
      ),
      breaks = c("printemps", "ete", "automne", "hiver","Longitude coordinate", "Latitude coordinate"))+
      geom_vline(data = segments_df, aes(xintercept = as.numeric(x1)), color = "black", linetype = "solid", size = 0.5)+
      labs(title = paste0("Spatial coordinates of the bird named ",list_of_animals," over time"),
           x = "Local timestamp of the study",
           y = "Standardised X and Y coordinates",
           color = "")+
      theme(axis.title = element_text(size = 14),
            plot.title = element_text(size = 16))+
      # Customize legends using guides
      guides(color = guide_legend(override.aes = list(size = 3, linetype = 1),
                                  title = "Seasons and coordinates"))
    
    ggsave(plot = plot_coord,
           filename = file.path(base,"TetrAlps/5_OUTPUTS/data_exploration/season_segmentation",paste0(vect_names[ani],".png")),
           width = 25 , height = 15 ,units = "cm")
  } #end for
  
  return(plot_coord)
}





coordXY_multiple_bg <- function(vect_names,
                                letter = FALSE,
                                write = FALSE,
                                outputfolder = file.path(base,"TetrAlps/5_OUTPUTS/data_exploration/season_segmentation/by_bunch"))
{
  if(letter == FALSE)
  {
    list_of_animals = vect_names
  }else{
    letter <- paste0("^",letter)
    animals_starting_with_Letter <- vect_names[grep(letter, vect_names, ignore.case = TRUE)]
    list_of_animals = animals_starting_with_Letter
  }
  
  bird_dt <- birds_bg_dt %>% filter(animal.ID %in% list_of_animals)
  bird_sf <- st_as_sf(bird_dt, coords = c("location.long", "location.lat"), crs = 4326, remove = FALSE)
  
  bird_dt$location.long <- as.numeric(bird_dt$location.long)
  bird_dt$location.lat <- as.numeric(bird_dt$location.lat)  
  
  # Standardize coordinates per animal
  bird_dt <- bird_dt %>%
    group_by(animal.ID) %>%
    mutate(
      standardized_long = (location.long - mean(location.long, na.rm = TRUE)) / sd(location.long, na.rm = TRUE),
      standardized_lat = (location.lat - mean(location.lat, na.rm = TRUE)) / sd(location.lat, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Display predefined season dates : 15 sept to 14 nov ; 15 nov to 14 feb ; 15 feb to 14 jun ; 15 jun to 14 sept (TO CHECK IN OFB COMPUTER)
  season_dates <- data.frame(month = c("Spring","Spring","Summer","Summer","Autumn","Autumn","Winter","Winter"), 
                             monthday = c("03-16","06-15","06-16","09-15","09-16","12-15","12-16","03-15"))
  
  # Calculate minimum and maximum timestamps
  min_timestamp <- min(as.POSIXct(bird_sf$study.local.timestamp), na.rm = TRUE)
  max_timestamp <- max(as.POSIXct(bird_sf$study.local.timestamp), na.rm = TRUE)
  
  year_bg_min <- lubridate::year(min_timestamp)
  year_bg_max <- lubridate::year(max_timestamp)
  
  season_dates <- sapply(season_dates, rep.int, times = (year_bg_max-year_bg_min)+1)
  season_dates <- as.data.frame(season_dates)
  season_dates$year <- as.numeric(year_bg_min)
  
  
  # Calculate the number of blocks of 8 rows
  n_blocks <- nrow(season_dates) /8
  
  # Loop through each block and adjust the year
  for (y in 1:n_blocks) 
  {
    # Define the start and end index for each block of 7
    start_index <- (y - 1) * 8 + 1
    end_index <- start_index + 7
    
    # Increment the year for this block
    season_dates$year[start_index:end_index] <- season_dates$year[start_index] + (y - 1)
    season_dates$year[y*8] <- season_dates$year[y*8] + 1
  }
  
  # Combine 'year' and 'monthday' to create a proper Date column
  season_dates <- season_dates %>%
    mutate(date = as.Date(paste(year, monthday, sep = "-")))
  
  odd_numbers <- seq(1, nrow(season_dates), by = 2)
  even_numbers <- seq(2, nrow(season_dates), by = 2)
  
  
  # Create start and end points for each segment
  season_dates2 <- data.frame(start_date = as.POSIXct(season_dates$date[odd_numbers]),
                              end_date = as.POSIXct(season_dates$date[even_numbers]),
                              season = season_dates$month[even_numbers])
  
  
  plot_coord <- ggplot()+
                # geom_point(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                #                                y = (bird_dt$location.long-mean(bird_dt$location.long))/sd(bird_dt$location.long),
                #                                color = "Longitude coordinate" ),
                #            size= 0.3, alpha = 0.5)+
                # geom_point(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                #                                y = (bird_dt$location.lat-mean(bird_dt$location.lat))/sd(bird_dt$location.lat),
                #                                color = "Latitude coordinate"), 
                #            size= 0.3, alpha = 0.5)+
                geom_smooth(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                                                y = standardized_long,
                                                color = bird_dt$animal.ID,
                                                linetype = "Longitude coordinate"), size = 0.5,
                            method = "gam")+
                geom_smooth(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                                                y = standardized_lat,
                                                color = bird_dt$animal.ID,
                                                linetype = "Latitude coordinate"), size = 0.5, 
                            method = "gam")+
                scale_linetype_manual(values = c("Latitude coordinate" = "solid",
                                                 "Longitude coordinate" = "dashed"))+
                labs(color = "Animals")+
                new_scale_color()+
                
                
                geom_segment(data = season_dates2, 
                             aes(x = start_date, 
                                 y = max((bird_dt$location.long-mean(bird_dt$location.long))/sd(bird_dt$location.long),(bird_dt$location.lat-mean(bird_dt$location.lat))/sd(bird_dt$location.lat)), 
                                 xend = end_date, 
                                 yend = max((bird_dt$location.long-mean(bird_dt$location.long))/sd(bird_dt$location.long),(bird_dt$location.lat-mean(bird_dt$location.lat))/sd(bird_dt$location.lat)), 
                                 color = season), size = 2)+
                scale_color_manual(values = c("Spring" = "seagreen3",
                                              "Summer" = "deeppink",
                                              "Autumn" = "orange",
                                              "Winter" = "blue"
                ),
                labels = c("Spring" = "Spring",
                           "Summer" = "Summer",
                           "Autumn" = "Autumn",
                           "Winter" = "Winter"
                ),
                breaks = c("Spring", "Summer", "Autumn", "Winter"))+
                
                geom_vline(data = season_dates2, aes(xintercept = as.numeric(start_date)), color = "darkgrey", linetype = "solid", size = 0.5)+
                labs(title = paste0("Spatial coordinates over time for a bunch of birds"),
                     x = "Local timestamp of the study",
                     y = "Standardised X and Y coordinates",
                     color = "Seasons",
                     linetype = "Coordinates")+
                theme(axis.title = element_text(size = 14),
                      plot.title = element_text(size = 16))+
    
    facet_grid(animal.life.stage~animal.sex)
              
  if(write == TRUE)
  {
    ggsave(filename = file.path(outputfolder,paste0("letter_",letter,".png")),
           width = 25 , height = 15 ,units = "cm")
  }
  
  return(plot_coord)
}






brkpts_coordXY <- function(
                    vect_names, 
                    threshold,
                    write = FALSE,
                    outputfolder = file.path(base,"TetrAlps/5_OUTPUTS/data_exploration/season_segmentation/breakpoints"))
{
    # Initialize the rupture_pts data frame with columns and no rows
    rupture_pts <- data.frame(pts = integer(), timestamp = as.POSIXct(character()), season = character(), animal = character()) # ,stringsAsFactors = FALSE in option
    
  for (ani in seq_along(vect_names)) # for all animals from vect_names 
  {
    
    
    # Iterate over animals and find rupture points
      bird_dt <- birds_bg_dt %>% filter(animal.ID %in% vect_names[ani])
      bird_dt$location.long <- as.numeric(bird_dt$location.long)
      bird_dt$location.lat <- as.numeric(bird_dt$location.lat)
      bird_dt$study.local.timestamp <- as.POSIXct(bird_dt$study.local.timestamp)
      
      # Standardize coordinates per animal
      animal_dt <- bird_dt %>% filter(animal.ID==vect_names[ani]) %>%
        mutate(
          standardized_long = (location.long - mean(location.long, na.rm = TRUE)) / sd(location.long, na.rm = TRUE),
          standardized_lat = (location.lat - mean(location.lat, na.rm = TRUE)) / sd(location.lat, na.rm = TRUE))
  
      for(i in (1:(nrow(animal_dt)-9)))
      {
        if(   (sd(animal_dt$standardized_long[i:(i+9)])  >  (threshold *sd(animal_dt$standardized_long)))
           || (sd(animal_dt$standardized_lat[i:(i+9)])  >  (threshold *sd(animal_dt$standardized_lat))) )
        {

          # Create a new row to add to rupture_pts
          new_row <- data.frame(
            pts = i,
            timestamp = animal_dt$study.local.timestamp[i+4],
            season = format(animal_dt$study.local.timestamp[i+4], "%B"),
            animal = animal_dt$animal.ID[i])
          
          # Append the new row to rupture_pts
          rupture_pts <- rbind(rupture_pts, new_row)
          
        }
      }
    # } # end for(ani in seq_along(vect_names))
    
    # Sort and group timestamps that are less than 10 days apart for each animal
    rupture_pts <- rupture_pts %>%
      arrange(animal, timestamp) %>%
      group_by(animal) %>%
      mutate(
        # Create a grouping variable that increments when the time gap between timestamps is 10 days or more
        cluster = cumsum(
          c(0, diff(as.numeric(timestamp)) >= 10 * 24 * 60 * 60) # 10 days in seconds
        )
      )
    
    # Keep only the mean timestamp of each cluster
    rupture_pts_filtered <- rupture_pts %>%
      group_by(animal, cluster) %>%
      summarize(
        pts = mean(pts),  # Calculate mean of 'pts' for reference, though it may not be as relevant here
        timestamp = as.POSIXct(mean(as.numeric(timestamp)), origin = "1970-01-01"),  # Mean timestamp calculation
        season = format(timestamp, "%B")  # Update season based on the mean timestamp
      ) %>%
      ungroup()
    
    
    # rupture_pts_start <- rupture_pts %>% filter(season %in% c("octobre","novembre","decembre"))
    # rupture_pts_end <- rupture_pts %>% filter(season %in% c("mars","avril","mai","juin"))
    # 
    # rupture_pts_winter_start_mean <- rupture_pts_start %>% group_by(animal) %>% summarize(mean(timestamp))
    # rupture_pts_winter_end_mean <- rupture_pts_end %>% group_by(animal) %>% summarize(mean(timestamp))
    # 
    # l <- list(winter_start = rupture_pts_start, winter_end = rupture_pts_end)
    
    
  # } # end for(ani in seq_along(vect_names))
    
    
    
    
 # for (ani in seq_along(vect_names)) # for all animals from vect_names 
 #  {    
    
    # selection of the data for the bird = ani
    list_of_animals = vect_names[ani]
    bird_dt <- birds_bg_dt %>% filter(animal.ID %in% list_of_animals) 
    bird_sf <- st_as_sf(bird_dt, coords = c("location.long", "location.lat"), crs = 4326, remove = FALSE)
    
    
    # Get rupture points for the current animal 
    animal_rupture_pts <- rupture_pts_filtered %>% filter(animal == list_of_animals)
    print(animal_rupture_pts)
    
    # Display predefined season dates : 15 sept to 14 nov ; 15 nov to 14 feb ; 15 feb to 14 jun ; 15 jun to 14 sept (TO CHECK IN OFB COMPUTER)
    season_dates <- as.data.frame(bird_sf %>% select(saison, saison2,study.local.timestamp) %>% group_by(saison2) %>% slice(1,n())) %>% select(-geometry)
    odd_numbers <- seq(1, nrow(season_dates), by = 2)
    even_numbers <- seq(2, nrow(season_dates), by = 2)
    
    bird_dt$location.long <- as.numeric(bird_dt$location.long)
    bird_dt$location.lat <- as.numeric(bird_dt$location.lat)  
    
    
    # Create a data frame for the segments
    h_segment <- max((bird_dt$location.long-mean(bird_dt$location.long))/sd(bird_dt$location.long),(bird_dt$location.lat-mean(bird_dt$location.lat))/sd(bird_dt$location.lat)) - 1
    segments_df <- data.frame(
      x1 = as.POSIXct(season_dates[odd_numbers, "study.local.timestamp"]),
      xend = as.POSIXct(season_dates[even_numbers, "study.local.timestamp"]),
      y1 = h_segment,
      yend = h_segment,
      season = season_dates[odd_numbers, "saison"]  # Add season names here
    )
    
    
    
    plot_coord <- ggplot()+
      geom_point(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                                     y = (bird_dt$location.long-mean(bird_dt$location.long))/sd(bird_dt$location.long),
                                     color = "Longitude coordinate" ),
                 size= 0.3, alpha = 0.5)+
      geom_point(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                                     y = (bird_dt$location.lat-mean(bird_dt$location.lat))/sd(bird_dt$location.lat),
                                     color = "Latitude coordinate"), 
                 size= 0.3, alpha = 0.5)+
      geom_smooth(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                                      y = (bird_dt$location.long-mean(bird_dt$location.long))/sd(bird_dt$location.long),
                                      color = "Longitude coordinate"),
                  method = "gam")+
      geom_smooth(data = bird_dt, aes(x = as.POSIXct(study.local.timestamp), 
                                      y = (bird_dt$location.lat-mean(bird_dt$location.lat))/sd(bird_dt$location.lat),
                                      color = "Latitude coordinate"), 
                  method = "gam")+
      geom_segment(data = segments_df, 
                   aes(x = x1, y = y1, xend = xend, yend = yend, color = season), size = 2)+
      scale_color_manual(values = c("Latitude coordinate" = "coral",
                                    "Longitude coordinate" = "turquoise3",
                                    "printemps" = "seagreen3",
                                    "ete" = "deeppink",
                                    "automne" = "orange",
                                    "hiver" = "blue"
      ),
      labels = c("Longitude coordinate" = "X coordinate (longitude)",
                 "Latitude coordinate" = "Y coordinate (latitude)",
                 "printemps" = "Spring",
                 "ete" = "Summer",
                 "automne" = "Autumn",
                 "hiver" = "Winter"
      ),
      breaks = c("printemps", "ete", "automne", "hiver","Longitude coordinate", "Latitude coordinate"))+
      geom_vline(data = segments_df, aes(xintercept = as.numeric(x1)), color = "black", linetype = "solid", size = 0.5)+
      geom_vline(data = animal_rupture_pts, aes(xintercept = as.numeric(timestamp)), color = "red", linetype = "dotted", size = 1) + # Add vertical lines for rupture points
      labs(title = paste0("Spatial coordinates of the bird named ",list_of_animals," over time"),
           x = "Local timestamp of the study",
           y = "Standardised X and Y coordinates",
           color = "")+
      theme(axis.title = element_text(size = 14),
            plot.title = element_text(size = 16))+
      # Customize legends using guides
      guides(color = guide_legend(override.aes = list(size = 3, linetype = 1),
                                  title = "Seasons and coordinates"))
    
    plot_coord
    
    ggsave(plot = plot_coord,
           filename = file.path(outputfolder,paste0(vect_names[ani],".png")),
           width = 25 , height = 15 ,units = "cm")
  } #end for
  
    print(animal_rupture_pts)
  return(plot_coord)
}

