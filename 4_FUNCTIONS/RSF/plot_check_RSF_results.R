#### PhD Tetras_test project ####

# Alice Bordes #

# June 2023 #

# Description:

# Plot check_results RSF_function

#*************************************************************


plot_check_RSF_res<-function(telemetry_data,akde_data,raster,analysis_object,data_visu="discrete",writeplot=FALSE)
{

  
  # Loading data 
  data_telemetry <- telemetry_data
  data_akde <- akde_data
  
    # Graph title options
  if (data_telemetry$animal.sex[1] == "male") {
    sexe_ani <- "male"
    sex_color <- "turquoise3"
  } else if (data_telemetry$animal.sex[1] == "femelle") {
    sexe_ani <- "female"
    sex_color <- "deeppink"
  } else {
    sexe_ani <- "unknown"
    sex_color <- "gray"
  }
  
  if(str_contains(data_telemetry$saison2[1],"hiver"))
  {
    season_text="winter"
  }
  if(str_contains(data_telemetry$saison2[1],"automne"))
  {
    season_text="autumn"
  }
  if(str_contains(data_telemetry$saison2[1],"ete"))
  {
    season_text="summer"
  }
  if(str_contains(data_telemetry$saison2[1],"printemps"))
  {
    season_text="spring"
  }
  
  # Background options 
  if(raster=="habitats")
  {
    raster_object <- rast(env_RL_list[["carto_habitats_winter"]])
  }
  
  if(raster=="strava")
  {
    raster_object<- rast(env_RL_list[["strava"]])
  }
  
  
  
  #extract the polygon shape of the akde (https://groups.google.com/g/ctmm-user/c/wtBXI4P7-7k)
  poly_95_bird<-SpatialPolygonsDataFrame.UD(data_akde,level.UD=0.95,level=0.95)
  crs(poly_95_bird)<-"EPSG:2154"
  poly_95_bird<-st_as_sf(poly_95_bird)
  
  #set names to plot home range incertitude
  low <- poly_95_bird$name[1]
  high <- poly_95_bird$name[3]
  est <- poly_95_bird$name[2]
  
  #subset the CI's and extract shape of middle contour
  poly_95_bird_est <- subset(poly_95_bird, name == est)
  poly_95_bird_low <- subset(poly_95_bird, name == low)
  poly_95_bird_high <- subset(poly_95_bird, name == high)
  
  # Extract the bounding box coordinates
  bbox <- st_bbox(poly_95_bird)
  # Create a polygon from the bounding box coordinates
  bbox_polygon <- st_as_sfc(st_bbox(poly_95_bird))
  # Convert the polygon to an sf object
  bbox_polygon_sf <- st_sf(geometry = bbox_polygon) 
  # Assign CRS to the bounding box polygon
  st_crs(bbox_polygon_sf) <- st_crs(poly_95_bird)
  
  # Ensure CRS match
  raster_object <- terra::project(raster_object, crs(poly_95_bird))
  
  # crop the raster to the polygon extents
  cropped_r <- crop(raster_object, extent(poly_95_bird))
      
  # Ensure CRS match
  cropped_r <- terra::project(cropped_r, crs(poly_95_bird))
  
  
  if(raster=="habitats")
  {
    
    # RSF ON HABITAT 
    
    
    if(analysis_object=="HR")
    {

      
      # Mask the raster using the polygon to get the values within the polygon only
      # masked_raster <- mask(cropped_r, poly_95_bird)
      
      
      # Plot the mask
      g_mask_simple<-ggplot()+
        geom_spatraster(data=cropped_r)+
        scale_fill_manual(name = "Habitat classes",
            values = c("1"="#CCCCCC","2"="#FF99CC","3"="#99FF99","4"="#993300","5"="#99CCFF"),na.value ="transparent",
            labels = c("1"="Soils and low vegetation","2"="Shrubs","3"="Trees","4"="Buildings","5"="Cliffs and water"))+
        coord_sf(xlim = c(st_bbox(poly_95_bird)["xmin"], st_bbox(poly_95_bird)["xmax"]),
                 ylim = c(st_bbox(poly_95_bird)["ymin"], st_bbox(poly_95_bird)["ymax"]))+
        labs( title=paste("Habitat cartography"),
              x = "Longitude",
              y = "Latitude",
              fill = "Legend")
      
      g_mask_telemetry<-ggplot()+
        geom_spatraster(data=cropped_r)+
        scale_fill_manual(name = "Habitat classes",
            values = c("1"="#CCCCCC","2"="#FF99CC","3"="#99FF99","4"="#993300","5"="#99CCFF"),na.value ="transparent",
            labels = c("1"="Soils and low vegetation","2"="Shrubs","3"="Trees","4"="Buildings","5"="Cliffs and water"))+
        labs( title=paste("Habitat cartography"),
              x = "Longitude",
              y = "Latitude",
              fill = "Legend")+
        coord_sf(xlim = c(st_bbox(poly_95_bird)["xmin"], st_bbox(poly_95_bird)["xmax"]),
                 ylim = c(st_bbox(poly_95_bird)["ymin"], st_bbox(poly_95_bird)["ymax"]))+
        geom_point(data=data_telemetry,aes(x=longitude,y=latitude))+
        geom_sf(data = poly_95_bird_est, color = "blue", fill = "blue", alpha = 0.1) +  # incertitude: 95% low 
        geom_sf(data=poly_95_bird_low,color="black",fill=NA)+
        geom_sf(data=poly_95_bird_high,color="black",fill=NA)
      
      
      
      # CLASS PROPORTIONS INSIDE THE HR
      # Extract the values from the masked raster:
      values_r_habitat <- getValues(raster(cropped_r))
      
      # Remove NA values 
      values_r_habitat <- values_r_habitat[!is.na(values_r_habitat)]
      
      # Count the occurrence of each class
      class_counts <- table(values_r_habitat)
      
      # Calculate the proportion of each class:
      total_pixels <- sum(class_counts)
      class_proportions <- round((class_counts / total_pixels),3)
      # Define class names
      class_names <- c(
        "1" = "Soils and low vegetation",
        "2" = "Shrubs",
        "3" = "Trees",
        "4" = "Buildings",
        "5" = "Cliffs and water"
      )
      
      # Match the class names to the proportions
      class_proportions_named <- as.data.frame(setNames(class_proportions, class_names[names(class_proportions)]))
      colnames(class_proportions_named)<-c("Class","Proportion")
      # Display the results
      print(class_proportions_named)
      # sum(class_proportions) # to check the sum = 1 = 100%
      
      
      # Create a pie chart using ggplot2
      g_pie_HR_or_study_area<-ggplot(class_proportions_named, aes(x="", y=Proportion, fill=Class)) +
        geom_bar(stat="identity", width=1, color="black") +
        scale_fill_manual(name = "Habitat classes",
          values = c("Soils and low vegetation"="#CCCCCC","Shrubs"="#FF99CC","Trees"="#99FF99","Buildings"="#993300","Cliffs and water"="#99CCFF"),na.value ="transparent",
          guide = guide_legend(ncol = 1))+
        coord_polar(theta="y") +
        theme_void() +
        labs(title=paste("Proportion of Habitat Classes\nin the bird's home range")) 
      
    } #end if analysis_object=="HR
    
    
    
    if(analysis_object=="study_area")
    {
      
      g_mask_simple<-ggplot()+
        geom_spatraster(data=raster_object)+
        scale_fill_manual(name = "Habitat classes",
          values = c("1"="#CCCCCC","2"="#FF99CC","3"="#99FF99","4"="#993300","5"="#99CCFF"),na.value ="transparent",
          labels = c("1"="Soils and low vegetation","2"="Shrubs","3"="Trees","4"="Buildings","5"="Cliffs and water"))+
        labs( title=paste("Habitat cartography"),
              x = "Longitude",
              y = "Latitude",
              fill = "Legend")
      
      g_mask_telemetry<-ggplot()+
        geom_spatraster(data=raster_object)+
        scale_fill_manual(name = "Habitat classes",
          values = c("1"="#CCCCCC","2"="#FF99CC","3"="#99FF99","4"="#993300","5"="#99CCFF"),na.value ="transparent",
          labels = c("1"="Soils and low vegetation","2"="Shrubs","3"="Trees","4"="Buildings","5"="Cliffs and water"))+
        labs( title=paste("Habitat cartography"),
              x = "Longitude",
              y = "Latitude",
              fill = "Legend")+
        geom_point(data=data_telemetry,aes(x=longitude,y=latitude))+
        geom_sf(data = poly_95_bird_est, color = "blue", fill = "blue", alpha = 0.1) +  # incertitude: 95% low 
        geom_sf(data=poly_95_bird_low,color="black",fill=NA)+
        geom_sf(data=poly_95_bird_high,color="black",fill=NA)+
        coord_sf(xlim = c(st_bbox(poly_95_bird)["xmin"], st_bbox(poly_95_bird)["xmax"]),
        ylim = c(st_bbox(poly_95_bird)["ymin"], st_bbox(poly_95_bird)["ymax"]))
      
      
      
      
      # CLASS PROPORTION INSIDE THE STUDY AREA
      # Extract the values from the masked raster:
      values_r_habitat <- getValues(raster(raster_object))
      
      # Remove NA values 
      values_r_habitat <- values_r_habitat[!is.na(values_r_habitat)]
      
      # Count the occurrence of each class
      class_counts <- table(values_r_habitat)
      
      # Calculate the proportion of each class:
      total_pixels <- sum(class_counts)
      class_proportions <- round((class_counts / total_pixels),3)
      # Define class names
      class_names <- c(
        "1" = "Soils and low vegetation",
        "2" = "Shrubs",
        "3" = "Trees",
        "4" = "Buildings",
        "5" = "Cliffs and water"
      )
      
      # Match the class names to the proportions
      class_proportions_named <- as.data.frame(setNames(class_proportions, class_names[names(class_proportions)]))
      colnames(class_proportions_named)<-c("Class","Proportion")
      # Display the results
      print(class_proportions_named)
      # sum(class_proportions) # to check the sum = 1 = 100%
      
      # Convert the class proportions to a data frame
      
      
      # Create a pie chart using ggplot2
      g_pie_or_histo_HR_or_study_area <-
        ggplot(class_proportions_named, aes(x="", y=Proportion, fill=Class)) +
          geom_bar(stat="identity", width=1, color="black") +
          scale_fill_manual(name = "Habitat classes",
              values = c("Soils and low vegetation"="#CCCCCC","Shrubs"="#FF99CC","Trees"="#99FF99","Buildings"="#993300","Cliffs and water"="#99CCFF"),na.value ="transparent",
              guide = guide_legend(ncol = 1))+
          coord_polar(theta="y") +
          theme_void() +
          labs(title=paste("Proportion of Habitat Classes\nin the study area")) 
      
    } #end if analysis_object=="study_area"
    
    # CLASS PROPORTIONS COVERED BY TELEMETRY POINTS
    
    # Determine which telemetry points fall within the masked area
    # Create an sf object from the longitude and latitude columns
    # Create a geometry column
    
    
    
    # Convert to a standard data frame
    data_telemetry <- as.data.frame(data_telemetry)
    
    class(data_telemetry) <- "data.frame" # force the class
    
    # Create a vector of points for the geometries
    geometry_list <- st_sfc(
      lapply(seq_len(nrow(data_telemetry)), function(i) {
        st_point(c(data_telemetry$x[i], data_telemetry$y[i]))
      }), crs = "EPSG:2154"
    )
    
    # Assign the geometry to the data frame
    data_telemetry$geometry <- geometry_list
    
    # Convert to sf object
    telemetry_sf <- st_as_sf(data_telemetry, sf_column_name = "geometry")

    
    # Filter the points that fall within the polygon
    telemetry_inside_mask <- telemetry_sf[st_within(telemetry_sf, bbox_polygon_sf, sparse = FALSE), ]
    
    # Assuming masked_raster is a SpatRaster object from the terra package
    # Convert sf object to SpatVector for extraction
    
    # Extract the values of the habitat classes at those telemetry points
    telemetry_values <- terra::extract(cropped_r, telemetry_inside_mask)
    
    # Combine the extracted values with the original data
    telemetry_inside_mask <- cbind(telemetry_inside_mask, telemetry_values)
    
    # Convert the values to a data frame
    telemetry_inside_mask <- as.data.frame(telemetry_inside_mask)
    
    print(paste("NA values",sum(is.na(telemetry_inside_mask))))
    # Remove NA values
    # telemetry_df <- na.omit(telemetry_df)
    
    
    # Count the occurrences of each class
    telemetry_class_counts <-table(telemetry_inside_mask$landcover_1m)
    
    # Calculate the proportion of each class:
    telemetry_total_pixels <- sum(telemetry_class_counts)
    telemetry_class_proportions <- round((telemetry_class_counts / telemetry_total_pixels),3)
    
    
    print(telemetry_class_counts)
    
    # Match the class names to the proportions
    telemetry_class_proportions_named <- as.data.frame(setNames(telemetry_class_proportions, class_names[names(telemetry_class_proportions)]))
    colnames(telemetry_class_proportions_named)<-c("Class","Proportion")
    
    # Display the results
    print(telemetry_class_proportions_named)
    # sum(class_proportions) # to check the sum = 1 = 100%
    
    # Convert the class proportions to a data frame
    
    
    # Create a pie chart using ggplot2
    g_pie_or_histo_telemetry <-
      ggplot(telemetry_class_proportions_named, aes(x="", y=Proportion, fill=Class)) +
        geom_bar(stat="identity", width=1, color="black") +
        scale_fill_manual(name = "Habitat classes",
            values = c("Soils and low vegetation"="#CCCCCC","Shrubs"="#FF99CC","Trees"="#99FF99","Buildings"="#993300","Cliffs and water"="#99CCFF"),na.value ="transparent",
            guide = guide_legend(ncol = 1))+
        coord_polar(theta="y") +
        theme_void() +
        labs(title=paste("Proportion of Habitat Classes\ncovered by GPS positions"))
    
    
    
    # plot grob arrange options
    plot_heights=c(0.7,0.3)
    
  }#end if raster == "habitats"
  
  
  
  
  
  
  if(raster=="strava")
  {
    
    
    # RSF ON STRAVA 
    
    # apply a buffer on the raster _ not working yet, not like that 
    # strava_buffed<-buffer(raster_object,50)
    if(analysis_object=="HR")
    {  
      
      # crop the raster to the polygon extents
      cropped_r <- crop(raster_object, extent(poly_95_bird))
      
      # Ensure CRS match
      cropped_r <- terra::project(cropped_r, crs(poly_95_bird))
      
      # Mask the raster using the polygon to get the values within the polygon only
      # masked_raster <- mask(cropped_r, poly_95_bird)
      
      
      # Plot the mask
      g_mask_simple <-
        ggplot()+
          geom_spatraster(data=cropped_r)+
          scale_fill_gradientn(name = "Strava intensity",colors=c("#CCCCCC11","#FF6600","#FF3333"))+
          labs( title=paste("Strava visitor intensity"),
                x = "Longitude",
                y = "Latitude",
                fill = "Legend")
      
      g_mask_telemetry <-
        ggplot()+
          geom_spatraster(data=cropped_r)+
          scale_fill_gradientn(name = "Strava intensity",colors=c("#CCCCCC11","#FF6600","#FF3333"))+
          labs( title=paste("Strava visitor intensity"),
                x = "Longitude",
                y = "Latitude",
                fill = "Legend")+
          coord_sf(xlim = c(st_bbox(poly_95_bird)["xmin"], st_bbox(poly_95_bird)["xmax"]),
                   ylim = c(st_bbox(poly_95_bird)["ymin"], st_bbox(poly_95_bird)["ymax"]))+
          geom_point(data=data_telemetry,aes(x=longitude,y=latitude))+
          geom_sf(data = poly_95_bird_est, color = "blue", fill = "blue", alpha = 0.1) +  # incertitude: 95% low 
          geom_sf(data=poly_95_bird_low,color="black",fill=NA)+
          geom_sf(data=poly_95_bird_high,color="black",fill=NA)
        
      
      # Extract the values from the masked raster:
      values_r_habitat <- getValues(raster(cropped_r))
      
      # Remove NA values 
      values_r_habitat <- values_r_habitat[!is.na(values_r_habitat)]
      
      if(data_visu=="discrete")
      {
      # Séparer les valeurs égales à 0 des autres valeurs
      values_zero <- values_r_habitat[values_r_habitat == 0]
      values_non_zero <- values_r_habitat[values_r_habitat != 0]
      
      
      # Déterminer les breaks de Jenks pour les valeurs non nulles
      num_classes <- 3 # Nombre de classes Jenks pour les valeurs non nulles
      jenks_breaks <- classInt::classIntervals(values_non_zero, n = num_classes, style = "jenks")
      
      print(jenks_breaks)
      
      # Assigner chaque valeur non nulle à une classe
      classified_values_non_zero <- cut(values_non_zero, breaks = jenks_breaks$brks, include.lowest = TRUE, labels = FALSE)
      
      # Créer un vecteur complet pour les valeurs classifiées, en ajoutant une classe pour les valeurs égales à 0
      classified_values <- rep(NA, length(values_r_habitat))
      classified_values[values_r_habitat == 0] <- 1  # Classe 1 pour les valeurs égales à 0
      classified_values[values_r_habitat != 0] <- classified_values_non_zero + 1  # Décalage des classes Jenks pour laisser la classe 1 aux valeurs égales à 0
      
      # Convertir les valeurs classifiées en facteur
      classified_values <- factor(classified_values, levels = 1:(num_classes + 1))
      
      # Count the occurrences of each class
      class_counts <- table(classified_values)
      
      # Calculate the proportion of each class:
      total_pixels <- sum(class_counts)
      class_proportions <- round((class_counts / total_pixels),3)
      # Define class names
      class_names <- c(
        "1" = "No strava trails",
        "2" = "Low strava visitors intensity",
        "3" = "Medium strava visitors intensity",
        "4" = "High strava visitors intensity"
      )
      
      # Match the class names to the proportions
      class_proportions_named <- as.data.frame(setNames(class_proportions, class_names[names(class_proportions)]))
      colnames(class_proportions_named)<-c("Class","Proportion")
      # Display the results
      print(class_proportions_named)
      # sum(class_proportions) # to check the sum = 1 = 100%
      
      # Convert the class proportions to a data frame
      
      
      # Create a chart 
      g_pie_or_histo_HR_or_study_area <-
        ggplot(class_proportions_named, aes(x="", y=Proportion, fill=Class)) +
          geom_bar(stat="identity", width=1, color="black") +
          geom_text(aes(label = Proportion), 
                    position = position_stack(vjust = 0.5)) +
          scale_fill_manual(name = "Disturbance classes",
            values = c("No strava trails"="transparent","Low strava visitors intensity"="#FFCC99","Medium strava visitors intensity"="#FF9966","High strava visitors intensity"="#FF3300"),
            guide = guide_legend(ncol = 2))+
          coord_polar(theta="y") +
          theme_void() +
          labs(title=paste("Proportion of Strava disturbance Classes\nin the bird's home range"))
      }
    
      # if(data_visu=="continuous")
      # {
      #   # Histogram
      #   g_pie_or_histo_HR_or_study_area<-
      #     ggplot() +
      #       geom_histogram(aes(x=values_r_habitat_sa),fill="red",alpha=0.4)+
      #       geom_histogram(aes(x=values_r_habitat_raster),fill="blue",alpha=0.4)+
      #       ggtitle("Strava intensity (proxy of human disturbance) in the bird's home range")+
      #       xlab("Strava intensity")+
      #       aes(xmax=255)
      #     # theme_classic()
      # }
      
    } #end if analysis_object=="HR
    
    
    if(analysis_object=="study_area")
    {  
      
      # Plot the mask
      g_mask_simple<-ggplot()+
        geom_spatraster(data=raster_object)+
        scale_fill_gradientn(name = "Strava intensity",colors=c("#CCCCCC11","#FF6600","#FF3333"))+
        labs( title=paste("Strava visitor intensity"),
              x = "Longitude",
              y = "Latitude",
              fill = "Legend")
      
      g_mask_telemetry<-ggplot()+
        geom_spatraster(data=raster_object)+
        scale_fill_gradientn(name = "Strava intensity",colors=c("#CCCCCC11","#FF6600","#FF3333"))+
        labs( title=paste("Strava visitor intensity"),
              x = "Longitude",
              y = "Latitude",
              fill = "Legend")+
        geom_point(data=data_telemetry,aes(x=longitude,y=latitude))+
        geom_sf(data = poly_95_bird_est, color = "blue", fill = "blue", alpha = 0.1) +  # incertitude: 95% low 
        geom_sf(data=poly_95_bird_low,color="black",fill=NA)+
        geom_sf(data=poly_95_bird_high,color="black",fill=NA)+
        coord_sf(xlim = c(st_bbox(poly_95_bird)["xmin"], st_bbox(poly_95_bird)["xmax"]),
                 ylim = c(st_bbox(poly_95_bird)["ymin"], st_bbox(poly_95_bird)["ymax"]))
      
      
      # Extract the values from the masked raster:
      values_r_habitat <- getValues(raster(raster_object))
      
      # Remove NA values 
      values_r_habitat <- values_r_habitat[!is.na(values_r_habitat)]
      
      if(data_visu=="discrete")
      {
        # Séparer les valeurs égales à 0 des autres valeurs
      values_zero <- values_r_habitat[values_r_habitat == 0]
      values_non_zero <- values_r_habitat[values_r_habitat != 0]
      
      # Déterminer les breaks de Jenks pour les valeurs non nulles
      num_classes <- 3 # Nombre de classes Jenks pour les valeurs non nulles
      jenks_breaks <- classInt::classIntervals(values_non_zero, n = num_classes, style = "jenks")
      
      # Assigner chaque valeur non nulle à une classe
      classified_values_non_zero <- cut(values_non_zero, breaks = jenks_breaks$brks, include.lowest = TRUE, labels = FALSE)
      
      # Créer un vecteur complet pour les valeurs classifiées, en ajoutant une classe pour les valeurs égales à 0
      classified_values <- rep(NA, length(values_r_habitat))
      classified_values[values_r_habitat == 0] <- 1  # Classe 1 pour les valeurs égales à 0
      classified_values[values_r_habitat != 0] <- classified_values_non_zero + 1  # Décalage des classes Jenks pour laisser la classe 1 aux valeurs égales à 0
      
      # Convertir les valeurs classifiées en facteur
      classified_values <- factor(classified_values, levels = 1:(num_classes + 1))
      
      # Count the occurrences of each class
      class_counts <- table(classified_values)
      
      # Calculate the proportion of each class:
      total_pixels <- sum(class_counts)
      class_proportions <- round((class_counts / total_pixels),3)
      # Define class names
      class_names <- c(
        "1" = "No strava trails",
        "2" = "Low strava visitors intensity",
        "3" = "Medium strava visitors intensity",
        "4" = "High strava visitors intensity"
      )
      
      # Match the class names to the proportions
      class_proportions_named <- as.data.frame(setNames(class_proportions, class_names[names(class_proportions)]))
      colnames(class_proportions_named)<-c("Class","Proportion")
      # Display the results
      print(class_proportions_named)
      # sum(class_proportions) # to check the sum = 1 = 100%
      
      # Convert the class proportions to a data frame
      
      
      # Create a pie chart using ggplot2
      g_pie_or_histo_HR_or_study_area <- 
        ggplot(class_proportions_named, aes(x="", y=Proportion, fill=Class)) +
          geom_bar(stat="identity", width=1, color="black") +
          geom_text(aes(label = Proportion), 
                    position = position_stack(vjust = 0.5)) +
          scale_fill_manual(name = "Disturbance classes",
            values = c("No strava trails"="transparent","Low strava visitors intensity"="#FFCC99","Medium strava visitors intensity"="#FF9966","High strava visitors intensity"="#FF3300"),
            guide = guide_legend(ncol = 2))+
          coord_polar(theta="y") +
          theme_void() +
          labs(title=paste("Proportion of Strava disturbance Classes\nin the study area"))
        
      }
      # if(data_visu=="continuous")
      # {
      #   # Histogram
      #   g_pie_or_histo_HR_or_study_area<-ggplot() +
      #     geom_histogram(aes(x=values_r_habitat_sa),fill="red",alpha=0.4)+
      #     geom_histogram(aes(x=values_r_habitat_raster),fill="blue",alpha=0.4)+
      #     ggtitle("Strava intensity (proxy of human disturbance) in the study area")+
      #     xlab("Strava intensity")+
      #     aes(xmax=255)
      #   # theme_classic()
      # }
      
      
    }#end if analysis_object == study_area
    
    
    
    
    # CLASS PROPORTIONS COVERED BY TELEMETRY POINTS
    
    # Determine which telemetry points fall within the masked area
    # Create an sf object from the longitude and latitude columns
    # Create a geometry column
    data_telemetry$geometry <- st_sfc(
      lapply(seq_len(nrow(data_telemetry)), function(i) {
        st_point(c(data_telemetry$x[i], 
                   data_telemetry$y[i]))
      }),
      crs = st_crs(poly_95_bird)
    )
    
    # Convert the data frame to an sf object
    telemetry_sf <- st_as_sf(data_telemetry$geometry)
    
    # Filter the points that fall within the polygon
    telemetry_inside_mask <- telemetry_sf[st_within(telemetry_sf, bbox_polygon_sf, sparse = FALSE), ]
    
    # Assuming masked_raster is a SpatRaster object from the terra package
    # Convert sf object to SpatVector for extraction
    # telemetry_inside_mask_vect <- vect(st_geometry(telemetry_inside_mask))
    
    # Extract the values of the habitat classes at those telemetry points
    telemetry_values <- terra::extract(cropped_r, telemetry_inside_mask)
    
    # Combine the extracted values with the original data
    telemetry_inside_mask <- cbind(telemetry_inside_mask, telemetry_values)
    
    
    # Convert the values to a data frame
    telemetry_inside_mask <- as.data.frame(telemetry_inside_mask)
    
    print(paste("NA values telemetry",sum(is.na(telemetry_inside_mask))))
    # Remove NA values
    # telemetry_df <- na.omit(telemetry_df)
    
    # Remove NA values 
    # telemetry_values <- telemetry_values$lyr.1[!is.na(telemetry_values$lyr.1)]
    telemetry_values <- telemetry_values$lyr.1[!is.na(telemetry_values$lyr.1)]
    
    if(data_visu=="discrete")
    {
    # Séparer les valeurs égales à 0 des autres valeurs
    telemetry_values_zero <- telemetry_values[telemetry_values == 0]
    telemetry_values_non_zero <- telemetry_values[telemetry_values != 0]
    
    # Assigner chaque valeur non nulle à une classe
    telemetry_classified_values_non_zero <- cut(telemetry_values_non_zero, breaks = jenks_breaks$brks, include.lowest = TRUE, labels = FALSE)
    
    # Créer un vecteur complet pour les valeurs classifiées, en ajoutant une classe pour les valeurs égales à 0
    telemetry_classified_values <- rep(NA, length(telemetry_values))
    telemetry_classified_values[telemetry_values == 0] <- 1  # Classe 1 pour les valeurs égales à 0
    telemetry_classified_values[telemetry_values != 0] <- telemetry_classified_values_non_zero + 1  # Décalage des classes Jenks pour laisser la classe 1 aux valeurs égales à 0
    
    # Convertir les valeurs classifiées en facteur
    telemetry_classified_values <- factor(telemetry_classified_values, levels = 1:(num_classes + 1))
    
    # Count the occurrences of each class
    telemetry_class_counts <- table(telemetry_classified_values)
    
    # Calculate the proportion of each class:
    telemetry_total_pixels <- sum(telemetry_class_counts)
    telemetry_class_proportions <- round((telemetry_class_counts / telemetry_total_pixels),3)
    
    # Match the class names to the proportions
    telemetry_class_proportions_named <- as.data.frame(setNames(telemetry_class_proportions, class_names[names(telemetry_class_proportions)]))
    colnames(telemetry_class_proportions_named)<-c("Class","Proportion")
    # Display the results
    print(telemetry_class_proportions_named)
    # sum(class_proportions) # to check the sum = 1 = 100%
    
    # Convert the class proportions to a data frame
    
    
    # Create a pie chart using ggplot2
    g_pie_or_histo_telemetry<-ggplot(telemetry_class_proportions_named, aes(x="", y=Proportion, fill=Class)) +
      geom_bar(stat="identity", width=1, color="black") +
      geom_text(aes(label = Proportion), 
                position = position_stack(vjust = 0.5)) +
      scale_fill_manual(name = "Disturbance classes",
        values = c("No strava trails"="transparent","Low strava visitors intensity"="#FFCC99","Medium strava visitors intensity"="#FF9966","High strava visitors intensity"="#FF3300"),guide = guide_legend(ncol = 2))+
      coord_polar(theta="y") +
      theme_void() +
      labs(title=paste("Proportion of Strava disturbance Classes\n covered by GPS positions")) 
    }
    
    if(data_visu=="continuous")
    {
      # Histogram
      g_pie_or_histo_telemetry<-ggplot() +
        geom_histogram(aes(x=values_r_habitat , y = stat(density), fill = "Available"),alpha=0.3)+
        geom_histogram(aes(x=telemetry_values, y = stat(density), fill = "Used"),alpha=0.3)+
        scale_fill_manual(name = "Legend", values = c("Available" = "green", "Used" = "blue")) +
        ggtitle("Strava intensity (proxy of human disturbance) covered by GPS positions")+
        xlab("Strava intensity")+
        aes(xmax=255)
      # theme_classic()
    }
    
    
    # plot grob arrange options
    plot_heights=c(0.6,0.4)
    
  }#end if raster == strava
  
  if(data_visu=="discrete")
  {  
  g_bird<-grid.arrange(top=textGrob(paste0(telemetry_data@info$identity, " (", sexe_ani, ") ", season_text," habitat use"),gp = gpar(fontsize = 18,col=sex_color)),
    arrangeGrob(g_mask_simple,g_mask_telemetry,ncol=2),arrangeGrob(g_pie_or_histo_HR_or_study_area,g_pie_or_histo_telemetry,ncol=2),nrow=2,heights=plot_heights)
  }
  
  if(data_visu=="continuous")
  {  
    g_bird<-grid.arrange(top=textGrob(paste0(telemetry_data@info$identity, " (", sexe_ani, ") ", season_text," habitat use"),gp = gpar(fontsize = 18,col=sex_color)),
                         arrangeGrob(g_mask_simple,g_mask_telemetry,ncol=2),arrangeGrob(g_pie_or_histo_telemetry,ncol=2),nrow=2,heights=plot_heights)
  }
  
  if(writeplot==TRUE)
  {
    ggsave(path=paste0(base,"/TetrAlps/5_OUTPUTS/RSF/check_results/RSF_on_",raster),filename=paste0(telemetry_data@info$identity,"_RSF_on_",raster,"_",analysis_object,"_",data_visu,".jpg"),plot=g_bird,height =10 ,width =15)
  }
  
  return()
  
}

