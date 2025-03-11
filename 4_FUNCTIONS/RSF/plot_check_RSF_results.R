#### PhD Tetras_test project ####

# Alice Bordes #

# June 2023 #

# Description:




# Function to check results of the RSF_function (plot use VS availiability)
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
    ggsave(path=paste0(base,"/5_OUTPUTS/RSF/check_results/RSF_on_",raster),filename=paste0(telemetry_data@info$identity,"_RSF_on_",raster,"_",analysis_object,"_",data_visu,".jpg"),plot=g_bird,height =10 ,width =15)
  }
  
  return()
  
}






# Function to identify outliers
#*************************************************************
# Identify outliers
is_outlier <- function(x, coefficient = 1.5) {
  x %in% boxplot.stats(x, coef = coefficient)$out # coef = no more than this value * the length of the box away from the box.
}
# dt_out<- rsf_results_table %>% filter(covariates %in% c("sl_open:Cliffs"))
# is_outlier(dt_out$est)
# dt_out[is_outlier(dt_out$est),"bird"]
#*************************************************************



# Function to switch variable names (more readible)
#*************************************************************
swap_temporal_variable <- function(name, 
                                   temporal_list = c("displaying", "day", "night")) {
  name <- as.character(name)  # Ensure it's a character
  
  # Extract parts before and after ":"
  parts <- unlist(strsplit(name, ":", fixed = TRUE))
  
  # Swap if the first part is a temporal variable
  if (length(parts) > 1 && parts[1] %in% temporal_list) {
    return(paste(parts[2], parts[1], sep=":"))  # Swap order
  } else {
    return(name)  # Keep the name unchanged
  }
}
#*************************************************************



# Function to formate RSF results
#*************************************************************
rsf_result_table <- function(data_rsf_results, 
                             coefficient)
{

  RSF_results_multpl_birds <- lapply(data_rsf_results, summary)
  
  rsf_results_table <- data.frame()
  for(bg in seq_along(RSF_results_multpl_birds))
  {
    rsf_results_dt <- as.data.frame(RSF_results_multpl_birds[[bg]]$CI) 
    rsf_results_dt$covariates <- row.names(rsf_results_dt) 
    rsf_results_dt$bird <- names(RSF_results_multpl_birds)[[bg]] 
    rsf_results_dt <- rsf_results_dt %>%
      mutate(
        covariates = sapply(strsplit(covariates, " "), `[[`, 1), # Split the string at spaces
        covariates = case_when(
          covariates == "area" ~ "area (km^2)",
          covariates == "τ[position]" ~ "τ[position] (days)",
          covariates == "diffusion" ~ "diffusion (ha/day)",
          TRUE ~ covariates # This ensures that any values in covariates that do not match the specified conditions remain unchanged.
        )
        , covariates = sapply(covariates, swap_temporal_variable)  # Swap temporal covariates
      )
    # bind the summary of the different birds
    rsf_results_table <- rbind(rsf_results_table, rsf_results_dt)
  }
  

  # create a table to save outliers' info
  dt_outliers <- data.frame("covariate" = character(), "predictor" = character(), stringsAsFactors = FALSE)
  
  
  # create a new result table with outliers removed
  out_rsf_results_table <- rsf_results_table 
  model_covar <- unique(rsf_results_table$covariates)[1:(length(unique(rsf_results_table$covariates))-6)]
  
  for(covar in seq_along(model_covar))
  {
    dt_out<- rsf_results_table %>% filter(covariates %in% c(model_covar[covar]))
    
    bird_name_out <- dt_out[is_outlier(dt_out$est, coefficient = coefficient),"bird"] 
    
    # If outliers are found, add the covariate and corresponding bird names to dt_outliers
    if(length(bird_name_out) > 0) {
      # Create a data frame to append to dt_outliers
      temp_outliers <- data.frame(
        covariate = rep(model_covar[covar], length(bird_name_out)),
        predictor = bird_name_out,
        stringsAsFactors = FALSE
      )
      
      # Append to dt_outliers
      dt_outliers <- rbind(dt_outliers, temp_outliers)
      
      # Update the main results table, setting 'est' to NA for outliers
      out_rsf_results_table[out_rsf_results_table$covariates == model_covar[covar] & 
                              out_rsf_results_table$bird %in% bird_name_out, "est"] <- NA
    }
  }
  print(dt_outliers)
  print(model_covar)

  
  
  l_result_table <- list(rsf_results_table, out_rsf_results_table) 
  names(l_result_table) <- c("raw_results", "removed_outliers")
  
  return(l_result_table)
  
}







# Function to plot RSF results (points)
#*************************************************************


points_plot_rsf <- function(data_table, 
                            list_excluded_covariates,
                            meta_data,
                            boxplot_by_group = FALSE,
                            easy_look_results = FALSE,
                            outputfolder = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model),
                            write = TRUE,
                            group = "none") {  # Add group parameter to control the group selection

  warning("For easy_look_results = TRUE, the estimated values of the interaction effects are added to the estimated value of the main effect.")
  
  # To create more readible graphs
  if(easy_look_results == TRUE) {
    dt_l <- list(data_table[[1]], data_table[[2]], meta_data)
    
    # print("Before modification:")
    # print(head(dt_l[[1]], n = 14))
    
    for(i in 1:3) {
      
      df <- dt_l[[i]]  
      
      # Split main and interaction effects
      main_effects <- df %>%
        dplyr::filter(!grepl(":", covariates)) %>%
        rename(main_est = est, main_low = low, main_high = high)
      
      interaction_terms <- df %>%
        dplyr::filter(grepl(":", covariates)) %>%
        mutate(main_covariate = sub(":.*", "", covariates))  # Extracting main effects
      
      
      # print("Main effects:")
      # print(main_effects)
      # 
      # print("Interactions:")
      # print(interaction_terms)
      
      # join interaction and main effects`
      df_adjusted <- interaction_terms %>% dplyr::select(-c("high","low")) %>%
        left_join(main_effects %>% dplyr::select(-any_of(c("period","p"))),  by = c("bird", "main_covariate" = "covariates")) %>%
        mutate(
          est = est + main_est,
          high = NA,
          low = NA
        ) %>%
        dplyr::select(-main_covariate)  

      # print("Interactions after adjustment:")
      # print(df_adjusted)

      # Merging main and interactions effects
      df_final <- bind_rows(df %>% dplyr::filter(!grepl(":", covariates)), df_adjusted)

      dt_l[[i]] <- df_final  # replace the original dataset
    }
    
    print("After modification:")
    print(head(dt_l[[1]], n = 14))
    # View(dt_l[[1]])
    
    meta_data = dt_l[[3]]
    
  }
  
  
  
  model_covar <- unique(data_table[[1]]$covariates)[1:(length(unique(data_table[[1]]$covariates))-6)]
  
  
  gg_points_rsf <- list()
  
  for(i in 1:length(data_table)) {
    
    if(easy_look_results == TRUE) { data_table[[i]] = dt_l[[i]] }
    
    # Convert covariates to a factor (preserving order)
    # data_table[[i]]$covariates <- factor(data_table[[i]]$covariates, levels = model_covar)
    
    # Prepare the dataframe, adding period based on covid
    data_table[[i]]$period <- ifelse(data_table[[i]]$bird %in% covid, 
                                     "covid_period", 
                                     "normal_period")
    
    # Filter covariates to plot
    data_table[[i]] = data_table[[i]] %>% filter(covariates %in% model_covar[!model_covar %in% list_excluded_covariates])
    
    
    # Create the base ggplot
    gg_points_rsf[[i]] <- ggplot(data = data_table[[i]], 
                            aes(y = as.factor(covariates), x = est)) +
      
      geom_vline(xintercept = 0, linetype = "dashed") +
      labs(y = "Covariates", 
           x = "Estimates", 
           title = paste0("Results of the RSF performed on ", length(unique(data_table[[i]]$bird)), " birds \n(estimated coefficients by covariate)")) +
      theme(axis.text.x = element_text(hjust = 1),  
            panel.background = element_blank(),
            plot.title = element_text(size = 28),
            plot.subtitle = element_text(size = 26),
            axis.title = element_text(size = 26),
            axis.text = element_text(size = 24),
            legend.title = element_text(size = 26),
            legend.text = element_text(size = 24))
    
    
    # Add conditional layers based on the group type
    if(group == "none") {
      # If no grouping is applied, all birds are black
      gg_points_rsf[[i]] <- gg_points_rsf[[i]] + 
        geom_boxplot(aes(color = covariates), fill = alpha("grey", 0.2), notch = TRUE) +
        new_scale_color() +
        geom_jitter(color = "#666666", size = 3, alpha = 0.6) +
        scale_color_manual(name = "Meta model", values = c("Meta estimates" = "blue")) +
        # Add blue segments for Meta estimates
        geom_segment(data = meta_data %>% dplyr::select(!p) %>% filter(covariates %in% model_covar[!model_covar %in% list_excluded_covariates]), 
                     aes(x = est, xend = est, y = as.numeric(as.factor(covariates)) - 0.4, yend = as.numeric(as.factor(covariates)) + 0.4, color = "Meta estimates"),
                     size = 1.2) 
        # Manual color adjustments
        # Add a blue asterisk above the segment if pval < 0.05
        # geom_text(data = meta_data %>% filter(p < 0.05 & covariates %in% model_covar[!model_covar %in% list_excluded_covariates]), 
        #           aes(x = est, y = as.numeric(as.factor(covariates)) + 1.1, label = "*"), 
        #           color = "blue", size = 12, vjust = -1)  # Adjust vjust to position the asterisk
    }
    
    if(group == "covid" & boxplot_by_group == FALSE) {
      gg_points_rsf[[i]] <- gg_points_rsf[[i]] + 
        geom_boxplot(aes(color = covariates), fill = alpha("grey", 0.2), notch = TRUE) +
        new_scale_color() +
        geom_jitter(aes(color = ifelse(bird %in% covid, "Monitored during covid", "Others")), 
                    size = 3, alpha = 0.6) +
        scale_color_manual(name = "Bird groups", 
                           values = c("Monitored during covid" = "red", "Others" = "#666666"),
                           labels = c("Monitored during covid" = "Monitored during covid", "Others" = "Monitored out covid")) +
        new_scale_color()+
        scale_color_manual(name = "Meta model", values = c("Meta estimates" = "blue"))
    }
    
    if(group == "covid" & boxplot_by_group == TRUE) {
      gg_points_rsf[[i]] <- gg_points_rsf[[i]] + 
        geom_boxplot(aes(color = covariates, fill = factor(period)), 
                     alpha = 0.2, 
                     position = position_dodge(width = 0.8),
                     notch = TRUE) +
        scale_fill_manual(name = "Bird groups",
                          values = c("covid_period" = "red", "normal_period" = "grey"),
                          labels = c("covid_period" = "Monitored during covid", "normal_period" = "Monitored out covid")) +
        new_scale_color()+
        scale_color_manual(name = "Meta model", values = c("Meta estimates" = "blue")) 
    }
    
    
    if(group == "sex") {
      gg_points_rsf[[i]] <- gg_points_rsf[[i]] + 
        geom_boxplot(aes(color = covariates), fill = alpha("grey", 0.2), notch = TRUE) +
        new_scale_color() +
        geom_jitter(aes(color = ifelse(bird %in% females, "Female", "Male")), 
                    size = 3, alpha = 0.6) +
        scale_color_manual(name = "Bird groups", 
                           values = c("Female" = "deeppink", "Male" = "turquoise"),
                           labels = c("females" = "Female", "males" = "Male")) +
        new_scale_color()+
        scale_color_manual(name = "Meta model", values = c("Meta estimates" = "blue"))
    }
    
  }
  
  # Save the plot
  if(write == TRUE & group == "none") {
    ggsave(gg_points_rsf[[1]], 
           file = file.path(outputfolder, paste0("rsf_points_", names(data_table)[1],"_",group,if(easy_look_results == TRUE) {"_easy_read"},".jpeg")),
           width = 25, height = 15, units = "in")
    ggsave(gg_points_rsf[[2]], 
           file = file.path(outputfolder, paste0("rsf_points_", names(data_table)[2],"_",group,if(easy_look_results == TRUE) {"_easy_read"},".jpeg")),
           width = 25, height = 15, units = "in")
    
  }
  if(write == TRUE & group != "none") {
    ggsave(gg_points_rsf[[2]], 
           file = file.path(outputfolder, paste0("rsf_points_", names(data_table)[2],"_",group,if(boxplot_by_group == TRUE){"_box"},if(easy_look_results == TRUE) {"_easy_read"},".jpeg")),
           width = 25, height = 15, units = "in")
  }
  
  # Return the last ggplot object (for checking or further manipulation)
  return(dt_l[[1]])
}


#*************************************************************










### Function to optain the statistic results of from a metamodel ----
#********************************************************************

metamodel <- function(raw_results,
                      remove_outliers = FALSE,
                      group = NULL,
                      coefficient = 1.5,
                      outputfolder = file.path(base, "5_OUTPUTS", "RSF", "rsf.fit_results", model))
{
    # Retrieve beta estimates from the ctmm models for each bird
    rsf_beta <- sapply(raw_results, "[[", "beta")
    # Retrieve variances estimates from the ctmm models for each bird
    rsf_var <- sapply(raw_results, function(x) diag(x[["COV"]])[1:nrow(rsf_beta)])


    rownames(rsf_beta) <- sapply(rownames(rsf_beta), swap_temporal_variable)
    rownames(rsf_var) <- sapply(rownames(rsf_var), swap_temporal_variable)
    
    
    # Write results in a output file
    write.csv(cbind(as.data.frame(t(rsf_beta)),
                    "any_outlier" = apply(apply(rsf_beta, 1, is_outlier), 1, any)),
                    # "strava_visit_outlier" = is_outlier(rsf_beta["strava:visitor.nb.std",], coefficient = coefficient)),
              file.path(outputfolder, "individual_parameters_metamodel.csv"))


    # Fixed effect and random effects meta-analysis based on estimates (e.g. log hazard ratios) and their standard errors. The inverse variance method is used for pooling.
    meta_models <- lapply(1:nrow(rsf_beta), function(x) {
      
      
      tryCatch({

      if(remove_outliers == TRUE)
      {
        ok <- !is_outlier(rsf_beta[x, ]) # remove outliers for each variable
        warning("If not specified, outlier detection parameter coefficient = 1.5 times the interquartile range ")
      }else{
        ok <- rep(TRUE,ncol(rsf_beta)) # keep outliers for each variable
      }

      meta_df <- metagen(TE = rsf_beta[x, ok],  # beta estimates
                         seTE = sqrt(rsf_var[x, ok]), # var estimates
                         studlab = names(raw_results)[ok]) # labels

      print(rownames(rsf_beta)[x])
      # print(meta_df)
      
      if(is.null(group))
      {
        metareg <- metareg(meta_df, ~1)
        
        print(metareg)
      }

      if(!is.null(group) && group == "covid")
      {
        # Meta-regression with covid group

        # Creation of an indicator for covid
        covid_indicator <- ifelse(names(raw_results) %in% covid, 1, 0)
        meta_df$data$covid_indicator <- covid_indicator[ok]
    

        metareg <- metareg(meta_df, ~ covid_indicator)
        
        print(metareg)
      }
      
      if(!is.null(group) && group == "sex")
      {
        # Meta-regression with covid group
        
        # Creation of an indicator for covid
        male_indicator <- ifelse(names(raw_results) %in% males, 1, 0)
        meta_df$data$male_indicator <- male_indicator[ok]
        
        metareg <- metareg(meta_df, ~ male_indicator)
        
        # print(metareg)
      }

      return(metareg)
    },error = function(e) {
      warning(paste("Meta-regression failed for row", x, ":", e$message))
      return(NA)  # Return NA if metareg() fails
    })
})
    
    
    print(meta_models)
    
    meta_model_coef <- data.frame(t(sapply(meta_models, function(x) {
      
      
      
      # Check if 'group' is NULL (simple meta-analysis)
      if (is.null(group)) {
                if (is.null(x) || (length(x) == 1 && is.na(x))) {
          return(rep(NA, 6)) 
        } else {
        result <- c(
          "se" = x$se, 
          "tau" = sqrt(x$tau2),
          "coef_effect" = ifelse(!is.null(x$beta[1]), x$beta[1], NA),
          "pval" = ifelse(!is.null(x$pval), x$pval, NA),
          "ci.lb" = ifelse(!is.null(x$ci.lb), x$ci.lb, NA),
          "ci.ub" = ifelse(!is.null(x$ci.ub), x$ci.ub, NA)
        ) }
      } else {
        
        if (is.null(x) || (length(x) == 1 && is.na(x))) {
          return(rep(NA, 11)) 
        } else {
          
        # Meta-regression case: two coefficients (intercept & group effect)
        result <- c(
          "se1" = x$se[1], 
          "se2" = x$se[2], 
          "tau" = sqrt(x$tau2),
          "intercept_effect" = ifelse(!is.null(x$beta[1]), x$beta[1], NA),
          "group_effect" = ifelse(length(x$beta) > 1, x$beta[2], NA),
          "pval_ref" = ifelse(length(x$pval) > 1, x$pval[1], NA),
          "pval_effect" = ifelse(length(x$pval) > 1, x$pval[2], NA),
          "ci.lb1" = ifelse(length(x$ci.lb) > 1, x$ci.lb[1], NA),
          "ci.ub1" = ifelse(length(x$ci.ub) > 1, x$ci.ub[1], NA),
          "ci.lb2" = ifelse(length(x$ci.lb) > 1, x$ci.lb[2], NA),
          "ci.ub2" = ifelse(length(x$ci.ub) > 1, x$ci.ub[2], NA)
        )
      }
      
      cat(result,"\n")
      print(names(result))
      return(result)
      
      }
      
    })))

    rownames(meta_model_coef) <- names(raw_results[[1]]$beta)

    write.csv(meta_model_coef, file.path(outputfolder,paste0("metamodel",if(remove_outliers == TRUE){"_out_removed"},".csv")))


    if (is.null(group)) {
      
      rsf_results_table_meta <- data.frame("covariates" = rownames(meta_model_coef),
                                           "est" = meta_model_coef$coef_effect,
                                           "p" = meta_model_coef$pval,
                                           "low" = meta_model_coef$ci.lb, # lower bound of the conf interval = meta_model_coef$coef - 1.96*meta_model_coef$se
                                           "high" = meta_model_coef$ci.ub,
                                           "bird" = "Meta",
                                           "period" = NA
                                           )
    } else {
      
      rsf_results_table_meta <- data.frame("covariates" = rownames(meta_model_coef),
                                           "est_gr_ref" = meta_model_coef$intercept_effect,
                                           "p_ref" = meta_model_coef$pval_ref,
                                           "est_gr_effect" = meta_model_coef$group_effect,
                                           "p_effect" = meta_model_coef$pval_effect,
                                           "low_ref" = meta_model_coef$ci.lb1, # lower bound of the conf interval = meta_model_coef$coef - 1.96*meta_model_coef$se
                                           "high_ref" = meta_model_coef$ci.ub1,
                                           "low_effect" = meta_model_coef$ci.lb2, # lower bound of the conf interval = meta_model_coef$coef - 1.96*meta_model_coef$se
                                           "high_effect" = meta_model_coef$ci.ub2,
                                           "bird" = "Meta",
                                           "period" = NA
                                           )
    }
    
    rsf_results_table_meta$covariates = sapply(rsf_results_table_meta$covariates, swap_temporal_variable)  # Swap temporal covariates

    ### Plot results of the satatistical test

      if (is.null(group)) {
        meta_model_table <- nice_table(rsf_results_table_meta %>% dplyr::select(-"bird", -"period"),
                                       note = c(
                                         "* p < .05, ** p < .01, *** p < .001"
                                       ))
        }else{

          meta_model_table <- nice_table(rsf_results_table_meta %>% dplyr::select(-"bird", -"period"),
                                         col.format.p = c(3,5),
                                         note = c(
                                           "* p_ref < .05, ** p_ref < .01, *** p_ref < .001, * p_effect < .05, ** p_effect < .01, *** p_effect < .001" 
                                         ))
          
        }
      
      
      
    flextable::save_as_docx(meta_model_table, path = file.path(outputfolder,paste0("rsf_meta_results",if(remove_outliers == TRUE){paste0("_out_removed_range_interQ=",coefficient)},if(!is.null(group)){paste0("_",group)},".docx")))



    return(rsf_results_table_meta)
}

#********************************************************************









#' 
#' ### 4_Metamodel ----
#' #********************************************************************
#' rsf_beta <- sapply(sum_rsf_multipl, "[[", "beta")
#' rsf_var <- sapply(sum_rsf_multipl, function(x) diag(x[["COV"]])[1:nrow(rsf_beta)])
#' 
#' is_outlier <- function(x) {
#'   x %in% boxplot.stats(x)$out
#' }
#' 
#' write.csv(cbind(as.data.frame(t(rsf_beta)), 
#'                 "any_outlier" = apply(apply(rsf_beta, 1, is_outlier), 1, any),
#'                 "strava_visit_outlier" = is_outlier(rsf_beta["strava:total.visitors.std",])),
#'           file.path(base, "5_OUTPUTS", "RSF", "meta_model", "individual_parameters_metamodel=59birds_individual_2025_01_13.csv"))
#' 
#' # Fixed effect and random effects meta-analysis based on estimates (e.g. log hazard ratios) and their standard errors. The inverse variance method is used for pooling.
#' meta_models <- lapply(1:nrow(rsf_beta), function(x) {
#'   ok <- !is_outlier(rsf_beta[x, ]) # remove outliers 
#'   meta_df <- metagen(TE = rsf_beta[x, ok],  # beta estimates
#'                      seTE = sqrt(rsf_var[x, ok]), # var estimates
#'                      studlab = names(sum_rsf_multipl)[ok]) # labels
#'   metareg(meta_df, ~1)
#'   # metareg(meta_df, ~period) ->
#' })
#' 
#' meta_model_coef <- data.frame(t(sapply(meta_models, 
#'                                        function(x) c("coef" = x$beta[1], 
#'                                                      "se" = x$se, 
#'                                                      "pval" = x$pval,
#'                                                      "ci.lb" = x$ci.lb,
#'                                                      "ci.ub" = x$ci.ub,
#'                                                      "tau" = sqrt(x$tau2)))))
#' rownames(meta_model_coef) <- names(sum_rsf_multipl[[1]]$beta)
#' write.csv(meta_model_coef, file.path(base, "5_OUTPUTS", "RSF", "meta_model","metamodel=59birds_individual_2025_01_13.csv"))
#' 
#' 
#' 
#' rsf_results_table2 <- data.frame("covariates" = rownames(meta_model_coef), 
#'                                  "est" = meta_model_coef$coef,
#'                                  "low" = meta_model_coef$ci.lb, # lower bound of the conf interval = meta_model_coef$coef - 1.96*meta_model_coef$se
#'                                  "high" = meta_model_coef$ci.ub, 
#'                                  "bird" = "Meta", 
#'                                  "period" = NA)
#' 
#' rsf_results_table <- rbind(rsf_results_table, rsf_results_table2[, -which(colnames(rsf_results_table2) == "period")])
#' 
#' 
#' 
#' # Filter the data for boxplot and Meta
#' boxplot_data <- rsf_results_table %>% filter(!is.na(period))
#' meta_data <- rsf_results_table %>% filter(bird == "Meta")
#' 
#' 
#' # Supress Fiasco estimation for Buildings (outlier) 
#' # rsf_results_table[rsf_results_table$covariates == "Buildings" & rsf_results_table$bird == "Fiasco", "est"] <- NA
#' rsf_results_table[rsf_results_table$covariates == "Buildings" & rsf_results_table$bird == "Dynamite_2", "est"] <- NA
#' rsf_results_table[rsf_results_table$covariates == "Cliffs" & rsf_results_table$bird == "Dameur", "est"] <- NA
#' 
#' 
#' 
#' # Plot1
#' 
#' ggplot(data = rsf_results_table %>% filter(covariates %in% c("strava","strava:total.visitors.std" , "Cliffs", "Trees", "Shrubs", "Buildings")), aes(y = covariates, x = est))+
#'   # ggplot(data = rsf_results_table %>% filter(covariates %in% c("leks")), aes(y = covariates, x = est))+
#'   geom_boxplot(aes(color = covariates), fill = alpha("grey", 0.2), notch = TRUE)+
#'   new_scale_color()+
#'   geom_jitter(color = alpha("black",0.6))+
#'   scale_color_manual(
#'     name = "Bird groups", # Legend title
#'     values = c(
#'       "Monitored during covid" = "red", # Group "covid_all" in black
#'       "Others" = "black"        # Others in red
#'     ),
#'     labels = c("covid_all" = "Covid All Birds", "Other" = "Other Birds") # Custom labels
#'   ) +
#'   # geom_text(
#'   #   aes(
#'   #     label = bird,             # Add bird names as labels
#'   #     color = ifelse(bird %in% covid_all, "covid_all", "Other")
#'   #   ),
#'   #   hjust = -0.2,               # Horizontal adjustment to move labels slightly
#'   #   vjust = 0.5,                # Vertical adjustment to align with points
#'   #   size = 4                    # Text size
#'   # ) +
#'   # Add blue segments for Meta estimates
#'   geom_segment(data = meta_data %>% filter(covariates %in% c("strava", "strava:total.visitors.std" , "Cliffs", "Trees", "Shrubs", "Buildings")), 
#'                aes(x = est, xend = est, y = as.numeric(as.factor(covariates)) - 0.4, yend = as.numeric(as.factor(covariates)) + 0.4, color = "Meta estimates"),
#'                size = 1.2) +
#'   # Manual color adjustments
#'   scale_color_manual(values = c("Meta estimates" = "blue")) +
#'   # Labels and theme adjustments
#'   labs(y = "Covariates", 
#'        x = "Estimates", 
#'        title = paste0("Results of the RSF performed on ", length(unique(rsf_results_table$bird))-1, " birds \n(estimated coefficients by covariate)"),
#'        color = NULL) +  # Remove "color" title from legend
#'   geom_vline(xintercept = 0, linetype = "dashed") +
#'   theme(axis.text.x = element_text(hjust = 1),  
#'         panel.background = element_blank(),
#'         plot.title = element_text(size = 22),
#'         plot.subtitle = element_text(size = 20),
#'         axis.title = element_text(size = 20),
#'         axis.text = element_text(size = 20),
#'         legend.title = element_text(size = 20),
#'         legend.text = element_text(size = 18))
#' 
#' 
#' 
#' 
#' 
#' # Plot2
#' ggplot(data = boxplot_data %>% filter(covariates %in% c("strava", "strava:total.visitors.std", "Cliffs", "Trees", "Shrubs", "Buildings")), 
#'        aes(y = covariates, x = est)) +
#'   # Boxplot colored by covariates, grouped by period
#'   geom_boxplot(aes(color = covariates, fill = factor(period)), 
#'                alpha = 0.2, 
#'                position = position_dodge(width = 0.8),
#'                notch = TRUE) +
#'   new_scale_color()+
#'   # Add blue segments for Meta estimates
#'   geom_segment(data = meta_data %>% filter(covariates %in% c("strava", "strava:total.visitors.std", "Cliffs", "Trees", "Shrubs", "Buildings")), 
#'                aes(x = est, xend = est, y = as.numeric(as.factor(covariates)) - 0.4, yend = as.numeric(as.factor(covariates)) + 0.4, color = "Meta estimates"),
#'                size = 1.2) +
#'   # Manual fill and color adjustments
#'   scale_fill_manual(name = "Period", values = c("red", "grey")) +
#'   scale_color_manual(values = c("Meta estimates" = "blue")) +
#'   # Labels and theme adjustments
#'   labs(y = "Covariates", 
#'        x = "Estimates", 
#'        title = paste0("Results of the RSF performed on ", length(unique(rsf_results_table$bird))-1, " birds \n(estimated coefficients by covariate)"),
#'        color = NULL) +  # Remove "color" title from legend
#'   geom_vline(xintercept = 0, linetype = "dashed") +
#'   theme(axis.text.x = element_text(hjust = 1),  
#'         panel.background = element_blank(),
#'         plot.title = element_text(size = 22),
#'         plot.subtitle = element_text(size = 20),
#'         axis.title = element_text(size = 20),
#'         axis.text = element_text(size = 20),
#'         legend.title = element_text(size = 20),
#'         legend.text = element_text(size = 18))
#' 
#' 
#' ### Statistical test 
#' 
#' meta_model_coef <- cbind(rownames(meta_model_coef),meta_model_coef)
#' names(meta_model_coef) <- c("rownames(meta_model_coef)" = "covariates", "coef" = "est", "se" = "se", "pval" = "p", "ci.lb" = "low", "ci.ub" = "high", "tau" = "tau")
#' meta_model_table <- nice_table(meta_model_coef %>% dplyr::select(-"tau", -"se"),
#'                                note = c(
#'                                  "* p < .05, ** p < .01, *** p < .001"
#'                                ))
#' flextable::save_as_docx(meta_model_table, path = file.path(base, "5_OUTPUTS", "RSF", "meta_model","formatted_results_text" , "metamodel=59birds_individual_2025_01_13_formatted.docx"))
#' 
#' 
#' 
#' 
#' #' ## High suitability thresholds
#' #' 
#' #' NOT WORKING 
#' # home_ranges <- lapply(l_akde_rsf_winter_meta, SpatialPolygonsDataFrame.UD, 
#' #                       level.UD = 0.95)
#' 
#' sapply_suitability <- function(x, R) {
#'   res <- future_lapply(x, ctmm::suitability, R = R, grid = R[[1]], 
#'                        future.seed = TRUE)
#'   res <- lapply(res, "[[", "est")
#'   terra::rast(lapply(res, terra::rast))
#' }
#' 
#' suit_ref <- sapply_suitability(sum_rsf_multipl, scaled_env_RL_list_selection)
#' 
#' 
#' median_in_mask <- function(x, y) {
#'   median(terra::values(terra::mask(x, terra::vect(y)[1,])), na.rm = TRUE)
#' }
#' hisuit_threshold <- mapply(median_in_mask, as.list(suit_ref), home_ranges)
#' #********************************************************************
#' 
#' 
#' 
#' 
#' 
#' 
#' 
