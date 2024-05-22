#### PhD Tetras_test project ####

# Alice Bordes #

# May 2023 #

######################################################################

# Description:

# to visualize home-ranges of birds and the capture site with corresponding distances

######################################################################

HR_dist_capture<-function(season,sex,color=FALSE,proj="+init=epsg:2154",writeplot=FALSE)
{
  #data
  if(proj=="+proj=longlat +datum=WGS84")
  {
    coordsyst<-sub(".*datum=([^ ]*)", "\\1", proj)
    coordsyst_d<-paste0("_",coordsyst)
    #matches any characters followed by "datum=" and then captures any non-space characters that come after it. The \\1 in the replacement string refers to the captured group, which in this case is the part after "datum=".
  }
  if(proj=="+init=epsg:2154")
  {
    coordsyst<-"Lambert93"
    coordsyst_d<-""
  }

  #data
  data_akde<-get(paste0("grouse_winter_akde_saved_",season,"_",sex,coordsyst_d))

  #graph options
  if(season=="hiver")
  {
    season_text="winter"
    color_birds<-read.table("C:/Users/albordes/Documents/PhD/TetrAlps/5_OUTPUTS/RSF/home_range_akde/mean_size_HR_season/color_birds_hiver.txt", sep = "", header = TRUE) # Color dataframe from plot_mean_area_HR("hiver") to associate a specific color with each home range and capture location for a given bird.
    season_col="blue"
  }
  if(season=="automne")
  {
    season_text="autumn"
    season_col="orange"
  }
  if(season=="printemps")
  {
    season_text="spring"
    season_col="springgreen3"
  }
  if(season=="ete")
  {
    season_text="summer"
    season_col="#FF6666"
  }
  if(color=="individual")
  {
    season_col=color_birds$colour[color_birds$ani_nom==data_akde[[i]]@info$identity]
  }


  #########################

  #function

color_birds_hiver <- read.table("C:/Users/albordes/Documents/PhD/TetrAlps/5_OUTPUTS/RSF/home_range_akde/mean_size_HR_season/color_birds_hiver.txt", sep = "", header = TRUE)

HR_dist_from_centroid_list <- list()
HR_dist_from_clothest_list <- list()

for (i in seq_along(data_akde)) { 
  # Extract the polygon shape of the akde
  poly_95 <- SpatialPolygonsDataFrame.UD(data_akde[[i]], level.UD = 0.95, level = 0.95)
  
  # Subset the CI's and extract shape of middle contour
  poly_95_2 <- subset(poly_95, name == paste(data_akde[[i]]@info$identity, "95% est"))
  poly_95_3 <- poly_95_2@polygons[[1]]
  
  # Extract polygon coordinates ATTENTION : multipolygon do not work properly with st_intersects
  polygon_coords <- list()
  for (j in 1:length(poly_95_3@Polygons)) {
    polygon_coords[[j]] <- poly_95_3@Polygons[[j]]@coords
  }
  
  # Create the MULTIPOLYGON geometry to calculate the centroid
  multi <- st_multipolygon(list(polygon_coords))
  multi_sfc <- st_sfc(multi)
  multi_coords <- st_coordinates(multi_sfc)
  
  # Ensure the CRS is consistent
  st_crs(multi_sfc) <- st_crs(synth_bg_3V$geometry_lambert)
  
  # Retrieve the coordinates of the centroid of the multipolygon
  centroid <- st_centroid(multi_sfc)
  
  # Retrieve the capture site coordinates
  capture_sites <- synth_bg_3V$geometry_lambert[synth_bg_3V$ani_nom == data_akde[[i]]@info$identity]
  
  # Initialize vectors to store distances for the current bird
  HR_dist_from_centroid <- numeric(length(capture_sites))
  HR_dist_from_clothest <- numeric(length(capture_sites))
  intersection <- logical(length(capture_sites))
  
  # Calculate the distances for each capture site
  for (c in 1:length(capture_sites)) {
    capture_site <- capture_sites[c]
    
    # Check if the capture site intersects any part of the multipolygon
    for (part in 1:length(polygon_coords)) {
      poly_part <- st_polygon(list(polygon_coords[[part]]))
      poly_part_sfc <- st_sfc(poly_part)
      st_crs(poly_part_sfc) <- st_crs(synth_bg_3V$geometry_lambert)
      
      # If it intersects, set the intersection for this capture site to TRUE and exit the loop
      if (st_intersects(capture_site, poly_part_sfc, sparse = FALSE)[1, 1]==TRUE) {
        intersection[c] <- TRUE
        break #exit the loop if an intersection is found
      }
    }
    
    # Calculate the distance from the centroid to the capture site
    HR_dist_from_centroid[c] <- as.numeric(st_distance(centroid, capture_site))
    
    if (intersection[c]) {
      HR_dist_from_clothest[c] <- 0
    } else {
      # Calculate the distance from each polygon point to the capture site
      dist_from_clothest <- numeric(nrow(multi_coords))
      for (k in 1:nrow(multi_coords)) {
        point_sfc <- st_sfc(st_point(multi_coords[k, 1:2]), crs = st_crs(multi_sfc))
        dist_from_clothest[k] <- as.numeric(st_distance(point_sfc, capture_site))
      }
      HR_dist_from_clothest[c] <- min(dist_from_clothest, na.rm = TRUE)
    }
  }
  
  # Store the distances in lists
  HR_dist_from_centroid_list[[i]] <- HR_dist_from_centroid
  HR_dist_from_clothest_list[[i]] <- HR_dist_from_clothest
  
  
if(writeplot==TRUE)
  {
        #graph title options
        if (any(synth_bg_3V$sexe[synth_bg_3V$ani_nom == data_akde[[i]]@info$identity] == "male")) {
          sexe_ani <- "male"
          sex_color <- "turquoise3"
        } else if (any(synth_bg_3V$sexe[synth_bg_3V$ani_nom == data_akde[[i]]@info$identity] == "femelle")) {
          sexe_ani <- "female"
          sex_color <- "deeppink"
        } else {
          sexe_ani <- "unknown"
          sex_color <- "gray"
        }
  
  
      # Open a page with 6 plots
 if (i %% 6 == 1)
   {
     png(filename = paste0(base, "/5_OUTPUTS/RSF/home_range_akde/Lambert93/distance_to_capture_site/",season_text,"/HR_distance_to_capture_site_",season,"_", i, "_", ifelse((i + 5)<length(data_akde),i + 5,length(data_akde)), ".png"), units="in", width=18, height = 10, res =300) # Naming files correctly
     par(mfrow = c(2,3),mgp=c(2,1,0),mar = c(3, 3, 2, 1))
     # par(mar = c(7, 5, 4, 5))  # oma = Outer margins # mar = Inner margins (bottom, left, top, right)
   }
      
      plot(data_akde[[i]], units = FALSE, xlim = c(e[1], e[2]), ylim = c(e[3], e[4]), col.grid = NA, bty = "n", col.UD = season_col, col.level = season_col, level = NA, cex.lab = 1.2, cex.main = 1.5)
      title(paste0("bird: ",data_akde[[i]]@info$identity," (",sexe_ani,")"),line=0.5,cex.main=2,col.main=sex_color)
      plot(borders_3V_vect, ext = e, border = "black", lwd = 2, add = TRUE)
      plot(capture_sites, col = "black", cex = 1, type = "p", pch = 20, add = TRUE)
      
      for (c in 1:length(capture_sites)) {
        mtext(side = 3, line = -0.8 - (c+(c/4)), adj = 0.1, cex = 1, paste("\nCapture site",c,"   Distance from centroid =", round(HR_dist_from_centroid[c], 0), "m ;", "from clothest =", round(HR_dist_from_clothest[c], 0), "m"))
      }
    
 if (i %% 6 == 0 || i == length(data_akde))
    {
      dev.off() # Close the device after every 6 plots or at the end
    }

  }  #close if(writeplot==TRUE)

} # close for (i in length(data_akde))

# Calculate and print the mean distances of all capture sites (when the animal has been capture several times) for each bird 
mean_dist_from_centroid <- sapply(HR_dist_from_centroid_list, function(x) mean(x, na.rm = TRUE))
mean_dist_from_clothest <- sapply(HR_dist_from_clothest_list, function(x) mean(x, na.rm = TRUE))



if (dev.cur() != 1) {dev.off()} # Ensure to close any open graphic devices


# Calculate and print the mean distances inside the 3V population
  print(paste("Descriptive statistics in",sex,"populations in",season_text,"season"))
  print(paste("Average distance from capture site to centroid of home range:",round(mean(mean_dist_from_centroid, na.rm = TRUE), 0),"m"))
  print(paste("Related sd:",round(sd(mean_dist_from_centroid, na.rm = TRUE),0),"m"))
  print(paste("Average distance from capture site to closest point of home range:",round(mean(mean_dist_from_clothest, na.rm = TRUE), 0),"m"))
  print(paste("Related sd:",round(sd(mean_dist_from_clothest, na.rm = TRUE),0),"m"))

# print(round(mean(mean_dist_from_centroid, na.rm = TRUE), 0))
# print(round(mean(mean_dist_from_clothest, na.rm = TRUE), 0))

# Optional: Print all distances for detailed inspection
# print(HR_dist_from_centroid_list)
# print(HR_dist_from_clothest_list)

  return()
}

