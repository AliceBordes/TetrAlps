#### PhD Tetras_test project ####
#'''''''''''''''''''''''''''''''''''
# Alice Bordes #

# March 2024 #

# Historic #
#'''''''''''''''''''''''''''''''''''
# Björn Reineking: Adapted from code obtained from Mathieu Garel
# - removed depencies on gdalUtils and raster
### adapté de N. Courbin, 13/04/2022
#'''''''''''''''''''''''''''''''''''


# Description:
#'''''''''''''''''''''''''''''''''''
### Import Strava heatmap using the WMTS flux from the PN Mercantour, thanks to Vincent Prunet, details https://github.com/PnMercantour/heatmap
### Convert RGB image in a single band image using the grDevices::rgb function. This function creates colors corresponding to the given intensities (between 0 and max) of the red, green and blue primaries, and accounts for alpha band too.
#'''''''''''''''''''''''''''''''''''



#' Download strava tiles
#'
#' @param zone A SpatExtent object. The projection of the SpatExtent must be specified in projwin_srs. Default is EPSG:3857.
#' @param sport Strava sport. One of "all", "ride", "run", "walk", "hike", "water", "winter"
#' @param outputfile Name of outputfile
#' @param outputpath Path where to save outputfile
#' @param projwin_srs Projection of zone
#' @export
#'
download_strava_heatmap <- function(zone, sport = "all",
                                    outputfile = paste0("strava-", sport, ".tif"),
                                    outputpath = getwd(),
                                    projwin_srs = "EPSG:3857")  # pseudo-mercator = projection strava 
      {
        activities <- c("all", "ride", "run", "water", "winter")
        stopifnot(sport %in% activities)
        if(inherits(zone, "SpatExtent")) {
          ulx <- zone[1]
          uly <- zone[4]
          lrx <- zone[2]
          lry <- zone[3]
        }
        opts <- c("-projwin", ulx, uly, lrx, lry,
                  "-projwin_srs", projwin_srs)# , "-overwrite")
        sf::gdal_utils("translate",
                       source = paste("WMTS:https://raw.githubusercontent.com/PnMercantour/heatmap/master/WMTS/strava-public.xml,layer=strava-", sport, sep=''),
                       destination = file.path(outputpath, outputfile),
                       options = opts)
        
        
        
        # Construction of a 3-band raster whose values give the distance between the observation point and the nearest crossing point of category greater than or equal to 1, 2 or 3
        # using gdal_proximity.py() which generates a raster proximity map indicating the distance from the center of each pixel to the center of the nearest pixel identified as a target pixel
        # Les pixels sont représentés par 3 bandes RGB et une bande alpha. On utilisera dans les traitements suivants la valeur de la bande alpha qui reflète bien les différents niveaux de fréquentation.
        # On définit 4 niveaux de fréquentation de 0 à 3 par une répartition égale (visuellement, ce mode de répartition semble cohérent avec l'original couleur produit par strava). On utilise pour cela gdal_translate
        
        strava_rgb <- terra::rast(file.path(outputpath, outputfile))
        
        ### Look at the RGB image
        # if (plot_rgb) {terra::plotRGB(strava_rgb)}
        
        ### Reduce information in a single band raster
        strava_one <- grDevices::rgb(strava_rgb[[1]][], 
                                     strava_rgb[[2]][], 
                                     strava_rgb[[3]][], 
                                     alpha = strava_rgb[[4]][], 
                                     maxColorValue = 255)  # La bande alpha (-b 4) prend une valeur nulle ou comprise 85 et 255.
        ordered_cols <- sort(unique(strava_one))
        # Verify that the gradient color is correclty ordered
        # ggplot2::qplot(x = 1:256, y = 1, fill = I(ordered_cols), geom = 'col', width = 1) + ggplot2::theme_void()
        
        # Recode color in numeric value between 0 and 255
        for(i in 1:256){
          strava_one[which(strava_one == ordered_cols[i])] <- i - 1
        }
        strava_one <- as.numeric(strava_one)
        strava_one <- matrix(strava_one, nrow = nrow(strava_rgb), ncol = ncol(strava_rgb), byrow=TRUE)
        strava_one <- terra::rast(strava_one, crs = terra::crs(strava_rgb), extent = terra::ext(strava_rgb))
        # if (plot_single) {terra::plot(strava_one, col = viridis::magma(256))}
        
        # Save single band image
        terra::writeRaster(strava_one, file = file.path(outputpath, paste0(outputfile, '_single.tif')), 
                           overwrite = T)
      }


download_strava_heatmap(zone=ext(slope_3V),sport="winter",outputfile="strava_3V_winter_sports_rgb.tif",outputpath=file.path(here(),"2_DATA/strava"),projwin_srs="+init=epsg:2154")

strava2<-rast(file.path(here(),"2_DATA/strava/strava_3V_winter_sports.tif"))
plot(strava2)


  