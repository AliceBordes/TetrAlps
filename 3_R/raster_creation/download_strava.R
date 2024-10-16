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


library(devtools)
# devtools::install_git('https://forgemia.inra.fr/bjoern.reineking/strava.git')
library(strava)
library(terra)

# extents of the study area
e_3Vallees <- c(963981.7, 1002351.7 ,6464374.9 ,6495464.1)
e_Foret_blanche <- c(982999.6-1000, 994999.7+1000, 6384999.6-1000, 6399999.7+1000)
e_Les_arcs <- c(991999.6-1000, 1004999.7+1000, 6499999.6-1000, 6507999.7+1000)
e_Bauges <- c(946999.6-1000, 955999.7 +1000, 6513999.6-1000, 6522999.7+1000)
e_Valcenis <- c(998999.6-1000, 1014999.7+1000, 6467999.6-1000, 6476999.7+1000)


setwd(file.path("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/strava/Valcenis"))

#' Download strava tiles (in epsg:3857)
#'
#' @param zone Study zone for which to download data. A terra::SpatExtent
#' object. The projection of the SpatExtent must be specified in argument
#' projwin_srs.
#' @param sport Strava sport. One of e.g. "all", "ride", "run", "water",
#' "winter". For a complete list, see strava:::strava_activities.
#' @param destfile Name of outputfile
#' @param projwin_srs Projection of zone (default: EPSG:3857)
#' @param tilematrix Which zoom level to choose (between 0 and 12)
#' (default: NULL, i.e. highest available resolution).
#' @export
#'
download_strava_heatmap <- function(zone, sport = "all",
                                    destfile = file.path(getwd(),
                                                         paste0("strava_",
                                                                sport, "_",
                                                                format(Sys.time(), "%Y_%m_%d"),
                                                                ".tif")),
                                    projwin_srs = "EPSG:3857",
                                    tilematrix = NULL) {
  
  stopifnot(sport %in% strava:::strava_activities)
  stopifnot(is.null(tilematrix) || tilematrix %in% c(0:12))
  if(inherits(zone, "SpatExtent")) {
    ulx <- zone[1]
    uly <- zone[4]
    lrx <- zone[2]
    lry <- zone[3]
  }
  opts <- c("-projwin", ulx, uly, lrx, lry, "-projwin_srs", projwin_srs)
  sf::gdal_utils("translate",
                 source = paste0("WMTS:",
                                 system.file("strava-public-full.xml",
                                             package = "strava"),
                                 ",layer=strava-", sport,
                                 ifelse(is.null(tilematrix), "",
                                        paste0(",tilematrix=", tilematrix))),
                 destination = destfile,
                 options = opts)
  
}

# e <- terra::ext(6.02, 6.12, 45.06, 45.15)
# download_strava_heatmap(e, "sport_NordicSki", projwin_srs = "EPSG:4326",
# tilematrix = 10)
# ww <- terra::rast(paste0("strava-sport_NordicSki_", Sys.Date(), ".tif"))
# terra::plotRGB(ww)
# terra::plot(strava::as_numeric(ww), col = viridis::magma(256))

download_strava_heatmap(zone=ext(e_Valcenis),sport="winter",projwin_srs="+init=epsg:2154")


