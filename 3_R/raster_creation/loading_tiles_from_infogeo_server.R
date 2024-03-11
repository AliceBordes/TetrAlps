#### PhD Tetras_test project ####

# Alice Bordes #

# March 2023 #

# Description:

# Loading tiles from infogeo server



### Loading packages 
#********************************************************************
library(terra)
library(ggplot2)
library(here)
#********************************************************************

### Settings 
#********************************************************************
base<-here()
#********************************************************************



# Creation of the vrt with all the tiles of the study site
#********************************************************************
# Define the folder path
tiles_path_folder <- "M:/ign/BD_ALTI/1m-V2/RGEALTI_MNT_1M_ASC_LAMB93_IGN69_D073_20210118/"

# Define the patterns to search for
y_coord<-as.character(seq(6460,6498,1))
x_coord<-as.character(seq(0960,1004,1))

# Initialize an empty vector to store patterns
patterns <- c()

# Generate all possible combinations of x_coord and y_coord
for (y in y_coord) {
  for (x in x_coord) {
    patterns <- c(patterns, paste0(x, "_", y))
  }
}

# Use grep to find files containing any of the patterns
l_mtn_files <- grep(paste(patterns, collapse = "|"), list.files(tiles_path_folder, full.names = TRUE), value = TRUE)

# Creation of the Virtual Raster Dataset (vrt) = 1 raster which is the aggregation  of several raster
mnt<-terra::vrt(l_mtn_files, paste0(base, "/2_DATA/mnt_ign.vrt"), overwrite=T)

# save the vrt as a new raster
mnt <- terra::rast(paste0(base, "/2_DATA/mnt_ign.vrt"))
writeRaster(mnt, filename=paste0(base, "/2_DATA/mnt_ign.tif"), overwrite=TRUE)
#********************************************************************


# Loading the raster saved (quick) and calculation of the slope
#********************************************************************
# mnt
mnt<-terra::rast(paste0(base, "/2_DATA/mnt_ign.tif"))

# slope from the mnt
slope_3V<-terra::terrain(mnt, v="slope")
writeRaster(slope_3V, filename=paste0(base, "/2_DATA/slope_3V_ign.tif"), overwrite=TRUE)









# aggregate the raster at a superior level
# mnt_9 <- terra::aggregate(mnt,9,fun="mean") # fact = 9 =  number of cells (pixels) in each direction (horizontally and vertically)
# fun = "modal" for a categorial raster = retains the majoritary class 
# writeRaster(mnt_9, filename=paste0(base, "/2_DATA/mnt_9_mean_ign.tif"), overwrite=TRUE)










