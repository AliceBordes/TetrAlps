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


### Loading data 
#********************************************************************
### DATASETS

# GPS locations of black grouses
data_bg_3V<-readRDS(paste0(base,"/1_RAW_DATA/tot.ind.trois_vallees2.rds"))
#********************************************************************


### Settings 
#********************************************************************
base<-here()
#********************************************************************

### Shape the study area ----
#********************************************************************
#e <- extent(971000,985000,6471000,6486000)
e1<-ext(borders_3V_vect)
e2<-ext(as.polygons(ext(data_bg_3V), crs=crs(data_bg_3V)))

e<-c(min(e1[1],e2[1])-1000,max(e1[2],e2[2])+1000,min(e1[3],e2[3])-1000,max(e1[4],e2[4])+1000) 
e_poly<-(as.polygons(ext(e), crs=crs(data_bg_3V)))
# ext(e_poly)

# change the coordinate system of the SpatVector from (9..,9..,6..,6..) to (6..,6..,45..,45..)
# borders_3V_vect_lat_long<-project(borders_3V_vect, y="+proj=longlat +datum=WGS84")
# e<-ext(borders_3V_vect_lat_long)
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
# cut the raster of the slope for the study area
mnt<-terra::crop(mnt,e)
crs(mnt)<-"+init=epsg:2154"

writeRaster(mnt, filename=file.path(base, "2_DATA/mnt_ign.tif"), overwrite=TRUE)

mnt_9 <- terra::aggregate(mnt,9,fun="mean")
# fun = "modal" for a categorial raster = retains the majoritary class 
# disagg = to disaggregate

writeRaster(mnt_9, filename=paste0(base, "/2_DATA/mnt_9_mean_ign.tif"), overwrite=TRUE)
#********************************************************************


# Loading the raster saved (quick) and calculation of the slope
#********************************************************************
# mnt
mnt<-terra::rast(paste0(base, "/2_DATA/mnt_ign.tif"))

# slope from the mnt
slope_3V<-terra::terrain(mnt, v="slope")

# cut the raster of the slope for the study area
slope_3V<-terra::crop(slope_3V,e)
# here the resolution of the raster slope = 1m 
writeRaster(slope_3V, filename=paste0(base, "/2_DATA/slope_3V_ign.tif"), overwrite=TRUE)

# to save time for the next analyses --> create raster slope with resolution at 9m
slope_3V_9<-terra::aggregate(slope_3V,fact=9,fun="mean")
writeRaster(slope_3V_9, filename=paste0(base, "/2_DATA/slope_3V_9_ign.tif"), overwrite=TRUE)







# aggregate the raster at a superior level
# mnt_9 <- terra::aggregate(mnt,9,fun="mean") # fact = 9 =  number of cells (pixels) in each direction (horizontally and vertically)
# fun = "modal" for a categorial raster = retains the majoritary class 
# writeRaster(mnt_9, filename=paste0(base, "/2_DATA/mnt_9_mean_ign.tif"), overwrite=TRUE)










