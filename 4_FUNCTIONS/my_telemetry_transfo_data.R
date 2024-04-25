#### PhD Tetras_test project ####

# Alice Bordes #

# November 2023 #

######################################################################

# Description:

# my telemetry function to obtain a file formatted to use the function as.telemetry() 
# of the ctmm package on this new data frame object

######################################################################

pre_telemetry<-function(dt)
{
  
  for(i in 1:ncol(dt)){
    if(names(dt)[i]=="id_li"){
      names(dt)[i]<-"event.id"
    }
    
    if(names(dt)[i]=="date"){
      names(dt)[i]<-"study.local.timestamp"
    }
    
    if(names(dt)[i]=="ani_nom"){
      names(dt)[i]<-"individual.local.identifier"
    }
    
    if(names(dt)[i]=="geometry"){
      #separate geometry in x and y coordinates
      #col_name <- deparse(substitute(names(dt)[i]))
       #     dt <- dt%>% mutate("utm.easting"=st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"X"],"utm.northing"=st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"Y"])
           
           
           
           dt$X_GPS_lambert93 <- st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"X"]
           dt$Y_GPS_lambert93 = st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"Y"]
           dt$WGS84 <- st_transform(dt$geometry,crs=4326)
           dt$location.long <- st_coordinates(st_cast(dt[["WGS84"]],"POINT"))[,"X"]
           dt$location.lat =st_coordinates(st_cast(dt[["WGS84"]],"POINT"))[,"Y"]

    }
    
    if(names(dt)[i]=="temperature"){
      names(dt)[i]<-"external.temperature"
    }
    
    if(names(dt)[i]=="acc_x"){
      names(dt)[i]<-"tilt.x"
    }
    if(names(dt)[i]=="acc_y"){
      names(dt)[i]<-"tilt.y"
    }
    if(names(dt)[i]=="acc_z"){
      names(dt)[i]<-"tilt.z"
    }
    
    if(names(dt)[i]=="altitude"){
      names(dt)[i]<-"height.above.mean.sea.level_GPS"
    }
    
    if(names(dt)[i]=="mnt_altitude"){
      names(dt)[i]<-"height.above.mean.sea.level"
    }
    
  }
  
  dt$timestamp<-as.POSIXct(dt[["study.local.timestamp"]], tz="UTC")
  dt$sensor.type<-"GPS"

  
  return(dt)
  
}

