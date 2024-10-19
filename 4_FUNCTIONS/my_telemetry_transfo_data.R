# #### PhD Tetras_test project ####
# 
# # Alice Bordes #
# 
# # November 2023 #
# 
# ######################################################################
# 
# # Description:
# 
# # my telemetry function to obtain a file formatted to use the function as.telemetry() 
# # of the ctmm package on this new data frame object
# 
# ######################################################################


pre_telemetry <- function(dt, coordsyst = "WGS84") {
  dt <- as.data.frame(dt)
  
  # Check and rename each column directly
  if ("id_li" %in% names(dt)) dt <- dt %>% rename(event.ID = id_li)
  if ("ani_nom" %in% names(dt)) dt <- dt %>% rename(animal.ID = ani_nom)
  if ("tag_id" %in% names(dt)) dt <- dt %>% rename(tag.ID = tag_id)
  if ("date" %in% names(dt)) dt <- dt %>% rename(study.local.timestamp = date)
  if ("temperature" %in% names(dt)) dt <- dt %>% rename(external.temperature = temperature)
  if ("acc_x" %in% names(dt)) dt <- dt %>% rename(tilt.x = acc_x)
  if ("acc_y" %in% names(dt)) dt <- dt %>% rename(tilt.y = acc_y)
  if ("acc_z" %in% names(dt)) dt <- dt %>% rename(tilt.z = acc_z)
  if ("altitude" %in% names(dt)) dt <- dt %>% rename(height.above.mean.sea.level_GPS = altitude)
  if ("mnt_altitude" %in% names(dt)) dt <- dt %>% rename(height.above.mean.sea.level = mnt_altitude)
  if ("mtg_libelle" %in% names(dt)) dt <- dt %>% rename(tag.manufacturer.name = mtg_libelle)
  if ("etg_libelle" %in% names(dt)) dt <- dt %>% rename(tag.energy = etg_libelle)
  if ("cpt_date_capture" %in% names(dt)) dt <- dt %>% rename(capture.timestamp = cpt_date_capture)
  if ("age_libelle" %in% names(dt)) dt <- dt %>% rename(animal.life.stage = age_libelle)
  if ("cpt_poids_animal" %in% names(dt)) dt <- dt %>% rename(animal.mass = cpt_poids_animal)
  if ("cpt_x" %in% names(dt)) dt <- dt %>% rename(capture.longitude = cpt_x)
  if ("cpt_y" %in% names(dt)) dt <- dt %>% rename(capture.latitude = cpt_y)
  if ("ela_libelle" %in% names(dt)) dt <- dt %>% rename(stress.at.capture = ela_libelle)
  if ("zet_nom" %in% names(dt)) dt <- dt %>% rename(locality = zet_nom)
  if ("sexe" %in% names(dt)) dt <- dt %>% rename(animal.sex = sexe)
  
  # Handle geometry transformations as before
  if ("geometry" %in% names(dt)) {
    if (coordsyst == "WGS84") {
      dt$geometry <- st_transform(dt$geometry, crs = 4326)
      dt$location.long <- st_coordinates(st_cast(dt[["geometry"]], "POINT"))[, "X"]
      dt$location.lat <- st_coordinates(st_cast(dt[["geometry"]], "POINT"))[, "Y"]
      
      if ("geometry_capture" %in% names(dt)) {
        dt$geometry_capture <- st_transform(dt$geometry_capture, crs = 4326)
        dt$capture.longitude <- st_coordinates(st_cast(dt[["geometry_capture"]], "POINT"))[, "X"]
        dt$capture.latitude <- st_coordinates(st_cast(dt[["geometry_capture"]], "POINT"))[, "Y"]
      }
    }
  }
  
  # Add sensor.type
  dt$sensor.type <- "GPS"
  
  return(dt)
}


# pre_telemetry<-function(dt,coordsyst="WGS84")
# {
#   
#   for(i in seq_along(ncol(dt)))
#     
#     {
#     if(names(dt)[i]=="id_li"){
#       names(dt)[i]<-"event.ID"
#     }
#     
#     if(names(dt)[i]=="tag_id"){
#       names(dt)[i]<-"tag.ID"
#     }
#     
#     if(names(dt)[i]=="espece"){
#       names(dt)[i]<-"animal.taxon"
#     }
#     
#     if(names(dt)[i]=="date"){
#       names(dt)[i]<-"study.local.timestamp"
#     }
#     
#     if(names(dt)[i]=="ani_nom"){
#       names(dt)[i]<-"animal.ID"
#     }
#     
#     if(names(dt)[i]=="cpt_poids_animal"){
#       names(dt)[i]<-"animal.mass"
#     }
#     
#     if(names(dt)[i]=="cpt_x"){
#       names(dt)[i]<-"capture.longitude"
#     }
#     
#     if(names(dt)[i]=="cpt_y"){
#       names(dt)[i]<-"capture.latitude"
#     }
# 
#     if(names(dt)[i]=="cpt_date_capture"){
#       names(dt)[i]<-"capture.timestamp"
#     }
# 
#     if(names(dt)[i]=="zet_nom"){
#       names(dt)[i]<-"locality"
#     }
# 
#     if(names(dt)[i]=="ela_libelle"){
#       names(dt)[i]<-"stress.at.capture"
#     }
#     
#     if(names(dt)[i]=="geometry"){
#       #separate geometry in x and y coordinates
#       #col_name <- deparse(substitute(names(dt)[i]))
#        #     dt <- dt%>% mutate("utm.easting"=st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"X"],"utm.northing"=st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"Y"])
#            
#            
#            
#            # dt$X_GPS_lambert93 <- st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"X"]
#            # dt$Y_GPS_lambert93 <- st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"Y"]
#            # dt$WGS84 <- st_transform(dt$geometry,crs=4326)
#            
#            if(coordsyst=="WGS84")
#             {
#               dt$geometry <- st_transform(dt$geometry,crs=4326)
#               dt$location.long <- st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"X"]
#               dt$location.lat <- st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"Y"]
#               
#               dt$geometry_capture <- st_transform(dt$geometry_capture,crs=4326)
#               dt$capture.longitude <- st_coordinates(st_cast(dt[["geometry_capture"]],"POINT"))[,"X"]
#               dt$capture.latitude <- st_coordinates(st_cast(dt[["geometry_capture"]],"POINT"))[,"Y"]
#             }
#            
#            if(coordsyst=="Lambert93")
#            {
#              dt$location.long <- st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"X"]
#              dt$location.lat <- st_coordinates(st_cast(dt[["geometry"]],"POINT"))[,"Y"]
# 
#            }
#   
# 
# 
#     }
#     
#     if(names(dt)[i]=="temperature"){
#       names(dt)[i]<-"external.temperature"
#     }
#     
#     if(names(dt)[i]=="acc_x"){
#       names(dt)[i]<-"tilt.x"
#     }
#     if(names(dt)[i]=="acc_y"){
#       names(dt)[i]<-"tilt.y"
#     }
#     if(names(dt)[i]=="acc_z"){
#       names(dt)[i]<-"tilt.z"
#     }
#     
#     if(names(dt)[i]=="altitude"){
#       names(dt)[i]<-"height.above.mean.sea.level_GPS"
#     }
#     
#     if(names(dt)[i]=="mnt_altitude"){
#       names(dt)[i]<-"height.above.mean.sea.level"
#     }
#     
#     if((names(dt)[i]=="marque_tag")||(names(dt)[i]=="mtg_libelle")){
#       names(dt)[i]<-"tag.manufacturer.name"
#     }
#     
#     if((names(dt)[i]=="energy")||(names(dt)[i]=="etg_libelle")){
#       names(dt)[i]<-"tag.energy"
#     }
#     
#     if(names(dt)[i]=="cor"){
#       names(dt)[i]<-"correction"
#     }
#     
#     if(names(dt)[i]=="sexe"){
#       names(dt)[i]<-"animal.sex"
#       # for(j in 1:nrow(dt))
#       # {
#       #   if(dt[j,i]=="female")
#       #   {
#       #     dt[j,i]<-"female"
#       #   }
#       #   if((dt[j,i]!="femelle")&(dt[j,i]!="female")&(dt[j,i]!="male"))
#       #   {
#       #     dt[j,i]<-"unknown"
#       #   }
#       # }
#     } #end if sexe
#     
#     if((names(dt)[i]=="age")||(names(dt)[i]=="age_libelle")){
#       names(dt)[i]<-"animal.life.stage"
#       # for(j in 1:nrow(dt))
#       # {
#       #   if(dt[j,i]=="adulte")
#       #   {
#       #     dt[j,i]<-"adult"
#       #   }
#       #   if((dt[j,i]!="adulte")&(dt[j,i]!="adult")&(dt[j,i]!="immature"))
#       #   {
#       #     dt[j,i]<-"unknown"
#       #   }
#       # }
#     } # end if age
#     
#     if(names(dt)[i]=="rel_angle"){
#       names(dt)[i]<-"turning.angle"
#     }
#     
#     if(names(dt)[i]=="dist"){
#       names(dt)[i]<-"distance"
#     }
#     
#     if(names(dt)[i]=="dt"){
#       names(dt)[i]<-"time.lag"
#     }
#     
#     if(names(dt)[i]=="timestamp"){
#       dt$timestamp<-as.POSIXct(dt[["study.local.timestamp"]], tz="UTC")
#     }
#     
#     if(names(dt)[i]=="sensor.type"){
#       dt$sensor.type<-"GPS"
#     }
#     
#   }
#   
#   return(dt)
#   
# }
# 

