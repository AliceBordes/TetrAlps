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
  if ("date" %in% names(dt)) dt <- dt %>% rename(timestamp = date)
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
  
  warning("Here the timestamp is the local time (= study.local.timestamp)")
  
  return(dt)
}

