#### PhD Tetras_test project ####

# Alice Bordes #

# November 2023 #

######################################################################

# Description:

# my telemetry function to obtain a file formatted to use the function as.telemetry() 
# of the ctmm package on this new data frame object

######################################################################

ornitela_formating<-function(dt,name=FALSE,sex=FALSE,species=FALSE,age=FALSE,energy=FALSE,brand=FALSE,coordsyst="WGS84")
{
  if(!str_contains(names(dt),"event.ID")){
    dt$event.ID<-1:nrow(dt)
  }
  
  if(!str_contains(names(dt),"animal.taxon")){
    dt$animal.taxon<-species
  }
  
  if(!str_contains(names(dt),"animal.ID")){
    dt$animal.ID<-name
  }
  
  if(!str_contains(names(dt),"animal.sex")){
    dt$animal.sex<-sex
  }
  
  if(!str_contains(names(dt),"animal.life.stage")){
    dt$animal.life.stage<-age
  }
  
  if(!str_contains(names(dt),"tag.energy")){
    dt$tag.energy<-energy
  }
  
  if(!str_contains(names(dt),"tag.manufacturer.name")){
    dt$tag.manufacturer.name<-brand
  }
  

  
  
  for(i in 1:ncol(dt)){
    
# specific to fox csv ornitela
    
    if(names(dt)[i]=="UTC_datetime"){
      names(dt)[i]<-"timestamp"
    }
    
    if(names(dt)[i]=="datatype"){
      names(dt)[i]<-"sensor.type"
    }
    
    if(names(dt)[i]=="device_id"){
      names(dt)[i]<-"tag.ID"
    }
    
    if(names(dt)[i]=="temperature_C"){
      names(dt)[i]<-"external.temperature"
    }
    
    if(names(dt)[i]=="Altitude_m"){
      names(dt)[i]<-"height.above.mean.sea.level"
    }
    
    if(names(dt)[i]=="Longitude"){
      names(dt)[i]<-"location.long"
    }
    
    if(names(dt)[i]=="Latitude"){
      names(dt)[i]<-"location.lat"
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
    
    if(names(dt)[i]=="direction_deg"){
      names(dt)[i]<-"turning.angle"
    }
    
  }
  
  
  dt$location.long<-as.numeric(dt$location.long)
  dt$location.lat<-as.numeric(dt$location.lat)

for(j in 2:(nrow(dt)-1))
{
  if((dt$location.long[j-1]==0 && dt$location.lat[j-1]==0) || (dt$location.long[j+1]==0 && dt$location.lat[j+1]==0))
  {
    dt$turning.angle[j]<-NA
  }
  
}
  
  # Remove columns with only NA values
  dt <- dt %>% select_if(~ !all(is.na(.)))
  
  # Remove rows with only NA values
  dt <- dt %>% filter(rowSums(is.na(.)) != ncol(.))
  
  # Remove rows with latitude and longitude = 0
  dt <- dt %>% filter(location.long!=0 & location.lat!=0)
  
  return(dt)
  
}

