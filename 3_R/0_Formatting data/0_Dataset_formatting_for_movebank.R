#### PhD Tetralpes project ####

# Alice Bordes #

# October 2024 #

# Description:

# Formatting datasets to upload data on movebank

### Loading libraries ---- 
#********************************************************************
library(tidyverse) # includes packages : ggplot2, tidyr, dplyr, lubridate, readr, purrr, tibble, stringr, forcats
#********************************************************************

### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD"
#********************************************************************

# Loading data ----
#********************************************************************
### DATASET
birds_bg_dt<-read.csv2(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry_2024_10_18.csv"),sep=",") #upload the file from a csv, not a move2 object
#********************************************************************
# sensor.type = GPS or acceleration

# create a dataset for events (GPS data and associated variables) 
  # Every event must contain an associated tag, sensor and timestamp
  # if tag.ID and animal.ID are present, the deployement is automatically generated
bg_events <- birds_bg_dt %>% select(-c(tag.manufacturer.name,
                                       tag.energy,
                                       animal.sex,
                                       animal.life.stage,
                                       animal.mass,
                                       capture.timestamp,
                                       stress.at.capture,
                                       capture.longitude,
                                       capture.latitude))

# create a dataset for deployements, animals and tags combined 
  # require animal.ID and tag.ID
  # capture.longitude and capture.latitude require no NA!
bg_deployement <- birds_bg_dt %>% select(animal.ID, 
                                         tag.ID,
                                         
                                         tag.manufacturer.name,
                                         tag.energy,
                                         animal.sex,
                                         animal.life.stage,
                                         animal.mass,
                                         capture.timestamp,
                                         stress.at.capture,
                                         capture.longitude,
                                         capture.latitude) #%>% filter (! duplicated(animal.ID))

# create a dataset for animals 
# require animal.ID and tag.ID
# capture.longitude and capture.latitude require no NA!
bg_animals <- birds_bg_dt %>% select(animal.ID, 
                                     tag.ID,
                                         
                                     animal.sex) #%>% filter (! duplicated(animal.ID))
# 
# bg_animals2 <- bg_animals
# for(i in 1:nrow(bg_animals))
# {
#   if(is.na(bg_animals2$capture.longitude[i]))
#   {
#    bg_animals2$capture.longitude[i] <- 1 
#    bg_animals2$capture.latitude[i] <- 1 
#   }
#   
# }


# create a dataset for animals 
bg_tags <- birds_bg_dt %>% select(tag.ID,
                                  tag.manufacturer.name,
                                  tag.energy) %>% filter (! duplicated(tag.ID))


write.csv(bg_events, file=paste0("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/Movebank/bg_events_",format(Sys.time(), "%m_%Y"),".csv"))
write.csv(bg_deployement, file=paste0("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/Movebank/bg_deployement_",format(Sys.time(), "%m_%Y"),".csv"))
# write.csv(bg_animals2, file=paste0("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/Movebank/bg_animals_",format(Sys.time(), "%m_%Y"),".csv"))
# write.csv(bg_tags, file=paste0("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/Movebank/bg_tags_",format(Sys.time(), "%m_%Y"),".csv"))



# Data importation procedure for Movebank ----
#********************************************************************
# 1) Create an account
# 2) Go to studies --> New study
# 3) Upload GPS data --> custom GPS data
# 4) Inform the 5 required categories (*) (for map species select "set fixed species for all rows")
# 5) Click on "Finish" --> importation is processing --> accept all automatically proposed deployements
# 6) Upload --> Import data --> reference data --> Use Movebank standard reference data format. If the study already contains deployements, do not select "replacing existing"
# ISSUE : NOT WORKING : overlapping deployement with Abel 17149, why???
# 6) Upload --> Import data --> reference data --> Use Custom reference data format. 
# POSSIBLE TO UPLOAD animals and tags BUT NOT A COMBINE ANIMALS TAGS AND DEPLOYEMENTS DATASET with capture locations, why???
#********************************************************************

