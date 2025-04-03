## PhD Tetralpes project ##

# Alice Bordes #

# September 2024 #

# Description:

# RSF on multiple indiv



#### Loading libraries ----
#********************************************************************
# remotes::install_github("ctmm-initiative/ctmm")
# install.packages(c("animation","bit64","dplyr","fftw","knitr","move","parallel","parsedate","quadprog","rmarkdown","sf","suncalc"))
library(ctmm)
library(dplyr)
#********************************************************************

### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD/Tetralps"
telemetry_file <- "multipl_telemetry_winter_saison2_2025_02_21"
#********************************************************************


### Loading functions ----
#********************************************************************
source(file.path(base,"4_FUNCTIONS","my_telemetry_transfo_data.R"))
source(file.path(base,"4_FUNCTIONS","Homerange_visu/mean_size_area.R"))
source(file.path(base,"4_FUNCTIONS","Homerange_visu/visu_home_range.R"))
source(file.path(base,"4_FUNCTIONS","Homerange_visu","distance_home_range_capture_site.R"))
source(file.path(base,"4_FUNCTIONS","Homerange_visu","multi_graph_home_range.R"))
source(file.path(base,"4_FUNCTIONS","RSF","plot_check_RSF_results.R"))
source(file.path(base,"4_FUNCTIONS","RSF","rsf_functions.R"))
source(file.path(base,"4_FUNCTIONS","Formatting_data/formatting_environment_data.R"))
#********************************************************************


#### Loading data ----
#********************************************************************
# calibration data
files <- lapply(vect_files, function(x){
  df <- read.csv2(file = x, sep = ",")  # Read the CSV file
  df$individual.local.identifier <- sub("([a-zA-Z]+).*", "\\1", basename(x)) # add animal.id (contained in file titles) and Extract animal ID: letters only, stopping at first "_" or number 
  return(df)
})
files <- do.call(bind_rows, files)
files$Latitude <- as.numeric(files$Latitude)
files$Longitude <- as.numeric(files$Longitude)
files <- files %>% filter(Latitude != 0 & Longitude != 0)

telemetry_files <- as.telemetry(files, projection = "EPSG:2154")

ctmm::projection(telemetry_files) <- "EPSG:2154"

# current telemetry data
load(file = file.path(base,"3_R","0_Heavy_saved_models","birds_3V", paste0(telemetry_file,".Rdata")))
l_telemetry_winter <- list_of_one(l_telemetry_winter)
l_telemetry_winter <- covariates_NAN_cleaned(l_telemetry_winter)

# env
load(file.path(base,"3_R","0_Heavy_saved_models","environment_3V","scaled_env_RL_list_10m.RData"))
#********************************************************************

#********************************************************************
head(telemetry_files[[1]])

# fit error parameters to calibration data
UERE_bg <- uere.fit(telemetry_files[1:3])
# do not run uere.fit on tracking data



# clean calibration data 
OUT1 <- outlie(telemetry_files[[1]])



# Loop to assign each "winter" dataframe to a variable with the bird name
for (bird in names(l_telemetry_winter)) {
  # Extract the 'winter' dataframe and assign it to a variable named after the bird
  l_telemetry_winter[[bird]]<- l_telemetry_winter[[bird]][["winter"]]
}

summary(UERE_bg)

# eventually replace 
uere(telemetry_files) <- UERE_bg

head(telemetry_files$dede)
plot(telemetry_files$dede)

# calculate residuals of calibration data w.r.t best error model
resid <- lapply(telemetry_files[1:3],residuals)
plot(resid)

# eventually replace 
uere(l_telemetry_winter) <- UERE_bg

head(l_telemetry_winter$Caramel_2)
plot(l_telemetry_winter$Caramel_2)

## PROBLEM 2: Are these GPS tags identical?

# create a list to store individualized error models
indiv <- list()

# calculate individual UEREs
# BECAUSE USUALLY YOU DON T HAVE NECESSERALY THE SAME CALIBRATION ERROR ON ALL YOU INDIVIDUAL (EVEN IF SAME DEVICE)
indiv[[1]] <- uere.fit(turtle[[1]])
indiv[[2]] <- uere.fit(turtle[[2]])
