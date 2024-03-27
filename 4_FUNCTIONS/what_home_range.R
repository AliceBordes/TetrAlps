#### PhD Tetras_test project ####

# Alice Bordes #

# March 2023 #

######################################################################

# Description:

# to plot home-ranges

######################################################################

### Loading functions
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/my_telemetry_transfo_data.R")
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/multiple_dt_indiv.R")
#********************************************************************


# FUNCTION
#********************************************************************


what_home_range<-function(data,season,sex,age,raster,e){
  
  if(season!="unspecify")
  {
    grouse_winter<-as.data.frame(data%>%filter(saison=="hiver"))
    grouse_summer<-as.data.frame(data%>%filter(saison=="ete"))
    grouse_spring<-as.data.frame(data%>%filter(saison=="printemps"))
    grouse_autumn<-as.data.frame(data%>%filter(saison=="automne"))
    
    dt_season<-list(grouse_winter,grouse_summer,grouse_spring,grouse_autumn)
    names(dt_season)<-c("winter","summer","spring","autumn")
    
    
    
    
    grouse_winter_telemetry_season<-list()
    best_model_season<-list()
    grouse_winter_akde_season<-list()
    
    for(j in 1:length(dt_season))
    {
      
      grouse_winter_raw<-dt_season[[j]]
      #create a list of data.frames for each animal
      grouse_winter<-multiple_dt_indiv(grouse_winter_raw,"nom")
      
      
      #' Create a dt formatted like a telemetry object
      grouse_winter_pretelemetry<- lapply(grouse_winter,pre_telemetry)
      
      
      # # Removing the first day of location data as it often shows some unrealistic movements
      for(i in 1:length(grouse_winter_pretelemetry))
      {
        grouse_winter_pretelemetry[[i]] <- grouse_winter_pretelemetry[[i]] %>% filter (study.local.timestamp >= (first(grouse_winter_pretelemetry[[i]]["study.local.timestamp"]) + ddays(1) ))
      }
      
      # Removing the bird with no location data after removing the first day of movements
      grouse_winter_pretelemetry<- grouse_winter_pretelemetry[sapply(grouse_winter_pretelemetry, function(x) dim(x)[1]) > 0]
      
      
      grouse_winter_telemetry<-grouse_winter_pretelemetry
      for(i in 1:length(grouse_winter_telemetry))
      {
        grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_telemetry[[i]],datum="WGS84")
        grouse_winter_telemetry[[i]]["x"] <- grouse_winter_pretelemetry[[i]]["X_GPS_lambert93"]
        grouse_winter_telemetry[[i]]["y"] <- grouse_winter_pretelemetry[[i]]["Y_GPS_lambert93"]
      }
      

      #' Fit ctmm model : Continuous-Time Movement Modeling

      grouse_winter_guess <- lapply(grouse_winter_telemetry,ctmm.guess, CTMM=ctmm(isotropic = TRUE), interactive = FALSE) #isotropic = TRUE => s'éloigne du centre de manière identique dans toutes les directions
      grouse_winter_guess_summary<-lapply(grouse_winter_guess,summary)
      fitted_models_grouse_winter<-lapply(grouse_winter_telemetry,ctmm.select,CTMM=grouse_winter_guess, verbose=TRUE)
      fitted_models_grouse_winter_summary<-lapply(fitted_models_grouse_winter,summary)
      
      
      
      best_model<-list()
      for (i in 1:length(grouse_winter_guess))
      {
        best_model[[i]]<-fitted_models_grouse_winter[[i]][1]
      }

      
      grouse_winter_akde<-list()
      #' Fit akde (take into account the autocorrelation of the positions in the dataset)
      for (i in 1:length(grouse_winter_guess)) 
      {
        grouse_winter_akde[[i]]<-akde(grouse_winter_telemetry[[i]],CTMM=best_model[[i]])
      }

           
      
      grouse_winter_telemetry_season[[j]]<-grouse_winter_telemetry
      # best_model_season[[j]]<-best_model
      # grouse_winter_akde_season[[j]]<-grouse_winter_akde
      
      
    } #end loop j on the 4 seasons
    
    names(grouse_winter_telemetry_season)<-c("winter","summer","spring","autumn")
    # names(best_model_season)<-c("winter","summer","spring","autumn")
    # names(grouse_winter_akde_season)<-c("winter","summer","spring","autumn")
    
    l<-list(grouse_winter_telemetry_season,best_model_season,grouse_winter_akde_season)
    
    
  } #end if
  
    
    
    # visualizing the home range density estimates against the position data of each bird of the Trois Vallées
    
    # png(filename = paste0(here(), "/5_OUTPUTS/RSF/home_range_akde/winter_home_ranges_season.png"),height = 4000,width = 4600,res=300) # Naming files correctly
    # par(oma = c(1,1,1,1))
    # plot(mnt,ext=e,col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"),
    #      main="Winter home-ranges at 95% for all resident black grouse\n in the Trois Vallées ski resort",
    #      xlab="Longitude",
    #      ylab="Latitude",
    #      cex.main=2,
    #      cex.lab = 1.5,
    #      plg = list(title = "DEM (m)",title.cex = 1.5,cex=1.2))
    # plot(borders_3V_vect,ext=e,add=T,border="black",lwd=2)
    # 
    # for (i in 1:length(grouse_winter_telemetry))
    # {
    #   plot(grouse_winter_telemetry_season[[i]],UD=grouse_winter_akde_season[[i]],
    #        units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=T,col.grid=NA,bty="n") 
    #   
    #   #col.grid=NA --> to removed the white grid in background
    #   # plot(grouse_winter_akde[[i]],units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=T,col.grid=NA,bty="n") #col.grid=NA --> to removed the white grid in background
    # }
    # dev.off()

    
  # }
  
  
  return(l)
}

dt_season_v1<-what_home_range(data_bg_3V_synth_fusion,season="yes",sex="unspecify",age="unspecify",raster=null,e=e)
View(dt_season_v1)


  
  
  
  
  
  
  
  
  
  
  
  
  if(sex!=unspecify)
  {
    grouse_male<-as.data.frame(data%>%filter(sexe==male))
    grouse_femelle<-as.data.frame(data%>%filter(sexe==femelle))
    
    dt_sex<-list(grouse_male,grouse_femelle)
    names(dt_sex)<-c("male","female")
  }
  
  if(age!=unspecify)
  {
    grouse_unknown<-as.data.frame(data%>%filter(age==indertermine))
    grouse_adult<-as.data.frame(data%>%filter(age==adulte))
    grouse_immature<-as.data.frame(data%>%filter(age==immature))
    
    dt_age<-list(grouse_unknown,grouse_adult,grouse_immature)
    names(dt_age)<-c("unknown","adult","immature")
  }
  

  #create a list of data.frames for each animal
  grouse_winter<-multiple_dt_indiv(grouse_winter_raw,"nom")
  
  
  #' Create a dt formatted like a telemetry object
  grouse_winter_pretelemetry<- lapply(grouse_winter,pre_telemetry)
  
  
  # # Removing the first day of location data as it often shows some unrealistic movements
  for(i in 1:length(grouse_winter_pretelemetry))
  {
    grouse_winter_pretelemetry[[i]] <- grouse_winter_pretelemetry[[i]] %>% filter (study.local.timestamp >= (first(grouse_winter_pretelemetry[[i]]["study.local.timestamp"]) + ddays(1) ))
  }
  
  # Removing the bird with no location data after removing the first day of movements
  grouse_winter_pretelemetry<- grouse_winter_pretelemetry[sapply(grouse_winter_pretelemetry, function(x) dim(x)[1]) > 0]
  
  
  grouse_winter_telemetry<-grouse_winter_pretelemetry
  for(i in 1:length(grouse_winter_telemetry))
  {
    grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_telemetry[[i]],datum="WGS84")
    grouse_winter_telemetry[[i]]["x"] <- grouse_winter_pretelemetry[[i]]["X_GPS_lambert93"]
    grouse_winter_telemetry[[i]]["y"] <- grouse_winter_pretelemetry[[i]]["Y_GPS_lambert93"]
  }
  

  
  
  
  return()
  
}






### Loading birds locations 
#********************************************************************

#focus on bird locations in winter season
grouse_winter_raw<-as.data.frame(data_bg_3V%>%filter(saison=="hiver"))

#create a list of data.frames for each animal
grouse_winter<-multiple_dt_indiv(grouse_winter_raw,"nom")


#' Create a dt formatted like a telemetry object
grouse_winter_pretelemetry<- lapply(grouse_winter,pre_telemetry)


# # Removing the first day of location data as it often shows some unrealistic movements
for(i in 1:length(grouse_winter_pretelemetry))
{
  grouse_winter_pretelemetry[[i]] <- grouse_winter_pretelemetry[[i]] %>% filter (study.local.timestamp >= (first(grouse_winter_pretelemetry[[i]]["study.local.timestamp"]) + ddays(1) ))
}

# Removing the bird with no location data after removing the first day of movements
grouse_winter_pretelemetry<- grouse_winter_pretelemetry[sapply(grouse_winter_pretelemetry, function(x) dim(x)[1]) > 0]


grouse_winter_telemetry<-grouse_winter_pretelemetry
for(i in 1:length(grouse_winter_telemetry))
{
  grouse_winter_telemetry[[i]]<- as.telemetry(grouse_winter_telemetry[[i]],datum="WGS84")
  grouse_winter_telemetry[[i]]["x"] <- grouse_winter_pretelemetry[[i]]["X_GPS_lambert93"]
  grouse_winter_telemetry[[i]]["y"] <- grouse_winter_pretelemetry[[i]]["Y_GPS_lambert93"]
}

grouse_winter_pretelemetry_all<-pre_telemetry(data_bg_3V)
grouse_winter_telemetry_all<-as.telemetry(as.data.frame(grouse_winter_pretelemetry_all[,-c(1,2,3)]))


# create a list with bird's names to plot the correct legend
vect_nicknames<-list()
for(i in (1:length(grouse_winter_pretelemetry)))
{
  vect_nicknames[[i]]<-unique(grouse_winter_pretelemetry[[i]]["individual.local.identifier"])
}
vect_nicknames<-unlist(vect_nicknames)
vect_nicknames<-as.vector(vect_nicknames)
# renamed each data frame from the list of data frames by the name of each bird 
names(grouse_winter_pretelemetry)<-vect_nicknames

#********************************************************************





#### 3_Fitting a RSF ####




#### 3.1_Fitting a RSF on each bird of the Trois Vallées ski resort ####

#' Fit ctmm model : Continuous-Time Movement Modeling

# Model fitting and selection first requires a prototype model with guesstimated parameters 
#' ctmm.guess --> large approximation of the maximum likelyhood, inaccurate estimation of the parameters (VS ctmm.fit() Performs maximum likelihood parameter and interval estimation of ctmm models using each model’s exact likelihood function)
grouse_winter_guess <- lapply(grouse_winter_telemetry,ctmm.guess, CTMM=ctmm(isotropic = TRUE), interactive = FALSE) #isotropic = TRUE => s'éloigne du centre de manière identique dans toutes les directions

#plots a variogram object overlayed with a continuous-time movement model guesstimated from the variogram's shape
# isotropic = TRUE beacuse we consider the home range (espace vital) 
# as a sphere (attractor center), 
# even if an ellipse is more realistic (anisotropy) 
# but the function is not optized with (isotropic=F)

#model selected (approximation of the parameters)
grouse_winter_guess_summary<-lapply(grouse_winter_guess,summary)

# selection of the 5 best model structures
fitted_models_grouse_winter<-lapply(grouse_winter_telemetry,ctmm.select,CTMM=grouse_winter_guess, verbose=TRUE)
#CTMM = GUESS marche pas toujours, CTMM = A ctmm movement-model object containing the initial parameter guesses
fitted_models_grouse_winter_summary<-lapply(fitted_models_grouse_winter,summary)
# "OUF anisotropic" is the "best" model, IID is the conventional model 
best_model<-fitted_models_grouse_winter[[1]][1]



# Visualizing the SVF of the guess model and comparison of the 2 best fitted models on variogram
# and save
for (i in 1:length(grouse_winter_guess)) {
  if (i %% 5 == 1) { # Start a new page for every 5 plots
    png(filename = paste0(here(), "/5_OUTPUTS/RSF/variograms/indiv_variogram", i, "_", i + 4, ".png"),height = 1000,width = 2400) # Naming files correctly
    par(mfcol = c(2,5))
  }
  
  plot(SVF, CTMM = grouse_winter_guess[[i]], col.CTMM = i, new = FALSE, fraction = 0.2, level = c(0.5, 0.95),
       main = paste0(vect_nicknames[i], "\n", grouse_winter_guess_summary[[i]]$name),
       sub = paste("\narea estimated =", round(grouse_winter_guess_summary[[i]]$CI[1, 2], 3)," km^2"),
       cex.sub = 1.2, font.sub = 2, cex.lab = 1.5, cex.main = 1.5)
  
  plot(SVF, CTMM = grouse_winter_guess[[i]], col.CTMM = i, new = FALSE, fraction = 0.0005, level = c(0.5, 0.95),
       cex.sub = 2, cex.lab = 1.5)
  
  if (i %% 5 == 0 || i == length(grouse_winter_guess)) {
    dev.off() # Close the device after every 5 plots or at the end
  }
}


#visualizing the home range density estimates against the position data                                                                                                                                         

#' Fit akde (take into account the autocorrelation of the positions in the dataset)
grouse_winter_akde<-lapply(grouse_winter_telemetry,akde,CTMM=best_model)

# and save

for (i in 1:length(grouse_winter_guess)) {
  if (i %% 9 == 1) { # Start a new page for every 5 plots
    jpeg(filename = paste0(here(), "/5_OUTPUTS/RSF/home_range_akde/home_range_akde", i, "_", i + 8, ".png"), units="in", width=15, height = 10, res =300) # Naming files correctly
    par(mfcol = c(3,3))
  }
  
  plot(grouse_winter_telemetry[[i]],UD=grouse_winter_akde[[i]],main=vect_nicknames[[i]],
       units=F,
       sub = paste("\narea estimated =", 
                   round(summary(grouse_winter_akde[[i]])$CI[,"est"], 3)," km^2",
                   "             CI=[",
                   round(summary(grouse_winter_akde[[i]])$CI[,"low"], 3),
                   ",",
                   round(summary(grouse_winter_akde[[i]])$CI[,"high"], 3),
                   "]"),
       cex.sub=1.2,col.sub="blue")
  
  if (i %% 9 == 0 || i == length(grouse_winter_guess)) {
    dev.off() # Close the device after every 5 plots or at the end
  }
}


ci<-c()
for (i in 1:length(grouse_winter_akde))
{
  print(summary(grouse_winter_akde[[i]])$CI)
  ci<-c(ci,summary(grouse_winter_akde[[i]])$CI[,"est"])
}

round(mean(ci),3)

# visualizing the home range density estimates against the position data of each bird of the Trois Vallées

png(filename = paste0(here(), "/5_OUTPUTS/RSF/home_range_akde/all_winter_home_ranges.png"),height = 4000,width = 4600,res=300) # Naming files correctly
par(oma = c(1,1,1,1))
plot(mnt,ext=e,col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"),
     main="Winter home-ranges at 95% for all resident black grouse\n in the Trois Vallées ski resort",
     xlab="Longitude",
     ylab="Latitude",
     cex.main=2,
     cex.lab = 1.5,
     plg = list(title = "DEM (m)",title.cex = 1.5,cex=1.2))
plot(borders_3V_vect,ext=e,add=T,border="black",lwd=2)

for (i in 1:length(grouse_winter_telemetry))
{
  plot(grouse_winter_telemetry[[i]],UD=grouse_winter_akde[[i]],
       units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=T,col.grid=NA,bty="n") 
  
  #col.grid=NA --> to removed the white grid in background
  # plot(grouse_winter_akde[[i]],units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=T,col.grid=NA,bty="n") #col.grid=NA --> to removed the white grid in background
}
dev.off()



# visualizing the home range density estimates against the position data of each bird of the Trois Vallées

png(filename = paste0(here(), "/5_OUTPUTS/RSF/home_range_akde/all_winter_home_ranges_ski_trails.png"),height = 4000,width = 4600,res=300) # Naming files correctly
par(oma = c(1,1,1,1))
plot(strava,ext=e,
     # col=colorRampPalette(c("#333333","#FF6633","#CCCCCC11"),alpha=T)(25),
     col=colorRampPalette(c("#CCCCCC11","#FF6600","#FF3333"),alpha=T)(25),
     main="Winter home-ranges at 95% for all resident black grouse\n in the Trois Vallées ski resort",
     xlab="Longitude",
     ylab="Latitude",
     cex.main=2,
     cex.lab = 1.5,
     plg = list(title = "Strava users \ntrafic \n(intensity)",title.cex = 1.5,cex=1.2))
plot(borders_3V_vect,ext=e,add=T,border="black",lwd=2)

for (i in 1:length(grouse_winter_telemetry))
{
  plot(grouse_winter_telemetry[[i]],UD=grouse_winter_akde[[i]],
       units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=T,col.grid=NA,bty="n") 
  
  #col.grid=NA --> to removed the white grid in background
  # plot(grouse_winter_akde[[i]],units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=T,col.grid=NA,bty="n") #col.grid=NA --> to removed the white grid in background
}
dev.off()

# plot(grouse_winter_akde[[1]],units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=T,col.grid=NA,bty="n") #col.grid=NA --> to removed the white grid in background
# plot(grouse_winter_telemetry[[2]],UD=grouse_winter_akde[[2]],units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=T,col.grid=NA,bty="n") #col.grid=NA --> to removed the white grid in background
#********************************************************************




#### 3.2_Fitting a RSF on all the population as a single indiv ####


#' Fit ctmm model : Continuous-Time Movement Modeling

# Model fitting and selection first requires a prototype model with guesstimated parameters 
#' ctmm.guess --> large approximation of the maximum likelyhood, inaccurate estimation of the parameters (VS ctmm.fit() Performs maximum likelihood parameter and interval estimation of ctmm models using each model’s exact likelihood function)
grouse_winter_guess_all <- ctmm.guess(grouse_winter_telemetry_all, CTMM=ctmm(isotropic = TRUE), interactive = FALSE) #isotropic = TRUE => s'éloigne du centre de manière identique dans toutes les directions

#model selected (approximation of the parameters)
grouse_winter_guess_summary_all<-summary(grouse_winter_guess_all)

# selection of the 5 best model structures
fitted_models_grouse_winter_all<-ctmm.select(grouse_winter_telemetry_all,CTMM=grouse_winter_guess_all, verbose=TRUE)
#CTMM = GUESS marche pas toujours, CTMM = A ctmm movement-model object containing the initial parameter guesses
fitted_models_grouse_winter_summary_all<-summary(fitted_models_grouse_winter_all)
# "OUF anisotropic" is the "best" model, IID is the conventional model 
best_model_all<-fitted_models_grouse_winter_all[[1]][1]



# Visualizing the SVF of the guess model and comparison of the 2 best fitted models on variogram

plot(SVF_pop, CTMM = grouse_winter_guess_all, col.CTMM = i, new = FALSE, fraction = 0.2, level = c(0.5, 0.95),
     sub = paste("\narea estimated =", round(grouse_winter_guess_summary_all$CI[1, 2], 3)),
     cex.sub = 1.2, font.sub = 2, cex.lab = 1.5, cex.main = 1.5)


#visualizing the home range density estimates against the position data                                                                                                                                         

#' Fit akde (take into account the autocorrelation of the positions in the dataset)
grouse_winter_akde_all<-akde(grouse_winter_telemetry_all,CTMM=best_model)

windows()
plot(grouse_winter_telemetry_all,UD=grouse_winter_akde_all,
     units=F,
     sub = paste("\narea estimated =", 
                 round(summary(grouse_winter_akde_all)$CI[,"est"], 3),
                 "             CI=[",
                 round(summary(grouse_winter_akde_all)$CI[,"low"], 3),
                 ",",
                 round(summary(grouse_winter_akde_all)$CI[,"high"], 3),
                 "]"),
     cex.sub=1.2,col.sub="blue")



# visualizing the home range density estimates against the position data of each bird of the Trois Vallées
windows()
plot(mnt,ext=e,col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"),
     main="Winter home-ranges at 95% for all resident black grouse\n in the Trois Vallées ski resort",
     xlab="Longitude",
     ylab="Latitude",
     cex.main=2,
     cex.lab = 1.5,
     plg = list(title = "MNT (m)",title.cex = 1.5,cex=1.2))
plot(borders_3V_vect,ext=e,add=T)

plot(grouse_winter_telemetry_all,UD=grouse_winter_akde_all,
     units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=T,col.grid=NA,bty="n") 

#********************************************************************




#' # Minimal example of rsf.fit
#********************************************************************

#' Create named list of rasters
# raster::readAll(slope_3V_9) # to save the raster (not Spatraster) in the RAM and save time 
be <- list("slope1" = slope_3V_9)

#' The integrator = "Riemann" option is still experimental, we use it here because it is much faster
grouse_winter_rsf_riemann<-list()
for (i in 1: length(grouse_winter_telemetry))
{
  grouse_winter_rsf_riemann[[i]]<-rsf.fit(grouse_winter_telemetry[[i]], grouse_winter_akde[[i]], R = be, integrator = "Riemann")
}

# R = must be a list of rasters to fit Poisson regression coefficients to (under a log link)

grouse_winter_rsf_riemann_summary<-lapply(grouse_winter_rsf_riemann,summary)


#' Range distribution (includes the ranging behaviour) (étendue de la distribution)
agde_grouse_winter<-list()
for (i in 1: length(grouse_winter_telemetry))
{
  agde_grouse_winter[[i]]<-agde(CTMM = grouse_winter_rsf_riemann[[i]],be)
}


windows()
par(mfrow=c(1,5))
for ( i in (1:length(grouse_winter_telemetry)))
{
  plot(agde_grouse_winter[[i]],main=vect_nicknames[[i]])
}



mean_rsf<-ctmm:mean(grouse_winter_rsf_riemann)
mean_akde<-ctmm:mean(grouse_winter_akde)

#********************************************************************

#rsf with more raster than the slope

#' Create named list of rasters
be2 <- list("slope1" = slope_3V_9,"high_vegetation"=raster_high_vege_classif_9m_9)

#' The integrator = "Riemann" option is still experimental, we use it here because it is much faster
grouse_winter_rsf_riemann2<-list()
for (i in 1: length(grouse_winter_telemetry))
{
  grouse_winter_rsf_riemann2[[i]]<-rsf.fit(grouse_winter_telemetry[[i]], grouse_winter_akde[[i]], R = be2, integrator = "Riemann")
}

# R = must be a list of rasters to fit Poisson regression coefficients to (under a log link)

grouse_winter_rsf_riemann2_summary<-lapply(grouse_winter_rsf_riemann2,summary)

