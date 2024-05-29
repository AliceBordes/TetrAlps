#### PhD Tetras_test project ####

# Alice Bordes #

# April 2023 #

######################################################################

# Description:

# to visualize the semi variogram of each bird, which give an idea of the minimal time require between 2 positions to consider them independent

######################################################################


SVF_indiv<-function(season=FALSE,timelags_vect=FALSE,writeplot=FALSE,proj="+init=epsg:2154")
{
  
  # Graph options
  season_text <- switch(season,
                        hiver = "winter",
                        automne = "autumn",
                        printemps = "spring",
                        ete = "summer")
  
  season_col <- switch(season,
                       hiver = "blue",
                       automne = "orange",
                       printemps = "springgreen3",
                       ete = "#FF6666")
# #graph options 
# if(season=="hiver")
# {
#   season_text="winter"
#   season_col="blue"
# }
# if(season=="automne")
# {
#   season_text="autumn"
#   season_col="orange"
# }
# if(season=="printemps")
# {
#   season_text="spring"
#   season_col="springgreen3"
# }
# if(season=="ete")
# {
#   season_text="summer"
#   season_col="#FF6666"
# }



#data
if(proj=="+proj=longlat +datum=WGS84")
{
  coordsyst<-sub(".*datum=([^ ]*)", "\\1", proj)
  coordsyst_d<-paste0("_",coordsyst)
  #matches any characters followed by "datum=" and then captures any non-space characters that come after it. The \\1 in the replacement string refers to the captured group, which in this case is the part after "datum=".
}
if(proj=="+init=epsg:2154")
{
  coordsyst<-"Lambert93"
  coordsyst_d<-""
}


data_akde<-get(paste0("grouse_winter_akde_saved_",season,"_malefemelle",coordsyst_d))
data_telemetry<-get(paste0("grouse_winter_telemetry_",season,"_malefemelle",coordsyst_d))



# population variogram considering the irregular sampling schedule
if(length(timelags_vect) > 1)
{
  timelags <- timelags_vect %#% "hour" # the order has no importance
  SVF_winter <- lapply(grouse_winter_telemetry_hiver_malefemelle_WGS84,variogram,dt=timelags) # population variogram considering the GPS were programmed to cycle between 1, 12, 24 hour sampling intervals
}else{
  SVF_winter <- lapply(grouse_winter_telemetry_hiver_malefemelle_WGS84,variogram) 
}


if(writeplot==TRUE)
{

  for (i in seq_along(data_akde)) {
    if (i %% 5 == 1) { # Start a new page for every 5 plots
      if (!file.exists(paste0(base, "/5_OUTPUTS/RSF/variograms/indiv_variograms_timelags=",paste(timelags_vect, collapse = "_"))))
      {
        dir.create(paste0(base, "/5_OUTPUTS/RSF/variograms/indiv_variograms_timelags=",paste(timelags_vect, collapse = "_")))
      }
      png(filename = paste0(base, "/5_OUTPUTS/RSF/variograms/indiv_variograms_timelags=",paste(timelags_vect, collapse = "_"),"/indiv_variogram_", i, "_", i + 4, ".png"),height = 1000,width = 2400) # Naming files correctly
      par(mfcol = c(2,5))
    }

    
    #graph title options
    if (any(synth_bg_3V$sexe[synth_bg_3V$ani_nom == data_akde[[i]]@info$identity] == "male")) {
      sexe_ani <- "male"
      sex_color <- "turquoise3"
    } else if (any(synth_bg_3V$sexe[synth_bg_3V$ani_nom == data_akde[[i]]@info$identity] == "femelle")) {
      sexe_ani <- "female"
      sex_color <- "deeppink"
    } else {
      sexe_ani <- "unknown"
      sex_color <- "gray"
    }
    
    # graphs
    plot(SVF_winter[[i]], CTMM = data_akde[[i]]@CTMM, col.CTMM = season_col, new = FALSE, fraction = 0.2, level = c(0.5, 0.95), cex.lab = 1.5, cex.main = 2)
    title(paste0("bird: ",data_akde[[i]]@info$identity," (",sexe_ani,")"),cex.main=3,font.main=2,col.main=sex_color)
    # position_independence_time = required time between two positions to consider them independent
    mtext(side = 3, line = -2.4, adj = 1, cex = 2, font=2, col=season_col, text=paste0(summary(data_akde[[1]]@CTMM)$name)," ")
    mtext(side = 3, line = -2, adj = 0, cex = 1.5, paste(" Area estimated =", round((summary(data_akde[[i]]@CTMM,unit=FALSE)$CI[1, 2])/1000000, 3)," km^2"))
    mtext(side = 3, line = -4, adj = 0, cex = 1.5,paste(" Position independent time =",round(tryCatch(summary(data_akde[[i]]@CTMM, unit = FALSE)$CI[2, 2] / 3600, error = function(e) NA), 1),"hours"))
    
    
    plot(SVF_winter[[i]], CTMM = data_akde[[i]]@CTMM, col.CTMM = season_col, new = FALSE, fraction = 0.005, level = c(0.5, 0.95), cex.lab = 1.5, cex.main = 1.5)
  
    print(data_akde[[i]]@info$identity)
    print(summary(data_akde[[i]]@CTMM,unit=FALSE)$CI)
    
  if (i %% 5 == 0 || i == length(data_akde)) 
    {
      dev.off()
    }

    
  } #close for i 

}  #close if(writeplot==TRUE)

if (dev.cur() != 1) {dev.off()} # Ensure to close any open graphic devices

# retrieve mean and max position_independent_time(h) in the population
position_independent_time<-c()
  for (i in seq_along(data_akde)) 
    {
     position_independent_time[i]<-tryCatch(summary(data_akde[[i]]@CTMM, unit = FALSE)$CI[2, 2] / 3600, error = function(e) NA)
    }

position_independent_time_dt<- data.frame("mean_position_independent_time(h)"=round(mean(position_independent_time,na.rm=TRUE),1),
                                          "max_position_independent_time(h)"=round(max(position_independent_time,na.rm=TRUE),1))
  
return(position_independent_time_dt)

}
