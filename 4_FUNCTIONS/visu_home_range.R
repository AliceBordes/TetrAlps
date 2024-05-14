#### PhD Tetras_test project ####

# Alice Bordes #

# April 2023 #

######################################################################

# Description:

# to visualize home-ranges of birds according several graphic options

######################################################################

visu_HR<-function(background=FALSE,season=FALSE,colorby=FALSE,color_palette=FALSE,showleks=FALSE,showpoints=FALSE,writeplot=FALSE,proj="+init=epsg:2154")
{
  #graph options 
  if(season=="hiver")
  {
    season_text="winter"
    season_col="blue"
  }
  if(season=="automne")
  {
    season_text="autumn"
    season_col="orange"
  }
  if(season=="printemps")
  {
    season_text="spring"
    season_col="springgreen3"
  }
  if(season=="ete")
  {
    season_text="summer"
    season_col="#FF6666"
  }
  if(colorby=="4seasons")
  {
    season_text="seasonal"
  }
  
  title_graph=paste(str_to_title(season_text),"home-ranges at 95% for all resident black grouse\n in the Trois Vallées ski resort")
  
  if(is.logical(color_palette)==FALSE)
  {
    colors<-as.vector(color_palette$colour)
  }

  #background options
    mnt_9_graph<-project(mnt_9,y=proj)
    borders_3V_vect_graph<-project(borders_3V_vect,y=proj)
    strava_graph<-project(strava_lambert,y=proj)
    
    lek_locations_vect_graph <- project(lek_locations_vect,y=proj)
    # transform lek_locations_vect in spatial object with metadata
    lek_sites_graph<-as_sf(lek_locations_vect_graph)
    # to apply a buffer around the lek sites
    lek_sites_graph$larger_lek<-st_buffer(lek_sites_graph$geometry, 100) # 100m
    
    #extent
    e_lambert93<-c(min(e1[1],e2[1])-1000,max(e1[2],e2[2])+1000,min(e1[3],e2[3])-1000,max(e1[4],e2[4])+1000)
    e_poly_lambert93<-(as.polygons(ext(e_lambert93), crs=crs(data_bg_3V)))
    # change the coordinate system of the SpatVector from (9..,9..,6..,6..) to (6..,6..,45..,45..)
    e_poly_lambert93<-project(e_poly_lambert93, y=proj)
    e<- as.numeric(as.vector(ext(e_poly_lambert93)))
 
    
    
  
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
    

        if(colorby=="4seasons")
        {
          data_akde_winter<-get(paste0("grouse_winter_akde_saved_hiver_malefemelle",coordsyst_d))
          data_akde_spring<-get(paste0("grouse_winter_akde_saved_printemps_malefemelle",coordsyst_d))
          data_akde_autumn<-get(paste0("grouse_winter_akde_saved_automne_malefemelle",coordsyst_d))
          data_akde_summer<-get(paste0("grouse_winter_akde_saved_ete_malefemelle",coordsyst_d))
          
          data_telemetry_winter<-get(paste0("grouse_winter_telemetry_hiver_malefemelle",coordsyst_d))
          data_telemetry_spring<-get(paste0("grouse_winter_telemetry_printemps_malefemelle",coordsyst_d))
          data_telemetry_autumn<-get(paste0("grouse_winter_telemetry_automne_malefemelle",coordsyst_d))
          data_telemetry_summer<-get(paste0("grouse_winter_telemetry_ete_malefemelle",coordsyst_d))
        }else{
          
          if(colorby=="sex")
          {
            data_akde_fem<-get(paste0("grouse_winter_akde_saved_",season,"_femelle",coordsyst_d))
            data_akde_mal<-get(paste0("grouse_winter_akde_saved_",season,"_male",coordsyst_d))
            data_telemetry_fem<-get(paste0("grouse_winter_telemetry_",season,"_femelle",coordsyst_d))
            data_telemetry_mal<-get(paste0("grouse_winter_telemetry_",season,"_male",coordsyst_d))
          }else{
            data_akde<-get(paste0("grouse_winter_akde_saved_",season,"_malefemelle",coordsyst_d))
            data_telemetry<-get(paste0("grouse_winter_telemetry_",season,"_malefemelle",coordsyst_d))
          }
        }

    
    


  #saving options



    if(writeplot==TRUE)
    {
      
      if (!file.exists(paste0(base, "/5_OUTPUTS/RSF/home_range_akde/",coordsyst))) 
        {
          dir.create(paste0(base, "/5_OUTPUTS/RSF/home_range_akde/",coordsyst))
        }
      
      png(filename = paste0(base, "/5_OUTPUTS/RSF/home_range_akde/",coordsyst,"/home_ranges_",
                            ifelse(is.logical(background) && !background, "",  paste0(background,"_")),
                            season,"_",
                            ifelse(is.logical(colorby) && !colorby, "", paste0(colorby,"_")),
                            ifelse(is.logical(showleks) && !showleks, "", "leks_"),
                            ".png"),
          height = 4200,width = 4600,res=300) # Naming files correctly
    }



  #background options
  if(background=="mnt")
  {
    par(oma = c(1,1,1,1))
    plot(mnt_9_graph,ext=e,col=c("#CCFFCC","#FFFFCC" ,"#FFCC99","#FF9966","#FF6600"),
         main=title_graph,
         xlab="Longitude",
         ylab="Latitude",
         cex.main=2,
         cex.lab = 1.5,
         plg = list(title = "DEM (m)",title.cex = 1.5,cex=1.2))
    plot(borders_3V_vect_graph,ext=e,add=TRUE,border="black",lwd=2)

  }

  if(background==FALSE)
  {
    par(oma = c(1,1,1,1))
    plot(borders_3V_vect_graph,ext=e,border="black",lwd=2,
         main=title_graph,
         xlab="Longitude",
         ylab="Latitude",
         cex.main=2,
         cex.lab = 1.5,
         plg = list(title = "DEM (m)",title.cex = 1.5,cex=1.2))
  }

  if(background=="strava")
  {
  par(oma = c(1,1,1,1))
  plot(strava_graph,ext=e,
       # col=colorRampPalette(c("#333333","#FF6633","#CCCCCC11"),alpha=TRUE)(25),
       col=colorRampPalette(c("#CCCCCC11","#FF6600","#FF3333"),alpha=TRUE)(25),
       main=title_graph,
       xlab="Longitude",
       ylab="Latitude",
       cex.main=2,
       cex.lab = 1.5,
       plg = list(title = "Strava users \ntrafic \n(intensity)",title.cex = 1.5,cex=1.2))
  plot(borders_3V_vect_graph,ext=e,add=TRUE,border="black",lwd=2)
  }

    


#UD options
if(colorby=="indiv")
{
  if(showpoints==TRUE)
  {
    for (i in 1:length(data_telemetry))
    {
      plot(data_telemetry[[i]],UD=data_akde[[i]],
           units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n",col.UD=colors[i],col.level=colors[i],level=NA)
    }

  }
  if(showpoints==FALSE)
  {
    for (i in 1:length(data_akde))
    {
      plot(data_akde[[i]],
           units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n",col.UD=colors[i],col.level=colors[i],level=NA)
    }
  }
}

if(colorby=="sex")
{

  if(showpoints==TRUE)
  {
    for (i in 1:length(data_telemetry_mal))
    {
      plot(data_telemetry_mal[[i]],UD=data_akde_mal[[i]],
           units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n",col.UD="turquoise")
    }

    for (i in 1:length(data_telemetry_fem))
    {
      plot(data_telemetry_fem[[i]],UD=data_akde_fem[[i]],
           units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n",col.UD="deeppink")
    }
  }
  if(showpoints==FALSE)
  {
    for (i in 1:length(data_akde_mal))
    {
      plot(data_akde_mal[[i]],
           units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n",col.UD="turquoise")
    }

    for (i in 1:length(data_akde_fem))
    {
      plot(data_akde_fem[[i]],
           units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n",col.UD="deeppink")
    }
  }
}


    
 if((colorby!="sex" & colorby!="indiv" & colorby!="4seasons")|colorby==FALSE)
  {
   
  if(showpoints==TRUE)
  {
    for (i in 1:length(data_telemetry))
    {
      plot(data_telemetry[[i]],UD=data_akde[[i]],
           units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n",col.UD=season_col)

      #col.grid=NA --> to removed the white grid in background
      # plot(grouse_winter_akde[[i]],units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n") #col.grid=NA --> to removed the white grid in background
    }
  }
  if(showpoints==FALSE)
  {
    for (i in 1:length(data_akde))
    {
      plot(data_akde[[i]],
           units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n",col.UD=season_col)

      #col.grid=NA --> to removed the white grid in background
      # plot(grouse_winter_akde[[i]],units=F,xlim=c(e[1],e[2]),ylim=c(e[3],e[4]),add=TRUE,col.grid=NA,bty="n") #col.grid=NA --> to removed the white grid in background
    }
  }

}


#lek option
if(showleks==TRUE)
{
  plot(lek_sites_graph$larger_lek,col="green",border="black",add=TRUE)
  # par(xpd=TRUE)
  add_legend("bottom", legend=c("lek site"), fill=c("green"), bty="n",border=c("black"),inset = c(-2, 1.2))

}




####################################################################



if(colorby=="4seasons")
{
  if(showpoints==TRUE)
  {
    # visualizing the home range density estimates per season in the Trois Vallées
    # names of all the birds
    vect_names_all<-c()
    for(i in 1:length(data_akde_spring))
    {
      vect_names_all[i]<-data_akde_spring[[i]]@info$identity
    }

    # Loop through each bird
    for (i in 1:length(vect_names_all)) {

      jpeg(filename = paste0(base, "/5_OUTPUTS/RSF/home_range_akde/season/all_seasons_home_range_akde_",vect_names_all[i],".png"),
           units = "in", width = 15, height = 10, res = 300)

      par(mfcol = c(2, 2))
      # Plot AKDE for Winter
      if (!is.null(data_akde_winter[[vect_names_all[i]]])) {
        plot(data_telemetry_winter[[vect_names_all[i]]],UD=data_akde_winter[[vect_names_all[i]]],
             units = F, xlim = c(e[1], e[2]), ylim = c(e[3], e[4]), col.grid = NA, bty = "n",
             col.UD = "blue",
             main = paste("Winter - Bird:", vect_names_all[i]), cex.main = 1.2,
             col.main = "blue",
             sub = paste("\narea estimated =",
                         round((summary(data_akde_winter[[vect_names_all[i]]],unit=F)$CI[,"est"])/1000000, 3)," km^2",
                         "             CI=[",
                         round((summary(data_akde_winter[[vect_names_all[i]]],unit=F)$CI[,"low"])/1000000, 3),
                         ",",
                         round((summary(data_akde_winter[[vect_names_all[i]]],unit=F)$CI[,"high"])/1000000, 3),
                         "]"),
             cex.sub=1.2,col.sub="black")

        plot(borders_3V_vect_graph, ext = e, border = "black", lwd = 2, add = TRUE)
      }

      # Plot AKDE for Spring
      if (!is.null(data_akde_spring[[vect_names_all[i]]])) {
        plot(data_telemetry_spring[[vect_names_all[i]]],UD=data_akde_spring[[vect_names_all[i]]],
             units = F, xlim = c(e[1], e[2]), ylim = c(e[3], e[4]), col.grid = NA, bty = "n",
             col.UD = "springgreen3",
             main = paste("Spring - Bird:", vect_names_all[i]), cex.main = 1.2,
             col.main = "springgreen3",
             sub = paste("\narea estimated =",
                         round((summary(data_akde_spring[[vect_names_all[i]]],unit=F)$CI[,"est"])/1000000, 3)," km^2",
                         "             CI=[",
                         round((summary(data_akde_spring[[vect_names_all[i]]],unit=F)$CI[,"low"])/1000000, 3),
                         ",",
                         round((summary(data_akde_spring[[vect_names_all[i]]],unit=F)$CI[,"high"])/1000000, 3),
                         "]"),
             cex.sub=1.2,col.sub="black")

        plot(borders_3V_vect_graph, ext = e, border = "black", lwd = 2, add = TRUE)
      }

      # Plot AKDE for Summer
      if (!is.null(data_akde_summer[[vect_names_all[i]]])) {
        plot(data_telemetry_summer[[vect_names_all[i]]],UD=data_akde_summer[[vect_names_all[i]]],
             units = F, xlim = c(e[1], e[2]), ylim = c(e[3], e[4]), col.grid = NA, bty = "n",
             col.UD = "#FF6666",
             main = paste("Summer - Bird:", vect_names_all[i]), cex.main = 1.2,
             col.main = "#FF6666",
             sub = paste("\narea estimated =",
                         round((summary(data_akde_summer[[vect_names_all[i]]],unit=F)$CI[,"est"])/1000000, 3)," km^2",
                         "             CI=[",
                         round((summary(data_akde_summer[[vect_names_all[i]]],unit=F)$CI[,"low"])/1000000, 3),
                         ",",
                         round((summary(data_akde_summer[[vect_names_all[i]]],unit=F)$CI[,"high"])/1000000, 3),
                         "]"),
             cex.sub=1.2,col.sub="black")

        plot(borders_3V_vect_graph, ext = e, border = "black", lwd = 2, add = TRUE)
      }

      # Plot AKDE for Autumn
      if (!is.null(data_akde_autumn[[vect_names_all[i]]])) {
        plot(data_telemetry_autumn[[vect_names_all[i]]],UD=data_akde_autumn[[vect_names_all[i]]],
             units = F, xlim = c(e[1], e[2]), ylim = c(e[3], e[4]), col.grid = NA, bty = "n",
             col.UD = "orange",
             main = paste("Autumn - Bird:", vect_names_all[i]), cex.main = 1.2,
             col.main = "orange",
             sub = paste("\narea estimated =",
                         round((summary(data_akde_autumn[[vect_names_all[i]]],unit=F)$CI[,"est"])/1000000, 3)," km^2",
                         "             CI=[",
                         round((summary(data_akde_autumn[[vect_names_all[i]]],unit=F)$CI[,"low"])/1000000, 3),
                         ",",
                         round((summary(data_akde_autumn[[vect_names_all[i]]],unit=F)$CI[,"high"])/1000000, 3),
                         "]"),
             cex.sub=1.2,col.sub="black")

        plot(borders_3V_vect_graph, ext = e, border = "black", lwd = 2, add = TRUE)
      }
      dev.off()
    }

  }



  if(showpoints==FALSE)
  {
          # visualizing the home range density estimates per season in the Trois Vallées
          # names of all the birds
          vect_names_all<-c()
          for(i in 1:length(data_akde_spring))
          {
            vect_names_all[i]<-data_akde_spring[[i]]@info$identity
          }

          # Loop through each bird
          for (i in 1:length(vect_names_all)) {

            jpeg(filename = paste0(base, "/5_OUTPUTS/RSF/home_range_akde/season/all_seasons_home_range_akde_",vect_names_all[i],".png"),
                 units = "in", width = 15, height = 10, res = 300)

            par(mfcol = c(2, 2))
            # Plot AKDE for Winter
            if (!is.null(data_akde_winter[[vect_names_all[i]]])) {
              plot(data_akde_winter[[vect_names_all[i]]],
                   units = F, xlim = c(e[1], e[2]), ylim = c(e[3], e[4]), col.grid = NA, bty = "n",
                   col.UD = "blue",
                   main = paste("Winter - Bird:", vect_names_all[i]), cex.main = 1.2,
                   col.main = "blue",
                   sub = paste("\narea estimated =",
                               round((summary(data_akde_winter[[vect_names_all[i]]],unit=F)$CI[,"est"])/1000000, 3)," km^2",
                               "             CI=[",
                               round((summary(data_akde_winter[[vect_names_all[i]]],unit=F)$CI[,"low"])/1000000, 3),
                               ",",
                               round((summary(data_akde_winter[[vect_names_all[i]]],unit=F)$CI[,"high"])/1000000, 3),
                               "]"),
                   cex.sub=1.2,col.sub="black")

              plot(borders_3V_vect_graph, ext = e, border = "black", lwd = 2, add = TRUE)
            }

            # Plot AKDE for Spring
            if (!is.null(data_akde_spring[[vect_names_all[i]]])) {
              plot(data_akde_spring[[vect_names_all[i]]],
                   units = F, xlim = c(e[1], e[2]), ylim = c(e[3], e[4]), col.grid = NA, bty = "n",
                   col.UD = "springgreen3",
                   main = paste("Spring - Bird:", vect_names_all[i]), cex.main = 1.2,
                   col.main = "springgreen3",
                   sub = paste("\narea estimated =",
                               round((summary(data_akde_spring[[vect_names_all[i]]],unit=F)$CI[,"est"])/1000000, 3)," km^2",
                               "             CI=[",
                               round((summary(data_akde_spring[[vect_names_all[i]]],unit=F)$CI[,"low"])/1000000, 3),
                               ",",
                               round((summary(data_akde_spring[[vect_names_all[i]]],unit=F)$CI[,"high"])/1000000, 3),
                               "]"),
                   cex.sub=1.2,col.sub="black")

              plot(borders_3V_vect_graph, ext = e, border = "black", lwd = 2, add = TRUE)
            }

            # Plot AKDE for Summer
            if (!is.null(data_akde_summer[[vect_names_all[i]]])) {
              plot(data_akde_summer[[vect_names_all[i]]],
                   units = F, xlim = c(e[1], e[2]), ylim = c(e[3], e[4]), col.grid = NA, bty = "n",
                   col.UD = "#FF6666",
                   main = paste("Summer - Bird:", vect_names_all[i]), cex.main = 1.2,
                   col.main = "#FF6666",
                   sub = paste("\narea estimated =",
                               round((summary(data_akde_summer[[vect_names_all[i]]],unit=F)$CI[,"est"])/1000000, 3)," km^2",
                               "             CI=[",
                               round((summary(data_akde_summer[[vect_names_all[i]]],unit=F)$CI[,"low"])/1000000, 3),
                               ",",
                               round((summary(data_akde_summer[[vect_names_all[i]]],unit=F)$CI[,"high"])/1000000, 3),
                               "]"),
                   cex.sub=1.2,col.sub="black")

              plot(borders_3V_vect_graph, ext = e, border = "black", lwd = 2, add = TRUE)
            }

            # Plot AKDE for Autumn
            if (!is.null(data_akde_autumn[[vect_names_all[i]]])) {
              plot(data_akde_autumn[[vect_names_all[i]]],
                   units = F, xlim = c(e[1], e[2]), ylim = c(e[3], e[4]), col.grid = NA, bty = "n",
                   col.UD = "orange",
                   main = paste("Autumn - Bird:", vect_names_all[i]), cex.main = 1.2,
                   col.main = "orange",
                   sub = paste("\narea estimated =",
                               round((summary(data_akde_autumn[[vect_names_all[i]]],unit=F)$CI[,"est"])/1000000, 3)," km^2",
                               "             CI=[",
                               round((summary(data_akde_autumn[[vect_names_all[i]]],unit=F)$CI[,"low"])/1000000, 3),
                               ",",
                               round((summary(data_akde_autumn[[vect_names_all[i]]],unit=F)$CI[,"high"])/1000000, 3),
                               "]"),
                   cex.sub=1.2,col.sub="black")

              plot(borders_3V_vect_graph, ext = e, border = "black", lwd = 2, add = TRUE)
            }
            dev.off()
          }

  }
}



  if(writeplot==TRUE)
  {
    dev.off()
  }

  return()
}




