#### PhD Tetras_test project ####

# Alice Bordes #

# April 2023 #

######################################################################

# Description:

# to obtain a plot of mean size home-range in crescent order for all birds at a given season

######################################################################

plot_mean_area_HR<-function(season,colorby="indiv",data_sex,writeplot=FALSE)
{
  #graph options 
  if(season=="hiver")
  {
    season_text="winter"
  }
  if(season=="automne")
  {
    season_text="autumn"
  }
  if(season=="printemps")
  {
    season_text="spring"
  }
  if(season=="ete")
  {
    season_text="summer"
  }
  
  #data
    
  data_akde<-get(paste0("grouse_winter_akde_saved_",season,"_malefemelle"))
  
  summ<-lapply(data_akde,summary,unit=FALSE)
  dt_mean_area<-c()
  dt_min_area<-c()
  dt_max_area<-c()
  dt_ani_na<-c()
  for(i in 1:length(data_akde)){
    dt_mean_area[i]<-summ[[i]]$CI[,colnames(summ[[i]]$CI)=="est"]/1000000
    dt_min_area[i]<-summ[[i]]$CI[,colnames(summ[[i]]$CI)=="low"]/1000000
    dt_max_area[i]<-summ[[i]]$CI[,colnames(summ[[i]]$CI)=="high"]/1000000
    dt_ani_na[i]<-names(summ[i])
  }
  
  dt_area<-data.frame("ani_nom"=dt_ani_na,"mean_area"=dt_mean_area,"min_area"=dt_min_area,"max_area"=dt_max_area,"colour"=rainbow(length(data_akde)))
  dt_area<-  dt_area%>%arrange(mean_area)
  dt_area<-left_join(dt_area,as.data.frame(data_sex) %>% select(ani_nom,sexe),by="ani_nom")
  dt_area<-distinct(dt_area)
  
  dt_meantot<-data.frame("ani_nom"="Mean","mean_area"=mean(dt_mean_area),"min_area"=mean(dt_min_area),"max_area"=mean(dt_max_area),"colour"="#000000","sexe"="")
  
  dt_area2<-rbind(dt_area,dt_meantot)
  
  # Create a factor for ani_nom with "Mean" at the beginning
  dt_area2$ani_nom <- factor(dt_area2$ani_nom, levels = c("Mean", unique(dt_area$ani_nom)))
  
  write.table(dt_area2, 
              file = paste0(base, "/5_OUTPUTS/RSF/home_range_akde/mean_size_HR_season/color_birds_", season, ".txt"))
  
  
  gplot<-ggplot()+
    xlab("Mean home-range size")+
    ylab("Bird names")+
    ggtitle(paste0("Home-range size estimations at 95% in ",season_text))+
    theme(plot.title = element_text(size=22, face="bold"),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          axis.text.x = element_text(size=14),
          # axis.text.y = element_text(size=12),
          # legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          legend.position = "right",
          legend.key = element_blank())
  
  # colorization of means
  if(colorby=="indiv")
  {
    gplot2<- gplot + 
      geom_pointrange(data = dt_area2, aes(y = ani_nom, x = mean_area, xmin = min_area, xmax = max_area, col = colour)) +
      geom_vline(xintercept=mean(dt_mean_area), linetype="dotted", color = "black", size=1)+
      scale_color_identity("Bird names") # Using scale_color_identity to keep the colors as is
    
  }
  
  if(colorby=="sex")
  {
    gplot2<- gplot +
      geom_pointrange(data = dt_area2, aes(y = ani_nom, x = mean_area, xmin = min_area, xmax = max_area, col = sexe)) +
      geom_vline(xintercept=mean(dt_mean_area), linetype="dotted", color = "black", size=1)+
      scale_color_manual("Sex", values = c("male" = "blue", "femelle" = "deeppink2"),
                         labels=c("male" = "male", "femelle" = "female"))
    
  }
    
    
    
  #not working yet
  # if (max(dt_area2$mean_area) > 200) {
  #   gplot <- gplot +
  #     scale_x_break(c(0,max(dt_area2$mean_area)+ 20), scales = 1.5) + scale_x_break(c(max(dt_area2$mean_area)+ 20,max(dt_area2$max_area)+20), scales=2)
  # }
  
  if(writeplot==TRUE)
  {
   ggsave(plot = gplot2,
          filename = paste0("mean_size_HR_",
                           season,"_",
                           ifelse(is.logical(colorby) && !colorby, "", paste0(colorby,"_")),".png"),
          path = paste0(base, "/5_OUTPUTS/RSF/home_range_akde/mean_size_HR_season/"),
          units = "in", width = 15, height = 10, dpi = 300)
  }
  
  return(gplot2)
}



