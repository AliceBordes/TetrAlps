#### PhD Tetralpes project ####

# Alice Bordes #

# September 2024 #

# Description:

# Segmentation with smoove R package 
# Question: "Are movement phases related to season?" 

# Method : mcp = Regression with Multiple Change Points
# mcp does regression with one or Multiple Change Points (MCP) between Generalized and hierarchical Linear Segments using Bayesian inference. 
# mcp aims to provide maximum flexibility for analyses with a priori knowledge about the number of change points and the form of the segments in between.

### Loading libraries ---- 
#********************************************************************
library(ggplot2)
library(dplyr)
require(sf)
require(mapview)
library(mcp)
library(smoove)
# require(devtools)
# library(remotes)
# install_github("EliGurarie/smoove", build_vignettes = TRUE)


### Settings ----
#********************************************************************
base<-"C:/Users/albordes/Documents/PhD"
#********************************************************************

# Loading data ----
#********************************************************************
### DATASET
birds_bg_dt<-read.csv2(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry_2024_10_18.csv"),sep=",") #upload the file from a csv, not a move2 object
#********************************************************************

# Loading functions ----
#********************************************************************
source(file.path(base,"Tetralps/4_FUNCTIONS/plot_coord.R")) 
#********************************************************************


#********************************************************************
setwd(base)

#select the bird and the season
bird="Alpha"
season="hiver2"
list_of_animals = bird

#all animals
vect_ani <- unique(birds_bg_dt$animal.ID)
#********************************************************************


### 0_What segmentation for?
#********************************************************************
# Aim : identifying multiple behavioral modes
#********************************************************************


### 1_Visualizing data
#********************************************************************
bird_dt <- birds_bg_dt %>% filter(animal.ID %in% list_of_animals)
bird_sf <- st_as_sf(bird_dt, coords = c("location.long", "location.lat"), crs = 4326, remove = FALSE)

# choose to show each specific season or general season 
season_option = "saison2"
season_option = "saison"

mapview(bird_sf[,season_option]) # heavy to load
mapview(bird_sf[,season_option]) # heavy to load

# Plot the animal's steps after grouping points to create line geometries if you haven't already done so
bird_lines <- bird_sf %>%
  group_by(!!sym(season_option)) %>%  # Replace with actual grouping variable, like 'bird_id'
  summarise(geometry = st_combine(geometry)) %>%  # Combine points into multipoint
  st_cast("LINESTRING")  # Cast into a LINESTRING (i.e., a path)

ggplot() + 
  geom_sf(data = bird_sf, aes(col = !!sym(season_option))) +  # Points
  geom_sf(data = bird_lines, aes(col = !!sym(season_option))) +  # Lines
  theme_minimal()+
  labs(title = unique(bird_sf$animal.ID),
       x = "Longitude",
       y = "Latitude")
#********************************************************************


### 2_Looking at changing points in longitude and latitude coordinates time series
#********************************************************************
coordXY(vect_ani[1:4])
coordXY_multiple_bg(vect_ani, "G", write = TRUE)
#********************************************************************


### 3_Finding changing points ----
#********************************************************************
brkplot <- brkpts_coordXY(vect_name = vect_ani[1:4], 
                          threshold = 0.2, 
                          write = TRUE, 
                          outputfolder = file.path(base,"TetrAlps/5_OUTPUTS/data_exploration/season_segmentation/breakpoints"))

brkplot

#********************************************************************














### 2_Finding changing points with smoove ----
#********************************************************************

with(bird_sf, scan_track(x = as.numeric(bird_sf$location.long), y = as.numeric(bird_sf$location.lat), time = as.POSIXct(bird_sf$study.local.timestamp)))

# sweep : what are the multiple behaviors we can observe?

bird_sweep <- with(bird_sf, sweepRACVM(Z = cbind(as.numeric(bird_sf$location.long),as.numeric(bird_sf$location.lat)), 
                                     T = as.POSIXct(bird_sf$study.local.timestamp),
                                     windowsize = 15, windowstep = 3, # 1() days windowsize : seek changing points in a windows of 3 day, and moving this windows every 2h
                                     time.unit = "days", 
                                     model = "UCVM", progress=TRUE), method = "crawl")
# choice of the windowsize : 
# The smaller the window step, the more thorough the analysis, but ultimately its size is not very important as long as it is much smaller than the analysis window. 
# Ultimately, for strong changes, the analysis will not be very sensitive to reasonable values of either of these parameters.
# Note that if there are gaps in the data or they are irregular, the data extent of the window will be smaller, and if there are fewer than 30 data points in the window, the window will be skipped.


png(filename = file.path(base,"TetrAlps/5_OUTPUTS/data_exploration/season_segmentation",paste0(bird,".png")),
    width = 25 , height = 15 ,units = "cm", res = 300)
plotWindowSweep(bird_sweep)
# each color = a window

# to identify the change points : 
bird.cp <- findCandidateChangePoints(windowsweep = bird_sweep, clusterwidth = 12)
# clusterwidth = A time span within which very close change points are considered a single chagne point. 
abline(v = bird.cp)

# Display predefined season dates : 15 sept to 14 nov ; 15 nov to 14 feb ; 15 feb to 14 jun ; 15 jun to 14 sept (TO CHECK IN OFB COMPUTER)
season_dates <- as.data.frame(bird_sf %>% select(saison, saison2,study.local.timestamp) %>% group_by(saison2) %>% slice(1,n())) %>% select(-geometry)
odd_numbers <- seq(1, nrow(season_dates), by = 2)
even_numbers <- seq(2, nrow(season_dates), by = 2)

# Define a color palette for each season
season_colors <- c(
  "automne" = "orange",  # color for autumn
  "hiver" = "blue",      # color for winter
  "ete" = "deeppink",       # color for summer
  "printemps" = "seagreen3"   # color for spring
)

# Add a new column 'color' based on the 'saison' column
season_dates <- season_dates %>% mutate(color = season_colors[saison])


for(specific_season in seq_along(odd_numbers))
{
  segments(x0 = as.POSIXct(season_dates[odd_numbers[specific_season],"study.local.timestamp"]) , 
           y0 = 0, 
           x1 = as.POSIXct(season_dates[even_numbers[specific_season],"study.local.timestamp"]), 
           y1 = 0,
         col = season_dates[odd_numbers[specific_season],"color"], lwd = 2)
}
# Add legend
legend("bottomleft", legend = unique(season_dates[,1]), col = unique(season_dates$color), lwd = 2,
       inset=c(0,-0.2), xpd=TRUE, bty="n", horiz=TRUE)

while(dev.cur() > 1) {
  dev.off()
}




# is each one of the change point significant?
bird.cptable <- getCPtable(bird.cp, modelset = "UCVM", criterion = "BIC")

bird.phaselist <- estimatePhases(bird.cptable)

summarizePhases(bird.phaselist)


# Function from Eli Guararie 
# https://rdrr.io/github/EliGurarie/smoove/src/R/plotPhaseParameter.R

#' Plot Estimates From RACVM Partition
#' 
#' @details \code{getVariable} takes a partitionlist and generates a table indicating the start time, end time, estimate, low and high 95\% C.I. and selected model for a given parameter. 
#' @details \code{plotVariable} 
#' 
#' @param variable one of: \code{omega}, \code{mu.x}, \code{mu.y}, \code{eta}, \code{tau}
#' @param phaselist output of \code{\link{estimatePhases}}, i.e. a list of information on each of the selected phases
#' @param cols colors of bars (by default - a rich color palette)
#' @param label whether to label the plotted parameter (with the word "tau", "eta", etc.)
#' @param ymax maximum y
#' @param ... additional arguments to pass to plotting function.  A useful one is \code{log = "y"} for highly skewed data.
#' @return \code{getVariable} returns a data.frame with start time, end time, estimate, low and high 95\% C.I. and selected model for a given parameter, with each row corresponding to an enumerated partition phase.  
#' @examples 
#' library(smoove)
#' library(magrittr)
#' data(simSweep, package="smoove")
#' 
#' layout(c(1,1,1,2:6))
#' simCP.table <- simSweep %>%
#'   findCandidateChangePoints(clusterwidth = 4, verbose = FALSE) %>%
#'   getCPtable(modelset = c("UCVM", "ACVM"), criterion = "AIC")
#' 
#' simPhaselist <- estimatePhases(simCP.table)
#' Z <- attributes(simPhaselist)$Z
#' T <- attributes(simPhaselist)$time
#' 
#' if (!requireNamespace('gplots', quietly = TRUE))
#'  warning("package \"gplots\" is needed for this example. 
#'  Please install it first and then run this example",
#'         call. = FALSE)
#' 
#' cols <- gplots::rich.colors(length(simPhaselist))
#' T.cuts <- c(T[1], simCP.table$CP, T[length(T)])
#' Z.cols <- cols[cut(T, T.cuts, include.lowest = TRUE)]
#' 
#' phaseTable <- summarizePhases(simPhaselist)
#' plot(Z, asp=1, type="l", xpd=FALSE)
#' points(Z, col=Z.cols, pch=21, bg = scales::alpha(Z.cols, 0.5), cex=0.8)
#' legend("top", legend = paste0(phaseTable$phase, ": ", phaseTable$model),
#'        fill=cols, ncol=3, bty="n", title = "Phase: model")
#' 
#' par(mar=c(2,2,1,0), xpd=NA)
#' plotPhaseParameter("tau", simPhaselist, ylab="", xaxt="n", xlab="", col=cols, log="y")
#' plotPhaseParameter("eta", simPhaselist, ylab="", xaxt="n", xlab="", col=cols)
#' plotPhaseParameter("mu.x", simPhaselist, ylab= "", xaxt="n", xlab="", col=cols)
#' plotPhaseParameter("mu.y", simPhaselist, ylab= "", xaxt="n", xlab="", col=cols)
#' plotPhaseParameter("rms", simPhaselist, ylab= "", xlab="time", col=cols)
#' 
#' @export
#' 
plotPhaseParameter <- function(variable, phaselist, cols = 1:length(phaselist), 
                               label = TRUE, ...){
  variabletable <- getPhaseParameter(variable, phaselist)
  
  low.plot <- variabletable$low
  high.plot <- variabletable$high
  #high.plot[high.plot > ymax] <- ymax
  
  with(variabletable,{
    plot(range(start, end), 
         range(low.plot, high.plot, na.rm=TRUE), 
         type="n", ...)
    if(!is.na(low[1])) rect(start, low.plot, end, high.plot, col = alpha(cols, .5), bor=NA) 
    
    segments(start, hat, end, hat, lwd = 2, col=cols)
    segments(end[-length(end)], hat[-length(end)], start[-1], hat[-1], col="grey")
    
    if(label) mtext(variable, side = 3, at = start[1], font = 3, adj = 0, cex = 0.8)
    
    for(specific_season in seq_along(odd_numbers))
    {
      segments(x0 = as.POSIXct(season_dates[odd_numbers[specific_season],"study.local.timestamp"]) , 
               y0 = max(high.plot), 
               x1 = as.POSIXct(season_dates[even_numbers[specific_season],"study.local.timestamp"]), 
               y1 = max(high.plot),
               col = season_dates[odd_numbers[specific_season],"color"], lwd = 3)
    }
    # Add legend
    legend("bottomright", legend = unique(season_dates[,1]), col = unique(season_dates$color), lwd = 2,
           bty="n", y.intersp=0.8, x.intersp=0.5, inset=c(0,0.06))
    
    # legend("bottomleft", legend = unique(season_dates[,1]), col = seq_along(season_dates$saison), lwd = 2,
    #        inset=c(0,-0.11), xpd=NA, bty="n", horiz=TRUE,  x.intersp = 0)
    
    
  })
}




# Plot displaying function from Eli Guararie 
# https://rdrr.io/github/EliGurarie/smoove/src/R/plotPhaseList.R 
#' Plot Phase Partitioning of RACVM analysis

#' @param phaselist Complete "smoove" partitioning output - returned by \link{estimatePhases}
#' @param cols colors for phases (by default uses the `rich.colors` palette from `gplots`)
#' @param plot.parameters whether the parameters are plotted
#' @param parameters which parameters to plot (by default - ALL of the estimated parameters)
#' @param plot.legend whether to plot a legend
#' @param legend.where location of legend
#' @param layout  "horizontal" (default) or "vertical"- as preferred (partial string matching accepted)
#' @export
#' @example demo/plotPhaseList_example.R

plotPhaseList <- function(phaselist,
                          cols = gplots::rich.colors(length(phaselist)),
                          plot.parameters = TRUE,
                          parameters = NULL, 
                          plot.legend = TRUE, 
                          legend.where = "bottomright",
                          layout = c("horizontal","vertical")){
  
  Z <- attr(phaselist, "Z")
  time <- attr(phaselist, "time")
  phaseTable <- summarizePhases(phaselist)
  
  if(is.null(parameters)){
    allparameters <-  c("eta", "tau", "rms", "mu.x", "mu.y", "omega.x", "omega.y")
    parameters <- names(phaseTable)[names(phaseTable) %in% allparameters]
  }
  
  mars <- par()$mar
  omas <- par()$oma
  omas[1] <- 2
  
  n.param <- length(parameters)
  
  if(plot.parameters){
    if(grepl(layout[1], "vertical")){
      layout(1:(n.param + 1), 
             heights = c(1, rep(1/n.param, n.param)))
      par(mar = c(1, mars[2], 2, mars[4]), oma = omas)
    }
    else{
      par(mar = c(0, mars[2], 2, mars[4]), oma = omas)
      layout(cbind(rep(1, n.param), 1:n.param+1))
    }
  }
  
  T.cuts <- c(phaseTable$start, max(time))
  Z.cols <- cols[cut(time, T.cuts, include.lowest = TRUE)] 
  
  # plot Track
  plot(Z, asp=1, type="l", xpd=FALSE, xlab = "X", ylab = "Y")
  points(Z, col=Z.cols, pch=21, bg = alpha(Z.cols, 0.5), cex=0.8)
  if(plot.legend){
    legend(legend.where, legend = paste0(phaseTable$phase, ": ", phaseTable$model), 
           fill=cols, ncol=3, bty="n", title = "Phase: model")
  }
  
  # plotParameters
  if(plot.parameters){  
    mars <- par()$mar
    par(mar = c(0, mars[2], 1.5, mars[4]))
    for(p in parameters)
      plotPhaseParameter(p, phaselist, ylab="", xlab="", col=cols, 
                         xaxt= ifelse(p == parameters[length(parameters)], "s", "n"),
                         log = ifelse(p =="tau", "y", ""))
  }
}


par(mfrow=c(2,2))
plotPhaseList(bird.phaselist)

# eta = random rms speed of the movement process
# rms = root mean square (rms) speed of the movement process
# tau = time scale of autocorrelation




