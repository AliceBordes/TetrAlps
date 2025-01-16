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
# install_github("EliGurarie/bcpa")
# library(bcpa)
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
birds_bg_dt <- read.csv2(file.path(base,"Tetralps/2_DATA/data_bg_pretelemetry_2024_10.csv"),sep=",") #upload the file from a csv, not a move2 object
#********************************************************************

# Loading functions ----
#********************************************************************
source(file.path(base,"Tetralps/4_FUNCTIONS/coord_segmentation/plot_coord.R")) 
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/RSF/rsf_functions.R")
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
# Predefined season dates : 15 sept to 14 nov ; 15 nov to 14 feb ; 15 feb to 14 jun ; 15 jun to 14 sept (TO CHECK IN OFB COMPUTER)
coordXY(vect_ani)
coordXY_multiple_bg(vect_ani, "G", write = TRUE)


# count the results, considering the predefined season is included in winter patterns identified in the coordinate time series, considering the info is available only if telemetry data are available during the whole predefined winter

proportion_bird_seg <- read.csv2(file.path(base,"Tetralps/5_OUTPUTS/data_exploration/season_segmentation/breakpoints_synthesis.csv"),sep=";") #upload the file from a csv, not a move2 object

# replace absent_brk by no
proportion_bird_seg <- proportion_bird_seg %>%
  mutate(across(everything(), ~ ifelse(. == "absent_brk", "no", .)))

proportion_bird_seg <- proportion_bird_seg %>%
  mutate(
    results_winter1 = ifelse(winter1_first_breakpoint_before_November15 == "" & 
                               winter1_second_breakpoint_after_February14 == "",
                             NA,
                             ifelse(
                               winter1_first_breakpoint_before_November15 == "yes" & 
                                 winter1_second_breakpoint_after_February14 == "yes", 
                               "yes", 
                               ifelse(
                                 winter1_first_breakpoint_before_November15 == "no" & 
                                   winter1_second_breakpoint_after_February14 == "no", 
                                 "no", 
                                 "missing_data"
                               )
                             )
    ))

proportion_bird_seg <- proportion_bird_seg %>%
  mutate(
    results_winter2 = ifelse(winter2_first_breakpoint_before_November15 == "" & 
                               winter2_second_breakpoint_after_February14 == "",
                             NA,
                             ifelse(
                               winter2_first_breakpoint_before_November15 == "yes" & 
                                 winter2_second_breakpoint_after_February14 == "yes", 
                               "yes", 
                               ifelse(
                                 winter2_first_breakpoint_before_November15 == "no" & 
                                   winter2_second_breakpoint_after_February14 == "no", 
                                 "no", 
                                 "missing_data"
                               )
                             )
    ))

proportion_bird_seg <- proportion_bird_seg %>%
  mutate(
    results_winter3 = ifelse(winter3_first_breakpoint_before_November15 == "" & 
                               winter3_second_breakpoint_after_February14 == "",
                             NA,
                             ifelse(
                               winter3_first_breakpoint_before_November15 == "yes" & 
                                 winter1_second_breakpoint_after_February14 == "yes", 
                               "yes", 
                               ifelse(
                                 winter3_first_breakpoint_before_November15 == "no" & 
                                   winter3_second_breakpoint_after_February14 == "no", 
                                 "no", 
                                 "missing_data"
                               )
                             )
    ))



# Count for each winter
count_winter1 <- proportion_bird_seg %>% count(results_winter1)
count_winter2 <- proportion_bird_seg %>% count(results_winter2)
count_winter3 <- proportion_bird_seg %>% count(results_winter3)

# Rename the columns for clarity
count_winter1 <- count_winter1 %>% rename(winter1_count = n)
count_winter2 <- count_winter2 %>% rename(winter2_count = n)
count_winter3 <- count_winter3 %>% rename(winter3_count = n)

# Full join to combine the counts
combined_counts <- count_winter1 %>%
  full_join(count_winter2, by = c("results_winter1" = "results_winter2")) %>%
  full_join(count_winter3, by = c("results_winter1" = "results_winter3"))

# Replace NA with 0 for clarity
combined_counts[is.na(combined_counts)] <- 0

# View the combined counts
combined_counts <- combined_counts %>% mutate(results = rowSums(select(., winter1_count, winter2_count, winter3_count), na.rm = TRUE))
combined_counts <- combined_counts[-nrow(combined_counts),] #drop the NA
combined_counts <- combined_counts %>% mutate(results_prop = round((results/sum(combined_counts$results))*100,1))

combined_counts


# count the results, considering the predefined season is included in winter patterns identified in the coordinate time series, even if the second breakpoint in the coordinate time series is unknown beacause of a lack of data

proportion_bird_seg2 <- read.csv2(file.path(base,"Tetralps/5_OUTPUTS/data_exploration/season_segmentation/breakpoints_synthesis.csv"),sep=";") #upload the file from a csv, not a move2 object

proportion_bird_seg2 <- proportion_bird_seg2 %>%
  mutate(
    results_winter1 = ifelse(winter1_first_breakpoint_before_November15 == "" & 
                               winter1_second_breakpoint_after_February14 == "",
                             NA,
                             ifelse(
                               winter1_first_breakpoint_before_November15 == "yes" | 
                                 winter1_second_breakpoint_after_February14 == "yes", 
                               "yes", 
                               ifelse(
                                 winter1_first_breakpoint_before_November15 == "no" | 
                                   winter1_second_breakpoint_after_February14 == "no", 
                                 "no", 
                                 "missing_data"
                               )
                             )
    ))

proportion_bird_seg2 <- proportion_bird_seg2 %>%
  mutate(
    results_winter2 = ifelse(winter2_first_breakpoint_before_November15 == "" & 
                               winter2_second_breakpoint_after_February14 == "",
                             NA,
                             ifelse(
                               winter2_first_breakpoint_before_November15 == "yes" | 
                                 winter2_second_breakpoint_after_February14 == "yes", 
                               "yes", 
                               ifelse(
                                 winter2_first_breakpoint_before_November15 == "no" | 
                                   winter2_second_breakpoint_after_February14 == "no", 
                                 "no", 
                                 "missing_data"
                               )
                             )
    ))

proportion_bird_seg2 <- proportion_bird_seg2 %>%
  mutate(
    results_winter3 = ifelse(winter3_first_breakpoint_before_November15 == "" & 
                               winter3_second_breakpoint_after_February14 == "",
                             NA,
                             ifelse(
                               winter3_first_breakpoint_before_November15 == "yes" | 
                                 winter1_second_breakpoint_after_February14 == "yes", 
                               "yes", 
                               ifelse(
                                 winter3_first_breakpoint_before_November15 == "no" | 
                                   winter3_second_breakpoint_after_February14 == "no", 
                                 "no", 
                                 "missing_data"
                               )
                             )
    ))



# Count for each winter
count_winter1.2 <- proportion_bird_seg2 %>% count(results_winter1)
count_winter2.2 <- proportion_bird_seg2 %>% count(results_winter2)
count_winter3.2 <- proportion_bird_seg2 %>% count(results_winter3)

# Rename the columns for clarity
count_winter1.2 <- count_winter1.2 %>% rename(winter1_count = n)
count_winter2.2 <- count_winter2.2 %>% rename(winter2_count = n)
count_winter3.2 <- count_winter3.2 %>% rename(winter3_count = n)

# Full join to combine the counts
combined_counts.2 <- count_winter1.2 %>%
  full_join(count_winter2.2, by = c("results_winter1" = "results_winter2")) %>%
  full_join(count_winter3.2, by = c("results_winter1" = "results_winter3"))

# Replace NA with 0 for clarity
combined_counts.2[is.na(combined_counts.2)] <- 0

# View the combined counts
combined_counts.2 <- combined_counts.2 %>% mutate(results = rowSums(select(., winter1_count, winter2_count, winter3_count), na.rm = TRUE))
combined_counts.2 <- combined_counts.2[-nrow(combined_counts.2),] #drop the NA
combined_counts.2 <- combined_counts.2 %>% mutate(results_prop = round((results/sum(combined_counts.2$results))*100,1))

combined_counts.2
#********************************************************************



### 3_Finding changing points ----
#********************************************************************
# detach(package:plyr) # if plyr is loaded after dplyr, I get an overall summary instead of a grouped summary
brkplot <- brkpts_coordXY(vect_name = vect_ani, 
                          threshold = 1.2, 
                          write = TRUE, 
                          outputfolder = file.path(base,"TetrAlps/5_OUTPUTS/data_exploration/season_segmentation/breakpoints"))

brkplot

#********************************************************************




### 4_Finding changing points with bcpa ----
#********************************************************************
with(bird_dt, plot(study.local.timestamp, location.long, type = "o"))
depth.ws <- WindowSweep(bird_dt, variable = "location.long", time.var = "study.local.timestamp", windowsize = 25, windowstep = 1, progress=FALSE)


#********************************************************************




### 5_Looking at the overlap between the different winters encountered by 1 bird ----
#********************************************************************

# If we take a larger period for winter season 
        # Ensure `jour` is a Date object
        birds_bg_dt <- birds_bg_dt %>%
          mutate(jour = as.Date(jour)) # Ensure `jour` is in Date format
        
        # Add "Day of the Year" (`yday`) and a custom filter for the range
        filtered_birds_bg_dt <- birds_bg_dt %>%
          mutate(
            month_day = format(jour, "%m-%d"), # Optional: For debugging or reference
            # yday = yday(jour)                 # Extract the day of the year
          ) %>%
          filter(
            (month(jour) == 11 & day(jour) >= 15) |   # After November 15
              (month(jour) <= 4 & !(month(jour) == 4 & day(jour) > 15)) # Before April 15
          )
        
        head(filtered_birds_bg_dt)




# Creation of the telemetry, guess, fit and akde lists
tele_akde(data = filtered_birds_bg_dt,
          # birds_vect = names(l_telemetry_winterS),
          season = "hiver",
          subset_category = "all",
          outputfolder = file.path(base, "Tetralps", "3_R", "0_Heavy_saved_models", "birds_3V"),
          write = FALSE)
#load()




# Selection of the birds with multiple winters
# birds_multipl_winters <- c("Alpha","Caramel","Dalton","Dario","Donald","Dynamite","Dyonisos","Eros","Fast","Fleau","Foliedouce")
        l_telemetry_winter_multipl_win <- l_telemetry_winter[birds_multipl_winters] 
        l_fit_winter_multipl_win <- l_fit_winter[birds_multipl_winters] 
        
        dt_overlap_synthesis <- overlap_winter(telemetry_list = l_telemetry_winter_multipl_win, 
                                               fit_list = l_fit_winter_multipl_win, 
                                               overlap_type = "multiple winters")
        dt_overlap_synthesis$animal <- as.factor(dt_overlap_synthesis$animal)
         
        label_close_to_pts <- jitter(scale((as.numeric(factor(dt_overlap_synthesis$animal )))))

ggplot(data = dt_overlap_synthesis, aes(y = estimated_overlap))+
  geom_boxplot(fill = alpha("lightgreen", 0.4), width=max(abs(label_close_to_pts))*2)+
  geom_point(data = dt_overlap_synthesis, aes(x = label_close_to_pts), width = 0.2, color = "black", size = 3)+
  geom_text(data = dt_overlap_synthesis, aes(x = label_close_to_pts, label = animal), 
            position = position_jitter(width = 0.15, height = 0),  # Match jitter width to geom_jitter for alignment
            size = 5,  # Slightly above points
            hjust = 0.5,
            vjust = - 0.5) + # Center the labels horizontally
  labs(x = "Birds that have encounter multiple winters ",
       y = "Estimated minimum overlap between winters encountered by a bird (%)", 
       title = paste0("Estimated minimum overlap between winters encountered \nby the birds that have encounter multiple winters (mean = ",round(mean(dt_overlap_synthesis$estimated_overlap),2)*100,"%)"))+
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x=element_blank())
#********************************************************************




### 6_Looking at the overlap between the predefined and the large winter season ----
#********************************************************************
load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_telemetry_winter_all.Rdata"))
load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_guess_winter_all.Rdata"))
load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_fit_winter_all.Rdata"))
load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_akde_winter_all.Rdata"))

l_telemetry_winter_large_win <- l_telemetry_winter[!names(l_telemetry_winter) %in% birds_multipl_winters] 
l_fit_winter_large_win <- l_fit_winter[!names(l_telemetry_winter) %in% birds_multipl_winters] 

load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_telemetry_winter_saison2.Rdata"))
load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_guess_winter_saison2.Rdata"))
load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_fit_winter_saison2.Rdata"))
load(file = file.path(base,"Tetralps","3_R","0_Heavy_saved_models","birds_3V","multipl_akde_winter_saison2.Rdata"))

l_telemetry_winter_predefined_win <- l_telemetry_winter[!names(l_telemetry_winter) %in% birds_multipl_winters] 
l_fit_winter_predefined_win <- l_fit_winter[!names(l_telemetry_winter) %in% birds_multipl_winters] 

dt_overlap_synthesis <- overlap_winter(telemetry_list = l_telemetry_winter_predefined_win,
                                       telemetry_list2 = l_telemetry_winter_large_win,
                                       fit_list = l_fit_winter_predefined_win, 
                                       fit_list2 = l_fit_winter_large_win, 
                                       overlap_type = "season length")
dt_overlap_synthesis$animal <- as.factor(dt_overlap_synthesis$animal)

label_close_to_pts <- jitter(scale((as.numeric(factor(dt_overlap_synthesis$animal )))))

ggplot(data = dt_overlap_synthesis, aes(y = estimated_overlap))+
  geom_boxplot(fill = alpha("lightgreen", 0.4), width=max(abs(label_close_to_pts))*2)+
  geom_point(data = dt_overlap_synthesis, aes(x = label_close_to_pts), width = 0.2, color = "black", size = 3)+
  geom_text(data = dt_overlap_synthesis, aes(x = label_close_to_pts, label = animal), 
            position = position_jitter(width = 0.15, height = 0),  # Match jitter width to geom_jitter for alignment
            size = 5,  # Slightly above points
            hjust = 0.5,
            vjust = - 0.5) + # Center the labels horizontally
  labs(x = "Birds that have encounter multiple winters ",
       y = "Estimated minimum overlap between winters encountered by a bird (%)", 
       title = paste0("Estimated minimum overlap between winters encountered \nby the birds that have encounter multiple winters (mean = ",round(mean(dt_overlap_synthesis$estimated_overlap),2)*100,"%)"))+
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x=element_blank())
#********************************************************************



### Finding changing points with smoove ---- --> not correct if velocities not autocorrelated
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

# Display predefined season dates : 16 sept to 15 dec ; 16 dec to 15 mar ; 16 mar to 15 jun ; 16 jun to 15 sept 
season_dates <- as.data.frame(bird_sf %>% dplyr::select(saison, saison2,timestamp) %>% group_by(saison2) %>% slice(1,n())) %>% dplyr::select(-geometry)
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




