#### PhD TetraAlps project ####

# Alice Bordes #

# January 2025 #

# Description:

# Visualization visitor numbers time series



### Loading libraries ---- 
#********************************************************************
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)
#********************************************************************

# Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/Formatting_data/formatting_environment_data.R")
#********************************************************************


### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD/Tetralps"
#********************************************************************


# Loading data ----
#********************************************************************
# Visitor numbers
visitor_meribel <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/meribel_visitors.csv", sep=",")
visitor_meribel$Date <- as.Date(visitor_meribel$Date)

visitor_valtho <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/valtho_visitors.csv", sep=",")
visitor_valtho$Date <- as.Date(visitor_valtho$Date)

visitor_courch <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/courchevel_visitors.csv", sep=",")
visitor_courch$Date <- as.Date(visitor_courch$Date)

visitor_menui <- read.csv2("C:/Users/albordes/Documents/PhD/TetrAlps/2_DATA/ski_resorts_visitor_numbers/menuires_visitors.csv", sep=",")
visitor_menui$Date <- as.Date(visitor_menui$Date)
visitor_menui$Total <- as.integer(visitor_menui$Total)
#********************************************************************


### 1_Visualizing temporal predictors : daily visitor number ----
#********************************************************************

### 1.1_Daily visitor number across winter and ski seasons ----
#********************************************************************
visitor_nb <- rbind(visitor_menui %>% dplyr::select(Date, Total, ski_season) %>% mutate("resort" = "Les Ménuires"), 
                    visitor_meribel %>% dplyr::select(Date, Total, ski_season) %>% mutate("resort" = "Méribel"),
                    visitor_valtho %>% dplyr::select(Date, Total, ski_season) %>% mutate("resort" = "Val Thorens"),
                    visitor_courch %>% dplyr::select(Date, Total, ski_season) %>% mutate("resort" = "Courchevel"))

gg <- list()

for(i in 1:length(unique(visitor_nb$ski_season)))
{
  gg[[i]] <- ggplot()+
    geom_line(data = visitor_nb %>% filter(ski_season == unique(visitor_nb$ski_season)[i]) %>% filter(Date > as.Date("2016-01-01")), 
              aes(x = Date, y = Total, color = resort)) +
    facet_wrap(~resort, ncol = 1) +
    ggtitle(paste0("Period ", unique(visitor_nb$ski_season)[i])) +
    theme(legend.position = "none")
}

yearly_visitor_trends <- do.call(grid.arrange, c(gg, ncol = length(unique(visitor_nb$ski_season))))
#********************************************************************


### 1.2_Daily visitor number across winter and ski seasons ----
#********************************************************************

# download data on meteo france : https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=94&id_rubrique=32 
# metadata : https://donneespubliques.meteofrance.fr/client/document/doc_parametres_nivo_197.pdf ; http://www.meteo.fr/meteonet/DIR_reso40/fichiers_obs_france_web_reso40_f.htm 

# - Temperature (t) 
# - Average wind speed 10 mn (ff)
# - Precipitations
# - Total nebulosity (n)



#********************************************************************





# # 1_Data frame with total visitors/resort
# tot_dt <- as.data.frame(valtho_visitors %>% select(Date,Total))
# names(tot_dt) <- c("Date", "Valtho_tot")
# tot_dt <- full_join(tot_dt, courchevel_visitors %>% select(Date, Total), by = "Date")
# names(tot_dt) <- c("Date", "Valtho_tot", "Courch_tot")
# tot_dt <- full_join(tot_dt, meribel_visitors %>% select(Date, Total), by = "Date")
# names(tot_dt) <- c("Date", "Valtho_tot", "Courch_tot", "Meri_tot")
# tot_dt <- full_join(tot_dt, menuires_visitors %>% select(Date, Total), by = "Date")
# names(tot_dt) <- c("Date", "Valtho_tot", "Courch_tot", "Meri_tot", "Menui_tot")
# 
# 
# # Create an index for each resort (to represent sequential winter days)
# tot_dt <- tot_dt %>%
#   pivot_longer(cols = starts_with("Valtho_tot"):starts_with("Menui_tot"), 
#                names_to = "resort", 
#                values_to = "total") %>%
#   arrange(resort, Date) %>%
#   group_by(resort) %>%
#   mutate(index = row_number())  # Create a sequential index for each resort's data
# 
# tot_dt <- tot_dt[-1,]
# 
# #order by Date
# tot_dt <- tot_dt[order(tot_dt$Date),]
# 
# 
# # Visualization of the variable distribution 
# boxplot(combined_telemetry$total.visitors.std) 
# text(y = boxplot.stats(combined_telemetry$total.visitors.std)$stats, labels = round(boxplot.stats(combined_telemetry$total.visitors.std)$stats,2), x = 1.3, pos = 2) 
# text(y = boxplot.stats(combined_telemetry$total.visitors.std)$stats, labels = boxplot.stats(combined_telemetry$total.visitors)$stats, x = 1.35, pos = 4)
# ##
# 
# 
# 
# # 2_Plot
# 
# # Select which labels to display (e.g., every 5th index)
# skip_labels <- seq(1, length(df_tot$index), by = 30)  # Skip every 5th label
# 
# # Plot using ggplot
# ggplot(df_tot, aes(x = index, y = total, color = as.factor(resort), group = resort)) +
#   geom_line(size = 1) +  # Line plot for visitors over the index
#   facet_wrap(~resort, scales = "free_x", ncol = 1) +  # One graph per resort
#   labs(
#     x = "Winter Days (Sequential)",
#     y = "Total Visitors (Standardized)",
#     color = "Resort"
#   ) +
#   scale_color_brewer(palette = "Set2") +
#   scale_x_discrete(
#     labels = df_tot$Date[skip_labels],  # Map the index to the actual "jour" values
#     breaks = df_tot$index[skip_labels]  # Skip every 5th label (or adjust the step)
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
#     strip.text = element_text(size = 12, face = "bold")  # Bold facet titles
#   )
# 
