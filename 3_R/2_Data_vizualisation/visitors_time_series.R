#### PhD TetraAlps project ####

# Alice Bordes #

# January 2025 #

# Description:

# Visualization visitor numbers time series



### Loading libraries ---- 
#********************************************************************
library(ggplot2)
library(tidyverse)
library(dplyr)
#********************************************************************

# Loading functions ----
#********************************************************************
source("C:/Users/albordes/Documents/PhD/TetrAlps/4_FUNCTIONS/Formatting_data/formatting_environment_data.R")
#********************************************************************


### Settings ----
#********************************************************************
base <- "C:/Users/albordes/Documents/PhD"
#********************************************************************


# Loading data ----
#********************************************************************
# Ski resort visitors
meribel_visitors <- meribel_formatting(save = TRUE)
courchevel_visitors <- courchevel_formatting(save = TRUE)
valtho_visitors <- valtho_formatting(save = TRUE)
menuires_visitors <- menuires_formatting(save = TRUE)
#********************************************************************


# 1_Data frame with total visitors/resort
tot_dt <- as.data.frame(valtho_visitors %>% select(Date,Total))
names(tot_dt) <- c("Date", "Valtho_tot")
tot_dt <- full_join(tot_dt, courchevel_visitors %>% select(Date, Total), by = "Date")
names(tot_dt) <- c("Date", "Valtho_tot", "Courch_tot")
tot_dt <- full_join(tot_dt, meribel_visitors %>% select(Date, Total), by = "Date")
names(tot_dt) <- c("Date", "Valtho_tot", "Courch_tot", "Meri_tot")
tot_dt <- full_join(tot_dt, menuires_visitors %>% select(Date, Total), by = "Date")
names(tot_dt) <- c("Date", "Valtho_tot", "Courch_tot", "Meri_tot", "Menui_tot")


# Create an index for each resort (to represent sequential winter days)
tot_dt <- tot_dt %>%
  pivot_longer(cols = starts_with("Valtho_tot"):starts_with("Menui_tot"), 
               names_to = "resort", 
               values_to = "total") %>%
  arrange(resort, Date) %>%
  group_by(resort) %>%
  mutate(index = row_number())  # Create a sequential index for each resort's data

tot_dt <- tot_dt[-1,]

#order by Date
tot_dt <- tot_dt[order(tot_dt$Date),]


# 2_Plot

# Select which labels to display (e.g., every 5th index)
skip_labels <- seq(1, length(df_tot$index), by = 30)  # Skip every 5th label

# Plot using ggplot
ggplot(df_tot, aes(x = index, y = total, color = as.factor(resort), group = resort)) +
  geom_line(size = 1) +  # Line plot for visitors over the index
  facet_wrap(~resort, scales = "free_x", ncol = 1) +  # One graph per resort
  labs(
    x = "Winter Days (Sequential)",
    y = "Total Visitors (Standardized)",
    color = "Resort"
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_x_discrete(
    labels = df_tot$Date[skip_labels],  # Map the index to the actual "jour" values
    breaks = df_tot$index[skip_labels]  # Skip every 5th label (or adjust the step)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    strip.text = element_text(size = 12, face = "bold")  # Bold facet titles
  )

