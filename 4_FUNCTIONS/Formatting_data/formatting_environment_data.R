#### PhD TetraAlps project ####

# Alice Bordes #

# November 2024 #

# Description:

# Formatting environment dataset for RSF and SSF




# Functions to formate individually each visitor numbers dataset provided by ski resorts
#********************************************************************
# Ski resort visitors
# Méribel Mottaret
meribel_formatting <- function(folderpath = file.path(base,"TetrAlps","1_RAW_DATA","human_traffic_in_ski_resorts","Meribel_mottaret_RM_Historique_des_passages_2017_22/"),sheetname = "Passage Détail", save = FALSE, folderoutpath = file.path(base,"TetrAlps/2_DATA/ski_resorts_visitor_numbers"))
{
  resort_files <- list.files(path = folderpath,pattern = "^[^~$].*\\.xlsx$", full.names = TRUE) # function to exclude files that start with ~$ and keep those that finish with .xlsx
  
  list_dt <- list()
  
  for(file in seq_along(resort_files))
  {
    year <- substr(resort_files[file],nchar(resort_files[file])-9,nchar(resort_files[file])-5) 
    year1 <- as.numeric(substr(year,1,2))+2000
    year2 <- as.numeric(substr(year,4,5))+2000 
    
    data <- tryCatch(
      {
        read.xlsx(xlsxFile = resort_files[file], sheet = sheetname)
      }, 
      error = function(e) 
      {
        message("Error reading file: ", resort_files[file], "\n", e)
        return(NULL)
      })
    
    names(data)[1] <- "Mois" ; names(data)[2] <- "Jour"
    data <- data %>% fill(Mois, .direction = "down") # `fill()` defaults to replacing missing data from top to bottom
    data <- data %>% replace(is.na(.), 0) # assuming all the NA = 0 visitors
    data <- data %>% mutate(Date = paste(Mois, Jour, sep = "-"))
    data$Date <- as.Date(data$Date)
    data <- data %>% dplyr::select(Date, everything()) %>% dplyr::select(-Mois) %>% dplyr::select(-Jour)
    data <- data %>% mutate(Total = rowSums(dplyr::select(., where(is.numeric)), na.rm = TRUE))
    
    #assign the name of the season 
    data$ski_season <- paste0(year1,"_",year2)
    
    list_dt[[file]] <- data
  }
  # Assuming `list_dt` contains your data frames
  merged_data <- bind_rows(list_dt)
  
  if(save == TRUE)
  {
    write.csv(merged_data, file = file.path(folderoutpath,"meribel_visitors.csv"), row.names = FALSE)
  }
  
  return(merged_data)
}





# Courchevel
courchevel_formatting <- function(folderpath = file.path(base,"TetrAlps","1_RAW_DATA","human_traffic_in_ski_resorts","Courchevel_RM_Historique_des_passages_2017_22/"),sheetname = "Details", save = FALSE, folderoutpath = file.path(base,"TetrAlps/2_DATA/ski_resorts_visitor_numbers"))
{
  resort_files <- list.files(path = folderpath,pattern = "^[^~$].*\\.xlsx$", full.names = TRUE) # function to exclude files that start with ~$ and keep those that finish with .xlsx
  list_dt <- list()
  
  for(file in seq_along(resort_files))
  {
    year <- substr(resort_files[file],nchar(resort_files[file])-8,nchar(resort_files[file])-5)
    data <- tryCatch(
      {
        read.xlsx(xlsxFile = resort_files[file], sheet = sheetname, startRow = 3)
      }, 
      error = function(e) 
      {
        message("Error reading file: ", resort_files[file], "\n", e)
        return(NULL)
      })
    
    names(data)[1] <- "Jour" ; names(data)[2] <- "Date"
    data <- data %>% filter(Jour %in% c("lun.","mar.","mer.","jeu.","ven.","sam.","dim."))
    data <- data %>% replace(is.na(.), 0) # assuming all the NA = 0 visitors
    
    data$Date <- excel_numeric_to_date(as.numeric(data$Date))
    
    
    # correct the year 
    for(i in 1:nrow(data))
    {
      if(lubridate::month(data$Date[i]) < 11 && is.na(data$Date[i])==FALSE)
      {
        lubridate::year(data$Date[i]) <- as.numeric(year)
      }
      if(lubridate::month(data$Date[i]) > 10 && is.na(data$Date[i])==FALSE)
      {
        lubridate::year(data$Date[i]) <- as.numeric(year)-1
      }
      if(is.na(data$Date[i])==TRUE && lubridate::day(data$Date[i-1])==28 && lubridate::month(data$Date[i-1])==2)
      {
        data$Date[i] <- data$Date[i-1]+lubridate::days(1)
      }
    }
    
    data <- data %>% dplyr::select(.,-contains("otal"),-starts_with("X"))
    # Convert all columns to numeric except for "Jour" and "Date"
    data <- data %>% mutate(across(-c(Jour, Date), as.numeric))
    data <- data %>% mutate(Total = rowSums(dplyr::select(., where(is.numeric)), na.rm = TRUE))
    
    
    #assign the name of the season 
    data$ski_season <- paste0(as.numeric(year)-1,"_",as.numeric(year))
    
    data <- data %>% dplyr::select(-Jour)
    
    list_dt[[file]] <- data
  }
  # Merging data frames
  merged_data <- bind_rows(list_dt)
  
  if(save == TRUE)
  {
    write.csv(merged_data, file = file.path(folderoutpath,"courchevel_visitors.csv"), row.names = FALSE)
  }
  
  return(merged_data)
}




# ValThorens/Orelle
valtho_formatting <- function(folderpath = file.path(base,"TetrAlps","1_RAW_DATA","human_traffic_in_ski_resorts","ValThorens_Orelle_RM_Historique_des_passages_2017_22"), save = FALSE, folderoutpath = file.path(base,"TetrAlps/2_DATA/ski_resorts_visitor_numbers"))
{
  resort_files <- paste0(folderpath,".xlsx") # function to exclude files that start with ~$ and keep those that finish with .xlsx
  list_dt <- list()
  
  for(year in c(2017:2019,2021:2022))
  {
    data <- tryCatch(
      {
        read.xlsx(xlsxFile = resort_files, sheet = as.character(paste0(year,"-",year+1-2000)), startRow = 3)
      }, 
      error = function(e) 
      {
        message("Error reading file: ", resort_files, "\n", e)
        return(NULL)
      })
    
    data <- data[!grepl("otal", data[, 1]), ]
    data <- data[,-c(1,3)]
    
    data <- as.data.frame(t(data))
    
    lift_names <- data[1,]
    colnames(data) <- c("Date",lift_names[2:length(lift_names)])
    data <- data[-1,]
    
    data <- data %>% replace(is.na(.), 0) # assuming all the NA = 0 visitors
    data$Date <- excel_numeric_to_date(as.numeric(data$Date))
    
    # Convert all columns to numeric except for "Jour" and "Date"
    data <- data %>% mutate(across(-c(Date), as.numeric))
    data <- data %>% mutate(Total = rowSums(dplyr::select(., where(is.numeric)), na.rm = TRUE))
    
    
    #assign the name of the season 
    data$ski_season <- paste0(year,"_",year+1)
    
    list_dt[[year]] <- data
  }
  # Merging data frames
  merged_data <- bind_rows(list_dt)
  
  if(save == TRUE)
  {
    write.csv(merged_data, file = file.path(folderoutpath,"valtho_visitors.csv"), row.names = FALSE)
  }
  
  return(merged_data)
}





# Les Ménuires
menuires_formatting <- function(folderpath = file.path(base,"TetrAlps/1_RAW_DATA/human_traffic_in_ski_resorts/Les_Menuires_RM_Historique_des_passages_2017_22"), save = FALSE, folderoutpath = file.path(base,"TetrAlps/2_DATA/ski_resorts_visitor_numbers"))
{
  resort_files <- paste0(folderpath,".xlsx") # function to exclude files that start with ~$ and keep those that finish with .xlsx
  list_dt <- list()
  
  for(year in c(2017:2019,2021:2023))
  {
    data <- tryCatch(
      {
        read.xlsx(xlsxFile = resort_files, sheet = as.character(paste0(year,"-",year+1)), startRow = 2)
      }, 
      error = function(e) 
      {
        message("Error reading file: ", resort_files, "\n", e)
        return(NULL)
      })
    
    print(year)
    
    if(names(data)[1]=="X1" && names(data)[2]=="X2")
    {
      # Concatenate X1 and X2
      data <- data %>%
        mutate(X1 = paste(X1, X2, sep = " "))
      data <- data[,-2]
      data <- data[!grepl("otal", data$X1), ]
    }
    
    
    data <- data[, !grepl("totaux", names(data))]
    names(data)[1] <- "Date"
    data <- data[, !grepl("X", names(data))]
    data[1,] <- names(data)
    data <- data %>% filter(if_any(everything(), ~ !is.na(.)))
    
    
    if(any(duplicated(data$Date)))
    {
      duplicated_name <- unique(data$Date[duplicated(data$Date)])
      print(duplicated_name)
      
      data <- data[!(data$Date == duplicated_name & !duplicated(data$Date)), ] # the first line with "Bruyères2" = total "Bruyères1" + "Bruyères2" so we delete this one
    }
    
    row.names(data) <- data[,1]
    data <- data[,-1]
    
    
    
    data <- as.data.frame(t(data))
    data$Date <- excel_numeric_to_date(as.numeric(data$Date))
    data <- data %>% replace(is.na(.), 0) # assuming all the NA = 0 visitors
    
    data <- data[, !grepl("otal", names(data))]
    
    
    
    names(data) <- case_when(
      names(data) == "TPH Preyerand" ~ "TPH Preyerand",
      names(data) == "TB Croisette" ~ "TB Croisette",
      names(data) == "TK Jardin D'enfant" ~ "TK Jardin d'enfants", # Fixed case difference
      names(data) == "TK Jardin d'enfant" ~ "TK Jardin d'enfants", # Fixed case difference
      names(data) == "Tapis Pelvoux" ~ "Tapis Pelvoux",
      names(data) == "Tapis Plans" ~ "Tapis Plans",
      names(data) == "Tapis Reberty" ~ "Tapis Reberty",
      names(data) == "Tapis Bruyères" ~ "Tapis Bruyères",
      names(data) == "Tapis Preyerand" ~ "Tapis Preyerand",
      names(data) == "Tapis Biolley" ~ "Tapis Biolley",
      names(data) == "Luge Roc n'bob" ~ "LUGE ROC'N BOB", # Fixed casing and apostrophe
      names(data) == "LM Speed Moutain" ~ "LUGE Speed Mountain",
      names(data) == "LM TC Bruyères 1" ~ "TC Bruyères1",
      names(data) == "LM TC Bruyères 2" ~ "TC Bruyères2",
      names(data) == "LM TC Masse 1" ~ "TC Masse1",
      names(data) == "LM TC Masse 2" ~ "TC Masse2",
      names(data) == "LM TC Roc 1" ~ "TC Roc 1",
      names(data) == "LM TC St Martin 1" ~ "TC Saint Martin",
      names(data) == "LM TK Masse" ~ "TK Masse",
      names(data) == "LM TK Montaulever" ~ "TK Montaulever",
      names(data) == "LM TK Stade de Slalom" ~ "TK Stade",
      names(data) == "LM TK Teppes" ~ "TK Teppes",
      names(data) == "LM TK Village" ~ "TK Village",
      names(data) == "LM TSD Becca" ~ "TSD Becca",
      names(data) == "LM TSD Bettex" ~ "TSD Bettex",
      names(data) == "LM TSD Doron" ~ "TSD Doron",
      names(data) == "LM TSD Granges" ~ "TSD Granges",
      names(data) == "LM TSD Menuires" ~ "TSD Menuires",
      names(data) == "LM TSD Mont la Chambre" ~ "TSD Mont de la Chambre",
      names(data) == "LM TSD Reberty" ~ "TSD Reberty",
      names(data) == "LM TSD Roc 2" ~ "TSD Roc 2",
      names(data) == "LM TSD St Martin Express" ~ "TSD Saint Martin Express",
      names(data) == "LM TSD Sunny Express" ~ "TSD Sunny Express",
      names(data) == "LM TSF Lac Noir" ~ "TSF Lac Noir",
      names(data) == "LM TSF Rocher Noir" ~ "TSF Rocher Noir",
      names(data) == "LM TSF Tortollet" ~ "TSF Tortollet",
      TRUE ~ names(data) # Keep unmatched names as-is
    )
    
    # Convert all columns to numeric except for "Jour" and "Date"
    data <- data %>% mutate(across(-c(Date), as.numeric))
    data <- data %>% mutate(Total = rowSums(dplyr::select(., where(is.numeric)), na.rm = TRUE))
    
    #assign the name of the season 
    data$ski_season <- paste0(year,"_",year+1)
    
    list_dt[[year]] <- data
  }
  # Merging data frames
  merged_data <- bind_rows(list_dt)
  merged_data <- merged_data[-nrow(merged_data),]
  
  View(merged_data)
  
  if(save == TRUE)
  {
    write.csv(merged_data, file = file.path(folderoutpath,"menuires_visitors.csv"), row.names = FALSE)
  }
  
  return(merged_data)
}
#********************************************************************






# Functions to formate individually each snow depth dataset provided by ski resorts
#********************************************************************
### Snow depth    
# Méribel      
meribel_snow_formatting <- function(folderpath = file.path(base,"TetrAlps/1_RAW_DATA/environment/enneigement/meribel_meteo_france_neige.xlsx"),
                                    folderoutpath = file.path(base,"TetrAlps/2_DATA/snow_depth"), save = FALSE)
{
  list_data <- list()
  
  for(year in c(2016:2022))
  {
    data <- tryCatch(
      {
        read.xlsx(xlsxFile = folderpath, sheet = as.character(paste0("Saison ",year,".",year+1)))
      }, 
      error = function(e) 
      {
        message("Error reading file: ", folderpath, "\n", e)
        return(NULL)
      })
    colnames(data) <- c( "Date", "cumul.H.neige.cm", "Neige.fraiche.cm", "Cumul.neige.fraiche.avant.saison.cm")
    # Fill NA values in the Date column with the value above
    data <- data %>% fill(Date, .direction = "down")
    data$Date <- excel_numeric_to_date(data$Date)
    
    list_data[[year-2015]] <- data
  }
  
  snow_data <- do.call(rbind, list_data)
  
  if(save == TRUE)
  {
    write.csv(snow_data, file = file.path(folderoutpath,"meribel_snow_depth.csv"), row.names = FALSE)
  }
  
  return(snow_data)
  
}



# Courchevel

courchevel_snow_formatting <- function(folderpath = file.path(base,"TetrAlps/1_RAW_DATA/environment/enneigement/courchevel1850_hauteurdeneige_journaliere_2019_2023_13072023.xlsx"),
                                       folderoutpath = file.path(base,"TetrAlps/2_DATA/snow_depth"), save = FALSE)
{
  list_data <- list()
  
  for(year in c(2019:2022))
  {
    data <- tryCatch(
      {
        read.xlsx(xlsxFile = folderpath, sheet = as.character(paste0("cumul ",year-2000,"-",year+1-2000)), startRow = 2)
      }, 
      error = function(e) 
      {
        message("Error reading file: ", folderpath, "\n", e)
        return(NULL)
      })
    data <- data[,1:4]
    colnames(data) <- c( "Date", "cumul.H.neige.cm", "Neige.fraiche.cm", "moyenne")
    # Fill NA values in the Date column with the value above
    data$Date <- excel_numeric_to_date(as.numeric(data$Date))
    
    list_data[[year-2018]] <- data
  }
  
  snow_data <- do.call(rbind, list_data)
  
  if(save == TRUE)
  {
    write.csv(snow_data, file = file.path(folderoutpath,"courchevel_snow_depth.csv"), row.names = FALSE)
  }
  
  return(snow_data)
  
}
#********************************************************************






# Function to clean and homogenize ski lift names on the visitor numbers datasets provided by ski resorts with osm layers
#********************************************************************
cleaning_visitor_data <- function(data)
{
  list_dt <- list()
  ski_season_y <- unique(data$ski_season)
  data_name <- as.character(deparse(substitute(data)))
  
  
  for(year in seq_along(ski_season_y))
  {
    # Selection of the ski_season of interest 
    data_y <- data %>% filter(ski_season == ski_season_y[year]) 
    
    # Calculation of visitor numbers related to the ski season of interest for each ski lift
    data_nb <- colSums(data_y %>% dplyr::select(-Date, -ski_season))
    
    # Cleaning of ski lift names
    names(data_nb) <- stri_trans_general(names(data_nb), id = "Latin-ASCII") # delete accents
    
    
    if(str_contains(data_name, "meribel"))
    {
      find.list <- list("MO.TC.", "MO.TK.","MO.TSD.", "MO.", "KD.")
      resort = "Méribel"
    }
    if(str_contains(data_name, "valtho"))
    {
      find.list <- list("OR.TC.","OR.TS.","OR.TSD.", "VT.FU.","VT.TC.","VT.TK.","VT.TSD.","VT.","TPH.")
      resort = "Val Thorens"
    }
    if(str_contains(data_name, "courch"))
    {
      find.list <- list("TK.","TC.","TS.","TB.","CO.TK.","CO.TC.","CO.TS.","CO.TSD.","CO.TPH.")
      resort = "Courchevel"
    }
    if(str_contains(data_name, "menui"))
    {
      find.list <- list("TK.","TC.","TC.","TB.","TSF.","TSD.","TPH.")
      resort = "Les Ménuires"
    }
    
    if(!is.null(find.list))
    {
      find.list  <- paste(unlist(find.list), collapse = "|")
      names(data_nb) <- gsub(find.list ,"", names(data_nb)) #to delete the first letters of the name (MO. etc.)
    }
    
    
    data_dt <- data.frame(
      "ski_lift" = gsub("\\.", " ", names(data_nb)), # Replace dots with spaces
      "yearly_visitor_nb" = data_nb,
      "ski_season" = ski_season_y[year],
      "ski_resort" = resort
    )
    
    
    list_dt[[year]] <- data_dt
  }
  # Merging data frames
  merged_data <- bind_rows(list_dt)
  
  
  
  
  
  # Modify the names of ski_lift to allow the merge with the cable raster
  if(str_contains(data_name, "meribel"))
  {
    merged_data <- merged_data %>%
      mutate(
        ski_lift = case_when(
          str_detect(ski_lift, "Plattieres 3") ~ "Plattieres",  
          
          TRUE ~ ski_lift # Keep original value if no match
        ) 
      ) %>%
      group_by(ski_resort, ski_lift, ski_season) %>%
      summarize(
        yearly_visitor_nb = sum(yearly_visitor_nb, na.rm = TRUE), # Sum the visitor numbers
        .groups = "drop"  # Drop the grouping structure after summarizing
      )
  }
  if(str_contains(data_name, "valtho"))
  {
    merged_data <- merged_data %>%
      mutate(
        ski_lift = case_when(
          str_detect(ski_lift, "2 Lacs") ~ "Les 2 Lacs",
          str_detect(ski_lift, "Plateau") ~ "Plateau 1",  # Plateau = Plateau 1 = Plateau 2 idk why
          str_detect(ski_lift, "Caron  descente") ~ "Telecabine d'Orelle-Caron", # ici OR.TC.Caron..descente.
          str_detect(ski_lift, "Orelle Caron") ~ "Telecabine d'Orelle-Caron",
          str_detect(ski_lift, "Tyrolienne Bee") ~ "Bee Flying",
          str_detect(ski_lift, "Peclet") ~ "Funitel Peclet",
          str_detect(ski_lift, "Orelle") ~ "Telecabine d'Orelle",
          str_detect(ski_lift, "VT FU 3 Vallées") ~ "Funitel 3 Vallees",
          str_detect(ski_lift, "Plan Eau") ~ "Plan de l'Eau",
          str_detect(ski_lift, "3 Vallees Express") ~ "Telecabine d'Orelle", # cessassion of 3 Vallees Express in 2021, replaced by Telecabine d'Orelle
          
          # TS Glacier and TS Col suppressed in 2019 and 2020 respectively 
          
          TRUE ~ ski_lift # Keep original value if no match
        ) 
      ) %>%
      group_by(ski_resort, ski_lift, ski_season) %>%
      summarize(
        yearly_visitor_nb = sum(yearly_visitor_nb, na.rm = TRUE), # Sum the visitor numbers
        .groups = "drop"  # Drop the grouping structure after summarizing
      )
  }
  if(str_contains(data_name, "courch"))
  {
    merged_data$ski_lift <- str_to_title(merged_data$ski_lift)
    merged_data <- merged_data %>%
      mutate(
        ski_lift = case_when(
          str_detect(ski_lift, "Jardin alpin") ~ "Jardin Alpin",
          str_detect(ski_lift, "Tania") ~ "La Tania",
          str_detect(ski_lift, "Aiguille fruit") ~ "Aiguille du Fruit",
          str_detect(ski_lift, "Dou lanches") ~ "Dou des Lanches",
          str_detect(ski_lift, "Jardin enf.") ~ "Jardin d'Enfants",
          str_detect(ski_lift, "Loze") ~ "Loze Express",
          str_detect(ski_lift, "Pyramides") ~ "Pyramides 1",
          str_detect(ski_lift, "Source") ~ "Sources",
          str_detect(ski_lift, "Gros murger") ~ "TKD Gros Murger",
          str_detect(ski_lift, "Rocher ombre") ~ "Rocher de l'Ombre",
          str_detect(ski_lift, "Creux noirs") ~ "(projet) Creux Noirs",
          str_detect(ski_lift, "Bouc blanc") ~ "Bouc Blanc",
          str_detect(ski_lift, "Grandes combes") ~ "Grandes Combes",
          str_detect(ski_lift, "Petit moriond") ~ "Petit Moriond",
          str_detect(ski_lift, "Petite bosse") ~ "Petite Bosse",
          str_detect(ski_lift, "Roc merlet") ~ "Roc Merlet",
          str_detect(ski_lift, "Roc mugnier") ~ "Roc Mugnier",
          str_detect(ski_lift, "Stade tania") ~ "Stade Tania",
          str_detect(ski_lift, "Petite bosse") ~ "Petite Bosse", 
          str_detect(ski_lift, "Saulire") ~ "Saulire Express 1", 
          
          # Saulire = Saulire Express 1 + Saulire Express 2 ; cessassion of Envolee in 2018
          
          TRUE ~ ski_lift # Keep original value if no match
        ) 
      ) 
    
    new_data <- merged_data %>%
      filter(ski_lift == "Saulire Express 1") %>%
      mutate(ski_lift = case_when(
        ski_lift == "Saulire Express 1" ~ "Saulire Express 2",  # First new entry
        TRUE ~ ski_lift
      ))
    
    # Combine the new rows back into the original data
    merged_data <- bind_rows(merged_data, new_data)
    merged_data <- merged_data  %>%
      group_by(ski_resort, ski_lift, ski_season) %>%
      summarize(
        yearly_visitor_nb = sum(yearly_visitor_nb, na.rm = TRUE), # Sum the visitor numbers
        .groups = "drop"  # Drop the grouping structure after summarizing
      )
    
    
  }
  if(str_contains(data_name, "menui"))
  {
    merged_data <- merged_data %>%
      mutate(
        ski_lift = case_when(
          str_detect(ski_lift, "Biolley") ~ "Biolley 1", 
          str_detect(ski_lift, "Masse1") ~ "Masse",    
          str_detect(ski_lift, "Masse2") ~ "Masse",    
          str_detect(ski_lift, "Saint Martin") ~ "Saint Martin 1",  
          str_detect(ski_lift, "Bruyeres1") ~ "Bruyeres 1",  
          str_detect(ski_lift, "Bruyeres2") ~ "Bruyeres 2",  
          str_detect(ski_lift, "Tapis Pelvoux") ~ "Pelvoux",
          str_detect(ski_lift, "Tapis Reberty") ~ "Reberty",  
          str_detect(ski_lift, "Tapis Preyerand") ~ "Preyerand", 
          str_detect(ski_lift, "Tapis Plans") ~ "Plans", 
          str_detect(ski_lift, "Tapis Bruyeres") ~ "Bruyeres",
          
          # cessassion of Lac Noir in 2021, Montaulever in 2018
          
          TRUE ~ ski_lift # Keep original value if no match
        ) 
      ) %>%
      group_by(ski_resort, ski_lift, ski_season) %>%
      summarize(
        yearly_visitor_nb = sum(yearly_visitor_nb, na.rm = TRUE), # Sum the visitor numbers
        .groups = "drop"  # Drop the grouping structure after summarizing
      )
  }
  
  
  
  
  return(merged_data)
}
#********************************************************************




# Function that reshape the data to plot the correlation between the mean visitor number of 2 ski seasons
#********************************************************************
mean_visitor_plot <- function(list_of_data, season_to_compare1, season_to_compare2)
{
  l_wide_data <- list()
  
  for(l in seq_along(list_of_data))
  {
    data_name = names(list_of_data)[l]
    print(data_name)
    
    data <- list_of_data[[l]]
    
    visitor_mean_pv <- data %>% dplyr::select(-Total, -Total_std) %>%
      pivot_longer(cols = -ski_season, 
                   names_to = "ski_lift", 
                   values_to = "value") %>%
      mutate(ski_lift = gsub("\\.", " ", ski_lift)) # Clean up ski lift names if needed
    
    # Filter the data for the two specific seasons
    filtered_data <- visitor_mean_pv %>%
      filter(ski_season %in% c(season_to_compare1, season_to_compare2))
    
    # Reshape the data to wide format so we have season1 and season2 values in separate columns
    wide_data <- filtered_data %>%
      pivot_wider(names_from = ski_season, values_from = value) %>%
      rename("saison1" = season_to_compare1, "saison2" = season_to_compare2)  # Rename for clarity
    
    wide_data <- wide_data %>% mutate("resort" = data_name)
    l_wide_data[[l]] <- wide_data
    
  }# end for l  
  
  wide_data_all_resorts <- do.call(bind_rows,l_wide_data)
  wide_data_all_resorts <- as.data.frame(wide_data_all_resorts)
  
  cor_nb <- round(cor(wide_data_all_resorts$saison1, wide_data_all_resorts$saison2, , use = "complete.obs"),2)
  
  
  # Plot using geom_point() for the two seasons
  gg_mean_visitors <- 
    ggplot(wide_data_all_resorts , aes(x = saison1, y = saison2, label = ski_lift)) +
    geom_point(aes(color = resort)) + # Use blue for the points
    # geom_text(nudge_y = 200, size = 3) + # Add labels to the points, adjust 'nudge_y' for better positioning
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Diagonal reference line
    labs(
      title = "Average annual number of visitors per ski lift",
      x = paste0("Average annual number of visitors of ", season_to_compare1),
      y = paste0("Average annual number of visitors of ", season_to_compare2)
    ) +
    geom_text(x = max(wide_data_all_resorts$saison1, na.rm = TRUE)/10, 
              y = max(wide_data_all_resorts$saison2, na.rm = TRUE)/1.1, 
              label= paste0("correlation = ", cor_nb[[1]]), 
              hjust = 0, 
              size = 5)+
    theme(plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 13),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12))
  
  plot(gg_mean_visitors)
  
  return(wide_data_all_resorts)
}
#********************************************************************
