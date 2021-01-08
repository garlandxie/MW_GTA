import_2016_data <- function(file_name) {

  # make sure it's from the 2016 files  
  if(grepl("2016", file_name)) {
    df <- readxl::read_excel(here::here("data/original", file_name))
  } else {
    stop("Please ensure this is a 2016 excel file (containing a single sheet)")
  }
  
  # read data
  df <- readxl::read_excel(here::here("data/original", file_name))
  
  
  plant_types <- c("Plant Type", "PlantType", "plant_type", "PlantType")
  
  if(any(colnames(df) %in% plant_types)) {
    df <- df[, !(colnames(df) %in% plant_types)] 
  } else {
    df <- df
  }
  
  # rename columns
  colnames(df) <- c(
    "quadrat", 
    "spp", 
    "common_name", 
    "solitary",
    "cf",
    "cover", 
    "comments"
  )
  
  # coerce a character string
  df$cf <- as.character(df$cf)
  df$solitary <- as.character(df$solitary)
  df$cover <- as.character(df$cover)
  
  # add additional info from file names
  file_strings <- strsplit(file_name, split = "_")
  df$site <- file_strings[[1]][2]
  df$year <- substring(file_strings[[1]][5], first = 1, last = 4)
  
  # add in Summer column
  df$season <- "Summer"
  
  return(df)
  
}

import_data_spring <- function(file_name) {
  
  
  file_path <- here::here("data/original", file_name)
  
  # make sure it's from the 2016 files  
  
  if(grepl("2018", file_path) | grepl("2019", file_path)) {
    df <- readxl::read_excel(file_path)
  } else {
    stop("Please ensure this is a 2018 or 2019 excel file (spring data)")
  }
  
  # Some files do not have a "Spring" or "Summer" sheet name
  
  if(all(excel_sheets(file_path) %in% c("Spring", "Summer"))) {
    
    df <- readxl::read_excel(file_path, sheet = "Spring")
    
  } else {
    
    df <- readxl::read_excel(file_path)
  }

  plant_types <- c("Plant Type", "PlantType", "plant_type")
  
  if(any(colnames(df) %in% plant_types)) {
    df <- df[, !(names(df) %in% plant_types)] 
  } else {
    df <- df
  }
  
  # rename columns
  colnames(df) <- c(
    "quadrat", 
    "spp", 
    "common_name", 
    "solitary",
    "cf",
    "cover", 
    "comments"
  )
  
  # coerce a character string
  df$cf <- as.character(df$cf)
  df$solitary <- as.character(df$solitary)
  df$cover <- as.character(df$cover)
  
  # add additional info from file names
  file_strings <- strsplit(file_name, split = "_")
  df$site <- file_strings[[1]][2]
  df$year <- substring(file_strings[[1]][5], first = 1, last = 4)
  
  # add in Summer column
  df$season <- "Spring"
  
  return(df)
  
}


import_data_summer <- function(file_name) {
  
  
  file_path <- here::here("data/original", file_name)
  
  # make sure it's from the 2016 files  
  if(grepl("2018", file_path) | grepl("2019", file_path)) {
    df <- readxl::read_excel(file_path)
  } else {
    stop("Please ensure this is a 2018 or 2019 excel file (summer data)")
  }
  
  if(all(excel_sheets(file_path) %in% c("Spring", "Summer"))) {
    
    df <- readxl::read_excel(file_path, sheet = "Spring")
    
  } else {
    
    df <- readxl::read_excel(file_path)
  }
  
  # some files do not have a "Plant Type" column
  # I don't need this specific column for any analyses
  # so just remove it for now
  
  plant_types <- c("Plant Type", "PlantType", "plant_type", "PlantType")
  
  if(any(colnames(df) %in% plant_types)) {
    df <- df[, !(colnames(df) %in% plant_types)] 
  } else {
    df <- df
  }
  
  # rename columns
  colnames(df) <- c(
    "quadrat", 
    "spp", 
    "common_name", 
    "solitary",
    "cf",
    "cover", 
    "comments"
  )
  
  # coerce a character string
  df$cf <- as.character(df$cf)
  df$solitary <- as.character(df$solitary)
  df$cover <- as.character(df$cover)
  
  # add additional info from file names
  file_strings <- strsplit(file_name, split = "_")
  df$site <- file_strings[[1]][2]
  df$year <- substring(file_strings[[1]][5], first = 1, last = 4)
  
  # add in Summer column
  df$season <- "Summer"
  
  return(df)
  
}


get_spp_code <- function(spp) {
  
  split_spp <- strsplit(spp, split = " ")
  
  genus <- split_spp[[1]][1]
  species <- split_spp[[1]][2]
  
  genus_code <- substring(genus, first = 1L, last = 2L)
  species_code <- substring(species, first = 1L, last = 2L)
  
  code <- toupper(paste0(genus_code, species_code))
  
  return(code)
}


plot_cover_site_year <- function(df, yr, s, sn) {
  
  require(dplyr)
  require(ggplot2)
  require(gghighlight)
  
  df <- filter(df, year == yr, site == s, season == sn)
  
  gg <- df %>%
    ggplot(aes(x = quadrat, y = cover)) +
    geom_point() +
    facet_wrap(~spp_code) + 
    labs(
      title = paste(yr, s, sn, sep = " "), 
      x = NULL, 
      y = "Absolute Cover (%)") +
    gghighlight(spp_code == "CYRO", use_group_by = FALSE) +
    theme_bw()
  
  return(gg)
}
