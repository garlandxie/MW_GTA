#' bind_mw_2016
#' 
#' Row-bind multiple data sets from the Meadoway (TRCA) 2016 vegetative
#' surveying monitoring database provided by Scott MacIvor
#'
#'#' @param file_name an excel file 
#' with a strict file-name format "
#' MV24_plot_Meadow_Ground-Vegetation_year.xlsx
#' 
#'#' @return a data-frame consisting of the following columns (with data types): 
#' column 1 - quadrat_no (integer)
#' column 2 - species (character)
#' column 3 - common_name (character) 
#' column 4 - solitary (character)
#' column 5 - cf (character)
#' column 6 - comments (character)
#' column 7 - site (character)
#' column 8 - year (numeric)
#' column 9 - season (character)
#' 
#' #' @examples
#' Vegetative summer surveys in Section 1.1 Plot A 
#' bind_mw_2016(file_name = MW24A_Meadow_Ground-Vegetation_2016.xlsx)
#' 
#' #' Vegetative summer surveys in Section 7.1 Plot N 
#' bind_mw_2016(file_name = MW24_Meadow_Ground-Vegetation_2016.xlsx)
#' 
#' 
#' 
#' 

bind_mw_2016 <- function(file_name) {
  
  # make sure it's from the 2016 files  
  if(grepl("2016", file_name)) {
    df <- readxl::read_excel(
      here::here("data/original/veg_surveys/2016", file_name)
    )
  } else {
    stop("Please ensure this is a 2016 excel file (containing a single sheet)")
  }
  
  # read data
  df <- readxl::read_excel(
    here::here("data/original/veg_surveys/2016", file_name)
    )
  
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

#' bind_mw_2020
#'
#' Row-bind multiple data sets from the Meadoway (TRCA) 2020 vegetative
#' surveying monitoring database provided by Sisley Irwin
#'
#' @param file_name an excel file 
#' with a strict file-name format "
#' MW24_section-subsection_plot_Ground-Veg_season_year.xlsx
#' 
#' @return a data-frame consisting of the following columns (with data types): 
#' column 1 - quadrat_no (integer)
#' column 2 - species (character)
#' column 3 - common_name (character) 
#' column 4 - solitary (character)
#' column 5 - cf (character)
#' column 6 - comments (character)
#' column 7 - season (character)
#' column 8 - year (numeric)
#' column 9 - site (character)
#' column 10 - section(character)
#'
#' @examples
#' Vegetative summer surveys in Section 1.1 Plot A 
#' bind_mw_2020(file_name = MW25_1-1_A_Ground-Veg_Summer_2020.xlsx)
#' 
#' #' Vegetative summer surveys in Section 7.1 Plot N 
#' bind_mw_2020(file_name = MW25_7-1_N_Ground-Veg_Summer_2020.xlsx)
#' 
#' 
#' 
#' 

bind_mw_2020 <- function(file_name) {
  
  # import the data!
  folder <- "data/original/veg_surveys/2020"
  import <- readxl::read_excel(here::here(folder,file_name)) 
  
  # make R-friendly column names
  import_tidy <- janitor::clean_names(import) 
  
  # add relevant columns based on column indexes
  f <- unlist(strsplit(file_name, split = "_"))
  year_num <- f[which(grepl("2020", f))]
  
  import_tidy$season  <- f[which(f == "Summer")] 
  import_tidy$year    <- substring(year_num, first = 1L, last = 4L) 
  import_tidy$site    <- f[which(f %in% LETTERS)]
  import_tidy$section <- f[2] # IM LAZY
  
  # ensure consistent data types for all columns
  # across all data-sets during row binding operations
  # note: some rows may have missing values
  import_tidy$quadrat_no  <- as.integer(import_tidy$quadrat_no)
  import_tidy$species     <- as.character(import_tidy$species)
  import_tidy$solitary    <- as.character(import_tidy$solitary)
  import_tidy$cf          <- as.character(import_tidy$cf)
  import_tidy$cover       <- as.numeric(import_tidy$cover)
  import_tidy$comments    <- as.character(import_tidy$comments)
  import_tidy$season      <- as.character(import_tidy$season)  
  import_tidy$year        <- as.numeric(import_tidy$year) 
  import_tidy$section     <- as.character(import_tidy$section)
  
  return(import_tidy) 
}


#' bind_mw_2018
#'
#' Row-bind multiple data sets from the Meadoway (TRCA) 2018 vegetative
#' surveying monitoring database provided by Scott MacIvor
#' 
#' This function only applies to the SUMMER field season
#'
#' @param file_name an excel file 
#' with a strict file-name format "
#' MW24_section-subsection_plot_Ground-Veg_season_year.xlsx
#' 
#'#' @return a data-frame consisting of the following columns (with data types): 
#' column 1 - quadrat_no (integer)
#' column 2 - species (character)
#' column 3 - common_name (character) 
#' column 4 - solitary (character)
#' column 5 - cf (character)
#' column 6 - comments (character)
#' column 7 - site (character)
#' column 8 - year (numeric)
#' column 9 - season (character)
#'
#' #' @examples
#' Vegetative summer surveys in Section 1.1 Plot A 
#' bind_mw_2016(file_name = MW24A_Meadow_Ground-Vegetation_2016.xlsx)
#' 
#' #' Vegetative summer surveys in Section 7.1 Plot N 
#' bind_mw_2016(file_name = MW24_Meadow_Ground-Vegetation_2016.xlsx)
#' 
#' 
#' 
#' 

bind_mw_2018 <- function(file_name) {
  
  file_path <- here::here("data/original/veg_surveys/2018", file_name)
  
  # make sure it's from the 2016 files  
  if(grepl("2018", file_path)) {
    df <- readxl::read_excel(file_path)
  } else {
    stop("Please ensure this is a 2018 or 2019 excel file (summer data)")
  }
  
  if(all(excel_sheets(file_path) %in% c("Spring", "Summer"))) {
    
    df <- readxl::read_excel(file_path, sheet = "Summer")
    
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

#' bind_mw_2019
#'
#' Row-bind multiple data sets from the Meadoway (TRCA) 2019 vegetative
#' surveying monitoring database provided by Scott MacIvor
#' 
#' This function only applies to the SUMMER field season
#'
#' @param file_name an excel file 
#' with a strict file-name format "
#' MW24_section-subsection_plot_Ground-Veg_season_year.xlsx
#' 
#'#' @return a data-frame consisting of the following columns (with data types): 
#' column 1 - quadrat_no (integer)
#' column 2 - species (character)
#' column 3 - common_name (character) 
#' column 4 - solitary (character)
#' column 5 - cf (character)
#' column 6 - comments (character)
#' column 7 - site (character)
#' column 8 - year (numeric)
#' column 9 - season (character)
#'
#' #' @examples
#' Vegetative summer surveys in Section 1.1 Plot A 
#' bind_mw_2019(file_name = MW24A_Meadow_Ground-Vegetation_2019.xlsx)
#' 
#' #' Vegetative summer surveys in Section 7.1 Plot N 
#' bind_mw_2019(file_name = MW24_Meadow_Ground-Vegetation_2019.xlsx)
#' 
#' 
#' 
#' 

bind_mw_2019 <- function(file_name) {

  file_path <- here::here("data/original/veg_surveys/2019", file_name)
  
  # make sure it's from the 2016 files  
  if(grepl("2019", file_path)) {
    df <- readxl::read_excel(file_path)
  } else {
    stop("Please ensure this is a 2019 excel file (summer data)")
  }
  
  if(all(excel_sheets(file_path) %in% c("Spring", "Summer"))) {
    
    df <- readxl::read_excel(file_path, sheet = "Summer")
    
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

