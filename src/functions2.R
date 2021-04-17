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
#'
#' @examples
#' Vegetative summer surveys in Section 1.1 Plot A 
#' bind_mw_2020(file_name = MW25_1-1_A_Ground-Veg_Summer_2020.xlsx)
#' 
#' #' Vegetative summer surveys in Section 7.1 Plot N 
#' bind_mw_2020(file_name = MW25_7-1_N_Ground-Veg_Summer_2020.xlsx)

bind_mw_2020 <- function(file_name) {
  
  # import the data!
  folder <- "data/original/veg_surveys/2020"
  import <- readxl::read_excel(here::here(folder,file_name)) 
  
  # make R-friendly column names
  import_tidy <- janitor::clean_names(import) 
  
  # add relevant columns based on column indexes
  f <- unlist(strsplit(file_name, split = "_"))
  year_num <- f[which(grepl("2020", f))]
  
  import_tidy$season <- f[which(f == "Summer")] 
  import_tidy$year <- substring(year_num, first = 1L, last = 4L) 
  import_tidy$site <- f[which(f %in% LETTERS)]
  
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
  
  return(import_tidy) 
}

