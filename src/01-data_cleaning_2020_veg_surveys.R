# libraries --------------------------------------------------------------------
library(here)     # for creating relative file-paths
library(readxl)   # for importing excel files
library(purrr)    # for iterating row-binding operations
library(dplyr)    # for manipulating data
library(janitor)  # for cleaning column names
library(ggplot2)  # for visualizing data 
library(stringr)  # for manipulating character strings
library(tidyr)

# Import custom functions ------------------------------------------------------

# docs: https://github.com/garlandxie/MW_GTA/blob/master/src/functions2.R

source(here("src", "functions2.R"))

# Sections 3,4,5 ---------------------------------------------------------------

mw_356 <- read_excel(
  here("data/original/veg_surveys/2020", 
       "MV24_3-5-6_Ground-Veg_Summer-Spring_2020.xlsx"),
  sheet = 1
)

# check packaging
str(mw_356)
head(mw_356, n = 5)
tail(mw_356, n = 5)
dim(mw_356)

# Individual excel files -------------------------------------------------------

ls_2020 <- list.files(here("data/original/veg_surveys/2020"))

# there's one file with inconsistent data across columns
# so: remove this from the row-binding and clean it
# then: add the clean data set with the other ones
weird_df <- "MV24_4-2_C_Ground-Veg_Summer_2020.xlsx"
s_345 <- "MV24_3-5-6_Ground-Veg_Summer-Spring_2020.xlsx"

nice_df <- ls_2020[which(ls_2020 != weird_df & ls_2020 != s_345)]

mw_2020 <- map_dfr(nice_df, bind_mw_2020)
c_2020 <- bind_mw_2020(weird_df)

# check packaging 
str(mw_2020)
head(mw_2020, n = 5)
tail(mw_2020, n = 5)
dim(mw_2020)

str(c_2020)
head(c_2020, n = 5)
tail(c_2020, n = 5)
dim(c_2020)

# clean: c_2020 ----------------------------------------------------------------

# ensure consistent columns with the other data sets
c_tidy1 <- c_2020 %>%
  filter(!(solitary %in% c("FO", "GR"))) 


c_tidy2 <- c_2020 %>%
  filter(solitary %in% c("FO", "GR")) %>%
  select(
    quadrat_no, 
    species, 
    common_name, 
    solitary = cf,
    cf = cover, 
    cover = comments,
    comments = cover,
    season, 
    year, 
    site,
    section
  ) %>%
  mutate(cover = as.numeric(cover))

# merge 
mw_tidy <- rbind(mw_2020, c_tidy1, c_tidy2)

# check packaging 
str(mw_tidy)
head(mw_tidy, n = 5)
tail(mw_tidy, n = 5)
dim(mw_tidy)

# clean: sections 3,5,6 --------------------------------------------------------

mw_356_tidy <- mw_356 %>%
  
  clean_names() %>% 
  
  mutate(
    site = str_sub(plot_id, 11, 11),
    section = str_sub(plot_id, 7, 9)
    ) %>%
  
  filter(visit == "summer") %>%
  
  select(
    quadrat_no, 
    species, 
    common_name, 
    solitary,
    cf,
    cover,
    comments,
    season = visit,
    comments,
    year = visit_year,
    site,
    section) %>%
  
  mutate(
    cover = case_when(
      cover == "<1" ~ "0.1", 
      TRUE ~ cover)
  ) %>%
  
  mutate(cover = as.numeric(cover))

# merge
mw_tidy2 <- rbind(mw_tidy, mw_356_tidy) %>%
  select(
   quadrat = quadrat_no, 
   spp = species, 
   common_name, 
   solitary, 
   cf, 
   cover, 
   comments,
   site,
   year, 
   season,
   section
  ) %>%
  mutate(cover = replace_na(cover, "0.1"))
  

# check packaging
str(mw_tidy2)
head(mw_tidy2, n = 5)
tail(mw_tidy2, n = 5)
dim(mw_tidy2)


# save to disk! ----------------------------------------------------------------

write.csv(
  x = mw_tidy2,
  file = here("data/final", "veg_surveys_2020.csv"),
  row.names = FALSE
)
