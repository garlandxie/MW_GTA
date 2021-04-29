# libraries --------------------------------------------------------------------
library(readxl)
library(here)
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(visdat)
library(validate)
library(stringr)

# import -----------------------------------------------------------------------

# list of excel files from 2016
files_2018_mv <- 
  list.files(
    here("data/original/veg_surveys/2018"),
    pattern = "2018"
  )

# use custom functions
source(here("src", "functions2.R"))

# bind -------------------------------------------------------------------------

mw_2018 <- 
  bind_rows(
    map(files_2018_mv, 
        bind_mw_2018)
  )

# check packaging --------------------------------------------------------------

head(mw_2018, n = 5)
tail(mw_2018, n = 5)
dim(mw_2018)

# check for missing values -----------------------------------------------------

vis_dat(mw_2018)
vis_miss(mw_2018)

# data cleaning ----------------------------------------------------------------

mw_2018_tidy <- mw_2018 %>%
  mutate(
    cover = case_when(
      cover == "<1" ~ "0.1",
      TRUE ~ cover)
  ) %>%
  mutate(cover = as.numeric(cover)) %>%
  mutate(site = str_sub(site, start = 3L, end = 3L)) %>%
  mutate(section = case_when(
    site == "A" ~ "4.2",
    site == "B" ~ "4.2", 
    site == "C" ~ "4.2", 
    site == "D" ~ "4.3", 
    site == "E" ~ "4.3",
    site == "F" ~ "4.3",
    site == "G" ~ "4.1",
    site == "H" ~ "4.1",
    site == "I" ~ "4.1",
    site == "J" ~ "4.4", 
    site == "K" ~ "4.4", 
    site == "L" ~ "4.4", 
    site == "M" ~ "7.1",
    site == "N" ~ "7.1",
    site == "O" ~ "7.1",
    site == "P" ~ "1.2",
    site == "Q" ~ "1.3",
    site == "R" ~ "1.4",
    site == "S" ~ "2.2",
    site == "T" ~ "2.3", 
    site == "U" ~ "2.4",
    TRUE ~ site)
  )

# validate ---------------------------------------------------------------------

rules_2018 <- validator(
  five_quads = quadrat %in% c(1:5), 
  solit_TF = solitary %in% c("TRUE", "FALSE"), 
  cf_TF = cf %in% c("TRUE", "FALSE"),
  no_neg_cover = cover >= 0,
  year_2018 = year == "2018", 
  summer_only = season == "Summer",
  sites = site %in% c("A", "B", "C", 
                      "D", "E", "F",
                      "G", "H", "I", 
                      "J", "K", "L", 
                      "M", "N", "O",
                      "P", "Q", "R", 
                      "S", "T", "U"),
  sections = section %in% 
    c("1.2", "1.3", "1.4", 
      "2.2", "2.3", "2.4", 
      "4.1", "4.2", "4.3", "4.4",
      "7.1")
)

mw_2018_out <- confront(mw_2018_tidy, rules_2018)
summary(mw_2018_out)

# save to disk! ----------------------------------------------------------------

write.csv(
  x = mw_2018_tidy, 
  file = here("data/final", "veg_surveys_2018.csv"),
  row.names = FALSE
)


