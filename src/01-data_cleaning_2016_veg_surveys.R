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
files_2016_mv <- 
  list.files(
    here("data/original/veg_surveys/2016"),
    pattern = "2016"
  )

# use custom functions
source(here("src", "functions2.R"))

# bind -------------------------------------------------------------------------

# row-bind them all!!!
mw_2016 <- 
  bind_rows(
    map(files_2016_mv, 
        bind_mw_2016)
  )

# check packaging --------------------------------------------------------------

head(mw_2016, n = 5)
tail(mw_2016, n = 5)
dim(mw_2016)

# check for missing values -----------------------------------------------------

vis_dat(mw_2016)
vis_miss(mw_2016)

# data cleaning ----------------------------------------------------------------

mw_2016_tidy <- mw_2016 %>%
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
    TRUE ~ site)
    )

# validate ---------------------------------------------------------------------

rules_2016 <- validator(
  five_quads = quadrat %in% c(1:5), 
  solit_TF = solitary %in% c("TRUE", "FALSE"), 
  cf_TF = cf %in% c("TRUE", "FALSE"),
  no_neg_cover = cover >= 0,
  year_2016 = year == "2016", 
  summer_only = season == "Summer",
  sites = site %in% 
    c("A", "B", "C", 
      "D", "E", "F",
      "G", "H", "I", 
      "J", "K", "L", 
      "M", "N", "O"),
  sections = section %in%
    c("4.1", "4.2", "4.3", "4.4", "7.1")
)

mw_2016_out <- confront(mw_2016_tidy, rules_2016)
summary(mw_2016_out)

# save to disk! ----------------------------------------------------------------

write.csv(
  x = mw_2016_tidy, 
  file = here("data/final", "veg_surveys_2016.csv"),
  row.names = FALSE
)

