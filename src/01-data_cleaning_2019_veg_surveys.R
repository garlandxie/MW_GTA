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
files_2019_mv <- 
  list.files(
    here("data/original/veg_surveys/2019"),
    pattern = "2019"
  )

# import custom functions
# we can use bind_mw_2018?
source(here("src", "functions2.R"))

# bind -------------------------------------------------------------------------

# row bind them all!!!
mw_2019 <- 
  bind_rows(
    map(files_2019_mv, 
        bind_mw_2019)
  )

# check packaging --------------------------------------------------------------

head(mw_2019, n = 5)
tail(mw_2019, n = 5)
dim(mw_2019)

# check missing values ---------------------------------------------------------

vis_dat(mw_2019)
vis_miss(mw_2019)

# cleaning data ----------------------------------------------------------------

mw_2019_tidy <- mw_2019 %>%
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
    site == "R" ~ "1.4",
    site == "P" ~ "1.2", 
    site == "X" ~ "1.1",
    site == "W" ~ "1.4", 
    site == "V" ~ "1.3", 
    TRUE ~ site)
  )

# validate ---------------------------------------------------------------------

rules_2019 <- validator(
  five_quads = quadrat %in% c(1:5), 
  solit_TF = solitary %in% c("TRUE", "FALSE"), 
  cf_TF = cf %in% c("TRUE", "FALSE"),
  no_neg_cover = cover >= 0,
  year_2019 = year == "2019", 
  summer_only = season == "Summer",
  sites = site %in% c("A", "B", "C", 
                      "D", "E", "F",
                      "G", "H", "I", 
                      "J", "K", "L", 
                      "M", "N", "O",
                      "P", "R", "V",
                      "W", "X"),
  sections = section %in% c("1.1", "1.2", "1.3", "1.4", 
                            "4.1", "4.2", "4.3",  "4.4", 
                            "7.1")
)

mw_2019_out <- confront(mw_2019_tidy, rules_2019)
summary(mw_2019_out)

# save to disk -----------------------------------------------------------------

write.csv(
  x = mw_2019_tidy, 
  file = here("data/final", "veg_surveys_2019.csv"),
  row.names = FALSE
)
