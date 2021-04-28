# libraries --------------------------------------------------------------------
library(here)
library(dplyr)
library(readr)
library(visdat)

# import -----------------------------------------------------------------------

mw_2016 <- read_csv(
  here("data/final", "veg_surveys_2016.csv")
)

mw_2018 <- read_csv(
  here("data/final", "veg_surveys_2018.csv")
)

mw_2019 <- read_csv(
  here("data/final", "veg_surveys_2019.csv")
)

mw_2020 <- read_csv(
  here("data/final", "veg_surveys_2020.csv")
)

# row bind ---------------------------------------------------------------------

mw_tidy <- rbind(mw_2016, mw_2018, mw_2019, mw_2020)

# check packaging --------------------------------------------------------------

str(mw_tidy)
head(mw_tidy, n = 5)
tail(mw_tidy, n = 5)

# check for missing values -----------------------------------------------------

vis_dat(mw_tidy)
vis_miss(mw_tidy)

# 
