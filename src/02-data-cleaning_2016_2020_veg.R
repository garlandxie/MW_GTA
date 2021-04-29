# libraries --------------------------------------------------------------------
library(here)
library(dplyr)
library(readr)
library(visdat)
library(validate)

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

# validate ---------------------------------------------------------------------

rules_16_20 <- validator(
  five_quads = quadrat %in% c(1, 2, 3, 4, 5),
  summer     = season == "Summer",
  sol_TF     = solitary %in% c(TRUE, FALSE),
  cf_TF      = cf %in% c(TRUE, FALSE),
  pos_cover  = cover >= 0, 
  year_16_20 = year %in% c(2016, 2018, 2019, 2020),
  sites      = site %in% 
    c("A", "B", "C", "D", "E", "F",
      "G", "H", "I", "J", "K", "L", 
      "M", "N", "O", "P", "Q", "R", 
      "V", "W", "X", "Y"),
  sections   = section %in% 
    c("1.1", "1.2", "1.3", "1.4",
      "3.2", "3.3", 
      "4.1", "4.2", "4.3",  "4.4", 
      "5.1", "5.3", "5.4",
      "6.1", "6.2", "6.4",
      "7.1")
)

mw_tidy_out <- confront(mw_tidy, rules_16_20)
summary(mw_tidy_out)

# save to disk -----------------------------------------------------------------

write.csv(
  x = mw_tidy,
  file = here("data/final", "veg_surveys_2016-2020.csv"),
  row.names = FALSE
)

df <- mw_tidy %>% filter(section == "2.2")
unique(df$year)
unique(df$site)
