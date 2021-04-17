# libraries --------------------------------------------------------------------
library(here)     # for creating relative file-paths
library(readxl)   # for importing excel files
library(purrr)    # for iterating row-binding operations
library(dplyr)    # for manipulating data
library(janitor)  # for cleaning column names
library(validate) # for validating data sets for any errors
library(ggplot2)  # for visualizing data 

# compile data-sets ------------------------------------------------------------

source(here("src", "functions2.R"))
ls_2020 <- list.files(here("data/original/veg_surveys/2020"))

# there's one file with inconsistent data across columns
# so: remove this from the row-binding and clean it
# then: add the clean data set with the other ones
weird_df <- "MV24_4-2_C_Ground-Veg_Summer_2020.xlsx"
nice_df <- ls_2020[which(ls_2020 != odd_2020)]

mw_2020 <- map_dfr(nice_df, bind_mw_2020)
c_2020 <- bind_mw_2020(weird_df)

# validate ---------------------------------------------------------------------

# double-check  
rules_2020 <- validator(
    quadrat_no %in% c(1:5),
    solitary %in% c("TRUE", "FALSE"), 
    cf %in% c("TRUE", "FALSE"),
    cover %in% c(1:100), 
    season == "Summer", 
    year == "2020", 
    site %in% LETTERS
  )

summary(confront(mw_2020, rules_2020))
summary(confront(c_2020, rules_2020))

# clean ------------------------------------------------------------------------

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
    site
  )

# merge 
mw_tidy <- rbind(mw_2020, c_tidy1, c_tidy2)

# validate
summary(confront(mw_2020, rules_2020))

# check packaging --------------------------------------------------------------
str(mw_tidy)

# plots: species richness ------------------------------------------------------

# resident species richness
res_sr <- mw_tidy %>%
  filter(species != "Cynanchum rossicum") %>%
  group_by(year, season, site, quadrat_no) %>%
  summarize(sr = length(unique(species)))

(plot_res_sr <- res_sr %>%
  ggplot(aes(x = quadrat_no, y = sr)) + 
  geom_point() +
  geom_hline(yintercept = 15, linetype = "dashed") + 
  facet_wrap(~site, nrow = 3, ncol = 6) +
  labs(
    x = "Quadrat Number", 
    y = "Species Richness", 
    title = "Summer 2020 (DSV excluded)") +  
  theme_bw()
)

# plots: community abundance ---------------------------------------------------

# resident community abundance
res_abund <- mw_tidy %>%
  filter(species != "Cynanchum rossicum") %>%
  
  mutate(cover = case_when(
    cover == "<1" ~ "0.1",
    TRUE ~ cover
    )
  ) %>%
  
  mutate(cover = as.numeric(cover)) %>%
  group_by(year, season, site, quadrat_no) %>%
  summarize(total_abund = sum(cover, na.rm = TRUE))

(plot_res_abund <- res_abund %>%
    ggplot(aes(x = quadrat_no, y = total_abund)) + 
    geom_point() +
    geom_hline(yintercept = 50, linetype = "dashed") + 
    facet_wrap(~site, nrow = 3, ncol = 6) +
    labs(
      x = "Quadrat Number", 
      y = "Total community abundance", 
      title = "Summer 2020 (DSV excluded)") +  
    theme_bw()
)

