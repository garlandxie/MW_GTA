# libraries --------------------------------------------------------------------
library(here)     # for creating relative file-paths
library(readxl)   # for importing excel files
library(purrr)    # for iterating row-binding operations
library(dplyr)    # for manipulating data
library(janitor)  # for cleaning column names
library(ggplot2)  # for visualizing data 
library(stringr)  # for manipulating character strings

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
mw_tidy2 <- rbind(mw_tidy, mw_356_tidy)

# check packaging
str(mw_tidy2)
head(mw_tidy2, n = 5)
tail(mw_tidy2, n = 5)
dim(mw_tidy2)

# plots: species richness ------------------------------------------------------

# resident species richness
res_sr <- mw_tidy2 %>%
  filter(species != "Cynanchum rossicum") %>%
  group_by(year, season, section, site, quadrat_no) %>%
  summarize(sr = length(unique(species)))

(p_res_sr_s1 <- res_sr %>%
  filter(section %in% c("1-1", "1-2", "1-3")) %>%
  ggplot(aes(x = quadrat_no, y = sr)) + 
  geom_point() +
  geom_hline(yintercept = 15, linetype = "dashed") + 
  scale_y_continuous(
      limits = c(0, 30), 
      breaks = c(0, 5, 10, 15, 20, 25, 30)
  ) + 
  facet_wrap(~site, nrow = 1, ncol = 4) +
  labs(
    x = "Quadrat Number", 
    y = "Resident species richness", 
    title = "Section 1 - Summer 2020 (DSV excluded)") +  
  theme_bw()
)

(p_res_sr_s3 <- res_sr %>%
    filter(section %in% c("3.2", "3.3")) %>%
    ggplot(aes(x = quadrat_no, y = sr)) + 
    geom_point() +
    geom_hline(yintercept = 15, linetype = "dashed") + 
    scale_y_continuous(
      limits = c(0, 30), 
      breaks = c(0, 5, 10, 15, 20, 25, 30)
      ) + 
    facet_wrap(~site, nrow = 1, ncol = 4) +
    labs(
      x = "Quadrat Number", 
      y = "Resident species richness", 
      title = "Section 3 - Summer 2020 (DSV excluded)") +  
    theme_bw()
)


(p_res_sr_s4 <- res_sr %>%
    filter(section %in% c("4-1", "4-2", "4-3", "4-4")) %>%
    ggplot(aes(x = quadrat_no, y = sr)) + 
    geom_point() +
    geom_hline(yintercept = 15, linetype = "dashed") + 
    scale_y_continuous(
      limits = c(0, 30), 
      breaks = c(0, 5, 10, 15, 20, 25, 30)
    ) + 
    facet_wrap(~site) +
    labs(
      x = "Quadrat Number", 
      y = "Resident species richness", 
      title = "Section 4 - Summer 2020 (DSV excluded)") +  
    theme_bw()
)

(p_res_sr_s5 <- res_sr %>%
    filter(section %in% c("5.1", "5.3", "5.4")) %>%
    ggplot(aes(x = quadrat_no, y = sr)) + 
    geom_point() +
    geom_hline(yintercept = 15, linetype = "dashed") + 
    scale_y_continuous(
      limits = c(0, 30), 
      breaks = c(0, 5, 10, 15, 20, 25, 30)
    ) + 
    facet_wrap(~site) +
    labs(
      x = "Quadrat Number", 
      y = "Resident species richness", 
      title = "Section 5 - Summer 2020 (DSV excluded)") +  
    theme_bw()
)

(p_res_sr_s6 <- res_sr %>%
    filter(section %in% c("6.1", "6.2", "6.3")) %>%
    ggplot(aes(x = quadrat_no, y = sr)) + 
    geom_point() +
    geom_hline(yintercept = 15, linetype = "dashed") + 
    scale_y_continuous(
      limits = c(0, 30), 
      breaks = c(0, 5, 10, 15, 20, 25, 30)
    ) + 
    facet_wrap(~site) +
    labs(
      x = "Quadrat Number", 
      y = "Resident species richness", 
      title = "Section 6 - Summer 2020 (DSV excluded)") +  
    theme_bw()
)

(p_res_sr_s7 <- res_sr %>%
    filter(section %in% c("7-1")) %>%
    ggplot(aes(x = quadrat_no, y = sr)) + 
    geom_point() +
    geom_hline(yintercept = 15, linetype = "dashed") + 
    scale_y_continuous(
      limits = c(0, 30), 
      breaks = c(0, 5, 10, 15, 20, 25, 30)
    ) + 
    facet_wrap(~site) +
    labs(
      x = "Quadrat Number", 
      y = "Resident species richness", 
      title = "Section 7 - Summer 2020 (DSV excluded)") +  
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
  group_by(year, season, section, site, quadrat_no) %>%
  summarize(total_abund = sum(cover, na.rm = TRUE))

(plot_res_abund <- res_abund %>%
    ggplot(aes(x = quadrat_no, y = total_abund)) + 
    geom_point() +
    geom_line() + 
    geom_hline(yintercept = 50, linetype = "dashed") + 
    facet_wrap(section~site, nrow = 3, ncol = 6) +
    labs(
      x = "Quadrat Number", 
      y = "Resident community abundance (% cover)", 
      title = "Summer 2020 (DSV excluded)") +  
    theme_bw()
)

# plots: DSV -------------------------------------------------------------------

# DSV abundance
DSV_abund <- mw_tidy %>%
  filter(species == "Cynanchum rossicum") %>%
  
  mutate(cover = case_when(
    cover == "<1" ~ "0.1",
    TRUE ~ cover
  )
  ) %>%
  
  mutate(cover = as.numeric(cover)) %>%
  group_by(year, season, section, site, quadrat_no) %>%
  summarize(total_abund = sum(cover, na.rm = TRUE))

(plot_DSV_abund <- DSV_abund %>%
    ggplot(aes(x = quadrat_no, y = total_abund)) + 
    geom_point() +
    geom_line() + 
    geom_hline(yintercept = 10, linetype = "dashed") + 
    facet_wrap(section~site, nrow = 3, ncol = 6) +
    labs(
      x = "Quadrat Number", 
      y = "DSV abundance (% cover)", 
      title = "Summer 2020 (DSV-only)") +  
    theme_bw()
)

# save to disk! ----------------------------------------------------------------

ggsave(
  plot = p_res_sr_s1, 
  filename = here("output/figures", "p_res_sr_s1.png"), 
  device = "png",
  height = 10, 
  width = 10, 
  units = "cm"
)

ggsave(
  plot = p_res_sr_s3, 
  filename = here("output/figures", "p_res_sr_s3.png"), 
  device = "png",
  height = 10, 
  width = 10, 
  units = "cm"
)

ggsave(
  plot = p_res_sr_s4, 
  filename = here("output/figures", "p_res_sr_s4.png"), 
  device = "png",
  height = 10, 
  width = 10, 
  units = "cm"
)

ggsave(
  plot = p_res_sr_s5, 
  filename = here("output/figures", "p_res_sr_s5.png"), 
  device = "png",
  height = 10, 
  width = 10, 
  units = "cm"
)

ggsave(
  plot = p_res_sr_s6, 
  filename = here("output/figures", "p_res_sr_s6.png"), 
  device = "png",
  height = 10, 
  width = 10, 
  units = "cm"
)

ggsave(
  plot = p_res_sr_s7, 
  filename = here("output/figures", "p_res_sr_s7.png"), 
  device = "png",
  height = 10, 
  width = 10, 
  units = "cm"
)

# save to disk! ----------------------------------------------------------------

ggsave(
  plot = plot_res_abund, 
  filename = here("output/figures", "resident_abundance.png"), 
  device = "png"
)

write.csv(
  x = mw_tidy2,
  file = here("data/final", "veg_surveys_2020.csv"),
  row.names = FALSE
)