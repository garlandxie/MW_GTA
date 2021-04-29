# libraries --------------------------------------------------------------------
library(here)
library(ggplot2)
library(readr)
library(dplyr)
library(gghighlight)

# import -----------------------------------------------------------------------

mw <- read_csv(
  here("data/final", "veg_surveys_2016-2020.csv")
)

# check packaging --------------------------------------------------------------

dim(mw)
head(mw, n = 5)
tail(mw, n = 5)

# data cleaning ----------------------------------------------------------------

mw_summ <- mw %>%
  mutate(spp = case_when(
    spp == "taraxacum officinale" ~ "Taraxacum officinale",
    TRUE ~ spp)
    ) %>%
  filter(!(spp %in% c("Bare soil", "mosses", "Thatch"))) %>%
  mutate(year = as.character(year)) %>%
  group_by(year, season, section, site, quadrat) %>%
  summarize(
    total_cover = sum(cover, na.rm = TRUE),
    sr          = length(unique(spp))
    ) %>%
  arrange(year, section, site, quadrat)
  
# plot: cover ------------------------------------------------------------------

cover_max <- mw_summ %>%
  group_by(year, season, section, site) %>%
  summarize( 
    mean_cov = mean(total_cover, na.rm = TRUE) %>% round(digits = 0),
    sd_cov   = sd(total_cover, na.rm = TRUE) %>% round(digits = 0)
    )

(plot_cover_max <- cover_max %>%
  ggplot(aes(x = year, y = mean_cov, group = site, col = site)) + 
  geom_point() + 
  geom_line() + 
  geom_pointrange(aes(ymin = mean_cov - sd_cov, ymax = mean_cov + sd_cov)) + 
  geom_hline(yintercept = 80, linetype = "dashed") + 
  labs(
    x = "Year", 
    y = "Mean Total Cover (%)", 
    title = "Summer",
    caption = "Error bars: standard deviation (1 unit)") + 
  scale_x_discrete(breaks = c("2016", "2018", "2019", "2020")) +
  gghighlight(
    section %in% c("4.1", "4.2", "4.3", "4.4"),
    calculate_per_facet = TRUE) + 
  facet_wrap(~section) + 
  theme_bw()
)

# plot: species richness -------------------------------------------------------

sr_max <- mw_summ %>%
  group_by(year, season, section, site) %>%
  summarize( 
    mean_sr = mean(sr, na.rm = TRUE) %>% round(digits = 0),
    sd_sr   = sd(sr, na.rm = TRUE) %>% round(digits = 0)
  )

(plot_sr_max <- sr_max %>%
    ggplot(aes(x = year, y = mean_sr, group = site, col = site)) + 
    geom_point() + 
    geom_line() + 
    geom_pointrange(aes(ymin = mean_sr - sd_sr, ymax = mean_sr + sd_sr)) + 
    geom_hline(yintercept = 15, linetype = "dashed") + 
    labs(
      x = "Year", 
      y = "Mean Speces Richness", 
      title = "Summer",
      caption = "Error bars: standard deviation (1 unit)") + 
    gghighlight(
      section %in% c("4.1", "4.2", "4.3", "4.4"),
      calculate_per_facet = TRUE) + 
    facet_wrap(~section) + 
    theme_bw()
)

# save the plots! --------------------------------------------------------------

ggsave(
  plot = plot_sr_max, 
  filename = here("output/figures", "plot_sr_max_summer.png"),
  device = "png",
  height = 8, 
  width = 8, 
  units = "in"
)

ggsave(
  plot = plot_cover_max, 
  filename = here("output/figures", "plot_cover_max_summer.png"),
  device = "png",
  height = 8, 
  width = 8, 
  units = "in"
)
