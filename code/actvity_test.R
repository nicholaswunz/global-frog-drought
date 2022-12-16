# Load library
pacman::p_load(pander, Matrix, tidyverse, colorspace, RColorBrewer, cowplot, ggnewscale, 
               brms, rstan, performance, ape, phytools, ggtree, NicheMapR,
               ncdf4, raster, sf, rgdal, tmap, tmaptools, rasterSp)

# Functions
mytheme <- function() {
  theme_bw() +
    theme(panel.border          = element_rect(fill = NA, colour = "black"), # set border around plot.
          panel.grid.major      = element_blank(), # remove major grid lines
          panel.grid.minor      = element_blank(), # remove minor grid lines
          axis.line             = element_blank(), # remove axis lines
          axis.ticks            = element_line(colour = "black"),
          axis.text             = element_text(size = 10, colour = "black"), # axis text size
          axis.title            = element_text(size = 10), # axis title size
          axis.title.y          = element_text(vjust = 3), # increase distance from the y-axis
          axis.title.x          = element_text(vjust = -1), # increase distance from the x-axis
          panel.background      = element_rect(fill = NA),
          plot.background       = element_rect(fill = NA, color = NA), # remove background colour
          plot.margin           = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm"),
          legend.background     = element_rect(fill = NA, color = NA), # get rid of legend bg
          legend.box.background = element_rect(fill = NA, color = NA), # get rid of legend panel bg
          strip.text.x          = element_text(size = 10, color = "black", face = "bold"), # for facet plots
          strip.background      = element_rect(fill = NA, color = NA)
    )
} # set up plot theme


# Set directory
setwd('/Users/nicholaswu/Library/CloudStorage/OneDrive-WesternSydneyUniversity/Drought project') 

# Load and clean raw data
raw_dat <- read.csv("../data/raw_data.csv") %>%
  dplyr::select(study_ID:unit) %>%
  mutate(ecotype  = factor(ecotype),
         family   = factor(family),
         origin   = factor(origin),
         strategy = factor(case_when(strategy == "" ~ "none", TRUE ~ as.character(strategy))),
         strategy = fct_relevel(strategy, "none", "water-proof", "cocoon", "hollow"),
         trait    = factor(trait),
         response = factor(response),
         lnMass   = log(mean_mass_g),
         lnFlow   = log(airflow_cm_s + 1),
         lnMean   = log(unit_corrected_mean),
         es_v     = unit_corrected_sd^2 / sample_size, # sampling variance (v)
         es_sei   = sqrt(es_v), # standard error (SE)
         es_inv   = 1 / es_sei, # precision (inverse of SE) 
         es_kPa   = ifelse(trait == "water loss", 0.611 * exp(2500000 / 461.5 *(1 / 273 - 1 / (trt_temp + 273.15))), NA), # saturation vapor pressure (kPa) at a given temperature
         ea_kPa   = ifelse(trait == "water loss", RH_perc * es_kPa / 100, NA), # actual vapor pressure (kPa)
         VPD_kPa  = es_kPa - ea_kPa,
         lnVPD    = log(VPD_kPa))

ewl_dat <- raw_dat %>%
  dplyr::filter(response == "evaporative water loss") %>%
  dplyr::mutate(mg_h_mean = unit_corrected_mean * dors_SA_cm2,
                mg_h_sd   = unit_corrected_sd * dors_SA_cm2,    
                lnMean    = log(mg_h_mean),
                es_v      = mg_h_sd^2 / sample_size, # sampling variance (v)
                es_sei    = sqrt(es_v), # standard error (SE)
                es_inv    = 1 / es_sei, # precision (inverse of SE) )
                es_w      = 1 / es_v, # weight (inverse of variance) 
                h_70      = (mean_mass_g - (mean_mass_g * 0.7)) / (mg_h_mean * 0.001)) 

resist_dat <- raw_dat %>%
  filter(unit == "s cm")

wu_dat <- raw_dat %>%
  filter(response == "water uptake") %>%
  dplyr::mutate(mg_h_mean = unit_corrected_mean * vent_SA_cm2,
                mg_h_sd   = unit_corrected_sd * vent_SA_cm2,
                lnMean    = log(mg_h_mean),
                es_v      = mg_h_sd^2 / sample_size, # sampling variance (v)
                es_sei    = sqrt(es_v), # standard error (SE)
                es_inv    = 1 / es_sei) # precision (inverse of SE)

## SIMULATE CLIMATE CONDITIONS ##

source("/Users/nicholaswu/Desktop/global-frog-drought/code/micro_global_drought.R")

# Simulate rainfall with rhfact = 0.5 and rainfact = 0.54
longlat    <- c(153.09249, -27.6235) # Karawatha, QLD.
micro_curr_wet <- micro_global_drought(loc = longlat, timeinterval = 365, nyears = 1,
                                       runmoist = T, runshade = T,
                                       rhfact = 1, rainfact = 1,
                                       warm = 0)

micro_curr_dry <- micro_global_drought(loc = longlat, timeinterval = 365, nyears = 1,
                                       runmoist = T, runshade = T,
                                       rhfact = 0.5, rainfact = 0.54,
                                       warm = 0)

micro_warm_wet <- micro_global_drought(loc = longlat, timeinterval = 365, nyears = 1,
                                       runmoist = T, runshade = T,
                                       rhfact = 1, rainfact = 1,
                                       warm = 4)

micro_warm_dry <- micro_global_drought(loc = longlat, timeinterval = 365, nyears = 1,
                                       runmoist = T, runshade = T,
                                       rhfact = 0.5, rainfact = 0.54,
                                       warm = 4)

## SIMULATE ACTIVITY ## 

# Construct frog model
# compute the heat exchange by convection (extract mass transfer coefficient, Prandtl number and Schmidt number)
CONV_out <- NicheMapR::CONV_ENDO(TS     = 19, # skin temperature (°C)
                                 TENV   = 20, # fluid temperature (°C)
                                 SHAPE  = 4, # 4 is ellipsoid
                                 SURFAR = mean(resist_dat$dors_SA_cm2, na.rm = TRUE) / 10000,  # surface area for convection, m2
                                 FLTYPE = 0, # fluid type: 0 = air
                                 FURTST = 0, # test of presence of fur (length x diameter x density x depth) (-)
                                 D      = mean(resist_dat$D, na.rm = TRUE), 
                                 TFA    = 20, # initial fur/air interface temperature
                                 VEL    = mean(resist_dat$airflow_cm_s, na.rm = TRUE) / 100, # wind speed (m/s)
                                 ZFUR   = 0, # fur depth, mean (m)
                                 BP     = 101325, # barometric pressure at sea level
                                 ELEV   = 0) # elevation (m)

# basic parameters for 30 g frog
Ww_g         <- mean(raw_dat$mean_mass_g, na.rm = TRUE) # wet weight of animal (g)
r_s_low      <- resist_dat %>% dplyr::filter(strategy == "none") %>% dplyr::select(unit_corrected_mean) # skin resistance
pct_wet_high <- 1 / (CONV_out[5] * mean(r_s_low$unit_corrected_mean) + (CONV_out[11] / CONV_out[13]) ^ 0.6666666) * 100  # % of surface area acting as a free-water exchanger (Pirtle et al 2017)

# parameters for a water-proof frog
r_s_high    <- resist_dat %>% dplyr::filter(strategy == "water-proof") %>% dplyr::select(unit_corrected_mean) # skin resistance
pct_wet_low <- 1 / (CONV_out[5] * max(r_s_high$unit_corrected_mean) + (CONV_out[11] / CONV_out[13]) ^ 0.6666666) * 100 # % of surface area acting as a free-water exchanger (Pirtle et al 2017)

# Thermal traits based on Rhinella marina
Tmin   <- 13.7 # minimum Tb at which activity occurs (Kearney et al 2008)
Tmax   <- 36.4 # maximum Tb at which activity occurs (Kearney et al 2008)
#T_pref <- 24 # preferred Tb (Kearney et al 2008)
CTmax  <- 40 # critical thermal minimum (affects choice of retreat) Tracy et al 2012
CTmin  <- 5 # critical thermal maximum (affects choice of retreat) Kolbe et al 2010, McCann et al 2014

# Water balance traits
min_hyd <- 70 # minimum tolerated hydration before activity declines (% of fully hydrated animals)
hyd.death <- 50 # minimum tolerated hydration before death (% of fully hydrated animals)
wu_rate <- wu_dat %>% dplyr::filter(strategy == "none") %>% dplyr::select(mg_h_mean)
hyd_rate <- mean(wu_rate$mg_h_mean, na.rm = TRUE) / 1000 # maximum rehydration rate (g/h)
# depends on current and max hydration like this: hyd.rate * ((hyd - hyd.current) / hyd)

# behav = 'diurnal', 'nocturnal' or 'both'
# water; does the frog select depth according to water potential? (TRUE or FALSE)
# water.act; does the activity depend on water loss? (TRUE or FALSE)

source("/Users/nicholaswu/Desktop/global-frog-drought/code/behav_functions.R")

# NULL MODEL
null_curr_wet_mod <- sim.ecto(micro_curr_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = FALSE, burrow = FALSE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = FALSE, water.act = TRUE)

null_curr_dry_mod <- sim.ecto(micro_curr_dry, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = FALSE, burrow = FALSE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = FALSE, water.act = TRUE)

null_warm_wet_mod <- sim.ecto(micro_warm_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = FALSE, burrow = FALSE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = FALSE, water.act = TRUE)

null_warm_dry_mod <- sim.ecto(micro_warm_dry, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = FALSE, burrow = FALSE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = FALSE, water.act = TRUE)

# SHADE MODEL
shad_curr_wet_mod <- sim.ecto(micro_curr_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = FALSE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = FALSE, water.act = TRUE)

shad_curr_dry_mod <- sim.ecto(micro_curr_dry, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = FALSE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = FALSE, water.act = TRUE)

shad_warm_wet_mod <- sim.ecto(micro_warm_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = FALSE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = FALSE, water.act = TRUE)

shad_warm_dry_mod <- sim.ecto(micro_warm_dry, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = FALSE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = FALSE, water.act = TRUE)

# WATER-PROOF MODEL
tree_curr_wet_mod <- sim.ecto(micro_curr_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = FALSE, climb = TRUE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_low, 
                              water = TRUE, water.act = TRUE)

tree_curr_dry_mod <- sim.ecto(micro_curr_dry, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = FALSE, climb = TRUE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_low, 
                              water = TRUE, water.act = TRUE)

tree_warm_wet_mod <- sim.ecto(micro_warm_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = FALSE, climb = TRUE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_low, 
                              water = TRUE, water.act = TRUE)

tree_warm_dry_mod <- sim.ecto(micro_warm_dry, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = FALSE, climb = TRUE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_low, 
                              water = TRUE, water.act = TRUE)

# BURROWING MODEL
burr_curr_wet_mod <- sim.ecto(micro_curr_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = TRUE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = TRUE, water.act = TRUE)

burr_curr_dry_mod <- sim.ecto(micro_curr_dry, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = TRUE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = TRUE, water.act = TRUE)

burr_warm_wet_mod <- sim.ecto(micro_warm_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = TRUE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = TRUE, water.act = TRUE)

burr_warm_dry_mod <- sim.ecto(micro_warm_dry, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = TRUE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = TRUE, water.act = TRUE)

# Behaviour output
null_active <- plot.act(null_curr_wet_mod, micro_curr_wet) + 
  labs(x = "Day of the year", y = "Hour of the day") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  mytheme() + theme(legend.position = "bottom", legend.key.size = unit(2, 'mm'))

shad_active <- plot.act(shad_curr_wet_mod, micro_curr_wet) + 
  labs(x = "Day of the year", y = "Hour of the day") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  mytheme() + theme(legend.position = "bottom", legend.key.size = unit(2, 'mm'))

tree_active <- plot.act(tree_curr_wet_mod, micro_curr_wet) + 
  labs(x = "Day of the year", y = "Hour of the day") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  mytheme() + theme(legend.position = "bottom", legend.key.size = unit(2, 'mm'))

burr_active <- plot.act(burr_curr_wet_mod, micro_curr_wet) + 
  labs(x = "Day of the year", y = "Hour of the day") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  mytheme() + theme(legend.position = "bottom", legend.key.size = unit(2, 'mm'))

prow <- cowplot::plot_grid(
  null_active + theme(legend.position ="none"),
  shad_active + theme(legend.position ="none"),
  tree_active + theme(legend.positio ="none"),
  burr_active + theme(legend.position ="none"),
  ncol = 2, labels = c('a', 'b', 'c', 'd'))

legend_b <- cowplot::get_legend(tree_active + guides(color = guide_legend(nrow = 1)))

plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))

## CALCULATE HOURS OF ACTIVITY ##
# sum activity when active = TRUE, between T_tol, above H_tol, and hydrat <70.

# NULL MODEL
null_curr_wet_df <- data.frame(null_curr_wet_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = null_curr_wet_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(curr_wet = length(active[active == TRUE]))

null_curr_dry_df <- data.frame(null_curr_dry_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = null_curr_dry_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(curr_dry = length(active[active == TRUE]))

null_warm_wet_df <- data.frame(null_warm_wet_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = null_warm_wet_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(warm_wet = length(active[active == TRUE]))

null_warm_dry_df <- data.frame(null_warm_dry_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = null_warm_dry_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(warm_dry = length(active[active == TRUE]))

null_model <- null_curr_wet_df %>%
  merge(null_curr_dry_df, by = "day") %>%
  merge(null_warm_wet_df, by = "day") %>%
  merge(null_warm_dry_df, by = "day") %>%
  pivot_longer(!day, names_to = "condition", values_to = "hours") %>%
  dplyr::mutate(condition = factor(condition, levels = c("warm_dry", "warm_wet", "curr_dry", "curr_wet")))

# SHADE MODEL
shad_curr_wet_df <- data.frame(shad_curr_wet_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = shad_curr_wet_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(curr_wet = length(active[active == TRUE]))

shad_curr_dry_df <- data.frame(shad_curr_dry_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = shad_curr_dry_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(curr_dry = length(active[active == TRUE]))

shad_warm_wet_df <- data.frame(shad_warm_wet_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = shad_warm_wet_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(warm_wet = length(active[active == TRUE]))

shad_warm_dry_df <- data.frame(shad_warm_dry_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = shad_warm_dry_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(warm_dry = length(active[active == TRUE]))

shad_model <- shad_curr_wet_df %>%
  merge(shad_curr_dry_df, by = "day") %>%
  merge(shad_warm_wet_df, by = "day") %>%
  merge(shad_warm_dry_df, by = "day") %>%
  pivot_longer(!day, names_to = "condition", values_to = "hours") %>%
  dplyr::mutate(condition = factor(condition, levels = c("warm_dry", "warm_wet", "curr_dry", "curr_wet")))

# TREE MODEL
tree_curr_wet_df <- data.frame(tree_curr_wet_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = tree_curr_wet_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(curr_wet = length(active[active == TRUE]))

tree_curr_dry_df <- data.frame(tree_curr_dry_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = tree_curr_dry_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(curr_dry = length(active[active == TRUE]))

tree_warm_wet_df <- data.frame(tree_warm_wet_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = tree_warm_wet_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(warm_wet = length(active[active == TRUE]))

tree_warm_dry_df <- data.frame(tree_warm_dry_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = tree_warm_dry_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(warm_dry = length(active[active == TRUE]))

tree_model <- tree_curr_wet_df %>%
  merge(tree_curr_dry_df, by = "day") %>%
  merge(tree_warm_wet_df, by = "day") %>%
  merge(tree_warm_dry_df, by = "day") %>%
  pivot_longer(!day, names_to = "condition", values_to = "hours") %>%
  dplyr::mutate(condition = factor(condition, levels = c("warm_dry", "warm_wet", "curr_dry", "curr_wet")))

# FOSSORIAL MODEL
burr_curr_wet_df <- data.frame(burr_curr_wet_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = burr_curr_wet_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(curr_wet = length(active[active == TRUE]))

burr_curr_dry_df <- data.frame(burr_curr_dry_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = burr_curr_dry_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(curr_dry = length(active[active == TRUE]))

burr_warm_wet_df <- data.frame(burr_warm_wet_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = burr_warm_wet_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(warm_wet = length(active[active == TRUE]))

burr_warm_dry_df <- data.frame(burr_warm_dry_mod$act) %>%
  tibble::rowid_to_column("hour") %>%
  dplyr::rename(active = burr_warm_dry_mod.act) %>%
  dplyr::mutate(day = ceiling(1:8760/24)) %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(warm_dry = length(active[active == TRUE]))

burr_model <- burr_curr_wet_df %>%
  merge(burr_curr_dry_df, by = "day") %>%
  merge(burr_warm_wet_df, by = "day") %>%
  merge(burr_warm_dry_df, by = "day") %>%
  pivot_longer(!day, names_to = "condition", values_to = "hours") %>%
  dplyr::mutate(condition = factor(condition, levels = c("warm_dry", "warm_wet", "curr_dry", "curr_wet")))

## SUMMARISE T_hours ##
# T_hour by models
data.frame(strategy = c("Null", "Null", "Null", "Null",
                        "Shade only", "Shade only", "Shade only", "Shade only",
                        "Waterproof", "Waterproof", "Waterproof", "Waterproof",
                        "Burrowing", "Burrowing", "Burrowing", "Burrowing"),
           temp = c("current", "current", "warming", "warming"),
           rain = c("normal", "drought"),
           t_act_h = c(sum(null_curr_wet_df$curr_wet), sum(null_curr_dry_df$curr_dry), sum(null_warm_wet_df$warm_wet), sum(null_warm_dry_df$warm_dry),
                       sum(shad_curr_wet_df$curr_wet), sum(shad_curr_dry_df$curr_dry), sum(shad_warm_wet_df$warm_wet), sum(shad_warm_dry_df$warm_dry),
                       sum(tree_curr_wet_df$curr_wet), sum(tree_curr_dry_df$curr_dry), sum(tree_warm_wet_df$warm_wet), sum(tree_warm_dry_df$warm_dry),
                       sum(burr_curr_wet_df$curr_wet), sum(burr_curr_dry_df$curr_dry), sum(burr_warm_wet_df$warm_wet), sum(burr_warm_dry_df$warm_dry)))

# change in T_hour relative to curr_wet
data.frame(strategy = c("Null", "Shade only","Waterproof", "Burrowing"),
           curr_wet = c(sum(null_curr_wet_df$curr_wet), sum(shad_curr_wet_df$curr_wet), sum(tree_curr_wet_df$curr_wet), sum(burr_curr_wet_df$curr_wet)),
           curr_dry = c(sum(null_curr_dry_df$curr_dry), sum(shad_curr_dry_df$curr_dry), sum(tree_curr_dry_df$curr_dry), sum(burr_curr_dry_df$curr_dry)),
           warm_wet = c(sum(null_warm_wet_df$warm_wet), sum(shad_warm_wet_df$warm_wet), sum(tree_warm_wet_df$warm_wet), sum(burr_warm_wet_df$warm_wet)),
           warm_dry = c(sum(null_warm_dry_df$warm_dry), sum(shad_warm_dry_df$warm_dry), sum(tree_warm_dry_df$warm_dry), sum(burr_warm_dry_df$warm_dry))) %>%
  dplyr::mutate(delta_warm     = warm_wet - curr_wet,
                delta_dry      = curr_dry - curr_wet,
                delta_warm_dry = warm_dry - curr_wet) %>%
  dplyr::select(-c(curr_wet:warm_dry))

## FIGURE 4 ##
null_plot <- null_model %>%
  ggplot(aes(x = day, y = condition, fill = hours)) + 
  geom_tile() +
  scale_fill_continuous_sequential(palette = "Oslo") +
  ylab(NULL) + xlab("Day of the year") + 
  scale_x_continuous(expand = c(0, 0)) +
  mytheme() + theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 5, label.position = "bottom")) 

shad_plot <- shad_model %>%
  ggplot(aes(x = day, y = condition, fill = hours)) + 
  geom_tile() +
  scale_fill_continuous_sequential(palette = "Oslo") +
  ylab(NULL) + xlab("Day of the year") + 
  scale_x_continuous(expand = c(0, 0)) +
  mytheme() + theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 5, label.position = "bottom")) 

tree_plot <- tree_model %>%
  ggplot(aes(x = day, y = condition, fill = hours)) + 
  geom_tile() +
  scale_fill_continuous_sequential(palette = "Oslo") +
  ylab(NULL) + xlab("Day of the year") + 
  scale_x_continuous(expand = c(0, 0)) +
  mytheme() + theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 5, label.position = "bottom")) 

burr_plot <- burr_model %>%
  ggplot(aes(x = day, y = condition, fill = hours)) + 
  geom_tile() +
  scale_fill_continuous_sequential(palette = "Oslo") +
  ylab(NULL) + xlab("Day of the year") +
  scale_x_continuous(expand = c(0, 0)) +
  mytheme() + theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 5, label.position = "bottom")) 

prow_2 <- cowplot::plot_grid(
  null_plot + theme(legend.position ="none"), 
  shad_plot + theme(legend.position ="none"), 
  tree_plot + theme(legend.position ="none"), 
  burr_plot + theme(legend.position ="none"),
  ncol = 2, labels = c('a','b', 'b', 'c', 'd', 'e'),
  align = 'v', axis = 'l')

legend_b_2 <- cowplot::get_legend(burr_plot + guides(color = guide_legend(nrow = 1)))

plot_grid(prow_2, legend_b_2, ncol = 1, rel_heights = c(1, .1))
