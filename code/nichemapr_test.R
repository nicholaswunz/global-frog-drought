library(NicheMapR)
library(tidyverse)

# Load data
raw_dat <- read.csv("raw_data.csv") %>%
  dplyr::select(study_ID:unit) %>%
  dplyr::mutate(strategy = factor(case_when(strategy == "" ~ "none", TRUE ~ as.character(strategy))),
                strategy = fct_relevel(strategy, "none", "water-proof", "cocoon", "hollow"))

resist_dat <- raw_dat %>%
  dplyr::filter(unit == "s cm")

wu_dat <- raw_dat %>%
  filter(response == "water uptake") %>%
  dplyr::mutate(mg_h_mean = unit_corrected_mean * vent_SA_cm2,
                mg_h_sd   = unit_corrected_sd * vent_SA_cm2,
                lnMean    = log(mg_h_mean),
                es_v      = mg_h_sd^2 / sample_size, # sampling variance (v)
                es_sei    = sqrt(es_v), # standard error (SE)
                es_inv    = 1 / es_sei) # precision (inverse of SE)

# Frog model
# computes the heat exchange by convection (extract mass transfer coefficient, Prandtl number and Schmidt number)
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
Ww_g    <- mean(raw_dat$mean_mass_g, na.rm = TRUE) # wet weight of animal (g)
SVL_cm  <- 2.38 * Ww_g ^ 0.31 # snout-vent-length conversion from Pough 1980
r_s_low      <- resist_dat %>% dplyr::filter(strategy == "none") %>% dplyr::select(unit_corrected_mean) # skin resistance
pct_wet_low  <- 1 / (CONV_out[5] * mean(r_s_low$unit_corrected_mean) + (CONV_out[11] / CONV_out[13]) ^ 0.6666666)  # % of surface area acting as a free-water exchanger (Pirtle et al 2017)
r_s_high     <- resist_dat %>% dplyr::filter(strategy == "water-proof") %>% dplyr::select(unit_corrected_mean) # skin resistance
pct_wet_high <- 1 / (CONV_out[5] * max(r_s_high$unit_corrected_mean) + (CONV_out[11] / CONV_out[13]) ^ 0.6666666)  # % of surface area acting as a free-water exchanger (Pirtle et al 2017)

# Thermal traits based on Rhinella marina
T_RB_min <- 10 # min Tb at which they will attempt to leave retreat (pers obs for Brisbane)
T_B_min  <- 10 # min Tb at which leaves retreat to bask (pers obs for Brisbane)
T_F_min  <- 13.7 # minimum Tb at which activity occurs (Kearney et al 2008)
T_F_max  <- 37.4 # maximum Tb at which activity occurs (Kearney et al 2008)
T_pref   <- 24 # preferred Tb (will try and regulate to this) (Kearney et al 2008)
CT_max   <- 40 # critical thermal minimum (affects choice of retreat) Tracy et al 2012
CT_min   <- 5 # critical thermal maximum (affects choice of retreat) Kolbe et al 2010, McCann et al 2014

# Microclimate model
longlat    <- c(153.09249, -27.6235) # Karawatha, QLD.
micro_norm <- NicheMapR::micro_global(loc = longlat, timeinterval = 365)

# Basic model
ecto <- NicheMapR::ectotherm(Ww_g = Ww_g, alpha_max = 0.9, alpha_min = 0.9, 
                               shape = 4, pct_wet = pct_wet,
                               # thermal traits
                               T_F_max = T_F_max, T_F_min = T_F_min, T_B_min = T_B_min, T_RB_min = T_RB_min,
                               CT_max = CT_max, CT_min = CT_min, T_pref = T_pref, 
                               # behavioural traits
                               shade_seek = 1, # shade seeking?
                               burrow = 0, # can it burrow?
                               fossorial = 0, # fossorial activity?
                               climb = 0, # can it climb to thermoregulate?
                               nocturn = 1,  # nocturnal activity
                               diurn = 1, # diurnal activity
                               crepus = 1, # crepuscular activity
                               # activity threshold
                               pct_H_R = 30, # 30% loss in water mass before locomotor performance drops
                               pct_H_death = 50, # 50% loss in water mass before death
                               minshades = rep(0, 365), 
                               maxshades = micro_norm$maxshade)

environ <- as.data.frame(ecto$environ) # behaviour, Tb and environment
enbal   <- as.data.frame(ecto$enbal) # heat balance outputs
masbal  <- as.data.frame(ecto$masbal) # mass balance outputs
metout  <- as.data.frame(micro$metout) # above ground microclimate
environ <- cbind(environ,metout$SOLR) # add solar radiation for activity window plots

# check environment moisture and rainfall
ecto$humid
ecto$rainfall
environ$RELHUM

# not sure which is most relevant. 

# From the ecotherm tutorial
forage <- subset(environ, ACT == 2) # get foraging hours
bask   <- subset(environ, ACT == 1) # get basking hours
night  <- subset(environ, SOLAR == 0) # get night hours
with(night, plot(TIME ~ DOY, ylab = "Hour of Day", xlab = "Day of Year", pch = 15, cex = 1, 
                 col = 'dark blue')) # nighttime hours
with(forage, points(TIME ~ DOY, pch = 15, cex = 1, col = 'orange')) # foraging Tbs
with(bask, points(TIME ~ DOY, pch = 15, cex = 1, col = 'light blue')) # basking Tbs

unique(environ$ACT)

# Playing around with plots

# RH at surface in location
ggplot(metout) +
  geom_point(aes(x = DOY, y = RH)) +
  ylab("Relative humidity (%)")

# EWL across the day
test <- merge(environ, masbal, by = "TIME") # add solar radiation for activity window plots


ggplot(test) +
  geom_point(aes(x = TIME, y = H2OCut_g, colour = TSUB)) +
  ylab("Cutaneous water loss (g/h)")

# O2 consumption across the day
ggplot(masbal) +
  geom_point(aes(x = TIME, y = O2_ml)) +
  ylab("Oxygen consumption rate (ml/h)")

# Number of hours for activity
forage %>%
  group_by(DAY) %>%
  summarise(act_h = length(TIME),
            TA_mean = mean(TA),
            TA_max = max(TA),
            RH_mean = mean(RELHUM)) %>%
  ggplot() +
  geom_line(aes(x = DAY, y = act_h)) +
  geom_line(aes(x = DAY, y = TA_max), colour = "red") +
  geom_line(aes(x = DAY, y = RH_mean), colour = "blue") +
  ylab("Activity hours") +
  mytheme()

forage %>%
  ggplot() +
  geom_point(aes(x = DAY, y = TIME))

forage$TIME
unique(environ$DOY)


## TEST SIMULATIONS - 15/12/2022 ## -------------------------------------------------------------------

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
pct_wet_high  <- 1 / (CONV_out[5] * mean(r_s_low$unit_corrected_mean) + (CONV_out[11] / CONV_out[13]) ^ 0.6666666) * 100  # % of surface area acting as a free-water exchanger (Pirtle et al 2017)

# parameters for a water-proof frog
r_s_high     <- resist_dat %>% dplyr::filter(strategy == "water-proof") %>% dplyr::select(unit_corrected_mean) # skin resistance
pct_wet_low <- 1 / (CONV_out[5] * max(r_s_high$unit_corrected_mean) + (CONV_out[11] / CONV_out[13]) ^ 0.6666666) * 100 # % of surface area acting as a free-water exchanger (Pirtle et al 2017)

# Thermal traits based on Rhinella marina
Tmin   <- 13.7 # minimum Tb at which activity occurs (Kearney et al 2008)
Tmax   <- 36.4 # maximum Tb at which activity occurs (Kearney et al 2008)
T_pref <- 24 # preferred Tb (Kearney et al 2008)
CTmax  <- 40 # critical thermal minimum (affects choice of retreat) Tracy et al 2012
CTmin  <- 5 # critical thermal maximum (affects choice of retreat) Kolbe et al 2010, McCann et al 2014

# Water balance traits
min_hyd <- 70 # minimum tolerated hydration before activity declines (% of fully hydrated animals)
hyd.death <- 50 # minimum tolerated hydration before death (% of fully hydrated animals)
wu_rate <- wu_dat %>% dplyr::filter(strategy == "none") %>% dplyr::select(mg_h_mean)
hyd_rate <- mean(wu_rate$mg_h_mean, na.rm = TRUE) / 1000 # maximum rehydration rate (g/h)
# depends on current and max hydration like this: hyd.rate * ((hyd - hyd.current) / hyd)

# NULL MODEL - does not burrow or climb. Under sun condition only
null_curr_wet_mod <- sim.ecto(micro_curr_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = FALSE, burrow = FALSE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = FALSE, water.act = FALSE)

null_curr_dry_mod <- sim.ecto(micro_curr_dry, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = FALSE, burrow = FALSE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = FALSE, water.act = FALSE)

# SHADE MODEL - does not burrow or climb. Under shade condition only
shad_curr_wet_mod <- sim.ecto(micro_curr_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = FALSE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = FALSE, water.act = TRUE)

# WATER-PROOF MODEL - climbs but does not burrow. Under shade condition only
tree_curr_wet_mod <- sim.ecto(micro_curr_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = FALSE, climb = TRUE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_low, 
                              water = TRUE, water.act = TRUE)

# BURROWING MODEL - burrows but does not climb. Under shade condition only
burr_curr_wet_mod <- sim.ecto(micro_curr_wet, Ww_g = Ww_g, shape = 4, 
                              Tmax = Tmax, Tmin = Tmin, CTmin = CTmin, CTmax = CTmax,
                              behav = 'both', in.shade = TRUE, burrow = TRUE, climb = FALSE,
                              min.hyd = min_hyd, hyd.death = hyd.death,
                              hyd.rate = hyd_rate, pct_wet = pct_wet_high, 
                              water = TRUE, water.act = TRUE)

## Ask Urtzi to check ##
plot.act(null_curr_wet_mod, micro_curr_wet) + ggtitle("null model - wet")
plot.act(null_curr_dry_mod, micro_curr_dry) + ggtitle("null model - dry")

plot(null_curr_wet_mod$hydration, type='l', xlab='Time (h)', ylab='Hydration (%)', main = 'null model - wet')
plot(null_curr_dry_mod$hydration, type='l', xlab='Time (h)', ylab='Hydration (%)', main = 'null model - dry')  # the dry model is showing more hydration?

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
             
