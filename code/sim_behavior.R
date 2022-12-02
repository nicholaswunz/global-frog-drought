
setwd('~/Dropbox/1_papers/collaborations/Niky Wu/global-frog-drought/code/')

rm(list=ls())

library(NicheMapR)
source('micro_global_drought.R')
source('behav_functions.R')

longlat    <- c(153.09249, -27.6235) # Karawatha, QLD.
micro <- micro_global_drought(loc=longlat, rainfact = 0.80, rhfact = 0.90,
                              runmoist = T, runshade = T, 
                              timeinterval = 365, nyears = 1)
# micro <- micro_global_drought(loc=longlat, rainfact = 0.80, rhfact = 0.90,
#                               runmoist = T, runshade = T, 
#                               timeinterval = 12, nyears = 2)


# set parameter values

Ww_g = 40
shape = 4
pct_wet = 80
Tmax = 30 # maximum temperature for activity
Tmin = 10 # minimum temperature for activity
min.hyd = 80 # minimum hydration percentage
hyd.rate = 3 # maximum rehydration rate
# depends on current and max hydration like this: hyd.rate * ((hyd - hyd.current) / hyd)

# behav = 'diurnal', 'nocturnal' or 'both'
# water; does the frog select depth according to water potential? (TRUE or FALSE)
# water.act; does the activity depend on water loss? (TRUE or FALSE)

sim.res <- sim.ecto(micro, behav = 'nocturnal', Tmax = Tmax, Tmin = Tmin, in.shade = FALSE,
                    min.hyd = min.hyd, hyd.rate = hyd.rate, water = FALSE, water.act = FALSE,
                    burrow = TRUE, climb = FALSE,
                    Ww_g = Ww_g, shape = shape, pct_wet = pct_wet)

sim.res.waterdep <- sim.ecto(micro, behav = 'nocturnal', Tmax = Tmax, Tmin = Tmin, in.shade = FALSE,
                             min.hyd = min.hyd, hyd.rate = hyd.rate, water = TRUE, water.act = FALSE,
                             burrow = TRUE, climb = FALSE,
                             Ww_g = Ww_g, shape = shape, pct_wet = pct_wet)

sim.res.wateract <- sim.ecto(micro, behav = 'nocturnal', Tmax = Tmax, Tmin = Tmin, in.shade = FALSE,
                             min.hyd = min.hyd, hyd.rate = hyd.rate, water = FALSE, water.act = TRUE,
                             burrow = TRUE, climb = FALSE,
                             Ww_g = Ww_g, shape = shape, pct_wet = pct_wet)

sim.res.wateract.waterdep <- sim.ecto(micro, behav = 'nocturnal', Tmax = Tmax, Tmin = Tmin, in.shade = FALSE,
                                      min.hyd = min.hyd, hyd.rate = hyd.rate, water = TRUE, water.act = TRUE,
                                      burrow = TRUE, climb = FALSE,
                                      Ww_g = Ww_g, shape = shape, pct_wet = pct_wet)

# water = FALSE, water.act = FALSE
plot(sim.res$TBs, type='l')
plot(sim.res$hydration, type='l')
plot(sim.res$act, type='l')
plot(-sim.res$dep, type='l')

# water = TRUE, water.act = FALSE
plot(sim.res.waterdep$TBs, type='l')
plot(sim.res.waterdep$hydration, type='l')
plot(sim.res.waterdep$act, type='l')
plot(-sim.res.waterdep$dep, type='l')

# water = FALSE, water.act = TRUE
plot(sim.res.wateract$TBs, type='l')
plot(sim.res.wateract$hydration, type='l')
plot(sim.res.wateract$act, type='l')
plot(-sim.res.wateract$dep, type='l')

# water = TRUE, water.act = TRUE
plot(sim.res.wateract.waterdep$TBs, type='l')
plot(sim.res.wateract.waterdep$hydration, type='l')
plot(sim.res.wateract.waterdep$act, type='l')
plot(-sim.res.wateract.waterdep$dep, type='l')


range(sim.res$hydration)
range(sim.res.waterdep$hydration)
range(sim.res.wateract$hydration)
range(sim.res.wateract.waterdep$hydration)

sum(sim.res$act) /2 # activity hours per year
sum(sim.res.waterdep$act) /2
sum(sim.res.wateract$act) /2
sum(sim.res.wateract.waterdep$act) /2




# compare to the ectotherm model in NicheMapR with similar behavior
sim.res <- sim.ecto(micro, behav = 'diurnal', Tmax = Tmax, Tmin = Tmin, in.shade = TRUE,
                    min.hyd = min.hyd, hyd.rate = hyd.rate, water = FALSE, water.act = FALSE,
                    burrow = TRUE, climb = FALSE,
                    Ww_g = Ww_g, shape = shape, pct_wet = pct_wet)

ecto_nmr <- ectotherm(Ww_g = 40, shape = 4, M_1 = 0, M_2 = 0, M_3 = 0,
          postur = 0, pantmax = 0, pct_cond = 40, pct_wet = 80, # 0.01
          CT_min = 10, T_RB_min = 10, T_B_min = 10, T_F_min = 10,
          T_pref = 30, T_F_max = 30, CT_max = 30,
          diurn = 1, nocturn = 0, crepus = 0, shade_seek = 0, burrow=1,
          shdburrow = 2, minshades = micro$maxshade)

environ <- data.frame(ecto_nmr$environ)


with(environ, plot(TC, type='l'))
with(sim.res, points(TBs, type='l', col='red'))
with(environ, points(TC, type='l'))
# R is much slower than fortran
# but out results are equivalent to NicheMapR


