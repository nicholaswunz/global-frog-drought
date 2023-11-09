
setwd('~/Dropbox/4-collaborations/1-papers/Niky Wu/global-frog-drought/code/')

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
Tmax = 25 # maximum temperature for activity
Tmin = 10 # minimum temperature for activity
min.hyd = 80 # minimum hydration percentage
hyd.rate = 3 # maximum rehydration rate
# depends on current and max hydration like this: hyd.rate * ((hyd - hyd.current) / hyd)
CTmin = -2
CTmax = 29 
hyd.death = 50

# behav = 'diurnal', 'nocturnal' or 'both'
# water; does the frog select depth according to water potential? (TRUE or FALSE)
# water.act; does the activity depend on water loss? (TRUE or FALSE)
behav = 'diurnal'
in.shade = FALSE
burrow = FALSE
climb = FALSE

sim.res <- sim.ecto(micro, behav = behav, micro_func = "global",
                    Tmax = Tmax, Tmin = Tmin, in.shade = in.shade,
                    min.hyd = min.hyd, hyd.rate = hyd.rate,
                    CTmin = CTmin, CTmax = CTmax, hyd.death = hyd.death,
                    burrow = burrow, climb = climb,
                    Ww_g = Ww_g, shape = shape, pct_wet = pct_wet,
                    water = FALSE, water.act = FALSE)

sim.res.waterdep <- sim.ecto(micro, behav = behav, micro_func = "global",
                             Tmax = Tmax, Tmin = Tmin, in.shade = in.shade,
                             min.hyd = min.hyd, hyd.rate = hyd.rate, 
                             CTmin = CTmin, CTmax = CTmax, hyd.death = hyd.death,
                             burrow = burrow, climb = climb,
                             Ww_g = Ww_g, shape = shape, pct_wet = pct_wet,
                             water = TRUE, water.act = FALSE)

sim.res.wateract <- sim.ecto(micro, behav = behav, micro_func = "global",
                             Tmax = Tmax, Tmin = Tmin, in.shade = in.shade,
                             min.hyd = min.hyd, hyd.rate = hyd.rate, 
                             CTmin = CTmin, CTmax = CTmax, hyd.death = hyd.death,
                             burrow = burrow, climb = climb,
                             Ww_g = Ww_g, shape = shape, pct_wet = pct_wet,
                             water = FALSE, water.act = TRUE)

sim.res.wateract.waterdep <- sim.ecto(micro, behav = behav, micro_func = "global",
                                      Tmax = Tmax, Tmin = Tmin, in.shade = in.shade,
                                      min.hyd = min.hyd, hyd.rate = hyd.rate, 
                                      CTmin = CTmin, CTmax = CTmax, hyd.death = hyd.death,
                                      burrow = burrow, climb = climb,
                                      Ww_g = Ww_g, shape = shape, pct_wet = pct_wet,
                                      water = TRUE, water.act = TRUE)

# plot activity window plots
plot.act(sim.res, micro)
plot.act(sim.res.waterdep, micro)
plot.act(sim.res.wateract, micro)
plot.act(sim.res.wateract.waterdep, micro)

# water = FALSE, water.act = FALSE
plot(sim.res$TBs, type='l', xlab='Time (h)', ylab='Body temperature (ºC)')
plot(sim.res$hydration, type='l', xlab='Time (h)', ylab='Hydration (% of max hydration)')
plot(sim.res$T_tol, xlab='Time (h)', ylab='Thermal tolerance reached')
plot(sim.res$H_tol, xlab='Time (h)', ylab='Hydric tolerance reached')
plot(sim.res$act, xlab='Time (h)', ylab='Activity')
plot(sim.res$climb, xlab='Time (h)', ylab='Climbing')
plot(-sim.res$dep, type='l', xlab='Time (h)', ylab='Selected vertical position (cm)')

# water = TRUE, water.act = FALSE
plot(sim.res.waterdep$TBs, type='l', xlab='Time (h)', ylab='Body temperature (ºC)')
plot(sim.res.waterdep$hydration, type='l', xlab='Time (h)', ylab='Hydration (% of max hydration)')
plot(sim.res.waterdep$T_tol, xlab='Time (h)', ylab='Thermal tolerance reached')
plot(sim.res.waterdep$H_tol, xlab='Time (h)', ylab='Hydric tolerance reached')
plot(sim.res.waterdep$act, xlab='Time (h)', ylab='Activity')
plot(sim.res.waterdep$climb, xlab='Time (h)', ylab='Climbing')
plot(-sim.res.waterdep$dep, type='l', xlab='Time (h)', ylab='Selected vertical position (cm)')

# water = FALSE, water.act = TRUE
plot(sim.res.wateract$TBs, type='l', xlab='Time (h)', ylab='Body temperature (ºC)')
plot(sim.res.wateract$hydration, type='l', xlab='Time (h)', ylab='Hydration (% of max hydration)')
plot(sim.res.wateract$T_tol, xlab='Time (h)', ylab='Thermal tolerance reached')
plot(sim.res.wateract$H_tol, xlab='Time (h)', ylab='Hydric tolerance reached')
plot(sim.res.wateract$act, xlab='Time (h)', ylab='Activity')
plot(sim.res.wateract$climb, xlab='Time (h)', ylab='Climbing')
plot(-sim.res.wateract$dep, type='l', xlab='Time (h)', ylab='Selected vertical position (cm)')

# water = TRUE, water.act = TRUE
plot(sim.res.wateract.waterdep$TBs, type='l', xlab='Time (h)', ylab='Body temperature (ºC)')
plot(sim.res.wateract.waterdep$hydration, type='l', xlab='Time (h)', ylab='Hydration (% of max hydration)')
plot(sim.res.wateract.waterdep$T_tol, xlab='Time (h)', ylab='Thermal tolerance reached')
plot(sim.res.wateract.waterdep$H_tol, xlab='Time (h)', ylab='Hydric tolerance reached')
plot(sim.res.wateract.waterdep$act, xlab='Time (h)', ylab='Activity')
plot(sim.res.wateract.waterdep$climb, xlab='Time (h)', ylab='Climbing')
plot(-sim.res.wateract.waterdep$dep, type='l', xlab='Time (h)', ylab='Selected vertical position (cm)')


range(sim.res$hydration)
range(sim.res.waterdep$hydration)
range(sim.res.wateract$hydration)
range(sim.res.wateract.waterdep$hydration)

sum(sim.res$act) /2 # activity hours per year
sum(sim.res.waterdep$act) /2
sum(sim.res.wateract$act) /2
sum(sim.res.wateract.waterdep$act) /2




# compare to the ectotherm model in NicheMapR with similar behavior
sim.res <- sim.ecto(micro, behav = 'nocturnal', Tmax = Tmax, Tmin = Tmin, in.shade = FALSE,
                    min.hyd = min.hyd, hyd.rate = hyd.rate, water = FALSE, water.act = FALSE,
                    burrow = TRUE, climb = FALSE,
                    Ww_g = Ww_g, shape = shape, pct_wet = pct_wet)

ecto_nmr <- ectotherm(Ww_g = 40, shape = 4, M_1 = 0, M_2 = 0, M_3 = 0,
          postur = 0, pantmax = 0, pct_cond = 40, pct_wet = 80, # 0.01
          CT_min = 10, T_RB_min = 10, T_B_min = 10, T_F_min = 10,
          T_pref = 30, T_F_max = 30, CT_max = 30,
          diurn = 0, nocturn = 1, crepus = 0, shade_seek = 0, burrow=1,
          shdburrow = 0, minshades = micro$minshade)

environ <- data.frame(ecto_nmr$environ)


with(environ, plot(TC, type='l'))
with(sim.res, points(TBs, type='l', col='red'))

with(environ, plot(DEP, type='l'))
with(sim.res, points(-dep, type='l', col='red'))

# R is much slower than fortran
# but out results are equivalent to NicheMapR


