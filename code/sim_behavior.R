
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

Tmax = 30 # maximum temperature for activity
Tmin = 10 # minimum temperature for activity
min.hyd = 80 # minimum hydration percentage
hyd.rate = 3 # maximum rehydration rate
# depends on current and max hydration like this: hyd.rate * ((hyd - hyd.current) / hyd)

# behav = 'diurnal', 'nocturnal' or 'both'
# water; does the frog select depth according to water potential? (TRUE or FALSE)
# water.act; does the activity depend on water loss? (TRUE or FALSE)

sim.res <- sim.ecto(micro, behav = 'nocturnal', Tmax = Tmax, Tmin = Tmin, 
                    min.hyd = min.hyd, hyd.rate = hyd.rate, water = FALSE, water.act = FALSE)

sim.res.waterdep <- sim.ecto(micro, behav = 'nocturnal', Tmax = Tmax, Tmin = Tmin, 
                             min.hyd = min.hyd, hyd.rate = hyd.rate, water = TRUE, water.act = FALSE)

sim.res.wateract <- sim.ecto(micro, behav = 'nocturnal', Tmax = Tmax, Tmin = Tmin, 
                             min.hyd = min.hyd, hyd.rate = hyd.rate, water = FALSE, water.act = TRUE)

sim.res.wateract.waterdep <- sim.ecto(micro, behav = 'nocturnal', Tmax = Tmax, Tmin = Tmin, 
                                      min.hyd = min.hyd, hyd.rate = hyd.rate, water = TRUE, water.act = TRUE)

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


