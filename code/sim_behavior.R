
setwd('~/Dropbox/1_papers/collaborations/Niky Wu/global-frog-drought/code/')

rm(list=ls())

library(NicheMapR)
source('micro_global_drought.R')
source('behav_functions.R')

longlat    <- c(153.09249, -27.6235) # Karawatha, QLD.
micro <- micro_global_drought(loc=longlat, rainfact = 0.54,
                              runmoist = T, runshade = T, 
                              timeinterval = 365, 2)

micro.output <- retrieve.output(micro)


sim.res <- sim.ecto(micro, behav = 'diurnal', min.hyd=80, hyd.rate=3)
#sim.res <- unlist(sim.res)

plot(sim.res$hydration, type='l')
plot(sim.res$TBs, type='l')




