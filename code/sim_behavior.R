
setwd('~/Dropbox/1_papers/collaborations/Niky Wu/code')

rm(list=ls())

source('micro_global_drought2.R')
source('behav_functions.R')

micro <- micro_global_drought(rainfact = 0.54)

sim.res <- sim.ecto(micro)

sim.res <- unlist(sim.res)
plot(sim.res, type='l')






