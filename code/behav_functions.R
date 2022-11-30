
# function to retrieve output from micro objects
retrieve.output <- function(micro){
  
  micro.output <- list()
  
  # retrieve ouptut
  # date <- weather$date[1:nrow(micro$metout)]
  metout <- as.data.frame(micro$metout) # retrieve above ground microclimatic conditions, min shade
  shadmet <- as.data.frame(micro$shadmet) # retrieve above ground microclimatic conditions, max shade
  soil <- as.data.frame(micro$soil) # retrieve soil temperatures, minimum shade
  shadsoil <- as.data.frame(micro$shadsoil) # retrieve soil temperatures, maximum shade
  soilmoist <- as.data.frame(micro$soilmoist) # retrieve soil moisture, minimum shade
  shadmoist <- as.data.frame(micro$shadmoist) # retrieve soil moisture, maximum shade
  humid <- as.data.frame(micro$humid) # retrieve soil humidity, minimum shade
  shadhumid <- as.data.frame(micro$shadhumid) # retrieve soil humidity, maximum shade
  soilpot <- as.data.frame(micro$soilpot) # retrieve soil water potential, minimum shade
  shadpot <- as.data.frame(micro$shadpot) # retrieve soil water potential, maximum shade
  
  # append dates
  micro.output[['metout']] <- metout
  micro.output[['shadmet']] <- shadmet
  micro.output[['soil']] <- soil
  micro.output[['shadsoil']] <- shadsoil
  micro.output[['soilmoist']] <- soilmoist
  micro.output[['shadmoist']] <- shadmoist
  micro.output[['humid']] <- humid
  micro.output[['shadhumid']] <- shadhumid
  micro.output[['soilpot']] <- soilpot
  micro.output[['shadpot']] <- shadpot
  
  return(micro.output)
}



# function to define when an animal will be active
activity <- function(behav=c('diurnal', 'nocturnal', 'both'), Tmax=30, Tmin=10, Z=Z){
  if(behav == 'diurnal' & Z == 90 |
     behav == 'nocturnal' & Z < 90){
    FALSE
  } else {
    TRUE
  }
}


# function to find frogs position below-ground
seldep <- function(){
  
}


# set the environment given activity (1 = active above-ground; 0 = below-ground)
environment <- function(micro.output, activity, x){
  
  if(activity){
    # above-ground
    metout <- micro.output$metout
    soil <- micro.output$soil
    
    # get required inputs
    TA <- metout$TALOC[x]
    TGRD <- soil$D0cm[x]
    TSKY <- metout$TSKYC[x]
    VEL <- metout$VLOC[x]
    RH <- metout$RHLOC[x]
    QSOLR <- metout$SOLR[x]
  }
  else {
    # below-ground

    # TA <- 10
    # TGRD <- TA
    # TSKY <- TA
    # VEL <- 0.01
    # RH <- 99
    # QSOLR <- 0.0
    # # EMISSB <- 1.0
    # # EMISSK <- 1.0
    # 
    
    seldep()
  }
  
  return(list(TA=TA, TGRD=TGRD, TSKY=TSKY, VEL=VEL, RH=RH, QSOLR=QSOLR))
}




# function to run ectotherm simulations
sim.ecto <- function(micro, behav='diurnal', Tmax=30, Tmin=10){
  
  micro.output <- retrieve.output(micro)
  
  lapply(1:nrow(micro.output$metout), function(x){
    
    zenith <- micro.output$metout$ZEN[x]
    act <- activity(behav=behav, Tmax=Tmax, Tmin=Tmin, Z=zenith)
    env <- environment(micro.output, act, x)
    
    ecto <- ectoR_devel(Ww_g = 40,
                        shape = 4,
                        alpha = 0.85,
                        M_1 = 0,
                        postur = 0,
                        pantmax = 0,
                        pct_cond = 40,
                        pct_wet = 80,
                        K_sub = 0.1,
                        alpha_sub = (1 - micro$REF),
                        elev = micro$elev,
                        TA = env$TA,
                        TGRD = env$TGRD,
                        TSUBST = env$TGRD,
                        TSKY = env$TSKY,
                        VEL = env$VEL,
                        RH = env$RH,
                        QSOLR = env$QSOLR,
                        Z = zenith)
    ecto$TC
  })
}


