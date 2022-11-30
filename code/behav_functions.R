
# function to retrieve output from micro objects
retrieve.output <- function(micro){
  
  micro.output <- list()
  # retrieve ouptut
  # date <- weather$date[1:nrow(micro$metout)]
  micro.output[['metout']] <- as.data.frame(micro$metout) # above ground microclimatic conditions, min shade
  micro.output[['shadmet']] <- as.data.frame(micro$shadmet) # above ground microclimatic conditions, max shade
  micro.output[['soil']] <- as.data.frame(micro$soil) # soil temperatures, minimum shade
  micro.output[['shadsoil']] <- as.data.frame(micro$shadsoil) # soil temperatures, maximum shade
  micro.output[['soilmoist']] <- as.data.frame(micro$soilmoist) # soil moisture, minimum shade
  micro.output[['shadmoist']] <- as.data.frame(micro$shadmoist) # soil moisture, maximum shade
  micro.output[['humid']] <- as.data.frame(micro$humid) # soil humidity, minimum shade
  micro.output[['shadhumid']] <- as.data.frame(micro$shadhumid) # soil humidity, maximum shade
  micro.output[['soilpot']] <- as.data.frame(micro$soilpot) # soil water potential, minimum shade
  micro.output[['shadpot']] <- as.data.frame(micro$shadpot) # soil water potential, maximum shade
  
  return(micro.output)
}



# function to define when an animal will be active
activity <- function(micro.output, behav=c('diurnal', 'nocturnal', 'both'), Z=Z){
  if(!(behav %in% c('diurnal', 'nocturnal', 'both'))){
    stop('Behavior has to be \'diurnal\', \'nocturnal\', or \'both\'')
  }
  if(behav == 'diurnal' & Z == 90 |
     behav == 'nocturnal' & Z < 90){
    FALSE
  } else {
    TRUE    
  }
}


# activity based on thermal range
thermal.range <- function(TCinit, Tmax=30, Tmin=10, x){
  if(TCinit > Tmax | TCinit < Tmin){
    FALSE
  } else{
    TRUE
  }
}



# function to find frogs position below-ground
seldep <- function(micro.output, CTmax = 30, CTmin = 10, x){
  soil.temps <- micro.output$soil[x, 4:12] # soil temperatures from 2.5cm to 2m
  # selects the shallowest node with temperatures between CTmax and CTmin
  sel.node <- which(soil.temps <= CTmax & soil.temps >= CTmin)[1]
  colnames(soil.temps)[sel.node]
}


# set the environment given activity (1 = active above-ground; 0 = below-ground)
environment <- function(micro.output, activity, x){
  
  soil <- micro.output$soil
  
  if(activity){
    # above-ground
    metout <- micro.output$metout
    
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
    dep <- seldep(micro.output, CTmax = 30, CTmin = 10, x)
    
    TA <- soil[,dep][x]
    TGRD <- TA
    TSKY <- TA
    VEL <- 0.01
    RH <- 99
    QSOLR <- 0.0
    # EMISSB <- 1.0
    # EMISSK <- 1.0
  }
  return(list(TA=TA, TGRD=TGRD, TSKY=TSKY, VEL=VEL, RH=RH, QSOLR=QSOLR))
}




# function to run ectotherm simulations
sim.ecto <- function(micro, behav='diurnal', Tmax=30, Tmin=10){
  
  micro.output <- retrieve.output(micro)
  
  lapply(1:(micro$ndays * 24), function(x){
    
    zenith <- micro.output$metout$ZEN[x]
    act <- activity(micro.output, behav=behav, Z=zenith)
    
    if(act){
      env <- environment(micro.output, act, x)
      ecto <- ectoR_devel(Ww_g = 40,
                          shape = 4,
                          alpha = 0.85,
                          M_1 = 0,
                          postur = 0,
                          pantmax = 0,
                          pct_cond = 40,
                          pct_wet = 0.01,
                          #pct_wet = 80,
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
      TCinit <- ecto$TC
      suit.therm <- thermal.range(TCinit, Tmax=Tmax, Tmin=Tmin, x)
      if(!suit.therm){
        env <- environment(micro.output, act=FALSE, x)
        ecto <- ectoR_devel(Ww_g = 40,
                            shape = 4,
                            alpha = 0.85,
                            M_1 = 0,
                            postur = 0,
                            pantmax = 0,
                            pct_cond = 40,
                            pct_wet = 0.01,
                            #pct_wet = 80,
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
      }
      
    } else {
      env <- environment(micro.output, act=act, x)
      
      ecto <- ectoR_devel(Ww_g = 40,
                          shape = 4,
                          alpha = 0.85,
                          M_1 = 0,
                          postur = 0,
                          pantmax = 0,
                          pct_cond = 40,
                          pct_wet = 0.01,
                          #pct_wet = 80,
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
    }
    
    
    ecto$TC
    #print(x)
  })
}


