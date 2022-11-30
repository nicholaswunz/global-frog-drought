
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
thermal.range <- function(ecto, Tmax=30, Tmin=10){
  TC <- ecto$TC
  if(TC > Tmax | TC < Tmin){
    FALSE
  } else{
    TRUE
  }
}



# activity based on tolerance to desiccation
hydro.range <- function(ecto, hyd, min.water){
  newhyd <- hyd - data.frame(ecto$masbal)$H2OCut_g
  if(newhyd <= min.water){
    FALSE
  } else {
    TRUE
  }
}


update.hyd <- function(ecto, hyd, max.hyd){
  newhyd <- hyd - data.frame(ecto$masbal)$H2OCut_g
  if(newhyd > max.hyd) {
    newhyd <- hyd
  }
  if(newhyd < 0) {
    newhyd <- 0
  }
  return(newhyd)
}



rehydrate <- function(micro.output, env, hyd, hyd.current, hyd.rate=0.01, x){
  dep <- paste0('PT',substr(env$dep,2,nchar(env$dep)))
  wpot.soil <- micro.output$soilpot[x,dep]
  if(wpot.soil >= -72.5){
    newhyd <- hyd.current + hyd.rate * ((hyd - hyd.current) / hyd)
  } else {
    newhyd <- hyd.current
  }
  return(newhyd)
}


# function to find frogs position below-ground
seldep <- function(micro.output, CTmax = 30, CTmin = 10, water=FALSE, x){
  if(!water){
    soil.temps <- micro.output$soil[x, 4:12] # soil temperatures from 2.5cm to 2m
    # selects the shallowest node with temperatures between CTmax and CTmin
    sel.node <- which(soil.temps <= CTmax & soil.temps >= CTmin)[1]
    colnames(soil.temps)[sel.node]
  } else {
    soil.pots <- micro.output$soilpot[x, 4:12] # soil temperatures from 2.5cm to 2m
    # selects the shallowest node with water potential >= -72.5
    sel.node <- which(soil.pots >= -72.5)[1]
    paste0('D', substr(colnames(soil.pots)[sel.node], 3, nchar(colnames(soil.pots)[sel.node])))
  }
}


# set the environment given activity (1 = active above-ground; 0 = below-ground)
environment <- function(micro.output, activity, water=TRUE, x){
  
  soil <- micro.output$soil
  
  if(activity){
    # above-ground
    dep <- "D0cm"
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
    dep <- seldep(micro.output, CTmax = 30, CTmin = 10, water=water, x)
    
    TA <- soil[,dep][x]
    TGRD <- TA
    TSKY <- TA
    VEL <- 0.01
    RH <- 99
    QSOLR <- 0.0
    # EMISSB <- 1.0
    # EMISSK <- 1.0
  }
  return(list(TA=TA, TGRD=TGRD, TSKY=TSKY, VEL=VEL, RH=RH, QSOLR=QSOLR, dep=dep))
}




# function to run ectotherm simulations
sim.ecto <- function(micro, behav = 'nocturnal', Tmax = 30, Tmin = 10, 
                     min.hyd = 70, hyd.rate = 3, water = TRUE, water.act = TRUE,
                     Ww_g = 40,
                     shape = 4,
                     alpha = 0.85,
                     M_1 = 0,
                     postur = 0,
                     pantmax = 0,
                     pct_cond = 40,
                     pct_wet = 80, # 0.01
                     K_sub = 0.1,
                     alpha_sub = (1 - micro$REF),
                     elev = micro$elev){
  
  micro.output <- retrieve.output(micro)
  
  hyd <- Ww_g * 70/100 # water content ob body in grams
  min.water <- hyd * min.hyd/100
  
  hydration <- hyd # to stored hydration
  TBs <- c() # to store Tbs
  ACTs <- c() # to store activity (TRUE = active; FALSE = underground)
  DEPs <- c() # to store the depth if underground
  
  for(x in 1:(micro$ndays * 24)){ 
    zenith <- micro.output$metout$ZEN[x]
    act <- activity(micro.output, behav=behav, Z=zenith)
    ACTs <- c(ACTs, act)
    
    if(act){
      env <- environment(micro.output, act, water=water, x)
      DEPs <- c(DEPs, as.double(substr(env$dep, 2, nchar(env$dep)-2)))
      ecto <- ectoR_devel(Ww_g = Ww_g,
                          shape = shape,
                          alpha = alpha,
                          M_1 = M_1,
                          postur = postur,
                          pantmax = pantmax,
                          pct_cond = pct_cond,
                          pct_wet = pct_wet,
                          K_sub = K_sub,
                          alpha_sub = alpha_sub,
                          elev = elev,
                          TA = env$TA,
                          TGRD = env$TGRD,
                          TSUBST = env$TGRD,
                          TSKY = env$TSKY,
                          VEL = env$VEL,
                          RH = env$RH,
                          QSOLR = env$QSOLR,
                          Z = zenith)
      
      suit.therm <- thermal.range(ecto, Tmax = Tmax, Tmin = Tmin)
      if(water.act){
        suit.hydro <- hydro.range(ecto, hyd = hydration[x], min.water = min.water)
      } else {
        suit.hydro = TRUE
      }
      
      if(!(suit.therm) | !(suit.hydro)){
        env <- environment(micro.output, water=water, act=FALSE, x)
        DEPs <- c(DEPs, as.double(substr(env$dep, 2, nchar(env$dep)-2)))
        ecto <- ectoR_devel(Ww_g = Ww_g,
                            shape = shape,
                            alpha = alpha,
                            M_1 = M_1,
                            postur = postur,
                            pantmax = pantmax,
                            pct_cond = pct_cond,
                            pct_wet = pct_wet,
                            K_sub = K_sub,
                            alpha_sub = alpha_sub,
                            elev = elev,
                            TA = env$TA,
                            TGRD = env$TGRD,
                            TSUBST = env$TGRD,
                            TSKY = env$TSKY,
                            VEL = env$VEL,
                            RH = env$RH,
                            QSOLR = env$QSOLR,
                            Z = zenith)
        hydration <- c(hydration, rehydrate(micro.output, env, 
                                              hyd = hyd, 
                                              hyd.current = hydration[x], 
                                              hyd.rate=hyd.rate, 
                                              x = x))
        TBs <- c(TBs, ecto$TC)
        ACTs[x] <- (suit.therm) & (suit.hydro)
      } else {
        hydration <- c(hydration, update.hyd(ecto, hydration[x], hyd))
        TBs <- c(TBs, ecto$TC)
      }
    } else {
      env <- environment(micro.output, water=water, act=act, x)
      DEPs <- c(DEPs, as.double(substr(env$dep, 2, nchar(env$dep)-2)))
      ecto <- ectoR_devel(Ww_g = Ww_g,
                          shape = shape,
                          alpha = alpha,
                          M_1 = M_1,
                          postur = postur,
                          pantmax = pantmax,
                          pct_cond = pct_cond,
                          pct_wet = pct_wet,
                          K_sub = K_sub,
                          alpha_sub = alpha_sub,
                          elev = elev,
                          TA = env$TA,
                          TGRD = env$TGRD,
                          TSUBST = env$TGRD,
                          TSKY = env$TSKY,
                          VEL = env$VEL,
                          RH = env$RH,
                          QSOLR = env$QSOLR,
                          Z = zenith)
      hydration <- c(hydration, rehydrate(micro.output, env, 
                                          hyd = hyd, 
                                          hyd.current = hydration[x], 
                                          hyd.rate=hyd.rate, 
                                          x = x))
      TBs <- c(TBs, ecto$TC)
    }
  }
    
  return(list(hydration=hydration, TBs=TBs, act=ACTs, dep=DEPs))
  
}


