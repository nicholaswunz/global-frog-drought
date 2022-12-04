
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


# function to plot activity windows
plot.act <- function(sim.res, micro){
  require(ggplot2)
  dt <- micro$metout[,1]
  h <- rep(1:24, time=micro$ndays)
  act.dt <- data.frame(cbind(dt, h))
  colnames(act.dt) <- c('date','hour')
  act.dt$state <- NA
  for(i in 1:nrow(act.dt)){
    act.dt$state[i] <- ifelse(sim.res$act[i], 1, 0)
    act.dt$state[i] <- ifelse(sim.res$climb[i], 2, act.dt$state[i])
  }
  ggplot(act.dt) +
    geom_raster(aes(x=date, y=hour, fill=as.factor(state))) +
    scale_fill_manual(name = "Activity levels", 
                      values=c("#031c3b","#88db11","#db5711"),
                      labels=c("sheltered (inactive)","aboveground (active)","climbing (active)"))
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


# update hydration state based on evaporative water loss
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



# gain water from the soil based on re-hydration rates
rehydrate <- function(micro.output, env, climb, hyd, hyd.current, hyd.rate=0.01, x){
  if(climb){
    dep <- 'PT0cm'
  } else {
    dep <- paste0('PT',substr(env$dep,2,nchar(env$dep)))
  }
  wpot.soil <- micro.output$soilpot[x,dep]
  if(wpot.soil >= -72.5){
    newhyd <- hyd.current + hyd.rate * ((hyd - hyd.current) / hyd)
  } else {
    newhyd <- hyd.current
  }
  return(newhyd)
}


# function to find frogs position below-ground
seldep <- function(micro.output, Tmax = 30, Tmin = 10, water=FALSE, in.shade=FALSE, x){
  if(!water){
    if(!in.shade){
      soil.temps <- micro.output$soil[x, 4:12] # soil temperatures from 2.5cm to 2m
    } else {
      soil.temps <- micro.output$shadsoil[x, 4:12] # soil temperatures from 2.5cm to 2m
    }
    # selects the shallowest node with temperatures between Tmax and Tmin
    sel.node <- which(soil.temps <= Tmax & soil.temps >= Tmin)[1]
    colnames(soil.temps)[sel.node]
  } else {
    if(!in.shade){
      soil.temps <- micro.output$soil[x, 4:12] # soil temperatures from 2.5cm to 2m
      soil.pots <- micro.output$soilpot[x, 4:12] # soil temperatures from 2.5cm to 2m
    } else {
      soil.temps <- micro.output$shadsoil[x, 4:12] # soil temperatures from 2.5cm to 2m
      soil.pots <- micro.output$shadpot[x, 4:12] # soil temperatures from 2.5cm to 2m
    }
    # selects the shallowest node with water potential >= -72.5
    # and with temperatures between Tmax and Tmin
    sel.node <- which(soil.temps <= Tmax & soil.temps >= Tmin & soil.pots >= -72.5)[1]
    paste0('D', substr(colnames(soil.pots)[sel.node], 3, nchar(colnames(soil.pots)[sel.node])))
  }
}


# set the environment given activity (TRUE = active above-ground; FALSE = below-ground)
environment <- function(micro.output, activity=TRUE, Tmax = 30, Tmin = 10, water=TRUE, in.shade=FALSE, 
                        burrow=TRUE, climb=FALSE, x){
  
  if(!in.shade){
    soil <- micro.output$soil
  } else {
    soil <- micro.output$shadsoil
  }
  
  
  if(activity | !burrow){
    if(climb){ # warning, activity has to be true
      # above-ground
      dep <- "D-150cm"
      if(!in.shade){
        metout <- micro.output$metout
      } else {
        metout <- micro.output$shadmet
        metout$SOLR <- metout$SOLR * 0.1
      }
      
      # get required inputs
      TA <- metout$TAREF[x]
      TGRD <- TA
      TSKY <- metout$TSKYC[x]
      VEL <- metout$VREF[x]
      RH <- metout$RH[x]
      QSOLR <- metout$SOLR[x]
    } else {
      # above-ground
      dep <- "D0cm"
      if(!in.shade){
        metout <- micro.output$metout
      } else {
        metout <- micro.output$shadmet
        metout$SOLR <- metout$SOLR * 0.1
      }
      
      # get required inputs
      TA <- metout$TALOC[x]
      TGRD <- soil$D0cm[x]
      TSKY <- metout$TSKYC[x]
      VEL <- metout$VLOC[x]
      RH <- metout$RHLOC[x]
      QSOLR <- metout$SOLR[x]
    }
    
  } else {
    # below-ground
    dep <- seldep(micro.output, Tmax = Tmax, Tmin = Tmin, water=water, in.shade=in.shade, x)
    
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
sim.ecto <- function(micro, behav = 'nocturnal', Tmax = 30, Tmin = 10, in.shade = FALSE,
                     min.hyd = 70, hyd.rate = 3, water = TRUE, water.act = TRUE,
                     burrow=TRUE, climb=FALSE,
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
  
  hyd <- Ww_g * 70/100 # water content of body in grams
  min.water <- hyd * min.hyd/100
  
  hydration <- hyd # to stored hydration
  TBs <- c() # to store Tbs
  ACTs <- c() # to store activity (TRUE = active; FALSE = underground)
  DEPs <- c() # to store the depth if underground
  climbing <- c()
  
  for(x in 1:(micro$ndays * 24)){ 
    zenith <- micro.output$metout$ZEN[x]
    act <- activity(micro.output, behav=behav, Z=zenith)
    ACTs <- c(ACTs, act)
    climbing <- c(climbing, FALSE)
    
    if(act){
      env <- environment(micro.output, act, Tmax = Tmax, Tmin = Tmin, 
                         water=water, in.shade=in.shade,
                         burrow=burrow, climb=FALSE, x)
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
        if(!climb){
          env <- environment(micro.output, act=FALSE, Tmax = Tmax, Tmin = Tmin, 
                             water=water, in.shade=in.shade,
                             burrow=burrow, climb=climb, x)
          DEPs[x] <- as.double(substr(env$dep, 2, nchar(env$dep)-2))
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
                                              climb = FALSE, 
                                              hyd = hyd, 
                                              hyd.current = hydration[x], 
                                              hyd.rate=hyd.rate, 
                                              x = x))
          TBs <- c(TBs, ecto$TC)
          ACTs[x] <- (suit.therm) & (suit.hydro)
        } else {
          env <- environment(micro.output, act=act, Tmax = Tmax, Tmin = Tmin, 
                             water=water, in.shade=in.shade,
                             burrow=burrow, climb=climb, x)
          DEPs[x] <- as.double(substr(env$dep, 2, nchar(env$dep)-2))
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
            env <- environment(micro.output, act=FALSE, Tmax = Tmax, Tmin = Tmin, 
                               water=water, in.shade=in.shade,
                               burrow=burrow, climb=FALSE, x)
            DEPs[x] <- as.double(substr(env$dep, 2, nchar(env$dep)-2))
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
                                                climb = FALSE, 
                                                hyd = hyd, 
                                                hyd.current = hydration[x], 
                                                hyd.rate=hyd.rate, 
                                                x = x))
            TBs <- c(TBs, ecto$TC)
            ACTs[x] <- (suit.therm) & (suit.hydro)
          } else {
            hydration <- c(hydration, update.hyd(ecto, hydration[x], hyd))
            TBs <- c(TBs, ecto$TC)
            climbing[x] <- climb
          }
          
        }
        
      } else {
        hydration <- c(hydration, update.hyd(ecto, hydration[x], hyd))
        TBs <- c(TBs, ecto$TC)
      }
    } else {
      env <- environment(micro.output, act=act, Tmax = Tmax, Tmin = Tmin, 
                         water=water, in.shade=in.shade,
                         burrow=burrow, climb=FALSE, x)
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
                                          climb = climb,
                                          hyd = hyd, 
                                          hyd.current = hydration[x], 
                                          hyd.rate=hyd.rate, 
                                          x = x))
      TBs <- c(TBs, ecto$TC)
    }
  }
  
  hydration <- hydration * (100/hyd)
  return(list(hydration=hydration, TBs=TBs, act=ACTs, dep=DEPs, climb=climbing))
  
}


