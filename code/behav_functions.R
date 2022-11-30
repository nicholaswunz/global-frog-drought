


# function to define when an animal will be active
activity <- function(behav=c('diurnal', 'nocturnal'), Tmax=30, Tmin=10, Z=Z){
  if(behav == 'diurnal' & Z == 90 |
     behav == 'nocturnal' & Z < 90){
    FALSE
  } else {
    TRUE
  }
}


# set the environment given activity (1 = active above-ground; 0 = below-ground)
environment <- function(activity){
  if(activity){
    # above-ground
    TA <- TAs[x]
    TGRD <- TGRDs[x]
    TSKY <- TSKYs[x]
    VEL <- VELs[x]
    RH <- RHs[x]
    QSOLR <- QSOLRs[x]
  }
  else {
    # below-ground
    TA <- TAs[x]
    TGRD <- TA
    TSKY <- TA
    VEL <- 0.01
    RH <- 99
    QSOLR <- 0.0
    # EMISSB <- 1.0
    # EMISSK <- 1.0
  }
  
  return(TA=TA, TGRD=TGRD, TSKY=TSKY, VEL=VEL, RH=RH, QSOLR=QSOLR)
}




# function to run ectotherm simulations
sim.ecto <- function(micro){
  
  # extract full sun conditions
  metout <- as.data.frame(micro$metout)
  soil <- as.data.frame(micro$soil)
  
  # get required inputs
  TAs <- metout$TALOC
  TGRDs <- soil$D0cm
  TSKYs <- metout$TSKYC
  VELs <- metout$VLOC
  RHs <- metout$RHLOC
  QSOLRs <- metout$SOLR
  Zs <- metout$ZEN
  
  lapply(1:(micro$ndays * 24), function(x){
    act <- activity(behav='diurnal', Tmax=30, Tmin=10, Z=Zs[x])
    
    ecto <- ectoR_devel(Ww_g = 40,
                        shape = 3,
                        alpha = 0.85,
                        M_1 = 0,
                        postur = 0,
                        pantmax = 0,
                        pct_cond = 40,
                        K_sub = 0.1,
                        alpha_sub = (1 - micro$REF),
                        elev = micro$elev,
                        TA = TAs[x],
                        TGRD = TGRDs[x],
                        TSUBST = TGRDs[x],
                        TSKY = TSKYs[x],
                        VEL = VELs[x],
                        RH = RHs[x],
                        QSOLR = QSOLRs[x],
                        Z = Zs[x])
    
  })
}


