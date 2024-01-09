
############################
# Annotation - ** UPDATE THIS **
  # This function is directly based on the life_cycle_model() function in 'Life cycle model.R',
    # which in turn is based on the run.scenarios() function in 'WinterRunStochasticDSM_11_13_2019.R'
  # For life_cycle_model_calib(), made the following changes to the previous life_cycle_model() function
    # Changed inputs to the 16 parameters to be estimated
    # Run the model out for 18 years only (1:18)
    # Model output is now sum of squared differences between data-based and model-based estimates of abundance
      # Specifically, for years
############################

life_cycle_model_calib <-function(x1,x2,x3,x4,x5,x6,x7,x8,
                                  x9,x10,x11,x12,x13,x14,x15,x16){
  
  stochastic=0
  scenario=0
  vary="Nothing"
  pctil=1
  alt="originalDSM"
  
  grow.ic<-0.5; if(sum(vary == "juv.grow.ic")) grow.ic<-grow.ic*pctil[vary == "juv.grow.ic"]
  grow.fp<-1.06; if(sum(vary == "juv.grow.fp")) grow.fp<-grow.fp*pctil[vary == "juv.grow.fp"]
  territory_size<-c(0.05423379,0.14539419,0.48471474,0.00000000); if(sum(vary == "juv.terr.size")) territory_size<-territory_size*pctil[vary == "juv.terr.size"]
  
  spwn.hab.deg=states$spwnDecay
  if(sum(vary == "spawn.hab.decay")){
    this<-log((states$spwnDecay+ 0.000001)/((1-states$spwnDecay)+0.0000001))
    this<-this*pctil[vary == "spawn.hab.decay"]
    spwn.hab.deg<-inv.logit(this)
  }
  
  rear.hab.deg=states$rearDecay
  if(sum(vary == "rear.hab.decay")){
    this<-log((states$rearDecay+ 0.000001)/((1-states$rearDecay)+0.0000001))
    this<-this*pctil[vary == "rear.hab.decay"]
    rear.hab.deg<-inv.logit(this)
  }
  
  #SSE for survival intercepts
    # Previously calibrated
  #vect<-c(-0.6558315, -3.4999845,  1.4933417, -3.0188308,  2.0000003,  0.7999889, -3.5000000, -0.1999996,
  #        -3.4999920, -2.9839253,  3.4999976,  0.6466230,  0.0194795,  0.1000000,  0.3000000,  0.4820249)
  vect<-c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16)
  
  #SSE for habitat scalars
  #vect2<-c(1.1975408, 0.9999999, 0.5000000, 0.9934527, 1.0031454, 1.2284905, 0.9940085, 1.0307768, 1.0000048)
  vect2<-rep(1,9)
    
  IChab.spawn[1,,]<-IChab.spawn[1,,]*vect2[1] # Upper Sac
  
  IChab.fry[1,,]<-IChab.fry[1,,]*vect2[2] # Upper Sac
  IChab.juv[1,,]<-IChab.juv[1,,]*vect2[2] # Upper Sac
  IChab.fry[16,,]<-IChab.fry[16,,]*vect2[3] # Upper-mid Sac (corridor for above)
  IChab.juv[16,,]<-IChab.juv[16,,]*vect2[3] # Upper-mid Sac (corridor for above)
  # Sutter (corridor for above) is changed below
  IChab.fry[21,,]<-IChab.fry[21,,]*vect2[4] # Lower-mid Sac (corridor for above)
  IChab.juv[21,,]<-IChab.juv[21,,]*vect2[4] # Lower-mid Sac (corridor for above)
  # Yolo (corridor for above) is changed below
  IChab.fry[24,,]<-IChab.fry[24,,]*vect2[5] # Lower-mid Sac (corridor for above)
  IChab.juv[24,,]<-IChab.juv[24,,]*vect2[5] # Lower-mid Sac (corridor for above)
  
  IChab.sutter<-IChab.sutter*vect2[6]
  IChab.yolo<-IChab.yolo*vect2[7]
  DLThab[,,1]<-DLThab[,,1]*vect2[8]
  DLThab[,,2]<-DLThab[,,2]*vect2[9]
  
  spwners<-adultEscapeesKnown<-adultEscapeesSimulated<-adultTotalEscapees<-matrix(rep(0,31*30),ncol=30)
  selectedOptimalDecisions<-optimalDecisions<-matrix(rep(NA,31*20),ncol=20)
  juvenileBiomass<-matrix(rep(NA,31*25),ncol=25)
  survivalIncrease<-rep(0,31)
  proportionNatural<-matrix(rep(NA,31*25),ncol=25)
  
  # ** Add other metrics to track here **
  '/
  mnth.names <- c(9:12, 1:5)
  watershed.names <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                           "Bear Creek", "Big Chico Creek", "Butte Creek", 
                           "Clear Creek", "Cottonwood Creek", "Cow Creek", 
                           "Deer Creek", "Elder Creek", "Mill Creek",
                           "Paynes Creek", "Stony Creek", "Thomes Creek", 
                           "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                           "Feather River", "Yuba River", "Lower-mid Sacramento River", 
                           "Yolo Bypass", "American River", "Lower Sacramento River",
                           "Calaveras River", "Cosumnes River", "Mokelumne River",
                           "Merced River", "Stanislaus River", "Tuolumne River", 
                           "San Joaquin River")
  size.names <- c("s","m","l","vl")
  track_adult_en_route <- array(NA, dim=c(20,4,31), # want [,,1] for this - Upper Sacramento River
                                dimnames=list(c(1981:2000),c(1:4), watershed.names)) # years, months (1-4), watersheds
  track_pre.spawn.S <- array(NA, dim=c(20,2,31),
                             dimnames=list(c(1981:2000),c("Spring","Summer"),watershed.names))
  track_p.pulse.leave <- array(NA, dim=c(20,9,31,4),     # want [,,1,] for this, river_surv, and flood_surv - Upper Sacramento River
                               dimnames=list(c(1981:2000),c(9:12,1:5), watershed.names, size.names))# years, months (mnth.names), watersheds, sizes
  track_river_surv <- array(NA, dim=c(20,9,31,4),
                            dimnames=list(c(1981:2000),c(9:12,1:5), watershed.names, size.names)) # years, months (mnth.names), watersheds, sizes
  track_flood_surv <- array(NA, dim=c(20,9,31,4),
                            dimnames=list(c(1981:2000),c(9:12,1:5), watershed.names, size.names)) # years, months (mnth.names), watersheds, sizes
  track_D_juv_surv <- array(NA, dim=c(20,9,2,4),
                            dimnames=list(c(1981:2000),c(9:12,1:5), c("N","S"), size.names)) # years, months (mnth.names), deltas, sizes
  track_UM.Sac.S <- array(NA, dim=c(20,9,4),
                          dimnames=list(c(1981:2000),c(9:12,1:5),size.names)) # years, months (mnth.names), sizes
  track_LM.Sac.S <- array(NA, dim=c(20,9,4),
                          dimnames=list(c(1981:2000),c(9:12,1:5),size.names)) # years, months (mnth.names), sizes
  track_LL.Sac.S <- array(NA, dim=c(20,9,4),
                          dimnames=list(c(1981:2000),c(9:12,1:5),size.names)) # years, months (mnth.names), sizes
  track_newDsurv <- array(NA, dim=c(20,9,4,4), # want [,,1,] - survival through the South Delta for northern origin fish
                          dimnames=list(c(1981:2000),c(9:12,1:5),c("N","O1","O2","O3"),size.names)) # years, months (mnth.names), fish origin, size
  track_Sac.Delt.S <- array(NA, dim=c(20,9,2,4), # want [,,1,] - survival through the North Delta
                            dimnames=list(c(1981:2000),c(9:12,1:5), c("N","S"), size.names)) # years, months (mnth.names), Deltas, size
  track_prop.dlt.entrain <- array(NA, dim=c(20,9),
                                  dimnames=list(c(1981:2000),c(9:12,1:5))) # years, months
  '
  
  #################################################
  # annual dynamics begins here 
  #################################################
  for(yr2 in 1:18){
  #for(yr2 in 1:5){ # use this loop to generate seed #s of spawners
    #  yr2 = 1
    #  yr = 1
    yr<-ifelse(yr2>5,yr2-5,yr2) #use first 5 years to seed the model then run a full 20 years of decisions
    
    # Placeholders
    hatcheryFishByMonth<-spwnersBymonth<-nat.adultByMonth<-init.adultByMonth<-matrix(0,nrow=31,ncol=12)
    up.sac.fsh<-matrix(rep(0,15*4),ncol=4) # first 15 upper sac tribs
    sutter.fsh<-matrix(rep(0,17*4),ncol=4) # plus up-mid sac and sutter - rows 16 and 17
    yolo.fsh<-matrix(rep(0,20*4),ncol=4) # add 3 more rows for Bear, Feather, Yuba
    low.mid.sac.fsh<-matrix(rep(0,20*4),ncol=4) # lower Sac tribs
    low.sac.fsh<-matrix(rep(0,23*4),ncol=4) # add the American River row to lower Sac
    sj.fsh<-matrix(0,ncol=4,nrow=3) # SJ tribs
    juv.leav.shed<-juv.at.chips<-N.delt.fsh<-S.delt.fsh<-matrix(rep(0,31*4),ncol=4) 
    adults.in.ocean<-rep(0,31)
    
    # #natural straying allocation
    # cc.aloc<-c(rep(1,15),0,0,2,2,2,0,0,3,0,rep(0,7))/24
    # oth.aloc<-c(rep(1,15),0,0,1,1,1,0,0,1,0,rep(1,6),0)/25
    
    # average ocean transition month
    T.mo<-(2*(1-stochastic))+(Trans.Mo()*stochastic)
    
    #used triangle distribution to distribute returning spawners (including hatchery fish) across 4 months (January-April)
    returnProps<-c(0.125,0.375,0.375,0.125)
    
    #select which month each fish returns
    for(sitex in 1:31){
      if(known.Adults.seed[sitex,yr]<=1000000000 & stochastic==1){
        randSpawnersBymonth<-as.vector(rmultinom(1,known.Adults.seed[sitex,yr],returnProps))
      } else{randSpawnersBymonth<-round(known.Adults.seed[sitex,yr]*returnProps)}
      spwnersBymonth[sitex,1:4]<-(randSpawnersBymonth*stochastic)+((known.Adults.seed[sitex,yr]*returnProps)*(1-stochastic))
      randSpawnersBymonth<-NULL
    }
    
    #allocate hatchery fish returning from ocean to watersheds
    totHatchReturn<-round(runif(1,355,775))
    if(totHatchReturn<=1000000000 & stochastic==1){
      randHatchReturns<-as.vector(rmultinom(1,totHatchReturn,inps$hatch.alloc))
    } else{randHatchReturns<-round(inps$hatch.alloc*totHatchReturn)}
    hatch.adult<-(randHatchReturns*stochastic)+(inps$hatch.alloc*mean(c(355,775))*(1-stochastic)) # mean number hatchery returns based on CWT reports
    totHatchReturn<-randHatchReturns<-NULL
    
    # use grandTab estimates for first 5 years to get the number of in-ocean adults, 
    # then use the other 15 years for calibration    
    for(mnth in c(1:4)){
      # mnth=1  
      nat.adultByMonth[,mnth]<-spwnersBymonth[,mnth]
      
      # remove natural adults for hatchery operations
      nat.adultByMonth[,mnth]<-rbin2Vectors(nat.adultByMonth[,mnth],(1-inps$prop.nat.remov),stochastic)
      
      # total adult fish on spawning grounds
      init.adultByMonth[,mnth]<-nat.adultByMonth[,mnth]
    }
    nat.adult<-apply(nat.adultByMonth,1,sum)
    init.adult<-apply(init.adultByMonth,1,sum)
    adultTotalEscapees[,yr2]<-init.adult
    adultEscapeesKnown[,yr2]<-nat.adult 
    prop.nat<-c((1-prop.hatch),rep(0,30))
    
    if(yr2>5){
      #select which month each fish returns
      for(sitex in 1:31){
        if(spwners[sitex,yr2]<=1000000000 & stochastic==1){
          randSpawnersBymonth<-as.vector(rmultinom(1,round(spwners[sitex,yr2]),returnProps))
        } else{randSpawnersBymonth<-round(spwners[sitex,yr2]*returnProps)}
        spwnersBymonth[sitex,1:4]<-(randSpawnersBymonth*stochastic)+((spwners[sitex,yr2]*returnProps)*(1-stochastic))
        randSpawnersBymonth<-NULL
      }
      
      for(sitex in 1:31){
        if(hatch.adult[sitex]<=1000000000 & stochastic==1){
          randHatchBymonth<-as.vector(rmultinom(1,round(hatch.adult[sitex]),returnProps))
        } else{randHatchBymonth<-round(hatch.adult[sitex]*returnProps)}
        hatcheryFishByMonth[sitex,1:4]<-(randHatchBymonth*stochastic)+((hatch.adult[sitex]*returnProps)*(1-stochastic))
        randHatchBymonth<-NULL
      }
      
      for(mnth in 1:4){
        # mnth=1
        
        # number of wild fish that stray
        # prop.nat.stray<-Ad.Stray(wild=1,pctQnatl=retQ[,(1+yr)],SCDLT=inps$SCDELT,CrxChn=dlt.gates$days_closed[mnth])
        # stray<-prop.nat.stray*spwnersBymonth[,mnth]
        nat.adultByMonth[,mnth]<-spwnersBymonth[,mnth]#-stray+sum(stray*inps$SCDELT)*cc.aloc+sum(stray*(1-inps$SCDELT))*oth.aloc
        
        # are tisdale or yolo bypasses overtopped?
        BPovrT<-gate.top[mnth,yr,1]*inps$TISD+gate.top[mnth,yr,2]*inps$YOLO
        BPovrT<-ifelse(BPovrT>0,1,0)
        
        # adult en route survival
        adult_en_route<-Adult.S(aveT23=ptemp20mc[,mnth+2],BPovrT,harvest=inps$A.HARV,vect[11],vary=vary,pctil=pctil)
        
        '/# ** track, record adult en route survival **
        track_adult_en_route[yr, mnth,] <- adult_en_route
        '
        
        # estimate adult fish that made it to spawning grounds and remove natural adults for hatchery operations
        nat.adultByMonth[,mnth]<-rbin2Vectors(rbin2Vectors(nat.adultByMonth[,mnth],adult_en_route,stochastic),(1-inps$prop.nat.remov),stochastic)
        hatcheryFishByMonth[,mnth]<-rbin2Vectors(hatcheryFishByMonth[,mnth],adult_en_route,stochastic)
        
        # total adult fish on spawning grounds
        init.adultByMonth[,mnth]<-nat.adultByMonth[,mnth]+hatcheryFishByMonth[,mnth]
        
      }
      nat.adult<-apply(nat.adultByMonth,1,sum)
      init.adult<-apply(init.adultByMonth,1,sum)
      adultTotalEscapees[,yr2]<-init.adult
      adultEscapeesSimulated[,yr2]<-nat.adult 
      prop.nat<-nat.adult/(init.adult+0.0001)
    }
    
    proportionNatural[,yr2]<-prop.nat
    
    # egg to fry survival
    eg2fr<-egg2fry(prop.nat=prop.nat,scour=inps$P.scour.nst,tmp.eff=rep(vect[12],31),vary=vary,pctil=pctil) #different egg temp effect for winter run
    
    #IMPLEMENT DECISIONS
    if(yr2>5 & scenario>0){
      if(scenario==1){
        selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        
        #if adding habitat, add it till the end of days
        for(chosenTrib in 1:31){
          if(is.nan(selectedOptimalDecisions[chosenTrib,yr])==FALSE){
            amount<-(stochastic*((8093.72*ifelse(chosenTrib==1 & yr<11,2,1))*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5))))+((8093.72*ifelse(chosenTrib==1 & yr<11,2,1))*(1-stochastic))
            for(yrs in yr:21){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amount
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amount
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
          }
        }
      } #end scenario 1 if statement
      
      if(scenario==2){
        selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        
        #if adding habitat, add it till the end of days
        for(chosenTrib in 1:31){
          if(is.nan(selectedOptimalDecisions[chosenTrib,yr])==FALSE){
            amount<-(stochastic*((8093.72*ifelse(chosenTrib==1 & yr<11,2,1))*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5))))+((8093.72*ifelse(chosenTrib==1 & yr<11,2,1))*(1-stochastic))
            for(yrs in yr:21){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amount
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amount
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
          }
        }
      } #end scenario 2 if statement
      
      if(scenario==3){
        selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        
        #if adding habitat, add it till the end of days
        for(chosenTrib in 1:31){
          if(is.nan(selectedOptimalDecisions[chosenTrib,yr])==FALSE){
            amount<-(stochastic*((8093.72*ifelse(chosenTrib==1 & yr<11,2,1))*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5))))+((8093.72*ifelse(chosenTrib==1 & yr<11,2,1))*(1-stochastic))
            for(yrs in yr:21){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amount
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amount
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
          }
        }
      } #end scenario 3 if statement
      
      if(scenario==4){
        selectedOptimalDecisions<-matrix(c(3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3),ncol=20,byrow=TRUE)
        
        #if adding habitat, add it till the end of days
        for(chosenTrib in 1:31){
          if(is.nan(selectedOptimalDecisions[chosenTrib,yr])==FALSE){
            amount<-stochastic*((8093.72*ifelse(chosenTrib==21,2,1))*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+(8093.72*ifelse(chosenTrib==21,2,1))*(1-stochastic)
            for(yrs in yr:21){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amount
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amount
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
          }
        }
      } #end scenario 4 if statement
      
      if(scenario==5){
        selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,3,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        
        #if adding habitat, add it till the end of days
        for(chosenTrib in 1:31){
          if(is.nan(selectedOptimalDecisions[chosenTrib,yr])==FALSE){
            amount<-stochastic*((8093.72*ifelse(chosenTrib==1,3,1))*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+(8093.72*ifelse(chosenTrib==1,3,1))*(1-stochastic)
            for(yrs in yr:21){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amount
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amount
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
          }
        }
      } #end scenario 5 if statement
      
      if(scenario==6){
        selectedOptimalDecisionsREAL<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        
        selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        
        #if adding habitat, add it till the end of days
        for(chosenTrib in 1:31){
          if(is.nan(selectedOptimalDecisionsREAL[chosenTrib,yr])==FALSE){
            amount<-stochastic*((8093.72*ifelse(chosenTrib==1,3,1))*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+(8093.72*ifelse(chosenTrib==1,3,1))*(1-stochastic)
            for(yrs in yr:21){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amount
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amount
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
          }
        }
      } #end scenario 6 if statement
      
      if(scenario==7){
        selectedOptimalDecisions<-matrix(c(4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4),ncol=20,byrow=TRUE)
        
        #if adding habitat, add it till the end of days
        for(chosenTrib in 1:31){
          if(is.nan(selectedOptimalDecisions[chosenTrib,yr])==FALSE){
            amount<-stochastic*(8093.72*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+8093.72*(1-stochastic)
            floodPNew[chosenTrib]<-floodPNew[chosenTrib]+amount
          }
        }
      } #end scenario 7 if statement
      
      if(scenario==8){
        Effort<-matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        
        selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,5,5,5,5,5,5,5,5,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,4,3,3,4,4,4,4,3,4,4,3,4,3,4,4,4,3,3,4,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           4,3,3,3,3,4,4,3,3,4,3,4,3,3,4,4,4,3,4,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,4,4,4,4,3,3,4,3,3,3,4,4,3,3,4,4,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        
        chosenLocations<-which(is.na(selectedOptimalDecisions[,yr])==FALSE)
        for(chosenTrib in chosenLocations){
          amountSpawn<-stochastic*(4046.86*Effort[chosenTrib,yr]*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+(4046.86*Effort[chosenTrib,yr])*(1-stochastic)
          amountRear<-stochastic*(8093.72*Effort[chosenTrib,yr]*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+(8093.72*Effort[chosenTrib,yr])*(1-stochastic)
          for(yrs in yr:21){
            # add spawning habitat if added
            if(selectedOptimalDecisions[chosenTrib,yr]==2){
              IChab.spawn[chosenTrib,,yrs]<-IChab.spawn[chosenTrib,,yrs]+amountSpawn
              IChab.spawn[chosenTrib,,yrs]<-pmin(IChab.spawn[chosenTrib,,yrs],maxSpwnHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
            # add in-channel rearing habitat if added
            if(selectedOptimalDecisions[chosenTrib,yr]==3){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amountRear
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amountRear
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
          }
          # add floodplain habitat if added
          if(selectedOptimalDecisions[chosenTrib,yr]==4){
            floodPNew[chosenTrib]<-floodPNew[chosenTrib]+(stochastic*(8093.72*Effort[chosenTrib,yr])*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+8093.72*Effort[chosenTrib,yr]*(1-stochastic)
          }
          #same for increased survival
          if(selectedOptimalDecisions[chosenTrib,yr]==5){
            survivalIncrease[chosenTrib]<-survivalIncrease[chosenTrib]+(stochastic*(0.005*Effort[chosenTrib,yr]*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5))))+0.005*Effort[chosenTrib,yr]*(1-stochastic)
          }
        }
        
      } #end scenario 8 if statement
      
      
      if(scenario==9){
        selectedOptimalDecisions<-matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,
                                           3,3,3,NA,3,3,3,NA,3,3,3,NA,3,3,3,NA,3,3,3,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,NA,5,5,5,NA,5,5,5,NA,5,5,5,NA,5,5,5,NA,
                                           3,2,3,NA,3,3,3,NA,3,3,3,NA,3,3,3,NA,3,3,3,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,3,NA,NA,NA,3,NA,NA,NA,3,NA,NA,NA,3,NA,NA,NA,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           4,4,3,NA,4,3,3,NA,4,3,4,NA,4,4,3,NA,4,3,3,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,NA,NA,NA,5,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,4,NA,NA,NA,4,NA,NA,NA,4,NA,NA,NA,4,NA,NA,NA,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,4,NA,3,4,3,NA,4,4,4,NA,3,4,4,NA,3,3,4,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        
        chosenLocations<-which(is.na(selectedOptimalDecisions[,yr])==FALSE)
        for(chosenTrib in chosenLocations){
          amountSpawn<-stochastic*(4046.86*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+4046.86*(1-stochastic)
          amountRear<-stochastic*(8093.72*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+8093.72*(1-stochastic)
          for(yrs in yr:21){
            # add spawning habitat if added
            if(selectedOptimalDecisions[chosenTrib,yr]==2){
              IChab.spawn[chosenTrib,,yrs]<-IChab.spawn[chosenTrib,,yrs]+amountSpawn
              IChab.spawn[chosenTrib,,yrs]<-pmin(IChab.spawn[chosenTrib,,yrs],maxSpwnHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
            # add in-channel rearing habitat if added
            if(selectedOptimalDecisions[chosenTrib,yr]==3){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amountRear
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amountRear
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
          }
          # add floodplain habitat if added
          if(selectedOptimalDecisions[chosenTrib,yr]==4){
            floodPNew[chosenTrib]<-floodPNew[chosenTrib]+stochastic*(8093.72*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+8093.72*(1-stochastic)
          } 
          #same for increased survival
          if(selectedOptimalDecisions[chosenTrib,yr]==5){
            survivalIncrease[chosenTrib]<-survivalIncrease[chosenTrib]+stochastic*(0.005*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+0.005*(1-stochastic)
          }
        }
        
      } #end scenario 9 if statement
      
      
      if(scenario==10){
        selectedOptimalDecisions<-matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,
                                           NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,
                                           3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,NA,3,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,NA,NA,5,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        
        #if adding habitat, add it till the end of days
        chosenLocations<-which(is.na(selectedOptimalDecisions[,yr])==FALSE)
        for(chosenTrib in chosenLocations){
          amountRear<-stochastic*(8093.72*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+8093.72*(1-stochastic)
          for(yrs in yr:21){
            # add in-channel rearing habitat if added
            if(selectedOptimalDecisions[chosenTrib,yr]==3){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amountRear
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amountRear
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              
            }
          }
          
          # add floodplain habitat if added
          if(selectedOptimalDecisions[chosenTrib,yr]==4){
            floodPNew[chosenTrib]<-floodPNew[chosenTrib]+stochastic*(8093.72*ifelse(chosenTrib==16,3,1))*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5))+(8093.72*ifelse(chosenTrib==16,3,1))*(1-stochastic)
          } 
          #same for increased survival
          if(selectedOptimalDecisions[chosenTrib,yr]==5){
            survivalIncrease[chosenTrib]<-survivalIncrease[chosenTrib]+stochastic*(0.005*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+0.005*(1-stochastic)
          }
        }
      } #end scenario 10.2 if statement
      
      if(scenario==11){
        selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,5,NA,NA,NA,5,5,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,5,5,5,5,5,5,5,5,5,NA,5,NA,5,NA,NA,5,5,
                                           NA,NA,3,NA,NA,NA,3,3,3,3,3,3,NA,3,NA,3,NA,3,3,NA,
                                           2,2,NA,2,2,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,5,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           4,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,5,NA,5,NA,5,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           4,4,NA,NA,4,NA,NA,NA,NA,NA,NA,NA,4,NA,NA,4,4,4,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,3,3,NA,3,NA,3,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,4,4,NA,NA,4,4,4,4,4,4,NA,4,NA,NA,NA,NA,4,4,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,NA,3,NA,3,NA,NA,3,NA,3,NA,3,NA,3,3,3,NA,3,3,
                                           NA,NA,NA,NA,NA,4,NA,NA,NA,NA,NA,NA,NA,NA,4,NA,NA,NA,NA,NA,
                                           5,NA,NA,NA,NA,NA,NA,5,NA,5,NA,5,NA,5,NA,NA,5,5,5,NA,
                                           NA,NA,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,5,NA,NA,
                                           NA,NA,NA,NA,2,NA,NA,NA,NA,NA,NA,NA,NA,2,NA,NA,NA,NA,NA,NA,
                                           NA,5,5,4,4,NA,NA,NA,NA,NA,NA,NA,5,NA,5,NA,NA,NA,NA,5,
                                           NA,NA,NA,NA,NA,3,3,NA,3,NA,3,NA,NA,NA,NA,3,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        
        #if adding habitat, add it till the end of days
        chosenLocations<-which(is.na(selectedOptimalDecisions[,yr])==FALSE)
        for(chosenTrib in chosenLocations){
          amountSpawn<-stochastic*(4046.86*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+4046.86*(1-stochastic)
          amountRear<-stochastic*(8093.72*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+8093.72*(1-stochastic)
          for(yrs in yr:21){
            # add spawning habitat if added
            if(selectedOptimalDecisions[chosenTrib,yr]==2){
              IChab.spawn[chosenTrib,,yrs]<-IChab.spawn[chosenTrib,,yrs]+amountSpawn
              IChab.spawn[chosenTrib,,yrs]<-pmin(IChab.spawn[chosenTrib,,yrs],maxSpwnHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
            # add in-channel rearing habitat if added
            if(selectedOptimalDecisions[chosenTrib,yr]==3){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amountRear
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amountRear
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
          }
          # add floodplain habitat if added
          if(selectedOptimalDecisions[chosenTrib,yr]==4){
            floodPNew[chosenTrib]<-floodPNew[chosenTrib]+stochastic*(8093.72*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+8093.72*(1-stochastic)
          }
          #same for increased survival
          if(selectedOptimalDecisions[chosenTrib,yr]==5){
            survivalIncrease[chosenTrib]<-survivalIncrease[chosenTrib]+stochastic*(0.005*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+0.005*(1-stochastic)
          }
        }
      } #end scenario 11 if statement
      
      if(scenario==12){
        selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           3,3,3,3,4,4,4,4,4,3,3,4,4,4,3,4,4,3,4,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           5,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        chosenLocations<-which(is.na(selectedOptimalDecisions[,yr])==FALSE)
        for(chosenTrib in chosenLocations){
          amountSpawn<-stochastic*(4046.86*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+4046.86*(1-stochastic)
          amountRear<-stochastic*(8093.72*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+8093.72*(1-stochastic)
          for(yrs in yr:21){
            # add spawning habitat if added
            if(selectedOptimalDecisions[chosenTrib,yr]==2){
              IChab.spawn[chosenTrib,,yrs]<-IChab.spawn[chosenTrib,,yrs]+amountSpawn
              IChab.spawn[chosenTrib,,yrs]<-pmin(IChab.spawn[chosenTrib,,yrs],maxSpwnHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
            # add in-channel rearing habitat if added
            if(selectedOptimalDecisions[chosenTrib,yr]==3){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amountRear
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amountRear
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
          }
          # add floodplain habitat if added
          if(selectedOptimalDecisions[chosenTrib,yr]==4){
            floodPNew[chosenTrib]<-floodPNew[chosenTrib]+stochastic*(8093.72*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+8093.72*(1-stochastic)
          }
          #same for increased survival
          if(selectedOptimalDecisions[chosenTrib,yr]==5){
            survivalIncrease[chosenTrib]<-survivalIncrease[chosenTrib]+stochastic*(0.005*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+0.005*(1-stochastic)
          }
        }
        
      } #end scenario 12 if statement
      
      if(scenario==13){
        selectedOptimalDecisions<-matrix(c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           4,3,3,3,4,3,3,4,3,4,3,4,4,3,4,4,4,3,4,3,
                                           5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),ncol=20,byrow=TRUE)
        chosenLocations<-which(is.na(selectedOptimalDecisions[,yr])==FALSE)
        for(chosenTrib in chosenLocations){
          amountSpawn<-stochastic*(4046.86*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+4046.86*(1-stochastic)
          amountRear<-stochastic*(8093.72*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+8093.72*(1-stochastic)
          for(yrs in yr:21){
            # add spawning habitat if added
            if(selectedOptimalDecisions[chosenTrib,yr]==2){
              IChab.spawn[chosenTrib,,yrs]<-IChab.spawn[chosenTrib,,yrs]+amountSpawn
              IChab.spawn[chosenTrib,,yrs]<-pmin(IChab.spawn[chosenTrib,,yrs],maxSpwnHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
            # add in-channel rearing habitat if added
            if(selectedOptimalDecisions[chosenTrib,yr]==3){
              IChab.fry[chosenTrib,,yrs]<-IChab.fry[chosenTrib,,yrs]+amountRear
              IChab.juv[chosenTrib,,yrs]<-IChab.juv[chosenTrib,,yrs]+amountRear
              
              IChab.fry[chosenTrib,,yrs]<-pmin(IChab.fry[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
              IChab.juv[chosenTrib,,yrs]<-pmin(IChab.juv[chosenTrib,,yrs],maxRearHab$max_suit_sqm[chosenTrib])#can't go over max habitat possible
            }
          }
          # add floodplain habitat if added
          if(selectedOptimalDecisions[chosenTrib,yr]==4){
            floodPNew[chosenTrib]<-floodPNew[chosenTrib]+stochastic*(8093.72*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+8093.72*(1-stochastic)
          }
          #same for increased survival
          if(selectedOptimalDecisions[chosenTrib,yr]==5){
            survivalIncrease[chosenTrib]<-survivalIncrease[chosenTrib]+stochastic*(0.005*(min(max(rgamma(1,44.44444,scale=0.02250),0.5),1.5)))+0.005*(1-stochastic)
          }
        }
      } #end scenario 13 if statement
      
      
      
    }#end year if statement
    
    # used the minimum amount of habitat from January to April
    spawn_hab<-apply(IChab.spawn[,1:4,yr],1,min)
    
    # prespawn survival using the average number of degree days per site
    avg.DegDay<-(init.adultByMonth[,1]*(DegDay[,1,yr]+DegDay[,2,yr]+DegDay[,3,yr]+DegDay[,4,yr])+ #if you returned in January you get 4 months of degree days
                   init.adultByMonth[,2]*(DegDay[,2,yr]+DegDay[,3,yr]+DegDay[,4,yr])+             #if you returned in February you get 3 months of degree days
                   init.adultByMonth[,3]*(DegDay[,3,yr]+DegDay[,4,yr])+                           #if you returned in March you get 2 months of degree days
                   init.adultByMonth[,4]*(DegDay[,4,yr]))/(init.adult + 0.0001)                   #if you returned in April you get 1 month of degree days
    
    pre.spawn.S<-Adult.PSS(DegDay=avg.DegDay,vary=vary,pctil=pctil)
    
    '/# ** track, record spring pre-spawn survival
    if(yr2>5){track_pre.spawn.S[yr,1,] = pre.spawn.S} 
    '
    
    #how many made it to May?
    init.adult<-rbin2Vectors(init.adult,pre.spawn.S,stochastic)
    
    #use triangle distribution to get average degree days during spawning period
    spawntime<-c(0.2222222,0.5555556,0.2222222)
    
    if(init.adult[1]<=1000000000 & stochastic==1){
      ini.spawnersDDs<-as.vector(rmultinom(1,round(init.adult[1]),spawntime))
    } else{ini.spawnersDDs<-round(init.adult[1]*spawntime)}
    ini.spawnersDDs<-(ini.spawnersDDs*stochastic)+((init.adult[1]*spawntime)*(1-stochastic))
    
    avg.DegDay<-(c(ini.spawnersDDs[1],rep(0,30))*(DegDay[,5,yr])+ 
                   c(ini.spawnersDDs[2],rep(0,30))*(DegDay[,5,yr]+DegDay[,6,yr])+            
                   c(ini.spawnersDDs[3],rep(0,30))*(DegDay[,5,yr]+DegDay[,6,yr]+DegDay[,7,yr]))/(init.adult + 0.0001) 
    pre.spawn.S<-Adult.PSS(DegDay=avg.DegDay,vary=vary,pctil=pctil)

    '/# ** track, record summer pre-spawn survival **
    if(yr2>5){track_pre.spawn.S[yr,2,] = pre.spawn.S} 
    '  
        
    # simulate spawning to get a number of newly hatched fry
    fry<-spawnfun(escapement=init.adult,
                  s_adult_inriver=pre.spawn.S,
                  sex_ratio=0.5,
                  spawn_hab=spawn_hab,
                  redd_size=9.29,
                  prob_scour=inps$P.scour.nst,
                  s_egg_to_fry=eg2fr,
                  fecund=5522,
                  stoch=stochastic,vary=vary,pctil=pctil)
    juvs<-fry
    
    floodPActivation<-matrix(0,ncol=12,nrow=31)
    if(stochastic==1){
      for(floodTrib in 1:31){
        if(floodPNew[floodTrib]>0){ # did you add FP habitat?
          if(rbinom(1,1,0.67)==1){ # is it activated this year?
            fld.mnth<-round(runif(1,0.51,3.49)) # what month is it activated this year?
            floodPActivation[floodTrib,c(fld.mnth,fld.mnth+1)]<-floodPNew[floodTrib]
          }
        }
      }
    }else{
      for(floodTrib in 1:31){
        if(floodPNew[floodTrib]>0){
          floodPActivation[floodTrib,c(2,3)]<-floodPNew[floodTrib]*0.67
        }
      }
    }
    
    #simulate monthly juvenile dynamics (January - August)
    for(mnth in c(9:12,1:5)){
      #mnth = 9
      # estimate growth
      growthMatrices<-Hab.growth(daily.rates=c(grow.ic,grow.fp),wks.fld=fp.weeks[,mnth,ifelse(mnth>8,yr,yr+1)])
      trans_mat_river<-growthMatrices$T.mtx.ic
      trans_mat_flood<-growthMatrices$T.mtx.fp
      
      # set habitat amounts
      FP.hab<-floodP[,mnth,ifelse(mnth>8,yr,yr+1)]+floodPActivation[,mnth]
      FP.sutter<-floodp.sutter[mnth,ifelse(mnth>8,yr,yr+1)]
      FP.yolo<-floodp.yolo[mnth,ifelse(mnth>8,yr,yr+1)]
      if(mnth>8) RI.hab<-IChab.fry[,mnth,ifelse(mnth>8,yr,yr+1)] else RI.hab<-IChab.juv[,mnth,ifelse(mnth>8,yr,yr+1)]
      
      RI.hab.sutter<-IChab.sutter[mnth,ifelse(mnth>8,yr,yr+1)]
      RI.hab.yolo<-IChab.yolo[mnth,ifelse(mnth>8,yr,yr+1)]
      
      NDLThab<-DLThab[mnth,ifelse(mnth>8,yr,yr+1),1]
      SDLThab<-DLThab[mnth,ifelse(mnth>8,yr,yr+1),2]
      
      # set proportion fish stranding
      if(mnth>8) ws.strand<-inps$P.strand.early else ws.strand<-inps$P.strand.late
      ws.strand<-(rbinom(31,1,ws.strand)*stochastic)+(ws.strand*(1-stochastic))
      
      # estimate probability leaving as function of pulse flow
      pulse.flozz<-prop.pulse[,mnth+1]/100              
      p.pulse.leave<-Juv.PLS.M(pulse.flozz,vary=vary,pctil=pctil)
      
      '/# ** Track, store p.pulse.leave values **
      if(yr2>5){track_p.pulse.leave[yr, which(mnth.names==mnth),,] = p.pulse.leave} 
      '
      
      # monthly temperature
      aveT20<-(rbinom(31,1,inv.logit(-14.32252+0.72102*juv.tmp[,mnth,ifelse(mnth>8,yr,yr+1)]))*stochastic)+(inv.logit(-14.32252+0.72102*juv.tmp[,mnth,ifelse(mnth>8,yr,yr+1)])*(1-stochastic))
      maxT25<-(rbinom(31,1,inv.logit(-23.1766+1.4566*juv.tmp[,mnth,ifelse(mnth>8,yr,yr+1)]))*stochastic)+(inv.logit(-23.1766+1.4566*juv.tmp[,mnth,ifelse(mnth>8,yr,yr+1)])*(1-stochastic))
      aveT20D<-(rbinom(2,1,inv.logit(-18.30017+0.96991*juv.tmp.dlt[mnth,ifelse(mnth>8,yr,yr+1),]))*stochastic)+(inv.logit(-18.30017+0.96991*juv.tmp.dlt[mnth,ifelse(mnth>8,yr,yr+1),])*(1-stochastic))
      maxT25D<-(rbinom(2,1,inv.logit(-157.537+6.998*juv.tmp.dlt[mnth,ifelse(mnth>8,yr,yr+1),]))*stochastic)+(inv.logit(-157.537+6.998*juv.tmp.dlt[mnth,ifelse(mnth>8,yr,yr+1),])*(1-stochastic))
      
      # proportion and total water diverted 
      prop.div<-p.diver[,mnth,ifelse(mnth>8,yr,yr+1)]
      tot.div<-t.diver[,mnth,ifelse(mnth>8,yr,yr+1)]
      dlt.prop.div<-dlt.divers[mnth,ifelse(mnth>8,yr,yr+1),]
      dlt.tot.div<-dlt.divers.tot[mnth,ifelse(mnth>8,yr,yr+1),]
      
      # predator information
      high.pred<-(rbinom(31,1,inps$High.pred)*stochastic)+(inps$High.pred*(1-stochastic))
      C.points<-inps$contact
      D.points<-Dlt.inp$contct.pts
      Dlt.High.pred<-Dlt.inp$High.pred
      
      # estimate juvenile rearing survival
      river_surv<-Juv.IC.S(maxT25=maxT25,aveT20=aveT20,high.pred=high.pred, 
                           no.con.pts=C.points,prop.div=prop.div,tot.div=tot.div,strand=ws.strand,vect[1],vect[13],vect[14],vect[15],vary=vary,pctil=pctil)
      flood_surv<-Juv.FP.S(maxT25=maxT25,aveT20=aveT20,high.pred=high.pred,no.con.pts=C.points,
                           prop.div=prop.div,tot.div=tot.div,strand=ws.strand,wks.fld=fp.weeks[,mnth,ifelse(mnth>8,yr,yr+1)],vect[1],vect[13],vect[14],vect[15],vary=vary,pctil=pctil)
      
      river_surv<-river_surv+cbind(survivalIncrease,survivalIncrease,survivalIncrease,survivalIncrease)
      flood_surv<-flood_surv+cbind(survivalIncrease,survivalIncrease,survivalIncrease,survivalIncrease)
      river_surv<-ifelse(river_surv>1,1,river_surv)
      flood_surv<-ifelse(flood_surv>1,1,flood_surv)
      
      '/# ** track tributary juvenile rearing survival **
      if(yr2>5){track_river_surv[yr, which(mnth.names==mnth),,] = river_surv}
      if(yr2>5){track_flood_surv[yr, which(mnth.names==mnth),,] = flood_surv}
      '
      
      BP_surv<-Juv.BYP.S(maxT25=maxT25[22],aveT20=aveT20[22],high.pred=0,vect[2],vary=vary,pctil=pctil)
      Sut.S<-Yolo.S<-Juv.BYP.S(maxT25=maxT25[22],aveT20=aveT20[22],high.pred=0,vect[2],vary=vary,pctil=pctil)^0.5
      
      D_juv_surv<-Juv.DLT.S(maxT25=maxT25D,aveT20=aveT20D,high.pred=Dlt.High.pred, 
                            no.con.pts=D.points,prop.div=dlt.prop.div,tot.div=dlt.tot.div,vect[3],vect[13],vect[16],vary=vary,pctil=pctil)
      
      '/# ** track delta juvenile rearing survival **
      if(yr2>5){track_D_juv_surv[yr, which(mnth.names==mnth),,] = D_juv_surv}
      '
      
      # estimate juvenile migratory survival
      newDsurv<-DeltaS(DCC_open=0,hor_barr=0,Q_free[mnth,ifelse(mnth>8,yr,yr+1)],Q_vern[mnth,ifelse(mnth>8,yr,yr+1)],
                       Q_stck[mnth,ifelse(mnth>8,yr,yr+1)],Temp_vern[mnth,ifelse(mnth>8,yr,yr+1)],
                       Temp_pp[mnth,ifelse(mnth>8,yr,yr+1)],CVP_exp[mnth,ifelse(mnth>8,yr,yr+1)],
                       SWP_exp[mnth,ifelse(mnth>8,yr,yr+1)],Trap_trans,vary=vary,pctil=pctil)
      SacFlo<-upSacQ[mnth,ifelse(mnth>8,yr,yr+1)]
      SJ.S<-Juv.SJ.mig.S(vect[4],vary=vary,pctil=pctil)
      UM.Sac.S<-Juv.OUTM.S(Q.cms=SacFlo,prop.div=prop.div[16],tot.div=tot.div[16],aveT=juv.tmp[16,mnth,ifelse(mnth>8,yr,yr+1)],vect[5],vect[6],vect[14],vect[15],vary=vary,pctil=pctil)^0.5
      LM.Sac.S<-Juv.OUTM.S(Q.cms=SacFlo,prop.div=prop.div[21],tot.div=tot.div[21],aveT=juv.tmp[21,mnth,ifelse(mnth>8,yr,yr+1)],vect[5],vect[6],vect[14],vect[15],vary=vary,pctil=pctil)^0.5
      LL.Sac.S<-Juv.OUTM.S(Q.cms=SacFlo,prop.div=prop.div[24],tot.div=tot.div[24],aveT=juv.tmp[24,mnth,ifelse(mnth>8,yr,yr+1)],vect[5],vect[6],vect[14],vect[15],vary=vary,pctil=pctil)^0.5
      
      Sac.Delt.S<-JuvD.OUTM.S(Q.cms=Dlt.inf[mnth,ifelse(mnth>8,yr,yr+1),],pctdiv=dlt.prop.div*100,aveT=juv.tmp.dlt[mnth,ifelse(mnth>8,yr,yr+1),],vect[7],vect[8],vect[9],vary=vary,pctil=pctil)
      Bay.S<-mean(c(0.43,0.46,0.26,0.25,0.39)) # Chipps island to bay 
      
      '/# ** track tributary and delta migratory survival **
      if(yr2>5){
        track_UM.Sac.S[yr, which(mnth.names==mnth),] <- UM.Sac.S
        track_LM.Sac.S[yr, which(mnth.names==mnth),] <- LM.Sac.S
        track_LL.Sac.S[yr, which(mnth.names==mnth),] <- LL.Sac.S
        track_newDsurv[yr, which(mnth.names==mnth),,] <- newDsurv
        track_Sac.Delt.S[yr, which(mnth.names==mnth),,] <- Sac.Delt.S
      }
      '  
        
      #simulate fish moving through the system
      # UPPER SACRAMENTO WATERSHEDS
      if(mnth>8|mnth<5){ 
        USc.sheds<-fill.trib.func(juvs=juvs[1:15,],flood_hab=FP.hab[1:15],juv_hab=RI.hab[1:15],territory_size,
                                  stoch=stochastic)
        left<-rbin2Matrices(USc.sheds$river,p.pulse.leave[1:15,],stochastic)
        USc.sheds$river<-(USc.sheds$river-left)*stochastic+(USc.sheds$river*(1-p.pulse.leave[1:15,]))*(1-stochastic)
        USc.sheds$migr<-USc.sheds$migr+left; left<-NULL
      } else USc.sheds<-list(migr=juvs[1:15,])
      
      # keep track of fish leaving each watershed for SIT metric
      juv.leav.shed[1:15,]<-juv.leav.shed[1:15,]+USc.sheds$migr
      
      # migratory
      surv.mig<-USc.sheds$migr
      
      # MIDDLE SACRAMENTO AND SUTTER BP
      # add migrants to existing mid sac mainstem fish
      up.sac.fsh<-up.sac.fsh+surv.mig
      
      # detour fish to sutter and add sutter detoured fish to existing sutter fish
      detoured.fish<-rbinMatObject(up.sac.fsh,prop.Q.bypasses[mnth,ifelse(mnth>8,yr,yr+1),1],stochastic)
      sutter.fsh<-sutter.fsh+rbind(detoured.fish,matrix(0,ncol=4,nrow=2))  
      # remove detoured fish
      up.sac.fsh<-(up.sac.fsh-detoured.fish)*stochastic+(up.sac.fsh*(1-prop.Q.bypasses[mnth,ifelse(mnth>8,yr,yr+1),1]))*(1-stochastic); detoured.fish<-NULL
      
      # fill up sutter habitats no effect of pulsed flows here
      if(mnth>8|mnth<5){
        sutter.fsh<-fill.Umain.func(juvs=sutter.fsh,flood_hab=FP.sutter,juv_hab=RI.hab.sutter,territory_size,
                                    stoch=stochastic)
      } else sutter.fsh<-list(migr=sutter.fsh)
      
      # fill up upper mainstem, but in river fish can leave due to pulses
      if(mnth>8|mnth<5) {
        up.sac.fsh<-fill.Umain.func(juvs=up.sac.fsh,flood_hab=FP.hab[16],juv_hab=RI.hab[16],territory_size,
                                    stoch=stochastic)
        # pulsed flows
        left<-rbinMatVector(up.sac.fsh$Umain.river,p.pulse.leave[16,],stochastic)
        up.sac.fsh$Umain.river<-(up.sac.fsh$Umain.river-left)*stochastic+t((1-p.pulse.leave[16,])*t(up.sac.fsh$Umain.river))*(1-stochastic)
        up.sac.fsh$migr<-up.sac.fsh$migr+left; left<-NULL; 
      } else up.sac.fsh<-list(migr=up.sac.fsh)
      
      # fish leaving Sutter and Mid-Sacramento (survival migratory placeholder)
      surv.mig<-(rbind(rbin2MatSpec(up.sac.fsh$migr,UM.Sac.S,stochastic),matrix(0,ncol=4,nrow=2))+rbin2MatSpec(sutter.fsh$migr,Sut.S,stochastic))*stochastic+
        (rbind(up.sac.fsh$migr%z%UM.Sac.S,matrix(0,ncol=4,nrow=2))+sutter.fsh$migr%z%Sut.S)*(1-stochastic)
      
      # LOWER-MID SACRAMENTO WATERSHEDS
      # fill upstream watersheds first and send fish out if size 4 or not enough habitat
      if(mnth>8|mnth<5){ 
        LSc.sheds<-fill.trib.func(juvs=juvs[18:20,],flood_hab=FP.hab[18:20],juv_hab=RI.hab[18:20],territory_size,
                                  stoch=stochastic)
        left<-rbin2Matrices(LSc.sheds$river,p.pulse.leave[18:20,],stochastic)
        LSc.sheds$river<-(LSc.sheds$river-left)*stochastic+(LSc.sheds$river*(1-p.pulse.leave[18:20,]))*(1-stochastic)
        LSc.sheds$migr<-LSc.sheds$migr+left; left<-NULL; 
      } else LSc.sheds<-list(migr=juvs[18:20,])
      juv.leav.shed[18:20,]<-juv.leav.shed[18:20,]+LSc.sheds$migr
      
      # add lower mid sac trab migrants to migrants from upper sac tribs
      surv.mig<-rbind(surv.mig,LSc.sheds$migr)
      
      # migrating fish added to existing lower mid sacamento fish 
      low.mid.sac.fsh<-low.mid.sac.fsh+surv.mig
      
      if(alt != "originalDSM"){ # If we're running the model with updated inputs
        # detour fish to Yolo and add Yolo detoured fish to existing Yolo fish 
        detoured.fish<-rbinMatObject(low.mid.sac.fsh,prop.Q.bypasses[mnth,ifelse(mnth>8,yr,yr+1),2],stochastic)
        yolo.fsh<-yolo.fsh+detoured.fish
        
        # remove detoured fish
        low.mid.sac.fsh<-(low.mid.sac.fsh-detoured.fish)*(stochastic)+(low.mid.sac.fsh*(1-prop.Q.bypasses[mnth,ifelse(mnth>8,yr,yr+1),2]))*(1-stochastic); detoured.fish<-NULL
      } else {
        # detour fish to Yolo and add Yolo detoured fish to existing Yolo fish 
        detoured.fish<-rbinMatObject(low.mid.sac.fsh,prop.Q.bypasses[mnth,ifelse(mnth>8,yr,yr+1),5],stochastic)
        yolo.fsh<-yolo.fsh+detoured.fish
        
        # remove detoured fish
        low.mid.sac.fsh<-(low.mid.sac.fsh-detoured.fish)*(stochastic)+(low.mid.sac.fsh*(1-prop.Q.bypasses[mnth,ifelse(mnth>8,yr,yr+1),5]))*(1-stochastic); detoured.fish<-NULL
      }
      
      # fill up yolo habitats no effect of pulsed flows here
      if(mnth>8|mnth<5){
        yolo.fsh<-fill.Umain.func(juvs=yolo.fsh,flood_hab=FP.yolo,juv_hab=RI.hab.yolo,territory_size,
                                  stoch=stochastic)
      } else yolo.fsh<-list(migr=yolo.fsh)
      
      # assign LMSF fish to open habitat, in none leave or pulse flow in river leave. 
      if(mnth>8|mnth<5){ 
        low.mid.sac.fsh<-fill.Lmain.func(juvs=low.mid.sac.fsh,flood_hab=FP.hab[21],juv_hab=RI.hab[21],territory_size,
                                         stoch=stochastic) 
        left<-rbinMatVector(low.mid.sac.fsh$Lmain.river,p.pulse.leave[21,],stochastic)
        low.mid.sac.fsh$Lmain.river<-(low.mid.sac.fsh$Lmain.river-left)*stochastic+t((1-p.pulse.leave[21,])*t(low.mid.sac.fsh$Lmain.river))*(1-stochastic)
        low.mid.sac.fsh$migr<-low.mid.sac.fsh$migr+left; left<-NULL
      } else low.mid.sac.fsh<-list(migr=low.mid.sac.fsh)
      
      # Note these fish passed through and must pay survival cost through - survival migratory placeholder
      surv.mig<-rbind((rbin2MatSpec(low.mid.sac.fsh$migr,LM.Sac.S,stochastic)*stochastic+(low.mid.sac.fsh$migr%z%LM.Sac.S)*(1-stochastic)),matrix(0,ncol=4,nrow=3))
      
      # LOWER SACRAMENTO MAINSTEM - American River fish can use too
      # fill american R watershed first and send fish out if size 4 or not enough habitat
      if(mnth>8|mnth<5){ 
        AMR.sheds<-fill.AMR.func(juvs=juvs[23,],flood_hab=FP.hab[23],juv_hab=RI.hab[23],territory_size,
                                 stoch=stochastic)
        left<-rbinMatVector(AMR.sheds$river,p.pulse.leave[23,],stochastic)
        AMR.sheds$river<-(AMR.sheds$river-left)*stochastic+t((1-p.pulse.leave[23,])*t(AMR.sheds$river))*(1-stochastic)
        AMR.sheds$migr<-AMR.sheds$migr+left; left<-NULL; junk<-NULL
      } else AMR.sheds<-list(migr=juvs[23,])
      juv.leav.shed[23,]<-juv.leav.shed[23,]+AMR.sheds$migr
      
      # add lower mid sac trib migrants to migrants from upper sac tribs
      surv.mig[23,]<-surv.mig[23,]+AMR.sheds$migr
      
      # migrating fish added to existing lower sacamento fish 
      low.sac.fsh<-low.sac.fsh+surv.mig
      
      # assign LSF fish to open habitat, in none leave or pulse flow in river leave. 
      if(mnth>8|mnth<5){ 
        low.sac.fsh<-fill.Lmain.func(juvs=low.sac.fsh,flood_hab=FP.hab[24],juv_hab=RI.hab[24],territory_size,
                                     stoch=stochastic) 
        left<-rbinMatVector(low.sac.fsh$Lmain.river,p.pulse.leave[24,],stochastic)
        low.sac.fsh$Lmain.river<-(low.sac.fsh$Lmain.river-left)*stochastic+t((1-p.pulse.leave[24,])*t(low.sac.fsh$Lmain.river))*(1-stochastic)
        low.sac.fsh$migr<-low.sac.fsh$migr + left; left<-NULL
      } else low.sac.fsh<-list(migr=low.sac.fsh)
      
      # DELTA WATERSHEDS
      # fill upstream watersheds first and send fish out if size 4 or not enough habitat
      if(mnth>8|mnth<5){
        DL.sheds<-fill.trib.func(juvs=juvs[25:27,],flood_hab=FP.hab[25:27],juv_hab=RI.hab[25:27],territory_size,
                                 stoch=stochastic) 
        left<-rbin2Matrices(DL.sheds$river,p.pulse.leave[25:27,],stochastic)
        DL.sheds$river<-(DL.sheds$river-left)*stochastic+(DL.sheds$river*(1-p.pulse.leave[25:27,]))*(1-stochastic)
        DL.sheds$migr<-DL.sheds$migr+left; left<-NULL; junk<-NULL
      } else DL.sheds<-list(migr=juvs[25:27,])
      juv.leav.shed[25:27,]<-juv.leav.shed[25:27,]+DL.sheds$migr
      
      # SAN JOAQUIN WATERSHEDS
      if(mnth>8|mnth<5){
        SJ.sheds<-fill.trib.func(juvs=juvs[28:30,],flood_hab=FP.hab[28:30],juv_hab=RI.hab[28:30],territory_size,
                                 stoch=stochastic) 
        left<-rbin2Matrices(SJ.sheds$river,p.pulse.leave[28:30,],stochastic)
        SJ.sheds$river<-(SJ.sheds$river-left)*stochastic+(SJ.sheds$river*(1-p.pulse.leave[28:30,]))*(1-stochastic)
        SJ.sheds$migr<-SJ.sheds$migr+left; left<-NULL
      } else SJ.sheds<-list(migr=juvs[28:30,])
      juv.leav.shed[28:30,]<-juv.leav.shed[28:30,]+SJ.sheds$migr
      
      # add migrants to existing SJ mainstem fish
      sj.fsh<-sj.fsh+SJ.sheds$migr 
      
      # assign SJ fish to open habitat, in none leave or pulse flow in river leave. 
      if(mnth>8|mnth<5){
        sj.fsh<-fill.Lmain.func(juvs=sj.fsh,flood_hab=FP.hab[31],juv_hab=RI.hab[31],territory_size,
                                stoch=stochastic) 
        left<-rbinMatVector(sj.fsh$Lmain.river,p.pulse.leave[31,],stochastic)
        sj.fsh$Lmain.river<-(sj.fsh$Lmain.river-left)*stochastic+t((1-p.pulse.leave[31,])*t(sj.fsh$Lmain.river))*(1-stochastic)
        sj.fsh$migr<-sj.fsh$migr+left; left<-NULL; 
      } else sj.fsh<-list(migr=sj.fsh)
      
      # SEND FISH TO THE DELTA - note zeros for Low Sac rearing and SJ mainstem
      # route fish from the lower Sac to the south delta based on the flows at Freeport and Delta Cross Channel Gates
      survLowSacFish<-(rbin2MatSpec(low.sac.fsh$migr,LL.Sac.S,stochastic)*stochastic)+((low.sac.fsh$migr%z%LL.Sac.S)*(1-stochastic))
      
      if(alt != "originalDSM"){ # if we're running the model with updated inputs
        prop.dlt.entrain<-sum(route.2.sDlt(freeportQ[mnth,ifelse(mnth>8,yr,yr+1)]*35.3147, dlt.gates[mnth,ifelse(mnth>8,yr,yr+1),"days_closed"],vary=vary,pctil=pctil)[3:4]) #*35.3147 converts cms to cfs for function
      } else {
        prop.dlt.entrain<-sum(route.2.sDlt(freeportQ[mnth,ifelse(mnth>8,yr,yr+1)]*35.3147,dlt.gates$days_closed[mnth],vary=vary,pctil=pctil)[3:4]) #*35.3147 converts cms to cfs for function
      }
      
      '/# ** track, record prop.dlt.entrain **
      if(yr2>5){track_prop.dlt.entrain[yr, which(mnth.names==mnth)] = prop.dlt.entrain}
      '
      
      # north delta first
      sac.not.entrained<-rbinMatObject(survLowSacFish,(1-prop.dlt.entrain),stochastic)
      N.delt.fsh[1:23,]<-N.delt.fsh[1:23,]+sac.not.entrained+rbind((((rbin2MatSpec(yolo.fsh$migr,Yolo.S,stochastic)*stochastic)+(yolo.fsh$migr%z%Yolo.S))*(1-stochastic)),matrix(0,ncol=4,nrow=3))
      
      if(mnth>8|mnth<5){
        N.delt.fsh<-fill.delt.func(juvs=N.delt.fsh,est_hab=NDLThab,territory_size,
                                   stoch=stochastic)
      } else N.delt.fsh<-list(out2ocean=N.delt.fsh)
      
      # south delta next 
      S.delt.fsh[25:27,]<-S.delt.fsh[25:27,]+DL.sheds$migr
      S.delt.fsh[28:30,]<-S.delt.fsh[28:30,]+(rbin2MatSpec(sj.fsh$migr,SJ.S,stochastic)*stochastic)+((sj.fsh$migr%z%SJ.S)*(1-stochastic))
      S.delt.fsh<-S.delt.fsh+rbind((((survLowSacFish-sac.not.entrained)*stochastic)+((survLowSacFish*prop.dlt.entrain)*(1-stochastic))),matrix(0,ncol=4,nrow=8))
      sac.not.entrained<-NULL
      
      if(mnth>8|mnth<5){ 
        S.delt.fsh<-fill.delt.func(juvs=S.delt.fsh,est_hab=SDLThab,territory_size,
                                   stoch=stochastic)
      } else S.delt.fsh<-list(out2ocean=S.delt.fsh)
      
      
      # estimate fish at Golden Gate Bridge and Chipps Island
      ## new routing and survival for delta and SJ tribs
      holdSdelta<-array(0,dim=dim(S.delt.fsh$out2ocean))
      holdSdelta[1:24,]<-rbinMatVector(S.delt.fsh$out2ocean[1:24,],newDsurv[1,],stochastic)
      holdSdelta[26:27,]<-rbinMatVector(S.delt.fsh$out2ocean[26:27,],newDsurv[2,],stochastic)
      holdSdelta[25,]<-rbin2Vectors(S.delt.fsh$out2ocean[25,],newDsurv[3,],stochastic)
      holdSdelta[28:31,]<-rbinMatVector(S.delt.fsh$out2ocean[28:31,],newDsurv[4,],stochastic)
      migrants.out<-rbinMatVector(N.delt.fsh$out2ocean,Sac.Delt.S[1,],stochastic)
      migrants.at.GG<-rbinMatObject(migrants.out,Bay.S,stochastic)+rbinMatObject(holdSdelta,Bay.S,stochastic)
      juv.at.chips<-juv.at.chips+migrants.out+holdSdelta
      migrants.out<-holdSdelta<-NULL
      
      # JUVENILE GROWTH AND SURVIVAL    
      if(mnth>8|mnth<5){
        # upper sac tribs
        USc.sheds<-rearfunc(river_rear=USc.sheds$river,
                            flood_rear=USc.sheds$flood,
                            trans_mat_river=trans_mat_river,
                            trans_mat_flood=trans_mat_flood[,,1:15],
                            flood_surv=flood_surv[1:15,],
                            river_surv=river_surv[1:15,],
                            stoch=stochastic)
        juvs<-USc.sheds$riv.rear+USc.sheds$flood.rear
        
        # lower sac tribs
        LSc.sheds<-rearfunc(river_rear=LSc.sheds$river,
                            flood_rear=LSc.sheds$flood,
                            trans_mat_river=trans_mat_river,
                            trans_mat_flood=trans_mat_flood[,,18:20],
                            flood_surv=flood_surv[18:20,],
                            river_surv=river_surv[18:20,],
                            stoch=stochastic)
        juvs<-rbind(juvs,c(0,0,0,0),c(0,0,0,0),(LSc.sheds$riv.rear+LSc.sheds$flood.rear))
        
        # American River
        AMR.sheds<-rearfunc(river_rear=AMR.sheds$river,
                            flood_rear=AMR.sheds$flood,
                            trans_mat_river=trans_mat_river,
                            trans_mat_flood=trans_mat_flood[,,23],
                            flood_surv=flood_surv[23,],
                            river_surv=river_surv[23,],
                            stoch=stochastic)
        juvs<-rbind(juvs,c(0,0,0,0),c(0,0,0,0),(AMR.sheds$riv.rear+AMR.sheds$flood.rear))
        
        # delta tribs
        DL.sheds<-rearfunc(river_rear=DL.sheds$river,
                           flood_rear=DL.sheds$flood,
                           trans_mat_river=trans_mat_river,
                           trans_mat_flood=trans_mat_flood[,,25:27],
                           flood_surv=flood_surv[25:27,],
                           river_surv=river_surv[25:27,],
                           stoch=stochastic)
        juvs<-rbind(juvs,c(0,0,0,0),(DL.sheds$riv.rear+DL.sheds$flood.rear))
        
        # SJ tribs
        SJ.sheds<-rearfunc(river_rear=SJ.sheds$river,
                           flood_rear=SJ.sheds$flood,
                           trans_mat_river=trans_mat_river,
                           trans_mat_flood=trans_mat_flood[,,28:30],
                           flood_surv=flood_surv[28:30,],
                           river_surv=river_surv[28:30,],
                           stoch=stochastic)
        juvs<-rbind(juvs,(SJ.sheds$riv.rear+SJ.sheds$flood.rear),c(0,0,0,0))
        
        # upper sac rearing
        up.sac.fsh<-rearfunc(river_rear=up.sac.fsh$Umain.river,
                             flood_rear=up.sac.fsh$Umain.flood,
                             trans_mat_river=trans_mat_river,
                             trans_mat_flood=trans_mat_flood[,,16],
                             flood_surv=flood_surv[16,],
                             river_surv=river_surv[16,],
                             stoch=stochastic)
        up.sac.fsh<-up.sac.fsh$riv.rear+up.sac.fsh$flood.rear
        
        # lower mid sac rearing
        low.mid.sac.fsh<-rearfunc(river_rear=low.mid.sac.fsh$Lmain.river,
                                  flood_rear=low.mid.sac.fsh$Lmain.flood,
                                  trans_mat_river=trans_mat_river,
                                  trans_mat_flood=trans_mat_flood[,,21],
                                  flood_surv=flood_surv[21,],
                                  river_surv=river_surv[21,],
                                  stoch=stochastic)
        low.mid.sac.fsh<-low.mid.sac.fsh$riv.rear+low.mid.sac.fsh$flood.rear
        
        # lower sac rearing
        low.sac.fsh<-rearfunc(river_rear=low.sac.fsh$Lmain.river,
                              flood_rear=low.sac.fsh$Lmain.flood,
                              trans_mat_river=trans_mat_river,
                              trans_mat_flood=trans_mat_flood[,,24],
                              flood_surv=flood_surv[24,],
                              river_surv=river_surv[24,],
                              stoch=stochastic)
        low.sac.fsh<-low.sac.fsh$riv.rear+low.sac.fsh$flood.rear
        
        # SJ rearing
        sj.fsh<-rearfunc(river_rear=sj.fsh$Lmain.river,
                         flood_rear=sj.fsh$Lmain.flood,
                         trans_mat_river=trans_mat_river,
                         trans_mat_flood=trans_mat_flood[,,31],
                         flood_surv=flood_surv[31,],
                         river_surv=river_surv[31,],
                         stoch=stochastic)
        sj.fsh<-sj.fsh$riv.rear+sj.fsh$flood.rear
        
        # sutter BP rearing
        sutter.fsh<-rearfunc(river_rear=sutter.fsh$Umain.river,
                             flood_rear=sutter.fsh$Umain.flood,
                             trans_mat_river=trans_mat_river,
                             trans_mat_flood=trans_mat_flood[,,17],
                             flood_surv=BP_surv[1,],
                             river_surv=BP_surv[1,],
                             stoch=stochastic)
        sutter.fsh<-sutter.fsh$riv.rear+sutter.fsh$flood.rear
        
        # yolo BP rearing
        yolo.fsh<-rearfunc(river_rear=yolo.fsh$Umain.river,
                           flood_rear=yolo.fsh$Umain.flood,
                           trans_mat_river=trans_mat_river,
                           trans_mat_flood=trans_mat_flood[,,22],
                           flood_surv=BP_surv[1,],
                           river_surv=BP_surv[1,],
                           stoch=stochastic)
        yolo.fsh<-yolo.fsh$riv.rear+yolo.fsh$flood.rear
        
        # north delta rearing
        N.delt.fsh<-Delt.rearfunc(delt_juv=N.delt.fsh$Delt.rear,
                                  trans_mat_river=trans_mat_river,
                                  D_juv_surv=D_juv_surv[1,],
                                  stoch=stochastic)
        
        # south-central delta rearing
        S.delt.fsh<-Delt.rearfunc(delt_juv=S.delt.fsh$Delt.rear,
                                  trans_mat_river=trans_mat_river,
                                  D_juv_surv=D_juv_surv[2,],
                                  stoch=stochastic)
      }
      # OCEAN ENTRY SURVIVAL
      adults.in.ocean<-adults.in.ocean+Ocean.ent.surv(migrants=migrants.at.GG,mnth,T.mo,mo.ocean.surv,vect[10],stoch=stochastic,vary=vary,pctil=pctil)
    }
    # allocate fish among returns
    for(sitex in 1:31){
      if(adults.in.ocean[sitex]<=1000000000 & stochastic==1){
        randReturnSpawners<-as.vector(rmultinom(1,round(adults.in.ocean[sitex]),c(0.25,0.5,0.25)))
      } else{randReturnSpawners<-round(adults.in.ocean[sitex]*c(0.25,0.5,0.25))}
      spwners[sitex,(yr2+2):(yr2+4)]<-spwners[sitex,(yr2+2):(yr2+4)]+(randReturnSpawners*stochastic)+((adults.in.ocean[sitex]*c(0.25,0.5,0.25))*(1-stochastic))
      randReturnSpawners<-NULL
    }
  }
  
  # Minimize difference in observed vs. predicted spawners
  est.trib.adults<-knwn.trib.adults<-c()
  for(kk in 6:18){ # Adam only had years 6:18 - need to figure out why
    knwn.t.ad<-adultEscapeesKnown[1,kk]
    est.t.ad<-adultEscapeesSimulated[1,kk]
    knwn.trib.adults<-c(knwn.trib.adults,knwn.t.ad)
    est.trib.adults<-c(est.trib.adults,est.t.ad)
  }
  knwn.trib.adults<-knwn.trib.adults*(1-prop.hatch)
  # skipped filtering data, since all 'known' spawners > 100
  
  tada<-sum((knwn.trib.adults-est.trib.adults)^2)
  return(tada)
}
