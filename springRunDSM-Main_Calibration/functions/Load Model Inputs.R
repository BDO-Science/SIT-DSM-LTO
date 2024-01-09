############################
# Annotation
# This function is based on SpringRunStochasticDSM_11_13_2019.R
# Located in the springRunDSM-Main repository, pulled from https://github.com/CVPIA-OSC/springRunDSM/tree/main
# Pathing is changed to reflect this model's location in a new repository
# This function reads in the new input data depending on the alternative ('alt')
# Added new input, 'alt' that tells the model to use original flow inputs (is.na(alt)) or updated inputs (!is.na(alt))
# Added two switches based on 'alt' object to interface with 2 flow inputs in different ways
  # dlt.gates
  # prop.Q.bypasses

# ***!!*** # identifies areas that need to be modified for a winter-run/spring-run switch
############################

load_model_inputs<-function(alt = "originalDSM", 
                            data_dir_flow = here("cvpiaFlow", "Final Data Objects"),
                            data_dir_temp = here("cvpiaTemperature", "Final Data Objects"),
                            salmon_run = "spring",
                            vars_to_global = TRUE){
  
  #check that a valid alternative has been added (if not, an error will let you know)
  valid_alts <-  c("originalDSM", "LTO_NAA_HistHydro_HistSC", "LTO_NAA_2035CT_HistSC_dv", "CS3_L2020_DV_2021_ext_2040MED",  
                   "D1641_CalSimII_old", "ROR_CalSimII_old")
  if (!(alt %in% valid_alts)) stop("Invalid alt passed to load_model_inputs")
  
  # Read in baseline data
  calib_data<-readRDS(here("springRunDSM-main", "delta-calibration-1980_2017.rds")) # ***!!*** #
  
  original_inputs <- cvpiaData::load_baseline_data(salmon_run)
  #list2env(all_inputs)
  
  flow_vars <- c("upSacQ", "t.diver", "retQ", "prop.Q.bypasses",
                   "prop.pulse", "p.diver", "gate.top", "freeportQ", "flows_cfs", "Dlt.inf", 
                   "dlt.gates", "dlt.divers.tot", "dlt.divers", "delta_flows", 
                   "Q_free", "Q_vern", "Q_stck", "CVP_exp", "SWP_exp")
  temp_vars <- c("juv.tmp", "DegDay")
  
  calib_flow_vars <- c("Q_free", "Q_vern", "Q_stck", "CVP_exp", "SWP_exp")
  
  data_inputs <- list()
    
  if(alt == "originalDSM"){
    for(i in 1:length(calib_flow_vars)){
      data_inputs[[i]] <- calib_data[[calib_flow_vars[i]]]
      names(data_inputs)[i] <- calib_flow_vars[i]
    }
  } else {
    for(i in 1:length(flow_vars)){
      flow_file <- file.path(data_dir_flow, alt,  paste0(flow_vars[i], ".rds"))
      if(file.exists(flow_file)){
        data_inputs[[i]] <- readRDS(flow_file)
      } else {
        warning("File does not exist for flow_var ", flow_vars[i])
        data_inputs[[i]] <- NA
      }
      names(data_inputs)[i] <- flow_vars[i]
    }
    for(i in 1:length(temp_vars)){
      temp_file <- file.path(data_dir_temp, alt,  paste0(temp_vars[i], ".rds"))
      if(file.exists(temp_file)){
        data_inputs[[length(flow_vars)+i]] <- readRDS(temp_file)
      } else {
        warning("File does not exist for temp_var ", temp_vars[i])
        data_inputs[[length(flow_vars)+i]] <- NA
      }
      names(data_inputs)[length(flow_vars)+i] <- temp_vars[i]
    }
  }
 
  #add static data that don't change across alternatives
  # ***!!*** #
  known.Adults<-read.csv("springRunDSM-main/SimulaitonSeed_SpringRunAdults.csv") #Spring run GrandTab data
  known.Adults<-known.Adults[order(known.Adults$order),]
  data_inputs[["known.Adults"]] <- known.Adults
  
  data_inputs[["states"]] <- read.csv("springRunDSM-main/watershedInfo.csv")[, c("watershed", "order", "regulated", "dynam", "grp", 
                                                                                 "DiversityGroup", "spwnDecay", "rearDecay", "srpop")] # * added srpop for spring-run
  data_inputs[["hatcheryStream"]] <- read.csv("winterRunDSM-main/watershedInfo.csv")[, c("watershed", "hatchery")]
  # data_inputs[["winterRunStream"]] <- read.csv("winterRunDSM-main/watershedInfo.csv")[, c("watershed", "winterRunLoc")] # * not used for spring-run
  
  maxHabitat<-read.csv("springRunDSM-main/theoretical_max_area.csv")
  #table(maxHabitat$species)
  maxHabitat<-maxHabitat[which(maxHabitat$species=="sr"),]
  # maxHabitat<-maxHabitat[-1*(c(which(maxHabitat$watershed=="North Delta"),which(maxHabitat$watershed=="South Delta"))),] # * this line is not in spring-run
  data_inputs[["maxHabitat"]] <- maxHabitat
  
  maxSpwnHab<-maxHabitat[which(maxHabitat$lifestage=="spawning"),c(1,4)]
  maxSpwnHab<-merge(known.Adults[,1:2],maxSpwnHab,all=TRUE)
  maxSpwnHab<-maxSpwnHab[order(maxSpwnHab$order),]
  maxSpwnHab$max_suit_sqm[is.na(maxSpwnHab$max_suit_sqm)==TRUE]<-0 # * added this for spring-run
  data_inputs[["maxSpwnHab"]] <- maxSpwnHab
  
  maxRearHab<-maxHabitat[which(maxHabitat$lifestage=="rearing"),c(1,4)]
  maxRearHab<-merge(known.Adults[,1:2],maxRearHab,all=TRUE)
  maxRearHab<-maxRearHab[order(maxRearHab$order),] 
  maxRearHab$max_suit_sqm[is.na(maxRearHab$max_suit_sqm)==TRUE]<-0 # * added this for spring-run
  data_inputs[["maxRearHab"]] <- maxRearHab
  
  #make changes to the original inputs
    # * commented these out, as these manual adjustments apply on to winter-run
  #original_inputs$inps$prop.nat.remov[1]<-mean(c(0.18,0.09,0.07,0.13,0.02,0.03)) # from Doug Killam 2012 - 2017 data
  #original_inputs$inps$A.HARV[1]<-0.2 # from Corey Phillis
  
  # proportion hatchery
    # * spring-run has a vector, while winter-run uses a single value
  Butte_Creek=mean(c(0.01,0,0,0)) #2010 - 2013 from CWT reports
  Feather_River=mean(c(0.78,0.90,0.90,0.84))
  Yuba_River=mean(c(0.71,0.495,0.36,0.40))
  original_inputs$prop.hatch<-c(rep(0,5),Butte_Creek,rep(0,12),Feather_River,Yuba_River,rep(0,11))
  
  original_inputs$aveT201<-aveT20
  original_inputs$aveT20D1<-aveT20D
  original_inputs$floodPNew<-rep(0,31)
  
  #calculate total habitat for sutter and yolo
  original_inputs$IChab.sutter<-original_inputs$IChab.bypass[1,,]+original_inputs$IChab.bypass[2,,]+original_inputs$IChab.bypass[3,,]+original_inputs$IChab.bypass[4,,]
  original_inputs$IChab.yolo<-original_inputs$IChab.bypass[5,,]+original_inputs$IChab.bypass[6,,]
  original_inputs$floodp.sutter<-original_inputs$floodp.bypass[1,,]+original_inputs$floodp.bypass[2,,]+original_inputs$floodp.bypass[3,,]+original_inputs$floodp.bypass[4,,]
  original_inputs$floodp.yolo<-original_inputs$floodp.bypass[5,,]+original_inputs$floodp.bypass[6,,]
  
  #fix index when running the model
  original_inputs$Temp_vern<-calib_data$Temp_vern 
  # Temp_vern<-fix.tmp(Temp_vern)
  original_inputs$Temp_pp<-calib_data$Temp_pp 
  # Temp_pp<-fix.tmp(Temp_pp)   
  original_inputs$Trap_trans <-0
  
  
  #add variables to data_inputs list that haven't changed from the original_inputs list
  unchanged_data <- original_inputs[names(original_inputs)[!(names(original_inputs) %in% names(data_inputs))]]
  data_inputs <- c(data_inputs, unchanged_data)
  
  #fill in NAs with 0s
  vars_na <- c("p.diver", "t.diver", "DegDay", "retQ", "p.tempMC20", "prop.pulse", 
               "IChab.spawn", "IChab.fry", "IChab.juv", "floodP", "SR.pools", # added SR.pools for spring-run
               "Q_free", "Q_vern", "Q_stck", "Temp_vern", "Temp_pp", "CVP_exp", "SWP_exp")
  for(i in vars_na){
    data_inputs[[i]][is.na(data_inputs[[i]])] <- 0 #are there any that should be NA=0?
  }
  
  #add variables from the data_inputs list to the Global Environment
  if(vars_to_global){
    list2env(data_inputs,.GlobalEnv)
  }

  return(data_inputs)
  
}



