############################
# Annotation
  # This function is based on WinterRunStochasticDSM_11_13_2019.R
    # Located in the winterRunDSM-Main repository, pulled from https://github.com/CVPIA-OSC/winterRunDSM/tree/main
  # Pathing is changed to reflect this model's location in a new repository
  # This function reads in the new input data depending on the alternative ('alt')
    # This function is called by the 'Wrapper script.R' to import, organize, and standardize data inputs
############################

load_model_inputs<-function(alt = "originalDSM", 
                            data_dir_flow = here("cvpiaFlow", "Final Data Objects"),
                            data_dir_temp = here("cvpiaTemperature", "Final Data Objects"),
                            salmon_run = "winter",
                            vars_to_global = TRUE){
  
  #check that a valid alternative has been added (if not, an error will let you know)
  valid_alts <-  c("originalDSM", 
                   "Reclamation_2021LTO_CalSim3_EXP1_2022MED_rev10_090623_dynGWSW",
                   "Reclamation_2021LTO_CalSim3_EXP3_2022MED_rev10_090623_dynGWSW",
                   "Reclamation_2021LTO_CalSim3_NAA_2022MED_09072023",
                   "Reclamation_2021LTO_CalSim3_Alt1_2022MED_09092023",
                   "Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09072023",
                   "Reclamation_2021LTO_CalSim3_Alt2v1_wTUCP_2022MED_09072023",
                   "Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_090723",
                   "Reclamation_2021LTO_CalSim3_Alt2v3_noTUCP_2022MED_090723",
                   "Reclamation_2021LTO_CalSim3_ALT3_2022MED_092423",
                   "Reclamation_2021LTO_CalSim3_Alt4_2022MED_09082023", 
                   "Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09132024",
                   "Reclamation_2021LTO_CalSim3_Alt2v1_wTUCP_2022MED_09132024",
                   "Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_09132024",
                   "Reclamation_2021LTO_CalSim3_Alt2v3_noTUCP_2022MED_09132024",
                   "Reclamation_2021LTO_CalSim3_Alt4_2022MED_09162024")
  if (!(alt %in% valid_alts)) stop("Invalid alt passed to load_model_inputs")
  
  # Read in baseline data
  calib_data<-readRDS(here("winterRunDSM-main", "delta-calibration-1980_2017.rds"))
  
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
  known.Adults<-read.csv("winterRunDSM-main/SimulaitonSeed_WinterRunAdults.csv") #Winter run GrandTab data
  known.Adults<-known.Adults[order(known.Adults$order),]
  data_inputs[["known.Adults"]] <- known.Adults
  
  data_inputs[["states"]] <- read.csv("winterRunDSM-main/watershedInfo.csv")[, c("watershed", "order", "regulated", "dynam", "grp", "DiversityGroup", "spwnDecay", "rearDecay")]
  data_inputs[["hatcheryStream"]] <- read.csv("winterRunDSM-main/watershedInfo.csv")[, c("watershed", "hatchery")]
  data_inputs[["winterRunStream"]] <- read.csv("winterRunDSM-main/watershedInfo.csv")[, c("watershed", "winterRunLoc")]
  
  maxHabitat<-read.csv("winterRunDSM-main/theoretical_max_area.csv")
  #table(maxHabitat$species)
  maxHabitat<-maxHabitat[which(maxHabitat$species=="fr"),]
  maxHabitat<-maxHabitat[-1*(c(which(maxHabitat$watershed=="North Delta"),which(maxHabitat$watershed=="South Delta"))),]
  data_inputs[["maxHabitat"]] <- maxHabitat
  
  maxSpwnHab<-maxHabitat[which(maxHabitat$lifestage=="spawning"),c(1,4)]
  maxSpwnHab<-merge(known.Adults[,1:2],maxSpwnHab,all=TRUE)
  maxSpwnHab<-maxSpwnHab[order(maxSpwnHab$order),]
  data_inputs[["maxSpwnHab"]] <- maxSpwnHab
  
  maxRearHab<-maxHabitat[which(maxHabitat$lifestage=="rearing"),c(1,4)]
  maxRearHab<-merge(known.Adults[,1:2],maxRearHab,all=TRUE)
  maxRearHab<-maxRearHab[order(maxRearHab$order),] #is it important for them to be ordered?
  data_inputs[["maxRearHab"]] <- maxRearHab
  
  #make changes to the original inputs
  original_inputs$inps$prop.nat.remov[1]<-mean(c(0.18,0.09,0.07,0.13,0.02,0.03)) # from Doug Killam 2012 - 2017 data
  original_inputs$inps$A.HARV[1]<-0.2 # from Corey Phillis
  
  original_inputs$prop.hatch<-0.1759966 #proportion hatchery based on CWT reports
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
               "IChab.spawn", "IChab.fry", "IChab.juv", "floodP",
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



