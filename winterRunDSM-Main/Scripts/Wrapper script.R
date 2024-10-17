
#########################################
# Model annotation
  # This wrapper script is based on WinterRunStochasticDSM_11_13_2019.R
    # Located in the winterRunDSM-Main repository, pulled from https://github.com/CVPIA-OSC/winterRunDSM/tree/main
  # I changed pathing to reflect this model's location in a new repository
  # Note for running:
    # *Be sure to start a new R session before running models to prevent clashes between packages!
    # *Users will need to install the cvpiaData package from the FlowWest GitHub, in order to load base model inputs
      # e.g., remotes::install_github("FlowWest/CVPIAdata")
    # *We also recommend users install other dependent data packages, for documentation purposes
      # e.g., remotes::install_github("FlowWest/cvpiaHabitat")
      # remotes::install_github("FlowWest/cvpiaFlow")
      #remotes::install_github("FlowWest/cvpiaTemperature")
  # Changes to the run_scenarios() function
    # I moved the run_scenarios() function (renamed life_cycle_model()) into its own script
      # (located in 'functions' sub-folder) and call it in this script
    # I added a new input, 'alt', that tells the model to use the original or updated inputs
      # There are slight differences in the data structure between the two
    # Additional changes to this function are described in the corresponding script
  # Changes to 'Deterministic Runs'
    # Removed the for loop, so it only calls one scenario at a time (scenario 0, or no habitat addition)
    # Allows specification of the CalSim alternative, or 'alt'
      # Requires a valid name is provided, or an error is thrown
      # If a new alternative is named, it pulls updated inputs from the 'cvpiaFlow/Final Data Objects/' sub-folder
    # Imports seed number of spawners, to start simulation at yr2=6
    # Output from life_cycle_model() is saved as an .rds file in the 'Output' sub-folder
  # Changes to 'Stochastic Runs'
    # Created new custom data objects for storing results across iterations
    # Allows specification of the CalSim alternative, or 'alt'
      # Requires a valid name is provided, or an error is thrown
      # If a new alternative is named, it pulls updated inputs from the 'cvpiaFlow/Final Data Objects/' sub-folder
    # Imports seed number of spawners, to start simulation at yr2=6
    # Output from life_cycle_model() is saved as an .rds file in the 'Output' sub-folder
#########################################

#########################################
# Set up necessary functions and packages
#########################################

# Workaround to get rjava (dependent of xlsx) functional in lieu of a patched R v4.2.2
replacement <- function(category = "LC_ALL") {
  
  if (identical(category, "LC_MESSAGES"))
    return("")
  
  category <- match(category, .LC.categories)
  if (is.na(category)) 
    stop("invalid 'category' argument")
  .Internal(Sys.getlocale(category))
  
}
base <- asNamespace("base")
environment(replacement) <- base
unlockBinding("Sys.getlocale", base)
assign("Sys.getlocale", replacement, envir = base)
lockBinding("Sys.getlocale", base)

# Load packages
require(here); require(xlsx)
library(cvpiaData)
library(data.table)

# Life cycle model function
source(here("winterRunDSM-Main","functions","Life cycle model.R"))
source(here("winterRunDSM-Main","functions","Life cycle model_v2.R"))

# Dependent functions of the life cycle model function
fun.place<-paste(getwd(),"/winterRunDSM-main/functions",sep="")
functs<-dir(path=fun.place,full.names=FALSE,pattern=".R",recursive=FALSE)
for(ia in 1:length(functs)) source(paste(fun.place,functs[ia],sep="/"))

# Load Model Inputs function
source(here("winterRunDSM-Main","functions","Load Model Inputs.R"))

####################
# Deterministic Runs
####################

scenarioRun = 0

#add updated data inputs from the LTO Alternative of interest
  # Right now, this is set up to update both temperature and flow if an alternative CalSim run is selected
  # Need to ensure the same file naming structure is used for both new flow and temperature inputs

alt <- c("Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09132024")
  # All recent CalSim 3 alternatives
    # originalDSM
    ## Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09132024
    ## Reclamation_2021LTO_CalSim3_Alt2v1_wTUCP_2022MED_09132024
    ## Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_09132024
    ## Reclamation_2021LTO_CalSim3_Alt2v3_noTUCP_2022MED_09132024
    ## Reclamation_2021LTO_CalSim3_Alt4_2022MED_09162024
    # Reclamation_2021LTO_CalSim3_EXP1_2022MED_rev10_090623_dynGWSW
    # Reclamation_2021LTO_CalSim3_EXP3_2022MED_rev10_090623_dynGWSW
    # Reclamation_2021LTO_CalSim3_NAA_2022MED_09072023
    # Reclamation_2021LTO_CalSim3_Alt1_2022MED_09092023
    # Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09072023
    # Reclamation_2021LTO_CalSim3_Alt2v1_wTUCP_2022MED_09072023
    # Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_090723
    # Reclamation_2021LTO_CalSim3_Alt2v3_noTUCP_2022MED_090723
    # Reclamation_2021LTO_CalSim3_ALT3_2022MED_092423
    # Reclamation_2021LTO_CalSim3_Alt4_2022MED_09082023

load_model_inputs(alt)

# Update t.diver for Upper Sacramento River, in accordance with re-calibration
  # Only need to do this for originalDSM input b/c all inputs for Alternatives are already correct
if(alt == "originalDSM"){
  t.diver[1,,] <- readRDS("./winterRunDSM-Main_Calibration/New t.diver data for calibration/new.t.diver.rds")
}

# Old base model, available at "Life cycle model.R"
  # E.g., base<-life_cycle_model(stochastic=0,scenario=scenarioRun,vary="Nothing",pctil=1, alt=alt)
  # Only use this for generating seeds for life_cycle_model_v2 (requires going into script manually)
    # Need to do the following to obtain the spawners_seed.rds object: 
      # Set the following:
        #stochastic=0
        #scenario=0
        #vary="Nothing"
        #pctil=1
        #alt="originalDSM"
      # Go into the script for life_cycle_model and manually change number of yr2 (~line 120) from 1:25 to 1:5
      # Run model manually, in "Life cycle model.R", from ~lines 21-1555 (end of year loop)
      # Save spwners object as follows: saveRDS(spwners, "./winterRunDSM-Main/spawners_seed_recalibration.rds")


# Run updated model

  # Set seed spawners - see above
#seed <- readRDS("./winterRunDSM-Main/spawners_seed.rds")
seed <- readRDS("./winterRunDSM-Main/spawners_seed_recalibration.rds")

  # Run model
base<-life_cycle_model_v2(stochastic=0,scenario=scenarioRun,vary="Nothing",
                          pctil=1, alt=alt, spwners=seed)

# Save results
name<-paste0(here("winterRunDSM-main", "Output", "WR_Recal_Det_Scen"),scenarioRun,"_Alt_",alt,"_v2")
saveRDS(base, paste0(name,".rds"))

####################
# Stochastic Runs
####################

n.iter = 100
scenarioRun = 0

# Create data structures to hold outputs
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
out.all_spawners <- out.nat_spawners <- out.juv_biomass <- out.Bay.fish <- array (NA, dim=c(31,20,n.iter),
                                                                  dimnames=list(watershed.names,1980:1999,1:n.iter))
out.juv.at.chips <- array (NA, dim=c(20,31,4,n.iter),
                           dimnames=list(1980:1999,watershed.names,size.names,1:n.iter))
out.adult_en_route <- array(NA, dim=c(20,4,31,n.iter),
                            dimnames=list(1980:1999,1:4,watershed.names,1:n.iter))
out.pre.spawn.S <- array(NA, dim=c(20,2,31,n.iter),
                         dimnames=list(1980:1999,c("Spring","Summer"),watershed.names,1:n.iter))
out.p.pulse.leave <- out.river_surv <- out.flood_surv <- array(NA, dim=c(20,9,31,4,n.iter),
                                                               dimnames=list(1980:1999,c(9:12,1:5),watershed.names,size.names,1:n.iter))
out.D_juv_surv <- out.Sac.Delt.S <- array(NA, dim=c(20,9,2,4,n.iter),
                                          dimnames=list(1980:1999,c(9:12,1:5),c("N","S"),size.names,1:n.iter))
out.UM.Sac.S <- out.LM.Sac.S <- out.LL.Sac.S <- array(NA, dim=c(20,9,4,n.iter),
                                                      dimnames=list(1980:1999,c(9:12,1:5),size.names,1:n.iter))
out.newDsurv <- array(NA, dim=c(20,9,4,4,n.iter),
                      dimnames=list(1980:1999,c(9:12,1:5),c("N","O1","O2","O3"),size.names,1:n.iter))
out.prop.dlt.entrain <- array(NA, dim=c(20,9,n.iter),
                              dimnames=list(1980:1999,c(9:12,1:5),1:n.iter))

#add updated data inputs from the LTO Alternative of interest
# Right now, this is set up to update both temperature and flow if an alternative CalSim run is selected
# Need to ensure the same file naming structure is used for both new flow and temperature inputs

alt <- c("Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09132024")
  # All recent CalSim 3 alternatives
    ## Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09132024
    ## Reclamation_2021LTO_CalSim3_Alt2v1_wTUCP_2022MED_09132024
    ## Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_09132024
    ## Reclamation_2021LTO_CalSim3_Alt2v3_noTUCP_2022MED_09132024
    ## Reclamation_2021LTO_CalSim3_Alt4_2022MED_09162024
    # Reclamation_2021LTO_CalSim3_EXP1_2022MED_rev10_090623_dynGWSW
    # Reclamation_2021LTO_CalSim3_EXP3_2022MED_rev10_090623_dynGWSW
    # Reclamation_2021LTO_CalSim3_NAA_2022MED_09072023
    # Reclamation_2021LTO_CalSim3_Alt1_2022MED_09092023
    # Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09072023
    # Reclamation_2021LTO_CalSim3_Alt2v1_wTUCP_2022MED_09072023
    # Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_090723
    # Reclamation_2021LTO_CalSim3_Alt2v3_noTUCP_2022MED_090723
    # Reclamation_2021LTO_CalSim3_ALT3_2022MED_092423
    # Reclamation_2021LTO_CalSim3_Alt4_2022MED_09082023

load_model_inputs(alt)

# Update t.diver for Upper Sacramento River, in accordance with re-calibration
  # Only need to do this for originalDSM input b/c all inputs for Alternatives are already correct
if(alt == "originalDSM"){
  t.diver[1,,] <- readRDS("./winterRunDSM-Main_Calibration/New t.diver data for calibration/new.t.diver.rds")
}

# Set seed spawners - see above in Deterministic
#seed <- readRDS("./winterRunDSM-Main/spawners_seed.rds")
seed <- readRDS("./winterRunDSM-Main/spawners_seed_recalibration.rds")

# Run model
for(iter in 1:n.iter){
  
  # Run life cycle function
  base<-life_cycle_model_v2(stochastic=1,scenario=scenarioRun,vary="Nothing",
                            pctil=1, alt=alt, spwners=seed)
  
  # Save iteration-specific outputs
  out.all_spawners[,,iter] <- base$out.all_spawners
  out.nat_spawners[,,iter] <- base$out.nat_spawners
  out.juv_biomass[,,iter] <- base$out.juv_biomass
  out.Bay.fish[,,iter] <- base$out.Bay.fish
  out.juv.at.chips[,,,iter] <- base$out.juv.at.chips
  out.adult_en_route[,,,iter] <- base$out.adult_en_route
  out.pre.spawn.S[,,,iter] <- base$out.pre.spawn.S
  out.p.pulse.leave[,,,,iter] <- base$out.p.pulse.leave
  out.river_surv[,,,,iter] <- base$out.river_surv
  out.flood_surv[,,,,iter] <- base$out.flood_surv
  out.D_juv_surv[,,,,iter] <- base$out.D_juv_surv
  out.Sac.Delt.S[,,,,iter] <- base$out.Sac.Delt.S
  out.UM.Sac.S[,,,iter] <- base$out.UM.Sac.S
  out.LM.Sac.S[,,,iter] <- base$out.LM.Sac.S
  out.LL.Sac.S[,,,iter] <- base$out.LL.Sac.S
  out.newDsurv[,,,,iter] <- base$out.newDsurv
  out.prop.dlt.entrain[,,iter] <- base$out.prop.dlt.entrain

  print(iter)
  
}

stoch_results <- list(out.all_spawners=out.all_spawners, 
                      out.nat_spawners=out.nat_spawners,
                      out.juv_biomass=out.juv_biomass, 
                      out.Bay.fish=out.Bay.fish,
                      out.juv.at.chips=out.juv.at.chips,
                      out.adult_en_route=out.adult_en_route,
                      out.pre.spawn.S=out.pre.spawn.S,
                      out.p.pulse.leave=out.p.pulse.leave, 
                      out.river_surv=out.river_surv,
                      out.flood_surv=out.flood_surv, 
                      out.D_juv_surv=out.D_juv_surv,
                      out.Sac.Delt.S=out.Sac.Delt.S, 
                      out.UM.Sac.S=out.UM.Sac.S,
                      out.LM.Sac.S=out.LM.Sac.S, 
                      out.LL.Sac.S=out.LL.Sac.S,
                      out.newDsurv=out.newDsurv, 
                      out.prop.dlt.entrain=out.prop.dlt.entrain,
                      n.iter=n.iter)

# Save results
name<-paste0(here("winterRunDSM-main", "Output", "WR_Recal_Stoch_Scen"),scenarioRun,"_Alt_",alt,"_Iter",n.iter,"_v2")
saveRDS(stoch_results, paste0(name,".rds"))
