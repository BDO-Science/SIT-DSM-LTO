
##################################
# Annotation
  # This script creates flow input data structures, as utilized in DSMs, from CalSim II DSS output files
  # Notes on running this script
    # Specify which alternative you want to generate data inputs from in 'Set up directories...', below
      # by changing 'dss_file'
    # Users will need to install cvpiaData and cvpiaFlow R packages from GitHub
      # e.g., # remotes::install_github("FlowWest/CVPIAdata")
      # remotes::install_github("FlowWest/cvpiaFlow")
    # * This script requires users install HEC-DSSVue and update the 'HEC_direc' object, below,
      # in order to correctly tell R where to find the HEC-DSSVUE program
      # This program is required to interact with DSS data objects produced by CalSim
      # * We note that a different version of HEC-DSSVue may cause problems with this script
    # * This script also requires users install and load the following packages:
      # plyr, dplyr, stringr, lubridate, rJava, ggplot2, tidyr, reshape2, grid, measurements
  # CalSim nodes were identified using a combination of cvpiaData, cvpiaFlow GitHub package documentation and troubleshooting
###################################

##############################################################
# Set up directories, load libraries, identify input .dss file
##############################################################

# ***CHANGE THESE DIRECTORIES***

HEC_direc <- "C:\\Program Files\\HEC\\HEC-DSSVue\\" # Point to where your HEC-DSSVue program is located 
#dss_file <- 'LTO_NAA_HistHydro_HistSC'
#dss_file <- 'LTO_NAA_2035CT_HistSC_dv'
#dss_file <- 'ROR_CalSimII_old'
dss_file <- 'D1641_CalSimII_old'

# These directories do not need to be changed

#dss_direc <- paste0('./DSMflow/CalSim Input/',dss_file,"/",dss_file,".dss")
dss_direc <- paste0('./cvpiaFlow/CalSim Input/',dss_file,"/",dss_file,".dss")
output_direc <- paste0('./cvpiaFlow/Final Data Objects/',dss_file,"/")

# Function for getting CalSim time stamps turned into R dates. 
from_time_stamp <- function(x) {
  day_ref <- as.Date("1899-12-30")
  return(day_ref+x/1440)
}

# Load needed libraries
require(plyr); require(dplyr); require(stringr); require(lubridate); require(rJava); require(ggplot2); 
require(tidyr); require(reshape2); require(grid); require(measurements)

# Workaround to get rjava functional in lieu of a patched R v4.2.2
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

##################################################################
# Call and test the java function call for accessing the .dss file
##################################################################

# Get java connection up and running
dss_location <- HEC_direc 
jars <- c(list.files(paste0(HEC_direc,"jar"))) 
jars <- paste0(dss_location, "jar/", jars)
libs <- paste0("-Djava.library.path=",HEC_direc,"lib\\") 
.jinit(classpath = jars, parameters = libs)

# Open the DSS file through rJava
dssFile <- .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;",   method="open", dss_direc)

# Check that the DSS file is working
dssFile$get("/CALSIM/DXC_ESTDV/GATE-DAYS-OPEN//1MON/2020D09E/")$values %>% head()

#############################
# Create DSMflow data objects
#############################
  ################################
  # Create delta_flows data object (and dependencies: dlt_divers, dlt_divers_tot, Dlt.inf) 
    # Relationships to nodes come from cvpiaData annotation
    # *NOTE:the last extension, '2020D09E' (used in all subsequent DSS calls), may need to be updated with new CalSim versions
    # Need to convert cfs to cms for delta_total_diverted and delta_inflow
  ################################

# First create the delta_flows object from CalSim data (dataframe with 972 rows (dates 1922-2002) and 7 variables)

# Identify CalSim nodes necessary to construct cvpiaflow input variables
n_dlt_inflow_cfs_nodes <- c("C400", "C157")
s_dlt_inflow_cfs_nodes <- c("C401B", "C504", "C508", "C644")
n_dlt_div_cfs_nodes <- c("D403A", "D403B", "D403C", "D403D","D404")
s_dlt_div_cfs_nodes <- c("D418", "D419", "D412", "D410", "D413", "D409B", "D416", "D408_OR", "D408_VC")

nodes <- c(n_dlt_inflow_cfs_nodes, s_dlt_inflow_cfs_nodes,
           n_dlt_div_cfs_nodes, s_dlt_div_cfs_nodes)
  
# Create indexing data frame for pulling multiple CalSim nodes from the .dss file  
nodes_df <- data.frame(input=c(rep("n_dlt_inflow_cfs",2),
                               rep("s_dlt_inflow_cfs",4),
                               rep("n_dlt_div_cfs",5),
                               rep("s_dlt_div_cfs",9)),
                       nodes=nodes,
                       type=c(rep("FLOW-CHANNEL",6),
                              rep("FLOW-DELIVERY",14)))

# Create blank data frame for storing CalSim output values
delta_flows_raw <- data.frame(date=Date(), input=character(),
                              node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame  
for(i in 1:length(nodes_df$input)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                          nodes_df$type[i],"//1MON/2020D09E/"))
  times <- rmt.java$times %>% from_time_stamp
  times2 <- times[which(year(times) %in% seq(1922,2002))]
  values <- rmt.java$values[which(year(times) %in% seq(1922,2002))]
  len_values <- length(values)
  
  df_temp <- data.frame(date=times2, input=nodes_df$input[i],
                        node=nodes_df$nodes[i], value=values)
  delta_flows_raw <- rbind(delta_flows_raw, df_temp)
}

# Create delta_flows object
delta_flows <- delta_flows_raw %>% 
  group_by(date, input) %>% 
  summarise(value_sum=sum(value)) %>% # Generate delta_flow variable values by summing across nodes
  pivot_wider(names_from=input, values_from=value_sum) %>%
  select(date, n_dlt_inflow_cfs, s_dlt_inflow_cfs, n_dlt_div_cfs, s_dlt_div_cfs) %>%
  mutate(n_dlt_prop_div=n_dlt_div_cfs/n_dlt_inflow_cfs, # Generate new variables
         s_dlt_prop_div=ifelse(s_dlt_div_cfs/s_dlt_inflow_cfs>1,1, # Constrain proportions to <= 1
                               s_dlt_div_cfs/s_dlt_inflow_cfs))

# Save delta_flows object
saveRDS(delta_flows, paste0(output_direc, "delta_flows.rds"))
  
# Old plotting scripts to compare new delta_flows object to original

'/  
delta_flows$run <- "NAA"
delta_flows_old <- DSMflow::delta_flows
delta_flows_old$run <- "Old"
delta_flows_aggr <- rbind(delta_flows, delta_flows_old)

ggplot(data=delta_flows_aggr, aes(x=date, y=n_dlt_inflow_cfs, color=run)) +
  geom_line()
ggplot(data=delta_flows_aggr, aes(x=date, y=s_dlt_inflow_cfs, color=run)) +
  geom_line()
ggplot(data=delta_flows_aggr, aes(x=date, y=n_dlt_prop_div, color=run)) +
  geom_line()
ggplot(data=delta_flows_aggr, aes(x=date, y=s_dlt_prop_div, color=run)) +
  geom_line()

ggplot(data=delta_flows_aggr, 
       aes(x=as.numeric(month(date)), y=n_dlt_inflow_cfs, 
           group=year(date))) +
  geom_path() +
  facet_wrap(as.factor(delta_flows_aggr$run))

ggplot(data=delta_flows_aggr, 
       aes(x=as.numeric(month(date)), y=s_dlt_inflow_cfs, 
           group=year(date))) +
  geom_path() +
  facet_wrap(as.factor(delta_flows_aggr$run))
'
  
# Create dlt.divers (delta proportion diverted) object (array: 12 (months) x 21 (years; 1980-2000) x 2 (N/S Delta))
  
dlt.divers <- delta_flows %>% 
  rename(`North Delta`=n_dlt_prop_div, `South Delta`=s_dlt_prop_div) %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), year=year(date)) %>%
  filter(year %in% c(1980:2000)) %>%
  pivot_longer(cols = `North Delta`:`South Delta`, names_to="delta", values_to="divert") %>%
  #mutate(cms = conv_unit(divert, "ft3_per_sec", "m3_per_sec")) %>%
  ungroup() %>%
  select(divert, month, year, delta) %>%
  melt(id=c("month", "year", "delta")) %>%
  acast(month~year~delta)

# Error checker if we start workflow with cvpiaFlow::delta_flows (all good)
# dlt.divers - cvpiaData::dlt_divers

# Save dlt.divers object
saveRDS(dlt.divers, paste0(output_direc, "dlt.divers.rds"))
  
# Create dlt.divers.tot (delta total diverted) object (array: 12 (months) x 21 (years; 1980-2000) x 2 (N/S Delta))

dlt.divers.tot <- delta_flows %>% 
  rename(`North Delta`=n_dlt_div_cfs, `South Delta`=s_dlt_div_cfs) %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), year=year(date)) %>%
  filter(year %in% c(1980:2000)) %>%
  pivot_longer(cols = `North Delta`:`South Delta`, names_to="delta", values_to="divert") %>%
  #mutate(cms = conv_unit(divert, "ft3_per_sec", "m3_per_sec")) %>% # Use this to convert cfs to cms only if you don't have access to DSMflow
  mutate(cms = DSMflow::cfs_to_cms(divert)) %>% # Convert cfs to cms
  ungroup() %>%
  select(cms, month, year, delta) %>%
  melt(id=c("month", "year", "delta")) %>%
  acast(month~year~delta)

# Error checker if we start workflow with cvpiaFlow::delta_flows
# dlt.divers.tot - cvpiaData::dlt_divers_tot

# Save dlt.divers.tot object
saveRDS(dlt.divers.tot, paste0(output_direc, "dlt.divers.tot.rds"))
  
# Create Dlt.inf (delta inflow) object (array: 12 (months) x 21 (years; 1980-2000) x 2 (N/S Delta))

Dlt.inf <- delta_flows %>% 
  rename(`North Delta`=n_dlt_inflow_cfs, `South Delta`=s_dlt_inflow_cfs) %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), year=year(date)) %>%
  filter(year %in% c(1980:2000)) %>%
  pivot_longer(cols = `North Delta`:`South Delta`, names_to="delta", values_to="divert") %>%
  mutate(cms = conv_unit(divert, "ft3_per_sec", "m3_per_sec")) %>% # Convert cfs to cms
  ungroup() %>%
  select(cms, month, year, delta) %>%
  melt(id=c("month", "year", "delta")) %>%
  acast(month~year~delta)

# Error checker if we start workflow with cvpiaFlow::delta_flows
# Dlt.inf - cvpiaData::dlt_inflow

# Save Dlt.inf object
saveRDS(Dlt.inf, paste0(output_direc, "Dlt.inf.rds"))
  
  ###########
  # flows_cfs (and dependencies: prop.pulse, retQ) 
    # Relationships to nodes come from cvpiaData annotation
    # Node for Mokelumne River (index #26, or C91) is not available through CalSim
      # Data provided from model provided by EBMUD (East Bay Munipical Utility District)
      # For this watershed, I'm borrowing a nearby node "C501" for automated data pulling, then
        # filling in with cvpiaFlow::flows_cfs values for Mokelumne
    # For prop.pulse, we have to calculate using median and sd() from 1980-2000
      # We also remove the /100 step, as it's already performed in the model
      # We also have to convert to tibble and add watershed column to match formatting
    # For retQ, have to convert to data frame
  ###########

# Create the flows_cfs object from CalSim data
  # Dataframe with 972 rows (dates 1922-2002) and 31 variables

# Create indexing data frame for pulling multiple CalSim nodes from the .dss file 
nodes_df <- data.frame(input=c("Upper Sacramento River", "Antelope Creek",
                               "Battle Creek", "Bear Creek", "Big Chico Creek",
                               "Butte Creek", "Clear Creek", "Cottonwood Creek",
                               "Cow Creek", "Deer Creek", "Elder Creek",
                               "Mill Creek", "Paynes Creek", "Stony Creek",
                               "Thomes Creek", "Upper-mid Sacramento River",
                               "Bear River", "Feather River", "Yuba River",
                               "Lower-mid Sacramento River1", "Lower-mid Sacramento River2",
                               "American River", "Lower Sacramento River",
                               "Calaveras River", "Cosumnes River", "Mokelumne River",
                               "Merced River", "Stanislaus River", "Tuolumne River",
                               "San Joaquin River"),
                       nodes=c("C104", "C11307", "C10803", "C11001", "C11501",
                               "C217A", "C3", "C10802", "C10801", "C11309",
                               "C11303", "C11308", "C11001", "C142A", "C11304",
                               "C115", "C285", "C203", "C230", "C134", "C160",
                               "C9", "C166", "C92", "C501", "C501", "C561",
                               "C520", "C540", "C630"),
                       type="FLOW-CHANNEL")

# Create blank data frame for storing CalSim output values for *DSM Modeling*
flows_cfs_raw <- data.frame(date=Date(), input=character(),
                              node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame for *DSM Modeling*
for(i in 1:length(nodes_df$input)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/2020D09E/"))
  times <- rmt.java$times %>% from_time_stamp
  times2 <- times[which(year(times) %in% seq(1922,2002))]
  values <- rmt.java$values[which(year(times) %in% seq(1922,2002))]
  len_values <- length(values)
  
  df_temp <- data.frame(date=times2, input=nodes_df$input[i],
                        node=nodes_df$nodes[i], value=values)
  flows_cfs_raw <- rbind(flows_cfs_raw, df_temp)
}

# Manipulate data frame to produce the flows_cfs data object
flows_cfs <- flows_cfs_raw %>% 
  select(date, input, value) %>% #group_by(date, input) %>% 
  #summarise(value_sum=sum(value)) %>% 
  pivot_wider(names_from=input, values_from=value) %>%
  as.data.frame()

# Modify `Mokelumne River` data based on preference
flows_cfs$'Mokelumne River' <- NA
# Or
flows_cfs$'Mokelumne River' <- cvpiaFlow::flows_cfs$`Mokelumne River`

# Save flows_cfs object
saveRDS(flows_cfs, paste0(output_direc, "flows_cfs.rds"))

# Create flows_cfs_all object for use in DSM habitat estimates Line of Evidence

# Create blank data frame for storing CalSim output values for *WUA Modeling*
flows_cfs_raw <- data.frame(date=Date(), input=character(),
                            node=character(), value=numeric())

inputs <- c("Upper Sacramento River", "Antelope Creek",
            "Battle Creek", "Bear Creek", "Big Chico Creek",
            "Butte Creek", "Clear Creek", "Cottonwood Creek",
            "Cow Creek", "Deer Creek", "Elder Creek",
            "Mill Creek", "Paynes Creek", "Stony Creek",
            "Thomes Creek", "Upper-mid Sacramento River",
            "Bear River", "Feather River", "Yuba River",
            "Lower-mid Sacramento River1", "Lower-mid Sacramento River2",
            "American River", "Lower Sacramento River",
            "Calaveras River", "Cosumnes River", "Mokelumne River",
            "Merced River", "Stanislaus River", "Tuolumne River",
            "San Joaquin River")

# Extract values from CalSim nodes, store in data frame  
for(i in 1:length(nodes_df$input)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/2020D09E/"))
  times <- rmt.java$times %>% from_time_stamp
  values <- rmt.java$values
  len_values <- length(values)
  
  df_temp <- data.frame(date=times, input=nodes_df$input[i],
                        node=nodes_df$nodes[i], value=values)
  flows_cfs_raw <- rbind(flows_cfs_raw, df_temp)
}

# Manipulate data frame to produce the flows_cfs data object
flows_cfs_all <- flows_cfs_raw %>% 
  select(date, input, value) %>% #group_by(date, input) %>% 
  #summarise(value_sum=sum(value)) %>% 
  pivot_wider(names_from=input, values_from=value) %>%
  as.data.frame() %>%
  mutate(`Mokelumne River`= NA) %>% # calc comes from flows_cfs annotation
  pivot_longer(cols = `Upper Sacramento River`:`San Joaquin River`,
               names_to = "input", values_to="flow") %>%
  mutate(month=month(date), 
         year=year(date), 
         WY=ifelse(month%in%c(10:12),year+1,year)) %>%
  filter(input %in% inputs) %>% 
  arrange(factor(input, levels=inputs))

# Save flows_cfs_all object
saveRDS(flows_cfs_all, paste0(output_direc, "flows_cfs_all.rds"))

# Old plotting code

'/
flows_cfs$run <- "NAA"
flows_cfs_old <- DSMflow::flows_cfs
flows_cfs_old$run <- "Old"
flows_cfs_aggr <- rbind(flows_cfs, flows_cfs_old)

ggplot(data=flows_cfs_aggr, 
       aes(x=as.numeric(month(date)), y=`Upper Sacramento River`, 
           group=year(date))) +
  geom_path() +
  facet_wrap(as.factor(flows_cfs_aggr$run))

ggplot(data=flows_cfs_aggr, 
       aes(x=as.numeric(month(date)), y=`Battle Creek`, 
           group=year(date))) +
  geom_path() +
  facet_wrap(as.factor(flows_cfs_aggr$run))
'

# Now create the prop.pulse (proportion pulse flows) object, based on the flows_cfs object
  # Dataframe, 31 (rows=watersheds) x 13 (cols: watershed+months)
  # sd(flow) / median(flow)
  # sd() and median() are summarizing across years (1980-2000) for each month
  # *Note: watershed names differ somewhat between flows_cfs and dependents
  # *Assign 0s for `Sutter Bypass` and `Yolo Bypass` based on DSMflow data object

# Identify watershed names, in order provided in the DSM object
inputs <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
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

# Create prop.pulse
prop.pulse <- flows_cfs %>%
  mutate(`Lower-mid Sacramento River` = 35.6/58*`Lower-mid Sacramento River1` + 22.4/58*`Lower-mid Sacramento River2`) %>% # calc comes from flows_cfs annotation
  pivot_longer(cols = `Upper Sacramento River`:`Lower-mid Sacramento River`,
               names_to = "input", values_to="value") %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), year=year(date)) %>%
  filter(year %in% c(1980:2000)) %>% # tests with DSMflow::flow_cfs show the inputs are calculated with 1980:1999
  group_by(month, input) %>%
  summarize(prop = ifelse(is.infinite(sd(value)/median(value)),0,sd(value)/median(value))) %>% # ifelse added by me to avoid Inf values 
  as.data.frame() %>% 
  rbind(., expand.grid(month=month.abb[1:12], # Add Sutter and Yolo Bypass, which have 0 values in DSMflow
                       input=c("Sutter Bypass", "Yolo Bypass"),
                       prop=0)) %>% 
  filter(input %in% inputs) %>% # remove "Lower-mid Sacramento River1" and "2"
  ungroup() %>%
  mutate(input2 = factor(input, levels=inputs)) %>% # re-order according to DSMflow object
  select(month, input2, prop) %>%
  melt(id=c("month", "input2")) %>%
  acast(input2~month)

# Error checker if we start workflow with cvpiaFlow::flows_cfs (all good)
#prop.pulse - cvpiaData::prop_pulse[,2:13]

# Convert object into a tibble, prefaced by watershed names
prop.pulse <- prop.pulse %>% as_tibble() %>% cbind(inputs,.)

# Save prop.pulse object
saveRDS(prop.pulse, paste0(output_direc, "prop.pulse.rds"))

# Now create the retQ (proportion_flow_natal object), based on flows_cfs
# Dataframe, 31 (rows=watersheds) x 23 (cols: watershed+years 1979-2000)
  # *Note: watershed names differ somewhat between flows_cfs and retQ
  # Following inputs are assigned a value of 1:
    # Upper-mid Sacramento River, Lower-mid Sacramento River,
      # Lower Sacramento River, Calaveras River, Cosumnes River,
      # Mokelumne River, San Joaquin River
  # Following inputs are assigned a value of 0:
    # Sutter Bypass, Yolo Bypass
  # Remaining inputs are assigned using flow relationships in DSMflow annotation
    # Feather River is relative to 'Lower-mid Sacramento River2' based on trial-and-error

# Create retQ (proportion_flow_natal)
retQ <- flows_cfs %>%
  mutate(`Lower-mid Sacramento River` = 35.6/58*`Lower-mid Sacramento River1` + 22.4/58*`Lower-mid Sacramento River2`,
         `Upper Sacramento River` = `Upper Sacramento River`/`Upper-mid Sacramento River`,
         `Antelope Creek` = `Antelope Creek`/`Upper-mid Sacramento River`,
         `Battle Creek` = `Battle Creek`/`Upper-mid Sacramento River`,
         `Bear Creek` = `Bear Creek`/`Upper-mid Sacramento River`,
         `Big Chico Creek` = `Big Chico Creek`/`Upper-mid Sacramento River`,
         `Butte Creek` = `Butte Creek`/`Upper-mid Sacramento River`,
         `Clear Creek` = `Clear Creek`/`Upper-mid Sacramento River`,
         `Cottonwood Creek` = `Cottonwood Creek`/`Upper-mid Sacramento River`,
         `Cow Creek` = `Cow Creek`/`Upper-mid Sacramento River`,
         `Deer Creek` = `Deer Creek`/`Upper-mid Sacramento River`,
         `Elder Creek` = `Elder Creek`/`Upper-mid Sacramento River`,
         `Mill Creek` = `Mill Creek`/`Upper-mid Sacramento River`,
         `Paynes Creek` = `Paynes Creek`/`Upper-mid Sacramento River`,
         `Stony Creek` = `Stony Creek`/`Upper-mid Sacramento River`,
         `Thomes Creek` = `Thomes Creek`/`Upper-mid Sacramento River`,
         `Upper-mid Sacramento River` = 1,
         `Sutter Bypass` = 0,
         `Bear River` = `Bear River`/`Feather River`,
         `Yuba River` = `Yuba River`/`Feather River`,
         `Feather River` = `Feather River`/`Lower-mid Sacramento River2`,
         `Lower-mid Sacramento River` = 1,
         `Yolo Bypass` = 0,
         `American River` = `American River`/`Lower Sacramento River`,
         `Lower Sacramento River` = 1,
         `Calaveras River` = 1,
         `Cosumnes River` = 1,
         `Mokelumne River` = 1,
         `Merced River` = `Merced River`/`San Joaquin River`,
         `Stanislaus River` = `Stanislaus River`/`San Joaquin River`,
         `Tuolumne River` = `Tuolumne River`/`San Joaquin River`,
         `San Joaquin River` = 1) %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), year=year(date)) %>%
  filter(year %in% (1979:2000), month == "Oct") %>% # summarize for month of October
  as.data.frame() %>%
  pivot_longer(cols = `Upper Sacramento River`:`Yolo Bypass`,
               names_to = "input", values_to = "value") %>%
  mutate(value = pmin(value,1)) %>% # Ensure no proportions exceed 1
  filter(input %in% inputs) %>%
  ungroup() %>%
  mutate(input2 = factor(input, levels=inputs)) %>% # re-order according to DSMflow object
  select(year, input2, value) %>%
  melt(id=c("year", "input2")) %>%
  acast(input2~year)

# Error checker if we start workflow with cvpiaFlow::flows_cfs (all good)
# summary(retQ - cvpiaData::returnQ[,2:23])

# Save retQ object
saveRDS(retQ, paste0(output_direc, "retQ.rds"))

  ################
  # upSacQ
    # Matrix: 12 (months) x 21 (years, 1980-2000)
    # Need to convert flows from cfs to cms
    # Relationships to nodes come from cvpiaFlow annotation
  ################

# Create data frame for indexing
nodes_df <- data.frame(nodes=c("C109"),
                       type=c("FLOW-CHANNEL"))

# Create blank data frame for storing CalSim data
flows_raw <- data.frame(date=Date(), 
                        node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame  
for(i in 1:length(nodes_df$nodes)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/2020D09E/"))
  times <- rmt.java$times %>% from_time_stamp
  times2 <- times[which(year(times) %in% seq(1980,2000))]
  values <- rmt.java$values[which(year(times) %in% seq(1980,2000))]
  len_values <- length(values)
  
  df_temp <- data.frame(date=times2, #input=nodes_df$input[i],
                        node=nodes_df$nodes[i], value=values)
  flows_raw <- rbind(flows_raw, df_temp)
}

# Calculate new variables and convert cfs to cms
#flows_raw$value <- conv_unit(flows_raw$value, "ft3_per_sec", "m3_per_sec") # use this only if you don't have access to DSMflow
flows_raw$value <- DSMflow::cfs_to_cms(flows_raw$value)
flows_raw$month <- factor(month.abb[month(flows_raw$date)], levels=month.abb[1:12])
flows_raw$year <- year(flows_raw$date)

# Create upSacQ object
upSacQ <- flows_raw %>% 
  select(value, month, year) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)

# Save upSacQ object
saveRDS(upSacQ, paste0(output_direc, "upSacQ.rds"))

  ################
  # bypass_flows - USED ONLY IN SIT DSM HABITAT ESTIMATES LINE OF EVIDENCE
    # Data frame: 972 rows (dates, monthly, 1922-2002) with 7 variables
    # Flows stay expressed in cfs
    # Relationships to nodes come from DSMflow annotation
  ################

# Create data frame for indexing
nodes_df <- data.frame(input=c("sutter1", "sutter2", "sutter3", "sutter4",
                               "yolo1", "yolo2"),
                       nodes=c("D117", "C135", "C136A", "C137",
                               "D160", "C157"),
                       type=c("FLOW-DELIVERY", rep("FLOW-CHANNEL",3),
                              "FLOW-DELIVERY", "FLOW-CHANNEL"))

# Create blank data frame for storing CalSim data
bypass_flows_raw <- data.frame(date=Date(), input=character(),
                            node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame  
for(i in 1:length(nodes_df$input)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/2020D09E/"))
  times <- rmt.java$times %>% from_time_stamp
  values <- rmt.java$values
  len_values <- length(values)
  
  df_temp <- data.frame(date=times, input=nodes_df$input[i],
                        node=nodes_df$nodes[i], value=values)
  bypass_flows_raw <- rbind(bypass_flows_raw, df_temp)
}

# Create bypass_flows object
bypass_flows <- bypass_flows_raw %>% 
  select(date, input, value) %>% 
  pivot_wider(names_from=input, values_from=value) %>%
  #mutate(sutter2 = sutter2+sutter1, # commented this out for WUA analysis
  #       sutter3 = sutter3+sutter2,
  #       sutter4 = sutter4+sutter3,
  #       yolo2 = yolo2+yolo1) %>%
  as.data.frame() %>% # everything from this point on is only run for WUA analysis and can be commented out
  pivot_longer(cols=sutter1:yolo2, names_to="input", values_to="flow") %>%
  mutate(month=month(date), 
       year=year(date), 
       WY=ifelse(month%in%c(10:12),year+1,year))

# Save bypass_flows object
saveRDS(bypass_flows, paste0(output_direc, "bypass_flows.rds"))

  ####################################
  # Q_free/freeportQ, Q_vern, Q_stck, CVP_exp, SWP_exp
    # (All) Matrix: 12 (months) x 21 (years, 1980-2000)
      # cvpiaData:: versions run 1980-2017, but we only need 1980-2000
    # Have to convert all of these from cfs (CalSim default) to cms
    # Relationships to nodes come from cvpiaFlow annotation
      # cvpiaData used separate calibration datasets that didn't come from Calsim!
      # ** We are updating the input source, therefore, for this application of 'Main'
    # * Note: I treated freeportQ as identical to Q_free, though Q_free came from weird calibrated dataset
  ####################################

'/# Create data frame for indexing for ROR alternative - used to create dummy results for CVP_ and SWP_exports
  # This approach should be used if we want to manually fix CVP_/SWP_exports to 0
nodes_df <- data.frame(input=c("freeport_flows", "vernalis_flows", "stockton_flows",
                               "CVP_exports", "SWP_exports"),
                       nodes=c("C400", "C639", "C417A", "C417A", "C417A"),
                       type=c(rep("FLOW-CHANNEL",5)))
                       
# Or, pull in different CalSim variables that are available for ROR and can calculate DEL_CVP_TOTAL_N
  # This addresses missing DEL_CVP_TOTAL variable
  # This approach should be used if we want to use whatever non-pumping diversions occur in CVP_/SWP_exports
    # This is probably more consistent with current model calibrations
nodes_df <- data.frame(input=c("freeport_flows", "vernalis_flows", "stockton_flows",
                               rep("CVP_exports",6), "SWP_exports"),
                       nodes=c("C400", "C639", "C417A", 
                       "DEL_CVP_TOTAL_N", "DEL_CVP_PAG_S","DEL_CVP_PEX_S",
                       "DEL_CVP_PLS_S", "DEL_CVP_PMI_S", "DEL_CVP_PRF_S",
                       "DEL_SWP_TOTAL"),
                       type=c(rep("FLOW-CHANNEL",3),rep("DELIVERY-CVP",6),"DELIVERY-SWP"))
'

# Create data frame for indexing
nodes_df <- data.frame(input=c("freeport_flows", "vernalis_flows", "stockton_flows",
                               "CVP_exports", "SWP_exports"),
                       nodes=c("C400", "C639", "C417A", "DEL_CVP_TOTAL", "DEL_SWP_TOTAL"),
                       type=c(rep("FLOW-CHANNEL",3), "DELIVERY-CVP", "DELIVERY-SWP"))

# Create blank data frame for storing CalSim data
flows_raw <- data.frame(date=Date(), input=character(),
                            node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame  
for(i in 1:length(nodes_df$input)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/2020D09E/"))
  times <- rmt.java$times %>% from_time_stamp
  times2 <- times[which(year(times) %in% seq(1980,2000))]
  values <- rmt.java$values[which(year(times) %in% seq(1980,2000))]
  len_values <- length(values)
  
  df_temp <- data.frame(date=times2, input=nodes_df$input[i],
                        node=nodes_df$nodes[i], value=values)
  flows_raw <- rbind(flows_raw, df_temp)
}
flows_raw$value[which(flows_raw$date=="1980-06-30")]

# Calculate new variables and convert cfs to cms
#flows_raw$value <- conv_unit(flows_raw$value, "ft3_per_sec", "m3_per_sec") # Only use this if you don't have access to DSMflow
flows_raw$value <- DSMflow::cfs_to_cms(flows_raw$value)
flows_raw$month <- factor(month.abb[month(flows_raw$date)], levels=month.abb[1:12])
flows_raw$year <- year(flows_raw$date)

# Create Q_free object
Q_free <- flows_raw %>% filter(input=="freeport_flows") %>%
  select(value, month, year) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)

# Save Q_free object
saveRDS(Q_free, paste0(output_direc, "Q_free.rds"))

# Save freeportQ as identical to Q_free (not sure why they're used differently in 'Main')
freeportQ <- Q_free
saveRDS(freeportQ, paste0(output_direc, "freeportQ.rds"))

# Create Q_vern object
Q_vern <- flows_raw %>% filter(input=="vernalis_flows") %>%
  select(value, month, year) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)

# Save Q_vern object
saveRDS(Q_vern, paste0(output_direc, "Q_vern.rds"))

# Create Q_stck object
Q_stck <- flows_raw %>% filter(input=="stockton_flows") %>%
  select(value, month, year) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)

# Save Q_stck object
saveRDS(Q_stck, paste0(output_direc, "Q_stck.rds"))

# Create CVP_exp object
#flows_raw$value[which(flows_raw$input=="CVP_exports")] <- 0 # only used for ROR option 1
'/
CVP_exp <- flows_raw %>% filter(input=="CVP_exports") %>% # only used for ROR option 2, w/ missing DEL_CVP_TOTAL object
  group_by(month, year, input) %>%
  summarise(Tot = sum(value)) %>%
  select(Tot, month, year) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)
'
CVP_exp <- flows_raw %>% filter(input=="CVP_exports") %>%
  select(value, month, year) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)

# Save CVP_exp object
saveRDS(CVP_exp, paste0(output_direc, "CVP_exp.rds"))

# Create SWP_exp object
#flows_raw$value[which(flows_raw$input=="SWP_exports")] <- 0 # only used for ROR option 1
SWP_exp <- flows_raw %>% filter(input=="SWP_exports") %>%
  select(value, month, year) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)

# Save SWP_exp object
saveRDS(SWP_exp, paste0(output_direc, "SWP_exp.rds"))


# Old plotting code

'/
freeport_df <- freeport_flows %>% melt()
  freeport_df$run <- "NAA"
freeport_df_old <- DSMflow::freeport_flow %>% melt() 
  freeport_df_old$run <- "Old"
freeport_df_aggr <- rbind(freeport_df, freeport_df_old)

ggplot(data=freeport_df_aggr, 
       aes(x=Var1, y=value, 
           group=Var2)) +
  geom_path() +
  facet_wrap(as.factor(freeport_df_aggr$run)) +
  labs(x="Month", y="Freeport flows (cms)")

CVP_df <- CVP_exports %>% melt()
CVP_df$run <- "NAA"
CVP_df_old <- DSMflow::cvp_exports %>% melt() 
CVP_df_old$run <- "Old"
CVP_df_aggr <- rbind(CVP_df, CVP_df_old)

ggplot(data=CVP_df_aggr, 
       aes(x=Var1, y=value, 
           group=Var2)) +
  geom_path() +
  facet_wrap(as.factor(CVP_df_aggr$run)) +
  labs(x="Month", y="CVP exports (cms)")
'

  #######################
  # t.diver / p.diver
    # (All) Array: 31 (watershed) x 12 (month) x 21(year, 1980-2000)
    # Documentation for cvpiaData suggests I can keep values in cfs
      # ** CONFIRM THAT THIS IS ACCURATE ** # - CONFIRMED
      # ** Note: values for t.diver from cvpiaData only don't match for Upper Sac 
        # due to wrong R calcs (calculates p.diver instead of t.diver)
        # WE NOW GENERATE THE CORRECT VALUES FOR USE IN LTO MODELING
    # Relationships to nodes come from cvpiaData annotation
    # Node for Mokelumne River (index #26, or C91) is not available through CalSim II
      # Data provided from model provided by EBMUD
      # For the time being, I borrow from existing cvpiaFlow object for this river
  #######################

# Create initial raw data file

# Create data frame for indexing
nodes_df <- data.frame(nodes=c("D104", "D217", "D11305", "D11301",
                               "D17301", "D109", "D112",
                               "D113A", "D113B", "D114", "D118",
                               "D122A", "D122B", "D123", "D124A",
                               "D128_WTS", "D128", "D285", "D201", 
                               "D202", "D7A", "D7B", "D230", 
                               "D129A", "D134", "D162", "D165",
                               "D302", "D167", "D168", "D168A_WTS",
                               "D506A", "D506B", "D506C", "D507",
                               "D562", "D566", "D528", "D545",
                               "D637", "D630B", "D630A", "D620B",
                               "C104", "C11307", "C11308", "C11309",
                               "C217B", "C217A", "C11303", "C11304",
                               "C42", "C110", "C285", "C6", 
                               "C230", "C128", "C9", "C166",
                               "C92", "C561", "C520", "C540", 
                               "C637"),
                       type=c(rep("FLOW-DELIVERY", 43),rep("FLOW-CHANNEL", 21)))

# Create blank data frame for storing CalSim data
flows_raw <- data.frame(date=Date(),
                        node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame
for(i in 1:length(nodes_df$nodes)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/2020D09E/"))
  times <- rmt.java$times %>% from_time_stamp
  times2 <- times[which(year(times) %in% seq(1980,2000))]
  values <- rmt.java$values[which(year(times) %in% seq(1980,2000))]
  len_values <- length(values)
  
  df_temp <- data.frame(date=times2,
                        node=nodes_df$nodes[i], value=values)
  flows_raw <- rbind(flows_raw, df_temp)
}

# Calculate new variables and convert cfs to cms
#flows_raw$value <- conv_unit(flows_raw$value, "ft3_per_sec", "m3_per_sec") # Only use this if you don't have access to DSMflow
#flows_raw$value <- DSMflow::cfs_to_cms(flows_raw$value)
flows_raw$month <- factor(month.abb[month(flows_raw$date)], levels=month.abb[1:12])
flows_raw$year <- year(flows_raw$date)

# Manipulate the data object
flows <- flows_raw %>% pivot_wider(names_from="node", values_from="value")

# Generate total_diverted data object

# Calculate total diversion values, subset retained columns
total_diverted_raw <- flows %>%
  mutate(`Upper Sacramento River` = D104, # correct but inconsistent with 'Main'
         #`Upper Sacramento River` = D104/C104, # incorrect but consistent with 'Main'
         `Antelope Creek` = ifelse(C11307==0, 0, (C11307 / (C11307+C11308+C11309) * D11305)),
         `Battle Creek` = 0,
         `Bear Creek` = 0,
         `Big Chico Creek` = 0,
         `Butte Creek` = (C217B + D217),
         `Clear Creek` = 0,
         `Cottonwood Creek` = 0,
         `Cow Creek` = 0,
         `Deer Creek` = ifelse(C11309==0,0,(C11309 / (C11307+C11308+C11309) * D11305)),
         `Elder Creek` = ifelse(C11303==0,0,(C11303 / (C11303 + C11304) * D11301)),
         `Mill Creek` = ifelse(C11308==0,0,(C11308 / (C11307+C11308+C11309) * D11305)),
         `Paynes Creek` = 0,
         `Stony Creek` = D17301,
         `Thomes Creek` = ifelse(C11304==0,0,(C11304 / (C11303+C11304) * D11301)),
         `Upper-mid Sacramento River` = (D109+D112+D113A+D113B+D114+D118+D122A+
                                           D122B+D123+D124A+D128_WTS+D128),
         `Sutter Bypass` = 0,
         `Bear River` = D285,
         `Feather River` = (D201+D202+D7A+D7B),
         `Yuba River` = D230,
         `Lower-mid Sacramento River` = (D129A+D134+D162+D165),
         `Yolo Bypass` = 0,
         `American River` = D302,
         `Lower Sacramento River` = (D167+D168+D168A_WTS),
         `Calaveras River` = (D506A+D506B+D506C+D507),
         `Cosumnes River` = 0,
         `Mokelumne River` = 0,
         `Merced River` = (D562+D566),
         `Stanislaus River` = D528,
         `Tuolumne River` = D545,
         `San Joaquin River` = (D637+D630B+D630A+D620B)) %>%
  select(!(D104:C637)) %>%
  pivot_longer(cols=`Upper Sacramento River`:`San Joaquin River`, names_to="input", values_to = "value")

# Identify watershed names, in order, for proper order in final data object below
input=c("Upper Sacramento River", "Antelope Creek",
        "Battle Creek", "Bear Creek", "Big Chico Creek",
        "Butte Creek", "Clear Creek", "Cottonwood Creek",
        "Cow Creek", "Deer Creek", "Elder Creek",
        "Mill Creek", "Paynes Creek", "Stony Creek",
        "Thomes Creek", "Upper-mid Sacramento River",
        "Sutter Bypass",
        "Bear River", "Feather River", "Yuba River",
        "Lower-mid Sacramento River", "Yolo Bypass",
        "American River", "Lower Sacramento River",
        "Calaveras River", "Cosumnes River", "Mokelumne River",
        "Merced River", "Stanislaus River", "Tuolumne River",
        "San Joaquin River")
total_diverted_raw$input <- factor(total_diverted_raw$input, levels=input)

# Create t.diver data object 
t.diver <- total_diverted_raw %>% select(month:value) %>%
  melt(id=c("input", "month","year")) %>%
  acast(input ~ month ~ year)

# Assign Mokelumne River the same values as used in SIT DSM
t.diver[27,,] <- cvpiaData::total_diversion[27,,]

# Save t.diver object
saveRDS(t.diver, paste0(output_direc, "t.diver.rds"))

# Now generate the proportion_diverted data object

# Calculate proportion_diverted values, retain select columns
proportion_diverted_raw <- flows %>%
  mutate(`Upper Sacramento River` = pmin(D104 / C104,1),
         `Antelope Creek` = pmin(ifelse(C11307==0,0,(C11307 / (C11307+C11308+C11309) * D11305) / C11307),1),
         `Battle Creek` = 0,
         `Bear Creek` = 0,
         `Big Chico Creek` = 0,
         `Butte Creek` = pmin(ifelse((C217B+D217+C217A)==0,0,(C217B + D217) / (C217B+D217+C217A)),1),
         `Clear Creek` = 0,
         `Cottonwood Creek` = 0,
         `Cow Creek` = 0,
         `Deer Creek` = pmin(ifelse(C11309==0,0,(C11309 / (C11307+C11308+C11309) * D11305) / C11309),1),
         `Elder Creek` = pmin(ifelse(C11303==0,0,(C11303 / (C11303 + C11304) * D11301) / C11303),1),
         `Mill Creek` = pmin(ifelse(C11308==0,0,(C11308 / (C11307+C11308+C11309) * D11305) / C11308),1),
         `Paynes Creek` = 0,
         `Stony Creek` = pmin(ifelse(C42==0,0,D17301 / C42),1),
         `Thomes Creek` = pmin(ifelse(C11304==0,0,(C11304 / (C11303+C11304) * D11301) / C11304),1),
         `Upper-mid Sacramento River` = pmin((D109+D112+D113A+D113B+D114+D118+D122A+
                                           D122B+D123+D124A+D128_WTS+D128) / C110,1),
         `Sutter Bypass` = 0,
         `Bear River` = pmin(D285 / (C285+D285),1),
         `Feather River` = pmin((D201+D202+D7A+D7B) / C6,1),
         `Yuba River` = pmin(D230 / (C230+D230),1),
         `Lower-mid Sacramento River` = pmin((D129A+D134+D162+D165) / C128,1),
         `Yolo Bypass` = 0,
         `American River` = pmin(D302 / C9,1),
         `Lower Sacramento River` = pmin((D167+D168+D168A_WTS) / C166,1),
         `Calaveras River` = pmin(ifelse(C92==0,0,(D506A+D506B+D506C+D507) / C92),1),
         `Cosumnes River` = 0,
         `Mokelumne River` = 0,
         `Merced River` = pmin(ifelse(C561==0,0,(D562+D566) / C561),1),
         `Stanislaus River` = pmin(D528 / C520,1),
         `Tuolumne River` = pmin(ifelse(C540==0,0,D545 / C540),1),
         `San Joaquin River` = pmin((D637+D630B+D630A+D620B) / (D637+D630B+D630A+D620B+C637),1)) %>%
  select(!(D104:C637)) %>%
  pivot_longer(cols=`Upper Sacramento River`:`San Joaquin River`, names_to="input", values_to = "value")

# Identify watershed names, in order, for proper order in final data object below
input=c("Upper Sacramento River", "Antelope Creek",
        "Battle Creek", "Bear Creek", "Big Chico Creek",
        "Butte Creek", "Clear Creek", "Cottonwood Creek",
        "Cow Creek", "Deer Creek", "Elder Creek",
        "Mill Creek", "Paynes Creek", "Stony Creek",
        "Thomes Creek", "Upper-mid Sacramento River",
        "Sutter Bypass",
        "Bear River", "Feather River", "Yuba River",
        "Lower-mid Sacramento River", "Yolo Bypass",
        "American River", "Lower Sacramento River",
        "Calaveras River", "Cosumnes River", "Mokelumne River",
        "Merced River", "Stanislaus River", "Tuolumne River",
        "San Joaquin River")
proportion_diverted_raw$input <- factor(proportion_diverted_raw$input, levels=input)

# Create p.diver data object 
p.diver <- proportion_diverted_raw %>% select(month:value) %>%
  melt(id=c("input", "month","year")) %>%
  acast(input ~ month ~ year)

# Assign Mokelumne River the same values as used in SIT DSM
p.diver[27,,] <- cvpiaData::prop_diversion[27,,]

# Save p.diver object
saveRDS(p.diver, paste0(output_direc, "p.diver.rds"))

  ########################## 
  # prop.Q.bypasses, gate.top
    # Arrays: 12 (month) x 21 (year, 1980-2000) x 2(Sutter/Yolo)
    # Relationships to nodes come from cvpiaFlow annotation for gat.top
    # For prop.Q.bypasses, we are sticking with rel't for Yolo Bypass from cvpiaData
      # D160/(D160+C160), as opposed to D160/C134 (from DSMflow)
    # For prop.Q.bypasses, for Sutter Bypass, I think the correct calc should be:
      # (D117+D124+D125+D126)/C116
      # Instead, I'm pretty sure cvpiaData uses propQsutter1, or D117/C116, which is always 0
        # ** WE HAVE TO DECIDE IF WE SHOULD UPDATE OR STICK WITH THIS MOVING FORWARD
        # Currently, we're using the value D117/C116 for consistency with original DSM model runs
  ##########################

# Create data frame for indexing
nodes_df <- data.frame(nodes=c("D117", "D124", "D125", "D126", "D160",
                               "C116", "C134", "C137", "C160", "C157"),
                       type=c(rep("FLOW-DELIVERY",5),rep("FLOW-CHANNEL",5)))

# Create blank data frame for storing CalSim data
flows_raw <- data.frame(date=Date(), 
                        node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame
for(i in 1:length(nodes_df$nodes)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/2020D09E/"))
  times <- rmt.java$times %>% from_time_stamp
  times2 <- times[which(year(times) %in% seq(1980,2000))]
  values <- rmt.java$values[which(year(times) %in% seq(1980,2000))]
  len_values <- length(values)
  
  df_temp <- data.frame(date=times2, 
                        node=nodes_df$nodes[i], value=values)
  flows_raw <- rbind(flows_raw, df_temp)
}

# Calculate new variables and convert cfs to cms
flows_raw$month <- factor(month.abb[month(flows_raw$date)], levels=month.abb[1:12])
flows_raw$year <- year(flows_raw$date)

# Manipulate the data object
flows <- flows_raw %>% pivot_wider(names_from="node", values_from="value")

# Create prop.Q.bypasses

# Possibly accurate, but inconsistent with current usage in Main
bypass_raw <- flows %>%
  mutate(`Sutter Bypass` = pmin((D117+D124+D125+D126)/C116, 1), # Add pmin() to prevent proportion flow from >1
         `Yolo Bypass` = pmin(D160/(D160+C160), 1)) %>%
  select(!(D117:C157)) %>%
  pivot_longer(cols=`Sutter Bypass`:`Yolo Bypass`, names_to="input", values_to = "value")

# Possibly incorrect, but consistent with current usage in Main
bypass_raw <- flows %>%
  mutate(`Sutter Bypass` = pmin((D117)/C116, 1), # Add pmin() to prevent proportion flow from >1
         `Yolo Bypass` = pmin(D160/(D160+C160), 1)) %>%
  select(!(D117:C157)) %>%
  pivot_longer(cols=`Sutter Bypass`:`Yolo Bypass`, names_to="input", values_to = "value")

# Create prop.Q.bypasses object
prop.Q.bypasses <- bypass_raw %>% select(month:value) %>%
  melt(id=c("input", "month","year")) %>%
  acast(month ~ year ~ input)

# Save prop.Q.bypasses
saveRDS(prop.Q.bypasses, paste0(output_direc, "prop.Q.bypasses.rds"))

# Creating gate.top, based on transform_inputs_complete.R in DSMflow GitHub

gates_raw <- flows %>%
  mutate(`Sutter Bypass` = ifelse((D117+D124+D125+D126+C137) >= 100, TRUE, FALSE),
         `Yolo Bypass` = ifelse((D160+C157) >= 100, TRUE, FALSE)) %>%
  select(!(D117:C157)) %>%
  pivot_longer(cols=`Sutter Bypass`:`Yolo Bypass`, names_to="input", values_to = "value")

gate.top <- gates_raw %>% select(month:value) %>%
  melt(id=c("input", "month","year")) %>%
  acast(month ~ year ~ input)

# Save gate.top
saveRDS(gate.top, paste0(output_direc, "gate.top.rds"))

  #######################
  # delta_cross_channel_closed
    # Matrix: 2 (data type) x 12 (month)
      # OR Array: 12 (month) x 21 (year) x variable (days closed, prop closed)
      # Composed by row of cc_gates_days_closed & cc_gates_prop_days_closed
    # Multiple options
      # 1. Use existing values, based on 2009 BiOp
      # 2. Use expected values from 2019 BiOp
      # 3. Use updated CalSim outputs, either averaged across years (A) or year-specific (B)
        # ** I'm going with option 3B 
  # Use DXC for days the gate is open
  #######################

# Option 1

delta_cross_channel_closed <- DSMflow::delta_cross_channel_closed

# Option 2 (NEED TO DIG THROUGH BiOp)

# Option 3 - Pull from CalSim inputs
  # Years 1980-2000, for consistency with other Delta inputs

# Create data frame for indexing
nodes_df <- data.frame(nodes=c("DXC"), # the DXC node represents the realized # of days the gate is open
                       type=c("GATE-DAYS-OPEN"))

# Create blank data frame for storing CalSim data
gate_raw <- data.frame(date=Date(), 
                        node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame
for(i in 1:length(nodes_df$nodes)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/2020D09E/"))
  times <- rmt.java$times %>% from_time_stamp
  times2 <- times[which(year(times) %in% seq(1980,2000))]
  values <- rmt.java$values[which(year(times) %in% seq(1980,2000))]
  len_values <- length(values)
  
  df_temp <- data.frame(date=times2, #input=nodes_df$input[i],
                        node=nodes_df$nodes[i], value=values)
  gate_raw <- rbind(gate_raw, df_temp)
}

# Manipulate data to obtain raw data file for future use
gate_raw <- gate_raw %>%
  mutate(month = factor(month.abb[month(date)], levels=month.abb[1:12]),
         days = day(date),
         year = year(date),
         days_closed = ifelse(days - value < 0, 0, days-value), # DXC indicates 31 days closed for September (30 days in total)
         prop_closed = days_closed/days) 

  # Option A: Average across years from CalSim outputs

'/

# Calculate cc_gates_days_closed
cc_gates_days_closed <- gate_raw %>%
  group_by(month) %>%
  summarize(no_day = mean(days_closed)) %>%
  as.data.frame() %>%
  melt(id=c("month")) %>%
  select(month, value)
cc_gates_days_closed <- matrix(cc_gates_days_closed$value, nrow=1, ncol=12,
                               dimnames = list(c("count"),
                                               cc_gates_days_closed$month))

# Calculate cc_gates_prop_closed
cc_gates_prop_closed <- gate_raw %>%
  group_by(month) %>%
  summarize(no_day = mean(prop_closed)) %>%
  as.data.frame() %>%
  melt(id=c("month")) %>%
  select(month, value)
cc_gates_prop_closed <- matrix(cc_gates_prop_closed$value, nrow=1, ncol=12,
                               dimnames = list(c("proportion"),
                                               cc_gates_prop_closed$month))

# Create delta_cross_channel_closed
delta_cross_channel_closed <- rbind(cc_gates_days_closed,
                                    cc_gates_prop_closed)

# Save delta_cross_channel_closed
saveRDS(delta_cross_channel_closed, paste0(output_direc, "delta_cross_channel_closed.rds"))

'

  # Option B: Month and year-specific values from CalSim outputs
    # New data structure: 12 (month) x 21 (year) x 2 (days vs prop closed)

dlt.gates <- gate_raw %>%
  as.data.frame() %>%
  melt(id=c("month", "year"), measure.vars=c("days_closed","prop_closed"), value.name="value") %>%
  acast(month ~ year ~ variable, value.var="value")

# Save dlt.gates
saveRDS(dlt.gates, paste0(output_direc, "dlt.gates.rds"))
