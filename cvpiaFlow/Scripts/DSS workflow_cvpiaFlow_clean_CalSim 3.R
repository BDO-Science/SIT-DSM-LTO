
##################################
# Annotation
  # This script  creates flow input data structures, as utilized in DSMs, from CalSim 3 DSS output files
  # The 
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
  # CalSim nodes were identified using the original cvpiaData/cvpiaFlow GitHub package documentation and troubleshooting,
    # in conjunction with collaboration with CalSim 3 modelers to identify linkages between CalSim II and 3 
    # * We note possible areas of conflict or disagreement between the two CalSim versions
  # ****!! CalSim 3 node selection has not been subjected to any formal type of peer review !!****
###################################

##############################################################
# Set up directories, load libraries, identify input .dss file
##############################################################

# ***CHANGE THESE DIRECTORIES***

HEC_direc <- "C:\\Program Files\\HEC\\HEC-DSSVue\\" # Point to where your HEC-DSSVue program is located 
dss_file <- 'Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09132024'
  # All relevant dss files:
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

part_f <- "L2020A" # this is different for some files
  # Consider replacing this part of CalSim input pathing using paste0(), throughout

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
dssFile$get("/CALSIM/DXC/GATE-DAYS-OPEN//1MON/L2020A/")$values %>% head()

#############################
# Create cvpiaFlow data objects
#############################
  ################################
  # Create delta_flows data object (and dependencies: dlt_divers, dlt_divers_tot, Dlt.inf) 
    # UPDATED TO CALSIM 3 W/ MINOR QUESTIONS/ISSUES
    # Relationships to nodes come from cvpiaData annotation
    # *NOTE:the last extension, '2020D09E' (used in all subsequent DSS calls), may need to be updated with new CalSim versions
      # Updated to L2020A for CalSim 3
    # Need to convert cfs to cms for delta_total_diverted and delta_inflow
  ################################

# First create the delta_flows object from CalSim data (dataframe with 972 rows (dates 1922-2002) and 7 variables)

# Identify CalSim nodes necessary to construct DSMflow input variables
n_dlt_inflow_cfs_nodes <- c("C_SAC041", "C_CSL005")
s_dlt_inflow_cfs_nodes <- c("C_SAC029B", "D_SAC030_MOK014",
                            "C_MOK022", "C_CLV004", "C_SJR056")  
n_dlt_div_cfs_nodes <- c("C_CSL004B", "DD_SAC017_SACS")
s_dlt_div_cfs_nodes <- c("D_OMR028_DMC000", "D_OMR027_CAA000",
                         "DD_SJR026_SJRE", "DD_SJR013_SJRW",
                         "DD_MOK004_MOK", "DD_OMR027_OMR",
                         "D_RSL004_CCC004", "D_OMR021_ORP000",
                         "D_VCT002_ORP000")

# Caveats on updating to CalSim 3 from CalSim II
  # Node C401B is now split into C_SAC029B and D_SAC030_MOK014
  # Nodes D403A-D are now represented by just C_CSL004B
  # There's a fundamental difference in North Delta diversions between II and 3
    # Moving from D404 to DD_SAC017_SACS adds 200 TAF annually, due to differences in assumptions in consumptive use
    # Right now, there's nothing we can do to address this, other than acknowledging the difference

# Old nodes for CalSim II
  # n_dlt_inflow_cfs_nodes <- c("C400", "C157") 
  # s_dlt_inflow_cfs_nodes <- c("C401B", "C504", "C508", "C644")
  # n_dlt_div_cfs_nodes <- c("D403A", "D403B", "D403C", "D403D","D404")
  # s_dlt_div_cfs_nodes <- c("D418", "D419", "D412", "D410", "D413", "D409B", "D416", "D408_OR", "D408_VC")

nodes <- c(n_dlt_inflow_cfs_nodes, s_dlt_inflow_cfs_nodes,
           n_dlt_div_cfs_nodes, s_dlt_div_cfs_nodes)
  
# Create indexing data frame for pulling multiple CalSim nodes from the .dss file  
nodes_df <- data.frame(input=c(rep("n_dlt_inflow_cfs",2),
                               rep("s_dlt_inflow_cfs",5),
                               rep("n_dlt_div_cfs",2),
                               rep("s_dlt_div_cfs",9)),
                       nodes=nodes,
                       type=c(rep("CHANNEL",3),
                              "DIVERSION",
                              rep("CHANNEL",4),
                              rep("DIVERSION",10)))

# Create blank data frame for storing CalSim output values
delta_flows_raw <- data.frame(date=Date(), input=character(),
                              node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame  
for(i in 1:length(nodes_df$input)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                          nodes_df$type[i],"//1MON/L2020A/"))
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
    # UPDATED TO CALSIM 3 W/ MINOR CONNECTION COMMENTS/QUESTIONS
    # Relationships to nodes come from cvpiaData annotation
    # We now provide CalSim 3 based values for Mokelumne
    # For prop.pulse, we have to calculate using median and sd() from 1980-2000
      # We also remove the /100 step, as it's already performed in the model
      # We also have to convert to tibble and add watershed column to match formatting
    # For retQ, have to convert to data frame
  ###########

# Create the flows_cfs object from CalSim data
  # Dataframe with 972 rows (dates 1922-2002) and 31 variables

# Create indexing data frame for pulling multiple CalSim nodes from the .dss file 

# New nodes for CalSim 3

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
                       nodes=c("C_SAC273", "C_ANT010", 
                               "C_BTL006", "C_BCN005", "C_BCC004",
                               "C_BTC012", "C_CLR009", "C_CWD003", 
                               "C_COW003", "C_DRC005", "C_ELD005", 
                               "C_MLC004", "C_PYN001", "C_STN004", 
                               "C_THM005", "C_SAC193", 
                               "C_CMPFW", "C_FTR059", "C_YUB002", 
                               "C_SAC093", "C_SAC048",
                               "C_NTOMA", "C_SAC063", 
                               "C_NHGAN", "C_CSM005", "C_CMCHE", 
                               "C_MCD050", "C_STS059", "C_TUO054", 
                               "C_SJR081"),
                       type="CHANNEL")


# Caveats with updating CalSim II to CalSim 3
  # C_SAC273 represents flow after all diversions in the Upper Sac, which should be similar to C104
  # C11001, which previously represented Bear and Paynes Creek, is now divided into Bear
    # (C_BCN005) and Paynes (C_PYN001) Creek flows
  # We can now model MOkelumne flows, moving from C91 to C_CMCHE
  # There are additional notes in the Excel file of connections for the following watersheds
    # Battle Creek, Big Chico Creek, Butte Creek, Cottonwood Creek, Cow Creek,
    # Deer Creek, Mill Creek, Thomes Creek, Upper-mid Sac 

# Old nodes for CalSim II
'/
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
'

# Create blank data frame for storing CalSim output values
flows_cfs_raw <- data.frame(date=Date(), input=character(),
                              node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame  
for(i in 1:length(nodes_df$input)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/L2020A/"))
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
#flows_cfs$'Mokelumne River' <- NA
# Or
#flows_cfs$'Mokelumne River' <- cvpiaFlow::flows_cfs$`Mokelumne River`

# Save flows_cfs object
saveRDS(flows_cfs, paste0(output_direc, "flows_cfs.rds"))

# Create flows_cfs_all object for use in DSM habitat estimates Line of Evidence

# Create blank data frame for storing CalSim output values for *WUA Modeling* **
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
                                 nodes_df$type[i],"//1MON/L2020A/"))
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
  #mutate(`Mokelumne River`= NA) %>% # calc comes from flows_cfs annotation
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
    # UPDATED TO CALSIM 3 W/ NO QUESTIONS (single 1:1 conversion)
    # Matrix: 12 (months) x 21 (years, 1980-2000)
    # Need to convert flows from cfs to cms
    # Relationships to nodes come from cvpiaFlow annotation
  ################

# Create data frame for indexing
nodes_df <- data.frame(nodes=c("C_SAC257"),
                       type=c("CHANNEL"))

# No caveats/questions going from II -> 3

# Old Calsim 2
'/
nodes_df <- data.frame(nodes=c("C109"),
                       type=c("FLOW-CHANNEL"))
'

# Create blank data frame for storing CalSim data
flows_raw <- data.frame(date=Date(), 
                        node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame  
for(i in 1:length(nodes_df$nodes)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/L2020A/"))
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
    # UPDATED TO CALSIM 3 W/ MINOR ISSUES/QUESTIONS
    # Data frame: 972 rows (dates, monthly, 1922-2002) with 7 variables
    # Flows stay expressed in cfs
    # Relationships to nodes come from DSMflow annotation
  ################

# Create data frame for indexing
  # OLD, CALSIM II
nodes_df <- data.frame(input=c("sutter1", "sutter2", "sutter3", "sutter4",
                               "yolo1", "yolo2"),
                       nodes=c("D117", "C135", "C136A", "C137",
                               "D160", "C157"),
                       type=c("FLOW-DELIVERY", rep("FLOW-CHANNEL",3),
                              "FLOW-DELIVERY", "FLOW-CHANNEL"))

  # NEW, CALSIM 3                            
nodes_df <- data.frame(inputs=c("sutter1", "sutter1", "sutter1", 
                                "sutter2", 
                                "sutter3", 
                                "sutter4",
                                "yolo1",
                                "yolo2"),
                       nodes=c("SP_SAC193_BTC003", "SP_SAC188_BTC003", "SP_SAC178_BTC003", 
                               "C_BTC003", 
                               "C_SBP024", 
                               "C_SSL001",
                               "SP_SAC083_YBP037",
                               "C_CSL005"),
                       type=c(rep("RIVER-SPILLS",3),rep("CHANNEL",3),
                                "RIVER-SPILLS","CHANNEL"))
    # Notes:
        # sutter2 in CalSim II (C135) includes inflow from only the Moulton Weir
          # sutter2 in CalSim 3 (C_BTC003) includes inflow from both Moulton and Coulusa Weirs
        # sutter3 in CalSim 3 (C_SBP024) includes a groundwater influence and separate water contributions to refuge
          # than sutter3 in CalSim II (C136A)

# Create blank data frame for storing CalSim data
bypass_flows_raw <- data.frame(date=Date(), input=character(),
                            node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame  
for(i in 1:length(nodes_df$input)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/L2020A/"))
  times <- rmt.java$times %>% from_time_stamp
  values <- rmt.java$values
  len_values <- length(values)
  
  df_temp <- data.frame(date=times, input=nodes_df$input[i],
                        node=nodes_df$nodes[i], value=values)
  bypass_flows_raw <- rbind(bypass_flows_raw, df_temp)
}

bypass_flows_raw <- bypass_flows_raw %>% 
  group_by(input, date) %>%
  summarise(value_sum = sum(value)) %>%
  pivot_wider(names_from="input", values_from="value_sum")

# Create bypass_flows object
bypass_flows <- bypass_flows_raw %>% 
  #select(date, input, value) %>% 
  #pivot_wider(names_from=input, values_from=value) %>%
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
    # UPDATED TO CALSIM 3, W/ SUGGESTIONS FOR FUTURE CHANGES TO CVP_, SWP_exp
      # We propose alternative nodes for CVP_exp and SWP_exp based on modeler's comments
    # (All) Matrix: 12 (months) x 21 (years, 1980-2000)
      # cvpiaData:: versions run 1980-2017, but we only need 1980-2000
    # Have to convert all of these from cfs (CalSim default) to cms
    # Relationships to nodes come from cvpiaFlow annotation
      # cvpiaData used separate calibration datasets that didn't come from Calsim!
      # ** We are updating the input source, therefore, for this application of 'Main'
    # * Note: I treated freeportQ as identical to Q_free, though Q_free came from weird calibrated dataset
  ####################################

# Create data frame for indexing
nodes_df <- data.frame(input=c("freeport_flows", "vernalis_flows", "stockton_flows",
                               rep("CVP_exports", 2), rep("SWP_exports",3)),
                       nodes=c("C_SAC041", "C_SJR070", "C_SJR053A", 
                               "DEL_CVP_TOTAL_N", "DEL_CVP_TOTAL_S",
                               "DEL_SWP_PMI", "DEL_SWP_PAG", "DEL_SWP_PIN"),
                       type=c(rep("CHANNEL",3), rep("DELIVERY-CVP",2), 
                              rep("DELIVERY-SWP",3)))

# Caveats in going from CalSim 2 -> 3, based on discussion and notes from modeler
  # CVP_exports = DEL_CVP_TOTAL_N + DEL_CVP_TOTAL_S
    # Alternative suggested term: Diversions from the Jones pumping facility
      # D418 / FLOW-DELIVERY (CalSim II)
      # D_OMR028_DMC000 / DIVERSION (CalSim 3)
  # SWP_exports = DEL_SWP_PMI + DEL_SWP_PAG + DEL_SWP_PIN
    # Alternative suggested term: Diversions from the Banks pumping facility
      # D419 / FLOW-DELIVERY (CalSim II)
      # D_OMR027_CAA000 / DIVERSION (CalSim 3)
  # Alternative terms better capture actual pumping actions, as opposed to exports
  # From modeler: 'from the CalSim side, it doesn't make any sense to use the Del_swp/cvp* 
    # terms to determine entrainment at Banks and Jones'

# Old CalSim 2 
'/
nodes_df <- data.frame(input=c("freeport_flows", "vernalis_flows", "stockton_flows",
                               "CVP_exports", "SWP_exports"),
                       nodes=c("C400", "C639", "C417A", "DEL_CVP_TOTAL", "DEL_SWP_TOTAL"),
                       type=c(rep("FLOW-CHANNEL",3), "DELIVERY-CVP", "DELIVERY-SWP"))
'

# Create blank data frame for storing CalSim data
flows_raw <- data.frame(date=Date(), input=character(),
                            node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame  
for(i in 1:length(nodes_df$input)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/L2020A/"))
  times <- rmt.java$times %>% from_time_stamp
  times2 <- times[which(year(times) %in% seq(1980,2000))]
  values <- rmt.java$values[which(year(times) %in% seq(1980,2000))]
  len_values <- length(values)
  
  df_temp <- data.frame(date=times2, input=nodes_df$input[i],
                        node=nodes_df$nodes[i], value=values)
  flows_raw <- rbind(flows_raw, df_temp)
}

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
CVP_exp <- flows_raw %>% filter(input=="CVP_exports") %>%
  group_by(month, year, input) %>%
  summarise(Tot = sum(value)) %>%
  select(Tot, month, year) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)

# Save CVP_exp object
saveRDS(CVP_exp, paste0(output_direc, "CVP_exp.rds"))

# Create SWP_exp object
SWP_exp <- flows_raw %>% filter(input=="SWP_exports") %>%
  group_by(month, year, input) %>%
  summarise(Tot = sum(value)) %>%
  select(Tot, month, year) %>%
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
    # UPDATED TO CALSIM 3 W/ NOTED ISSUES/COMMENTS
    # (All) Array: 31 (watershed) x 12 (month) x 21(year, 1980-2000)
    # Documentation for cvpiaData suggests I can keep values in cfs
      # ** Note: values for t.diver from cvpiaData only don't match for Upper Sac 
        # due to wrong R calcs (calculates p.diver instead of t.diver)
        # WE NOW GENERATE THE CORRECT VALUES FOR USE IN LTO MODELING
    # Relationships to nodes come from cvpiaData annotation
    # We now provide CalSim based values for Mokelumne
  #######################

# Create initial raw data file

# Create data frame for indexing (CalSim 3)
nodes_df <- data.frame(input=c(rep("Upper Sacramento River_Div",7), # start diversion terms
                               rep("Antelope Creek_Div",1),
                               rep("Butte Creek_Div",5),
                               rep("Deer Creek_Div",2),
                               rep("Elder Creek_Div",1), 
                               rep("Mill Creek_Div",1),
                               rep("Stony Creek_Div",1),
                               rep("Thomes Creek_Div",1),
                               rep("Upper-mid Sacramento River_Div",16), # will need to be updated
                               rep("Bear River_Div",1),
                               rep("Feather River_Div",5),
                               rep("Yuba River_Div",1),
                               rep("Lower-mid Sacramento River_Div",13), # will need to be updated
                               rep("American River_Div",1),
                               rep("Lower Sacramento River_Div",2), # will need to be updated
                               rep("Calaveras River_Div",4),
                               rep("Mokelumne River_Div",7),
                               rep("Merced River_Div",2),
                               rep("Stanislaus River_Div",2),
                               rep("Tuolumne River_Div",4),
                               rep("San Joaquin River_Div",4),
                               rep("Upper Sacramento River_Flow",1), # start flow terms
                               rep("Antelope Creek_Flow",1),
                               rep("Butte Creek_Flow",6),
                               rep("Deer Creek_Flow",1),
                               rep("Elder Creek_Flow",1), 
                               rep("Mill Creek_Flow",1),
                               rep("Stony Creek_Flow",1),
                               rep("Thomes Creek_Flow",1),
                               rep("Upper-mid Sacramento River_Flow",1),
                               rep("Bear River_Flow",2),
                               rep("Feather River_Flow",1),
                               rep("Yuba River_Flow",2),
                               rep("Lower-mid Sacramento River_Flow",1),
                               rep("American River_Flow",1),
                               rep("Lower Sacramento River_Flow",1),
                               rep("Calaveras River_Flow",1),
                               rep("Mokelumne River_Flow", 1),
                               rep("Merced River_Flow",1),
                               rep("Stanislaus River_Flow",1),
                               rep("Tuolumne River_Flow",1),
                               rep("San Joaquin River_Flow",5)),
                       nodes=c("D_SAC296_WTPFTH", "D_SAC296_02_SA", "D_SAC294_WTPBLV", "D_SAC294_03_PA", "D_SAC289_03_SA", "D_SAC281_02_NA", "D_SAC273_03_NA",
                                  # Above: Upper Sacramento River_Div (prev. D104)
                               "D_ANT010_05_NA", # Antelope Creek_Div (prev. (C11307 / (C11307+C11308+C11309) * D11305))
                               "D_BTC045_ESL008", "D_BTC043_10_NA","D_BTC036_10_NA","D_BTC012_09_SA2","D_BTC012_CRK005", # Butte Creek_Div (previously D217+C217B)
                               "D_DRC010_05_NA", "D_DRC005_05_NA", # Deer Creek_Div (prev. (C11309 / (C11307+C11308+C11309) * D11305))
                               "D_ELD012_04_NA", # Elder Creek_Div (prev. (C11303 / (C11303 + C11304) * D11301))
                               "D_MLC006_05_NA", # Mill Creek_Div (prev. (C11308 / (C11307+C11308+C11309) * D11305))
                               "D_STN021_06_PA", # Stony Creek_Div (prev. D17301)
                               "D_THM012_04_NA", # Thomes Creek_Div (prev. (C11304 / (C11303+C11304) * D11301))
                               "D_SAC240_TCC001", "D_SAC240_05_NA", "D_SAC224_04_NA", "D_SAC207_GCC007", "D_SAC196_MTC000", "D_SAC185_08N_NA", "D_SAC185_09_NA", "D_SAC178_08N_SA1", "D_SAC162_09_SA2", "D_SAC159_08S_SA1", "D_SAC159_08N_SA1", "D_SAC146_08S_NA1", "D_SAC136_18_NA", "D_SAC136_18_SA", "D_SAC129_08S_NA2", "D_SAC122_19_SA", 
                                  # Above: Upper-mid Sacramento River_Div (prev. D109+D112+D113A+D113B+D114+D118+D122A+D122B+D123+D124A+D128_WTS+D128)
                               "D_BRR017_23_NA", # Bear River_Div (prev. D285)
                               "D_THRMF_12_NU1", "D_THRMF_11_NU1", "D_THRMA_WEC000", "D_THRMA_RVC000", "D_THRMA_JBC000", # Feather River_Div (prev. D201+D202+D7A+D7B)
                               "D_YUB011_15S_NA2", # Yuba River_Div (prev. D230)
                               "D_SAC121_08S_SA3", "D_SAC115_19_SA", "D_SAC109_08S_SA3", "D_SAC109_19_SA", "D_SAC099_19_SA", "D_SAC091_19_SA", "D_SAC083_21_SA", "D_SAC082_22_SA1", "D_SAC081_21_NA", "D_SAC078_22_SA1", "D_SAC075_22_NA", "D_SAC074_21_SA", "D_SAC065_WTPBTB", 
                                  # Above: Lower-mid Sacramento River_Div (prev. D129A+D134+D162+D165)
                               "D_AMR007_WTPFBN", # American River_Div (prev. D302)
                               "D_SAC050_FPT013", "D_SAC062_WTPSAC", # Lower Sacramento River_Div (prev. D167+D168+D168A_WTS)
                               "D_LJC022_60S_PA1","D_CLV037_CACWD", "D_CLV026_60S_PA1", "D_CLV026_WTPWDH", # Calaveras River_Div (prev. D506A+D506B+D506C+D507)
                               "D_MOK050_60N_NA3","D_MOK050_60N_NA5","D_MOK039_60N_NA5","D_MOK035_60N_NA4","D_MOK035_60N_NU1","D_MOK035_WTPDWS","D_MOK033_60N_NA5", # Mokelumne River_Div (prev. D503A+D503B+D503C+D502A+D502B)
                               "D_MCD042_63_NA2","D_MCD021_63_NA4", # Merced River_Div (prev. D562+D566)
                               "D_STS030_61_NA4","D_STS004_61_NA6", # Stanislaus River_Div (prev. D528)
                               "D_TUO047_61_NA3","D_TUO047_62_NA4","D_TUO015_61_NA3","D_TUO015_62_NA4", # Tuolumne River_Div (prev. D545)
                               "D_SJR062_50_PA1","D_SJR090_71_NA2","D_SJR081_61_NA5","D_SJR116_72_NA1", # San Joaquin River_Div (prev. D637+D630B+D630A+D620B)
                               "C_SAC273", # Upper Sacramento River_Flow (prev. C104)
                               "C_ANT010", # Antelope Creek_Flow (prev. C11307)
                               "D_BTC045_ESL008", "D_BTC043_10_NA","D_BTC036_10_NA","D_BTC012_09_SA2","D_BTC012_CRK005", "C_BTC012", # Butte Creek_Flow (prev. C217B+D217+C217A)
                               "C_DRC005", # Deer Creek_Flow (prev. C11309)
                               "C_ELD005", # Elder Creek_Flow (prev. C11303)
                               "C_MLC004", # Mill Creek_Flow (prev. C11308)
                               "C_STN026", # Stony Creek_Flow (prev. C42)
                               "C_THM005", # Thomes Creek_Flow (prev. C11304)
                               "C_SAC247", # Upper-mid Sacramento River_Flow (prev. C110)
                               "C_CMPFW","D_BRR017_23_NA", # Bear River_Flow (prev. C285+D285)
                               "C_OROVL", # Feather River_Flow (prev. C6)
                               "C_YUB002","D_YUB011_15S_NA2", # Yuba River_Flow (prev. C230+D230)
                               "C_SAC120", # Lower-mid Sacramento River_Flow (prev. C128)
                               "C_NTOMA", # American River_Flow (prev. C9)
                               "C_SAC063", # Lower Sacramento River_Flow (prev. C166)
                               "C_NHGAN", # Calaveras River_Flow (prev. C92)
                               "C_CMCHE", # Mokelumne River_Flow (prev. C91)
                               "C_MCD050", # Merced River_Flow (prev. C561)
                               "C_STS059", # Stanislaus River_Flow (prev. 520)
                               "C_TUO054", # Tuolumne River_Flow (prev. 540)
                               "D_SJR062_50_PA1","D_SJR090_71_NA2","D_SJR081_61_NA5","D_SJR116_72_NA1","C_SJR072"), # San Joaquin River_Flow (prev. D637+D630B+D630A+D620B+C637) 
                       type=c(rep("DIVERSION", 81), # These will need to be updated as Sacramento River regions are updated
                              rep("CHANNEL", 2), rep("DIVERSION",5), rep("CHANNEL",8), "DIVERSION",
                              rep("CHANNEL",2), "DIVERSION", rep("CHANNEL",8), rep("DIVERSION",4),
                              "CHANNEL"))

# Caveats/questions in converting CalSim 2 to 3
  # We're removing use of all 'Closure-Terms' in CalSim 3, that would have corresponded to 'Depletion' terms in CalSim II
    # CT_BENDBRIDGE/CT_BUTTECITY/CT_WILKINSSL - Upper-mid Sacramento River_Div (prev. D109, D118, D123)
    # CT_VERONA/CT_FREEPORT - Lower-mid Sacramento River_Div (prev. D134)
    # There's no good comparison between 'Depletion' and 'Closure-Terms', based both on definitions and how models handle error
      # We also don't expect flow from 'Closure-Terms' to actually divert and kill fish
      # Furthermore, 'Closure-Terms' have the potential to be positive
  # With these updates, diversions can exceed flow in some systems:
    # Thomes Creek, Mill Creek, Deer Creek, Calaveras River
    # Had to add a upper boundary of 1 to the calculations
  # D11301, previously used for Elder and Thomes Creek, also encompasses diversions off the Sacramento River (D_SAC224_04_NA)
    # I've removed this diversion and only used diversions directly off the Elder and Thomes Creek, respectively
  # D11305, previously used for Antelope/Deer/Mill Creek, also encompasses diversions off the Sacramento River (D_SAC240_05_NA)
    # I've removed this diversion and only used diversions directly off the Elder and Thomes Creek, respectively
  # Mokelumne River deliveries in CalSim 3 represent "all the diversions on the Mokelumne River below Camanche reservoir"
    # This is our best approximation of matching CalSim II deliveries for the time being
  # D528 (Stanislaus) and D545 (Tuolumne) have multiple joint diversion terms
  # Does C104 (Upper Sac) represent flow minus all previous Upper Sac diversions?
    # YES, but so does its replacement in CalSim 3. We can keep the same ruleset as before.
    # THIS RULESET MIGHT NEED TO BE UPDATED FOR LOGICAL CONSISTENCY
    # Proportion diverted = D104/(C104+D104)
  # C217B (Butte Creek) has multiple joint diversion terms
  # Methods for updating diversion terms for regions of the Sacramento River (Upper -> Lower)
    # See the document: 'Resolving Sacramento River boundaries.docx'
    # **Going with option 1 for the time being
    # **1 - Take CalSim II diversion terms to ID river region splits, then assign SAC diversions from CalSim 3 onto these regions
      # Upper Sacramento River:
        # Prev. D104
        # New: D_SAC296_WTPFTH, D_SAC296_02_SA, D_SAC294_WTPBLV, D_SAC294_03_PA, 
          # D_SAC289_03_SA, D_SAC281_02_NA, D_SAC273_03_NA
      # Upper-mid:
        # Prev. D109, D112, D113A, D113B, D114, D118, D122A, D122B, D123, D124A, D128_WTS, D128
        # New: D_SAC240_TCC001, D_SAC240_05_NA, D_SAC224_04_NA, D_SAC207_GCC007, 
          # D_SAC196_MTC000, D_SAC185_08N_NA, D_SAC185_09_NA, D_SAC178_08N_SA1, 
          # D_SAC162_09_SA2, D_SAC159_08S_SA1, D_SAC159_08N_SA1, D_SAC146_08S_NA1, 
          # D_SAC136_18_NA, D_SAC136_18_SA, D_SAC129_08S_NA2, D_SAC122_19_SA
      # Lower-mid:
        # Prev. D129A, D134, D162, D165
        # New: D_SAC121_08S_SA3, D_SAC115_19_SA, D_SAC109_08S_SA3, D_SAC109_19_SA, 
          # D_SAC099_19_SA, D_SAC091_19_SA, D_SAC083_21_SA, D_SAC082_22_SA1, 
          # D_SAC081_21_NA, D_SAC078_22_SA1, D_SAC075_22_NA, D_SAC074_21_SA, 
          # D_SAC065_WTPBTB
      # Lower:
        # Prev. D167, D168, D168A_WTS
        # New: D_SAC050_FPT013
    # 2 - ID river region splits based on definitive conceptual model, then assign SAC diversions from CalSim 3
      # Obtained Excel with upstream/downstream boundaries of regions based on FlowWest shapefile
        # sac_reach_bounding_coordinates.xlsx, salmonid_habitat_extents.zip
      # Shared boundaries with Cameron, received the corresponding CalSim 3 boundaries
        # Between Upper and Upper-mid: RBDD, C_SAC247 above and C_SAC240 below
          # Two diversion terms (D_SAC240_TCC001, D_SAC240_05_NA) fall on TOP of RBDD
          # Unresolved on best way to use these - we defaulted to applying to Upper-mid (see #1 above)
        # Between Upper-mid and Lower-mid: Wilkin's Slough, C_SAC122 above and C_SAC121 below
        # Between Lower-mid and Lower: American confluence, C_SAC064 above and C_SAC063 below
      # Only practical difference between this and assigning diversions based on CalSim II (above)
        # is how to assign two diversions exactly at RBDD - STILL UNRESOLVED
        # With #1 (above), D_SAC240_TCC001, D_SAC240_05_NA are applied to Upper-mid
    # 3 - Take CalSim II diversion terms, try to map CalSim 3 1:1 (INCOMPLETE/ABANDONED FOR NOW)
      # Upper Sacramento River: 
        # Prev. D104
        # New: D_WTPCSD_02_PA, D_SAC294_03_PA, D_WTPCSD_02_PU, D_WKYTN_02_PU, 
          # D_WTPJMS_03_PU1, D_WTPBLV_03_PU2, D_WTPBUK_03_PU3, D_SAC296_02_SA,
          # D_WTPFTH_02_SU, D_SAC289_03_SA, D_WTPFTH_03_SU, D_CLR009_02_NA ,
          # D_SAC281_02_NA, D_CWD009_02_NA, D_COW014_03_NA, D_SAC273_03_NA,
          # D_BCN005_03_NA, D_BTL006_03_NA,
        # Should I only include SAC terms, or **all terms for full consistency?
        # Lisa: we can justify using either, probably
          # Stick with all terms for full consistency (w/ calibration)
          # *Flag for future changes, and re-calibration, and provide and guidance
        # Discussion from Cameron: Easy to pull out D_SAC terms, but D_WTP have less clear sources...
        # *More motivation to use all for the time being
      # Upper-mid Sacramento River:
        # Prev. D109, D112, D113A, D113B, D114, D118, D122A, D122B, D123, D124A, D128_WTS, D128
        # New: D_SAC240_TCC001, D_SAC224_04_NA, D_SAC240_05_NA, D_MTC000_09_NA,
          # D_SAC185_08N_NA, D_SAC185_09_NA, D_SAC146_08S_NA1, D_SAC136_18_NA,
          # D_SAC129_08S_NA2, D_BSL001_18_NA, D_SBP031_18_NA, D_SBP021_DWRPS2,
          # D_SBP014_17S_NA, D_SBP012_DWRPS1, D_SAC207_GCC007
        # We could not map the following due to overlap with other terms/regions: 
          # D122A,D122B,D128,D128_WTS
        # We dropped D124A (Upper-mid Sac) as it refers to diversions to SITES that aren't modeled in CalSim 3
        # We removed D109, D118, D123 as closure terms
      # Lower-mid Sacramento River:
        # Prev. D129A, D134, D162, D165
        # New: NA
        # We could not map the following due to overlap with other terms/regions:
          # D129A, D162, D165
        # D134 removed as a closure term
      # Lower Sacramento River:
        # Prev. D167, D168, D168A_WTS
        # New: NA
        # We could not map the following due to overlap with other terms/regions:
          # D167, D168, D168A_WTS
        # D166B is an additional depletion term for Sac River that wasn't included in the model
          # However, there's no CalSim 3 counterpart, so we'll ignore it in this conversion


# Old CalSim 2 variables
'/                                                         
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
'

# Create blank data frame for storing CalSim data
flows_raw <- data.frame(date=Date(), input=character(),
                        node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame
for(i in 1:length(nodes_df$nodes)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/L2020A/"))
  times <- rmt.java$times %>% from_time_stamp
  times2 <- times[which(year(times) %in% seq(1980,2000))]
  values <- rmt.java$values[which(year(times) %in% seq(1980,2000))]
  len_values <- length(values)
  
  df_temp <- data.frame(date=times2, input=nodes_df$input[i],
                        node=nodes_df$nodes[i], value=values)
  flows_raw <- rbind(flows_raw, df_temp)
}

# Calculate new variables and convert cfs to cms
#flows_raw$value <- conv_unit(flows_raw$value, "ft3_per_sec", "m3_per_sec") # Only use this if you don't have access to DSMflow
#flows_raw$value <- DSMflow::cfs_to_cms(flows_raw$value)
flows_raw$month <- factor(month.abb[month(flows_raw$date)], levels=month.abb[1:12])
flows_raw$year <- year(flows_raw$date)

# Manipulate the data object
  # NEED TO ADD GROUPING AND sum() by month, year, input
flows <- flows_raw %>% 
  group_by(month, year, input) %>%
  summarise(value_sum = sum(value)) %>%
  pivot_wider(names_from="input", values_from="value_sum")

# Generate total_diverted data object

# Calculate total diversion values, subset retained columns
total_diverted_raw <- flows %>%
  mutate(`Upper Sacramento River` = `Upper Sacramento River_Div`, # correct but inconsistent with 'Main'
         #`Upper Sacramento River` = `Upper Sacramento River_Div`/`Upper Sacramento River_Flow`, # incorrect but consistent with 'Main'
         `Antelope Creek` = `Antelope Creek_Div`,
         `Battle Creek` = 0,
         `Bear Creek` = 0,
         `Big Chico Creek` = 0,
         `Butte Creek` = `Butte Creek_Div`,
         `Clear Creek` = 0,
         `Cottonwood Creek` = 0,
         `Cow Creek` = 0,
         `Deer Creek` = `Deer Creek_Div`,
         `Elder Creek` = `Elder Creek_Div`,
         `Mill Creek` = `Mill Creek_Div`,
         `Paynes Creek` = 0,
         `Stony Creek` = `Stony Creek_Div`,
         `Thomes Creek` = `Thomes Creek_Div`,
         `Upper-mid Sacramento River` = `Upper-mid Sacramento River_Div`,
         `Sutter Bypass` = 0,
         `Bear River` = `Bear River_Div`,
         `Feather River` = `Feather River_Div`,
         `Yuba River` = `Yuba River_Div`,
         `Lower-mid Sacramento River` = `Lower-mid Sacramento River_Div`,
         `Yolo Bypass` = 0,
         `American River` = `American River_Div`,
         `Lower Sacramento River` = `Lower Sacramento River_Div`,
         `Calaveras River` = `Calaveras River_Div`,
         `Cosumnes River` = 0,
         `Mokelumne River` = `Mokelumne River_Div`,
         `Merced River` = `Merced River_Div`,
         `Stanislaus River` = `Stanislaus River_Div`,
         `Tuolumne River` = `Tuolumne River_Div`,
         `San Joaquin River` = `San Joaquin River_Div`) %>%
  select(!(`American River_Div`:`Yuba River_Flow`)) %>%
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
  # No longer needed since we have Mokelumne flow and diversions
  #t.diver[27,,] <- cvpiaData::total_diversion[27,,]

# Save t.diver object
saveRDS(t.diver, paste0(output_direc, "t.diver.rds"))

# Now generate the proportion_diverted data object

# Calculate proportion_diverted values, retain select columns
proportion_diverted_raw <- flows %>%
  mutate(`Upper Sacramento River` = pmin(`Upper Sacramento River_Div`/`Upper Sacramento River_Flow`,1), 
    `Antelope Creek` = pmin(ifelse(`Antelope Creek_Flow`==0,0,`Antelope Creek_Div`/`Antelope Creek_Flow`),1), # correction to remove INF
    `Battle Creek` = 0,
    `Bear Creek` = 0,
    `Big Chico Creek` = 0,
    `Butte Creek` = pmin(`Butte Creek_Div`/`Butte Creek_Flow`,1),
    `Clear Creek` = 0,
    `Cottonwood Creek` = 0,
    `Cow Creek` = 0,
    `Deer Creek` = pmin(ifelse(`Deer Creek_Flow`==0,0,`Deer Creek_Div`/`Deer Creek_Flow`),1),
    `Elder Creek` = pmin(ifelse(`Elder Creek_Flow`==0,0,`Elder Creek_Div`/`Elder Creek_Flow`),1),
    `Mill Creek` = pmin(ifelse(`Mill Creek_Flow`==0,0,`Mill Creek_Div`/`Mill Creek_Flow`),1),
    `Paynes Creek` = 0,
    `Stony Creek` = pmin(ifelse(`Stony Creek_Flow`==0,0,`Stony Creek_Div`/`Stony Creek_Flow`),1),
    `Thomes Creek` = pmin(ifelse(`Thomes Creek_Flow`==0,0,`Thomes Creek_Div`/`Thomes Creek_Flow`),1),
    `Upper-mid Sacramento River` = pmin(`Upper-mid Sacramento River_Div`/`Upper-mid Sacramento River_Flow`,1),
    `Sutter Bypass` = 0,
    `Bear River` = pmin(`Bear River_Div`/`Bear River_Flow`,1),
    `Feather River` = pmin(`Feather River_Div`/`Feather River_Flow`,1),
    `Yuba River` = pmin(`Yuba River_Div`/`Yuba River_Flow`,1),
    `Lower-mid Sacramento River` = pmin(`Lower-mid Sacramento River_Div`/`Lower-mid Sacramento River_Flow`,1),
    `Yolo Bypass` = 0,
    `American River` = pmin(`American River_Div`/`American River_Flow`,1),
    `Lower Sacramento River` = pmin(`Lower Sacramento River_Div`/`Lower Sacramento River_Flow`,1),
    `Calaveras River` = pmin(ifelse(`Calaveras River_Flow`==0,0,`Calaveras River_Div`/`Calaveras River_Flow`),1),
    `Cosumnes River` = 0,
    `Mokelumne River` = pmin(`Mokelumne River_Div`/`Mokelumne River_Flow`,1),
    `Merced River` = pmin(`Merced River_Div`/`Merced River_Flow`,1),
    `Stanislaus River` = pmin(`Stanislaus River_Div`/`Stanislaus River_Flow`,1),
    `Tuolumne River` = pmin(`Tuolumne River_Div`/`Tuolumne River_Flow`,1),
    `San Joaquin River` = pmin(`San Joaquin River_Div`/`San Joaquin River_Flow`,1)) %>%
  select(!(`American River_Div`:`Yuba River_Flow`)) %>%
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
p.diver <- proportion_diverted_raw %>% 
  mutate(value_bound = pmin(value, 1)) %>% # prevent diversions from exceeding 1 (occurring on Mill/Deer/Thomes Creek, Calaveras River)
  select(month, year, input, value_bound) %>%
  melt(id=c("input", "month","year")) %>%
  acast(input ~ month ~ year)
summary(p.diver) # check for NAs due to 0 flow values in the denominator
  # Also check for p.diver values > 1
  # I have catchers for most/all of problem rivers

# Assign Mokelumne River the same values as used in SIT DSM
  # No need with CalSim 3
#p.diver[27,,] <- cvpiaData::prop_diversion[27,,]

# Save p.diver object
saveRDS(p.diver, paste0(output_direc, "p.diver.rds"))

  ########################## 
  # prop.Q.bypasses, gate.top
    # UPDATED TO CALSIM 3 (UPDATE CAUSES SYSTEMATIC OVERTOPPING OF SUTTER BYPASS)
    # Arrays: 12 (month) x 21 (year, 1980-2000) x 2(Sutter/Yolo)
    # Relationships to nodes come from cvpiaFlow annotation for gat.top
    # For prop.Q.bypasses, we are sticking with rel't for Yolo Bypass from cvpiaData
      # D160/(D160+C160), as opposed to D160/C134 (from DSMflow)
    # For prop.Q.bypasses, for Sutter Bypass, I think the correct calc should be:
      # (D117+D124+D125+D126)/C116
      # Instead, I'm pretty sure cvpiaData uses propQsutter1, or D117/C116, which is always 0
        # ** WE HAVE TO DECIDE IF WE SHOULD UPDATE OR STICK WITH THIS MOVING FORWARD
        # Currently, we're using the calculation used in 'Main'
    # New Yolo Bypass values, w/ updated hydrology/operations, results in less frequent overtopping
      # The implementation should be right, so either:
        # Operations have a big change in Yolo diversion terms
        # Previous diversion values were close to 100 cfs threshold, so it doesn't take much to shift
        # Something about previous input values/calculation does not match the documentation
  ##########################

# Create data frame for indexing
nodes_df <- data.frame(inputs=c("D117", "D117", "D117", 
                                "D124", "D125", "D126", 
                                "D160", 
                                "C116", "C134", "C137", 
                                "C160", "C157"),
                       nodes=c("SP_SAC193_BTC003", "SP_SAC188_BTC003", "SP_SAC178_BTC003", 
                               "SP_SAC159_BTC003", "SP_SAC148_BTC003", "SP_SAC122_SBP021", 
                               "SP_SAC083_YBP037",
                               "C_SAC195", "C_SAC093", "C_SSL001", 
                               "C_SAC048", "C_CSL005"),
                       type=c(rep("RIVER-SPILLS",7),rep("CHANNEL",5)))

# Caveats/questions in converting CalSim II -> 3 from Cameron
  # D117 split into 3 terms
  # Connection to C116: Cameron, "This is a best guess. It looks like it was intended to be after the groundwater"
  # Connection to C137: Lots of changes in CalSim3 to connections between Sac, weirs, Feather, and Butte
    # Is there a better connection? C_SSL001 triggers constant overtopping of Sutter Bypass, even in summer, which isn't realistic
    # Modeler: new CalSim 3 potentially diverts more water into Sutter from Butte and the Sacramento
      # * Keep as is for now, and acknowledge we lose sensitivity to operations with current ruleset
      # * Consider updating the 100 cfs overtopping ruleset in the future
      # * Cameron also doesn't know why we include C137, since it already encompasses diversion flow

# Old CalSim 2 variables
'/
nodes_df <- data.frame(nodes=c("D117", "D124", "D125", "D126", "D160",
                               "C116", "C134", "C137", "C160", "C157"),
                       type=c(rep("FLOW-DELIVERY",5),rep("FLOW-CHANNEL",5)))
'

# Create blank data frame for storing CalSim data
flows_raw <- data.frame(date=Date(), input=character(),
                        node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame
for(i in 1:length(nodes_df$nodes)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/L2020A/"))
  times <- rmt.java$times %>% from_time_stamp
  times2 <- times[which(year(times) %in% seq(1980,2000))]
  values <- rmt.java$values[which(year(times) %in% seq(1980,2000))]
  len_values <- length(values)
  
  df_temp <- data.frame(date=times2, input=nodes_df$input[i],
                        node=nodes_df$nodes[i], value=values)
  flows_raw <- rbind(flows_raw, df_temp)
}

# Calculate new variables
flows_raw$month <- factor(month.abb[month(flows_raw$date)], levels=month.abb[1:12])
flows_raw$year <- year(flows_raw$date)

# Manipulate the data object
flows <- flows_raw %>% 
  group_by(input, month, year) %>%
  summarise(value_sum = sum(value)) %>%
  pivot_wider(names_from="input", values_from="value_sum")

# Create prop.Q.bypasses

# Possibly accurate, but inconsistent with current usage in Main
bypass_raw <- flows %>%
  mutate(`Sutter Bypass` = pmin((D117+D124+D125+D126)/C116, 1), # Add pmin() to prevent proportion flow from >1
         `Yolo Bypass` = pmin(D160/(D160+C160), 1)) %>%
  select(!(C116:D160)) %>%
  pivot_longer(cols=`Sutter Bypass`:`Yolo Bypass`, names_to="input", values_to = "value")

# Possibly incorrect, but consistent with current usage in Main
bypass_raw <- flows %>%
  mutate(`Sutter Bypass` = pmin((D117)/C116, 1), # Add pmin() to prevent proportion flow from >1
         `Yolo Bypass` = pmin(D160/(D160+C160), 1)) %>%
  select(!(C116:D160)) %>%
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
  select(!(C116:D160)) %>%
  pivot_longer(cols=`Sutter Bypass`:`Yolo Bypass`, names_to="input", values_to = "value")

gate.top <- gates_raw %>% select(month:value) %>%
  melt(id=c("input", "month","year")) %>%
  acast(month ~ year ~ input)

# Save gate.top
saveRDS(gate.top, paste0(output_direc, "gate.top.rds"))

  #######################
  # dlt.gates
    # UPDATED TO CALSIM 3 (no changes required)
    # Matrix: 2 (data type) x 12 (month)
      # OR Array: 12 (month) x 21 (year) x variable (days closed, prop closed)
      # Composed by row of cc_gates_days_closed & cc_gates_prop_days_closed
    # Multiple options
      # 1. Use existing values, based on 2009 BiOp
      # 2. Use expected values from 2019 BiOp
      # 3. Use updated CalSim outputs, either averaged across years (A) or year-specific (B)
        # ** I'm going with option 3B for the time being
  # Use DXC for days the gate is open
  #######################

# Option 1

delta_cross_channel_closed <- DSMflow::delta_cross_channel_closed

# Option 2 (NEED TO DIG THROUGH BiOp)

# Option 3 - Pull from recent CalSim inputs
  # Years 1980-2000, for consistency with other Delta inputs
  # TO DO:
    # Figure out if I need to round days closed to whole numbers (for option A)
    # Figure out necessary number of years to average (connect to model)

# Create data frame for indexing
nodes_df <- data.frame(nodes=c("DXC"), # the DXC node represents the realized # of days the gate is open
                       type=c("GATE-DAYS-OPEN"))

# Create blank data frame for storing CalSim data
gate_raw <- data.frame(date=Date(), 
                        node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame
for(i in 1:length(nodes_df$nodes)){
  rmt.java <- dssFile$get(paste0("/CALSIM/",nodes_df$nodes[i],"/",
                                 nodes_df$type[i],"//1MON/L2020A/"))
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
