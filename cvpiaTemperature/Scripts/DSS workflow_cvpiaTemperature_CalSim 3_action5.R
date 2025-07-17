
##################################
# Annotation
  # This script creates temperature input data structures, as utilized in DSMs, from HEC-5Q DSS output files
    # Provides new temperatures for Upper Sacramento River, Cottonwood Creek, and Clear Creek
  # Notes on running this script
    # Specify which alternative you want to generate data inputs from in 'Set up directories...', below
      # by changing 'HEC_file' and 'dss_file'
    # Users will need to install cvpiaData and cvpiaTemperature R packages from GitHub
      # e.g., # remotes::install_github("FlowWest/CVPIAdata")
      # remotes::install_github("FlowWest/cvpiaTemperature")
    # * This script requires users install HEC-DSSVue and update the 'HEC_direc' object, below,
      # in order to correctly tell R where to find the HEC-DSSVUE program
      # This program is required to interact with DSS data objects produced by CalSim
      # * We note that a different version of HEC-DSSVue may cause problems with this script
    # * This script also requires users install and load the following packages:
      # plyr, dplyr, stringr, lubridate, rJava, ggplot2, tidyr, reshape2, grid, measurements
    # We note there was some uncertainty as to how best update Upper Sacramento temperatures
      # This uncertainty is documented in the code below
  # HEC-5Q nodes were identified using a combination of cvpiaData, cvpiaTemperature GitHub package documentation and troubleshooting
###################################

##############################################################
# Set up directories, load libraries, identify input .dss file
##############################################################

# ***CHANGE THESE DIRECTORIES***

HEC_direc <- "C:\\Program Files\\HEC\\HEC-DSSVue\\" # Point to where your HEC-DSSVue program is located 
#dss_file <- 'LTO_NAA_HistHydro_HistSC'
HEC_file <- "CS3_Alt5_JPF_wTUCP_2022MED_0525_dv 1"
  # All alternative-specific HEC file locations:
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
    # "CS3_Alt5_JPF_wTUCP_2022MED_0525_dv 1"
dss_file <- 'SR_WQ_Report'

# These directories do not need to be changed

dss_direc <- paste0('./cvpiaTemperature/HEC5Q Input/',HEC_file,"/",dss_file,".dss")
output_direc <- paste0('./cvpiaTemperature/Final Data Objects/',HEC_file,"/")

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
dssFile$get("/SACRAMENTO/JELLYS FERRY/TEMP_F//1Day/R2019/")$values %>% head

#############################
# Create cvpiaTemperature data objects
#############################

  ############################
  # DegDay, juv.tmp
    # Goal: Update values for Upper Sacramento River ONLY with either:
      # Average of BLW KESWICK and RED BLUFF DAM HEC-5Q variables (v1 - Drew Loney)
        # OR
      # BLW CLEAR CREEK HEC-5Q variable (v2 - Mike Wright)
    # The variable for updating USR is uncertain b/c documentation shows COTTONWOOD CR
      # However, USR temperatures are different than Cottonwood Creek, which theoretically pulls from the same node
      # Some adjustment was made to USR that was not accurately documented
    # DegDay (from cvpiaData::load_baseline_data('winter'))
      # Identical to cvpiaData::degday
    # juv.tmp (from cvpiaData::load_baseline_data('winter'))
      # Identical to cvpiaData::rearing_temps
  ############################

nodes_df <- data.frame(input=c("Upper Sacramento River_v1", "Upper Sacramento River_v1",
                               "Upper Sacramento River_v2", 
                               "Clear Creek",
                               "Cottonwood Creek"), 
                       partA=c("SACRAMENTO", "SACRAMENTO", "SACRAMENTO",
                               "CLEAR CREEK", 
                               "SACRAMENTO"),
                       partB=c("BLW KESWICK", "RED BLUFF DAM", 
                               "BLW CLEAR CREEK",
                               "IGO",
                               "COTTONWOOD CR"),
                       partF=c("R2019","R2019","R2019","R2019","R2019"))

hec_raw <- data.frame(date=Date(), input=character(),
                        node=character(), value=numeric())

# Extract values from CalSim nodes, store in data frame  
for(i in 1:length(nodes_df$input)){
  rmt.java <- dssFile$get(paste0("/",nodes_df$partA[i],"/",nodes_df$partB[i],
                                 "/TEMP_F//1Day/",nodes_df$partF[i],"/"))
  times <- rmt.java$times %>% from_time_stamp
  times2 <- times[which(year(times) %in% seq(1922,2002))]
  values <- rmt.java$values[which(year(times) %in% seq(1922,2002))]
  len_values <- length(values)
  
  df_temp <- data.frame(date=times2, input=nodes_df$input[i], 
                        node=nodes_df$partB[i], value=values)
  hec_raw <- rbind(hec_raw, df_temp)
}

# Create juv.tmp.USR object with available data
  # juv.tmp for only the Upper Sacramento River

  # v1 (avg of two temperatures)

juv.tmp.USR.v1 <- hec_raw %>% 
  mutate(value = ((value-32)*(5/9))) %>% # convert temps from F to C
  filter(input=="Upper Sacramento River_v1") %>%
  group_by(date, input) %>%
  summarise(value_avg = mean(value)) %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), 
         year=year(date)) %>%
  filter(year %in% c(1980:2000)) %>%
  group_by(month, year) %>%
  summarize(mean_temp = mean(value_avg)) %>%
  ungroup() %>%
  select(month, year, mean_temp) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)

  # v2 (below Clear Creek temperature)

juv.tmp.USR.v2 <- hec_raw %>% 
  mutate(value = ((value-32)*(5/9))) %>% # convert temps from F to C
  filter(input=="Upper Sacramento River_v2") %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), 
         year=year(date)) %>%
  filter(year %in% c(1980:2000)) %>%
  group_by(month, year) %>%
  summarize(mean_temp = mean(value)) %>%
  ungroup() %>%
  select(month, year, mean_temp) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)

  # Explore v1 vs v2
hist(juv.tmp.USR.v2-juv.tmp.USR.v1)
  # v2 tends, on average, to be 0.5 to 1 C cooler than v1
  # I think v1 is better as a representation of rearing conditions?
    # v2 may be a better representation of spawning/holding conditions?
  
# Pull in original rearing temperatures data file
juv.tmp <- cvpiaData::rearing_temps
  hist(juv.tmp[1,,]-juv.tmp.USR.v1); summary(c(juv.tmp[1,,]-juv.tmp.USR.v1))
  hist(juv.tmp[1,,]-juv.tmp.USR.v2); summary(c(juv.tmp[1,,]-juv.tmp.USR.v2))
  # Given the test HEC-5Q file has a 2035 hydrology with an expected 0.5-1 F bump in temperature,
    # v1 might be most representative of original conditions.
    # V1, on average, is 0.6 C (1.2 F) warmer than the 2019 version
      # v2, on average, is only 0.06 C (0.1 F) warmer than the 2019 version
    # HOWEVER, trends in residuals for v2 is most similar to those for Clear and Cottonwood Creeks
      # For consistency, let's roll with v2

# New Clear Creek temperatures
  
juv.tmp.Clear <- hec_raw %>% 
  mutate(value = ((value-32)*(5/9))) %>% # convert temps from F to C
  filter(input=="Clear Creek") %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), 
         year=year(date)) %>%
  filter(year %in% c(1980:2000)) %>%
  group_by(month, year) %>%
  summarize(mean_temp = mean(value)) %>%
  ungroup() %>%
  select(month, year, mean_temp) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)
hist(juv.tmp[7,,]-juv.tmp.Clear) # pretty much centered on zero, in contrast to selected Upper Sac option

# New Cottonwood Creek temperatures

juv.tmp.Cottonwood <- hec_raw %>% 
  mutate(value = ((value-32)*(5/9))) %>% # convert temps from F to C
  filter(input=="Cottonwood Creek") %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), 
         year=year(date)) %>%
  filter(year %in% c(1980:2000)) %>%
  group_by(month, year) %>%
  summarize(mean_temp = mean(value)) %>%
  ungroup() %>%
  select(month, year, mean_temp) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)
hist(juv.tmp[8,,]-juv.tmp.Cottonwood)

# Replace original Upper Sacramento River/Clear Creek/Cottonwood Creek rearing temperatures with new
  # Choose v2 as a reasonable, similar replacement to OG values for USR the time being
juv.tmp[1,,] <- juv.tmp.USR.v2
juv.tmp[7,,] <- juv.tmp.Clear
juv.tmp[8,,] <- juv.tmp.Cottonwood

# Save juv.tmp
saveRDS(juv.tmp, paste0(output_direc, "juv.tmp.rds"))

# Use cvpiaData::degday for filling in Degree Day data for non Upper Sacramento values

# Create DegDay.USR object with available data
  # DegDay for only the Upper Sacramento River

  # v1 (avg of two temperatures)

DegDay.USR.v1 <- hec_raw %>% 
  mutate(value = ((value-32)*(5/9))) %>% # convert temps from F to C
  filter(input=="Upper Sacramento River_v1") %>%
  group_by(date, input) %>%
  summarise(value_avg = mean(value)) %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), 
         year=year(date)) %>%
  filter(year %in% c(1979:2000)) %>%
  group_by(month, year) %>%
  summarize(degday = sum(value_avg)) %>%
  ungroup() %>%
  select(month, year, degday) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)

  # v2 (below Clear Creek temperature)

DegDay.USR.v2 <- hec_raw %>% 
  mutate(value = ((value-32)*(5/9))) %>% # convert temps from F to C
  filter(input=="Upper Sacramento River_v2") %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), 
         year=year(date)) %>%
  filter(year %in% c(1979:2000)) %>%
  group_by(month, year) %>%
  summarize(degday = sum(value)) %>%
  ungroup() %>%
  select(month, year, degday) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)

  # Explore v1 vs v2
hist(DegDay.USR.v2-DegDay.USR.v1)
  # v2 tends, on average, to be cooler than v1
    # I think v1 is maybe better as a representation of rearing conditions?
    # v2 may be a better representation of spawning/holding conditions?

# Pull in original rearing temperatures data file
DegDay <- cvpiaData::degday
  hist(DegDay[1,,]-DegDay.USR.v1); summary(c(DegDay[1,,]-DegDay.USR.v1))
  hist(DegDay[1,,]-DegDay.USR.v2); summary(c(DegDay[1,,]-DegDay.USR.v2))
  # Given the test HEC-5Q file has a 2035 hydrology with an expected 0.5-1 F bump in temperature,
    # v1 might be most representative of original conditions.
    # V1, on average, is 18 C degree-days warmer than the 2019 version
    # v2, on average, is only 1.5 C degree days warmer than the 2019 version

  
# New Clear Creek DegDay

DegDay.Clear <- hec_raw %>% 
  mutate(value = ((value-32)*(5/9))) %>% # convert temps from F to C
  filter(input=="Clear Creek") %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), 
         year=year(date)) %>%
  filter(year %in% c(1979:2000)) %>%
  group_by(month, year) %>%
  summarize(degday = sum(value)) %>%
  ungroup() %>%
  select(month, year, degday) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)
hist(DegDay[7,,]-DegDay.Clear); summary(c(DegDay[7,,]-DegDay.Clear))

# New Cottonwood Creek DegDay

DegDay.Cottonwood <- hec_raw %>% 
  mutate(value = ((value-32)*(5/9))) %>% # convert temps from F to C
  filter(input=="Cottonwood Creek") %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb[1:12]), 
         year=year(date)) %>%
  filter(year %in% c(1979:2000)) %>%
  group_by(month, year) %>%
  summarize(degday = sum(value)) %>%
  ungroup() %>%
  select(month, year, degday) %>%
  melt(id=c("month", "year")) %>%
  acast(month~year)
hist(DegDay[8,,]-DegDay.Cottonwood); summary(c(DegDay[8,,]-DegDay.Cottonwood))

# Replace original Upper Sacramento River/Clear Creek/Cottonwood Creek rearing temperatures with new
# Choose v2 as a reasonable, similar replacement to OG values for USR for the time being
DegDay[1,,] <- DegDay.USR.v2
DegDay[7,,] <- DegDay.Clear
DegDay[8,,] <- DegDay.Cottonwood

# Save DegDay
saveRDS(DegDay, paste0(output_direc, "DegDay.rds"))
