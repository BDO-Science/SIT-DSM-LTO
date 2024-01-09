
##########################
# Annotation
  # Code pulled from cvpiaFlow GitHub
  # This script uses original DSM CalSim inputs to correct total diversions in the Upper Sacramento
    # Creates data files for use in both base DSM runs with original inputs and in recalibration efforts
##########################

require(tidyverse)
require(lubridate)
require(reshape2)

calsim <- readRDS('./winterRunDSM-Main_Calibration/Raw CalSim data for input revision/cvpia_calsim.rds')
cvpia_nodes <- read.csv('./winterRunDSM-Main_Calibration/Raw CalSim data for input revision/cvpia_calsim_nodes.csv', skip = 1)
watersheds <- "Upper Sacramento River"

need_split <- cvpia_nodes$cal_sim_flow_nodes %>% str_detect(', ')
div_split <- cvpia_nodes$cal_sim_flow_nodes[need_split] %>% str_split(', ') %>% flatten_chr()
div_flow_nodes <- c(cvpia_nodes$cal_sim_flow_nodes[!need_split], div_split)

need_split <- cvpia_nodes$cal_sim_diversion_nodes %>% str_detect(', ')
div_split <- cvpia_nodes$cal_sim_diversion_nodes[need_split] %>% str_split(', ') %>% flatten_chr()
div_nodes <- c(cvpia_nodes$cal_sim_diversion_nodes[!need_split], div_split)
diversion_nodes <- div_nodes[!is.na(div_nodes)] %>% str_trim('both') %>% str_replace(',', '')

combined_flow_nodes <- c('C11305', 'C11301')

all_div_nodes <- c(div_flow_nodes, diversion_nodes, combined_flow_nodes, 'date') %>% unique()
all_div_nodes
node_columns <- names(calsim) %in% all_div_nodes

div_calsim <- calsim[, node_columns]

# total diverted for only Upper Sacramento

temp_diver <- div_calsim %>%
  mutate(`Upper Sacramento River` = D104) %>%
  select(date, watersheds[-27]) %>%
  mutate(month=month(date), year=year(date))

# Convert data.frame into 1 x 12 (months) x 21 (years, 1980-2000) array
new.t.diver <- temp_diver %>% select(`Upper Sacramento River`:year) %>%
  filter(year %in% c(1980:2000)) %>%
  reshape2::melt(id=c("month","year")) %>%
  acast(month ~ year)

# Save new.t.diver as .rds

saveRDS(new.t.diver, "./winterRunDSM-Main_Calibration/New t.diver data for calibration/new.t.diver.rds")


# Do the same for t.diver[2,,] (Antelope Creek), then compare values to double-check workflow and input data are correct
  # Within this workflow, I can also make sure I'm creating the correct synthetic time series
  # Checks out - can re-run code below to double-check if interested
'/
temp_diver <- div_calsim %>%
  mutate(`Antelope Creek` = (C11307/(C11307+C11308+C11309)*D11305)) %>%
  select(date, `Antelope Creek`) %>%
  mutate(month=month(date), year=year(date))

# Convert data.frame into 1 x 12 (months) x 21 (years, 1980-2000) array
new.t.diver <- temp_diver %>% select(`Antelope Creek`:year) %>%
  filter(year %in% c(1980:2000)) %>%
  reshape2::melt(id=c("month","year")) %>%
  acast(month ~ year)

new.t.diver - cvpiaData::total_diversion[2,,]

synth.t.diver <- array(NA, dim=c(12,20))

old.index.df <- data.frame(year.index=1:21,
                           year=1980:2000)
new.index.df <- data.frame(year.index = 1:20,
                       year = 1998:2017,
                       year.proxy.OG = c(1998,1999,1993,1981,1989,
                                         1993,1993,1993,1998,1994,
                                         1988,1994,1985,1997,1985,
                                         1994,1992,1992,1989,1998),
                       year.proxy.FW = c(1998,1999,2000,1987,1985,
                                      2000,1985,1980,1995,1989,
                                      1994,1981,1985,1997,1985,
                                      1987,1992,1992,1985,1998))

for(i in 1:20){
  synth.t.diver[,i] <- new.t.diver[,which(old.index.df$year==new.index.df$year.proxy.OG[i])]
}

# obtain t.diver by running Wrapper script_calibration.R
synth.t.diver - t.diver[2,,]
'
# Identify and confirm synthetic year time series for both FlowWest and cvpiaCalibration
  # Identified OG time series at cvpiaCalibration: https://github.com/FlowWest/cvpiaCalibration/tree/master/data-raw 
    # Obtained 'water_year_synthetic_series.xlsx', saved in 'winterRunDSM-Main_Calibration'
  # Identified FlowWest time series for Rearing Inputs at: https://cvpia-osc.github.io/winterRunDSM/articles/calibration-2021.html
  # Above, I identified that original calibration used the OG time series, so I'll use the same here
    # ** Need to annotate this somewhere **

old.index.df <- data.frame(year.index=1:21,
                           year=1980:2000)
new.index.df <- data.frame(year.index = 1:20,
                           year = 1998:2017,
                           year.proxy.OG = c(1998,1999,1993,1981,1989,
                                             1993,1993,1993,1998,1994,
                                             1988,1994,1985,1997,1985,
                                             1994,1992,1992,1989,1998),
                           year.proxy.FW = c(1998,1999,2000,1987,1985,
                                             2000,1985,1980,1995,1989,
                                             1994,1981,1985,1997,1985,
                                             1987,1992,1992,1985,1998))

# Shuffle year structure of t.diver[1,,]

synth.t.diver <- array(NA, dim=c(12,20))

for(i in 1:20){
  synth.t.diver[,i] <- new.t.diver[,which(old.index.df$year==new.index.df$year.proxy.OG[i])]
}

# Save t.diver[1,,] as .rds

saveRDS(synth.t.diver, "./winterRunDSM-Main_Calibration/New t.diver data for calibration/synth.t.diver.rds")
