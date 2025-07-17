
##########################
# Annotation
  # This script imports habitat estimates created by 'WUA_Wrapper.R' and plots summaries across alternatives
  # Justification for the selected spawner abundances is provided in the analysis documentation
  # Notes on running this analysis
    # * Ensure all libraries called in the 'Set up' block of code are installed on your computer
    # * You can modify which alternatives you compare by changing the following objects:
      # n.alt, alt_plot_names, alt_file_names
##########################

##############
# Set up 
  # Call libraries
  # Establish directories
  # Identify alternatives to summarize habitat for
##############

# Call libraries
require(here)
require(dplyr)
require(tidyr) 
require(reshape2) 
require(ggplot2)
require(lubridate)

# Install necessary functions
func_dir <- "./R/"
func_names <- list.files(func_dir) %>%
  .[substr(.,1,3)%in%c("set","uti")]
for(i in 1:length(func_names)){source(paste0(func_dir,func_names[i]))}

# Identify alternative(s), set up comparisons of interest

  # Example
n.alt <- 1
alt_plot_names <- c("Ex CalSim 3")
alt_file_names <- c("CS3_L2020_DV_2021_ext_2040MED") # CalSim 3 example
out.name <- "Test"

  # EIS (NAA, Alternatives 1-5)
n.alt <- 8
alt_plot_names <- c("NAA","ALT1","PA v1 wTUCP","PA v1 woTUCP","PA v2 noTUCP","PA v3 noTUCP", "ALT3","ALT4")
alt_file_names <- c("Reclamation_2021LTO_CalSim3_NAA_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_Alt1_2022MED_09092023",
                    "Reclamation_2021LTO_CalSim3_Alt2v1_wTUCP_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_090723",
                    "Reclamation_2021LTO_CalSim3_Alt2v3_noTUCP_2022MED_090723",
                    "Reclamation_2021LTO_CalSim3_ALT3_2022MED_092423",
                    "Reclamation_2021LTO_CalSim3_Alt4_2022MED_09082023")
out.name <- "EIS"

  # BA (NAA, EXP1, EXP3, PA (ALT2; four versions))
n.alt <- 7
alt_plot_names <- c("NAA","EXP1","EXP3","PA v1 wTUCP","PA v1 woTUCP", "PA v2 noTUCP", "PA v3 noTUCP")
alt_file_names <- c("Reclamation_2021LTO_CalSim3_NAA_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_EXP1_2022MED_rev10_090623_dynGWSW",
                    "Reclamation_2021LTO_CalSim3_EXP3_2022MED_rev10_090623_dynGWSW",
                    "Reclamation_2021LTO_CalSim3_Alt2v1_wTUCP_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_090723",
                    "Reclamation_2021LTO_CalSim3_Alt2v3_noTUCP_2022MED_090723")
out.name <- "BA"

  # Ch 4 (NAA, EXP1, EXP3, PA (ALT2; two versions))
n.alt <- 5
alt_plot_names <- c("No Action Alternative (NAA)","Run of River (EXP1)",
                    "Minimum Release (EXP3)","Proposed Action no VA no TUCP (ALT2 v1)", 
                    "Proposed Action Delta VAs no TUCP (ALT2 v2)")
alt_file_names <- c("Reclamation_2021LTO_CalSim3_NAA_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_EXP1_2022MED_rev10_090623_dynGWSW",
                    "Reclamation_2021LTO_CalSim3_EXP3_2022MED_rev10_090623_dynGWSW",
                    "Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_090723")
out.name <- "Ch4"

# Establish directories
out_dir <- paste0("./WUA_LTO/Habitat output/",alt_file_names,"/")

####################
# Plotting
####################

  ##################################################
  # Spawning habitat area + spawner limitation ratio
  ##################################################

# Load WYT reference for plotting
wyt_ref <- read.csv("./wyt_ref.csv")

spawn_hab_df_final <- data.frame("Watershed"=character(),
                                 "WY"=numeric(),
                                 "Month"=numeric(),
                                 "Run-Type"=character(),
                                 "value"=numeric(),
                                 "Alternative"=character())

for(i in 1:n.alt){

  # Import spawning habitat data
  spawn_hab <- readRDS(paste0(out_dir[i],alt_file_names[i],"_WUA_SPAWN_Ch4.rds"))
  
  # Convert spawn_hab to acres for plotting
  spawn_hab <- spawn_hab %>% square_meters_to_acres()
  
  # convert back to a data.frame (see Wrapper script.R, stochastic)
  spawn_hab_df <- spawn_hab %>%
    reshape2::melt(varnames=c("Watershed","WY","Month","Run-Type")) %>%
    drop_na(value) %>%
    mutate(Alternative=alt_plot_names[i])
  
  spawn_hab_df_final <- rbind(spawn_hab_df_final, spawn_hab_df)
  
}
spawn_hab_df_final$Alternative <- factor(spawn_hab_df_final$Alternative,
                                         levels=alt_plot_names[c(2,3,1,4,5,6,7)])
spawn_hab_df_final_wyt <- spawn_hab_df_final %>%
  right_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W"))) %>%
  drop_na()

# WR
spawn_hab_df_wr <- spawn_hab_df_final %>% filter(`Run-Type`=="wr")
wr_spawn <- ggplot(spawn_hab_df_wr, aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  #facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); wr_spawn
wr_spawn_bar <- ggplot(spawn_hab_df_wr %>%
                         group_by(Month, Alternative) %>%
                         summarize(Mean=mean(value)), 
                       aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  #facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); wr_spawn_bar

# WR by WYT
spawn_hab_df_wr <- spawn_hab_df_final_wyt %>% filter(`Run-Type`=="wr")
wr_spawn_wyt <- ggplot(spawn_hab_df_wr, aes(x=WYT, y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Water Year Type", fill="")+
  facet_wrap(~factor(Month))+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); wr_spawn
wr_spawn_bar <- ggplot(spawn_hab_df_wr %>%
                         group_by(Month, Alternative) %>%
                         summarize(Mean=mean(value)), 
                       aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  #facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); wr_spawn_bar

# WR Spawner Limitations - Upper Sacramento River
spawn_hab_df_wr <- spawn_hab_df_final %>% 
  filter(`Run-Type`=="wr") %>%
  mutate(value = acres_to_square_meters(value), # convert to square meters
         `Spawner Capacity` = value/9.29, # calculate female spawner capacity
         `Spawner:Capacity Ratio`=2777/`Spawner Capacity`) # calculate spawner ratio
wr_spawn_limit <- ggplot(spawn_hab_df_wr, aes(x=factor(Month), y=`Spawner:Capacity Ratio`, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawner Habitat Limitation Ratio", x="Month", fill="")+
  facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE))

# SR
spawn_hab_df_sr <- spawn_hab_df_final %>% filter(`Run-Type`=="sr")
sr_spawn <- ggplot(spawn_hab_df_sr, aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_spawn
sr_spawn_USR <- ggplot(spawn_hab_df_sr %>% filter(Watershed=="Upper Sacramento River"), 
                       aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_spawn_USR
sr_spawn_CC <- ggplot(spawn_hab_df_sr %>% filter(Watershed=="Clear Creek"), 
                      aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_spawn_CC

sr_spawn_USR_bar <- ggplot(spawn_hab_df_sr %>% 
                             filter(Watershed=="Upper Sacramento River") %>%
                             group_by(Month, Alternative) %>%
                             summarize(Mean=mean(value)), 
                       aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_spawn_USR_bar
sr_spawn_CC_bar <- ggplot(spawn_hab_df_sr %>% 
                            filter(Watershed=="Clear Creek") %>%
                            group_by(Month, Alternative) %>%
                            summarize(Mean=mean(value)), 
                      aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_spawn_CC_bar

# SR by WYT
spawn_hab_df_sr <- spawn_hab_df_final_wyt %>% filter(`Run-Type`=="sr")
sr_spawn_wyt <- ggplot(spawn_hab_df_sr, aes(x=WYT, y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Water Year Type", fill="")+
  facet_grid(factor(Month)~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_spawn_wyt
sr_spawn_USR_wyt <- ggplot(spawn_hab_df_sr %>% filter(Watershed=="Upper Sacramento River"), 
                       aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_spawn_USR_wyt
sr_spawn_CC_wyt <- ggplot(spawn_hab_df_sr %>% filter(Watershed=="Clear Creek"), 
                      aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_spawn_CC_wyt


# SR Spawner Limitations - Upper Sacramento River, Clear Creek
spawn_hab_df_sr <- spawn_hab_df_final %>% 
  filter(`Run-Type`=="sr") %>%
  mutate(value = acres_to_square_meters(value), # convert to square meters
         `Spawner Capacity` = value/9.29, # calculate female spawner capacity
         `Spawner:Capacity Ratio`=ifelse(Watershed=="Upper Sacramento River", # calculate spawner ratio
                                         48/`Spawner Capacity`, # Upper Sacramento River
                                         109/`Spawner Capacity`)) # Clear Creek
sr_spawn_limit <- ggplot(spawn_hab_df_sr, 
                         aes(x=factor(Month), y=`Spawner:Capacity Ratio`, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawner Habitat Limitation Ratio", x="Month", fill="")+
  facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE))
sr_spawn_limit_USR <- ggplot(spawn_hab_df_sr %>% filter(Watershed=="Upper Sacramento River"), 
                             aes(x=factor(Month), y=`Spawner:Capacity Ratio`, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawner Habitat Limitation Ratio", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_spawn_limit_USR
sr_spawn_limit_CC <- ggplot(spawn_hab_df_sr %>% filter(Watershed=="Clear Creek"), 
                            aes(x=factor(Month), y=`Spawner:Capacity Ratio`, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawner Habitat Limitation Ratio", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_spawn_limit_CC

# ST
spawn_hab_df_st <- spawn_hab_df_final %>% filter(`Run-Type`=="st")
st_spawn <- ggplot(spawn_hab_df_st, aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn
st_spawn_USR <- ggplot(spawn_hab_df_st %>% filter(Watershed=="Upper Sacramento River"), 
                       aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_USR
st_spawn_CC <- ggplot(spawn_hab_df_st %>% filter(Watershed=="Clear Creek"), 
                      aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_CC
st_spawn_AR <- ggplot(spawn_hab_df_st %>% filter(Watershed=="American River"), 
                      aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_AR
st_spawn_SR <- ggplot(spawn_hab_df_st %>% filter(Watershed=="Stanislaus River"), 
                      aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_SR

st_spawn_USR_bar <- ggplot(spawn_hab_df_st %>% 
                             filter(Watershed=="Upper Sacramento River") %>%
                             group_by(Month, Alternative) %>%
                             summarise(Mean=mean(value)), 
                       aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_USR_bar
st_spawn_CC_bar <- ggplot(spawn_hab_df_st %>% 
                            filter(Watershed=="Clear Creek") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                      aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_CC_bar
st_spawn_AR_bar <- ggplot(spawn_hab_df_st %>% 
                            filter(Watershed=="American River") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                      aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_AR_bar
st_spawn_SR_bar <- ggplot(spawn_hab_df_st %>% 
                            filter(Watershed=="Stanislaus River") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                      aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_SR_bar

#ST by WYT
# SR by WYT
spawn_hab_df_st <- spawn_hab_df_final_wyt %>% filter(`Run-Type`=="st")
st_spawn_wyt <- ggplot(spawn_hab_df_st, aes(x=WYT, y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Water Year Type")+
  facet_wrap(factor(Month)~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_wyt
st_spawn_USR_wyt <- ggplot(spawn_hab_df_st %>% filter(Watershed=="Upper Sacramento River"), 
                       aes(x=WYT, y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Water Year Type", fill="")+
  facet_wrap(~factor(Month), scales = "free") +
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_USR_wyt
st_spawn_CC_wyt <- ggplot(spawn_hab_df_st %>% filter(Watershed=="Clear Creek"), 
                      aes(x=WYT, y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Water Year Type", fill="")+
  facet_wrap(~factor(Month), scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_CC_wyt
st_spawn_AR_wyt <- ggplot(spawn_hab_df_st %>% filter(Watershed=="American River"), 
                      aes(x=WYT, y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Water Year Type", fill="")+
  facet_wrap(~factor(Month), scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_AR_wyt
st_spawn_StR_wyt <- ggplot(spawn_hab_df_st %>% filter(Watershed=="Stanislaus River"), 
                      aes(x=WYT, y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Spawning Habitat (acres)", x="Month", fill="")+
  facet_wrap(~factor(Month), scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_spawn_StR_wyt



# Consider adding steelhead spawner limitation plots if I get reliable data

  ##################################################
  # Juvenile habitat area - floodplain
  ##################################################

flood_hab_df_final <- data.frame("Watershed"=character(),
                                 "WY"=numeric(),
                                 "Month"=numeric(),
                                 "Run-Type"=character(),
                                 "value"=numeric(),
                                 "Alternative"=character())

for(i in 1:n.alt){
  
  # Import flooplain habitat data
  flood_hab <- readRDS(paste0(out_dir[i],alt_file_names[i],"_WUA_FLOOD_Ch4.rds"))
  
  # Convert flood_hab to acres for plotting
  flood_hab <- flood_hab %>% square_meters_to_acres()
  
  # convert back to a data.frame (see Wrapper script.R, stochastic)
  flood_hab_df <- flood_hab %>%
    reshape2::melt(varnames=c("Watershed","WY","Month","Run-Type")) %>%
    drop_na(value) %>%
    mutate(Alternative=alt_plot_names[i])
  
  flood_hab_df_final <- rbind(flood_hab_df_final, flood_hab_df)
  
}
flood_hab_df_final$Alternative <- factor(flood_hab_df_final$Alternative,
                                         levels=alt_plot_names[c(2,3,1,4,5)])

# WR
flood_hab_df_wr <- flood_hab_df_final %>% filter(`Run-Type`=="wr")
wr_flood <- ggplot(flood_hab_df_wr, aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  #facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); wr_flood

  # Calculate mean, only plot mean as barplot
wr_flood_bar <- ggplot(flood_hab_df_wr %>%
                         group_by(Month, Alternative) %>%
                         summarize(Mean=mean(value)),
                       aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  #facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); wr_flood_bar

# SR
flood_hab_df_sr <- flood_hab_df_final %>% filter(`Run-Type`=="sr")
sr_flood <- ggplot(flood_hab_df_sr, aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_flood
sr_flood_USR <- ggplot(flood_hab_df_sr %>% filter(Watershed=="Upper Sacramento River"), 
                       aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_flood_USR
sr_flood_CC <- ggplot(flood_hab_df_sr %>% filter(Watershed=="Clear Creek"), 
                      aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_flood_CC
sr_flood_SJ <- ggplot(flood_hab_df_sr %>% filter(Watershed=="San Joaquin River"), 
                      aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_flood_SJ
  
  # Calculate mean, plot as bars
sr_flood_USR_bar <- ggplot(flood_hab_df_sr %>% 
                         filter(Watershed=="Upper Sacramento River") %>%
                         group_by(Month, Alternative) %>%
                         summarise(Mean=mean(value)), 
                       aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_flood_USR_bar
sr_flood_CC_bar <- ggplot(flood_hab_df_sr %>% 
                        filter(Watershed=="Clear Creek") %>%
                        group_by(Month, Alternative) %>%
                        summarise(Mean=mean(value)), 
                      aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_flood_CC_bar
sr_flood_SJ_bar <- ggplot(flood_hab_df_sr %>% 
                        filter(Watershed=="San Joaquin River") %>%
                        group_by(Month, Alternative) %>%
                        summarise(Mean=mean(value)), 
                      aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_flood_SJ_bar

# ST
flood_hab_df_st <- flood_hab_df_final %>% filter(`Run-Type`=="st")
st_flood <- ggplot(flood_hab_df_st, aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_flood
st_flood_USR <- ggplot(flood_hab_df_st %>% filter(Watershed=="Upper Sacramento River"), 
                       aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_flood_USR
st_flood_CC <- ggplot(flood_hab_df_st %>% filter(Watershed=="Clear Creek"), 
                      aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_flood_CC
st_flood_AR <- ggplot(flood_hab_df_st %>% filter(Watershed=="American River"), 
                      aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_flood_AR
st_flood_SR <- ggplot(flood_hab_df_st %>% filter(Watershed=="Stanislaus River"), 
                      aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_flood_SR
st_flood_SJ <- ggplot(flood_hab_df_st %>% filter(Watershed=="San Joaquin River"), 
                      aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_flood_SJ

  # Calculate mean, plot as bars
st_flood_USR_bar <- ggplot(flood_hab_df_st %>% 
                             filter(Watershed=="Upper Sacramento River") %>%
                             group_by(Month, Alternative) %>%
                             summarise(Mean=mean(value)), 
                       aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_flood_USR_bar
st_flood_CC_bar <- ggplot(flood_hab_df_st %>% 
                            filter(Watershed=="Clear Creek") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                      aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_flood_CC_bar
st_flood_AR_bar <- ggplot(flood_hab_df_st %>% 
                            filter(Watershed=="American River") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                      aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_flood_AR_bar
st_flood_SR_bar <- ggplot(flood_hab_df_st %>% 
                            filter(Watershed=="Stanislaus River") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                      aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_flood_SR_bar
st_flood_SJ_bar <- ggplot(flood_hab_df_st %>% 
                            filter(Watershed=="San Joaquin River") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                      aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Floodplain Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_flood_SJ_bar

  ##################################################
  # Juvenile habitat area - instream
  ##################################################

instream_hab_df_final <- data.frame("Watershed"=character(),
                                 "WY"=numeric(),
                                 "Month"=numeric(),
                                 "Run-Type"=character(),
                                 "value"=numeric(),
                                 "Alternative"=character())

for(i in 1:n.alt){
  
  # Import flooplain habitat data
  instream_hab <- readRDS(paste0(out_dir[i],alt_file_names[i],"_WUA_INSTREAM_Ch4.rds"))
  
  # Convert instream_hab to acres for plotting
  instream_hab <- instream_hab %>% square_meters_to_acres()
  
  # convert back to a data.frame (see Wrapper script.R, stochastic)
  instream_hab_df <- instream_hab %>%
    reshape2::melt(varnames=c("Watershed","WY","Month","Run-Type")) %>%
    drop_na(value) %>%
    mutate(Alternative=alt_plot_names[i])
  
  instream_hab_df_final <- rbind(instream_hab_df_final, instream_hab_df)
  
}
instream_hab_df_final$Alternative <- factor(instream_hab_df_final$Alternative,
                                         levels=alt_plot_names[c(2,3,1,4,5)])

# WR
instream_hab_df_wr <- instream_hab_df_final %>% filter(`Run-Type`=="wr")
wr_instream <- ggplot(instream_hab_df_wr, aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  #facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); wr_instream

  # Calculate mean, only plot mean as barplot
wr_instream_bar <- ggplot(instream_hab_df_wr %>%
                         group_by(Month, Alternative) %>%
                         summarize(Mean=mean(value)),
                       aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  #facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); wr_instream_bar

# SR
instream_hab_df_sr <- instream_hab_df_final %>% filter(`Run-Type`=="sr")
sr_instream <- ggplot(instream_hab_df_sr, aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_instream
sr_instream_USR <- ggplot(instream_hab_df_sr %>% filter(Watershed=="Upper Sacramento River"), 
                          aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_instream_USR
sr_instream_CC <- ggplot(instream_hab_df_sr %>% filter(Watershed=="Clear Creek"), 
                         aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_instream_CC
sr_instream_SJ <- ggplot(instream_hab_df_sr %>% filter(Watershed=="San Joaquin River"), 
                         aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_instream_SJ

  # Calculate mean, plot as bars
sr_instream_USR_bar <- ggplot(instream_hab_df_sr %>% 
                             filter(Watershed=="Upper Sacramento River") %>%
                             group_by(Month, Alternative) %>%
                             summarise(Mean=mean(value)), 
                           aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_instream_USR_bar
sr_instream_CC_bar <- ggplot(instream_hab_df_sr %>% 
                            filter(Watershed=="Clear Creek") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                          aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_instream_CC_bar
sr_instream_SJ_bar <- ggplot(instream_hab_df_sr %>% 
                            filter(Watershed=="San Joaquin River") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                          aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); sr_instream_SJ_bar

# ST
instream_hab_df_sr <- instream_hab_df_final %>% filter(`Run-Type`=="sr")
instream_hab_df_st <- instream_hab_df_final %>% filter(`Run-Type`=="st")
st_instream <- ggplot(instream_hab_df_st, aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  facet_wrap(~Watershed, scales="free")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_instream
st_instream_USR <- ggplot(instream_hab_df_st %>% filter(Watershed=="Upper Sacramento River"), 
                          aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_instream_USR
st_instream_CC <- ggplot(instream_hab_df_st %>% filter(Watershed=="Clear Creek"), 
                         aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_instream_CC
st_instream_AR <- ggplot(instream_hab_df_st %>% filter(Watershed=="American River"), 
                         aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_instream_AR
st_instream_SR <- ggplot(instream_hab_df_st %>% filter(Watershed=="Stanislaus River"), 
                         aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_instream_SR
st_instream_SJ <- ggplot(instream_hab_df_st %>% filter(Watershed=="San Joaquin River"), 
                         aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_instream_SJ

  # Calculate mean, plot as bars
st_instream_USR_bar <- ggplot(instream_hab_df_st %>% 
                             filter(Watershed=="Upper Sacramento River") %>%
                             group_by(Month, Alternative) %>%
                             summarise(Mean=mean(value)), 
                           aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_instream_USR_bar
st_instream_CC_bar <- ggplot(instream_hab_df_st %>% 
                            filter(Watershed=="Clear Creek") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                          aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_instream_CC_bar
st_instream_AR_bar <- ggplot(instream_hab_df_st %>% 
                            filter(Watershed=="American River") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                          aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_instream_AR_bar
st_instream_SR_bar <- ggplot(instream_hab_df_st %>% 
                            filter(Watershed=="Stanislaus River") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                          aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_instream_SR_bar
st_instream_SJ_bar <- ggplot(instream_hab_df_st %>% 
                            filter(Watershed=="San Joaquin River") %>%
                            group_by(Month, Alternative) %>%
                            summarise(Mean=mean(value)), 
                          aes(x=factor(Month), y=Mean, fill=Alternative))+
  geom_col(position=position_dodge())+
  labs(y="Instream Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); st_instream_SJ_bar


  ########################
  # Bypass rearing habitat
  ########################

bypass_hab_df_final <- data.frame("WY"=numeric(),
                                  "Month"=numeric(),
                                  "Region"=character(),
                                  "value"=numeric(),
                                  "Alternative"=character())

for(i in 1:n.alt){
  
  # Import flooplain habitat data
  bypass_hab <- readRDS(paste0(out_dir[i],alt_file_names[i],"_WUA_BYPASS_Ch4.rds"))
  
  # Convert instream_hab to acres for plotting
  bypass_hab <- bypass_hab %>% square_meters_to_acres()
  
  # convert back to a data.frame (see Wrapper script.R, stochastic)
  bypass_hab_df <- bypass_hab %>%
    reshape2::melt(varnames=c("Region","WY","Month")) %>%
    pivot_wider(names_from=Region, values_from = value) %>%
    mutate(Sutter=sutter1+sutter2+sutter3+sutter4,
           Yolo=yolo1+yolo2) %>%
    pivot_longer(cols=sutter1:Yolo,
                 names_to="Region",
                 values_to="value") %>%
    mutate(Alternative=alt_plot_names[i])
  
  bypass_hab_df_final <- rbind(bypass_hab_df_final, bypass_hab_df)
  
}
bypass_hab_df_final$Alternative <- factor(bypass_hab_df_final$Alternative,
                                         levels=alt_plot_names[c(2,3,1,4,5)])

# Plotting Yolo, Sutter Bypasses total habitat availability
bypass_hab_df_plot <- bypass_hab_df_final %>% filter(Region%in%c("Yolo","Sutter"))
bypass_rear <- ggplot(bypass_hab_df_plot, aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  facet_wrap(~Region, scales="free")+
  labs(y="Bypass Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); bypass_rear
bypass_rear_Sutter <- ggplot(bypass_hab_df_plot %>% filter(Region=="Sutter"), 
                             aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Bypass Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); bypass_rear_Sutter
bypass_rear_Yolo <- ggplot(bypass_hab_df_plot %>% filter(Region=="Yolo"), 
                           aes(x=factor(Month), y=value, fill=Alternative))+
  geom_boxplot()+
  labs(y="Bypass Rearing Habitat (acres)", x="Month", fill="")+
  theme(legend.position='bottom', legend.text=element_text(size=7))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE)); bypass_rear_Yolo

  ######################
  # Saving plots
  ######################

# Spawning habitat

  # WR
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_WR Spawning Habitat USR.png"),
       wr_spawn, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_WR Spawning Habitat USR_WYT.png"),
       wr_spawn_wyt, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_WR Spawning Habitat USR_bar.png"),
       wr_spawn_bar, width=6, height=4, units=c("in"))

  # SR
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Spawning Habitat USR.png"),
       sr_spawn_USR, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Spawning Habitat all watersheds_WYT.png"),
       sr_spawn_wyt, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Spawning Habitat USR_WYT.png"),
       sr_spawn_USR_wyt, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Spawning Habitat CC_WYT.png"),
       sr_spawn_CC_wyt, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Spawning Habitat CC.png"),
       sr_spawn_CC, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Spawning Habitat USR_bar.png"),
       sr_spawn_USR_bar, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Spawning Habitat CC_bar.png"),
       sr_spawn_CC_bar, width=6, height=4, units=c("in"))

  # ST
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat USR.png"),
       st_spawn_USR, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat USR_WYT.png"),
       st_spawn_wyt, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat CC.png"),
       st_spawn_CC, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat AR.png"),
       st_spawn_AR, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat SR.png"),
       st_spawn_SR, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat USR_bar.png"),
       st_spawn_USR_bar, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat CC_bar.png"),
       st_spawn_CC_bar, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat AR_bar.png"),
       st_spawn_AR_bar, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat SR_bar.png"),
       st_spawn_SR_bar, width=6, height=4, units=c("in"))

ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat USR_wyt.png"),
       st_spawn_USR_wyt, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat all watersheds_WYT.png"),
       st_spawn_wyt, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat CC_wyt.png"),
       st_spawn_CC_wyt, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat AR_wyt.png"),
       st_spawn_AR_wyt, width=6, height=4, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Spawning Habitat StR_wyt.png"),
       st_spawn_StR_wyt, width=6, height=4, units=c("in"))



# Rearing

  # Floodplain
    # WR
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_WR Floodplain Habitat USR.png"),
       wr_flood, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_WR Floodplain Habitat USR_bar.png"),
       wr_flood_bar, width=6, height=6, units=c("in"))

    # SR
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Floodplain Habitat USR.png"),
       sr_flood_USR, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Floodplain Habitat CC.png"),
       sr_flood_CC, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Floodplain Habitat SJ.png"),
       sr_flood_SJ, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Floodplain Habitat USR_bar.png"),
       sr_flood_USR_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Floodplain Habitat CC_bar.png"),
       sr_flood_CC_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Floodplain Habitat SJ_bar.png"),
       sr_flood_SJ_bar, width=6, height=6, units=c("in"))

    # ST
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Floodplain Habitat USR.png"),
       st_flood_USR, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Floodplain Habitat CC.png"),
       st_flood_CC, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Floodplain Habitat AR.png"),
       st_flood_AR, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Floodplain Habitat SR.png"),
       st_flood_SR, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Floodplain Habitat SJ.png"),
       st_flood_SJ, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Floodplain Habitat USR_bar.png"),
       st_flood_USR_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Floodplain Habitat CC_bar.png"),
       st_flood_CC_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Floodplain Habitat AR_bar.png"),
       st_flood_AR_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Floodplain Habitat SR_bar.png"),
       st_flood_SR_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Floodplain Habitat SJ_bar.png"),
       st_flood_SJ_bar, width=6, height=6, units=c("in"))
  
  # Instream

    # WR
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_WR Instream Habitat USR.png"),
       wr_instream, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_WR Instream Habitat USR_bar.png"),
       wr_instream_bar, width=6, height=6, units=c("in"))

    # SR
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Instream Habitat USR.png"),
       sr_instream_USR, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Instream Habitat CC.png"),
       sr_instream_CC, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Instream Habitat SJ.png"),
       sr_instream_SJ, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Instream Habitat USR_bar.png"),
       sr_instream_USR_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Instream Habitat CC_bar.png"),
       sr_instream_CC_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_SR Instream Habitat SJ_bar.png"),
       sr_instream_SJ_bar, width=6, height=6, units=c("in"))
  
    # ST
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Instream Habitat USR.png"),
       st_instream_USR, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Instream Habitat CC.png"),
       st_instream_CC, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Instream Habitat AR.png"),
       st_instream_AR, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Instream Habitat SR.png"),
       st_instream_SR, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Instream Habitat SJ.png"),
       st_instream_SJ, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Instream Habitat USR_bar.png"),
       st_instream_USR_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Instream Habitat CC_bar.png"),
       st_instream_CC_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Instream Habitat AR_bar.png"),
       st_instream_AR_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Instream Habitat SR_bar.png"),
       st_instream_SR_bar, width=6, height=6, units=c("in"))
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_ST Instream Habitat SJ_bar.png"),
       st_instream_SJ_bar, width=6, height=6, units=c("in"))
  
  # Bypass
'/
ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_Bypass Rearing.png"),
       bypass_rear, width=6, height=4, units=c("in"))
  ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_Bypass Rearing Sutter.png"),
         bypass_rear_Sutter, width=6, height=4, units=c("in"))
  ggsave(paste0("./WUA_LTO/Results/Figures/",out.name,"_Bypass Rearing Yolo.png"),
         bypass_rear_Yolo, width=6, height=4, units=c("in"))
'