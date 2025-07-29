
############
# This script provides tabular summaries (.csv's) for model outputs of interest
  # for both EIS and BA sets of alternatives
# The script only summarizes results from deterministic model runs
############

#######
# Setup
#######

require(tidyverse)
require(here)

# Indicate model run type
model_type <- "Det"

n.alt <- 9
######
## EIS
  # Either 7 or 8 alternatives, depending on whether Alt3 is available
######

# These parameters will be updated once we have final estimates for all alternatives.
alt_plot_names <- c("NAA","Alt1","Alt2wTUCPwoVA","Alt2woTUCPwoVA",
                    "Alt2woTUCPDeltaVA","Alt2woTUCPAllVA",
                    "Alt3",
                    "Alt4",
                    "Action5")
n.alt <- length(alt_plot_names)
alt_file_names <- c("Reclamation_2021LTO_CalSim3_NAA_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_Alt1_2022MED_09092023",
                    "Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09132024",
                    "Reclamation_2021LTO_CalSim3_Alt2v1_wTUCP_2022MED_09132024",
                    "Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_09132024",
                    "Reclamation_2021LTO_CalSim3_Alt2v3_noTUCP_2022MED_09132024",
                    "Reclamation_2021LTO_CalSim3_ALT3_2022MED_092423",
                    "Reclamation_2021LTO_CalSim3_Alt4_2022MED_09162024",
                    "CS3_Alt5_JPF_wTUCP_2022MED_0525_dv 1")

input_data <- list()
# Load DSM outputs for plotting
if(model_type=="Det"){
  for(i in 1:n.alt){
    input_data[[i]] <- readRDS(here("winterRunDSM-Main", "Output", 
                                    paste0("WR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[i],"_v2.rds")))
  }
} else if(model_type=="Stoch"){
  for(i in 1:n.alt){
    input_data[[i]] <- readRDS(here("winterRunDSM-Main", "Output", 
                                    paste0("WR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[i],"_Iter",n.iter,"_v2.rds")))
  }  
}

# Load WYT reference for plotting
wyt_ref <- read.csv("./winterRunDSM-Main/wyt_ref.csv")

  #############################
  # Population abundance/growth
  #############################

# Ending abundance - all spawners

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
year.names <- 1980:1999

for(i in 1:n.alt){
  dimnames(input_data[[i]]$out.all_spawners) <- list(watershed.names, year.names)
}

spawn_df <- data.frame(Watershed=character(), Year=numeric(),
                       Iteration=numeric(),
                       value=numeric(), Alternative=character(),
                       `All Spawners`=character())

for(i in 1:n.alt){
  spawn_df_temp <- input_data[[i]]$out.all_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Alternative=alt_plot_names[i], `All Spawners`=round(value))
  
  spawn_df <- rbind(spawn_df, spawn_df_temp)
}

df.table1 <- spawn_df %>%
  select(Year, Alternative, `All Spawners`) %>%
  pivot_wider(names_from=Alternative, values_from=`All Spawners`) %>%
  select(Year, alt_plot_names)
write.csv(df.table1, paste0("./winterRunDSM-Main/Output/Tables/EIS_AllSpawnersAct5.csv"),
          row.names=FALSE)

df.table1b <- df.table1 %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100,Action5=(Action5-NAA)/NAA*100) %>%
  select(Year, Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         Alt3,
         Alt4,Action5)
write.csv(df.table1b, paste0("./winterRunDSM-Main/Output/Tables/EIS_AllSpawnersPercDiffAct5.csv"),
          row.names=FALSE)

# Ending abundance - natural spawners

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
year.names <- 1980:1999

for(i in 1:n.alt){
  dimnames(input_data[[i]]$out.nat_spawners) <- list(watershed.names, year.names)
}

spawn_df <- data.frame(Watershed=character(), Year=numeric(),
                       Iteration=numeric(),
                       value=numeric(), Alternative=character(),
                       `Natural Spawners`=character())

for(i in 1:n.alt){
  spawn_df_temp <- input_data[[i]]$out.nat_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Alternative=alt_plot_names[i], `Natural Spawners`=round(value))
  
  spawn_df <- rbind(spawn_df, spawn_df_temp)
}

df.table2 <- spawn_df %>%
  select(Year, Alternative, `Natural Spawners`) %>%
  pivot_wider(names_from=Alternative, values_from=`Natural Spawners`) %>%
  select(Year, alt_plot_names)
write.csv(df.table2, paste0("./winterRunDSM-Main/Output/Tables/EIS_NatSpawnersAct5.csv"),
          row.names=FALSE)

df.table2b <- df.table2 %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100,Action5=(Action5-NAA)/NAA*100) %>%
  select(Year, Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         Alt3,
         Alt4,Action5)
write.csv(df.table2b, paste0("./winterRunDSM-Main/Output/Tables/EIS_NatSpawnersPercDiffAct5.csv"),
          row.names=FALSE)

# Mean lambda, end lambda

lambda <- array(NA, dim=c(n.alt,19))

lambda_mean <- numeric(n.alt)
end_lambda <- numeric(n.alt)

for(j in 1:n.alt){
  for(i in 2:20){
    lambda[j,i-1] <- input_data[[j]]$out.all_spawners[1,i]/input_data[[j]]$out.all_spawners[1,i-1]
  }
  lambda_mean[j] <- exp(mean(log(lambda[j,])))
  end_lambda[j] <- input_data[[j]]$out.all_spawners[1,20]/input_data[[j]]$out.all_spawners[1,1]
}

  # Mean lambda tables 
df.table3 <- tibble(`Mean Lambda`=lambda_mean,
                             Alternative=alt_plot_names) %>%
  pivot_wider(values_from=`Mean Lambda`, names_from=Alternative) %>%
  as.data.frame() %>%
  select(alt_plot_names)
write.csv(df.table3, paste0("./winterRunDSM-Main/Output/Tables/EIS_MeanLambdaAct5.csv"),
          row.names=FALSE)

df.table3b <- df.table3 %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100,Action5=(Action5-NAA)/NAA*100) %>%
  select(Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         Alt3,
         Alt4,Action5)
write.csv(df.table3b, paste0("./winterRunDSM-Main/Output/Tables/EIS_MeanLambdaPercDiffAct5.csv"),
          row.names=FALSE)

  #Mean lambda faceted by WYT
mlambda_wyt_df <-as.data.frame(lambda)
names(mlambda_wyt_df) <- year.names[2:20]
mlambda_wyt_df <- as_tibble(mlambda_wyt_df) %>%
  mutate(Alternative = alt_plot_names) %>%
  pivot_longer(!Alternative, names_to = "year", values_to = "lambda") %>%
  mutate(WY =as.integer(year)) %>%
  left_join(., wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W"))) %>%
  group_by(WYT, Alternative) %>% #group by a variable for summarizing
  summarize(wy_mean_lambda = mean(lambda)) %>%
  pivot_wider(names_from=Alternative, values_from=wy_mean_lambda) %>%
  as.data.frame() %>%
  select(WYT, alt_plot_names)
write.csv(mlambda_wyt_df, paste0("./winterRunDSM-Main/Output/Tables/EIS_WYTMeanLambdaAct5.csv"),
          row.names=FALSE)

mlambda_wyt_dfb <- mlambda_wyt_df %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100,Action5=(Action5-NAA)/NAA*100) %>%
  select(WYT,Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         Alt3,
         Alt4,Action5)
write.csv(mlambda_wyt_dfb, paste0("./winterRunDSM-Main/Output/Tables/EIS_WYTMeanLambdaPercDiffAct5.csv"),
          row.names=FALSE)

  # End lambda
df.table4 <- tibble(`End Lambda`=end_lambda,
                    Alternative=alt_plot_names) %>%
  pivot_wider(values_from=`End Lambda`, names_from=Alternative) %>%
  as.data.frame() %>%
  select(alt_plot_names)
write.csv(df.table4, paste0("./winterRunDSM-Main/Output/Tables/EIS_EndLambdaAct5.csv"),
          row.names=FALSE)

df.table4b <- df.table4 %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100,Action5=(Action5-NAA)/NAA*100) %>%
  select(Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         Alt3,
         Alt4,Action5)
write.csv(df.table4b, paste0("./winterRunDSM-Main/Output/Tables/EIS_EndLambdaPercDiffAct5.csv"),
          row.names=FALSE)
  
  ########################
  # Demographic parameters
    # Faceted by WYT
  ########################

# S rearing survival, Upper Sacramento River

rear_df <- data.frame(Year=numeric(), Month=numeric(),
                      Watershed=character(), Size=character(), 
                      value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  rear_df_temp <- input_data[[i]]$out.river_surv %>%
    reshape2::melt(varnames=c("Year","Month","Watershed","Size")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  rear_df <- rbind(rear_df, rear_df_temp)
}

rear_s_df <- rear_df %>% filter(Size=="s") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

rear_s_df_all <- rear_s_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

rear_s_df_WYT <- rear_s_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(rear_s_df_all, rear_s_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/EIS_SRearUSRAct5.csv"),
          row.names=FALSE)

df.tablexb <- df.tablex %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100,Action5=(Action5-NAA)/NAA*100) %>%
  select(WYT,Month,
         Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         Alt3,
         Alt4,Action5)
write.csv(df.tablexb, paste0("./winterRunDSM-Main/Output/Tables/EIS_SRearUSRPercDiffAct5.csv"),
          row.names=FALSE)

# VL migratory survival, Upper-mid Sacramento River

migr_df <- data.frame(Year=numeric(), Month=numeric(),
                      Size=character(), value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  migr_df_temp <- input_data[[i]]$out.UM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df, migr_df_temp)
}

migr_vl_df <- migr_df %>% filter(Size=="vl") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

migr_vl_df_all <- migr_vl_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

migr_vl_df_WYT <- migr_vl_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(migr_vl_df_all, migr_vl_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/EIS_VLMigrUMAct5.csv"),
          row.names=FALSE)

df.tablexb <- df.tablex %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100,Action5=(Action5-NAA)/NAA*100) %>%
  select(WYT,Month,
         Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         Alt3,
         Alt4,Action5)
write.csv(df.tablexb, paste0("./winterRunDSM-Main/Output/Tables/EIS_VLMigrUMPercDiffAct5.csv"),
          row.names=FALSE)

# VL migratory survival, Lower-mid Sacramento River
  # Copy+paste above, but for out.LM.Sac.S

migr_df <- data.frame(Year=numeric(), Month=numeric(),
                      Size=character(), value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  migr_df_temp <- input_data[[i]]$out.LM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df, migr_df_temp)
}

migr_vl_df <- migr_df %>% filter(Size=="vl") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

migr_vl_df_all <- migr_vl_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

migr_vl_df_WYT <- migr_vl_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(migr_vl_df_all, migr_vl_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/EIS_VLMigrLMAct5.csv"),
          row.names=FALSE)

df.tablexb <- df.tablex %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100,Action5=(Action5-NAA)/NAA*100) %>%
  select(WYT,Month,
         Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         Alt3,
         Alt4,Action5)
write.csv(df.tablexb, paste0("./winterRunDSM-Main/Output/Tables/EIS_VLMigrLMPercDiffAct5.csv"),
          row.names=FALSE)

# VL migratory survival, Lower Sacramento River
  # Copy+paste above, but for out.LL.Sac.S

migr_df <- data.frame(Year=numeric(), Month=numeric(),
                      Size=character(), value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  migr_df_temp <- input_data[[i]]$out.LL.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df, migr_df_temp)
}

migr_vl_df <- migr_df %>% filter(Size=="vl") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

migr_vl_df_all <- migr_vl_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

migr_vl_df_WYT <- migr_vl_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(migr_vl_df_all, migr_vl_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/EIS_VLMigrLLAct5.csv"),
          row.names=FALSE)

df.tablexb <- df.tablex %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100,Action5=(Action5-NAA)/NAA*100) %>%
  select(WYT,Month,
         Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         Alt3,
         Alt4,Action5)
write.csv(df.tablexb, paste0("./winterRunDSM-Main/Output/Tables/EIS_VLMigrLLPercDiffAct5.csv"),
          row.names=FALSE)

# VL migratory survival, N Delta

migr_df <- data.frame(Year=numeric(), Month=numeric(), Delta=character(),
                      Size=character(), value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  migr_df_temp <- input_data[[i]]$out.Sac.Delt.S %>%
    reshape2::melt(varnames=c("Year","Month","Delta","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df, migr_df_temp)
}

migr_vl_df <- migr_df %>% filter(Delta=="N",Size=="vl") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

migr_vl_df_all <- migr_vl_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

migr_vl_df_WYT <- migr_vl_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(migr_vl_df_all, migr_vl_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/EIS_VLMigrNDeltaAct5.csv"),
          row.names=FALSE)

df.tablexb <- df.tablex %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100,Action5=(Action5-NAA)/NAA*100) %>%
  select(WYT,Month,
         Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         Alt3,
         Alt4,Action5)
write.csv(df.tablexb, paste0("./winterRunDSM-Main/Output/Tables/EIS_VLMigrNDeltaPercDiffAct5.csv"),
          row.names=FALSE)

# VL migratory survival, S Delta

migr_df <- data.frame(Year=numeric(), Month=numeric(), Origin=character(),
                      Size=character(), value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  migr_df_temp <- input_data[[i]]$out.newDsurv %>%
    reshape2::melt(varnames=c("Year","Month","Origin","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df, migr_df_temp)
}

migr_vl_df <- migr_df %>% filter(Origin=="N",Size=="vl") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

migr_vl_df_all <- migr_vl_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

migr_vl_df_WYT <- migr_vl_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(migr_vl_df_all, migr_vl_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/EIS_VLMigrSDeltaAct5.csv"),
          row.names=FALSE)

df.tablexb <- df.tablex %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100,Action5=(Action5-NAA)/NAA*100) %>%
  select(WYT,Month,
         Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         Alt3,
         Alt4,Action5)
write.csv(df.tablexb, paste0("./winterRunDSM-Main/Output/Tables/EIS_VLMigrSDeltaPercDiffAct5.csv"),
          row.names=FALSE)

######
## BA
######

# These parameters will be updated once we have final estimates for all alternatives.
n.alt <- 8
alt_plot_names <- c("EXP1","EXP3","NAA","Alt2wTUCPwoVA","Alt2woTUCPwoVA",
                    "Alt2woTUCPDeltaVA","Alt2woTUCPAllVA", "Act5")
alt_file_names <- c("Reclamation_2021LTO_CalSim3_EXP1_2022MED_rev10_090623_dynGWSW",
                    "Reclamation_2021LTO_CalSim3_EXP3_2022MED_rev10_090623_dynGWSW",
                    "Reclamation_2021LTO_CalSim3_NAA_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09132024",
                    "Reclamation_2021LTO_CalSim3_Alt2v1_wTUCP_2022MED_09132024",
                    "Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_09132024",
                    "Reclamation_2021LTO_CalSim3_Alt2v3_noTUCP_2022MED_09132024",
                    "CS3_Alt5_JPF_wTUCP_2022MED_0525_dv 1")

# Set up comparisons of interest - OLD
# FOR TESTING PURPOSES ONLY
# Comparing 2019 Operations NAA, CalSim 3
# n.alt <- 2 # No. of Scenarios to compare
# alt_plot_names <- c("Original", "Ex CalSim 3")
# alt_file_names <- c("originalDSM",
#                "CS3_L2020_DV_2021_ext_2040MED")
# out.name <- "OGvsCalSim3"

input_data <- list()
# Load DSM outputs for plotting
if(model_type=="Det"){
  for(i in 1:n.alt){
    input_data[[i]] <- readRDS(here("winterRunDSM-Main", "Output", 
                                    paste0("WR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[i],"_v2.rds")))
  }
} else if(model_type=="Stoch"){
  for(i in 1:n.alt){
    input_data[[i]] <- readRDS(here("winterRunDSM-Main", "Output", 
                                    paste0("WR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[i],"_Iter",n.iter,"_v2.rds")))
  }  
}

# Load WYT reference for plotting
wyt_ref <- read.csv("./winterRunDSM-Main/wyt_ref.csv")

#############################
# Population abundance/growth
#############################

# Ending abundance - all spawners

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
year.names <- 1980:1999

for(i in 1:n.alt){
  dimnames(input_data[[i]]$out.all_spawners) <- list(watershed.names, year.names)
}

spawn_df <- data.frame(Watershed=character(), Year=numeric(),
                       Iteration=numeric(),
                       value=numeric(), Alternative=character(),
                       `All Spawners`=character())

for(i in 1:n.alt){
  spawn_df_temp <- input_data[[i]]$out.all_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Alternative=alt_plot_names[i], `All Spawners`=value)
  
  spawn_df <- rbind(spawn_df, spawn_df_temp)
}

df.table1 <- spawn_df %>%
  select(Year, Alternative, `All Spawners`) %>%
  pivot_wider(names_from=Alternative, values_from=`All Spawners`) %>%
  select(Year, alt_plot_names)
write.csv(df.table1, paste0("./winterRunDSM-Main/Output/Tables/BA_AllSpawnersAct5.csv"),
          row.names=FALSE)

# Ending abundance - natural spawners

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
year.names <- 1980:1999

for(i in 1:n.alt){
  dimnames(input_data[[i]]$out.nat_spawners) <- list(watershed.names, year.names)
}

spawn_df <- data.frame(Watershed=character(), Year=numeric(),
                       Iteration=numeric(),
                       value=numeric(), Alternative=character(),
                       `Natural Spawners`=character())

for(i in 1:n.alt){
  spawn_df_temp <- input_data[[i]]$out.nat_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Alternative=alt_plot_names[i], `Natural Spawners`=value)
  
  spawn_df <- rbind(spawn_df, spawn_df_temp)
}

df.table2 <- spawn_df %>%
  select(Year, Alternative, `Natural Spawners`) %>%
  pivot_wider(names_from=Alternative, values_from=`Natural Spawners`) %>%
  select(Year, alt_plot_names)
write.csv(df.table2, paste0("./winterRunDSM-Main/Output/Tables/BA_NatSpawnersAct5.csv"),
          row.names=FALSE)

# Mean lambda, end lambda

lambda <- array(NA, dim=c(n.alt,19))

lambda_mean <- numeric(n.alt)
end_lambda <- numeric(n.alt)

for(j in 1:n.alt){
  for(i in 2:20){
    lambda[j,i-1] <- input_data[[j]]$out.all_spawners[1,i]/input_data[[j]]$out.all_spawners[1,i-1]
  }
  lambda_mean[j] <- exp(mean(log(lambda[j,])))
  end_lambda[j] <- input_data[[j]]$out.all_spawners[1,20]/input_data[[j]]$out.all_spawners[1,1]
}

# Mean lambda tables 
df.table3 <- tibble(`Mean Lambda`=lambda_mean,
                    Alternative=alt_plot_names) %>%
  pivot_wider(values_from=`Mean Lambda`, names_from=Alternative) %>%
  as.data.frame() %>%
  select(alt_plot_names)
write.csv(df.table3, paste0("./winterRunDSM-Main/Output/Tables/BA_MeanLambdaAct5.csv"),
          row.names=FALSE)

# End lambda
df.table4 <- tibble(`End Lambda`=end_lambda,
                    Alternative=alt_plot_names) %>%
  pivot_wider(values_from=`End Lambda`, names_from=Alternative) %>%
  as.data.frame() %>%
  select(alt_plot_names)
write.csv(df.table4, paste0("./winterRunDSM-Main/Output/Tables/BA_EndLambdaAct5.csv"),
          row.names=FALSE)

#Mean lambda faceted by WYT
mlambda_wyt_df <-as.data.frame(lambda)
names(mlambda_wyt_df) <- year.names[2:20]
mlambda_wyt_df <- as_tibble(mlambda_wyt_df) %>%
  mutate(Alternative = alt_plot_names) %>%
  pivot_longer(!Alternative, names_to = "year", values_to = "lambda") %>%
  mutate(WY =as.integer(year)) %>%
  left_join(., wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W"))) %>%
  group_by(WYT, Alternative) %>% #group by a variable for summarizing
  summarize(wy_mean_lambda = mean(lambda)) %>%
  pivot_wider(names_from=Alternative, values_from=wy_mean_lambda) %>%
  as.data.frame() %>%
  select(WYT, alt_plot_names)
write.csv(mlambda_wyt_df, paste0("./winterRunDSM-Main/Output/Tables/BA_WYTMeanLambdaAct5.csv"),
          row.names=FALSE)


########################
# Demographic parameters
# Faceted by WYT
########################

# S rearing survival, Upper Sacramento River

rear_df <- data.frame(Year=numeric(), Month=numeric(),
                      Watershed=character(), Size=character(), 
                      value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  rear_df_temp <- input_data[[i]]$out.river_surv %>%
    reshape2::melt(varnames=c("Year","Month","Watershed","Size")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  rear_df <- rbind(rear_df, rear_df_temp)
}

rear_s_df <- rear_df %>% filter(Size=="s") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

rear_s_df_all <- rear_s_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

rear_s_df_WYT <- rear_s_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(rear_s_df_all, rear_s_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/BA_SRearUSRAct5.csv"),
          row.names=FALSE)

# VL migratory survival, Upper-mid Sacramento River

migr_df <- data.frame(Year=numeric(), Month=numeric(),
                      Size=character(), value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  migr_df_temp <- input_data[[i]]$out.UM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df, migr_df_temp)
}

migr_vl_df <- migr_df %>% filter(Size=="vl") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

migr_vl_df_all <- migr_vl_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

migr_vl_df_WYT <- migr_vl_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(migr_vl_df_all, migr_vl_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/BA_VLMigrUMAct5.csv"),
          row.names=FALSE)

# VL migratory survival, Lower-mid Sacramento River
# Copy+paste above, but for out.LM.Sac.S

migr_df <- data.frame(Year=numeric(), Month=numeric(),
                      Size=character(), value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  migr_df_temp <- input_data[[i]]$out.LM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df, migr_df_temp)
}

migr_vl_df <- migr_df %>% filter(Size=="vl") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

migr_vl_df_all <- migr_vl_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

migr_vl_df_WYT <- migr_vl_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(migr_vl_df_all, migr_vl_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/BA_VLMigrLMAct5.csv"),
          row.names=FALSE)

# VL migratory survival, Lower Sacramento River
# Copy+paste above, but for out.LL.Sac.S

migr_df <- data.frame(Year=numeric(), Month=numeric(),
                      Size=character(), value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  migr_df_temp <- input_data[[i]]$out.LL.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df, migr_df_temp)
}

migr_vl_df <- migr_df %>% filter(Size=="vl") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

migr_vl_df_all <- migr_vl_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

migr_vl_df_WYT <- migr_vl_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(migr_vl_df_all, migr_vl_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/BA_VLMigrLLAct5.csv"),
          row.names=FALSE)

# VL migratory survival, N Delta

migr_df <- data.frame(Year=numeric(), Month=numeric(), Delta=character(),
                      Size=character(), value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  migr_df_temp <- input_data[[i]]$out.Sac.Delt.S %>%
    reshape2::melt(varnames=c("Year","Month","Delta","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df, migr_df_temp)
}

migr_vl_df <- migr_df %>% filter(Delta=="N",Size=="vl") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

migr_vl_df_all <- migr_vl_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

migr_vl_df_WYT <- migr_vl_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(migr_vl_df_all, migr_vl_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/BA_VLMigrNDeltaAct5.csv"),
          row.names=FALSE)

# VL migratory survival, S Delta

migr_df <- data.frame(Year=numeric(), Month=numeric(), Origin=character(),
                      Size=character(), value=numeric(), 
                      Alternative=character(), WY=numeric())

for(i in 1:n.alt){
  migr_df_temp <- input_data[[i]]$out.newDsurv %>%
    reshape2::melt(varnames=c("Year","Month","Origin","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df, migr_df_temp)
}

migr_vl_df <- migr_df %>% filter(Origin=="N",Size=="vl") %>%
  left_join(.,wyt_ref) %>%
  mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))

migr_vl_df_all <- migr_vl_df %>%
  group_by(Month, Alternative) %>%
  summarize(Mean=mean(value)) %>%
  mutate(WYT="All") %>%
  select(WYT, Month, Alternative, Mean)

migr_vl_df_WYT <- migr_vl_df %>%
  group_by(WYT, Month, Alternative) %>%
  summarize(Mean=mean(value))

df.tablex <- rbind(migr_vl_df_all, migr_vl_df_WYT) %>%
  pivot_wider(names_from=Alternative, values_from=Mean) %>%
  select(WYT, Month, alt_plot_names)

write.csv(df.tablex, paste0("./winterRunDSM-Main/Output/Tables/BA_VLMigrSDeltaAct5.csv"),
          row.names=FALSE)


