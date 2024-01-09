#################
# Annotation 
  # This script imports spring-run DSM output data and provides summaries
    # (tables + figures) of outputs relevant to the SRKW analysis
#################

#################
# Plotting setting
  # Plan on only using deterministic
#################

# Indicate model run type
model_type <- "Det"

# Manual inputs for Chipps->Golden Gate Survival (Bay.S)
# and biomass of each size class (bio)
# These values were taken directly from life cycle model scripts
Bay.S <- 0.358
bio <- c(0.5, 1.8, 9.1, 31.4)

#####################
# Load packages, inputs
#####################

require(dplyr); require(here)
require(tidyr); 
require(reshape2); 
require(ggplot2)

# All available
n.alt <- 10
alt_plot_names <- c("Alt1","Alt2wTUCPwoVA","Alt2woTUCPwoVA","Alt2woTUCPDeltaVA",
                    "Alt2woTUCPAllVA","Alt3","Alt4","EXP1","EXP3","NAA")
alt_file_names <- c("Reclamation_2021LTO_CalSim3_Alt1_2022MED_09092023",
                    "Reclamation_2021LTO_CalSim3_Alt2v1_wTUCP_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_Alt2v1_woTUCP_2022MED_09072023",
                    "Reclamation_2021LTO_CalSim3_Alt2v2_noTUCP_2022MED_090723",
                    "Reclamation_2021LTO_CalSim3_Alt2v3_noTUCP_2022MED_090723",
                    "Reclamation_2021LTO_CalSim3_ALT3_2022MED_092423",
                    "Reclamation_2021LTO_CalSim3_Alt4_2022MED_09082023",
                    "Reclamation_2021LTO_CalSim3_EXP1_2022MED_rev10_090623_dynGWSW",
                    "Reclamation_2021LTO_CalSim3_EXP3_2022MED_rev10_090623_dynGWSW",
                    "Reclamation_2021LTO_CalSim3_NAA_2022MED_09072023")
out.name <- "All"

input_data <- list()
# Load DSM outputs for plotting
if(model_type=="Det"){
  for(i in 1:n.alt){
    input_data[[i]] <- readRDS(here("springRunDSM-Main", "Output", 
                                    paste0("SR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[i],"_v2.rds")))
  }
} else if(model_type=="Stoch"){
  for(i in 1:n.alt){
    input_data[[i]] <- readRDS(here("springRunDSM-Main", "Output", 
                                    paste0("SR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[i],"_Iter",n.iter,"_v2.rds")))
  }  
}

######################
# Generating summaries
  # We note that annual juvenile outmigration summaries reflect a combination of
    # sub-yearlings produced from spawning during the reported water year and 
    # yearlings produced during the previous water year
######################

# Total juvenile abundance at Chipps, across size classes

  # Creating summary object
juv.at.chips_df <- data.frame(`Water Year`=numeric(),
                              Alternative=character(),
                              Abundance=numeric())

for(i in 1:n.alt){
  juv.at.chips_df_temp <- input_data[[i]]$out.juv.at.chips %>%
    reshape2::melt(varnames=c("Year","Watershed","Size")) %>%
    mutate(Alternative=alt_plot_names[i], Abundance=round(value),
           `Water Year`=Year+1) %>%
    group_by(`Water Year`, Alternative) %>%
    summarize(Abundance=sum(Abundance))
  
  juv.at.chips_df <- rbind(juv.at.chips_df, juv.at.chips_df_temp)
}

  # Plotting
juv.at.chips <- ggplot(juv.at.chips_df, aes(x=`Water Year`, y=Abundance, col=Alternative))+
  geom_line() +
  labs(y="Juvenile Abundance passing Chipps Island", x="Water Year")
juv.at.chips
ggsave(paste0("./springRunDSM-Main/Output/Figures/",out.name,"_SRKW_Juv Abund at Chipps.png"),
       width=6, height=5, units=c("in"))

  # Base table
juv.at.chips_table <- juv.at.chips_df %>%
  pivot_wider(names_from=Alternative, values_from=Abundance)
write.csv(juv.at.chips_table, paste0("./springRunDSM-Main/Output/Tables/",out.name,"_SRKW_SR Juv Abund at Chipps.csv"),
          row.names=FALSE)

# Percent difference table (EIS only)
juv.at.chips_tableb <- juv.at.chips_table %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100) %>%
  select(`Water Year`,Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,`Alt2woTUCPAllVA`,
         Alt3,Alt4)
write.csv(juv.at.chips_tableb, paste0("./springRunDSM-Main/Output/Tables/",out.name,"_SRKW_SR Juv Abund at Chipps Perc Diff.csv"),
          row.names=FALSE)


# Total juvenile biomass at Chipps, across size classes

  # Creating summary object
bio.at.chips_df <- data.frame(`Water Year`=numeric(),
                              Alternative=character(),
                              Biomass=numeric())

for(i in 1:n.alt){
  bio.at.chips_df_temp <- input_data[[i]]$out.juv.at.chips %>%
    reshape2::melt(varnames=c("Year","Watershed","Size")) %>%
    mutate(Alternative=alt_plot_names[i], 
           Biomass=ifelse(Size=="s", round(value)*bio[1],
                          ifelse(Size=="m", round(value)*bio[2],
                                 ifelse(Size=="l", round(value)*bio[3], round(value)*bio[4]))),
           `Water Year`=Year+1) %>%
    group_by(`Water Year`, Alternative) %>%
    summarize(Biomass=sum(Biomass))
  
  bio.at.chips_df <- rbind(bio.at.chips_df, bio.at.chips_df_temp)
}

  # Plotting
bio.at.chips <- ggplot(bio.at.chips_df, aes(x=`Water Year`, y=Biomass/1000, col=Alternative))+
  geom_line() +
  labs(y="Juvenile Biomass passing Chipps Island (kg)", x="Water Year")
bio.at.chips
ggsave(paste0("./springRunDSM-Main/Output/Figures/",out.name,"_SRKW_Juv Biomass at Chipps.png"),
       width=6, height=5, units=c("in"))

  # Base table
bio.at.chips_table <- bio.at.chips_df %>%
  pivot_wider(names_from=Alternative, values_from=Biomass)
write.csv(bio.at.chips_table, paste0("./springRunDSM-Main/Output/Tables/",out.name,"_SRKW_SR Juv Biomass at Chipps.csv"),
          row.names=FALSE)

  # Percent difference table (EIS only)
bio.at.chips_tableb <- bio.at.chips_table %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100) %>%
  select(`Water Year`,Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,Alt3,Alt4)
write.csv(bio.at.chips_tableb, paste0("./springRunDSM-Main/Output/Tables/",out.name,"_SRKW_SR Juv Biomass at Chipps Perc Diff.csv"),
          row.names=FALSE)


# Total juvenile abundance at Golden Gate, across size classes

  # Creating summary object
juv.at.GG_df <- data.frame(`Water Year`=numeric(),
                           Alternative=character(),
                           Abundance=numeric())

for(i in 1:n.alt){
  juv.at.GG_df_temp <- input_data[[i]]$out.juv.at.chips %>%
    reshape2::melt(varnames=c("Year","Watershed","Size")) %>%
    mutate(Alternative=alt_plot_names[i], Abundance=round(value*Bay.S),
           `Water Year`=Year+1) %>%
    group_by(`Water Year`, Alternative) %>%
    summarize(Abundance=sum(Abundance))
  
  juv.at.GG_df <- rbind(juv.at.GG_df, juv.at.GG_df_temp)
}

  # Plotting
juv.at.GG <- ggplot(juv.at.GG_df, aes(x=`Water Year`, y=Abundance, col=Alternative))+
  geom_line() +
  labs(y="Juvenile Abundance passing Golden Gate", x="Water Year")
juv.at.GG
ggsave(paste0("./springRunDSM-Main/Output/Figures/",out.name,"_SRKW_Juv Abund at GG.png"),
       width=6, height=5, units=c("in"))

  # Base table
juv.at.GG_table <- juv.at.GG_df %>%
  pivot_wider(names_from=Alternative, values_from=Abundance)
write.csv(juv.at.GG_table, paste0("./springRunDSM-Main/Output/Tables/",out.name,"_SRKW_SR Juv Abund at GG.csv"),
          row.names=FALSE)

  # Percent difference table (EIS only)
juv.at.GG_tableb <- juv.at.GG_table %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100) %>%
  select(`Water Year`,Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,Alt3,Alt4)
write.csv(juv.at.GG_tableb, paste0("./springRunDSM-Main/Output/Tables/",out.name,"_SRKW_SR Juv Abund at GG Perc Diff.csv"),
          row.names=FALSE)


# Total juvenile biomass at Golden Gate, across size classes

  # Creating summary object
bio.at.GG_df <- data.frame(`Water Year`=numeric(),
                           Alternative=character(),
                           Biomass=numeric())

for(i in 1:n.alt){
  bio.at.GG_df_temp <- input_data[[i]]$out.juv.at.chips %>%
    reshape2::melt(varnames=c("Year","Watershed","Size")) %>%
    mutate(Alternative=alt_plot_names[i], 
           Biomass=ifelse(Size=="s", round(value*Bay.S)*bio[1],
                          ifelse(Size=="m", round(value*Bay.S)*bio[2],
                                 ifelse(Size=="l", round(value*Bay.S)*bio[3], 
                                        round(value*Bay.S)*bio[4]))),
           `Water Year`=Year+1) %>%
    group_by(`Water Year`, Alternative) %>%
    summarize(Biomass=sum(Biomass))
  
  bio.at.GG_df <- rbind(bio.at.GG_df, bio.at.GG_df_temp)
}

  # Plotting
bio.at.GG <- ggplot(bio.at.GG_df, aes(x=`Water Year`, y=Biomass/1000, col=Alternative))+
  geom_line() +
  labs(y="Juvenile Biomass passing Golden Gate (kg)", x="Water Year")
bio.at.GG
ggsave(paste0("./springRunDSM-Main/Output/Figures/",out.name,"_SRKW_Juv Biomass at GG.png"),
       width=6, height=5, units=c("in"))

  # Base table
bio.at.GG_table <- bio.at.GG_df %>%
  pivot_wider(names_from=Alternative, values_from=Biomass)
write.csv(bio.at.GG_table, paste0("./springRunDSM-Main/Output/Tables/",out.name,"_SRKW_SR Juv Biomass at GG.csv"),
          row.names=FALSE)

  # Percent difference table (EIS only)
bio.at.GG_tableb <- bio.at.GG_table %>%
  mutate(Alt1=(Alt1-NAA)/NAA*100,
         `Alt2wTUCPwoVA`=(`Alt2wTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPwoVA`=(`Alt2woTUCPwoVA`-NAA)/NAA*100,
         `Alt2woTUCPDeltaVA`=(`Alt2woTUCPDeltaVA`-NAA)/NAA*100,
         `Alt2woTUCPAllVA`=(`Alt2woTUCPAllVA`-NAA)/NAA*100,
         Alt3=(Alt3-NAA)/NAA*100,
         Alt4=(Alt4-NAA)/NAA*100) %>%
  select(`Water Year`,Alt1,`Alt2wTUCPwoVA`,`Alt2woTUCPwoVA`,`Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,Alt3,Alt4)
write.csv(bio.at.GG_tableb, paste0("./springRunDSM-Main/Output/Tables/",out.name,"_SRKW_SR Juv Biomass at GG Perc Diff.csv"),
          row.names=FALSE)


