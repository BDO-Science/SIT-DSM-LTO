
#################
# Annotation
  # This script imports winter-run DSM output data and plots summaries of relevant outputs
  # Users can change the following settings in 'Plotting Settings' to change what is plotted:
    # Whether to analyze and plot determinsitic or stochastic model output
      # model_type = "Det" (Deterministic model output) or "Stoch" (Stochastic output)
        # If model_type = "Stoch", provide the correct n.iter value
#################

###################
# Plotting Settings
###################

# Indicate model run type
model_type <- "Det"
model_type <- "Stoch"; n.iter=100

# Indicate set of alternatives to compare
out.name <- "EIS_Act5" # NAA, Alt1-4 (4 phases of Alt2) used EIS set for Action 5
out.name <- "BA_Act5"

#####################
# Load packages, inputs
#####################

require(dplyr); require(here)
require(tidyr); 
require(reshape2); 
require(ggplot2)

if(out.name=="BA_Act5"){ # EIS (NAA, Alternatives 1-4)
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
} 

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

# Color reference
  # Update this with final hexcodes!
  # *Ensure order of 'Alternative' matches order of alternatives in alt_plot_names
col_ref <- data.frame(Alternative=c(#"EXP1","EXP3",
                                    "NAA","Alt1",
                                    "Alt2wTUCPwoVA", "Alt2woTUCPwoVA",
                                    "Alt2woTUCPDeltaVA", "Alt2woTUCPAllVA",
                                    "Alt3","Alt4", "Action5"),
                      Hexcode=c("#88CCEE", #"#CC6677",
                                "#DDCC77", "#117733",
                                "#332288", "#AA4499",
                                "#44AA99", "#999933",
                                "#882255", "#661100"))

# Load WYT reference for plotting
wyt_ref <- read.csv("./winterRunDSM-Main/wyt_ref.csv")

#############################################
# Phase 2
  # Calculate summaries of individual demographic rates
#############################################

# Boxplots summarizing 1980-2000
  # Across all WYTs

  ###############
  # Deterministic
  ###############

if(model_type=="Det"){
  
  # Adult en route survival
  
  adult_df <- data.frame(Year=numeric(), Month=numeric(),
                         Watershed=character(), value=numeric(),
                         Alternative=character())
  
  for(i in 1:n.alt){
  
    adult_df_temp <- input_data[[i]]$out.adult_en_route %>%
      reshape2::melt(varnames=c("Year","Month","Watershed")) %>%
      filter(Watershed=="Upper Sacramento River") %>%
      mutate(Month=factor(Month, levels=c(1:4)), Alternative=alt_plot_names[i])
    
    adult_df <- rbind(adult_df, adult_df_temp)
  
  }

  adult_en_route <- ggplot(adult_df, 
                           aes(x=Month, y=value, 
                               fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Adult En Route Survival", fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(adult_df, adult_df_temp)

  # **Summarize adult pre-spawn survival the Upper Sacramento River**
  
  adult_df <- data.frame(Year=numeric(), Season=character(),
                         Watershed=character(), value=numeric(),
                         Alternative=character())
  
  for(i in 1:n.alt){
    adult_df_temp <- input_data[[i]]$out.pre.spawn.S %>%
      reshape2::melt(varnames=c("Year","Season","Watershed")) %>%
      filter(Watershed=="Upper Sacramento River") %>%
      mutate(Alternative=alt_plot_names[i])
    
    adult_df <- rbind(adult_df, adult_df_temp)
  }

  pre.spawn.S <- ggplot(adult_df, 
                        aes(x=Season, y=value, 
                            fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot()+
    labs(x="Season", y="Adult Pre-Spawn Survival", fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(adult_df_temp, adult_df)

  # **Summarize in-channel rearing survival in the Upper Sacramento River (< 42 mm)**
  
  rear_df <- data.frame(Year=numeric(), Month=numeric(),
                         Watershed=character(), Size=character(), 
                         value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    rear_df_temp <- input_data[[i]]$out.river_surv %>%
      reshape2::melt(varnames=c("Year","Month","Watershed","Size")) %>%
      filter(Watershed=="Upper Sacramento River") %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])
  
    rear_df <- rbind(rear_df, rear_df_temp)
  }  
  
  rear_small_df <- rear_df %>% filter(Size=="s")
  
  river_surv <- ggplot(rear_small_df, 
                       aes(x=Month, y=value, 
                           fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Rearing Survival Upper Sacramento (<42 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(rear_df, rear_df_temp, rear_small_df)

  # Summarize North Delta rearing survival (< 42 mm)
  
  rear_df <- data.frame(Year=numeric(), Month=numeric(),
                        Delta=character(), Size=character(), 
                        value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    rear_df_temp <- input_data[[i]]$out.D_juv_surv %>%
      reshape2::melt(varnames=c("Year","Month","Delta", "Size")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])
  
    rear_df <- rbind(rear_df, rear_df_temp)
  }

  rear_N_small_df <- rear_df %>%
    filter(Delta=="N", Size=="s")
  
  D_juv_surv <- ggplot(rear_N_small_df, 
                       aes(x=Month, y=value, 
                           fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="North Delta Rearing Survival (<42 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(rear_df_temp, rear_df, rear_N_small_df)
  
  # **Proportion migrating as a function of pulse flows (<42 mm)**
  
  pulse_df <- data.frame(Year=numeric(), Month=numeric(),
                        Watershed=character(), Size=character(), 
                        value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    pulse_df_temp <- input_data[[i]]$out.p.pulse.leave %>%
      reshape2::melt(varnames=c("Year","Month","Watershed", "Size")) %>%
      filter(Watershed=="Upper Sacramento River") %>%  
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])
  
    pulse_df <- rbind(pulse_df, pulse_df_temp)
  }
  
  pulse_small_df <- pulse_df %>% filter(Size=="s")
  
  p.pulse.leave <- ggplot(pulse_small_df, 
                          aes(x=Month, y=value, 
                              col=factor(Alternative, levels=alt_plot_names)))+
    geom_point() +
    labs(x="Month", y="Upper Sac. River Migration from Pulse Flows (<42 mm)",
         col="Alternative")#+
    #scale_colour_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(pulse_df_temp, pulse_df, pulse_small_df)

  # **Migratory survival through the Upper-Mid Sacramento River (>110 mm)**
  
  migr_df <- data.frame(Year=numeric(), Month=numeric(),
                        Size=character(), 
                        value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    migr_df_temp <- input_data[[i]]$out.UM.Sac.S %>%
      reshape2::melt(varnames=c("Year","Month","Size")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])

    migr_df <- rbind(migr_df, migr_df_temp)
  }
  
  migr_small_df <- migr_df %>% filter(Size=="vl")
  
  UM.Sac.S <- ggplot(migr_small_df, 
                     aes(x=Month, y=value, 
                         fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Upper-mid Sacramento Migratory Survival (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df_temp, migr_df, migr_small_df)

  # Summarize proportion of fish entrained into the South Delta
  
  entrain_df <- data.frame(Year=numeric(), Month=numeric(),
                        value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    entrain_df_temp <- input_data[[i]]$out.prop.dlt.entrain %>%
      reshape2::melt(varnames=c("Year","Month")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])

    entrain_df <- rbind(entrain_df, entrain_df_temp)
  }

  prop.dlt.entrain <- ggplot(entrain_df, 
                             aes(x=Month, y=value, 
                                 fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Proportional Entrainment to South Delta",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(entrain_df_temp, entrain_df)

  # Migratory survival through the North Delta (>110 mm)
  
  migr_df <- data.frame(Year=numeric(), Month=numeric(),
                        Delta=character(), Size=character(), 
                        value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    migr_df_temp <- input_data[[i]]$out.Sac.Delt.S %>%
      reshape2::melt(varnames=c("Year","Month","Delta","Size")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])
  
    migr_df <- rbind(migr_df, migr_df_temp)
  }
    
  migr_N_df <- migr_df %>% filter(Delta=="N", Size=="vl")
  
  Sac.Delt.S <- ggplot(migr_N_df, 
                       aes(x=Month, y=value, 
                           fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Migratory Survival through North Delta (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])

  rm(migr_df_temp, migr_df, migr_N_df)

  # Migratory survival through South Delta for North origin fish (>110 mm)
  
  migr_df <- data.frame(Year=numeric(), Month=numeric(),
                        Origin=character(), Size=character(), 
                        value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    migr_df_temp <- input_data[[i]]$out.newDsurv %>%
      reshape2::melt(varnames=c("Year","Month","Origin","Size")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])

    migr_df <- rbind(migr_df, migr_df_temp)
  }

  migr_N_df <- migr_df %>% filter(Origin=="N", Size=="vl")
  
  newDsurv <- ggplot(migr_N_df, 
                     aes(x=Month, y=value, 
                         fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Migratory Survival through South Delta (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df, migr_N_df, migr_df_temp)

  # Upper-mid Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
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

  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  UM.Sac.S.facet <- ggplot(migr_small_df, 
                           aes(x=WYT, y=value, 
                               fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="WYT", y="Upper-mid Sacramento Migratory Survival (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df_temp, migr_df, migr_small_df)

  # Lower-mid Migratory Survival, grouped by WYT, faceted by month (>110 mm)

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

  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  LM.Sac.S.facet <- ggplot(migr_small_df, 
                           aes(x=WYT, y=value, 
                               fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="WYT", y="Lower-mid Sacramento Migratory Survival (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df_temp, migr_df, migr_small_df)
  
  # Lower Migratory Survival, grouped by WYT, faceted by month (>110 mm)

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

  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  LL.Sac.S.facet <- ggplot(migr_small_df, 
                           aes(x=WYT, y=value, 
                               fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="WYT", y="Lower Sacramento Migratory Survival (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df_temp, migr_df, migr_small_df)
  
  # N. Delta Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
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

  migr_N_df <- migr_df %>% filter(Delta=="N", Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  Sac.Delt.S.facet <- ggplot(migr_N_df, 
                             aes(x=WYT, y=value, 
                                 fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="WYT", y="Migratory Survival through North Delta (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df_temp, migr_df, migr_N_df)

  # S. Delta Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
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

  migr_N_df <- migr_df %>% filter(Origin=="N", Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  newDsurv.facet <- ggplot(migr_N_df, 
                           aes(x=WYT, y=value, 
                               fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="WYT", y="Migratory Survival through South Delta (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df_temp, migr_df, migr_N_df)
  
}

  ############
  # Stochastic
  ############

if(model_type=="Stoch"){
  
  # Summarize adult en route survival to the Upper Sacramento River
  
  adult_df <- data.frame(Year=numeric(), Month=numeric(),
                         Watershed=character(), Iteration=numeric(),
                         value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    adult_df_temp <- input_data[[i]]$out.adult_en_route %>%
      reshape2::melt(varnames=c("Year","Month","Watershed","Iteration")) %>%
      filter(Watershed=="Upper Sacramento River") %>%
      mutate(Month=factor(Month, levels=c(1:4)), Alternative=alt_plot_names[i])
    
    adult_df <- rbind(adult_df, adult_df_temp)
  }

  adult_en_route <- ggplot(adult_df, 
                           aes(x=Month, y=value, 
                               fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Adult En Route Survival", fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(adult_df, adult_df_temp)

  # Summarize adult pre-spawn survival the Upper Sacramento River
  
  adult_df <- data.frame(Year=numeric(), Season=character(),
                         Watershed=character(), Iteration=numeric(),
                         value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    adult_df_temp <- input_data[[i]]$out.pre.spawn.S %>%
      reshape2::melt(varnames=c("Year","Season","Watershed","Iteration")) %>%
      filter(Watershed=="Upper Sacramento River") %>%
      mutate(Alternative=alt_plot_names[i])
  
    adult_df <- rbind(adult_df, adult_df_temp)
  }

  pre.spawn.S <- ggplot(adult_df, 
                        aes(x=Season, y=value, 
                            fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot()+
    labs(x="Season", y="Adult Pre-Spawn Survival", fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(adult_df, adult_df_temp)

  # Summarize in-channel rearing survival in the Upper Sacramento River (< 42 mm)
  
  rear_df <- data.frame(Year=numeric(), Month=numeric(),
                        Watershed=character(), Size=character(),
                        Iteration=numeric(),
                        value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    rear_df_temp <- input_data[[i]]$out.river_surv %>%
      reshape2::melt(varnames=c("Year","Month","Watershed","Size","Iteration")) %>%
      filter(Watershed=="Upper Sacramento River") %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])

    rear_df <- rbind(rear_df, rear_df_temp)
  }
  
  rear_small_df <- rear_df %>% filter(Size=="s")
  
  river_surv <- ggplot(rear_small_df, 
                       aes(x=Month, y=value, 
                           fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Rearing Survival Upper Sacramento (<42 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(rear_df, rear_small_df, rear_df_temp)

  # Summarize North Delta rearing survival (< 42 mm)
  
  rear_df <- data.frame(Year=numeric(), Month=numeric(),
                        Delta=character(), Size=character(), 
                        Iteration=character(),
                        value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    rear_df_temp <- input_data[[i]]$out.D_juv_surv %>%
      reshape2::melt(varnames=c("Year","Month","Delta", "Size","Iteration")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])
    
    rear_df <- rbind(rear_df, rear_df_temp)
  }

  rear_N_small_df <- rear_df %>%
    filter(Delta=="N", Size=="s")
  
  D_juv_surv <- ggplot(rear_N_small_df, 
                       aes(x=Month, y=value, 
                           fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="North Delta Rearing Survival (<42 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(rear_df, rear_N_small_df, rear_df_temp)

  # Proportion migrating in the Upper Sacramento River as a function of pulse flows (<42 mm)
  
  pulse_df <- data.frame(Year=numeric(), Month=numeric(),
                         Watershed=character(), Size=character(),
                         Iteration=character(),
                         value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    pulse_df_temp <- input_data[[i]]$out.p.pulse.leave %>%
      reshape2::melt(varnames=c("Year","Month","Watershed", "Size","Iteration")) %>%
      filter(Watershed=="Upper Sacramento River") %>%  
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])
  
    pulse_df <- rbind(pulse_df, pulse_df_temp)
  }
  
  pulse_small_df <- pulse_df %>% filter(Size=="s")
  
  p.pulse.leave <- ggplot(pulse_small_df, 
                          aes(x=Month, y=value, 
                              col=factor(Alternative, levels=alt_plot_names)))+
    geom_point() +
    labs(x="Month", y="Upper Sac. River Migration from Pulse Flows (<42 mm)",
         col="Alternative")#+
    #scale_colour_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(pulse_df, pulse_small_df, pulse_df_temp)
  
  # Migratory survival through the Upper-Mid Sacramento River (>110 mm)
  
  migr_df <- data.frame(Year=numeric(), Month=numeric(),
                        Size=character(), Iteration=numeric(),
                        value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    migr_df_temp <- input_data[[i]]$out.UM.Sac.S %>%
      reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])
  
    migr_df <- rbind(migr_df, migr_df_temp)
  }

  migr_small_df <- migr_df %>% filter(Size=="vl")
  
  UM.Sac.S <- ggplot(migr_small_df, 
                     aes(x=Month, y=value, 
                         fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Upper-mid Sacramento Migratory Survival (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df, migr_small_df, migr_df_temp)

  # Summarize proportion of fish entrained into the South Delta
  
  entrain_df <- data.frame(Year=numeric(), Month=numeric(),
                           Iteration=numeric(),
                           value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    entrain_df_temp <- input_data[[i]]$out.prop.dlt.entrain %>%
      reshape2::melt(varnames=c("Year","Month","Iteration")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])
  
    entrain_df <- rbind(entrain_df, entrain_df_temp)
  }

  prop.dlt.entrain <- ggplot(entrain_df, 
                             aes(x=Month, y=value, 
                                 fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Proportional Entrainment to South Delta",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(entrain_df, entrain_df_temp)

  # Migratory survival through the North Delta (>110 mm)
  
  migr_df <- data.frame(Year=numeric(), Month=numeric(),
                        Delta=character(), Size=character(), 
                        Iteration=numeric(),
                        value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    migr_df_temp <- input_data[[i]]$out.Sac.Delt.S %>%
      reshape2::melt(varnames=c("Year","Month","Delta","Size","Iteration")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])
  
    migr_df <- rbind(migr_df, migr_df_temp)
  }

  migr_N_df <- migr_df %>% filter(Delta=="N", Size=="vl")
  
  Sac.Delt.S <- ggplot(migr_N_df, 
                       aes(x=Month, y=value, 
                           fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Migratory Survival through North Delta (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df, migr_N_df, migr_df_temp)

  # Migratory survival through South Delta for North origin fish (>110 mm)
  
  migr_df <- data.frame(Year=numeric(), Month=numeric(),
                        Origin=character(), Size=character(), 
                        Iteration=numeric(),
                        value=numeric(), Alternative=character())
  
  for(i in 1:n.alt){
    migr_df_temp <- input_data[[i]]$out.newDsurv %>%
      reshape2::melt(varnames=c("Year","Month","Origin","Size","Iteration")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i])

    migr_df <- rbind(migr_df, migr_df_temp)
  }

  migr_N_df <- migr_df %>% filter(Origin=="N", Size=="vl")
  
  newDsurv <- ggplot(migr_N_df, 
                     aes(x=Month, y=value, 
                         fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    labs(x="Month", y="Migratory Survival through South Delta (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df, migr_N_df, migr_df_temp)

  # Upper-mid Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df <- data.frame(Year=numeric(), Month=numeric(),
                        Size=character(), Iteration=numeric(),
                        value=numeric(), 
                        Alternative=character(), WY=numeric())
  
  for(i in 1:n.alt){
    migr_df_temp <- input_data[[i]]$out.UM.Sac.S %>%
      reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
             WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))

    migr_df <- rbind(migr_df, migr_df_temp)
  }

  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  UM.Sac.S.facet <- ggplot(migr_small_df, 
                           aes(x=WYT, y=value, 
                               fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="WYT", y="Upper-mid Sacramento Migratory Survival (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df_temp, migr_df, migr_small_df)
  
  # Lower-mid Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df <- data.frame(Year=numeric(), Month=numeric(),
                        Size=character(), Iteration=numeric(),
                        value=numeric(), 
                        Alternative=character(), WY=numeric())
  
  for(i in 1:n.alt){
    migr_df_temp <- input_data[[i]]$out.LM.Sac.S %>%
      reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
             WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))

    migr_df <- rbind(migr_df, migr_df_temp)
  }

  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  LM.Sac.S.facet <- ggplot(migr_small_df, 
                           aes(x=WYT, y=value, 
                               fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="WYT", y="Lower-mid Sacramento Migratory Survival (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df_temp, migr_df, migr_small_df)
  
  # Lower Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df <- data.frame(Year=numeric(), Month=numeric(),
                        Size=character(), Iteration=numeric(), 
                        value=numeric(), 
                        Alternative=character(), WY=numeric())
  
  for(i in 1:n.alt){
    migr_df_temp <- input_data[[i]]$out.LL.Sac.S %>%
      reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
             WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))

    migr_df <- rbind(migr_df, migr_df_temp)
  }

  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  LL.Sac.S.facet <- ggplot(migr_small_df, 
                           aes(x=WYT, y=value, 
                               fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="WYT", y="Lower Sacramento Migratory Survival (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df_temp, migr_df, migr_small_df)
  
  # N. Delta Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df <- data.frame(Year=numeric(), Month=numeric(), Delta=character(),
                        Size=character(), Iteration=numeric(), 
                        value=numeric(), 
                        Alternative=character(), WY=numeric())
  
  for(i in 1:n.alt){
    migr_df_temp <- input_data[[i]]$out.Sac.Delt.S %>%
      reshape2::melt(varnames=c("Year","Month","Delta","Size","Iteration")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
             WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))

    migr_df <- rbind(migr_df, migr_df_temp)
  }

  migr_N_df <- migr_df %>% filter(Delta=="N", Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  Sac.Delt.S.facet <- ggplot(migr_N_df, 
                             aes(x=WYT, y=value, 
                                 fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="WYT", y="Migratory Survival through North Delta (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df_temp, migr_df, migr_N_df)
  
  # S. Delta Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df <- data.frame(Year=numeric(), Month=numeric(), Origin=character(),
                        Size=character(), Iteration=numeric(),
                        value=numeric(), 
                        Alternative=character(), WY=numeric())
  
  for(i in 1:n.alt){
    migr_df_temp <- input_data[[i]]$out.newDsurv %>%
      reshape2::melt(varnames=c("Year","Month","Origin","Size","Iteration")) %>%
      mutate(Month=factor(Month, levels=c(9:12,1:5)), Alternative=alt_plot_names[i],
             WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
    migr_df <- rbind(migr_df, migr_df_temp)
  }

  migr_N_df <- migr_df %>% filter(Origin=="N", Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  newDsurv.facet <- ggplot(migr_N_df, 
                           aes(x=WYT, y=value, 
                               fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="WYT", y="Migratory Survival through South Delta (>110 mm)",
         fill="Alternative")#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  rm(migr_df_temp, migr_df, migr_N_df)
  
}


##################################
# Phase 3
  # Summary of life cycle outcomes
##################################
  #####################
  # Lambda calculations
    # Based on all spawners to avoid 0 abundances
  #####################

if(model_type=="Det"){
  
  lambda <- array(NA, dim=c(n.alt,19))
  lambda_mean <- numeric(n.alt)
  end_lambda <- numeric(n.alt)
  
  lambda_plot <- data.frame(Year=numeric(), Alternative=character(),
                            Lambda=numeric())
  lambda_diff_plot <- data.frame(Year=numeric(), Alternative=character(),
                            Lambda_diff=numeric())
  
  for(j in 1:n.alt){
    for(i in 2:20){
      lambda[j,i-1] <- input_data[[j]]$out.all_spawners[1,i]/input_data[[j]]$out.all_spawners[1,i-1]
    }
    lambda_mean[j] <- exp(mean(log(lambda[j,])))
    end_lambda[j] <- input_data[[j]]$out.all_spawners[1,20]/input_data[[j]]$out.all_spawners[1,1]
    
    lambda_plot_temp <- data.frame(Year=c(1981:1999),
                                   Alternative=rep(alt_plot_names[j],19),
                                   Lambda=lambda[j,])
    lambda_plot <- rbind(lambda_plot, lambda_plot_temp)
  }
  
  for(j in 2:n.alt){
    lambda_diff_plot_temp <- data.frame(Year=c(1981:1999),
                                        Alternative=rep(alt_plot_names[j],19),
                                        Lambda_diff=c((lambda[j,]-lambda[1,])/lambda[1,]*100))
    lambda_diff_plot <- rbind(lambda_diff_plot, lambda_diff_plot_temp)
    
  }

}
lambda_mean
end_lambda


if(model_type=="Stoch"){
  
  lambda <- array(NA, dim=c(n.alt,19, input_data[[1]]$n.iter), 
                  dimnames=list(alt_plot_names,1981:1999,1:input_data[[1]]$n.iter))
  lambda_mean <- array(NA, dim=c(n.alt, input_data[[1]]$n.iter))
  end_lambda <- array(NA, dim=c(n.alt, input_data[[1]]$n.iter))
  
  lambda_df <- data.frame(Lambda_mean=numeric(), Lambda_end=numeric(),
                          Alternative=character())

  for(k in 1:n.alt){
    for(j in 1:input_data[[1]]$n.iter){
      for(i in 2:20){
        lambda[k,i-1,j] <- input_data[[k]]$out.all_spawners[1,i,j]/input_data[[k]]$out.all_spawners[1,i-1,j]
      }
      lambda_mean[k,j] <- exp(mean(log(lambda[k,,j])))
      end_lambda[k,j] <- input_data[[k]]$out.all_spawners[1,20,j]/input_data[[k]]$out.all_spawners[1,1,j]
    }
    lambda_df_temp <- data.frame(Lambda_mean=lambda_mean[k,], 
                                 Lambda_end=end_lambda[k,], 
                                 Alternative=alt_plot_names[k])
    lambda_df <- rbind(lambda_df, lambda_df_temp)
  }
  
  # Create data frame of all lambdas for plotting
  lambda_df_raw <- lambda %>%
    reshape2::melt(varnames=c("Alternative","Year","Iteration")) %>%
    mutate("Lambda"=value)
  
  lamda_df_wyt <- lambda_df_raw %>%
    mutate(WY = Year) %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
}
apply(lambda_mean, 1, summary)
apply(end_lambda, 1, summary)
apply(lambda, c(1, 2), summary)

  ############################
  # Set up deterministic plots
  ############################

if(model_type=="Det"){

  # Assign proper names
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
    dimnames(input_data[[i]]$out.nat_spawners) <- list(watershed.names, year.names)
  }

  # All spawners
  
  spawn_df <- data.frame(Watershed=character(), Year=numeric(),
                         value=numeric(), Alternative=character(),
                         `All Spawners`=character())
  
  for(i in 1:n.alt){
    spawn_df_temp <- input_data[[i]]$out.all_spawners %>%
      reshape2::melt(varnames=c("Watershed","Year")) %>%
      filter(Watershed=="Upper Sacramento River") %>%
      mutate(Alternative=alt_plot_names[i], `All Spawners`=value)
  
    spawn_df <- rbind(spawn_df, spawn_df_temp)
  }

  all_spawners <- ggplot(spawn_df, aes(x=Year, y=`All Spawners`, 
                                       col=factor(Alternative, levels=alt_plot_names)))+
    geom_line()+
    labs(col="Alternative")#+
    #scale_colour_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  # Nat spawners
  
  spawn_df <- data.frame(Watershed=character(), Year=numeric(),
                         value=numeric(), Alternative=character(),
                         `Natural Spawners`=character())
  
  for(i in 1:n.alt){
    spawn_df_temp <- input_data[[i]]$out.nat_spawners %>%
      reshape2::melt(varnames=c("Watershed","Year")) %>%
      filter(Watershed=="Upper Sacramento River") %>%
      mutate(Alternative=alt_plot_names[i], `Natural Spawners`=value)
  
    spawn_df <- rbind(spawn_df, spawn_df_temp)
  }

  nat_spawners <- ggplot(spawn_df, aes(x=Year, y=`Natural Spawners`, 
                                       col=factor(Alternative, levels=alt_plot_names)))+
    geom_line()+
    labs(col="Alternative")#+
    #scale_colour_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])

  # Natural spawners, relative to some baseline
    # Here, use NAA as baseline for comparison

  spawn_df_ref <- input_data[[1]]$out.nat_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[1], `Natural Spawners`=value) %>%
    group_by(Year) %>%
    summarize(Mean=mean(`Natural Spawners`), 
              Lo_95=quantile(`Natural Spawners`,0.025),
              Hi_95=quantile(`Natural Spawners`,0.975))
  
  spawn_df <- data.frame(Year=numeric(), Mean=numeric(),
                         Lo_95=numeric(), Hi_95=numeric())
  
  for(i in 2:n.alt){
    spawn_df_temp <- input_data[[i]]$out.nat_spawners %>%
      reshape2::melt(varnames=c("Watershed","Year")) %>%
      filter(Watershed=="Upper Sacramento River") %>%
      mutate(Alternative=alt_plot_names[i], `Natural Spawners`=value,) %>%
      group_by(Year) %>%
      summarize(Mean=mean(`Natural Spawners`), 
                Lo_95=quantile(`Natural Spawners`,0.025),
                Hi_95=quantile(`Natural Spawners`,0.975)) %>%
      mutate(`% Difference (Mean)`=(.$Mean-spawn_df_ref$Mean)/spawn_df_ref$Mean*100,
             `% Difference (2.5% Quant)`=(.$Lo_95-spawn_df_ref$Lo_95)/spawn_df_ref$Lo_95*100,
             `% Difference (97.5% Quant)`=(.$Hi_95-spawn_df_ref$Hi_95)/spawn_df_ref$Hi_95*100,
             Alternative=alt_plot_names[i])

    spawn_df <- rbind(spawn_df, spawn_df_temp)
  }
  
  nat_spawners_diff <- ggplot(spawn_df, aes(x=Year, y=`% Difference (Mean)`, 
                                            col=factor(Alternative, levels=alt_plot_names)))+
    geom_line()+
    labs(y="% Difference in Natural Spawners Relative to NAA",
         col="Alternative")#+
    #scale_colour_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  # Lambda plot
  
  lambda_all <- ggplot(lambda_plot, aes(x=Year, y=Lambda, 
                                        col=factor(Alternative, levels=alt_plot_names)))+
    geom_line()+
    labs(y="Lambda", col="Alternative")#+
    #scale_colour_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)])
  
  # % Difference in Lambda Plot
  
  lambda_all_diff <- ggplot(lambda_diff_plot, aes(x=Year, y=Lambda_diff, 
                                                  col=factor(Alternative, levels=alt_plot_names)))+
    geom_line()+
    labs(y="% Difference in Lambda Relative to originalDSM",
         col="Alternative")#+
    #scale_colour_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)]) 
}

  #########################
  # Set up stochastic plots
  #########################

if(model_type=="Stoch"){

  # Assign proper names
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
  iter.names <- 1:input_data[[1]]$n.iter
  
  for(i in 1:n.alt){
    dimnames(input_data[[i]]$out.all_spawners) <- list(watershed.names, year.names, iter.names)
    dimnames(input_data[[i]]$out.nat_spawners) <- list(watershed.names, year.names, iter.names)
  }
    
  # All spawners
  
  spawn_df <- data.frame(Watershed=character(), Year=numeric(),
                         Iteration=numeric(),
                         value=numeric(), Alternative=character(),
                         `All Spawners`=character())
  
  for(i in 1:n.alt){
    spawn_df_temp <- input_data[[i]]$out.all_spawners %>%
      reshape2::melt(varnames=c("Watershed","Year","Iteration")) %>%
      filter(Watershed=="Upper Sacramento River") %>%
      mutate(Alternative=alt_plot_names[i], `All Spawners`=value)

    spawn_df <- rbind(spawn_df, spawn_df_temp)
  }

  all_spawners <- ggplot(spawn_df, aes(x=Year, y=`All Spawners`))+
    geom_line(col="black", aes(group=Iteration))+
    geom_smooth(spawn_df, mapping=aes(x=Year, y=`All Spawners`), method="gam")+  
    facet_wrap(~factor(Alternative, levels=alt_plot_names))
  
  # Natural spawners
  
  spawn_df <- data.frame(Watershed=character(), Year=numeric(),
                         Iteration=numeric(),
                         value=numeric(), Alternative=character(),
                         `Natural Spawners`=character())
  
  for(i in 1:n.alt){
    spawn_df_temp <- input_data[[i]]$out.nat_spawners %>%
      reshape2::melt(varnames=c("Watershed","Year","Iteration")) %>%
      filter(Watershed=="Upper Sacramento River") %>%
      mutate(Alternative=alt_plot_names[i], `Natural Spawners`=value)

    spawn_df <- rbind(spawn_df, spawn_df_temp)
  }

  nat_spawners <- ggplot(spawn_df, aes(x=Year, y=`Natural Spawners`))+
    geom_line(col="black", aes(group=Iteration))+
    geom_smooth(spawn_df, mapping=aes(x=Year, y=`Natural Spawners`), method="gam")+ # how do 
    facet_wrap(~factor(Alternative, levels=alt_plot_names))
  
  # Iteration-specific lambda values (boxplots by Scenario)
  
  lambda_mean <- ggplot(lambda_df, aes(x=factor(Alternative, levels=alt_plot_names), 
                                       y=Lambda_mean))+
    geom_boxplot()+
    labs(y="Mean Lambda", x="Alternative") +
    scale_x_discrete(guide=guide_axis(n.dodge=2))
  
  lambda_mean_wyt <- ggplot(lamda_df_wyt, aes(x=WYT, y=Lambda, 
                                              fill=factor(Alternative, levels=alt_plot_names)))+
    geom_boxplot()+
    labs(y="Lambda", x="WYT", fill="Alternative") +
    scale_x_discrete(guide=guide_axis(n.dodge=2))#+
    #scale_fill_manual(values=col_ref$Hexcode[which(col_ref$Alternative%in%alt_plot_names)]) 
  
  lambda_end <- ggplot(lambda_df, aes(x=factor(Alternative, levels=alt_plot_names), 
                                      y=Lambda_end))+
    geom_boxplot()+
    labs(y="Lambda Based on Last and First Year", x="Alternative") +
    scale_x_discrete(guide=guide_axis(n.dodge=2))
  
  # Iteration-specific lambda values (time series)
  
  lambda_all <- ggplot(lambda_df_raw, aes(x=Year, y=Lambda))+
    geom_line(aes(group=Iteration))+
    geom_smooth(lambda_df_raw, mapping=aes(x=Year, y=Lambda), method="gam")+
    geom_hline(yintercept=1)+
    facet_wrap(~factor(Alternative, levels=alt_plot_names))
  
}

################
# Generate, save all plots
################

# Population parameters (abundance, growth rate)
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_All Spawners.png"),
       all_spawners, width=6, height=6, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Natural Spawners.png"),
       nat_spawners, width=6, height=6, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Lambda Timeseries.png"),
       lambda_all, width=6, height=6, units=c("in"))

if(model_type=="Det"){
  ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Natural Spawners Perc Diff.png"),
         nat_spawners_diff, width=6, height=4, units=c("in"))
  ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Lambda Timeseries Perc Diff.png"),
         lambda_all_diff, width=6, height=4, units=c("in"))
}
if(model_type=="Stoch"){
  ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Lambda Mean Boxplot.png"),
         lambda_mean, width=6, height=4, units=c("in"))
  ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Lambda End Boxplot.png"),
         lambda_end, width=6, height=4, units=c("in"))
  ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Lambda WYT Boxplot.png"),
         lambda_mean_wyt, width=6, height=4, units=c("in"))
}

# Life state transition parameters, not grouped by month + WYT
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Adult En Route.png"),
       adult_en_route, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Adult Prespawn.png"),
       pre.spawn.S, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Juv Upper Sac Rear S.png"),
       river_surv, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Juv N Delta Rear S.png"),
       D_juv_surv, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Prop Pulse Leave S.png"),
       p.pulse.leave, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Prop Pulse Leave S.png"),
       p.pulse.leave, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Juv UM Sac Migr VL.png"),
       UM.Sac.S, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_S Delta Entrainment.png"),
       prop.dlt.entrain, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Juv N Delta Migr VL.png"),
       Sac.Delt.S, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Juv S Delta Migr VL.png"),
       newDsurv, width=6, height=5, units=c("in"))

# Migratory survival parameters, vl size, grouped by month + WYT

ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Juv UM Sac Migr VL Facet.png"),
       UM.Sac.S.facet, width=7, height=9, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Juv LM Sac Migr VL Facet.png"),
       LM.Sac.S.facet, width=7, height=9, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Juv LL Sac Migr VL Facet.png"),
       LL.Sac.S.facet, width=7, height=9, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Juv N Delta Migr VL Facet.png"),
       Sac.Delt.S.facet, width=7, height=9, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/Figures/",out.name,"_",model_type,"_Juv S Delta Migr VL Facet.png"),
       newDsurv.facet, width=7, height=9, units=c("in"))
