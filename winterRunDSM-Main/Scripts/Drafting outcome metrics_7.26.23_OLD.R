
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

#####################
# Load packages, inputs
#####################

require(dplyr); require(here)
require(tidyr); 
require(reshape2); 
require(ggplot2)

# Set up comparisons of interest
  # Comparing 2019 Operations NAA, CalSim 3
n.alt <- 2 # No. of Scenarios to compare
alt_plot_names <- c("Original", "Ex CalSim 3")
alt_file_names <- c("originalDSM",
                    "CS3_L2020_DV_2021_ext_2040MED")
out.name <- "OGvsCalSim3"

# Load DSM outputs for plotting
if(model_type=="Det"){
  alt1 <- readRDS(here("winterRunDSM-Main", "Output", 
                       paste0("WR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[1],"_v2.rds")))
  alt2 <- readRDS(here("winterRunDSM-Main", "Output", 
                       paste0("WR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[2],"_v2.rds")))
  #alt3   <- readRDS(here("winterRunDSM-Main", "Output", 
  #                     paste0("WR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[3],"_v2.rds")))
} else if(model_type=="Stoch"){
  alt1 <- readRDS(here("winterRunDSM-Main", "Output", 
                       paste0("WR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[1],"_Iter",n.iter,"_v2.rds")))
  alt2 <- readRDS(here("winterRunDSM-Main", "Output", 
                       paste0("WR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[2],"_Iter",n.iter,"_v2.rds")))
  #alt3   <- readRDS(here("winterRunDSM-Main", "Output", 
  #                     paste0("WR_Recal_",model_type,"_Scen0_Alt_",alt_file_names[3],"Iter",n.iter,"_v2.rds")))
}

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
  
  adult_df_1 <- alt1$out.adult_en_route %>%
    reshape2::melt(varnames=c("Year","Month","Watershed")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Month=factor(Month, levels=c(1:4)), Scenario=alt_plot_names[1])
  
  adult_df_2 <- alt2$out.adult_en_route %>%
    reshape2::melt(varnames=c("Year","Month","Watershed")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Month=factor(Month, levels=c(1:4)), Scenario=alt_plot_names[2])
  
  #adult_df_3 <- alt3$out.adult_en_route %>%
    #reshape2::melt(varnames=c("Year","Month","Watershed")) %>%
    #filter(Watershed=="Upper Sacramento River") %>%
    #mutate(Month=factor(Month, levels=c(1:4)), Scenario=alt_plot_names[3])
  
  adult_df <- rbind(adult_df_1, adult_df_2)
  #adult_df <- rbind(adult_df_1, adult_df_2, adult_df_3)
  
  adult_en_route <- ggplot(adult_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Adult En Route Survival")
  
  rm(adult_df_1, adult_df_2, adult_df)
  #rm(adult_df_1, adult_df_2, adult_df_3, adult_df)
  
  # Summarize adult pre-spawn survival the Upper Sacramento River
  
  adult_df_1 <- alt1$out.pre.spawn.S %>%
    reshape2::melt(varnames=c("Year","Season","Watershed")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[1])
  
  adult_df_2 <- alt2$out.pre.spawn.S %>%
    reshape2::melt(varnames=c("Year","Season","Watershed")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[2])
  
  #adult_df_3 <- alt3$out.pre.spawn.S %>%
  #  reshape2::melt(varnames=c("Year","Season","Watershed")) %>%
  #  filter(Watershed=="Upper Sacramento River") %>%
  #  mutate(Scenario=alt_plot_names[3])
  
  adult_df <- rbind(adult_df_1, adult_df_2)
  #adult_df <- rbind(adult_df_1, adult_df_2, adult_df_3)
  
  pre.spawn.S <- ggplot(adult_df, aes(x=Season, y=value, fill=Scenario))+
    geom_boxplot()+
    labs(x="Season", y="Adult Pre-Spawn Survival")
  
  rm(adult_df_1, adult_df_2, adult_df)
  #rm(adult_df_1, adult_df_2, adult_df_3, adult_df)
  
  # Summarize in-channel rearing survival in the Upper Sacramento River (< 42 mm)
  
  rear_df_1 <- alt1$out.river_surv %>%
    reshape2::melt(varnames=c("Year","Month","Watershed","Size")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  rear_df_2 <- alt2$out.river_surv %>%
    reshape2::melt(varnames=c("Year","Month","Watershed","Size")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #rear_df_3 <- alt3$out.river_surv %>%
  #  reshape2::melt(varnames=c("Year","Month","Watershed","Size")) %>%
  #  filter(Watershed=="Upper Sacramento River") %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  rear_df <- rbind(rear_df_1, rear_df_2)
  #rear_df <- rbind(rear_df_1, rear_df_2, rear_df_3)
  rear_small_df <- rear_df %>% filter(Size=="s")
  
  river_surv <- ggplot(rear_small_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Rearing Survival Upper Sacramento (<42 mm)")
  
  rm(rear_df_1, rear_df_2, rear_df, rear_small_df)
  #rm(rear_df_1, rear_df_2, rear_df_3, rear_df, rear_small_df)
  
  # Summarize North Delta rearing survival (< 42 mm)
  
  rear_df_1 <- alt1$out.D_juv_surv %>%
    reshape2::melt(varnames=c("Year","Month","Delta", "Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  rear_df_2 <- alt2$out.D_juv_surv %>%
    reshape2::melt(varnames=c("Year","Month","Delta", "Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #rear_df_3 <- alt3$out.D_juv_surv %>%
  #  reshape2::melt(varnames=c("Year","Month","Delta", "Size")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  rear_df <- rbind(rear_df_1, rear_df_2)
  #rear_df <- rbind(rear_df_1, rear_df_2, rear_df_3)
  rear_N_small_df <- rear_df %>%
    filter(Delta=="N", Size=="s")
  
  D_juv_surv <- ggplot(rear_N_small_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="North Delta Rearing Survival (<42 mm)")
  
  rm(rear_df_1, rear_df_2, rear_df, rear_N_small_df)
  #rm(rear_df_1, rear_df_2, rear_df_3, rear_df, rear_N_small_df)
  
  # Proportion migrating as a function of pulse flows (<42 mm)
  
  pulse_df_1 <- alt1$out.p.pulse.leave %>%
    reshape2::melt(varnames=c("Year","Month","Watershed", "Size")) %>%
    filter(Watershed=="Upper Sacramento River") %>%  
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  pulse_df_2 <- alt2$out.p.pulse.leave %>%
    reshape2::melt(varnames=c("Year","Month","Watershed", "Size")) %>%
    filter(Watershed=="Upper Sacramento River") %>%  
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #pulse_df_3 <- alt3$out.p.pulse.leave %>%
  #  reshape2::melt(varnames=c("Year","Month","Watershed", "Size")) %>%
  #  filter(Watershed=="Upper Sacramento River") %>%  
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])

  pulse_df <- rbind(pulse_df_1, pulse_df_2)
  #pulse_df <- rbind(pulse_df_1, pulse_df_2, pulse_df_3)
  pulse_small_df <- pulse_df %>% filter(Size=="s")
  
  p.pulse.leave <- ggplot(pulse_small_df, aes(x=Month, y=value, col=Scenario))+
    geom_point() +
    labs(x="Month", y="Upper Sac. River Migration from Pulse Flows (<42 mm)")
  
  rm(pulse_df_1, pulse_df_2, pulse_df, pulse_small_df)
  #rm(pulse_df_1, pulse_df_2, pulse_df_3, pulse_df, pulse_small_df)
  
  # Migratory survival through the Upper-Mid Sacramento River (>110 mm)
  
  migr_df_1 <- alt1$out.UM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  migr_df_2 <- alt2$out.UM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #migr_df_3 <- alt3$out.UM.Sac.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Size")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_small_df <- migr_df %>% filter(Size=="vl")
  
  UM.Sac.S <- ggplot(migr_small_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Upper-mid Sacramento Migratory Survival (>110 mm)")
  
  rm(migr_df_1, migr_df_2, migr_df, migr_small_df)
  #rm(migr_df_1, migr_df_2, migr_df_3, migr_df, migr_small_df)
  
  # Summarize proportion of fish entrained into the South Delta
  
  entrain_df_1 <- alt1$out.prop.dlt.entrain %>%
    reshape2::melt(varnames=c("Year","Month")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  entrain_df_2 <- alt2$out.prop.dlt.entrain %>%
    reshape2::melt(varnames=c("Year","Month")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #entrain_df_3 <- alt3$out.prop.dlt.entrain %>%
  #  reshape2::melt(varnames=c("Year","Month")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  entrain_df <- rbind(entrain_df_1, entrain_df_2)
  #entrain_df <- rbind(entrain_df_1, entrain_df_2, entrain_df_3)
  
  prop.dlt.entrain <- ggplot(entrain_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Proportional Entrainment to South Delta")
  
  rm(entrain_df_1, entrain_df_2, entrain_df)
  #rm(entrain_df_1, entrain_df_2, entrain_df_3, entrain_df)
  
  # Migratory survival through the North Delta (>110 mm)
  
  migr_df_1 <- alt1$out.Sac.Delt.S %>%
    reshape2::melt(varnames=c("Year","Month","Delta","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  migr_df_2 <- alt2$out.Sac.Delt.S %>%
    reshape2::melt(varnames=c("Year","Month","Delta","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #migr_df_3 <- alt3$out.Sac.Delt.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Delta","Size")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_N_df <- migr_df %>% filter(Delta=="N", Size=="vl")
  
  Sac.Delt.S <- ggplot(migr_N_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Migratory Survival through North Delta (>110 mm)")

  rm(migr_df_1, migr_df_2, migr_df, migr_N_df)
  #rm(migr_df_1, migr_df_2, migr_df_3, migr_df, migr_N_df)
  
  # Migratory survival through South Delta for North origin fish (>110 mm)
  
  migr_df_1 <- alt1$out.newDsurv %>%
    reshape2::melt(varnames=c("Year","Month","Origin","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  migr_df_2 <- alt2$out.newDsurv %>%
    reshape2::melt(varnames=c("Year","Month","Origin","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #migr_df_3 <- alt3$out.newDsurv %>%
  #  reshape2::melt(varnames=c("Year","Month","Origin","Size")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_N_df <- migr_df %>% filter(Origin=="N", Size=="vl")
  
  newDsurv <- ggplot(migr_N_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Migratory Survival through South Delta (>110 mm)")
  
  rm(migr_df, migr_N_df, migr_df_1, migr_df_2)
  #rm(migr_df, migr_N_df, migr_df_1, migr_df_2, migr_df_3)
  
  # Upper-mid Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df_1 <- alt1$out.UM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df_2 <- alt2$out.UM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  #migr_df_3 <- alt3$out.UM.Sac.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Size")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3],
  #         WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  UM.Sac.S.facet <- ggplot(migr_small_df, aes(x=WYT, y=value, fill=Scenario))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="Month", y="Upper-mid Sacramento Migratory Survival (>110 mm)")
  
  rm(migr_df_1, migr_df_2, migr_df, migr_small_df)

  # Lower-mid Migratory Survival, grouped by WYT, faceted by month (>110 mm)

  migr_df_1 <- alt1$out.LM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df_2 <- alt2$out.LM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  #migr_df_3 <- alt3$out.LM.Sac.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Size")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3],
  #         WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  LM.Sac.S.facet <- ggplot(migr_small_df, aes(x=WYT, y=value, fill=Scenario))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="Month", y="Lower-mid Sacramento Migratory Survival (>110 mm)")
  
  rm(migr_df_1, migr_df_2, migr_df, migr_small_df)
  
  # Lower Migratory Survival, grouped by WYT, faceted by month (>110 mm)

  migr_df_1 <- alt1$out.LL.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df_2 <- alt2$out.LL.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  #migr_df_3 <- alt3$out.LL.Sac.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Size")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3],
  #         WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  LL.Sac.S.facet <- ggplot(migr_small_df, aes(x=WYT, y=value, fill=Scenario))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="Month", y="Lower Sacramento Migratory Survival (>110 mm)")
  
  rm(migr_df_1, migr_df_2, migr_df, migr_small_df)
  
  # N. Delta Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df_1 <- alt1$out.Sac.Delt.S %>%
    reshape2::melt(varnames=c("Year","Month","Delta","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df_2 <- alt2$out.Sac.Delt.S %>%
    reshape2::melt(varnames=c("Year","Month","Delta","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  #migr_df_3 <- alt3$out.Sac.Delt.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Delta","Size")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3],
  #         WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_N_df <- migr_df %>% filter(Delta=="N", Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  Sac.Delt.S.facet <- ggplot(migr_N_df, aes(x=WYT, y=value, fill=Scenario))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="Month", y="Migratory Survival through North Delta (>110 mm)")
  
  rm(migr_df_1, migr_df_2, migr_df, migr_N_df)

  # S. Delta Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df_1 <- alt1$out.newDsurv %>%
    reshape2::melt(varnames=c("Year","Month","Origin","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df_2 <- alt2$out.newDsurv %>%
    reshape2::melt(varnames=c("Year","Month","Origin","Size")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  #migr_df_3 <- alt3$out.newDsurv %>%
  #  reshape2::melt(varnames=c("Year","Month","Origin","Size")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3],
  #         WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_N_df <- migr_df %>% filter(Origin=="N", Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  newDsurv.facet <- ggplot(migr_N_df, aes(x=WYT, y=value, fill=Scenario))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="Month", y="Migratory Survival through South Delta (>110 mm)")
  
  rm(migr_df_1, migr_df_2, migr_df, migr_N_df)
  
}

  ############
  # Stochastic
  ############

if(model_type=="Stoch"){
  
  # Summarize adult en route survival to the Upper Sacramento River
  
  adult_df_1 <- alt1$out.adult_en_route %>%
    reshape2::melt(varnames=c("Year","Month","Watershed","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Month=factor(Month, levels=c(1:4)), Scenario=alt_plot_names[1])
  
  adult_df_2 <- alt2$out.adult_en_route %>%
    reshape2::melt(varnames=c("Year","Month","Watershed","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Month=factor(Month, levels=c(1:4)), Scenario=alt_plot_names[2])
  
  #adult_df_3 <- alt3$out.adult_en_route %>%
  #  reshape2::melt(varnames=c("Year","Month","Watershed","Iteration")) %>%
  #  filter(Watershed=="Upper Sacramento River") %>%
  #  mutate(Month=factor(Month, levels=c(1:4)), Scenario=alt_plot_names[3])
  
  adult_df <- rbind(adult_df_1, adult_df_2)
  #adult_df <- rbind(adult_df_1, adult_df_2, adult_df_3)
  
  adult_en_route <- ggplot(adult_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Adult En Route Survival")
  
  rm(adult_df, adult_df_1, adult_df_2)
  #rm(adult_df, adult_df_1, adult_df_2, adult_df_3)
  
  # Summarize adult pre-spawn survival the Upper Sacramento River
  
  adult_df_1 <- alt1$out.pre.spawn.S %>%
    reshape2::melt(varnames=c("Year","Season","Watershed","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[1])
  
  adult_df_2 <- alt2$out.pre.spawn.S %>%
    reshape2::melt(varnames=c("Year","Season","Watershed","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[2])
  
  #adult_df_3 <- alt3$out.pre.spawn.S %>%
  #  reshape2::melt(varnames=c("Year","Season","Watershed","Iteration")) %>%
  #  filter(Watershed=="Upper Sacramento River") %>%
  #  mutate(Scenario=alt_plot_names[3])
  
  adult_df <- rbind(adult_df_1, adult_df_2)
  #adult_df <- rbind(adult_df_1, adult_df_2, adult_df_3)
  
  pre.spawn.S <- ggplot(adult_df, aes(x=Season, y=value, fill=Scenario))+
    geom_boxplot()+
    labs(x="Season", y="Adult Pre-Spawn Survival")
  
  rm(adult_df, adult_df_1, adult_df_2)
  #rm(adult_df, adult_df_1, adult_df_2, adult_df_3)
  
  # Summarize in-channel rearing survival in the Upper Sacramento River (< 42 mm)
  
  rear_df_1 <- alt1$out.river_surv %>%
    reshape2::melt(varnames=c("Year","Month","Watershed","Size","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  rear_df_2 <- alt2$out.river_surv %>%
    reshape2::melt(varnames=c("Year","Month","Watershed","Size","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #rear_df_3 <- alt3$out.river_surv %>%
  #  reshape2::melt(varnames=c("Year","Month","Watershed","Size","Iteration")) %>%
  #  filter(Watershed=="Upper Sacramento River") %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  rear_df <- rbind(rear_df_1, rear_df_2)
  #rear_df <- rbind(rear_df_1, rear_df_2, rear_df_3)
  rear_small_df <- rear_df %>% filter(Size=="s")
  
  river_surv <- ggplot(rear_small_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Rearing Survival Upper Sacramento (<42 mm)")
  
  rm(rear_df, rear_small_df, rear_df_1, rear_df_2)
  #rm(rear_df, rear_small_df, 
  #   rear_df_1, rear_df_2, rear_df_3)
  
  # Summarize North Delta rearing survival (< 42 mm)
  
  rear_df_1 <- alt1$out.D_juv_surv %>%
    reshape2::melt(varnames=c("Year","Month","Delta", "Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  rear_df_2 <- alt2$out.D_juv_surv %>%
    reshape2::melt(varnames=c("Year","Month","Delta", "Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #rear_df_3 <- alt3$out.D_juv_surv %>%
  #  reshape2::melt(varnames=c("Year","Month","Delta", "Size","Iteration")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  rear_df <- rbind(rear_df_1, rear_df_2)
  #rear_df <- rbind(rear_df_1, rear_df_2, rear_df_3)
  rear_N_small_df <- rear_df %>%
    filter(Delta=="N", Size=="s")
  
  D_juv_surv <- ggplot(rear_N_small_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="North Delta Rearing Survival (<42 mm)")
  
  rm(rear_df, rear_N_small_df, rear_df_1, rear_df_2)
  #rm(rear_df, rear_N_small_df, rear_df_1, rear_df_2, rear_df_3)
  
  # Proportion migrating in the Upper Sacramento River as a function of pulse flows (<42 mm)
  
  pulse_df_1 <- alt1$out.p.pulse.leave %>%
    reshape2::melt(varnames=c("Year","Month","Watershed", "Size","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%  
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  pulse_df_2 <- alt2$out.p.pulse.leave %>%
    reshape2::melt(varnames=c("Year","Month","Watershed", "Size","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #pulse_df_3 <- alt3$out.p.pulse.leave %>%
  #  reshape2::melt(varnames=c("Year","Month","Watershed", "Size","Iteration")) %>%
  #  filter(Watershed=="Upper Sacramento River") %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  pulse_df <- rbind(pulse_df_1, pulse_df_2)
  #pulse_df <- rbind(pulse_df_1, pulse_df_2, pulse_df_3)
  pulse_small_df <- pulse_df %>% filter(Size=="s")
  
  p.pulse.leave <- ggplot(pulse_small_df, aes(x=Month, y=value, col=Scenario))+
    geom_point() +
    labs(x="Month", y="Upper Sac. River Migration from Pulse Flows (<42 mm)")
  
  rm(pulse_df, pulse_small_df, pulse_df_1, pulse_df_2)
  #rm(pulse_df, pulse_small_df, pulse_df_1, pulse_df_2, pulse_df_3)
  
  # Migratory survival through the Upper-Mid Sacramento River (>110 mm)
  
  migr_df_1 <- alt1$out.UM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  migr_df_2 <- alt2$out.UM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #migr_df_3 <- alt3$out.UM.Sac.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_small_df <- migr_df %>% filter(Size=="vl")
  
  UM.Sac.S <- ggplot(migr_small_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Upper-mid Sacramento Migratory Survival (>110 mm)")
  
  rm(migr_df, migr_small_df, migr_df_1, migr_df_2)
  #rm(migr_df, migr_small_df, migr_df_1, migr_df_2, migr_df_3)
  
  # Summarize proportion of fish entrained into the South Delta
  
  entrain_df_1 <- alt1$out.prop.dlt.entrain %>%
    reshape2::melt(varnames=c("Year","Month","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  entrain_df_2 <- alt2$out.prop.dlt.entrain %>%
    reshape2::melt(varnames=c("Year","Month","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #entrain_df_3 <- alt3$out.prop.dlt.entrain %>%
  #  reshape2::melt(varnames=c("Year","Month","Iteration")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  entrain_df <- rbind(entrain_df_1, entrain_df_2)
  #entrain_df <- rbind(entrain_df_1, entrain_df_2, entrain_df_3)
  
  prop.dlt.entrain <- ggplot(entrain_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Proportional Entrainment to South Delta")
  
  rm(entrain_df, entrain_df_1, entrain_df_2)
  #rm(entrain_df, entrain_df_1, entrain_df_2, entrain_df_3)
  
  # Migratory survival through the North Delta (>110 mm)
  
  migr_df_1 <- alt1$out.Sac.Delt.S %>%
    reshape2::melt(varnames=c("Year","Month","Delta","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  migr_df_2 <- alt2$out.Sac.Delt.S %>%
    reshape2::melt(varnames=c("Year","Month","Delta","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #migr_df_3 <- alt3$out.Sac.Delt.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Delta","Size","Iteration")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_N_df <- migr_df %>% filter(Delta=="N", Size=="vl")
  
  Sac.Delt.S <- ggplot(migr_N_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Migratory Survival through North Delta (>110 mm)")
  
  rm(migr_df, migr_N_df, migr_df_1, migr_df_2)
  #rm(migr_df, migr_N_df, migr_df_1, migr_df_2, migr_df_3)
  
  # Migratory survival through South Delta for North origin fish (>110 mm)
  
  migr_df_1 <- alt1$out.newDsurv %>%
    reshape2::melt(varnames=c("Year","Month","Origin","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1])
  
  migr_df_2 <- alt2$out.newDsurv %>%
    reshape2::melt(varnames=c("Year","Month","Origin","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2])
  
  #migr_df_3 <- alt3$out.newDsurv %>%
  #  reshape2::melt(varnames=c("Year","Month","Origin","Size","Iteration")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3])
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_N_df <- migr_df %>% filter(Origin=="N", Size=="vl")
  
  newDsurv <- ggplot(migr_N_df, aes(x=Month, y=value, fill=Scenario))+
    geom_boxplot() +
    labs(x="Month", y="Migratory Survival through South Delta (>110 mm)")
  
  rm(migr_df, migr_N_df, migr_df_1, migr_df_2)
  #rm(migr_df, migr_N_df, migr_df_1, migr_df_2, migr_df_3)
  
  # Upper-mid Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df_1 <- alt1$out.UM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df_2 <- alt2$out.UM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  #migr_df_3 <- alt3$out.UM.Sac.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3],
  #         WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  UM.Sac.S.facet <- ggplot(migr_small_df, aes(x=WYT, y=value, fill=Scenario))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="Month", y="Upper-mid Sacramento Migratory Survival (>110 mm)")
  
  rm(migr_df_1, migr_df_2, migr_df, migr_small_df)
  
  # Lower-mid Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df_1 <- alt1$out.LM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df_2 <- alt2$out.LM.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  #migr_df_3 <- alt3$out.LM.Sac.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3],
  #         WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  LM.Sac.S.facet <- ggplot(migr_small_df, aes(x=WYT, y=value, fill=Scenario))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="Month", y="Lower-mid Sacramento Migratory Survival (>110 mm)")
  
  rm(migr_df_1, migr_df_2, migr_df, migr_small_df)
  
  # Lower Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df_1 <- alt1$out.LL.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df_2 <- alt2$out.LL.Sac.S %>%
    reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  #migr_df_3 <- alt3$out.LL.Sac.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Size","Iteration")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3],
  #         WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_small_df <- migr_df %>% filter(Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  LL.Sac.S.facet <- ggplot(migr_small_df, aes(x=WYT, y=value, fill=Scenario))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="Month", y="Lower Sacramento Migratory Survival (>110 mm)")
  
  rm(migr_df_1, migr_df_2, migr_df, migr_small_df)
  
  # N. Delta Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df_1 <- alt1$out.Sac.Delt.S %>%
    reshape2::melt(varnames=c("Year","Month","Delta","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df_2 <- alt2$out.Sac.Delt.S %>%
    reshape2::melt(varnames=c("Year","Month","Delta","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  #migr_df_3 <- alt3$out.Sac.Delt.S %>%
  #  reshape2::melt(varnames=c("Year","Month","Delta","Size","Iteration")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3],
  #         WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_N_df <- migr_df %>% filter(Delta=="N", Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  Sac.Delt.S.facet <- ggplot(migr_N_df, aes(x=WYT, y=value, fill=Scenario))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="Month", y="Migratory Survival through North Delta (>110 mm)")
  
  rm(migr_df_1, migr_df_2, migr_df, migr_N_df)
  
  # S. Delta Migratory Survival, grouped by WYT, faceted by month (>110 mm)
  
  migr_df_1 <- alt1$out.newDsurv %>%
    reshape2::melt(varnames=c("Year","Month","Origin","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[1],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df_2 <- alt2$out.newDsurv %>%
    reshape2::melt(varnames=c("Year","Month","Origin","Size","Iteration")) %>%
    mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[2],
           WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  #migr_df_3 <- alt3$out.newDsurv %>%
  #  reshape2::melt(varnames=c("Year","Month","Origin","Size","Iteration")) %>%
  #  mutate(Month=factor(Month, levels=c(9:12,1:5)), Scenario=alt_plot_names[3],
  #         WY=ifelse(Month%in%c(10:12,1:5),Year+1,Year))
  
  migr_df <- rbind(migr_df_1, migr_df_2)
  #migr_df <- rbind(migr_df_1, migr_df_2, migr_df_3)
  migr_N_df <- migr_df %>% filter(Origin=="N", Size=="vl") %>%
    left_join(.,wyt_ref) %>%
    mutate(WYT=factor(WYT, levels=c("C","D","BN","AN","W")))
  
  newDsurv.facet <- ggplot(migr_N_df, aes(x=WYT, y=value, fill=Scenario))+
    geom_boxplot() +
    facet_wrap(~Month) +
    labs(x="Month", y="Migratory Survival through South Delta (>110 mm)")
  
  rm(migr_df_1, migr_df_2, migr_df, migr_N_df)
  
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

  for(i in 2:20){
    lambda[1,i-1] <- alt1$out.all_spawners[1,i]/alt1$out.all_spawners[1,i-1]
    lambda[2,i-1] <- alt2$out.all_spawners[1,i]/alt2$out.all_spawners[1,i-1]
    #lambda[3,i-1] <- alt3$out.all_spawners[1,i]/alt3$out.all_spawners[1,i-1]
  }
  lambda_mean_1 <- exp(mean(log(lambda[1,])))
  lambda_mean_2 <- exp(mean(log(lambda[2,])))
  #lambda_mean_3 <- exp(mean(log(lambda[3,])))
  
  end_lambda_1 <- alt1$out.all_spawners[1,20]/alt1$out.all_spawners[1,1]
  end_lambda_2 <- alt2$out.all_spawners[1,20]/alt2$out.all_spawners[1,1]
  #end_lambda_3 <- alt3$out.all_spawners[1,20]/alt3$out.all_spawners[1,1]
  
  lambda_plot <- data.frame(Year=rep(1981:1999,2),
                            Scenario=c(rep(alt_plot_names[1],19),
                                       rep(alt_plot_names[2],19)),
                                       #rep(alt_plot_names[3],19)),
                                 Lambda=c(lambda[1,],lambda[2,]))#,lambda[3,]))
  
  lambda_diff_plot <- data.frame(Year=rep(1981:1999,2),
                                 Scenario=c(rep(alt_plot_names[2],19)),#rep(alt_plot_names[3],19)),
                            Lambda_diff=c((lambda[2,]-lambda[1,])/lambda[1,]*100))#,
                                          #(lambda[3,]-lambda[1,])/lambda[1,]*100))
}
lambda_mean_1; lambda_mean_2; #lambda_mean_3
end_lambda_1; end_lambda_2; #end_lambda_3


if(model_type=="Stoch"){
  
  lambda <- array(NA, dim=c(n.alt,19, alt1$n.iter), 
                  dimnames=list(alt_plot_names,1981:1999,1:alt1$n.iter)); 
  lambda_mean_1 <- numeric(alt1$n.iter)
  lambda_mean_2 <- numeric(alt1$n.iter)
  #lambda_mean_3 <- numeric(alt1$n.iter)
  
  end_lambda_1 <- numeric(alt1$n.iter)
  end_lambda_2 <- numeric(alt1$n.iter)
  #end_lambda_3 <- numeric(alt1$n.iter)
  
  for(j in 1:alt1$n.iter){
    for(i in 2:20){
      lambda[1,i-1,j] <- alt1$out.all_spawners[1,i,j]/alt1$out.all_spawners[1,i-1,j]
      lambda[2,i-1,j] <- alt2$out.all_spawners[1,i,j]/alt2$out.all_spawners[1,i-1,j]
      #lambda[3,i-1,j] <- alt3$out.all_spawners[1,i,j]/alt3$out.all_spawners[1,i-1,j]
    }
    lambda_mean_1[j] <- exp(mean(log(lambda[1,,j])))
    lambda_mean_2[j] <- exp(mean(log(lambda[2,,j])))
    #lambda_mean_3[j] <- exp(mean(log(lambda[3,,j])))
    
    end_lambda_1[j] <- alt1$out.all_spawners[1,20,j]/alt1$out.all_spawners[1,1,j]
    end_lambda_2[j] <- alt2$out.all_spawners[1,20,j]/alt2$out.all_spawners[1,1,j]
    #end_lambda_3[j] <- alt3$out.all_spawners[1,20,j]/alt3$out.all_spawners[1,1,j]
  }
  
  # Create data frame of all lambdas for plotting
  lambda_df_raw <- lambda %>%
    reshape2::melt(varnames=c("Scenario","Year","Iteration")) %>%
    mutate("Lambda"=value)
  
  # Create data frame for boxplot summary of lambda by Scenario
  lambda_df_1 <- data.frame(Lambda_mean=lambda_mean_1, 
                               Lambda_end=end_lambda_1, Scenario=alt_plot_names[1])
  lambda_df_2 <- data.frame(Lambda_mean=lambda_mean_2,
                               Lambda_end=end_lambda_2, Scenario=alt_plot_names[2])
  #lambda_df_3 <- data.frame(Lambda_mean=lambda_mean_3,
  #                           Lambda_end=end_lambda_3, Scenario=alt_plot_names[3])
  lambda_df <- rbind(lambda_df_1, lambda_df_2)
  #lambda_df <- rbind(lambda_df_1, lambda_df_2, lambda_df_3)
}
summary(lambda_mean_1); summary(lambda_mean_2); #summary(lambda_mean_3);
summary(end_lambda_1); summary(end_lambda_2); #summary(end_lambda_3);



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
  
  dimnames(alt1$out.all_spawners) <- list(watershed.names, year.names)
  dimnames(alt2$out.all_spawners) <- list(watershed.names, year.names)
  #dimnames(alt3$out.all_spawners) <- list(watershed.names, year.names)
  
  # All spawners
  
  spawn_df_1 <- alt1$out.all_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[1], `All Spawners`=value)
  
  spawn_df_2 <- alt2$out.all_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[2], `All Spawners`=value)
  
  #spawn_df_3 <- alt3$out.all_spawners %>%
  #  reshape2::melt(varnames=c("Watershed","Year")) %>%
  #  filter(Watershed=="Upper Sacramento River") %>%
  #  mutate(Scenario=alt_plot_names[3], `All Spawners`=value)
  
  spawn_df <- rbind(spawn_df_1, spawn_df_2)
  #spawn_df <- rbind(spawn_df_1, spawn_df_2, spawn_df_3)
  
  all_spawners <- ggplot(spawn_df, aes(x=Year, y=`All Spawners`, col=Scenario))+
    geom_line()
  
  # Nat spawners
  
  dimnames(alt1$out.nat_spawners) <- list(watershed.names, year.names)
  dimnames(alt2$out.nat_spawners) <- list(watershed.names, year.names)
  #dimnames(alt3$out.nat_spawners) <- list(watershed.names, year.names)
  
  spawn_df_1 <- alt1$out.nat_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[1], `Natural Spawners`=value)
  
  spawn_df_2 <- alt2$out.nat_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[2], `Natural Spawners`=value)
  
  #spawn_df_3 <- alt3$out.nat_spawners %>%
  #  reshape2::melt(varnames=c("Watershed","Year")) %>%
  #  filter(Watershed=="Upper Sacramento River") %>%
  #  mutate(Scenario=alt_plot_names[3], `Natural Spawners`=value)
  
  spawn_df <- rbind(spawn_df_1, spawn_df_2)
  #spawn_df <- rbind(spawn_df_1, spawn_df_2, spawn_df_3)
  
  nat_spawners <- ggplot(spawn_df, aes(x=Year, y=`Natural Spawners`, col=Scenario))+
    geom_line()

  # Natural spawners, relative to some baseline
    # Here, use alt1 as baseline for time being

  spawn_df_1 <- alt1$out.nat_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[1], `Natural Spawners`=value) %>%
    group_by(Year) %>%
    summarize(Mean=mean(`Natural Spawners`), 
              Lo_95=quantile(`Natural Spawners`,0.025),
              Hi_95=quantile(`Natural Spawners`,0.975))
  
  spawn_df_2 <- alt2$out.nat_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[2], `Natural Spawners`=value,) %>%
    group_by(Year) %>%
    summarize(Mean=mean(`Natural Spawners`), 
              Lo_95=quantile(`Natural Spawners`,0.025),
              Hi_95=quantile(`Natural Spawners`,0.975)) %>%
    mutate(`% Difference (Mean)`=(.$Mean-spawn_df_1$Mean)/spawn_df_1$Mean*100,
           `% Difference (2.5% Quant)`=(.$Lo_95-spawn_df_1$Lo_95)/spawn_df_1$Lo_95*100,
           `% Difference (97.5% Quant)`=(.$Hi_95-spawn_df_1$Hi_95)/spawn_df_1$Hi_95*100,
           Scenario=alt_plot_names[2])
  '/
  spawn_df_3 <- alt3$out.nat_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[3], `Natural Spawners`=value,) %>%
    group_by(Year) %>%
    summarize(Mean=mean(`Natural Spawners`), 
              Lo_95=quantile(`Natural Spawners`,0.025),
              Hi_95=quantile(`Natural Spawners`,0.975)) %>%
    mutate(`% Difference (Mean)`=(.$Mean-spawn_df_1$Mean)/spawn_df_1$Mean*100,
           `% Difference (2.5% Quant)`=(.$Lo_95-spawn_df_1$Lo_95)/spawn_df_1$Lo_95*100,
           `% Difference (97.5% Quant)`=(.$Hi_95-spawn_df_1$Hi_95)/spawn_df_1$Hi_95*100,
           Scenario=alt_plot_names[3])
  '
  spawn_df <- rbind(spawn_df_2)
  
  nat_spawners_diff <- ggplot(spawn_df, aes(x=Year, y=`% Difference (Mean)`, col=Scenario))+
    geom_line()+
    labs(y="% Difference in Abundance Relative to originalDSM") # This will be NAA in final version
  
  # Lambda plot
  
  lambda_all <- ggplot(lambda_plot, aes(x=Year, y=Lambda, col=Scenario))+
    geom_line()+
    labs(y="Lambda")
  
  # % Difference in Lambda Plot
  
  lambda_all_diff <- ggplot(lambda_diff_plot, aes(x=Year, y=Lambda_diff, col=Scenario))+
    geom_line()+
    labs(y="% Difference in Lambda Relative to originalDSM") # This will be NAA in final version
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
  iter.names <- 1:alt1$n.iter
  
  dimnames(alt1$out.all_spawners) <- list(watershed.names, year.names, iter.names)
  dimnames(alt2$out.all_spawners) <- list(watershed.names, year.names, iter.names)
  #dimnames(alt3$out.all_spawners) <- list(watershed.names, year.names, iter.names)
  
  dimnames(alt1$out.nat_spawners) <- list(watershed.names, year.names, iter.names)
  dimnames(alt2$out.nat_spawners) <- list(watershed.names, year.names, iter.names)
  #dimnames(alt3$out.nat_spawners) <- list(watershed.names, year.names, iter.names)
  
  # All spawners
  
  spawn_df_1 <- alt1$out.all_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[1], `All Spawners`=value)
  
  spawn_df_2 <- alt2$out.all_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[2], `All Spawners`=value)
  
  #spawn_df_3 <- alt3$out.all_spawners %>%
  #  reshape2::melt(varnames=c("Watershed","Year","Iteration")) %>%
  #  filter(Watershed=="Upper Sacramento River") %>%
  #  mutate(Scenario=alt_plot_names[3], `All Spawners`=value)
  
  spawn_df <- rbind(spawn_df_1, spawn_df_2)
  #spawn_df <- rbind(spawn_df_1, spawn_df_2, spawn_df_3)
  
  all_spawners <- ggplot(spawn_df, aes(x=Year, y=`All Spawners`))+
    geom_line(col="black", aes(group=Iteration))+
    geom_smooth(spawn_df, mapping=aes(x=Year, y=`All Spawners`), method="gam")+  
    facet_wrap(~Scenario)
  
  # Natural spawners
  
  spawn_df_1 <- alt1$out.nat_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[1], `Natural Spawners`=value)
  
  spawn_df_2 <- alt2$out.nat_spawners %>%
    reshape2::melt(varnames=c("Watershed","Year","Iteration")) %>%
    filter(Watershed=="Upper Sacramento River") %>%
    mutate(Scenario=alt_plot_names[2], `Natural Spawners`=value)
  
  #spawn_df_3 <- alt3$out.nat_spawners %>%
  #  reshape2::melt(varnames=c("Watershed","Year","Iteration")) %>%
  #  filter(Watershed=="Upper Sacramento River") %>%
  #  mutate(Scenario=alt_plot_names[3], `Natural Spawners`=value)
  
  spawn_df <- rbind(spawn_df_1, spawn_df_2)
  #spawn_df <- rbind(spawn_df_1, spawn_df_2, spawn_df_3)
  
  nat_spawners <- ggplot(spawn_df, aes(x=Year, y=`Natural Spawners`))+
    geom_line(col="black", aes(group=Iteration))+
    geom_smooth(spawn_df, mapping=aes(x=Year, y=`Natural Spawners`), method="gam")+ # how do 
    facet_wrap(~Scenario)
  
  # Iteration-specific lambda values (boxplots by Scenario)
  
  lambda_mean <- ggplot(lambda_df, aes(x=Scenario, y=Lambda_mean))+
    geom_boxplot()+
    labs(y="Mean Lambda", x="Scenario")
  
  lambda_end <- ggplot(lambda_df, aes(x=Scenario, y=Lambda_end))+
    geom_boxplot()+
    labs(y="Lambda Based on Last and First Year", x="Scenario")
  
  # Iteration-specific lambda values (time series)
  
  lambda_all <- ggplot(lambda_df_raw, aes(x=Year, y=Lambda))+
    geom_line(aes(group=Iteration))+
    geom_smooth(lambda_df_raw, mapping=aes(x=Year, y=Lambda), method="gam")+
    geom_hline(yintercept=1)+
    facet_wrap(~Scenario)
  
}

################
# Generate, save all plots
################

# Population parameters (abundance, growth rate)
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_All Spawners.png"),
       all_spawners, width=6, height=4, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Natural Spawners.png"),
       nat_spawners, width=6, height=4, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Lambda Timeseries.png"),
       lambda_all, width=6, height=4, units=c("in"))

if(model_type=="Det"){
  ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Natural Spawners Perc Diff.png"),
         nat_spawners_diff, width=6, height=4, units=c("in"))
  ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Lambda Timeseries Perc Diff.png"),
         lambda_all_diff, width=6, height=4, units=c("in"))
}
if(model_type=="Stoch"){
  ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Lambda Mean Boxplot.png"),
         lambda_mean, width=6, height=4, units=c("in"))
  ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Lambda End Boxplot.png"),
         lambda_end, width=6, height=4, units=c("in"))
}

# Life state transition parameters, not grouped by month + WYT
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Adult En Route.png"),
       adult_en_route, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Adult Prespawn.png"),
       pre.spawn.S, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Juv Upper Sac Rear S.png"),
       river_surv, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Juv N Delta Rear S.png"),
       D_juv_surv, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Prop Pulse Leave S.png"),
       p.pulse.leave, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Prop Pulse Leave S.png"),
       p.pulse.leave, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Juv UM Sac Migr VL.png"),
       UM.Sac.S, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_S Delta Entrainment.png"),
       prop.dlt.entrain, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Juv N Delta Migr VL.png"),
       Sac.Delt.S, width=6, height=5, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Juv S Delta Migr VL.png"),
       newDsurv, width=6, height=5, units=c("in"))

# Migratory survival parameters, vl size, grouped by month + WYT

ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Juv UM Sac Migr VL Facet.png"),
       UM.Sac.S.facet, width=7, height=9, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Juv LM Sac Migr VL Facet.png"),
       LM.Sac.S.facet, width=7, height=9, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Juv LL Sac Migr VL Facet.png"),
       LL.Sac.S.facet, width=7, height=9, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Juv N Delta Migr VL Facet.png"),
       Sac.Delt.S.facet, width=7, height=9, units=c("in"))
ggsave(paste0("./winterRunDSM-Main/Output/",out.name,"_",model_type,"_Juv S Delta Migr VL Facet.png"),
       newDsurv.facet, width=7, height=9, units=c("in"))
