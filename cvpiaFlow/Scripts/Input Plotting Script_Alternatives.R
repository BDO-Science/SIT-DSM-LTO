############################
# Annotation
  # This script creates comparison plots for original DSM inputs and NAA CalSim 3 inputs
    # It calls inputs specifically for winter-run, but it generally applies across run types
############################

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


# Load Model Inputs function
source(here::here("winterRunDSM-main","functions","Load Model Inputs.R"))
 
####################
# Import formatted flow data for different alternatives
####################

#add updated data inputs from the LTO Alternative of interest
alts <- c("originalDSM", "Reclamation_2021LTO_CalSim3_NAA_2022MED_09072023") 
comp <- "OG vs CalSim 3 NAA"

#create a list of the variables of interest
flow_vars <- c("upSacQ", "t.diver", "retQ", "prop.Q.bypasses",
               "prop.pulse", "p.diver", "gate.top", "freeportQ", "Dlt.inf", 
               "dlt.gates", "dlt.divers.tot", "dlt.divers", 
               "Q_free", "Q_vern", "Q_stck",  "SWP_exp") 
#temp_vars <- c("DegDay", "juv.tmp")
all_vars <- c(flow_vars)#, temp_vars)

data_inputs1 <- list()
data_inputs2 <- list()
#data_inputs3 <- list()

for(a in 1:length(alts)){ #loop over the alternatives to save the data from each
  load_model_inputs(alts[a])  
  #create a data list
  
  for(i in 1:length(all_vars)){
    
    if(a==1){
      data_inputs1[[i]] <- get(all_vars[i])
      names(data_inputs1)[i] <- all_vars[i]
    }
    if(a==2){
      data_inputs2[[i]] <- get(all_vars[i])
      names(data_inputs2)[i] <- all_vars[i]
    }
  }
}

#Plotting for comparison of original flow inputs to CalSim 3 NAA

library(ggplot2)
library(tidyr)
library(plyr); library(dplyr)


#############
# Plotting
#############

mo_convert <- data.frame(no=c(1:12),
                         nam=c("Jan","Feb","Mar","Apr","May","Jun",
                                      "Jul","Aug","Sep","Oct","Nov","Dec"))
shed_convert <- data.frame(no=c(1:31),
                           nam=c("Upper Sacramento River","Antelope Creek","Battle Creek","Bear Creek",                
                                 "Big Chico Creek","Butte Creek","Clear Creek","Cottonwood Creek",          
                                 "Cow Creek","Deer Creek","Elder Creek","Mill Creek",                
                                 "Paynes Creek","Stony Creek","Thomes Creek","Upper-mid Sacramento River",
                                 "Sutter Bypass","Bear River","Feather River","Yuba River",                
                                 "Lower-mid Sacramento River","Yolo Bypass","American River","Lower Sacramento River",    
                                 "Calaveras River","Cosumnes River","Mokelumne River","Merced River",              
                                 "Stanislaus River","Tuolumne River","San Joaquin River"))
 

  ###################
  # vars set up as 12x21 table
    # "SWP_exp", "Q_free", "Q_vern", "Q_stck"
  ###################

myr_vars <- c("SWP_exp", "Q_free", "Q_vern", "Q_stck") 

for(i in 1:length(myr_vars)){
  df1 <- data_inputs1[[myr_vars[i]]] %>% melt()
  df1$run <- alts[1]
  for(j in 1:length(df1$Var1)){
    df1$Var1[j] <- mo_convert[which(mo_convert$no==df1$Var1[j]),2]
  }
  df2 <- data_inputs2[[myr_vars[i]]] %>% melt()
  df2$run <- alts[2]
  #df3 <- data_inputs3[[myr_vars[i]]] %>% melt()
  #df3$run <- alts[3]
  df_aggr <- rbind(df1, df2)
  df_aggr$run <- factor(df_aggr$run, levels = alts)
  df_aggr$Var1 <- factor(df_aggr$Var1, levels=mo_convert[,2])
  
  plt <- ggplot(data=df_aggr, 
         aes(x=Var1, y=value, 
             group=Var2)) +
    theme_classic() +
    geom_path() +
    facet_wrap(~run) +
    #facet_wrap(as.factor(df_aggr$run), labeller = as_labeller(c( "NAA" = "CalSim II Historic Hydro NAA",
    #                                                                 "D1641" = "D1641_CalSimII_old",
    #                                                                 "ROR" = "ROR_CalSimII_old")
    #)) +
    labs(x="Month", y= myr_vars[i],
         title = "Comparison of SIT DSM Alternatives", 
         subtitle = paste0(myr_vars[i], " Across Years (1980-2000)"))
  
  plt
  ggsave(paste0("./cvpiaFlow/Plots/",comp,"/", myr_vars[i], ".png"), width = 12, height = 6, device = 'png', dpi = 300)
 
}

  ###############
  # variables with orig inputs set up as a 12x21 tibble
    # "upSacQ", "freeportQ"
  ###############

tib_vars <- c("upSacQ", "freeportQ")
for(i in 1:length(tib_vars)){
  df1 <- data_inputs1[[tib_vars[i]]] %>% as.matrix() %>% melt()
  df1$run <- alts[1]
  for(j in 1:length(df1$Var1)){
    df1$Var1[j] <- mo_convert[which(mo_convert$no==df1$Var1[j]),2]
  }
  df2 <- data_inputs2[[tib_vars[i]]] %>% as.matrix() %>% melt()
  df2$run <- alts[2]
  #df3 <- data_inputs3[[myr_vars[i]]] %>% melt()
  #df3$run <- alts[3]
  df_aggr <- rbind(df1, df2)
  df_aggr$run <- factor(df_aggr$run, levels = alts)
  df_aggr$Var1 <- factor(df_aggr$Var1, levels=mo_convert[,2])
  
  plt <- ggplot(data=df_aggr, 
                aes(x=Var1, y=value, 
                    group=Var2)) +
    theme_classic() +
    geom_path() +
    facet_wrap(~run) +
    #facet_wrap(as.factor(df_aggr$run), labeller = as_labeller(c( "NAA" = "CalSim II Historic Hydro NAA",
    #                                                                 "D1641" = "D1641_CalSimII_old",
    #                                                                 "ROR" = "ROR_CalSimII_old")
    #)) +
    labs(x="Month", y= tib_vars[i],
         title = "Comparison of SIT DSM Alternatives' Inputs", 
         subtitle = paste0(tib_vars[i], " Across Years (1980-2000)"))
  
  plt
  ggsave(paste0("./cvpiaFlow/Plots/",comp,"/", tib_vars[i], ".png"), width = 12, height = 6, device = 'png', dpi = 300)
  
}

  ################
  # variables with inputs that are a matrix
    # "gate.top", "Dlt.inf", "dlt.divers.tot", "dlt.divers"
  ################

myr3_vars <- c("gate.top", "Dlt.inf", "dlt.divers.tot", "dlt.divers") 
for(i in 1:length(myr3_vars)){
  for(m in 1:2){
    df1 <- data_inputs1[[myr3_vars[i]]][,,m] %>% melt()
    df1$run <- alts[1]
    for(j in 1:length(df1$Var1)){
      df1$Var1[j] <- mo_convert[which(mo_convert$no==df1$Var1[j]),2]
    }
    df2 <- data_inputs2[[myr3_vars[i]]][,,m] %>% melt()
    df2$run <- alts[2]
    #df3 <- data_inputs3[[myr3_vars[i]]][,,m] %>% melt()
    #df3$run <- alts[3]
    
    df_aggr <- rbind(df1, df2)
    df_aggr$run <- factor(df_aggr$run, levels = alts)
    df_aggr$Var1 <- factor(df_aggr$Var1, levels=mo_convert[,2])
    
    plt <- ggplot(data=df_aggr, 
                  aes(x=Var1, y=value, 
                      group=Var2)) +
      theme_classic() +
      geom_path() +
      facet_wrap(~run) +
      #facet_wrap(as.factor(df_aggr$run), labeller = as_labeller(c( "NAA" = "CalSim II Historic Hydro NAA",
      #                                                             "D1641" = "D1641_CalSimII_old",
      #                                                             "ROR" = "ROR_CalSimII_old")
      #)) +
      labs(x="Month", y= myr3_vars[i],
           title = "Comparison of SIT DSM Alternatives' Inputs", 
           subtitle = paste0(myr3_vars[i], "_", dimnames(get(myr3_vars[i]))[[3]][m],  " Across Years (1980-2000)"))
    
    
    plt
    ggsave(paste0("./cvpiaFlow/Plots/",comp,"/", myr3_vars[i],"_",dimnames(get(myr3_vars[i]))[[3]][m], ".png"), width = 12, height = 6, device = 'png', dpi = 300)
    
    
  }
  
}
#"prop.Q.bypasses" should go in the above list, except that for orig_data_inputs there are more than two matrix slices (1=Sutter, 5=Yolo)

  ############
  #for "prop.Q.bypasses"
  ############

for(m in 1:2){
  df1 <- data_inputs1[["prop.Q.bypasses"]][,,m] %>% melt()
  df1$run <- alts[1]
  for(j in 1:length(df1$Var1)){
    df1$Var1[j] <- mo_convert[which(mo_convert$no==df1$Var1[j]),2]
  }
  df2 <-  data_inputs2[["prop.Q.bypasses"]][,,m] %>% melt()
  df2$run <- alts[2]
  #df3 <- data_inputs3[["prop.Q.bypasses"]][,,m] %>% melt()
  #df3$run <- alts[3]
    
  df_aggr <- rbind(df1, df2)
  df_aggr$run <- factor(df_aggr$run, levels = alts)
  df_aggr$Var1 <- factor(df_aggr$Var1, levels=mo_convert[,2])
    
    plt <- ggplot(data=df_aggr, 
                  aes(x=Var1, y=value, 
                      group=Var2)) +
      theme_classic() +
      geom_path() +
      facet_wrap(~run) +
      #facet_wrap(as.factor(df_aggr$run), labeller = as_labeller(c( "NAA" = "CalSim II Historic Hydro NAA",
      #                                                             "D1641" = "D1641_CalSimII_old",
      #                                                             "ROR" = "ROR_CalSimII_old")
      #)) +
      labs(x="Month", y= "prop.Q.bypasses",
           title = "Comparison of SIT DSM Alternatives' Inputs", 
           subtitle = paste0("prop.Q.bypasses", "_", dimnames(get("prop.Q.bypasses"))[[3]][m],  " Across Years (1980-2000)"))
    
    
    plt
    ggsave(paste0("./cvpiaFlow/Plots/",comp,"/", "prop.Q.bypasses", "_", dimnames(get("prop.Q.bypasses"))[[3]][m], ".png"), width = 12, height = 6, device = 'png', dpi = 300)
  
}

#"dlt.gates" should also go in the above list, but for orig_data_inputs there is a funky tibble

  ###############
  # for dlt. gates
    # for 'originalDSM' comparison (df1), had to deal with different file structure
  ###############

for(m in 1:2){
  df1 <- data_inputs1[["dlt.gates"]][,c(1,m+1)] %>% 
    rename(value=names(.)[2]) %>%
    mutate(Var1=month, Var2=1980) %>%
    select(Var1, Var2, value)
  df1$run <- alts[1]
  for(j in 1:length(df1$Var1)){
    df1$Var1[j] <- mo_convert[which(mo_convert$no==df1$Var1[j]),2]
  }
  df2 <- data_inputs2[["dlt.gates"]][,,m] %>% melt()
  df2$run <- alts[2]
  #df3 <- data_inputs3[["dlt.gates"]][,,m] %>% melt()
  #df3$run <- alts[3]
  
  df_aggr <- rbind(df1, df2)
  df_aggr$run <- factor(df_aggr$run, levels = alts)
  df_aggr$Var1 <- factor(df_aggr$Var1, levels=mo_convert[,2])
  
  plt <- ggplot(data=df_aggr, 
                aes(x=Var1, y=value, 
                    group=Var2)) +
    theme_classic() +
    geom_path() +
    facet_wrap(~run) +
    #facet_wrap(as.factor(df_aggr$run), labeller = as_labeller(c( "NAA" = "CalSim II Historic Hydro NAA",
    #                                                             "D1641" = "D1641_CalSimII_old",
    #                                                             "ROR" = "ROR_CalSimII_old")
    #)) +
    labs(x="Month", y= "dlt.gates",
         title = "Comparison of SIT DSM Alternatives' Inputs", 
         subtitle = paste0("dlt.gates", " Across Years (1980-2000)"))
  
  plt
  ggsave(paste0("./cvpiaFlow/Plots/",comp,"/dlt.gates_", dimnames(get("dlt.gates"))[[3]][m], ".png"), width = 12, height = 6, device = 'png', dpi = 300)
  
}

  ##############
  # prop.pulse, retQ
    # DROPPED retQ FOR TIME BEING
  ##############

ws_vars <- c("prop.pulse")
time_step <- c("Months", "Years")
ws_list <- c("Upper Sacramento River", "Clear Creek")
for(i in 1:length(ws_vars)){
  df1 <- data_inputs1[[ws_vars[i]]][,2:13] 
  dimnames(df1)[[1]] <- pull(data_inputs1[[ws_vars[i]]],watershed) 
  df1 <- df1 %>% as.matrix() %>% melt()
  df1$run <- alts[1]
  for(j in 1:length(df1$Var1)){
    df1$Var2[j] <- mo_convert[which(mo_convert$no==df1$Var2[j]),2]
  }
  names(df1) <- c("watershed", "variable", "value", "run")
  
  df2 <- data_inputs2[[ws_vars[i]]][,2:13] 
  dimnames(df2)[[1]] <- pull(data_inputs2[[ws_vars[i]]],inputs) 
  df2 <- df2 %>% as.matrix() %>% melt()
  df2$run <- alts[2]
  names(df2) <- c("watershed", "variable", "value", "run")
  
  df_aggr <- rbind(df1, df2)
  df_aggr$run <- factor(df_aggr$run, levels = alts)
  
  if(time_step[i]=="Months"){
    df_aggr$variable <- factor(df_aggr$variable, levels=mo_convert[,2])    
  }
  if(time_step[i] == "Years"){
    df_aggr$variable <- substr(df_aggr$variable, 3,4) 
    df_aggr$variable <- ordered(df_aggr$variable, levels = c(79:99, "00"))
  }  
  
  for(w in 1:length(ws_list)){
   wsDat <- subset(df_aggr, df_aggr$watershed == ws_list[w]) 
    
 
  plt <- ggplot(data=wsDat, 
                aes(x=variable, y=value, 
                    group=run)) +
    theme_classic() +
    geom_path() +
    facet_wrap(~run) +
    #facet_wrap(as.factor(wsDat$run), labeller = as_labeller(c( "NAA" = "CalSim II Historic Hydro NAA",
    #                                                           "D1641" = "D1641_CalSimII_old",
    #                                                           "ROR" = "ROR_CalSimII_old")
    #)) +
    labs(x= time_step[i], y= ws_vars[i],
         title = "Comparison of SIT DSM Alternatives' Inputs", 
         subtitle = paste0(ws_vars[i], " in the ", ws_list[w], "watershed, Across ",time_step[i]))
  
  plt
  ggsave(paste0("./cvpiaFlow/Plots/",comp,"/", ws_vars[i], "_", ws_list[w], ".png"), width = 12, height = 6, device = 'png', dpi = 300)
  }
}

  ############################
  # "t.diver", "p.diver"
    # here we need to deal with month as columns and years as matrix slices
  ############################

div_vars <- c("t.diver", "p.diver")
ws_list <- c("Upper Sacramento River", "Clear Creek", "Upper-mid Sacramento River", "Lower-mid Sacramento River", "Lower Sacramento River")
for(i in 1:length(div_vars)){
 df_aggr_allyr <- data.frame()
  for(m in 1:length(dimnames(t.diver)[[3]])){
    df1 <- data_inputs1[[div_vars[i]]][,,m] %>% melt()
    df1$run <- alts[1]
    df1$year <- dimnames(t.diver)[[3]][m]
    for(j in 1:length(df1$Var1)){
      df1$Var1[j] <- shed_convert[which(shed_convert$no==df1$Var1[j]),2]
      df1$Var2[j] <- mo_convert[which(mo_convert$no==df1$Var2[j]),2]
    }
    df2 <- data_inputs2[[div_vars[i]]][,,m] %>% melt()
    df2$run <- alts[2]
    df2$year <- dimnames(t.diver)[[3]][m]

    df_aggr <- rbind(df1, df2)
    df_aggr_allyr <- rbind(df_aggr_allyr, df_aggr)
  }
  df_aggr_allyr$run <- factor(df_aggr$run, levels = alts)
  df_aggr_allyr$Var2 <- factor(df_aggr_allyr$Var2, levels=mo_convert[,2]) 
  for(w in 1:length(ws_list)){
    wsDat <- subset(df_aggr_allyr, df_aggr_allyr$Var1 == ws_list[w]) 
    
    plt <- ggplot(data=wsDat, 
                  aes(x=Var2, y=value, 
                      group=year)) +
      theme_classic() +
      geom_path() +
      facet_wrap(~run) +
      #facet_wrap(as.factor(wsDat$run), labeller = as_labeller(c( "NAA" = "CalSim II Historic Hydro NAA",
      #                                                           "D1641" = "D1641_CalSimII_old",
      #                                                           "ROR" = "ROR_CalSimII_old")
      #)) +
      labs(x="Month", y= div_vars,
           title = "Comparison of SIT DSM Alternatives' Inputs", 
           subtitle = paste0(div_vars[i], "_", ws_list[w],  " Across Years (1980-2000)"))
    
    
    plt
    ggsave(paste0("./cvpiaFlow/Plots/",comp,"/",div_vars[i], "_", ws_list[w], ".png"), width = 12, height = 6, device = 'png', dpi = 300)
    
    
  }
  
}

  ################
  # temp vars
    # here we need to deal with month as columns and years as matrix slices
    # NEEDS TO BE FIXED/MOVED TO cvpiaTemperature
  ################  

t_vars <- c("DegDay", "juv.tmp")
all_ws <- read.csv("winterRunDSM-main/watershedInfo.csv")[, c("watershed")]
ws_list <- c("Upper Sacramento River", "Upper-mid Sacramento River", "Lower-mid Sacramento River", "Lower Sacramento River")
for(i in 1:length(t_vars)){
  tmp_df_aggr_allyr <- data.frame()
  for(m in 1:length(dimnames(t.diver)[[3]])){
    tmp_df_c2 <- C2_data_inputs[[t_vars[i]]][,,m] %>% melt()
    tmp_df_c2$run <- "C2NAA"
    tmp_df_c2$year <- dimnames(t.diver)[[3]][m]
    tmp_df_c2$Var1 <- all_ws
    tmp_df_c3 <- C3_data_inputs[[t_vars[i]]][,,m] %>% melt()
    tmp_df_c3$run <- "C3"
    tmp_df_c3$year <- dimnames(t.diver)[[3]][m]
    tmp_df_c3$Var1 <- all_ws
    tmp_df_old <- orig_data_inputs[[t_vars[i]]][,,m] %>% melt() 
    tmp_df_old$run <- "Orig"
    tmp_df_old$year <- dimnames(t.diver)[[3]][m]
    tmp_df_old$Var1 <- all_ws
    tmp_df_aggr <- rbind(tmp_df_c2, tmp_df_c3, tmp_df_old)
    tmp_df_aggr <- tmp_df_aggr %>% mutate(Var2 = month.abb[as.numeric(Var2)])
    tmp_df_aggr_allyr <- rbind(tmp_df_aggr_allyr, tmp_df_aggr)
    
  }
  tmp_df_aggr_allyr$run <- factor(tmp_df_aggr_allyr$run, levels = c("Orig", "C2NAA", "C3"))
  for(w in 1:length(ws_list)){
    wsDat <- subset(tmp_df_aggr_allyr, tmp_df_aggr_allyr$Var1 == ws_list[w]) 
    wsDat$Var2 <- factor(wsDat$Var2, levels = month.abb) 
    plt <- ggplot(data=wsDat, 
                  aes(x=Var2, y=value, 
                      group=year)) +
      theme_classic() +
      geom_path() +
      facet_wrap(as.factor(wsDat$run), labeller = as_labeller(c( "Orig" = "Original Input",
                                                                 "C2NAA" = "CalSim II Historic Hydro NAA",
                                                                 "C3" = "CalSim III")
      )) +
      labs(x="Month", y= t_vars,
           title = "Comparison of SIT DSM Inputs", 
           subtitle = paste0(t_vars[i], "_", ws_list[w],  " Across Years (1980-2000)"))
    
    
    plt
    ggsave(paste0("fig/", t_vars[i], "_", ws_list[w], ".png"), width = 12, height = 6, device = 'png', dpi = 300)
    
    
  }
  
}


  #####
  # Plots of delta_flow
    # UNNECESSARY
    # NEEDS TO BE FIXED
  #####

delta_flows$run <- "NAA"
delta_flows_old <- DSMflow::delta_flows
delta_flows_old$run <- "Old"
delta_flows_aggr <- rbind(delta_flows, delta_flows_old)


##North Delta Inflow
ggplot(data=delta_flows_aggr, 
       aes(x=factor(month.abb[month(date)], levels = month.abb), y=n_dlt_inflow_cfs, 
           group=year(date))) +
  theme_classic() +
  labs(x = "Month", y = "North Delta Inflow (cfs)", 
       title = "Historic NAA vs. Original SIT DSM Inputs", 
       subtitle = "North Delta Inflow Across Years (1922-2002)") +
  geom_path() +
  facet_wrap(as.factor(delta_flows_aggr$run), labeller = as_labeller(c("NAA" = "Historic Hydro NAA",
                                                                       "Old" = "Original Input")
  ))

##South Delta Inflow
ggplot(data=delta_flows_aggr, 
       aes(x=factor(month.abb[month(date)], levels = month.abb), y=s_dlt_inflow_cfs, 
           group=year(date))) +
  theme_classic() +
  labs(x = "Month", y = "South Delta Inflow (cfs)", 
       title = "Historic NAA vs. Original SIT DSM Inputs", 
       subtitle = "South Delta Inflow Across Years (1922-2002)") +
  geom_path() +
  facet_wrap(as.factor(delta_flows_aggr$run), labeller = as_labeller(c("NAA" = "Historic Hydro NAA",
                                                                       "Old" = "Original Input")
  ))

  ####################
  # Plots of flows_cfs
  ####################

df1 <- cvpiaFlow::flows_cfs %>%
  mutate(Month = month(date), Year = year(date), run=alts[1]) %>%
  pivot_longer(`Upper Sacramento River`:`San Joaquin River`, values_to="value", names_to="watershed") %>%
  filter(Year %in% c(1980:2000))
df2 <- readRDS(paste0("./cvpiaFlow/Final Data Objects/",alts[2],"/flows_cfs.rds")) %>%
  mutate(Month = month(date), Year = year(date), run=alts[2]) %>%
  pivot_longer(`Upper Sacramento River`:`San Joaquin River`, values_to="value", names_to="watershed") %>%
  filter(Year %in% c(1980:2000))
df_all <- rbind(df1, df2)

# Upper Sac
df_up_sac <- df_all %>% filter(watershed=="Upper Sacramento River")
ggplot(data=df_up_sac, 
       aes(x=Month, y=value, 
           group=year(date))) +
  theme_classic() +
  labs(x = "Month", y = "Flow (cfs)", 
       title = "CalSim 3 NAA vs. Original SIT DSM Inputs", 
       subtitle = "Upper Sacramento River Flow Across Years (1980-2000)") +
  geom_path() +
  facet_wrap(~run)
ggsave(paste0("./cvpiaFlow/Plots/",comp,"/Flows Upper Sacramento River.png"), width = 12, height = 6, device = 'png', dpi = 300)


# Clear Creek
df_clear <- df_all %>% filter(watershed=="Clear Creek")
ggplot(data=df_clear, 
       aes(x=Month, y=value, 
           group=year(date))) +
  theme_classic() +
  labs(x = "Month", y = "Flow (cfs)", 
       title = "CalSim 3 NAA vs. Original SIT DSM Inputs", 
       subtitle = "Clear Creek Flow Across Years (1980-2000)") +
  geom_path() +
  facet_wrap(~run)
ggsave(paste0("./cvpiaFlow/Plots/",comp,"/Flows Clear Creek.png"), width = 12, height = 6, device = 'png', dpi = 300)


# American River

df_amer <- df_all %>% filter(watershed=="American River")
ggplot(data=df_amer, 
       aes(x=Month, y=value, 
           group=year(date))) +
  theme_classic() +
  labs(x = "Month", y = "Flow (cfs)", 
       title = "CalSim 3 NAA vs. Original SIT DSM Inputs", 
       subtitle = "American River Flow Across Years (1980-2000)") +
  geom_path() +
  facet_wrap(~run)
ggsave(paste0("./cvpiaFlow/Plots/",comp,"/Flows American River.png"), width = 12, height = 6, device = 'png', dpi = 300)


# Stanislaus River

df_stan <- df_all %>% filter(watershed=="Stanislaus River")
ggplot(data=df_stan, 
       aes(x=Month, y=value, 
           group=year(date))) +
  theme_classic() +
  labs(x = "Month", y = "Flow (cfs)", 
       title = "CalSim 3 NAA vs. Original SIT DSM Inputs", 
       subtitle = "Stanislaus River Flow Across Years (1980-2000)") +
  geom_path() +
  facet_wrap(~run)
ggsave(paste0("./cvpiaFlow/Plots/",comp,"/Flows Stanislaus River.png"), width = 12, height = 6, device = 'png', dpi = 300)

  ##############
  # bypass_flows
  ##############

df1 <- cvpiaFlow::bypass_flows %>%
  mutate(Month = month(date), Year = year(date), run=alts[1]) %>%
  pivot_longer(`sutter1`:`yolo2`, values_to="value", names_to="watershed") %>%
  filter(Year %in% c(1980:2000))
  
df2 <- readRDS(paste0("./cvpiaFlow/Final Data Objects/",alts[2],"/bypass_flows.rds")) %>%
  mutate(Month = month(date), Year = year(date), run=alts[2], 
         watershed=input, value=flow) %>%
  filter(Year %in% c(1980:2000)) %>%
  select(date, Month, Year, run, watershed, value)

df_all <- rbind(df1, df2)

#Sutter 4
df_sut <- df_all %>% filter(watershed=="sutter4")
ggplot(data=df_sut, 
       aes(x=Month, y=value, group=year(date))) +
  theme_classic() +
  labs(x = "Month", y = "Flow (cfs)", 
       title = "CalSim 3 NAA vs. Original SIT DSM Inputs", 
       subtitle = "Sutter Bypass 4 Flow Across Years (1980-2000)") +
  geom_path() +
  facet_wrap(~run)
ggsave(paste0("./cvpiaFlow/Plots/",comp,"/Flows Sutter Bypass 4.png"), width = 12, height = 6, device = 'png', dpi = 300)

#Yolo 2
df_yol <- df_all %>% filter(watershed=="yolo2")
ggplot(data=df_yol, 
       aes(x=Month, y=value, 
           group=year(date))) +
  theme_classic() +
  labs(x = "Month", y = "Flow (cfs)", 
       title = "CalSim 3 NAA vs. Original SIT DSM Inputs", 
       subtitle = "Yolo Bypass 2 Flow Across Years (1980-2000)") +
  geom_path() +
  facet_wrap(~run)
ggsave(paste0("./cvpiaFlow/Plots/",comp,"/Flows Yolo Bypass 2.png"), width = 12, height = 6, device = 'png', dpi = 300)

