############################
# Annotation
  # This script creates comparison plots for original DSM inputs and NAA HEC-5Q (PA Logic) inputs
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
alt_names <- c("2019 Original", "2021 LTO NAA")
comp <- "originalDSM vs LTO NAA"

#create a list of the variables of interest
'/
flow_vars <- c("upSacQ", "t.diver", "retQ", "prop.Q.bypasses",
               "prop.pulse", "p.diver", "gate.top", "freeportQ", "Dlt.inf", 
               "dlt.gates", "dlt.divers.tot", "dlt.divers", 
               "Q_free", "Q_vern", "Q_stck",  "SWP_exp") 
'
temp_vars <- c("DegDay", "juv.tmp")
all_vars <- c(temp_vars)

data_inputs1 <- list()
data_inputs2 <- list()

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

#Plotting libraries for comparison of original temperature inputs to HEC-5Q NAA

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

  ##################
  # Upper Sacramento
  ##################

  # Monthly Degree Day Accumulation

df1 <- data_inputs1$DegDay %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[1]) %>%
  filter(Watershed==1)

df2 <- data_inputs2$DegDay %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[2]) %>%
  filter(Watershed==1)

df_aggr <- rbind(df1, df2)
for(i in 1:length(df_aggr$Month)){
  df_aggr$Month[i] <- mo_convert[which(mo_convert$no==df_aggr$Month[i]),2]
}
df_aggr$run <- factor(df_aggr$run, levels = alt_names)
df_aggr$Month <- factor(df_aggr$Month, levels=mo_convert[,2])

plt <- ggplot(data=df_aggr, 
              aes(x=Month, y=value, 
                  group=Year)) +
  theme_classic() +
  geom_path() +
  facet_wrap(~run) +
  labs(x="Month", y="Degree Day Accumulation (C)")

ggsave(paste0("./cvpiaTemperature/Plots/",comp,"/USR DD.png"), width = 12, height = 6, device = 'png', dpi = 300)


  # Average Temperature

df1 <- data_inputs1$juv.tmp %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[1]) %>%
  filter(Watershed==1)

df2 <- data_inputs2$juv.tmp %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[2]) %>%
  filter(Watershed==1)

df_aggr <- rbind(df1, df2)
for(i in 1:length(df_aggr$Month)){
  df_aggr$Month[i] <- mo_convert[which(mo_convert$no==df_aggr$Month[i]),2]
}
df_aggr$run <- factor(df_aggr$run, levels = alt_names)
df_aggr$Month <- factor(df_aggr$Month, levels=mo_convert[,2])

plt <- ggplot(data=df_aggr, 
              aes(x=Month, y=value, 
                  group=Year)) +
  theme_classic() +
  geom_path() +
  facet_wrap(~run) +
  labs(x="Month", y="Average Temperature (C)")

ggsave(paste0("./cvpiaTemperature/Plots/",comp,"/USR AT.png"), width = 12, height = 6, device = 'png', dpi = 300)

  #############
  # Clear Creek
  #############

# Monthly Degree Day Accumulation

df1 <- data_inputs1$DegDay %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[1]) %>%
  filter(Watershed==7)

df2 <- data_inputs2$DegDay %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[2]) %>%
  filter(Watershed==7)

df_aggr <- rbind(df1, df2)
for(i in 1:length(df_aggr$Month)){
  df_aggr$Month[i] <- mo_convert[which(mo_convert$no==df_aggr$Month[i]),2]
}
df_aggr$run <- factor(df_aggr$run, levels = alt_names)
df_aggr$Month <- factor(df_aggr$Month, levels=mo_convert[,2])

plt <- ggplot(data=df_aggr, 
              aes(x=Month, y=value, 
                  group=Year)) +
  theme_classic() +
  geom_path() +
  facet_wrap(~run) +
  labs(x="Month", y="Degree Day Accumulation (C)")

ggsave(paste0("./cvpiaTemperature/Plots/",comp,"/ClearC DD.png"), width = 12, height = 6, device = 'png', dpi = 300)

# Average Temperature

df1 <- data_inputs1$juv.tmp %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[1]) %>%
  filter(Watershed==7)

df2 <- data_inputs2$juv.tmp %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[2]) %>%
  filter(Watershed==7)

df_aggr <- rbind(df1, df2)
for(i in 1:length(df_aggr$Month)){
  df_aggr$Month[i] <- mo_convert[which(mo_convert$no==df_aggr$Month[i]),2]
}
df_aggr$run <- factor(df_aggr$run, levels = alt_names)
df_aggr$Month <- factor(df_aggr$Month, levels=mo_convert[,2])

plt <- ggplot(data=df_aggr, 
              aes(x=Month, y=value, 
                  group=Year)) +
  theme_classic() +
  geom_path() +
  facet_wrap(~run) +
  labs(x="Month", y="Average Temperature (C)")

ggsave(paste0("./cvpiaTemperature/Plots/",comp,"/ClearC AT.png"), width = 12, height = 6, device = 'png', dpi = 300)

  #############
  # Cottonwood Creek
  #############

# Monthly Degree Day Accumulation

df1 <- data_inputs1$DegDay %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[1]) %>%
  filter(Watershed==8)

df2 <- data_inputs2$DegDay %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[2]) %>%
  filter(Watershed==8)

df_aggr <- rbind(df1, df2)
for(i in 1:length(df_aggr$Month)){
  df_aggr$Month[i] <- mo_convert[which(mo_convert$no==df_aggr$Month[i]),2]
}
df_aggr$run <- factor(df_aggr$run, levels = alt_names)
df_aggr$Month <- factor(df_aggr$Month, levels=mo_convert[,2])

plt <- ggplot(data=df_aggr, 
              aes(x=Month, y=value, 
                  group=Year)) +
  theme_classic() +
  geom_path() +
  facet_wrap(~run) +
  labs(x="Month", y="Degree Day Accumulation (C)")

ggsave(paste0("./cvpiaTemperature/Plots/",comp,"/CottonwoodC DD.png"), width = 12, height = 6, device = 'png', dpi = 300)

# Average Temperature

df1 <- data_inputs1$juv.tmp %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[1]) %>%
  filter(Watershed==8)

df2 <- data_inputs2$juv.tmp %>% 
  reshape2::melt(varnames=c("Watershed","Month","Year")) %>% 
  mutate(run=alt_names[2]) %>%
  filter(Watershed==8)

df_aggr <- rbind(df1, df2)
for(i in 1:length(df_aggr$Month)){
  df_aggr$Month[i] <- mo_convert[which(mo_convert$no==df_aggr$Month[i]),2]
}
df_aggr$run <- factor(df_aggr$run, levels = alt_names)
df_aggr$Month <- factor(df_aggr$Month, levels=mo_convert[,2])

plt <- ggplot(data=df_aggr, 
              aes(x=Month, y=value, 
                  group=Year)) +
  theme_classic() +
  geom_path() +
  facet_wrap(~run) +
  labs(x="Month", y="Average Temperature (C)")

ggsave(paste0("./cvpiaTemperature/Plots/",comp,"/CottonwoodC AT.png"), width = 12, height = 6, device = 'png', dpi = 300)