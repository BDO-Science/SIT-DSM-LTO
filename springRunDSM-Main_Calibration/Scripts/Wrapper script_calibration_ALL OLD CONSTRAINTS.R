#####################
# Annotation
  # This wrapper script is based on SpringRunStochasticDSM_11_13_2019CALIBRATION.R
    # Located in the springRunDSM-Main repository, obtained from Peterson and Duarte 2020 authors
  # I changed pathing to reflect this model's location in a new repository
  # Note for running:
    # *Be sure to start a new R session before running models to prevent clashes between packages
    # *Users will need to install the following packages from GitHub, in order to load base model inputs
      # remotes::install_github("FlowWest/CVPIAdata")
      # remotes::install_github("FlowWest/cvpiaCalibration")
      # remotes::install_github("CVPIA-OSC/DSMCalibrationData")
    # *We also recommend users install other dependent data packages, for documentation purposes
      # e.g., remotes::install_github("FlowWest/cvpiaHabitat")
      # remotes::install_github("FlowWest/cvpiaFlow")
      # remotes::install_github("FlowWest/cvpiaTemperature")
  # Changes to the run_scenarios() function
    # I moved the run_scenarios() function (renamed life_cycle_model_spring_calibration_obsYuba_fixed(), 
      # life_cycle_model_spring_calibration_obsYuba_fixed_visual()) into its own scripts
    # (located in 'functions' sub-folder) and call it in this script
  # Output from calibrations is saved as an .rds file in the 'Output' sub-folder
  # Changed method of calibration to match FlowWest documentation
  # Conducts summary plotting of parameter estimates and model fit
#####################

#####################
# Install libraries
#####################

#options(download.file.method = "wininet")
#remotes::install_github("FlowWest/cvpiaCalibration")
#remotes::install_github("CVPIA-OSC/DSMCalibrationData")

require(GA)
#require(tidyverse)
require(cvpiaCalibration)
require(data.table)

################
# Load data, functions
################

# Read in functions
fun.place<-paste(getwd(),"./springRunDSM-Main_Calibration/functions",sep="")
functs<-dir(path=fun.place,full.names=FALSE,pattern=".R",recursive=FALSE)
for(ia in 1:length(functs)) source(paste(fun.place,functs[ia],sep="/"))

# Read in data

calib_data<-readRDS("./springRunDSM-Main_Calibration/delta-calibration-1980_2017.rds")

all_inputs<-cvpiaCalibration::spring_inputs 
list2env(all_inputs,.GlobalEnv)

known.Adults.seed <- DSMCalibrationData::grandtab_imputed$spring # fill in missing values w/ mean escapement
# use these to seed the first 5 years

known.Adults <- DSMCalibrationData::grandtab_observed$spring # NAs for missing values
known.Adults <- ifelse(known.Adults<100,NA,known.Adults)
# use these to determine years/watersheds to include in model fit metric

known.Adults.ref<-read.csv("./springRunDSM-main_Calibration/SimulaitonSeed_SpringRunAdults.csv") #Spring run GrandTab data
known.Adults.ref<-known.Adults.ref[order(known.Adults.ref$order),]

# Modify data

maxHabitat<-read.csv("./springRunDSM-Main_Calibration/theoretical_max_area.csv")
table(maxHabitat$species)
maxHabitat<-maxHabitat[which(maxHabitat$species=="sr"),]
maxSpwnHab<-maxHabitat[which(maxHabitat$lifestage=="spawning"),c(1,4)]
maxSpwnHab<-merge(known.Adults.ref[,1:2],maxSpwnHab,all=TRUE)
maxSpwnHab<-maxSpwnHab[order(maxSpwnHab$order),]
maxSpwnHab$max_suit_sqm[is.na(maxSpwnHab$max_suit_sqm)==TRUE]<-0
maxRearHab<-maxHabitat[which(maxHabitat$lifestage=="rearing"),c(1,4)]
maxRearHab<-merge(known.Adults.ref[,1:2],maxRearHab,all=TRUE)
maxRearHab<-maxRearHab[order(maxRearHab$order),]
maxRearHab$max_suit_sqm[is.na(maxRearHab$max_suit_sqm)==TRUE]<-0

#proportion hatchery
Butte_Creek=mean(c(0.01,0,0,0)) #2010 - 2013 from CWT reports
Feather_River=mean(c(0.78,0.90,0.90,0.84))
Yuba_River=mean(c(0.71,0.495,0.36,0.40))
prop.hatch<-c(rep(0,5),Butte_Creek,rep(0,12),Feather_River,Yuba_River,rep(0,11))

#calculate total habitat for sutter and yolo
IChab.sutter<-IChab.bypass[1,,]+IChab.bypass[2,,]+IChab.bypass[3,,]+IChab.bypass[4,,]
IChab.yolo<-IChab.bypass[5,,]+IChab.bypass[6,,]
floodp.sutter<-floodp.bypass[1,,]+floodp.bypass[2,,]+floodp.bypass[3,,]+floodp.bypass[4,,]
floodp.yolo<-floodp.bypass[5,,]+floodp.bypass[6,,]

aveT201<-aveT20
aveT20D1<-aveT20D
ptemp20mc<-p.tempMC20

Q_free<-calib_data$Q_free[,19:38]    
Q_vern<-calib_data$Q_vern[,19:38]    
Q_stck<-calib_data$Q_stck[,19:38]    
Temp_vern<-calib_data$Temp_vern[,19:38]  
Temp_pp<-calib_data$Temp_pp[,19:38] 
CVP_exp<-calib_data$CVP_exp[,19:38]    
SWP_exp<-calib_data$SWP_exp[,19:38]  
Trap_trans <-0

#fill in NAs with 0s
p.diver[is.na(p.diver)]<-0 
t.diver[is.na(t.diver)]<-0 
DegDay[is.na(DegDay)]<-0 
retQ[is.na(retQ)]<-0 
p.tempMC20[is.na(p.tempMC20)]<-0 
prop.pulse[is.na(prop.pulse)]<-0 
IChab.spawn[is.na(IChab.spawn)]<-0 
IChab.fry[is.na(IChab.fry)]<-0 
IChab.juv[is.na(IChab.juv)]<-0 
floodP[is.na(floodP)]<-0
SR.pools[is.na(SR.pools)]<-0

Q_free[is.na(Q_free)]<-0
Q_vern[is.na(Q_vern)]<-0
Q_stck[is.na(Q_stck)]<-0
Temp_vern[is.na(Temp_vern)]<-0  
Temp_pp[is.na(Temp_pp)]<-0
CVP_exp[is.na(CVP_exp)]<-0
SWP_exp[is.na(SWP_exp)]<-0

#Goals<-read.csv("Doubling goals.csv")

regulated<-c(1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,1,1,1,0,1,1,1,0,1,1,1,1,1)
regulated[c(16,17,21,22,24,31)]<-0 #assume mainstem and bypass habitats don't degrade for this

states<-data.frame(known.Adults.ref$watershed,known.Adults.ref$order,regulated)
colnames(states)<-c("watershed","order","regulated")
groups<-read.csv("./springRunDSM-Main_Calibration/Grouping.csv")
states<-merge(states,groups)
states<-states[order(states$order),]
states$grp<-ifelse(states$grp==9,7,groups$grp) #all mainstem sections are now group 7
states$DiversityGroup[c(18,19,20,23,26,27)]<-5 #split diversity group
states$DiversityGroup<-ifelse(states$grp==7,6,states$DiversityGroup) #all mainstem sections are now diversity group 6

states$spwnDecay<-ifelse(states$grp==1,0.973647181,ifelse(states$grp==2,0.994949237,ifelse(states$grp==3,0.994949237,
                                                                                           ifelse(states$grp==4,0.979148362,ifelse(states$grp==5,0.962256359,0.989793782)))))
states$rearDecay<-ifelse(states$grp==1,0.987175243,ifelse(states$grp==2,0.997487405,ifelse(states$grp==3,0.997487405,
                                                                                           ifelse(states$grp==4,0.989793782,ifelse(states$grp==5,0.981853233,0.994949237)))))

hatcheryStream<-data.frame(known.Adults.ref$watershed,c(rep(0,2),1,rep(0,15),1,rep(0,3),1,rep(0,3),1,1,rep(0,3)))
colnames(hatcheryStream)<-c("watershed","hatchery")

states$srpop<-rep(0,dim(states)[1])
states$srpop[c(2,3,6,7,10,12,19,20)]<-1 #these are where spring run are
floodPNew<-rep(0,31)

# Replace t.diver[1,,] (Upper Sacramento River) with correct diversion values for calibration purposes
synth.t.diver <- readRDS("./winterRunDSM-Main_Calibration/New t.diver data for calibration/synth.t.diver.rds")
t.diver[1,,] <- synth.t.diver
                                                                                                                                                                                      
# Calculate normalizing factors (mean escapement) and weights (data availability)

# Watersheds for tracking abundance
# Antelope Creek [2]*
# Battle Creek [3] # NOT INCLUDED IN OG CALCULATION
# Butte Creek [6]*
# Clear Creek [7]*
# Deer Creek [10]*
# Mill Creek [12]*
# Feather River [19]*
# Yuba River [20]*
track.sheds <- c(2,3,6,7,10,12,19,20)

# Calculate mean escapement for each tracked watershed for normalization
mean.esc <- numeric(31)
#mean.esc.test <- numeric(31) # used to show that averages for imputed values are based on full, unfiltered time series 1998-2016
for(i in track.sheds){
  mean.esc[i] <- mean(known.Adults[i,1:14] * (1-prop.hatch[i]), na.rm=T)
  #mean.esc.test[i] <- mean(known.Adults[i,], na.rm=T)
}

# Calculate proportional data availability for weighting
weights <- numeric(31)
num_obs <- rowSums(!is.na(known.Adults[track.sheds,1:14]))
tot_obs <- sum(!is.na(known.Adults[track.sheds,1:14]))
weights[track.sheds] <- num_obs/tot_obs

###########################
# Test, troubleshoot model
###########################

# test model
x <- life_cycle_model_calib_obsYuba_fixed(-2.25097915,-2.31387713,1.86641130,
                       -0.55423070,-3.49999933,-2.56726844,
                       -2.52403823,1.42642277,-2.79409568,
                       2.04438137,0.99985602,-2.89141694,
                       0.75077947,-3.10495582,2.91518569,
                       -3.49954625,-1.49855839,-3.22990407,
                       2.49974122,-2.96201071,0.09999992,
                       0.01000010,0.19126503,0.61104442,
                       -0.70866158,2.10420292,-2.59452699,
                       2.79189516,-1.53805220)
# Starting value of model fit: -40833.83

#########################
# Set up, run calibration
#########################

# set bounds to calibrate intercepts for survival
# default values, based on FlowWest recommendations
#minz <- c(rep(-3.5,14),2.5,rep(-3.5,14)) # set 2.5 lower bound for en route survival [15]
#maxz <- rep(3.5,29)

# modified default values, forcing some expected covariate coefficients to be positive
minz <- c(rep(-3.5,14),0,rep(-3.5,5),rep(0,4),rep(-3.5,5))
maxz <- c(rep(3.5,15),rep(0,5),rep(3.5,6),0,3.5,0)
  # set 0 for lower bound for en route survival [15], effects of predators/diversions on survival [21:24]
  # set -2 for upper bound for ocean entry survival [16:20,27,29] - strict OC
    # -1 for upper bound - looseOC
    # 0 for upper bound - looserOC

# specify GA target size
ps <- 100 # 150 recommend by FlowWest
# ps=10, ~30 min for convergence

# set initial values
# These are the current parameter values in the spring-run DSM
  # Except for parameter 19 (Feather River ocean entry), which gets assigned value -1
    # Needs to be within <-1 constraint
sugg1<-c(-2.25097915,-2.31387713,1.86641130,
         -0.55423070,-3.49999933,-2.56726844,
         -2.52403823,1.42642277,-2.79409568,
         2.04438137,0.99985602,-2.89141694,
         0.75077947,-3.10495582,2.91518569,
         -3.49954625,-1.49855839,-3.22990407,
         0, #2.49974122, # replaced prev. Feather River ocean entry to fall within constraints
         -2.96201071,0.09999992,
         0.01000010,0.19126503,0.61104442,
         -0.70866158,2.10420292,-2.59452699,
         2.79189516,-1.53805220)
sugg <- matrix(sugg1, nrow=ps, ncol=29, byrow=TRUE)

# Run calibration
res <- ga(type="real-valued",
          fitness=
            function(x) -life_cycle_model_calib_obsYuba_fixed(
              x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],
              x[9],x[10],x[11],x[12],x[13],x[14],x[15],x[16],
              x[17],x[18],x[19],x[20],x[21],x[22],x[23],x[24],
              x[25],x[26],x[27],x[28],x[29]
            ),
          lower=minz,
          upper=maxz,
          suggestions=sugg,
          popSize=ps,
          maxiter=10000,
          run=50,
          parallel=TRUE,
          pmutation=.4)
plot(res)
summary(res)
summary(res)$solution
Sys.time() # started this run at 6:36 PM
saveRDS(res, paste0("./springRunDSM-Main_Calibration/Output/Calib Output_popSize",ps,"_looserOC_obsYubafix_",as.numeric(Sys.time()),".rds"))

# Import saved outputs

# popSize=10, strict marine survival constraints (-3.5 to -2)
x1 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_StrictOC_1688402190.87142.rds")
x2 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_StrictOC_1688407679.49013.rds")
x3 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_StrictOC_1688413142.38748.rds")

# popSize=10, loose marine survival constraints (-3.5 to -1)
x4 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looseOC_1688586666.19725.rds")
x5 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looseOC_1688580741.60342.rds")
x6 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looseOC_1688602352.56247.rds")

# popSize=10, looser marine survival constraints (-3.5 to 0)
x7 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_1688666392.085.rds")
x8 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_1688661981.89079.rds")
x9 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_1688660131.48187.rds")

# popSize=10, looser marine survival constraints (-3.5 to 0), obs. Yuba spawners
x10 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_obsYuba_1688689068.91919.rds")
x11 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_obsYuba_1688677248.18456.rds")
x12 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_obsYuba_1688670807.69497.rds")

# popSize=100, looser marine survival constraints (-3.5 to 0), obs. Yuba spawners
x13 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize100_looserOC_obsYuba_1689116793.75586.rds")
x14 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize100_looserOC_obsYuba_1689130505.78926.rds")
x15 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize100_looserOC_obsYuba_1689189020.65146.rds")

# popSize=10, looser marine survival constraints (-3.5 to 0), obs. Yuba spawners, fixed sim vs known
x16 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_obsYubafix_1691622281.89959.rds")
x17 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_obsYubafix_1691627840.21047.rds")
x18 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_obsYubafix_1691630718.80601.rds")

# popSize=100, looser marine survival constraints (-3.5 to 0), obs. Yuba spawners, fixed sim vs known
x19 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize100_looserOC_obsYubafix_1691709203.18658.rds")
x20 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize100_looserOC_obsYubafix_1691749353.83552.rds")
x21 <- readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize100_looserOC_obsYubafix_1691808930.96922.rds")


# Format saved outputs

grouped <- c(1:6,25,26,28,7:20,29,27,21:24)
x0.df <- data.frame(id=as.factor(1:29), value=sugg1[grouped], run="Start")
# popSize10, strict OC
x1.df <- data.frame(id=as.factor(1:29), value=c(summary(x1)$solution)[grouped], run="popSize=10, StrictOC, run 1")
x2.df <- data.frame(id=as.factor(1:29), value=c(summary(x2)$solution)[grouped], run="popSize=10, StrictOC, run 2")
x3.df <- data.frame(id=as.factor(1:29), value=c(summary(x3)$solution)[grouped], run="popSize=10, StrictOC, run 3")
# popSize10, loose OC
x4.df <- data.frame(id=as.factor(1:29), value=c(summary(x4)$solution)[grouped], run="popSize=10, LooseOC, run 1")
x5.df <- data.frame(id=as.factor(1:29), value=c(summary(x5)$solution)[grouped], run="popSize=10, LooseOC, run 2")
x6.df <- data.frame(id=as.factor(1:29), value=c(summary(x6)$solution)[grouped], run="popSize=10, LooseOC, run 3")
# popSize10, looser OC
x7.df <- data.frame(id=as.factor(1:29), value=c(summary(x7)$solution)[grouped], run="popSize=10, LooserOC, run 1")
x8.df <- data.frame(id=as.factor(1:29), value=c(summary(x8)$solution)[grouped], run="popSize=10, LooserOC, run 2")
x9.df <- data.frame(id=as.factor(1:29), value=c(summary(x9)$solution)[grouped], run="popSize=10, LooserOC, run 3")
# popSize10, looser OC, observed Yuba spawners (not imputed)
x10.df <- data.frame(id=as.factor(1:29), value=c(summary(x10)$solution)[grouped], run="popSize=10, LooserOC, obs Yuba, run 1")
x11.df <- data.frame(id=as.factor(1:29), value=c(summary(x11)$solution)[grouped], run="popSize=10, LooserOC, obs Yuba, run 2")
x12.df <- data.frame(id=as.factor(1:29), value=c(summary(x12)$solution)[grouped], run="popSize=10, LooserOC, obs Yuba, run 3")
# popSize100, looser OC, observed Yuba spawners (not imputed)
x13.df <- data.frame(id=as.factor(1:29), value=c(t(summary(x13)$solution))[rep(grouped,2)], run="popSize=100, LooserOC, obs Yuba, run 1")
x14.df <- data.frame(id=as.factor(1:29), value=c(summary(x14)$solution)[grouped], run="popSize=100, LooserOC, obs Yuba, run 2")
x15.df <- data.frame(id=as.factor(1:29), value=c(summary(x15)$solution)[grouped], run="popSize=100, LooserOC, obs Yuba, run 3")
# popSize100, looser OC, observed Yuba spawners (not imputed)
x13.df <- data.frame(id=as.factor(1:29), value=c(t(summary(x13)$solution))[rep(grouped,2)], run="popSize=100, LooserOC, obs Yuba, run 1")
x14.df <- data.frame(id=as.factor(1:29), value=c(summary(x14)$solution)[grouped], run="popSize=100, LooserOC, obs Yuba, run 2")
x15.df <- data.frame(id=as.factor(1:29), value=c(summary(x15)$solution)[grouped], run="popSize=100, LooserOC, obs Yuba, run 3")
# popSize10, looser OC, observed Yuba spawners (not imputed), fixed known vs sim spawners
x16.df <- data.frame(id=as.factor(1:29), value=c(summary(x16)$solution)[grouped], run="popSize=10, LooserOC, obs Yuba fix, run 1")
x17.df <- data.frame(id=as.factor(1:29), value=c(summary(x17)$solution)[grouped], run="popSize=10, LooserOC, obs Yuba fix, run 2")
x18.df <- data.frame(id=as.factor(1:29), value=c(summary(x18)$solution)[grouped], run="popSize=10, LooserOC, obs Yuba fix, run 3")
# popSize10, looser OC, observed Yuba spawners (not imputed), fixed known vs sim spawners
x19.df <- data.frame(id=as.factor(1:29), value=c(summary(x19)$solution)[grouped], run="popSize=100, LooserOC, obs Yuba fix, run 1")
x20.df <- data.frame(id=as.factor(1:29), value=c(t(summary(x20)$solution))[rep(grouped,4)], run="popSize=100, LooserOC, obs Yuba fix, run 2")
x21.df <- data.frame(id=as.factor(1:29), value=c(summary(x21)$solution)[grouped], run="popSize=100, LooserOC, obs Yuba fix, run 3")


# Plotting parameter estimates and model fit

require(ggplot2)

# popSize=10, strict OC
x.df <- rbind(x0.df,x1.df, x2.df, x3.df)
y <- c(summary(x1)$fitness,
       summary(x2)$fitness,
       summary(x3)$fitness)
y.diff <- y-max(y)
y.df <- data.frame(run=as.factor(c("popSize=10, run 1","popSize=10, run 2",
                                   "popSize=10, run 3")),
                   fit=y.diff)

ggplot(data=x.df, aes(x=id, y=value, color=run))+
  geom_boxplot()+
  geom_vline(xintercept=seq(1.5,28.5,by=1))+
  labs(x="Parameter ID", y="Parameter Estimate")

ggplot(data=y.df, aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

# popSize=10, loose OC
x.df <- rbind(x0.df,x4.df, x5.df, x6.df)
y <- c(summary(x4)$fitness,
       summary(x5)$fitness,
       summary(x6)$fitness)
y.diff <- y-max(y)
y.df <- data.frame(run=as.factor(c("popSize=10, looseOC, run 1",
                                   "popSize=10, looseOC, run 2",
                                   "popSize=10, looseOC, run 3")),
                   fit=y.diff)

ggplot(data=x.df, aes(x=id, y=value, color=run))+
  geom_boxplot()+
  geom_vline(xintercept=seq(1.5,28.5,by=1))+
  labs(x="Parameter ID", y="Parameter Estimate")

ggplot(data=y.df, aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

# popSize=10, looser OC
x.df <- rbind(x0.df,x7.df, x8.df, x9.df)
y <- c(summary(x7)$fitness,
       summary(x8)$fitness,
       summary(x9)$fitness)
y.diff <- y-max(y)
y.df <- data.frame(run=as.factor(c("popSize=10, looserOC, run 1",
                                   "popSize=10, looserOC, run 2",
                                   "popSize=10, looserOC, run 3")),
                   fit=y.diff)

ggplot(data=x.df, aes(x=id, y=value, color=run))+
  geom_boxplot()+
  geom_vline(xintercept=seq(1.5,28.5,by=1))+
  labs(x="Parameter ID", y="Parameter Estimate")

ggplot(data=y.df, aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

# popSize=10, looser OC, observed Yuba counts
x.df <- rbind(x0.df,x10.df, x11.df, x12.df)
y <- c(summary(x10)$fitness,
       summary(x11)$fitness,
       summary(x12)$fitness)
y.diff <- y-max(y)
y.df <- data.frame(run=as.factor(c("popSize=10, looserOC, obs Yuba, run 1",
                                   "popSize=10, looserOC, obs Yuba, run 2",
                                   "popSize=10, looserOC, obs Yuba, run 3")),
                   fit=y.diff)

ggplot(data=x.df, aes(x=id, y=value, color=run))+
  geom_boxplot()+
  geom_vline(xintercept=seq(1.5,28.5,by=1))+
  labs(x="Parameter ID", y="Parameter Estimate")

ggplot(data=y.df, aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

# popSize=100, looser OC, observed Yuba counts
x.df <- rbind(x0.df,x13.df, x14.df, x15.df)
y <- c(summary(x13)$fitness,
       summary(x14)$fitness,
       summary(x15)$fitness)
y.diff <- y-max(y)
y.df <- data.frame(run=as.factor(c("popSize=100, looserOC, obs Yuba, run 1",
                                   "popSize=100, looserOC, obs Yuba, run 2",
                                   "popSize=100, looserOC, obs Yuba, run 3")),
                   fit=y.diff)

ggplot(data=x.df, aes(x=id, y=value, color=run))+
  geom_boxplot()+
  geom_vline(xintercept=seq(1.5,28.5,by=1))+
  labs(x="Parameter ID", y="Parameter Estimate")

ggplot(data=y.df, aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

# popSize=10, looser OC, observed Yuba counts, fixed obs vs sim spawners
x.df <- rbind(x0.df,x16.df, x17.df, x18.df)
y <- c(summary(x16)$fitness,
       summary(x17)$fitness,
       summary(x18)$fitness)
y.diff <- y-max(y)
y.df <- data.frame(run=as.factor(c("popSize=10, looserOC, obs Yuba fix, run 1",
                                   "popSize=10, looserOC, obs Yuba fix, run 2",
                                   "popSize=10, looserOC, obs Yuba fix, run 3")),
                   fit=y.diff)

ggplot(data=x.df, aes(x=id, y=value, color=run))+
  geom_boxplot()+
  geom_vline(xintercept=seq(1.5,28.5,by=1))+
  labs(x="Parameter ID", y="Parameter Estimate")

ggplot(data=y.df, aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

# popSize=100, looser OC, observed Yuba counts, fixed obs vs sim spawners
x.df <- rbind(x0.df,x19.df, x20.df, x21.df)
y <- c(summary(x19)$fitness,
       summary(x20)$fitness,
       summary(x21)$fitness)
y.diff <- y-max(y)
y.df <- data.frame(run=as.factor(c("popSize=100, looserOC, obs Yuba fix, run 1",
                                   "popSize=100, looserOC, obs Yuba fix, run 2",
                                   "popSize=100, looserOC, obs Yuba fix, run 3")),
                   fit=y.diff)

ggplot(data=x.df, aes(x=id, y=value, color=run))+
  geom_boxplot()+
  geom_vline(xintercept=seq(1.5,28.5,by=1))+
  labs(x="Parameter ID", y="Parameter Estimate")

ggplot(data=y.df, aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

# Plotting model fit to time series
  # Visualizing, summarizing model fit

# Make sure we have necessary reference objects available
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
track.sheds <- c(2,3,6,7,10,12,19,20)
known.Adults <- DSMCalibrationData::grandtab_observed$spring # NAs for missing values
known.Adults <- ifelse(known.Adults<100,NA,known.Adults)
sugg1<-c(-2.25097915,-2.31387713,1.86641130,
         -0.55423070,-3.49999933,-2.56726844,
         -2.52403823,1.42642277,-2.79409568,
         2.04438137,0.99985602,-2.89141694,
         0.75077947,-3.10495582,2.91518569,
         -3.49954625,-1.49855839,-3.22990407,
         2.49974122,-2.96201071,0.09999992,
         0.01000010,0.19126503,0.61104442,
         -0.70866158,2.10420292,-2.59452699,
         2.79189516,-1.53805220)
grouped <- c(1:6,25,26,28,7:20,29,27,21:24)

# popSize=10, strict OC (run 2 had best fit)
x <- summary(readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_StrictOC_1688407679.49013.rds"))$solution[1,]

# popSize=10, loose OC (run 3 had best fit)
x <- summary(readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looseOC_1688602352.56247.rds"))$solution[1,]

# popSize=10, looser OC (run 1 had best fit)
x <- summary(readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_1688666392.085.rds"))$solution[1,]

# popSize=10, looser OC, obs. Yuba (run X had best fit)
x <- summary(readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_obsYuba_1688670807.69497.rds"))$solution[1,]

# popSize=100, looser OC, obs. Yuba (run 2 had best fit)
x <- summary(readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize100_looserOC_obsYuba_1689130505.78926.rds"))$solution[1,]
x; x[grouped]

# popSize=10, looser OC, obs. Yuba fix (run 3 had best fit)
x <- summary(readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize10_looserOC_obsYubafix_1691630718.80601.rds"))$solution[1,]

# popSize=100, looser OC, obs. Yuba fix (run 3 had best fit)
  # * These are the final model results! * #
x <- summary(readRDS("./springRunDSM-Main_Calibration/Output/Calib Output_popSize100_looserOC_obsYubafix_1691808930.96922.rds"))$solution[1,]
x; c(x[grouped])

y <- life_cycle_model_calib_obsYuba_fixed_visual(x[1],x[2],x[3],x[4],
                                   x[5],x[6],x[7],x[8],
                                   x[9],x[10],x[11],x[12],
                                   x[13],x[14],x[15],x[16],
                                   x[17],x[18],x[19],x[20],
                                   x[21],x[22],x[23],x[24],
                                   x[25],x[26],x[27],x[28],
                                   x[29])
y2 <- life_cycle_model_calib_obsYuba_fixed_visual(sugg1[1],sugg1[2],sugg1[3],sugg1[4],
                                    sugg1[5],sugg1[6],sugg1[7],sugg1[8],
                                    sugg1[9],sugg1[10],sugg1[11],sugg1[12],
                                    sugg1[13],sugg1[14],sugg1[15],sugg1[16],
                                    sugg1[17],sugg1[18],sugg1[19],sugg1[20],
                                    sugg1[21],sugg1[22],sugg1[23],sugg1[24],
                                    sugg1[25],sugg1[26],sugg1[27],sugg1[28],
                                    sugg1[29])

# Remove spawner estimates in which observed spawner numbers <100 or missing (
  # NAs in known.Adults
#id.nas <- which(is.na(known.Adults)==TRUE)
y[[1]] <- ifelse(is.na(known.Adults),NA,y[[1]])
y[[2]] <- ifelse(is.na(known.Adults),NA,y[[2]])
y2[[2]] <- ifelse(is.na(known.Adults),NA,y2[[2]])

# Bring in tidyverse
require(tidyverse)

# Use tricks to convert matrix of simulated/known values into data.frames
known.spawners <- y[[1]] %>% reshape2::melt(varnames=c("Watershed","Year")) %>%
  filter(Watershed %in% watershed.names[track.sheds]) %>%
  mutate(Type="Known Spawners")
est.spawners.new <- y[[2]] %>% reshape2::melt(varnames=c("Watershed","Year")) %>%
  filter(Watershed %in% watershed.names[track.sheds]) %>%
  mutate(Type="Estimated Spawners-New")
est.spawners.old <- y2[[2]] %>% reshape2::melt(varnames=c("Watershed","Year")) %>%
  filter(Watershed %in% watershed.names[track.sheds]) %>%
  mutate(Type="Estimated Spawners-Old")

# Combine all estimates
z <- rbind(known.spawners, est.spawners.new, est.spawners.old) 

# Plot
ggplot(z, aes(y=value, x=Year, color=Type))+
  geom_line()+
  labs(title="",y="Spawners")+
  facet_wrap(~Watershed)
R2_new <- numeric(8); R2_old <- numeric(8)
for(i in 1:8){
  R2_new[i] <- cor(y[[1]][track.sheds[i],1:14],y[[2]][track.sheds[i],1:14], use="pairwise.complete.obs")^2
  R2_old[i] <- cor(y[[1]][track.sheds[i],1:14],y2[[2]][track.sheds[i],1:14], use="pairwise.complete.obs")^2
}
R2_new; R2_old
  # Overall correlation
'/
known_all <- c(y[[1]][track.sheds[1],1:14],y[[1]][track.sheds[2],1:14],
               y[[1]][track.sheds[3],1:14],y[[1]][track.sheds[4],1:14],
               y[[1]][track.sheds[5],1:14],y[[1]][track.sheds[6],1:14],
               y[[1]][track.sheds[7],1:14],y[[1]][track.sheds[8],1:14])
est_new_all <- c(y[[2]][track.sheds[1],1:14],y[[2]][track.sheds[2],1:14],
               y[[2]][track.sheds[3],1:14],y[[2]][track.sheds[4],1:14],
               y[[2]][track.sheds[5],1:14],y[[2]][track.sheds[6],1:14],
               y[[2]][track.sheds[7],1:14],y[[2]][track.sheds[8],1:14])
cor(known_all, est_new_all, use="pairwise.complete") # Cor = 0.751 (popSize100)
'

# Modify estimates for scatterplotting
zz <- z %>% 
  pivot_wider(names_from=Type, values_from=value) %>%
  filter(Year %in% 1998:2011)
ggplot(zz, aes(y=`Known Spawners`, x=`Estimated Spawners-New`))+
  geom_point()+
  labs(title="")+
  coord_equal(xlim=c(0,16000),ylim=c(0,16000))+
  geom_abline(slope=1,intercep=0)
ggplot(zz, aes(y=`Known Spawners`, x=`Estimated Spawners-New`, col=Watershed))+
  geom_point()+
  labs(title="")+
  coord_equal(xlim=c(0,16000),ylim=c(0,16000))+
  geom_abline(slope=1,intercep=0)
ggplot(zz, aes(y=`Known Spawners`, x=`Estimated Spawners-New`))+
  geom_point()+
  labs(title="")+
  coord_equal(xlim=c(0,16000),ylim=c(0,16000))+
  geom_abline(slope=1,intercep=0)+
  facet_wrap(~Watershed)

# Correlation all spawners
  # Cor = 0.763 (popSize100)
cor(zz$`Known Spawners`, zz$`Estimated Spawners-New`, use="pairwise.complete")
# Correlation no Feather River
  # Cor = 0.847
zz2 <- zz %>% filter(Watershed %in% watershed.names[c(2,3,6,7,10,12,20)])
cor(zz2$`Known Spawners`, zz2$`Estimated Spawners-New`, use="pairwise.complete")
# Correlation no Feather River or Butte Creek
  # Cor = 0.516
zz3 <- zz %>% filter(Watershed %in% watershed.names[c(2,3,7,10,12,20)])
cor(zz3$`Known Spawners`, zz3$`Estimated Spawners-New`, use="pairwise.complete")

  # Scatterplot, no Feather River or Butte Creek
ggplot(zz3, aes(y=`Known Spawners`, x=`Estimated Spawners-New`, col=Watershed))+
  geom_point()+
  labs(title="")+
  coord_equal(xlim=c(0,5000),ylim=c(0,5000))+
  geom_abline(slope=1,intercep=0)
