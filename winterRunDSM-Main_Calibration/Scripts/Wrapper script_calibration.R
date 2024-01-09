#####################
# Annotation
  # This wrapper script is based on WinterRunStochasticDSM_11_13_2019CALIBRATION.R
    # Located in the winterRunDSM-Main repository, obtained from Peterson and Duarte 2020 authors
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
    # I moved the run_scenarios() function (renamed life_cycle_model_calibration_allyears(), 
      # life_cycle_model_calibration_allyears_visual()) into its own scripts
    # (located in 'functions' sub-folder) and call it in this script
  # Output from calibrations is saved as an .rds file in the 'Output' sub-folder
  # Changed method of calibration to match FlowWest documentation
  # Conducts summary plotting of parameter estiamtes and model fit
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

fun.place<-paste0("./winterRunDSM-Main_Calibration/functions")
functs<-dir(path=fun.place,full.names=FALSE,pattern=".R",recursive=FALSE)
for(ia in 1:length(functs)) source(paste(fun.place,functs[ia],sep="/"))

# Read in data

calib_data<-readRDS("./winterRunDSM-Main_Calibration/delta-calibration-1980_2017.rds")

all_inputs<-cvpiaCalibration::winter_inputs #for calibration (1998-2017)
list2env(all_inputs,.GlobalEnv)

known.Adults <- DSMCalibrationData::grandtab_observed$winter
known.Adults[3,] <- 0 # set Battle Creek abundances to 0
known.Adults.seed<-known.Adults
# v2019 original calibration used a "Calibration_WinterRunAdults.csv" file for this
# ** I assume these are identical, but need to double check **

known.Adults.ref<-read.csv("winterRunDSM-main_Calibration/SimulaitonSeed_WinterRunAdults.csv") #Winter run GrandTab data
known.Adults.ref<-known.Adults.ref[order(known.Adults.ref$order),]


# Modify data

inps$prop.nat.remov[1]<-mean(c(0.18,0.09,0.07,0.13,0.02,0.03)) # from Doug Killam 2012 - 2017 data
inps$A.HARV[1]<-0.2 # from Corey Phillis

prop.hatch<-0.1759966 #proportion hatchery based on CWT reports

maxHabitat<-read.csv("./winterRunDSM-Main_Calibration/theoretical_max_area.csv")
table(maxHabitat$species)
maxHabitat<-maxHabitat[which(maxHabitat$species=="fr"),]
maxHabitat<-maxHabitat[-1*(c(which(maxHabitat$watershed=="North Delta"),which(maxHabitat$watershed=="South Delta"))),]
maxSpwnHab<-maxHabitat[which(maxHabitat$lifestage=="spawning"),c(1,4)]
maxSpwnHab<-merge(known.Adults.ref[,1:2],maxSpwnHab,all=TRUE)
maxSpwnHab<-maxSpwnHab[order(maxSpwnHab$order),]
maxRearHab<-maxHabitat[which(maxHabitat$lifestage=="rearing"),c(1,4)]
maxRearHab<-merge(known.Adults.ref[,1:2],maxRearHab,all=TRUE)
maxRearHab<-maxRearHab[order(maxRearHab$order),]

aveT201<-aveT20
aveT20D1<-aveT20D
ptemp20mc<-p.tempMC20

#calculate total habitat for sutter and yolo
IChab.sutter<-IChab.bypass[1,,]+IChab.bypass[2,,]+IChab.bypass[3,,]+IChab.bypass[4,,]
IChab.yolo<-IChab.bypass[5,,]+IChab.bypass[6,,]
floodp.sutter<-floodp.bypass[1,,]+floodp.bypass[2,,]+floodp.bypass[3,,]+floodp.bypass[4,,]
floodp.yolo<-floodp.bypass[5,,]+floodp.bypass[6,,]

#fix index when running the model
Q_free<-calib_data$Q_free[,19:38]   
Q_vern<-calib_data$Q_vern[,19:38]   
Q_stck<-calib_data$Q_stck[,19:38]      
Temp_vern<-calib_data$Temp_vern[,19:38]   
# Temp_vern<-fix.tmp(Temp_vern)
Temp_pp<-calib_data$Temp_pp[,19:38]  
# Temp_pp<-fix.tmp(Temp_pp)   
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

Q_free[is.na(Q_free)]<-0
Q_vern[is.na(Q_vern)]<-0
Q_stck[is.na(Q_stck)]<-0
Temp_vern[is.na(Temp_vern)]<-0  
Temp_pp[is.na(Temp_pp)]<-0
CVP_exp[is.na(CVP_exp)]<-0
SWP_exp[is.na(SWP_exp)]<-0

regulated<-c(1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,1,1,1,0,1,1,1,0,1,1,1,1,1)

states<-data.frame(known.Adults.ref$watershed,known.Adults.ref$order,regulated)
colnames(states)<-c("watershed","order","regulated")
groups<-read.csv("./winterRunDSM-Main_Calibration/Grouping.csv")
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

floodPNew<-rep(0,31)

winterRunStream<-data.frame(known.Adults.ref$watershed,c(1,rep(0,14),1,rep(0,4),1,rep(0,2),1,rep(0,7)))
colnames(winterRunStream)<-c("watershed","winterRunLoc")

# Replace t.diver[1,,] (Upper Sacramento River) with correct diversion values for calibration purposes
synth.t.diver <- readRDS("./winterRunDSM-Main_Calibration/New t.diver data for calibration/synth.t.diver.rds")
t.diver[1,,] <- synth.t.diver

############
# test model
############

life_cycle_model_calib(-0.6558315, -3.4999845,  1.4933417, -3.0188308,  2.0000003,  0.7999889, -3.5000000, -0.1999996,
                       -3.4999920, -2.9839253,  3.4999976,  0.6466230,  0.0194795,  0.1000000,  0.3000000,  0.4820249)
  # Calibration fitting value with default params: 228984717

life_cycle_model_calib_allyears(-0.6558315, -3.4999845,  1.4933417, -3.0188308,  2.0000003,  0.7999889, -3.5000000, -0.1999996,
                       -3.4999920, -2.9839253,  3.4999976,  0.6466230,  0.0194795,  0.1000000,  0.3000000,  0.4820249)
  # Model fit with default params: 246735021

#########################
# Set up, run calibration
#########################

# set bounds to calibrate intercepts for survival
  # default values, based on FlowWest recommendations
#minz <- c(rep(-3.5,10),0,rep(-3.5,5)) # set 0 for lower bound for en route survival [11]
#maxz <- rep(3.5,16)
  # modified default values, forcing some expected covariate coefficients to be positive
minz <- c(rep(-3.5,10),0,-3.5,rep(0,4)) # set 0 for lower bound for en route survival [11]
maxz <- c(rep(3.5,9),-1,rep(3.5,6))
  # -2 upper bound for marine survival = OC
  # -1 upper bound for marine survival = LooseOC
  # values used in Adam's script
#minz<-c(-3.5,-3.5,1,-3.5,2,-0.2,-3.5,-0.2,-3.5,-3.5,2.5,0.5,0.01,0.01,0.1,0.1)
#maxz<-c( 3.5,-2.5,2,-2.5,3,0.8,-2.5,0.8,-2.5,2.5,3.5,0.999,0.1,0.1,0.3,1)

# specify GA target size
ps <- 100 # 150 recommend by FlowWest
  # ps=10, ~30 min for convergence

# set initial values
  # These are the current parameter values in the winter-run DSM
sugg1<-c(-0.6558315, -3.4999845,  1.4933417, -3.0188308,  2.0000003,  0.7999889, -3.5000000, -0.1999996,
        -3.4999920, -2.9839253,  3.4999976,  0.6466230,  0.0194795,  0.1000000,  0.3000000,  0.4820249)
sugg <- matrix(sugg1, nrow=ps, ncol=16, byrow=TRUE)

# Run calibration
res <- ga(type="real-valued",
          fitness=
            function(x) -life_cycle_model_calib_allyears(
              x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],
              x[9],x[10],x[11],x[12],x[13],x[14],x[15],x[16]
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
Sys.time() # started this run at 6:36 PM
saveRDS(res, paste0("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize",ps,"_LooseOC_long_time",as.numeric(Sys.time()),".rds"))

#-171223862 (decent but not great fit to available data)
# Check calibration fit

# popSize=10, no informed marine survival constraints
x1 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize10_time1687393263.55645.rds")
x2 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize10_time1687389435.15028.rds")
x3 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize10_time1687395949.77712.rds")

# popSize=100, no informed marine survival constraints
x4 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_time1687401569.13949.rds")
x5 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_time1687481798.50559.rds")

# popSize=100, informed marine survival constraints [-3.5, -2]
x6 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_OC_time1687552152.82004.rds")
x7 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_OC_time1687558887.76344.rds")
x8 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_OC_time1687853325.02821.rds")

# Long time series (1998-2016), popSize=100, informed marine survival constraints [-3.5, -2]
x9 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_OC_long_time1687571980.16226.rds")
x10 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_OC_long_time1687805231.91924.rds")
x11 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_OC_long_time1687833794.48462.rds")

# Long time series (1998-2016), popSize=100, looser marine survival constraints [-3.5, -1]
x12 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_LooseOC_long_time1688756969.5581.rds")
x13 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_LooseOC_long_time1688699789.43093.rds")
x14 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_LooseOC_long_time1688776261.69249.rds")


x0.df <- data.frame(id=as.factor(1:16), value=sugg1, run="Start")
# Short, popSize10, no OC
x1.df <- data.frame(id=as.factor(1:16), value=c(summary(x1)$solution), run="popSize=10, run 1")
x2.df <- data.frame(id=as.factor(1:16), value=c(summary(x2)$solution), run="popSize=10, run 2")
x3.df <- data.frame(id=as.factor(1:16), value=c(summary(x3)$solution), run="popSize=10, run 3")
# Short, popSize100, no OC
x4.df <- data.frame(id=rep(as.factor(1:16),4), value=c(t(summary(x4)$solution)), run="popSize=100, run 1") # 4 sets of solutions
x5.df <- data.frame(id=rep(as.factor(1:16),1), value=c(t(summary(x5)$solution)), run="popSize=100, run 2") 
# Short, popSize100, OC
x6.df <- data.frame(id=rep(as.factor(1:16),4), value=c(t(summary(x6)$solution)), run="popSize=100, OC, run 1") # 4 sets of solutions
x7.df <- data.frame(id=rep(as.factor(1:16),1), value=c(t(summary(x7)$solution)), run="popSize=100, OC, run 2") 
x8.df <- data.frame(id=rep(as.factor(1:16),5), value=c(t(summary(x8)$solution)), run="popSize=100, OC, run 3") 
# Long, popSize100, OC
x9.df <- data.frame(id=rep(as.factor(1:16),1), value=c(t(summary(x9)$solution)), run="Long, popSize=100, OC, run 1") 
x10.df <- data.frame(id=rep(as.factor(1:16),1), value=c(t(summary(x10)$solution)), run="Long, popSize=100, OC, run 2") 
x11.df <- data.frame(id=rep(as.factor(1:16),3), value=c(t(summary(x11)$solution)), run="Long, popSize=100, OC, run 3") # 3 sets of solutions
# Long, popSize100, LooseOC
x12.df <- data.frame(id=rep(as.factor(1:16),1), value=c(t(summary(x12)$solution)), run="Long, popSize=100, LooseOC, run 1") 
x13.df <- data.frame(id=rep(as.factor(1:16),1), value=c(t(summary(x13)$solution)), run="Long, popSize=100, LooseOC, run 2") 
x14.df <- data.frame(id=rep(as.factor(1:16),2), value=c(t(summary(x14)$solution)), run="Long, popSize=100, LooseOC, run 3") # 3 sets of solutions

# Short, no OC
x.df <- rbind(x0.df,x1.df, x2.df, x3.df, x4.df, x5.df)
y <- c(summary(x1)$fitness,
       summary(x2)$fitness,
       summary(x3)$fitness,
       summary(x4)$fitness,
       summary(x5)$fitness)
y.diff <- y-max(y)
y.df <- data.frame(run=as.factor(c("popSize=10, run 1","popSize=10, run 2",
                                   "popSize=10, run 3","popSize=100, run 1",
                                   "popSize=100, run 2")),
                   fit=y.diff)

require(ggplot2)

ggplot(data=x.df, aes(x=id, y=value, color=run))+
  geom_boxplot()+
  labs(x="Parameter ID", y="Parameter Estimate")

ggplot(data=y.df, aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

ggplot(data=y.df[-5,], aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")


# OC, short
x.df2 <- rbind(x0.df, x6.df, x7.df, x8.df)
y2 <- c(summary(x6)$fitness,
       summary(x7)$fitness,
       summary(x8)$fitness)
y2.diff <- y2-max(y2)
y2.df <- data.frame(run=as.factor(c("popSize=100, OC, run 1",
                                    "popSize=100, OC, run 2",
                                    "popSize=100, OC, run 3")),
                   fit=y2.diff)

require(ggplot2)

ggplot(data=x.df2, aes(x=id, y=value, color=run))+
  geom_boxplot()+
  geom_vline(xintercept=seq(1.5,15.5,by=1))+
  labs(x="Parameter ID", y="Parameter Estimate")

ggplot(data=y2.df, aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

ggplot(data=y2.df[-2,], aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

# Long, OC
x.df2 <- rbind(x0.df, x9.df, x10.df, x11.df)
y2 <- c(summary(x9)$fitness,
        summary(x10)$fitness,
        summary(x11)$fitness)
y2.diff <- y2-max(y2)
y2.df <- data.frame(run=as.factor(c("Long, popSize=100, OC, run 1",
                                    "Long, popSize=100, OC, run 2",
                                    "Long, popSize=100, OC, run 3")),
                    fit=y2.diff)

require(ggplot2)

ggplot(data=x.df2, aes(x=id, y=value, color=run))+
  geom_boxplot()+
  geom_vline(xintercept=seq(1.5,15.5,by=1))+
  labs(x="Parameter ID", y="Parameter Estimate")

ggplot(data=y2.df, aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

ggplot(data=y2.df[-3,], aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

# Long, LooseOC
x.df2 <- rbind(x0.df, x12.df, x13.df, x14.df)
y2 <- c(summary(x12)$fitness,
        summary(x13)$fitness,
        summary(x14)$fitness)
y2.diff <- y2-max(y2)
y2.df <- data.frame(run=as.factor(c("Long, popSize=100, LooseOC, run 1",
                                    "Long, popSize=100, LooseOC, run 2",
                                    "Long, popSize=100, LooseOC, run 3")),
                    fit=y2.diff)

require(ggplot2)

ggplot(data=x.df2, aes(x=id, y=value, color=run))+
  geom_boxplot()+
  geom_vline(xintercept=seq(1.5,15.5,by=1))+
  labs(x="Parameter ID", y="Parameter Estimate")

ggplot(data=y2.df, aes(y=fit, x=run))+
  geom_col()+
  labs(x="Run ID", y="Difference in Model Fit from Best (0=Best)")

# Visualizing, summarizing model fit

x <- summary(x7)$solution[1,]
y <- life_cycle_model_calib_visual(x[1],x[2],x[3],x[4],
                                   x[5],x[6],x[7],x[8],
                                   x[9],x[10],x[11],x[12],
                                   x[13],x[14],x[15],x[16])
y2 <- life_cycle_model_calib_visual(sugg1[1],sugg1[2],sugg1[3],sugg1[4],
                                    sugg1[5],sugg1[6],sugg1[7],sugg1[8],
                                    sugg1[9],sugg1[10],sugg1[11],sugg1[12],
                                    sugg1[13],sugg1[14],sugg1[15],sugg1[16])
z <- data.frame(Year=1998:2010,
                Type=c(rep("Known Spawners",13),
                       rep("Estimated Spawners-New",13),
                       rep("Estimated Spawners-Old",13)),
                Spawners=c(y[[1]][1:13],y[[2]][1:13],y2[[2]][1:13]))
ggplot(z, aes(y=Spawners, x=Year, color=Type))+
  geom_line()+
  labs(title="Short Time Series Calibration")
cor(y[[1]][1:13],y[[2]][1:13])^2 # R2=0.053
cor(y[[1]][1:13],y2[[2]][1:13])^2 # R2=0.010
z <- data.frame(Year=1998:2010,
                `Known Spawners`=y[[1]][1:13],
                `Estimated Spawners`=y[[2]][1:13])
ggplot(z, aes(y=`Known.Spawners`, x=`Estimated.Spawners`))+
  geom_point()+
  labs(title="Short Time Series Calibration")+
  coord_equal(xlim=c(0,16000),ylim=c(0,16000))+
  geom_abline(slope=1,intercep=0)

# Long time series
  # * This is the final selected set of parameters! * #
x11 <- readRDS("./winterRunDSM-Main_Calibration/Output/Calib Output_popSize100_OC_long_time1687833794.48462.rds")
x <- summary(x11)$solution[1,]
y <- life_cycle_model_calib_allyears_visual(x[1],x[2],x[3],x[4],
                                       x[5],x[6],x[7],x[8],
                                       x[9],x[10],x[11],x[12],
                                       x[13],x[14],x[15],x[16])
y2 <- life_cycle_model_calib_allyears_visual(sugg1[1],sugg1[2],sugg1[3],sugg1[4],
                                            sugg1[5],sugg1[6],sugg1[7],sugg1[8],
                                            sugg1[9],sugg1[10],sugg1[11],sugg1[12],
                                            sugg1[13],sugg1[14],sugg1[15],sugg1[16])
z <- data.frame(Year=1998:2016,
                Type=c(rep("Known Spawners",19),
                       rep("Estimated Spawners-New",19),
                       rep("Estimated Spawners-Old",19)),
                Spawners=c(y[[1]][1:19],y[[2]][1:19],y2[[2]][1:19]))
ggplot(z, aes(y=Spawners, x=Year, color=Type))+
  geom_line()+
  labs(title="Long Time Series Calibration")
z <- data.frame(Year=1998:2016,
                `Known Spawners`=y[[1]][1:19],
                `Estimated Spawners`=y[[2]][1:19])
ggplot(z, aes(y=`Known.Spawners`, x=`Estimated.Spawners`))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(title="Long Time Series Calibration")+
  coord_equal(xlim=c(0,16000),ylim=c(0,16000))+
  geom_abline(slope=1,intercept=0)

cor(y[[1]][1:19],y[[2]][1:19])^2 # R2=0.188, R=0.434
cor(y[[1]][1:19],y2[[2]][1:19])^2 # R2=0.071, R=0.266

