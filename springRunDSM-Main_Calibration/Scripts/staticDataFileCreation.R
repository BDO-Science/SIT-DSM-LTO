#######################
# Annotation
  # This code is used to create the watershedInfo.csv file and double check that it is created correctly
  # It is not necessary to re-run this code, as the correct watershedInfo.csv is already provided in the repository
#######################

calib_data<-readRDS(here("springRunDSM-main", "delta-calibration-1980_2017.rds"))

all_inputs<-cvpiaData::load_baseline_data('spring')
list2env(all_inputs,.GlobalEnv)



#Watershed Info
regulated<-c(1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,1,1,1,0,1,1,1,0,1,1,1,1,1) # * new regulated info - unique to SR
regulated[c(16,17,21,22,24,31)]<-0 # * assume mainstem and bypass habitats don't degrade for this - unique to SR

known.Adults<-read.csv("springRunDSM-main/SimulaitonSeed_SpringRunAdults.csv") #Spring run GrandTab data
known.Adults<-known.Adults[order(known.Adults$order),]
states<-data.frame(known.Adults$watershed,known.Adults$order,regulated)
colnames(states)<-c("watershed","order","regulated")
groups<-read.csv(here("springRunDSM-main", "Grouping.csv"))
states<-merge(states,groups)
states<-states[order(states$order),]
states$grp<-ifelse(states$grp==9,7,groups$grp) #all mainstem sections are now group 7
states$DiversityGroup[c(18,19,20,23,26,27)]<-5 #split diversity group
states$DiversityGroup<-ifelse(states$grp==7,6,states$DiversityGroup) #all mainstem sections are now diversity group 6

states$spwnDecay<-ifelse(states$grp==1,0.973647181,ifelse(states$grp==2,0.994949237,ifelse(states$grp==3,0.994949237,
                                                                                           ifelse(states$grp==4,0.979148362,ifelse(states$grp==5,0.962256359,0.989793782)))))
states$rearDecay<-ifelse(states$grp==1,0.987175243,ifelse(states$grp==2,0.997487405,ifelse(states$grp==3,0.997487405,
                                                                                           ifelse(states$grp==4,0.989793782,ifelse(states$grp==5,0.981853233,0.994949237)))))

hatcheryStream<-data.frame(known.Adults$watershed,c(rep(0,2),1,rep(0,15),1,rep(0,3),1,rep(0,3),1,1,rep(0,3)))
colnames(hatcheryStream)<-c("watershed","hatchery")

# *** Following 2 lines are unique to SpringRun ***
states$srpop<-rep(0,dim(states)[1])
states$srpop[c(2,3,6,7,10,12,19,20)]<-1 #these are where spring run are

# The two lines below are not used in springRun
'/
winterRunStream<-data.frame(known.Adults$watershed,c(1,rep(0,14),1,rep(0,4),1,rep(0,2),1,rep(0,7)))
colnames(winterRunStream)<-c("watershed","winterRunLoc")
'

watershedInfo <- merge(states, hatcheryStream, by = "watershed")
# watershedInfo <- merge(watershedInfo, winterRunStream, by = "watershed") # * not needed for spring-run
watershedInfo <- watershedInfo[order(watershedInfo$order),]

write.csv(watershedInfo, here("springRunDSM-main", "watershedInfo.csv"), row.names = FALSE)







