
###############################################################################
##    Winter CHINOOK SALMON SCIENCE INTEGRATION TEAM MODEL December 2019     ##
##      Primary Authors:                                                     ##
##                                                                           ##
##      James T. Peterson                                                    ##
##      U.S. Geological Survey, Oregon Cooperative Fish and Wildlife         ##
##      Research Unit, Oregon State University                               ##
##      Corvallis, Oregon 97331-3803, jt.peterson@oregonstate.edu            ##
##                                                                           ##
##      Adam Duarte                                                          ##
##      Oregon Cooperative Fish and Wildlife Research Unit,                  ##
##      Oregon State University,                                             ##
##      Corvallis, Oregon 97331-3803, adam.duarte@oregonstate.edu            ##
##                                                                           ##
##     Although this software program has been used by                       ##
##     the U.S. Geological Survey (USGS), no warranty, expressed             ##
##     or implied, is made by the USGS or the U.S. Government as to          ##
##     the accuracy and functioning of the program and related program       ##
##     material nor shall the fact of distribution constitute any            ##
##     such warranty, and no responsibility is assumed by the USGS           ##
##     in connection therewith.                                              ##
##                                                                           ##
##    IP-117068                                                              ##
##                                                                           ##
###############################################################################


####################################################################
#### NEW NEW NEW
##### Ocean entry survival
mo.ocean.surv<-matrix(c(1,0.00154275,0.00157200,0.00165275,0.00181575,
                        2,0.00471025,0.00479900,0.00504425,0.00553875,
                        3,0.01210450,0.01233050,0.01295575,0.01421550,
                        4,0.01361600,0.01387050,0.01457250,0.01598750,
                        5,0.00795950,0.00810925,0.00852175,0.00935475,
                        6,0.01004325,0.01023150,0.01075100,0.01179850,
                        7,0.03255950,0.03315450,0.03479450,0.03808850), byrow=T,ncol = 5)

#### OCEAN ENTRY SURVIVAL mnth is month of simulation
#### T.month is month of ocean transition randomly generated each year, see below 
Ocean.ent.surv<- function(migrants,mnth,T.mo, mo.ocean.surv,int,stoch,vary,pctil){
  B0<- rep(int,dim(migrants)[1])#-2.472915
  if(sum(vary == "juv.oceanS.int")) B0<- B0*pctil[vary == "juv.oceanS.int"]
  monthz<- 0.35;  if(sum(vary == "juv.oceanS.monthz")) monthz<- monthz*pctil[vary == "juv.oceanS.monthz"]
  len<-c(-7.4775822,	-5.9142029,	-1.7382561,	6.1051743)*0.012
  if(sum(vary == "juv.oceanS.size")) len<- len*pctil[vary == "juv.oceanS.size"]
  drain<- c(0,0,0,0)
  if(mnth<=T.mo) mo.since = 0 else mo.since <- max(1,mnth-T.mo)
  
  SS<-NULL
  for(ii in 1:dim(migrants)[1]){
    SS1<-inv.logit(B0[ii] + monthz*mo.since + len)  
    SS<-rbind(SS,SS1)
  }
  
  XXXX<-dim(migrants)
  if(max(migrants)<=1000000000 & stoch==1){
    junk<-matrix(rbinom((XXXX[1]*XXXX[2]),prob=as.vector(t(matrix(rep(drain,XXXX[1]),ncol=4,byrow=TRUE))),size=round(as.vector(t(migrants)))),ncol=4,byrow=TRUE)
  }else{junk<-round(t(drain*t(migrants)))}
  out<-junk*stoch+(t(drain*t(migrants)))*(1-stoch)
  
  
  XXXX2<-dim(migrants)
  if(max(migrants)<=1000000000 & stoch==1){
    junk2<-matrix(rbinom((XXXX2[1]*XXXX2[2]),prob=as.vector(t(SS)),size=round(as.vector(t(migrants)))),ncol=4,byrow=TRUE)
  }else{junk2<-round(SS*migrants)}
  out2<-junk2*stoch+(SS*migrants)*(1-stoch)
  
  
  if (mo.since == 0) mo.adult<-rowSums(out) else mo.adult<-rowSums(out2)
  return(mo.adult)
  
}


#Ocean.ent.surv(migrants,mnth= 3,T.mo= 2, mo.ocean.surv)
