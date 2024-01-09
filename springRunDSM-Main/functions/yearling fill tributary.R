
###############################################################################
##    Spring CHINOOK SALMON SCIENCE INTEGRATION TEAM MODEL December 2019     ##
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
fill.trib.yearling.func<-function(juvs,flood_hab,juv_hab,territory_size,flood_surv,river_surv,trans_mat_river,trans_mat_flood,stoch){
  territory_size[4]<-territory_size[3]
  xxx<-nrow(juvs)

  #Holding vectors
  flood_rear<-matrix(rep(0,4),ncol=4,nrow=xxx)
  river_rear<-matrix(rep(0,4),ncol=4,nrow=xxx)
  deaths<-matrix(rep(0,4),ncol=4,nrow= xxx)
  spaces<-dim(river_rear)[1]*dim(river_rear)[2]
  
  #Assigning ALL individuals to flood habitat largest first 
  flood_avail<-flood_hab
  if(stoch==0){
    for(r in 1:xxx){
      for(i in 4:1){
        flood_rear[r,i]<-min((flood_avail[r]/territory_size[i]),juvs[r,i])
        flood_avail[r]<-max(flood_avail[r]-flood_rear[r,i]*territory_size[i],0)
      }}
    flood_rear<-pmax(flood_rear,0)
    #Who's left for in-river rearing
    juvs<-pmax(juvs-flood_rear,0)
    #In-river rearing
    rear_avail<-juv_hab
    for(r in 1:xxx){
      for(i in 4:1){
        river_rear[r,i]<-min((rear_avail[r]/territory_size[i]),juvs[r,i])
        rear_avail[r]<-max(rear_avail[r]-river_rear[r,i]*territory_size[i],0)
      }}
    
  } else{
    for(r in 1:xxx){
      for(i in 4:1){
        flood_rear[r,i]<-min(floor(flood_avail[r]/territory_size[i]),juvs[r,i])
        flood_avail[r]<-max(flood_avail[r]-flood_rear[r,i]*territory_size[i],0)
      }}
    
    flood_rear<-pmax(flood_rear,0)
    #Who's left for in-river rearing
    juvs<-pmax(juvs-flood_rear,0)
    #In-river rearing
    rear_avail<-juv_hab
    for(r in 1:xxx){
      for(i in 4:1){
        river_rear[r,i]<-min(floor(rear_avail[r]/territory_size[i]),juvs[r,i])
        rear_avail[r]<-max(rear_avail[r]-river_rear[r,i]*territory_size[i],0)
      }}
  }
  
  river_rear<-pmax(river_rear,0)
  juvs<-pmax(juvs-river_rear,0)
  
  #if you do not fit in the habitat, you die,
  #then you still might die based on habitat-specific survival
  spaces<-dim(river_rear)[1]*dim(river_rear)[2]
  if(max(river_rear)<=1000000000 & stoch==1){
    junk<-matrix(rbinom(spaces,prob=as.vector(t(river_surv)),size=round(as.vector(t(river_rear)))),ncol=4,byrow=TRUE)
  } else{junk<-round(river_rear*river_surv)}
  
  if(max(flood_rear)<=1000000000 & stoch==1){
    junk2<-matrix(rbinom(spaces,prob=as.vector(t(flood_surv)),size=round(as.vector(t(flood_rear)))),ncol=4,byrow=TRUE)
  } else{junk2<-round(flood_rear*flood_surv)}
  
  river_rear<-(junk*stoch)+(river_rear*river_surv)*(1-stoch)
  flood_rear<-(junk2*stoch)+(flood_rear*flood_surv)*(1-stoch)
  
  
  #Growth
  river_rear<-river_rear %*% trans_mat_river
  flood_rear.outz<-c()
  if(length(dim(trans_mat_flood))>2){
    for(ii in 1:dim(trans_mat_flood)[3]){
      this<-flood_rear[ii,] %*% trans_mat_flood[,,ii]
      flood_rear.outz<-rbind(flood_rear.outz,this)
    }
  } else{flood_rear.outz<- flood_rear  %*% trans_mat_flood}
  
  if(stoch==1){
    flood_rear.outz<-round(flood_rear.outz)
    river_rear<-round(river_rear)
  }
  
  list(river=river_rear,flood=flood_rear.outz)
}