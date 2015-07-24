###   alliance_cow

alliance_v4.1_by_dyad_yearly <- read.csv("C:/Users/cx/Desktop/Uni/Masterarbeit/alliance_v4.1_by_dyad_yearly.csv")

alliance<- alliance_v4.1_by_dyad_yearly

alliance<-alliance[,-19]
alliance<-alliance[,-(15:17)]
alliance<-alliance[,-(6:13)]
alliance<-alliance[,-1]

d<-dim(alliance)[1]

for(i in d:1){
  if(alliance[i,6]<1950){
    alliance<-alliance[-i,]
  }
}

d<-dim(alliance)[1]
daml2<-list()
for(i in 1:64){
  daml2[[i]]<-matrix(0,224,224)
  rownames(daml2[[i]])<-laenderliste$V1[1:224]
  colnames(daml2[[i]])<-laenderliste$V1[1:224]
}

for (i in 1:d){
  
  if(alliance[i,5]==1){
    r<-which(alliance[i,1]==laenderliste$COW)
    t<-which(alliance[i,3]==laenderliste$COW)
    daml2[[alliance[i,6]-1949]][r,t]<-1
    daml2[[alliance[i,6]-1949]][t,r]<-1
  }
}

for (i in 1:d){
  print(i)
  if(alliance[i,5]==1){
    r<-which(alliance[i,1]==laenderliste$COW2)
    t<-which(alliance[i,3]==laenderliste$COW2)
    daml2[[alliance[i,6]-1949]][r,t]<-1
    daml2[[alliance[i,6]-1949]][t,r]<-1
  }
}

daml2[[64]]<-daml2[[63]]
daml<-daml2
rm(daml2, alliance, alliance_v4.1_by_dyad_yearly, d, i, r, t)
