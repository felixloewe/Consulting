polity <- read.csv("C:/Users/Landsberg/Desktop/Uni/consulting/p4v2013csv.csv")

library(reshape)
polity<-p4v2013csv
polity<- polity[,-(12:36)]
polity<- polity[,-(6:10)]
polity<- polity[,-3]
polity<- polity[,-1]

for (i in 16727:1){
  if (polity[i,3]<1950){
   polity<-polity[-i,] 
  }
}

M<- matrix(0,257, 64)
colnames(M)<-1950:2013
rownames(M)<- laenderliste$V1
M<-M[-(225:257),]

for (i in 1:8827){
  M[cownat(polity[i,1]),polity[i,3]-1949]<-polity[i,4]  
}
 
# find NA

for(i in 1:224){
  for(j in 1:64){
    if (is.na(M[i,j])==TRUE){
      print(i)
      print(j)
    }
  }
}

M[is.na(M)==TRUE]<-0
M[72,]<-10
polity<-M
rm(M)

# create adjacency matrix with polity IV difference



poldiff<-function(time){
  M<- matrix(0,224,224)
  for(i in 1:224){
    a<- polity[i,time-1949]
    for (j in 1:224){
      b<- polity[j,time-1949]
      M[i,j]<- abs(a-b)
    }}
  return(M)
}

rm(i,j)
