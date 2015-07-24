# military capability

NMC_v4_0 <- read.csv("C:/Users/Landsberg/Desktop/Uni/consulting/Masterarbeit/NMC_v4_0.csv")
MC<-NMC_v4_0

MC<-MC[,-11]
MC<-MC[,-(4:9)]

for (i in 14199:1){
  if(MC[i,3]<1950){
    MC<-MC[-i,]
  }
}

MiC<- matrix(0,224,64)
rownames(MiC)<- laenderliste$V1[1:224]
colnames(MiC)<-1950:2013

for(i in 1:8485){
  d<-which(MC[i,2]==laenderliste$COW)
  s<-MC[i,3]-1949
  MiC[d,s]<-MC[i,4]+MC[i,4]^2
}

# data only till 2007 -> extending until 2003
MiC[,59]<-MiC[,58]
MiC[,60]<-MiC[,58]
MiC[,61]<-MiC[,58]
MiC[,62]<-MiC[,58]
MiC[,63]<-MiC[,58]
MiC[,64]<-MiC[,58]

MC<-MiC
# Germany has different COW from 1950-1990
MC[72,6]<-NMC_v4_0[5334,10]
MC[72,7]<-NMC_v4_0[5335,10]
MC[72,8]<-NMC_v4_0[5336,10]
MC[72,9]<-NMC_v4_0[5337,10]
MC[72,10]<-NMC_v4_0[5338,10]
MC[72,11]<-NMC_v4_0[5339,10]
MC[72,12]<-NMC_v4_0[5340,10]
MC[72,13]<-NMC_v4_0[5341,10]
MC[72,14]<-NMC_v4_0[5342,10]
MC[72,15]<-NMC_v4_0[5343,10]
MC[72,16]<-NMC_v4_0[5344,10]
MC[72,17]<-NMC_v4_0[5345,10]
MC[72,18]<-NMC_v4_0[5346,10]
MC[72,19]<-NMC_v4_0[5347,10]
MC[72,20]<-NMC_v4_0[5348,10]
MC[72,21]<-NMC_v4_0[5349,10]
MC[72,22]<-NMC_v4_0[5350,10]
MC[72,23]<-NMC_v4_0[5351,10]
MC[72,24]<-NMC_v4_0[5352,10]
MC[72,25]<-NMC_v4_0[5353,10]
MC[72,26]<-NMC_v4_0[5354,10]
MC[72,27]<-NMC_v4_0[5355,10]
MC[72,28]<-NMC_v4_0[5356,10]
MC[72,29]<-NMC_v4_0[5357,10]
MC[72,30]<-NMC_v4_0[5358,10]
MC[72,31]<-NMC_v4_0[5359,10]
MC[72,32]<-NMC_v4_0[5360,10]
MC[72,33]<-NMC_v4_0[5361,10]
MC[72,34]<-NMC_v4_0[5362,10]
MC[72,35]<-NMC_v4_0[5363,10]
MC[72,36]<-NMC_v4_0[5364,10]
MC[72,37]<-NMC_v4_0[5365,10]
MC[72,38]<-NMC_v4_0[5366,10]
MC[72,39]<-NMC_v4_0[5367,10]
MC[72,40]<-NMC_v4_0[5368,10]
MC[72,41]<-NMC_v4_0[5369,10]
MC[16,22]<-0

rm(MiC, NMC_v4_0)
rm(d,i,s)
