# OECD Matrix
# OEEC Gründung 1948
# OECD Gründung 1961
 
# 13 wirtschaftsstärksten Nationen

View(laenderliste)

OE<- matrix('F', 224,64)
rownames(OE)<- laenderliste$V1[1:224]
colnames(OE)<-1950:2013



# 15 MIIT > 0.3
OE[19,]<-'M' #Belgium
OE[54,]<-'M'# Denmark
OE[72,]<-'M' #Germany
OE[67,]<-'M' #France
OE[88,]<-'M' #Ireland
OE[90,]<-'M' #Italy
OE[34,]<-'M' #Canada
OE[134,]<-'M' #Netherlands
OE[12,]<-'M'  #Austria
OE[182,]<-'M' #Sweden
OE[183,]<-'M' #Switzerland
OE[177,]<-'M' #Spain
OE[115,]<-'M' #Malaysia
OE[202,]<-'M' #USA
OE[201,]<-'M' #UK

# später beigetreten
#OE[92,15:64]<-'M'
#OE[66, 20:64 ]<-'M'
#OE[11, 22:64]<-'M'
#OE[135, 24:64]<-'M'
#OE[122, 45:64 ]<-'M'
#OE[51, 46:64]<-'M'
#OE[99,47:64]<-'M'
#OE[82, 47:64]<-'M'
#OE[150, 47:64]<-'M'
#OE[168, 51:64]<-'M'
#OE[38, 61:64]<-'M'
#OE[169, 61:64]<-'M'
#OE[89, 61:64]<-'M'
#OE[63, 61:64]<-'M'#


### G7

g7<-matrix('N',224,64)
rownames(g7)<-laenderliste$V1[1:224]
colnames(g7)<-1950:2013

g7[34,]<-'M'
g7[67,]<-'M'
g7[74,]<-'M'
g7[90,]<-'M'
g7[92,]<-'M'
g7[201,]<-'M'
g7[202,]<-'M'

