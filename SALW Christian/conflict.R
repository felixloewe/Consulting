# intrastate conflicts

conflict <- read.csv("C:/Users/Landsberg/Desktop/conflict.csv")
C<-conflict
C<-C[,-(6:8)]
C<-C[,-3]

  coml<- matrix(0,224,70)
  rownames(coml)<-laenderliste$V1[1:224]
  colnames(coml)<-1945:2014

for(i in 338:1){
  if(C[i,5]==0){
    C<-C[-i,]
  }
}
C<-C[-(267:268),]
C<-C[-(260:261),]
C<-C[-21,]


for (i in 1:281){
  print(i)
    s<-which(laenderliste$V1==as.vector(C[i,4]))
    for(j in C[i,1]:(as.integer(C[i,2])+1944)){
      coml[s,j-1944]<-C[i,3]+coml[s,j-1944]
    }
}

# international conflicts

icoml<- list() #1 =1945
for(i in 1:69){
  icoml[[i]]<-matrix(0,224,224)
  rownames(icoml[[i]])<-laenderliste$V1[1:224]
  colnames(icoml[[i]])<-laenderliste$V1[1:224]
}

icoml[[2]][84,142]<-1
icoml[[3]][84,142]<-1
icoml[[4]][84,142]<-1
icoml[[4]][59,89]<-1
icoml[[4]][59,93]<-1
icoml[[4]][59,105]<-1
icoml[[4]][59,184]<-1
icoml[[4]][89,93]<-1
icoml[[4]][89,105]<-1
icoml[[4]][89,184]<-1
icoml[[4]][93,105]<-1
icoml[[4]][93,184]<-1
icoml[[4]][105,184]<-1
icoml[[5]][59,89]<-1
icoml[[5]][59,93]<-1
icoml[[5]][59,105]<-1
icoml[[5]][59,184]<-1
icoml[[5]][89,93]<-1
icoml[[5]][89,105]<-1
icoml[[5]][89,184]<-1
icoml[[5]][93,105]<-1
icoml[[5]][93,184]<-1
icoml[[5]][105,184]<-1
icoml[[6]][39,185]<-1
icoml[[6]][98,99]<-1
icoml[[7]][98,99]<-1
icoml[[8]][98,99]<-1
icoml[[9]][98,99]<-1
icoml[[10]][39,185]<-1
icoml[[11]][39,185]<-1
icoml[[12]][82,176]<-1
icoml[[13]][81,136]<-1
icoml[[13]][120,128]<-1
icoml[[14]][120,128]<-1
icoml[[15]][120,128]<-1
icoml[[16]][120,128]<-1
icoml[[17]][120,128]<-1
icoml[[18]][120,128]<-1
icoml[[19]][120,128]<-1
icoml[[20]][120,128]<-1
icoml[[21]][120,128]<-1
icoml[[22]][120,128]<-1
icoml[[23]][120,128]<-1
icoml[[24]][120,128]<-1
icoml[[25]][120,128]<-1
icoml[[26]][120,128]<-1
icoml[[27]][120,128]<-1
icoml[[28]][120,128]<-1
icoml[[29]][120,128]<-1
icoml[[30]][120,128]<-1
icoml[[31]][120,128]<-1
icoml[[18]][39,84]<-1
icoml[[18]][31,155]<-1
icoml[[19]][31,155]<-1
icoml[[20]][31,155]<-1
icoml[[18]][85,115]<-1
icoml[[19]][85,115]<-1
icoml[[20]][85,115]<-1
icoml[[21]][85,115]<-1
icoml[[22]][85,115]<-1
icoml[[19]][171,59]<-1
icoml[[20]][171,59]<-1
icoml[[19]][4,128]<-1
icoml[[20]][4,128]<-1
icoml[[20]][64,171]<-1
icoml[[21]][84,142]<-1
icoml[[23]][59,89]<-1
icoml[[23]][59,93]<-1
icoml[[23]][59,184]<-1
icoml[[23]][89,93]<-1
icoml[[23]][89,184]<-1
icoml[[23]][93,184]<-1
icoml[[24]][59,89]<-1
icoml[[24]][59,93]<-1
icoml[[24]][59,184]<-1
icoml[[24]][89,93]<-1
icoml[[24]][89,184]<-1
icoml[[24]][93,184]<-1
icoml[[25]][59,89]<-1
icoml[[25]][59,93]<-1
icoml[[25]][59,184]<-1
icoml[[25]][89,93]<-1
icoml[[25]][89,184]<-1
icoml[[25]][93,184]<-1
icoml[[26]][59,89]<-1
icoml[[26]][59,93]<-1
icoml[[26]][59,184]<-1
icoml[[26]][89,93]<-1
icoml[[26]][89,184]<-1
icoml[[26]][93,184]<-1
icoml[[25]][176,39]<-1
icoml[[25]][60,81]<-1
icoml[[27]][142,16]<-1
icoml[[27]][142,84]<-1
icoml[[29]][59,89]<-1
icoml[[29]][59,184]<-1
icoml[[29]][89,184]<-1
icoml[[30]][86,87]<-1
icoml[[31]][86,87]<-1
icoml[[31]][129,224]<-
  icoml[[32]][129,224]<-1
icoml[[33]][129,224]<-1
icoml[[34]][129,224]<-1
icoml[[35]][129,224]<-1
icoml[[31]][120,128]<-1
icoml[[32]][120,128]<-1
icoml[[33]][120,128]<-1
icoml[[34]][120,128]<-1
icoml[[35]][120,128]<-1
icoml[[36]][120,128]<-1
icoml[[37]][120,128]<-1
icoml[[38]][120,128]<-1
icoml[[39]][120,128]<-1
icoml[[40]][120,128]<-1
icoml[[41]][120,128]<-1
icoml[[42]][120,128]<-1
icoml[[43]][120,128]<-1
icoml[[44]][120,128]<-1
icoml[[45]][120,128]<-1
icoml[[33]][6,42]<-1
icoml[[33]][32,188]<-1
icoml[[34]][32,188]<-1
icoml[[35]][32,188]<-1
icoml[[36]][32,188]<-1
icoml[[37]][32,188]<-1
icoml[[38]][32,188]<-1
icoml[[39]][32,188]<-1
icoml[[40]][32,188]<-1
icoml[[41]][32,188]<-1
icoml[[42]][32,188]<-1
icoml[[43]][32,188]<-1
icoml[[34]][89,105]<-1
icoml[[34]][6,42]<-1
icoml[[34]][219,220]<-1
icoml[[35]][219,220]<-1
icoml[[34]][187,198]<-1
icoml[[35]][187,198]<-1
icoml[[34]][214,32]<-1
icoml[[35]][214,32]<-1
icoml[[36]][214,32]<-1
icoml[[37]][214,32]<-1
icoml[[38]][214,32]<-1
icoml[[39]][214,32]<-1
icoml[[40]][214,32]<-1
icoml[[41]][214,32]<-1
icoml[[42]][214,32]<-1
icoml[[43]][214,32]<-1
icoml[[44]][214,32]<-1
icoml[[45]][214,32]<-1
icoml[[35]][39,214]<-1
icoml[[36]][86,87]<-1
icoml[[37]][86,87]<-1
icoml[[38]][86,87]<-1
icoml[[39]][86,87]<-1
icoml[[40]][86,87]<-1
icoml[[41]][86,87]<-1
icoml[[42]][86,87]<-1
icoml[[43]][86,87]<-1
icoml[[44]][86,87]<-1
icoml[[37]][81,136]<-1
icoml[[38]][81,136]<-1
icoml[[39]][81,136]<-1
icoml[[40]][81,136]<-1
icoml[[41]][81,136]<-1
icoml[[42]][81,136]<-1
icoml[[38]][89,184]<-1
icoml[[38]][8,201]<-1
icoml[[39]][89,105]<-1
icoml[[40]][89,105]<-1
icoml[[41]][89,105]<-1
icoml[[42]][89,105]<-1
icoml[[43]][89,105]<-1
icoml[[44]][89,105]<-1
icoml[[45]][89,105]<-1
icoml[[46]][89,105]<-1
icoml[[38]][89,105]<-1
icoml[[41]][39,214]<-1
icoml[[42]][39,214]<-1
icoml[[43]][39,214]<-1
icoml[[45]][145,202]<-1
icoml[[45]][163, 120,]<-1
icoml[[46]][87,101]<-1
icoml[[47]][87,101]<-1
icoml[[46]][9,13]<-1
icoml[[47]][9,13]<-1
icoml[[48]][9,13]<-1
icoml[[49]][9,13]<-1
icoml[[50]][9,13]<-1
icoml[[51]][58,148]<-1
icoml[[54]][64,62]<-1
icoml[[55]][64,62]<-1
icoml[[56]][64,62]<-1
icoml[[55]][84,142]<-1
icoml[[62]][89,105]<-1
icoml[[64]][70,154]<-1

# symmetrize
for (i in 1:69){
  icoml[[i]]<-icoml[[i]]+t(icoml[[i]])
}

  icov<-matrix(0,224,64)
  rownames(icov)<-laenderliste$V1[1:224]
  colnames(icov)<-1950:2013

  
for (i in 1:64){
q<-icoml[[i+5]]
m<-MC[,i]
k<-q*m
for(j in 1:224){
  icov[j,i]<-sum(k[j,])
}
}

InCo<-icov
rm(C, icov, k, q,i,j,m,s, conflict)
