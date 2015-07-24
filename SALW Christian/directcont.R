## generate a adjacency matrix with the directcont.short-dataset (from Prof. Skyler Cranmer) to use in arms trade ergms
# load directcont.short into Global Environment
# need laenderliste like in AM
library(statnet)
library(network)


load("C://Users//Landsberg//Desktop//Uni//consulting//weitere_Kovariablen//directcont.short.RData")

defall<- function(atop){A<- as.matrix.network(atop)
                        eM<- matrix(0,224,224)
                        rownames(eM)<- laenderliste$V1[1:224]
                        colnames(eM)<- laenderliste$V1[1:224]
                        d<-dim(A)[1]
                        
                        for (i in 1:d){
                          for (j in 1:d){
                            dyad<- A[i,j]
                            cowr<- rownames(A)[i]
                            natr<-cownat(cowr)
                            cowc<- colnames(A)[j]
                            natc<- cownat(cowc)
                            eM[natr,natc]<-dyad
                          }}
                        
                        return(eM)
}

#for years 1966-1990. reason for split: germany changes cow 
defall2<- function(atop){A<- as.matrix.network(atop)
                         eM<- matrix(0,224,224)
                         rownames(eM)<- laenderliste$V1[1:224]
                         colnames(eM)<- laenderliste$V1[1:224]
                         d<-dim(A)[1]
                         
                         for (i in 1:d){
                           for (j in 1:d){
                             dyad<- A[i,j]
                             cowr<- rownames(A)[i]
                             natr<-cownat2(cowr)
                             cowc<- colnames(A)[j]
                             natc<- cownat2(cowc)
                             eM[natr,natc]<-dyad
                           }}
                         
                         return(eM)
}

directcont<- function(atop){A<- as.matrix.network(atop)
                        eM<- matrix(0,224,224)
                        rownames(eM)<- laenderliste$V1[1.224]
                        colnames(eM)<- laenderliste$V1[1.224]
                        d<-dim(A)[1]
                        
                        for (i in 1:d){
                          for (j in 1:d){
                            dyad<- A[i,j]
                            cowr<- rownames(A)[i]
                            natr<-cownat(cowr)
                            cowc<- colnames(A)[j]
                            natc<- cownat(cowc)
                            eM[natr,natc]<-dyad
                          }}
                        
                        return(eM)
}

dcml<- list()
dcml[[1]]<- defall2(directcont.short[[1]])
dcml[[2]]<- dcml[[1]]
dcml[[3]]<- dcml[[1]]
dcml[[4]]<- dcml[[1]]
dcml[[5]]<- dcml[[1]]
dcml[[6]]<- dcml[[1]]
dcml[[7]]<- dcml[[1]]
dcml[[8]]<- dcml[[1]]
dcml[[9]]<- dcml[[1]]
dcml[[10]]<- dcml[[1]]
dcml[[11]]<- dcml[[1]]
dcml[[12]]<- dcml[[1]]
dcml[[13]]<- dcml[[1]]
dcml[[14]]<- dcml[[1]]
dcml[[15]]<- dcml[[1]]
dcml[[16]]<- dcml[[1]]
dcml[[17]]<- dcml[[1]] # 1966
dcml[[18]]<- defall2(directcont.short[[2]])
dcml[[19]]<-  defall2(directcont.short[[3]])
dcml[[20]]<-  defall2(directcont.short[[4]])
dcml[[21]]<-  defall2(directcont.short[[5]])
dcml[[22]]<-  defall2(directcont.short[[6]])
dcml[[23]]<-  defall2(directcont.short[[7]])
dcml[[24]]<-  defall2(directcont.short[[8]])
dcml[[25]]<-  defall2(directcont.short[[9]])
dcml[[26]]<-  defall2(directcont.short[[10]])
dcml[[27]]<-  defall2(directcont.short[[11]])
dcml[[28]]<-  defall2(directcont.short[[12]])
dcml[[29]]<-  defall2(directcont.short[[13]])
dcml[[30]]<-  defall2(directcont.short[[14]])
dcml[[31]]<-  defall2(directcont.short[[15]])
dcml[[32]]<-  defall2(directcont.short[[16]])
dcml[[33]]<-  defall2(directcont.short[[17]])
dcml[[34]]<-  defall2(directcont.short[[18]])
dcml[[35]]<-  defall2(directcont.short[[19]])
dcml[[36]]<-  defall2(directcont.short[[20]])
dcml[[37]]<-  defall2(directcont.short[[21]])
dcml[[38]]<-  defall2(directcont.short[[22]])
dcml[[39]]<-  defall2(directcont.short[[23]])
dcml[[40]]<-  defall2(directcont.short[[24]])
dcml[[41]]<-  defall(directcont.short[[25]])
dcml[[42]]<-  defall(directcont.short[[26]])
dcml[[43]]<-  defall(directcont.short[[27]])
dcml[[44]]<-  defall(directcont.short[[28]])
dcml[[45]]<-  defall(directcont.short[[29]])
dcml[[46]]<-  defall(directcont.short[[30]])
dcml[[47]]<-  defall(directcont.short[[31]])
dcml[[48]]<-  defall(directcont.short[[32]])
dcml[[49]]<-  defall(directcont.short[[33]])
dcml[[50]]<-  defall(directcont.short[[34]])
dcml[[51]]<-  defall(directcont.short[[35]]) #2000
dcml[[52]]<-  dcml[[51]] 
dcml[[53]]<-  dcml[[51]] 
dcml[[54]]<-  dcml[[51]] 
dcml[[55]]<-  dcml[[51]] 
dcml[[56]]<-  dcml[[51]] 
dcml[[57]]<-  dcml[[51]] 
dcml[[58]]<-  dcml[[51]] 
dcml[[59]]<-  dcml[[51]] 
dcml[[60]]<-  dcml[[51]] 
dcml[[61]]<-  dcml[[51]] 
dcml[[62]]<-  dcml[[51]] 
dcml[[63]]<-  dcml[[51]] 
dcml[[64]]<-  dcml[[51]] 

rm(directcont, directcont.short)
rm(defall, defall2)
