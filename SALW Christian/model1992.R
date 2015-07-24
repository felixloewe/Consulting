####### Chirstian ERGM #######
##1992
load("ws_christian.RData")
i <- 1992

#Netzwerk vorbereiten
net<-network(amallr2(i,1,1), directed=TRUE)
polscore<-as.matrix(amallr2(i,9,1))
gseven<-amallr2(i,15,1)
net<-network::set.vertex.attribute(net, 'MIIT', gseven)
alliance<-as.matrix(amallr2(i,2,1))
contiguity<-as.matrix(amallr2(i,3,1))
path<- as.matrix(amallr2(i, 11,1))
mc<-amallr2(i,10,1) 
net<- network::set.vertex.attribute(net,"CINC", mc)
co<- amallr2(i,13,1)
net<- network::set.vertex.attribute(net, 'CO', co)
bi<-amallr2(i,5,1)
net<- network::set.vertex.attribute(net,"GDP", bi)


#Modell Aufruf
model1992<- ergm(net~edges+gwodegree(1, fixed=FALSE)+idegree(1)+mutual+gwesp(1, fixed=FALSE)+
               nodematch('MIIT', diff=TRUE, keep=c(1))+nodeofactor('MIIT', base=2)+
               nodeifactor('MIIT', base=2)+
               edgecov(alliance)+ nodeocov('GDP')+     nodeicov("GDP")  +
               edgecov(contiguity)+edgecov(polscore)+ edgecov(path)+
               nodeicov('CO')+  nodeicov('CINC')+ nodeocov('CINC')
             )

gof1992 <- gof(model1992)

save.image("model1992")



