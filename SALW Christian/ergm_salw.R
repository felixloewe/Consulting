# ergm salw 
# start 1992



# matrix to add results
SRergMa<-matrix(0,57,60)
colnames(SRergMa)<-1954:2013
rownames(SRergMa)<-c('edges', 'KI', 'pKI', 'gwo', 'KI', 'KI','gwod', 'KI', 'KI', 'id1', 'KI','KI'
                    , 'mutual', 'KI', 'KI', 'gwesp', 'KI', 'KI','gwespd','KI', 'KI','nodematch', 'KI', 'KI',
                    'nodeofactor', 'KI', 'KI',
                    'nodeifactor', 'KI', 'KI', 'DA',
                    'KI', 'KI', 'Go', 'KI',
                    'KI', 'Gi', 'KI', 'KI','DC','Ki', 'Ki', 'Pol','Ki', 'Ki', 'patd',
                    'KI', 'KI', 'conf', 'KI', 'KI', 
                    'MiCi','KI', 'KI', 'MiCo','KI', 'KI')


#set.seed(123)
for (i in 1992:1992){# start 1952 # hier weiter
  
  
  #print(i)
  
  net<-network(amallr2(i,1,1), directed=TRUE)
  
  polscore<-as.matrix(amallr2(i,9,1))
  #od<- sqrt(degree(net, cmode="outdegree"))
  #net<- set.vertex.attribute(net,"od", od)
  
  #id<- sqrt(degree(net, cmode="indegree"))
  #net<- set.vertex.attribute(net,"id", id)
 #oecd<-as.matrix(amallr2(i,16,1))
 #Gseven<-as.matrix(amallr2(i,18,1))
 gseven<-amallr2(i,15,1)
 net<-network::set.vertex.attribute(net, 'MIIT', gseven)
  
  alliance<-as.matrix(amallr2(i,2,1))
  contiguity<-as.matrix(amallr2(i,3,1))
  path<- as.matrix(amallr2(i, 11,1))
  #embargo<-as.matrix(amallr2(i,4,1))
  mc<-amallr2(i,10,1) 
  net<- network::set.vertex.attribute(net,"CINC", mc)
  #distance<- as.matrix(amallr2(i,12,1))
  co<- amallr2(i,13,1)
  net<- network::set.vertex.attribute(net, 'CO', co)
  
  bi<-amallr2(i,5,1)
  #bi[bi==0]<-203
  #bi<-log(bi)-log(203)
  net<- network::set.vertex.attribute(net,"GDP", bi)
  
  #conf<- as.matrix(amallr2(i,8,1))
  
  model<- ergm(net~edges+gwodegree(1, fixed=FALSE)+idegree(1)+mutual+gwesp(1, fixed=FALSE)+
                 nodematch('MIIT', diff=TRUE, keep=c(1))+nodeofactor('MIIT', base=2)+
                 nodeifactor('MIIT', base=2)+
                 edgecov(alliance)+ nodeocov('GDP')+     nodeicov("GDP")  +
                 edgecov(contiguity)+edgecov(polscore)+ edgecov(path)+
                 nodeicov('CO')+  nodeicov('CINC')+ nodeocov('CINC'))
  # control=control.ergm(MCMC.samplesize=40000))
  
  
  SRergMa[1,i-1953]<- summary(model)$coef[1,1] # coefficient
  SRergMa[2,i-1953]<- summary(model)$coef[1,2] # std. error
  SRergMa[3,i-1953]<- summary(model)$coef[1,4] # p-value
  SRergMa[4,i-1953]<- summary(model)$coef[2,1]
  SRergMa[5,i-1953]<- summary(model)$coef[2,2]
  SRergMa[6,i-1953]<- summary(model)$coef[2,4]
  SRergMa[7,i-1953]<- summary(model)$coef[3,1]
  SRergMa[8,i-1953]<- summary(model)$coef[3,2]
  SRergMa[9,i-1953]<- summary(model)$coef[3,4]
  SRergMa[10,i-1953]<- summary(model)$coef[4,1]
  SRergMa[11,i-1953]<- summary(model)$coef[4,2]
  SRergMa[12,i-1953]<-summary(model)$coef[4,4]
  SRergMa[13,i-1953]<- summary(model)$coef[5,1]
  SRergMa[14,i-1953]<- summary(model)$coef[5,2]
  SRergMa[15,i-1953]<- summary(model)$coef[5,4]
  SRergMa[16,i-1953]<- summary(model)$coef[6,1]
  SRergMa[17,i-1953]<- summary(model)$coef[6,2]
  SRergMa[18,i-1953]<- summary(model)$coef[6,4]
  SRergMa[19,i-1953]<- summary(model)$coef[7,1]
  SRergMa[20,i-1953]<- summary(model)$coef[7,2]
  SRergMa[21,i-1953]<- summary(model)$coef[7,4]
  SRergMa[22,i-1953]<- summary(model)$coef[8,1]
  SRergMa[23,i-1953]<- summary(model)$coef[8,2]
  SRergMa[24,i-1953]<- summary(model)$coef[8,4]
  SRergMa[25,i-1953]<- summary(model)$coef[9,1]
  SRergMa[26,i-1953]<- summary(model)$coef[9,2]
  SRergMa[27,i-1953]<- summary(model)$coef[9,4]
  SRergMa[28,i-1953]<- summary(model)$coef[10,1]
  SRergMa[29,i-1953]<- summary(model)$coef[10,2]
  SRergMa[30,i-1953]<- summary(model)$coef[10,4]
  SRergMa[31,i-1953]<- summary(model)$coef[11,1]
  SRergMa[32,i-1953]<- summary(model)$coef[11,2]
  SRergMa[33,i-1953]<- summary(model)$coef[11,4]
  SRergMa[34,i-1953]<- summary(model)$coef[12,1]
  SRergMa[35,i-1953]<- summary(model)$coef[12,2]
  SRergMa[36,i-1953]<- summary(model)$coef[12,4]
  SRergMa[37,i-1953]<- summary(model)$coef[13,1]
  SRergMa[38,i-1953]<- summary(model)$coef[13,2]
  SRergMa[39,i-1953]<- summary(model)$coef[13,4]
  SRergMa[40,i-1953]<- summary(model)$coef[14,1]
  SRergMa[41,i-1953]<- summary(model)$coef[14,2]
  SRergMa[42,i-1953]<- summary(model)$coef[14,4]
  SRergMa[43,i-1953]<- summary(model)$coef[15,1]
  SRergMa[44,i-1953]<- summary(model)$coef[15,2]
  SRergMa[45,i-1953]<- summary(model)$coef[15,4]
  SRergMa[46,i-1953]<- summary(model)$coef[16,1]
  SRergMa[47,i-1953]<- summary(model)$coef[16,2]
  SRergMa[48,i-1953]<- summary(model)$coef[16,4]
  SRergMa[49,i-1953]<- summary(model)$coef[17,1]
  SRergMa[50,i-1953]<- summary(model)$coef[17,2]
  SRergMa[51,i-1953]<- summary(model)$coef[17,4]
  SRergMa[52,i-1953]<- summary(model)$coef[18,1]
  SRergMa[53,i-1953]<- summary(model)$coef[18,2]
  SRergMa[54,i-1953]<- summary(model)$coef[18,4]
  SRergMa[55,i-1953]<- summary(model)$coef[19,1]
  SRergMa[56,i-1953]<- summary(model)$coef[19,2]
  SRergMa[57,i-1953]<- summary(model)$coef[19,4]
  write.csv(SRergMa, 'SRergManeu.csv')
}



summary(model)
mcmc.diagnostics(model, vars.per.page=4)
gofer<- gof(model, dsp=FALSE, istar=FALSE, ostar=FALSE, rocprgof=FALSE)
plot(gofer, boxplot.geodist.max=9, boxplot.odegree.max=9, boxplot.idegree.max=9     , boxplot.esp.max=9,col='lightblue')
