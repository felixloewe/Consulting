# mod=1, Adjacency matrix
# mod=2, Defence Agreement
# mod=3, Direct Contiguity
# mod=4, Embargo
# mod=5, GDP
# mod=6, Military Expenditure
# mod=8, Conflict 
# mod=9, polity IV
# mod=10, military capability
# mod=11, Path Dependency
# mod=12, Distance
# mod=13, Intrastate conflict
# mod=14, International Conflict
# mod=15, OECD node
# mod=16, OECD relational
# mod=17, G7
# mod=18, G7 relational
# mod=19, Nato

amallr<- function(year, mod, tiv){
  DA<-daml[[year-1949]]
  DC<-dcml[[year-1949]]
  
  #em<-EM[[year-1962]]
  
  bip<-GDP[,year-1949]
  
  #me<-ME[,year-1987]
  
  #con<- conflict[[year-1965]]
  
  inc<-InCo[,year-1949]
  pol<- poldiff(year)
  co<-coml[,year-1944]
  mc<-MC[,year-1949]
  pd<-PathD[[year-1949]]
  di<-DI+1
  di<-log(di)
  oe<-OE[,year-1949]
  oer<-oef(year)
  g7r<-g7f(year)
  gs<-g7[,year-1949]
  
  
  A<-amk[[year-1949]]  
  A[A < tiv] <-0
  A[A >= tiv] <-1
  
  for (i in 257:1){
    if (EX[i,year-1949]==0){
      A<-A[-i,]
      
      A<-A[,-i]
      DA<-DA[-i,]
      DA<-DA[,-i]
      
      DC<-DC[-i,]
      DC<-DC[,-i]
      
      #em<-em[-i,]
      #em<-em[,-i]
      co<-co[-i]
      mc<-mc[-i]
      oe<-oe[-i]
      gs<-gs[-i]
      
      oer<-oer[,-i]
      oer<-oer[-i,]
      
      g7r<-g7r[,-i]
      g7r<-g7r[-i,]
      
      bip<-bip[-i]
      
      #me<-me[-i]
      inc<-inc[-i]
      
      di<-di[-i,]
      di<-di[,-i]
      
      #con<- con[-i,]
      #con<- con[,-i]
      
      pol<-pol[-i,]
      pol<-pol[,-i]
      
      pd<-pd[-i,]
      pd<-pd[,-i]
      
    }
  }
  if (mod==1){
    return(A)}
  
  if (mod==2){
    return(DA)
  }
  if (mod==3){
    return(DC)}
 # if (mod==4){
  #  return(em)}
  if (mod==5){
    return(bip)}
  if (mod==6){
    return(me)}
  if (mod==8){
    return(con)}
  if (mod==9){
    return(pol)}
  if (mod==10){
    return(mc)}
  if (mod==11){
    return(pd)}
 if (mod==12){
   return(di)}
 if (mod==13){
   return(co)}
 if (mod==14){
   return(inc)}
 if (mod==15){
   return(oe)}
 if (mod==16){
   return(oer)}
 if (mod==17){
   return(gs)}
 if (mod==18){
   return(g7r)}
}

### t-2 lag

amallr2<- function(year, mod, tiv){
  DA<-daml[[year-1951]]
  DC<-dcml[[year-1951]]
  
  #em<-EM[[year-1962]]
  
  bip<-GDP[,year-1951]
  
  #me<-ME[,year-1987]
  
  #con<- conflict[[year-1965]]
  inc<-InCo[,year-1951]
  
  pol<- poldiff(year)
  co<-coml[,year-1946]
  mc<-MC[,year-1951]
  pd<-PathD[[year-1991]]
 # di<-DI+1
  #di<-log(di)
  oe<-OE[,year-1949]
  gs<-g7[,year-1949]
  #g7r<-g7f(year)
  #nat<-nato[, year-1949]
  
  #oer<-oef(year)
  
  A<-salw[[year-1991]]  
  A[A < tiv] <-0
  A[A >= tiv] <-1
  
  for (i in 224:1){
    if (EX[i,year-1949]==0){
      A<-A[-i,]
      
      A<-A[,-i]
      DA<-DA[-i,]
      DA<-DA[,-i]
      
      DC<-DC[-i,]
      DC<-DC[,-i]
      
      #em<-em[-i,]
      #em<-em[,-i]
      co<-co[-i]
      mc<-mc[-i]
      oe<-oe[-i]
     # gs<-gs[-i]
    #  nat<-nat[-i]
      
     # oer<-oer[-i,]
      #oer<-oer[,-i]
      
      #g7r<-g7r[-i,]
      #g7r<-g7r[,-i]
      
      bip<-bip[-i]
      inc<-inc[-i]
      #me<-me[-i]
      
    #  di<-di[-i,]
     # di<-di[,-i]
      
      #con<- con[-i,]
      #con<- con[,-i]
      
      pol<-pol[-i,]
      pol<-pol[,-i]
      
  #    pd<-pd[-i,]
   #   pd<-pd[,-i]
      
    }
  }
  if (mod==1){
    return(A)}
  
  if (mod==2){
    return(DA)
  }
  if (mod==3){
    return(DC)}
  # if (mod==4){
  #  return(em)}
  if (mod==5){
    return(bip)}
  if (mod==6){
    return(me)}
  if (mod==8){
    return(con)}
  if (mod==9){
    return(pol)}
  if (mod==10){
    return(mc)}
  if (mod==11){
    return(pd)}
  if (mod==12){
    return(di)}
  if (mod==13){
    return(co)}
  if (mod==14){
    return(inc)}
  if (mod==15){
    return(oe)}
  if (mod==16){
    return(oer)}
  if (mod==17){
    return(gs)}
  if (mod==18){
    return(g7r)}
  if (mod==19){
    return(nat)}
}

## order date

amallr3<- function(year, mod, tiv){
  DA<-daml[[year-1949]]
  DC<-dcml[[year-1949]]
  
  #em<-EM[[year-1962]]
  
  bip<-GDP[,year-1949]
  
  #me<-ME[,year-1987]
  
  #con<- conflict[[year-1965]]
  
  inc<-InCo[,year-1949]
  pol<- poldiff(year)
  co<-coml[,year-1944]
  mc<-MC[,year-1949]
  pd<-PathD[[year-1991]]
  #di<-DI+1
  #di<-log(di)
  
  oe<-OE[,year-1949]
  A<-salw[[year-1991]]  
  A[A < tiv] <-0
  A[A >= tiv] <-1
  
  for (i in 257:1){
    if (EX[i,year-1949]==0){
      A<-A[-i,]
      
      A<-A[,-i]
      DA<-DA[-i,]
      DA<-DA[,-i]
      
      DC<-DC[-i,]
      DC<-DC[,-i]
      
      #em<-em[-i,]
      #em<-em[,-i]
      co<-co[-i]
      mc<-mc[-i]
      oe<-oe[-i]
      
      bip<-bip[-i]
      
      #me<-me[-i]
      inc<-inc[-i]
      
      #di<-di[-i,]
      #di<-di[,-i]
      
      #con<- con[-i,]
      #con<- con[,-i]
      
      pol<-pol[-i,]
      pol<-pol[,-i]
      
      pd<-pd[-i,]
      pd<-pd[,-i]
      
    }
  }
  if (mod==1){
    return(A)}
  
  if (mod==2){
    return(DA)
  }
  if (mod==3){
    return(DC)}
  # if (mod==4){
  #  return(em)}
  if (mod==5){
    return(bip)}
  if (mod==6){
    return(me)}
  if (mod==8){
    return(con)}
  if (mod==9){
    return(pol)}
  if (mod==10){
    return(mc)}
  if (mod==11){
    return(pd)}
  if (mod==12){
    return(di)}
  if (mod==13){
    return(co)}
  if (mod==14){
    return(inc)}
  if (mod==15){
    return(oe)}
}


### moving 3 year

amallr4<- function(year, mod, tiv){ #start 1954
  DA<-daml[[year-1949]]
  DC<-dcml[[year-1949]]
  
  #em<-EM[[year-1962]]
  
  bip<-GDP[,year-1949]
  
  #me<-ME[,year-1987]
  
  #con<- conflict[[year-1965]]
  inc<-InCo[,year-1949]
  
  pol<- poldiff(year)
  co<-coml[,year-1944]
  mc<-MC[,year-1949]
  pd<-PathD[[year-1949]]
  di<-DI+1
  di<-log(di)
  oe<-OE[,year-1949]
  gs<-g7[,year-1949]
  g7r<-g7f(year)
  
  oer<-oef(year)
  
  A<-amk[[year-1949]]  +amk[[year-1950]]+amk[[year-1951]]
  A[A < tiv] <-0
  A[A >= tiv] <-1
  
  for (i in 257:1){
    if (EX[i,year-1949]==0){
      A<-A[-i,]
      
      A<-A[,-i]
      DA<-DA[-i,]
      DA<-DA[,-i]
      
      DC<-DC[-i,]
      DC<-DC[,-i]
      
      #em<-em[-i,]
      #em<-em[,-i]
      co<-co[-i]
      mc<-mc[-i]
      oe<-oe[-i]
      gs<-gs[-i]
      
      oer<-oer[-i,]
      oer<-oer[,-i]
      
      g7r<-g7r[-i,]
      g7r<-g7r[,-i]
      
      bip<-bip[-i]
      inc<-inc[-i]
      #me<-me[-i]
      
      di<-di[-i,]
      di<-di[,-i]
      
      #con<- con[-i,]
      #con<- con[,-i]
      
      pol<-pol[-i,]
      pol<-pol[,-i]
      
      pd<-pd[-i,]
      pd<-pd[,-i]
      
    }
  }
  if (mod==1){
    return(A)}
  
  if (mod==2){
    return(DA)
  }
  if (mod==3){
    return(DC)}
  # if (mod==4){
  #  return(em)}
  if (mod==5){
    return(bip)}
  if (mod==6){
    return(me)}
  if (mod==8){
    return(con)}
  if (mod==9){
    return(pol)}
  if (mod==10){
    return(mc)}
  if (mod==11){
    return(pd)}
  if (mod==12){
    return(di)}
  if (mod==13){
    return(co)}
  if (mod==14){
    return(inc)}
  if (mod==15){
    return(oe)}
  if (mod==16){
    return(oer)}
  if (mod==17){
    return(gs)}
  if (mod==18){
    return(g7r)}
}
