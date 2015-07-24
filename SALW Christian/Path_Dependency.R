# Path Dependency

pathdep<- function(time){
  a<-matrix(0,224,224)
  for (i in (time-1995):(time-1992)){
    a<-a+salw[[i]]   
  }
  return(a)
}

PathD<-list()
PathD[[1]]<-matrix(0,224,224) # 1992
PathD[[2]]<-salw[[1]] #1993
PathD[[3]]<-salw[[2]]+salw[[1]] #1994
PathD[[4]]<-salw[[1]]+salw[[2]]+salw[[3]] #1995
PathD[[5]]<-pathdep(1996)
PathD[[6]]<-pathdep(1997)
PathD[[7]]<-pathdep(1998)
PathD[[8]]<-pathdep(1999)
PathD[[9]]<-pathdep(2000)
PathD[[10]]<-pathdep(2001)
PathD[[11]]<-pathdep(2002)
PathD[[12]]<-pathdep(2003)
PathD[[13]]<-pathdep(2004)
PathD[[14]]<-pathdep(2005)
PathD[[15]]<-pathdep(2006)
PathD[[16]]<-pathdep(2007)
PathD[[17]]<-pathdep(2008)
PathD[[18]]<-pathdep(2009)
PathD[[19]]<-pathdep(2010)
PathD[[20]]<-pathdep(2011)

