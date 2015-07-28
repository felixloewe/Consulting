#SALW readin

# install.packages("network")
# install.packages("stringr")
# install.packages("igraph")
# install.packages("countrycode")
# install.packages("ergm")
library(countrycode)
library(stringr)
library(igraph)
library(network)
library(ergm)

# NISAT-Database MasterTableFinal einlesen (109.000 Zeilen = 109.000 Exporte)
MADdata <-read.csv("SALW Christian/Data/MAD_Data_1992_2011.csv")




# Leerzeichen in Import- und Exportland entfernen
MADdata$Reporter_Name <- str_trim(MADdata$Reporter_Name)
MADdata$Partner_Name <- str_trim(MADdata$Partner_Name)
MADdata$Value <- as.numeric(MADdata$Value)

# Spalten vertauschen, sodass Exportland an erster Stelle steht
MADdata <- cbind(Reporter_Code = MADdata$Reporter_Code, Partner_Code = MADdata$Partner_Code, MADdata[,-c(1,8)])

# Kontinente der Import- und Exportlaender hinzufügen
# source("R-Code/continents.R", local = T)

# Waffenwert in Millionen umwandeln
MADdata$ValueMil <- MADdata$Value/10^6



salw<-list() #from 1992 on
for (i in 1:20){
  salw[[i]]<-matrix(0,224,224)
  rownames(salw[[i]])<-laenderliste$V1[1:224]
  colnames(salw[[i]])<-laenderliste$V1[1:224]
}


for (i in 1:109522) {
  s<-which (MADdata[i,1]==laenderliste$COW)
  r<-which (MADdata[i,2]==laenderliste$COW)
  y<-MADdata[i,5]
  v<-MADdata[i,10]
  salw[[y-1991]][s,r]<-salw[[y-1991]][s,r]+v
}
