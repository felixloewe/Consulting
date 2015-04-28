# conflicts Datensatz aufbereiten:
#einlesen
Conflict <- read.csv("Importdaten/extern/conflict.csv", stringsAsFactors = F)
Conflict$End <- as.integer(Conflict$End)
#tippfehler korrigieren
Conflict$End[314] <- 2013

#Jahreszahlen für einfachere Filterung in jede Zeile
while(length(which(is.na(Conflict$End))) >= 1){
Conflict$End[which(is.na(Conflict$End))] <- Conflict$End[which(is.na(Conflict$End)) - 1]
Conflict$Begin[which(is.na(Conflict$Begin))] <- Conflict$Begin[which(is.na(Conflict$Begin)) - 1]
}
Conflict <- Conflict[Conflict$End >= 1992,]

Conflict$Begin[which(Conflict$Begin <= 1992)]<- 1992
