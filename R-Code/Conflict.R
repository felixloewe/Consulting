# conflicts Datensatz aufbereiten:
# Ausgabe:
# - Conflcit: Conflict - Gesamtdatensatz
# - ConflcitYear: Liste mit Datensatz pro Jahr
install.packages("mefa")
library(mefa)

# einlesen
Conflict <- read.csv("Importdaten/extern/conflict.csv", stringsAsFactors = F)
Conflict$End <- as.integer(Conflict$End)

# tippfehler korrigieren
Conflict$End[314] <- 2013

# Jahreszahlen und Mag - Score für einfachere Filterung in jede Zeile
while(length(which(is.na(Conflict$Mag))) >= 1){
Conflict$Mag[which(is.na(Conflict$Mag))] <- Conflict$Mag[which(is.na(Conflict$Mag)) - 1]
}
while(length(which(is.na(Conflict$End))) >= 1){
  Conflict$End[which(is.na(Conflict$End))] <- Conflict$End[which(is.na(Conflict$End)) - 1]
  Conflict$Begin[which(is.na(Conflict$Begin))] <- Conflict$Begin[which(is.na(Conflict$Begin)) - 1]
}
# Filterung nach Jahreszahlen
Conflict <- Conflict[Conflict$End >= 1992,]

# Begin auf mindestens 1992 setzen für spätere Jahresdaten
Conflict$Begin[which(Conflict$Begin <= 1992)]<- 1992

# Jährliche Daten erstellen
year <- vector()
times <- vector(length = nrow(Conflict))
for(i in 1:nrow(Conflict)){
times[i] <- Conflict$End[i] - Conflict$Begin[i] + 1
year <- c(year, Conflict$End[i]:Conflict$Begin[i] )
}
Conflict <- rep(Conflict, times)
Conflict <- cbind(year,Conflict)

#Spalten auswählen
Conflict <- Conflict[,c("year", "Mag", "States.Directly.Involved")]
names(Conflict)[3] <- "country.name"

#cow code erstellen
ccode <- countrycode(Conflict$country.name,origin = "country.name", destination = "cown")
Conflict <- cbind(ccode,Conflict)

#Datensatz aufteilen in Jahre
Year <- 1992:2011
ConflictYear <- lapply(Year, function(jahr) subset(Conflict, year==jahr))

# Maximum-Konfliktscore pro Land gebildet
# Grund: es gibt in einigen Ländern mehrere innere Konflikte pro Jahr
for (i in 1:20){
ConflictYear[[i]] <- aggregate(ConflictYear[[i]], by = list(ccode = ConflictYear[[i]]$ccode), FUN = max)
ConflictYear[[i]] <- ConflictYear[[i]][,-1] 
}

rm("ccode", "i", "times", "year")
