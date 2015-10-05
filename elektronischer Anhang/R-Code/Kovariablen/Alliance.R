# alliance_v4.1_by_dyad_yearly Datensatz aufbereiten
# Ausgabe:
# - Alliance: Alliance - Gesamtdatensatz
# - AllianceYear: Datensatz nach Jahren aufgeteilt
# - AAlliance: Adjacency-Matrix

# einlesen
Alliance <- read.csv("Importdaten/extern/alliance_v4.1_by_dyad_yearly.csv")
# Jahre und Variablen auswählen
Alliance <- Alliance[Alliance$year >= 1992 & Alliance$year <= 2011,c("ccode1", "state_name1", "ccode2", "state_name2", "year")]

# Datensatz auftrennen
Year <- 1992:2011
AllianceYear <- lapply(Year, function(jahr) subset(Alliance, year==jahr))

# Codes der beteiligten Länder zusammen stellen
ccode <- c(names(table(Alliance$ccode1)),names(table(Alliance$ccode2)))
ccode <- ccode[-which(duplicated(ccode))]

# Adjacency Matrix erstellen für jedes Jahr
l <- length(ccode)
AAlliance <- list()
l <- length(ccode)

for(y in 1:20){
  mat <- matrix(0,nrow = l, ncol = l , dimnames = list(ccode, ccode))
  A1 <- (AllianceYear[[y]])
    for(i in 1:nrow(A1)){
    c1 <- as.character(A1[i,"ccode1"])
    c2 <- as.character(A1[i,"ccode2"])
    mat[c1,c2] <- mat[c2,c1] <- 1
  }
  AAlliance[[y]] <- mat
}

rm(A1, mat, c1, c2, ccode, i, l, y)
