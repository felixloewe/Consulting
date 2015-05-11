# Polity IV Datensatz aufbereiten
# Ausgabe:
# - Polity: Gesamtdatensatz für alle Jahre
# - PolityYear: Liste mit Datensatz pro Jahr
# - APolity: Adjacency Matrix mit unterschieden im Score, Zeilen und Spaltennamen sind die cow Codes


# einlesen
Polity <- read.csv("Importdaten/extern/Polity IV.csv")
# Zeilen und Spalten auswählen
Polity <- Polity[Polity$year >= 1992 & Polity$year <= 2011, c("ccode", "year", "polity")]
# "Interruption (-66)", "Interregnum (-77)", "Transition (-88)" auf NA setzen
is.na(Polity$polity) <- which(Polity$polity < -10)
# Nach Jahren trennen
Year <- 1992:2011
PolityYear <- lapply(Year, function(jahr) subset(Polity, year==jahr))

#gewichtete Adjacency - Matrix erstellen. Einträge sind Unterschiede der Scores

ccode.names <- names(table(Polity$ccode))
l <- length(ccode.names)
APolity <- list()

for(y in 1:20){
  mat <- matrix(0,nrow = l, ncol = l)
  p1 <- PolityYear[[y]]
  for(i in 1:l){
    for(j in 1:i){
      v1 <- p1[p1$ccode == ccode.names[i],"polity"]
      v2 <- p1[p1$ccode == ccode.names[j],"polity"]
      if(length(v1) > 0 & length(v2) > 0)
        mat[i,j] <- mat[j,i] <-  abs(v1 - v2)
    }
  }
  rownames(mat) <- ccode.names
  colnames(mat) <- ccode.names
  nas <- which(is.na(mat))
  mat[nas] <- 0
  APolity[[y]] <- mat
}
# Environment aufräumen:
rm("ccode.names", "i", "j", "l", "mat", "p1", "v1", "v2", "y", "nas")